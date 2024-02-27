#Script to connect WEAP energy demand for water results with SWITCH load input files

#This script supports the analysis of the manuscript submitted to Nature Communications on climate impacts and adaptation of the WECC grid. It does the following: 
# 1. Takes the WEAP monthly energy use results under the climate scenarios and baseline historical climate by water category
# 2. Multiplies the monthly energy use by category by the hourly load shape of that category to calculate hourly energy use
# 3. Aggregates the hourly energy use across all water categories for each load zone
# 4. Calculates hourly delta absolute energy use for each load zone and climate scenario
# 5. Applies those hourly delta energy use results for each of the load zones in the SWITCH baseline input files, adjusting the load time series. 
# 6. The script outputs new SWITCH load input files that account for the climate sceanrios


library(tidyverse)
library(lubridate)
library(stringr)
library(readxl)

rm(list=ls())  #clear variables in R space

#directory where all WEAP results are saved

#this is the id of the SWITCH baseline run that will be adjusted for the WEAP results
SWITCH_run_id <- "id_202_WECC_0_carbon_baseline_5y_24_sample_barrier"

#what is the sampling interval per day in SWITCH? every 4 hours or 24 hours?
# hours_sampled <- 4
hours_sampled <- 24

WEAP_run_id <- "Nov_8_2022results"

#replace file path here:
WEAP.output.dir <- ""

#SWITCH input directory
#replace file path here:
SWITCH.input.dir <- ""

scenario_list <- c("CCSM","HadGEM2-ES","CanESM","CESM1-CAM5","ACCESS-1.0","CNRM-CM5",
                   "GFDL-ESM2M","bcc-csm1-1", "MPI-ESM-LR", "MIROC5", "HadGEM2-CC", "GFDL-CM3" ,
                    "CMCC-CMS"  , "CMCC-CM", "CESM1-BGC")


#which climate impacts are included? Either hydropower and energy for water only, or hydropower and energy for water + energy for heating and cooling (CDD_HDD)
# climate_impacts_included <- "Hydro_Load"
climate_impacts_included <- "CDD_HDD_Hydro_Load"


mapping_file <- "Mapping_WEAP_energy_for_water_to_SWITCH_loadzonesOct2022_v1.xlsx"


for (i in 1:length(scenario_list)){
  
  CC_scenario <- scenario_list[[i]]
  
  if (climate_impacts_included == "Hydro_Load") {
    
    #check if a climate scenario folder has been made that copies the baseline input files 
    climate_scenario_directory <- paste(SWITCH.input.dir, SWITCH_run_id,"_",CC_scenario, "_Hydro_Load/", sep="")
    
  } else if (climate_impacts_included == "CDD_HDD_Hydro_Load") {
    #check if a climate scenario folder has been made that copies the baseline input files 
    climate_scenario_directory <- paste(SWITCH.input.dir, SWITCH_run_id,"_",CC_scenario, "_CDD_HDD_Hydro_Load/", sep="")
  }
  
  print(CC_scenario)
  
  #####HISTORICAL REFERENCE ENERGY DEMAND FILES FROM WEAP
  
  # read in each type of energy demand
  #read in WEAP Reference outputs
  
  WEAP_output_file_list <- c("Demand Site Electricity Use Monthly","Diversion Electricity Use Monthly","Transmission Link Electricity Use Monthly","Electricity Use Return Flows Monthly")
  
  WEAP_reference_electricty <- data.frame()
  
  for (i in 1:length(WEAP_output_file_list)){
    
    WEAP_ref_output <- WEAP_output_file_list[[i]]
    
    WEAP_ref_input_filename <- paste("RefLOCA","_", WEAP_ref_output,".csv", sep="")
    WEAP_data_ref <- read.table(paste(WEAP.output.dir, "WEAP_results/", WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/", WEAP_ref_input_filename, sep=""), skip = 5, header = TRUE, sep =',')
    
    #renaming some column names
    WEAP_data_ref <- rename(WEAP_data_ref, "Year" = "X.Columns...Year", "Month" = "Timestep")
    WEAP_data_ref$Max.Gigawatt.Hour. <- NULL
    WEAP_data_ref$Min.Gigawatt.Hour. <- NULL
    WEAP_data_ref$Sum.Gigawatt.Hour. <- NULL
    WEAP_data_ref$Median.Gigawatt.Hour. <- NULL
    WEAP_data_ref$SD.Gigawatt.Hour. <- NULL
    WEAP_data_ref$Mean.Gigawatt.Hour. <- NULL
    WEAP_data_ref$RMS.Gigawatt.Hour. <- NULL
    
    #transpose to long format
    WEAP_data_ref_long <- gather(WEAP_data_ref, WEAP_object_name_raw, Reference_ElectricityUse_GWh, 3:ncol(WEAP_data_ref))
    
    #parse object name from column
    #removing GWh unit label
    WEAP_object_name <- str_replace(WEAP_data_ref_long$WEAP_object_name_raw, ".Gigawatt.Hour.","\\.")
    #replacing . with space and removing trailing space
    WEAP_object_name1 <- str_trim(str_replace_all(WEAP_object_name, "\\."," "), side = "right")
    #checking length
    WEAP_object_name1 <- cbind(WEAP_object_name1, str_length(WEAP_object_name1))
    WEAP_object_name1 <- as.data.frame(WEAP_object_name1)
    colnames(WEAP_object_name1) <- c("WEAP_object_name", "length")
    WEAP_object_name1$length <- NULL
    #binding corrected name and deleting old name
    WEAP_data_ref_long <- cbind(WEAP_data_ref_long, WEAP_object_name1)
    WEAP_data_ref_long$WEAP_object_name_raw <- NULL
    
    #designating each year to a period for averaging later
    #dropping first years because of spin up years
    WEAP_data_ref_long <- WEAP_data_ref_long %>% filter(Year > 2024 & Year < 2056)
    
    WEAP_data_ref_long$WEAP_file_name <- WEAP_ref_output
    
    WEAP_reference_electricty <- rbind(WEAP_reference_electricty, WEAP_data_ref_long)
    
  }  
  
  ###CALCULATING PERIOD AVERAGE
  
  #calculate rolling average of monthly energy demand +/- 5 years to have 11-year window average. Then only filter out the midpoint years which are going to be the sampled investment periods
  #list of WEAP objects
  WEAP_object_list <- list(unique(WEAP_reference_electricty$WEAP_object_name))
  
  WEAP_reference_electricty_avg <- data.frame()
  
  for (g in 1:length(WEAP_object_list[[1]])) {
    
    #filter out the data for each object
    WEAP_ref_electricity_by_object <- WEAP_reference_electricty %>% filter(WEAP_reference_electricty$WEAP_object_name == WEAP_object_list[[1]][g])
    # #calculate 10-yr moving window average for each month of year
    w = 120
    
    for (j in seq_len(length(WEAP_ref_electricity_by_object$Reference_ElectricityUse_GWh))) {
      #creates a sequence of indices of monthly observations from minus 5 years to plus 5 years with each annual observation
      ind <- seq((j - floor(w / 2)), (j + floor(w / 2)), by=12)
      #removes indices that are negative and greater than the length of the column
      ind <- ind[ind>0]
      ind <- ind[ind<=length(WEAP_ref_electricity_by_object$Reference_ElectricityUse_GWh)]
      #average the generation for each month of year across the years of the averaging window
      WEAP_ref_electricity_by_object$Avg_Reference_ElectricityUse_GWh[j] <- mean(WEAP_ref_electricity_by_object$Reference_ElectricityUse_GWh[ind]) 
      
      
    }
    #stacking the monthly averages of the individual generators
    WEAP_reference_electricty_avg <- rbind(WEAP_reference_electricty_avg, WEAP_ref_electricity_by_object)  
    
  }
  
  #dropping the point estimates of monthly energy use for the particular year to only keep the average
  WEAP_reference_electricty_avg$Reference_ElectricityUse_GWh <- NULL
  
  WEAP_reference_electricty_avg <- rename(WEAP_reference_electricty_avg, period = Year, Reference_ElectricityUse_GWh = Avg_Reference_ElectricityUse_GWh)
  
  #selecting the years that are sampled for the investment periods
  
  WEAP_reference_electricty_avg <- WEAP_reference_electricty_avg %>% filter(WEAP_reference_electricty_avg$period %in% c(2025, 2030, 2035, 2040, 2045, 2050, 2055))
  
  # #Calculating decadal average energy demand
  #creating a "date" that uses the mid point of the period as the year, the month, and day = 1 as a representative date for that decadal monthly average
  
  #calculating the number of days for each month
  WEAP_reference_electricty_avg$Date <- ISOdate(WEAP_reference_electricty_avg$period, WEAP_reference_electricty_avg$Month, 1)
  WEAP_reference_electricty_avg$days_in_month <- days_in_month(WEAP_reference_electricty_avg$Date)
  WEAP_reference_electricty_avg$Year <- WEAP_reference_electricty_avg$period
  
  ###READ IN MAPPING FILE THAT MATCHES WEAP OBJECTS WITH SECTORS, ENERGY INCLUDED, AND LOAD ZONES
  
  #read in mapping file that includes all the WEAP objects for electricity and the associated sector and included energy use
  
  water_energy_object_mapping <- read_excel(paste(WEAP.output.dir, mapping_file, sep=""), sheet = "All_WEAP_Electricity_Demand_Obj", col_names = TRUE )
  
  #join with reference energy demands by WEAP object name
  WEAP_reference_electricty_mapped <- left_join(WEAP_reference_electricty_avg, water_energy_object_mapping, c("WEAP_object_name"="WEAP_object_name", "WEAP_file_name" = "WEAP_file_name"))
  
  missing_WEAP_reference_electricty_mapped <- WEAP_reference_electricty_mapped %>% filter(is.na(SWITCH_load_zone))
  
  #####READING IN WEAP ENERGY DEMAND FILES FROM WEAP UNDER CLIMATE CHANGE, CALCULATING DELTA DUE TO CLIMATE CHANGE
  
  # read in CC results for each type of energy demand
  
  WEAP_CC_electricty <- data.frame()
  
  for (i in 1:length(WEAP_output_file_list)){
    
    WEAP_CC_output <- WEAP_output_file_list[[i]]
    
    WEAP_CC_input_filename <- paste(CC_scenario, "_", WEAP_CC_output, ".csv", sep="")
    WEAP_data_CC <- read.table(paste(WEAP.output.dir, "WEAP_results/", WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/", WEAP_CC_input_filename, sep=""), skip = 5, header = TRUE, sep =',')
    
    #renaming some column names
    WEAP_data_CC <- rename(WEAP_data_CC, "Year" = "X.Columns...Year", "Month" = "Timestep")
    WEAP_data_CC$Max.Gigawatt.Hour. <- NULL
    WEAP_data_CC$Min.Gigawatt.Hour. <- NULL
    WEAP_data_CC$Sum.Gigawatt.Hour. <- NULL
    WEAP_data_CC$Median.Gigawatt.Hour. <- NULL
    WEAP_data_CC$SD.Gigawatt.Hour. <- NULL
    WEAP_data_CC$Mean.Gigawatt.Hour. <- NULL
    WEAP_data_CC$RMS.Gigawatt.Hour. <- NULL
    
    #transpose to long format
    WEAP_data_CC_long <- gather(WEAP_data_CC, WEAP_object_name_raw, CC_ElectricityUse_GWh, 3:ncol(WEAP_data_CC))
    
    #parse object name from column
    #removing GWh unit label
    WEAP_object_name <- str_replace(WEAP_data_CC_long$WEAP_object_name_raw, ".Gigawatt.Hour.","\\.")
    #replacing . with space and removing trailing space
    WEAP_object_name1 <- str_trim(str_replace_all(WEAP_object_name, "\\."," "), side = "right")
    #checking length
    WEAP_object_name1 <- cbind(WEAP_object_name1, str_length(WEAP_object_name1))
    WEAP_object_name1 <- as.data.frame(WEAP_object_name1)
    colnames(WEAP_object_name1) <- c("WEAP_object_name", "length")
    WEAP_object_name1$length <- NULL
    #binding corrected name and deleting old name
    WEAP_data_CC_long <- cbind(WEAP_data_CC_long, WEAP_object_name1)
    WEAP_data_CC_long$WEAP_object_name_raw <- NULL
    
    #designating each year to a period for averaging later
    #dropping first year because of spin up year
    WEAP_data_CC_long <- WEAP_data_CC_long %>% filter(Year > 2024 & Year < 2056)
    
    WEAP_data_CC_long$WEAP_file_name <- WEAP_CC_output
    WEAP_data_CC_long$WEAP_CC_scenario <- CC_scenario
    
    WEAP_CC_electricty <- rbind(WEAP_CC_electricty, WEAP_data_CC_long)
    
  } 
  

  ######CALCULATING "CC ANOMOLY FACTORS" AS CLIMATE CHANGE IMPACT COMPARED TO REFERENCE SCENARIO

  ###CALCULATING PERIOD AVERAGE
  
  #calculate rolling average of monthly energy demand +/- 5 years to have 11-year window average. Then only filter out the midpoint years which are going to be the sampled investment periods
  #list of WEAP objects
  WEAP_object_list <- list(unique(WEAP_CC_electricty$WEAP_object_name))
  
  WEAP_CC_electricty_avg <- data.frame()
  
  for (g in 1:length(WEAP_object_list[[1]])) {
    
    #filter out the data for each object
    WEAP_CC_electricty_by_object <- WEAP_CC_electricty %>% filter(WEAP_CC_electricty$WEAP_object_name == WEAP_object_list[[1]][g])
    # #calculate 10-yr moving window average for each month of year
    w = 120
    
    for (j in seq_len(length(WEAP_CC_electricty_by_object$CC_ElectricityUse_GWh))) {
      #creates a sequence of indices of monthly observations from minus 5 years to plus 5 years with each annual observation
      ind <- seq((j - floor(w / 2)), (j + floor(w / 2)), by=12)
      #removes indices that are negative and greater than the length of the column
      ind <- ind[ind>0]
      ind <- ind[ind<=length(WEAP_CC_electricty_by_object$CC_ElectricityUse_GWh)]
      #average the generation for each month of year across the years of the averaging window
      WEAP_CC_electricty_by_object$Avg_CC_ElectricityUse_GWh[j] <- mean(WEAP_CC_electricty_by_object$CC_ElectricityUse_GWh[ind]) 
      
      
    }
    #stacking the monthly averages of the individual generators
    WEAP_CC_electricty_avg <- rbind(WEAP_CC_electricty_avg, WEAP_CC_electricty_by_object)  
    
  }
  
  #dropping the point estimates of monthly energy use for the particular year to only keep the average
  WEAP_CC_electricty_avg$CC_ElectricityUse_GWh <- NULL
  
  WEAP_CC_electricty_avg <- rename(WEAP_CC_electricty_avg, period = Year, CC_ElectricityUse_GWh = Avg_CC_ElectricityUse_GWh)
  
  #selecting the years that are sampled for the investment periods
  
  WEAP_CC_electricty_avg <- WEAP_CC_electricty_avg %>% filter(WEAP_CC_electricty_avg$period %in% c(2025, 2030, 2035, 2040, 2045, 2050, 2055))
  
  #creating a "date" that uses the mid point of the period as the year, the month, and day = 1 as a representative date for that decadal monthly average
  
  #calculating the number of days for each month
  WEAP_CC_electricty_avg$Date <- ISOdate(WEAP_CC_electricty_avg$period, WEAP_CC_electricty_avg$Month, 1)
  WEAP_CC_electricty_avg$Year <- WEAP_CC_electricty_avg$period

  # join CC with reference
  WEAP_Ref_CC_electricty <- left_join(WEAP_reference_electricty_mapped, WEAP_CC_electricty_avg , c("period" = "period", "Year"="Year","Month"="Month",
                                                                                              "WEAP_object_name"="WEAP_object_name", "Date"="Date", 
                                                                                              "WEAP_file_name" = "WEAP_file_name"))
  
  # calculate delta for each row
  WEAP_Ref_CC_electricty$Delta_ElectricityUse_GWh <- WEAP_Ref_CC_electricty$CC_ElectricityUse_GWh - WEAP_Ref_CC_electricty$Reference_ElectricityUse_GWh
  
  #output a copy of the raw WEAP output into the SWITCH folder with the run being linked with the WEAP scenario
  write.csv(WEAP_Ref_CC_electricty, file = paste(climate_scenario_directory, 'WEAP_results/', "WEAP_Ref_CC_electricity_monthly_by_object.csv", sep=""), row.names = FALSE)
  
  
  #####AGGREGATE DELTA WEAP MONTHLY ENERGY DEMAND BY SECTOR AND LOAD ZONE AND JOIN WITH SAMPLED TIMEPOINTS FROM SWITCH
  
  # aggregate by load zone, and by type of energy demand (sector) and energy included
  WEAP_agg_delta_elec_lz_sector <- WEAP_Ref_CC_electricty %>% group_by(period, Year, Month, Date, days_in_month,SWITCH_load_zone, SWITCH_load_zone_id, WEAP_file_name, Sector, `Included Energy`) %>% 
    summarize(CC_ElectricityUse_GWh = sum(CC_ElectricityUse_GWh), Reference_ElectricityUse_GWh = sum(Reference_ElectricityUse_GWh), Delta_ElectricityUse_GWh = sum(Delta_ElectricityUse_GWh))
  
  #output a copy of the raw WEAP output into the SWITCH folder with the run being linked with the WEAP scenario
  write.csv(WEAP_agg_delta_elec_lz_sector, file = paste(climate_scenario_directory, 'WEAP_results/', "Aggregated_WEAP_Ref_CC_electricity_monthly_by_object.csv", sep=""), row.names = FALSE)
  
  read_in_SWITCH_data <- function(climate_scenario_directory, input_file) {
    
    #reading in the WEAP results for each scenario
    project_directory <- paste(climate_scenario_directory, 'inputs/', sep="")
    
    SWITCH_input_file <- paste(project_directory, input_file, sep="")
    
    #reading in specific table and range
    SWITCH_data <- read.csv(file = SWITCH_input_file)
    
    #writing a copy of the file in case it is modified
    output_dir <- paste(project_directory,"Archive/",sep = "")
    if (!dir.exists(output_dir)) {dir.create(output_dir)}
    
    write.csv(SWITCH_data, paste(output_dir,"no_CC_orig_",input_file,sep=""), row.names = FALSE)
    
    
    return(SWITCH_data)
    
  }
  ###READ IN SWITCH SAMPLED TIMEPOINTS
  
  switch_sampled_timepoints <- read_in_SWITCH_data(climate_scenario_directory = climate_scenario_directory, input_file = "timepoints.csv")
  
  #parse time points to human readable date and components
  switch_sampled_timepoints$Month <- as.integer(substr(switch_sampled_timepoints$timestamp, 5, 6))
  switch_sampled_timepoints$Year <- as.integer(substr(switch_sampled_timepoints$timestamp, 1, 4))
  switch_sampled_timepoints$Day <- as.integer(substr(switch_sampled_timepoints$timestamp, 7, 8))
  switch_sampled_timepoints$Hour_UTC <- as.integer(substr(switch_sampled_timepoints$timestamp, 9, 10))
  #constructing datetime in UTC (time zone of time points)
  switch_sampled_timepoints$date_time_UTC <- ymd_hms(paste(paste(switch_sampled_timepoints$Year,switch_sampled_timepoints$Month,switch_sampled_timepoints$Day, sep="-"), 
                                                           paste(switch_sampled_timepoints$Hour_UTC,"00","00",sep=":"),sep=" "), tz = "UTC")
  
  #checking tz:
  attributes(switch_sampled_timepoints$date_time_UTC)$tzone
  
  ###JOIN SWITCH SAMPLED TIMEPOINTS WITH MONTHLY CC ENERGY DELTA
  
  #join month and year with energy demand by load zone and sector
  
  WEAP_agg_delta_elec_lz_sector$Date <- NULL
  
  WEAP_delta_elec_lz_sector_sampled_tps <- left_join(switch_sampled_timepoints, WEAP_agg_delta_elec_lz_sector, c("Year"="Year", "Month"="Month"))
  
  ###JOIN MONTHLY ENERGY DELTA AND SAMPLED TIMEPOINTS WITH SECTORAL HOURLY WEIGHTS FOR ENERGY DEMAND FOR WATER
  
  #read in hourly weights by sector
  #read in mapping file that has all hourly weights for each day, in UTC time zone, with each hour's weight representing the total weight of that hour and the previous 3 hours since each sampled hour 
  
  if(hours_sampled == 4){
    water_energy_hourly_weights <- read_excel(paste(WEAP.output.dir, mapping_file, sep=""), sheet = "hour_weighting_by sector", col_names = TRUE, range="A59:I83" ) 
  }else if (hours_sampled ==24){
    water_energy_hourly_weights <- read_excel(paste(WEAP.output.dir, mapping_file, sep=""), sheet = "hour_weighting_by sector", col_names = TRUE, range="A32:I56" )  
  }
  
  
  #transpose to long format
  water_energy_hourly_weights_long <- gather(water_energy_hourly_weights, hour_weighting_category, hourly_weight_per_day, 3:ncol(water_energy_hourly_weights))
  
  #read in mapping of which hourly weights are associated with each sector
  sector_to_hourly_weight_mapping <- read_excel(paste(WEAP.output.dir, mapping_file, sep=""), sheet = "sector_hour_weight_mapping", col_names = TRUE, range="A1:D17" )
  
  #join hourly weights with sectoral mapping
  sector_to_hourly_weight_mapping2 <- left_join(sector_to_hourly_weight_mapping, water_energy_hourly_weights_long, c("hour_weighting_category"="hour_weighting_category"))
  
  #join hourly weights with sampled time points, calculate day in month weights
  WEAP_delta_elec_lz_sector_sampled_tps_weights <- left_join(WEAP_delta_elec_lz_sector_sampled_tps, sector_to_hourly_weight_mapping2, c("Hour_UTC"="Hour",
                                                                                                                                        "Sector"="sector",
                                                                                                                                        "Included Energy" = "energy_included", 
                                                                                                                                        "WEAP_file_name"="WEAP_output_file"))
  
  WEAP_delta_elec_lz_sector_sampled_tps_weights <- WEAP_delta_elec_lz_sector_sampled_tps_weights %>% filter(`Included Energy` != "None")
  
  #multiply day in month and hourly weights for each sampled time point with monthly energy demand by sector
  WEAP_delta_elec_lz_sector_sampled_tps_weights$Sampled_Delta_ElecUse_GWh <- WEAP_delta_elec_lz_sector_sampled_tps_weights$Delta_ElectricityUse_GWh * (1/WEAP_delta_elec_lz_sector_sampled_tps_weights$days_in_month) * WEAP_delta_elec_lz_sector_sampled_tps_weights$hourly_weight_per_day
  
  # output change in load for sampled timepoints due to climate impacts from water
  write.csv(x = WEAP_delta_elec_lz_sector_sampled_tps_weights, paste(climate_scenario_directory, 'inputs/Archive/',"Disaggregated_CC_impact_energy_for_water_by_sampled_tps.csv",sep=""), row.names = F)
  
  # aggregate deltas in energy demand by sampled time point and load zone
  WEAP_agg_delta_elec_lz_sampled_tps <- WEAP_delta_elec_lz_sector_sampled_tps_weights %>% group_by(timepoint_id, SWITCH_load_zone) %>% summarize(Sampled_Delta_ElecUse_GWh = sum(Sampled_Delta_ElecUse_GWh))
  
  # read in SWITCH sampled load, for each all load zones
  switch_load <- read_in_SWITCH_data(climate_scenario_directory = climate_scenario_directory, input_file = "loads.csv")
  
  # join aggregated hourly sampled deltas with loads by zone and timepoint
  switch_load_with_CCdelta <- left_join(switch_load, WEAP_agg_delta_elec_lz_sampled_tps, c("LOAD_ZONE" = "SWITCH_load_zone", "TIMEPOINT" = "timepoint_id"))
  
  #add the delta in energy use for water to the load for each timepint and load zone
  switch_load_with_CCdelta$zone_demand_mw_with_CCdelta <- switch_load_with_CCdelta$zone_demand_mw + (switch_load_with_CCdelta$Sampled_Delta_ElecUse_GWh) * 1000
  
  #if the load with the CC impact is negative, keep the original load
  switch_load_with_CCdelta$zone_demand_mw_with_CCdelta <- ifelse(switch_load_with_CCdelta$zone_demand_mw_with_CCdelta < 0, switch_load_with_CCdelta$zone_demand_mw, switch_load_with_CCdelta$zone_demand_mw_with_CCdelta)
  
  switch_load_with_CCdelta$zone_demand_perc_change <- (switch_load_with_CCdelta$zone_demand_mw_with_CCdelta - switch_load_with_CCdelta$zone_demand_mw)/switch_load_with_CCdelta$zone_demand_mw
  
  # output new load
  write.csv(x = switch_load_with_CCdelta, paste(climate_scenario_directory, 'inputs/Archive/',"loads_incl_CC_impact_energy_for_water_with_old_loads.csv",sep=""), row.names = F)
  
  #remove unneeded columns
  switch_load_with_CCdelta_final <- switch_load_with_CCdelta
  switch_load_with_CCdelta_final$Sampled_Delta_ElecUse_GWh <- NULL
  switch_load_with_CCdelta_final$zone_demand_perc_change <- NULL
  switch_load_with_CCdelta_final$zone_demand_mw <- NULL
  switch_load_with_CCdelta_final <- rename(switch_load_with_CCdelta_final, zone_demand_mw = zone_demand_mw_with_CCdelta)
  switch_load_with_CCdelta_final$zone_demand_mw <- round(switch_load_with_CCdelta_final$zone_demand_mw, 3)
  
  write.csv(x = switch_load_with_CCdelta_final, paste(climate_scenario_directory, 'inputs/',"loads.csv",sep=""), row.names = F, quote = F)
  
  #adding up new and old loads with weights to get total annual loads
  #reading in time sampling weights
  
  switch_sampled_weights <- read_in_SWITCH_data(climate_scenario_directory = climate_scenario_directory, input_file = "timeseries.csv")
  switch_sampled_weights_tps <- left_join(switch_sampled_weights, switch_sampled_timepoints, c("TIMESERIES" = "timeseries"))
  
  switch_load_with_CCdelta_weightstps <- left_join(switch_load_with_CCdelta, switch_sampled_weights_tps, c("TIMEPOINT" = "timepoint_id"))
  
  #UPDATED TO REFLECT 5-year periods
  #scaling up to annual amount
  switch_load_with_CCdelta_weightstps$scaled_zone_demand_mw_with_CCdelta <- switch_load_with_CCdelta_weightstps$zone_demand_mw_with_CCdelta * switch_load_with_CCdelta_weightstps$ts_duration_of_tp * switch_load_with_CCdelta_weightstps$ts_scale_to_period/5
  switch_load_with_CCdelta_weightstps$scaled_zone_demand_mw<- switch_load_with_CCdelta_weightstps$zone_demand_mw * switch_load_with_CCdelta_weightstps$ts_duration_of_tp * switch_load_with_CCdelta_weightstps$ts_scale_to_period/5
  switch_load_with_CCdelta_weightstps$scaled_zone_CCdeltademand_mw<- switch_load_with_CCdelta_weightstps$scaled_zone_demand_mw_with_CCdelta - switch_load_with_CCdelta_weightstps$scaled_zone_demand_mw
  
  switch_load_monthly_lz_cc <- switch_load_with_CCdelta_weightstps %>% group_by(LOAD_ZONE, Month, Year) %>% summarize(scaled_zone_demand_mw_with_CCdelta = sum(scaled_zone_demand_mw_with_CCdelta), 
                                                                                                                      scaled_zone_demand_mw = sum(scaled_zone_demand_mw),
                                                                                                                      scaled_zone_CCdeltademand_mw = sum(scaled_zone_CCdeltademand_mw))
  # output new load and deltas monthly by load zone
  write.csv(x = switch_load_monthly_lz_cc, paste(climate_scenario_directory, 'inputs/Archive/',"monthly_loads_by_loadzone_and_CC_delta.csv",sep=""), row.names = F)
  
  
  switch_load_annual_lz_cc <- switch_load_with_CCdelta_weightstps %>% group_by(LOAD_ZONE, Year) %>% summarize(scaled_zone_demand_mw_with_CCdelta = sum(scaled_zone_demand_mw_with_CCdelta), 
                                                                                                                     scaled_zone_demand_mw = sum(scaled_zone_demand_mw),
                                                                                                                     scaled_zone_CCdeltademand_mw = sum(scaled_zone_CCdeltademand_mw))
  
  # output new load and deltas annually by load zone
  write.csv(x = switch_load_annual_lz_cc, paste(climate_scenario_directory, 'inputs/Archive/',"annual_loads_by_loadzone_and_CC_delta.csv",sep=""), row.names = F)
  
  switch_load_annual_WECC_cc <- switch_load_with_CCdelta_weightstps %>% group_by(Year) %>% summarize(scaled_zone_demand_mw_with_CCdelta = sum(scaled_zone_demand_mw_with_CCdelta), 
                                                                                                              scaled_zone_demand_mw = sum(scaled_zone_demand_mw),
                                                                                                              scaled_zone_CCdeltademand_mw = sum(scaled_zone_CCdeltademand_mw))
  
  # output new load
  write.csv(x = switch_load_annual_WECC_cc, paste(climate_scenario_directory, 'inputs/Archive/',"annual_loads_WECC_and_CC_delta.csv",sep=""), row.names = F)

}

###PLOTTING Load profiles by sector

if(hours_sampled == 4){
  water_energy_hourly_weights <- read_excel(paste(WEAP.output.dir, mapping_file, sep=""), sheet = "hour_weighting_by sector", col_names = TRUE, range="A59:I83" ) 
}else if (hours_sampled ==24){
  water_energy_hourly_weights <- read_excel(paste(WEAP.output.dir, mapping_file, sep=""), sheet = "hour_weighting_by sector", col_names = TRUE, range="A32:I56" )  
}

water_energy_hourly_weights <- rename(water_energy_hourly_weights, `Agricultural irrigation` = AgUse_hour_weight,  `Domestic` = Domestic_hour_weight, 
                                      `Commercial & Industrial`=Commercial_industrial_hour_weight, `Groundwater pumping` = Groundwater_pump_hour_weight, `Urban irrigation` = Urban_outdoor_hour_weight,
                                      Conveyance = Diversion_hour_weight, `Wastewater treatment`=WW_treat_hour_weight)
#transpose to long format
water_energy_hourly_weights_long <- gather(water_energy_hourly_weights, hour_weighting_category, hourly_weight_per_day, 3:ncol(water_energy_hourly_weights))

#read in mapping of which hourly weights are associated with each sector
sector_to_hourly_weight_mapping <- read_excel(paste(WEAP.output.dir, mapping_file, sep=""), sheet = "sector_hour_weight_mapping", col_names = TRUE, range="A1:D17" )

#join hourly weights with sectoral mapping
sector_to_hourly_weight_mapping2 <- left_join(sector_to_hourly_weight_mapping, water_energy_hourly_weights_long, c("hour_weighting_category"="hour_weighting_category"))

#plot of load shapes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

###FIGURE 6
plot_file_name <- "Plot of water load daily load shapes.png"

png(paste(SWITCH.input.dir, 'compare scenarios/SWITCH_WEAP_runs/WEAP_Nov_8_results_SWITCH_Mar_27_results/', plot_file_name, sep=""), width=1200, height=1000, res=100)

plot <- ggplot() + geom_line(data=water_energy_hourly_weights_long, aes(x=Hour, y=hourly_weight_per_day, color=hour_weighting_category), size=1) + 
  scale_color_manual(name = "Water Load Category", values = cbPalette) +
# , breaks=c("No storage", "GridMet", "E3SM 1.0", "RRM-E3SM 0.0325"),
#                      values = c("No storage" = "grey", "GridMet" = "#CC79A7", "RRM-E3SM 0.0325" = "#0072B2", "E3SM 1.0" = "#E69F00")) +
  ggtitle(paste("Daily load shapes of water load energy categories", sep=", ")) + xlab("Hour") + ylab("Hourly weighting fraction") +
  scale_x_continuous(limits=c(0,24))+
  theme(axis.text.y = element_text(size=14), axis.title.x = element_text(size=16), axis.text.x = element_text(size=12, angle = 90), axis.title.y=element_text(size=16), 
        legend.text = element_text(size = 14), legend.title = element_text(size=14), legend.position="bottom", plot.title = element_text(size=18,hjust=0.5), 
        strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 16)) 
print(plot)

dev.off()  

