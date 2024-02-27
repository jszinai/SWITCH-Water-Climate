#Script to connect WEAP hydropower generation results with SWITCH load input files

#This script supports the analysis of the manuscript submitted to Nature Communications on climate impacts and adaptation of the WECC grid. It does the following: 
# 1. Takes the WEAP monthly hydropower generation potential results under the climate scenarios and baseline historical climate, 
# 2. Calculates monthly delta ratios for each generator and climate scenario
# 3. Calculates average monthly delta ratios by load zone
# 4. Applies those delta ratios for each of the hydropower generators (multiplying the baseline average and minimum power) in the SWITCH baseline input files. 
# 5. For generators not modeled in WEAP (<30 MW, or in Canada), the load zone average delta ratio is used to adjust the baseline average and minimum power levels in the SWITCH input files.
# 6. The script outputs new SWITCH hydropower input files (hydro timeseries and reserve capacity) that account for the climate sceanrios

library(tidyverse)
library(lubridate)
library(stringr)
library(readxl)

rm(list=ls())  #clear variables in R space

#directory where all WEAP results are saved

#this is the id of the SWITCH baseline run that will be adjusted for the WEAP results

SWITCH_run_id <- "id_202_WECC_0_carbon_baseline_5y_24_sample_barrier"

WEAP_run_id <- "Nov_8_2022results"

#replace file path here:
WEAP.output.dir <- ""


#COPY SWITCH BASELINE RUN INPUTS AND RESULTS FROM SERVER TO THIS FOLDER:

#SWITCH input directory

#replace file path here:
SWITCH.input.dir <- ""

#list of climate scenarios
scenario_list <- c("CCSM","HadGEM2-ES","CanESM","CESM1-CAM5","ACCESS-1.0","CNRM-CM5",
                   "GFDL-ESM2M","bcc-csm1-1", "MPI-ESM-LR", "MIROC5", "HadGEM2-CC", "GFDL-CM3" ,
                   "CMCC-CMS"  , "CMCC-CM", "CESM1-BGC")

# climate_impacts_included <- "Hydro_Load"
climate_impacts_included <- "CDD_HDD_Hydro_Load"


####MAKING A NEW DIRECTORY FOR SWITCH CLIMATE SCENARIO RUNS
make_climate_scenario_dir <- function(WEAP_scenario) {
  
  # update this path to CDD_HDD if that is the baseline instead of no-climate-change
  
  if (climate_impacts_included == "Hydro_Load") {
    
    #check if a climate scenario folder has been made that copies the baseline input files 
    climate_scenario_directory <- paste(SWITCH.input.dir, SWITCH_run_id,"_",WEAP_scenario, "_Hydro_Load/", sep="")
    
    #baseline for the Hydro and load from water scenarios is the no CC SWITCH reference scenario
    baseline_directory <- paste(SWITCH.input.dir, SWITCH_run_id,"/", sep="")
    
    #make a copy of inputs from switch baseline run because this is what needs to be modified for the climate scenario runs
    if(dir.exists(climate_scenario_directory) == FALSE){
      
      dir.create(climate_scenario_directory)
      dir.create(paste(climate_scenario_directory,"/inputs/",sep=""))
      dir.create(paste(climate_scenario_directory,"/inputs/maps/",sep=""))
      
      file.copy(list.files(paste(baseline_directory,"/inputs/",sep=""), full.names = TRUE, recursive = TRUE), paste(climate_scenario_directory,"/inputs/",sep=""), recursive=TRUE)
      file.copy(list.files(paste(baseline_directory,"/inputs/maps/",sep=""), full.names = TRUE, recursive = TRUE), paste(climate_scenario_directory,"/inputs/maps/",sep=""), recursive=TRUE)
      
      #make a folder for WEAP results that will be linked with SWITCH for the particular climate run
      dir.create(paste(climate_scenario_directory,"/WEAP_results/",sep=""))
    }
  } else if (climate_impacts_included == "CDD_HDD_Hydro_Load") {
    #check if a climate scenario folder has been made that copies the baseline input files 
    climate_scenario_directory <- paste(SWITCH.input.dir, SWITCH_run_id,"_",WEAP_scenario, "_CDD_HDD_Hydro_Load/", sep="")
    
    #baseline input files are from the CDD_HDD only directories that have the CDD_HDD modified loads and time sampling
    baseline_directory <- paste(SWITCH.input.dir, SWITCH_run_id,"_",WEAP_scenario, "_CDD_HDD/", sep="")
    
    #make a copy of inputs from switch baseline run because this is what needs to be modified for the climate scenario runs
    if(dir.exists(climate_scenario_directory) == FALSE){
      
      dir.create(climate_scenario_directory)
      dir.create(paste(climate_scenario_directory,"/inputs/",sep=""))
      dir.create(paste(climate_scenario_directory,"/inputs/maps/",sep=""))
      
      file.copy(list.files(paste(baseline_directory,"/inputs/",sep=""), full.names = TRUE, recursive = TRUE), paste(climate_scenario_directory,"/inputs/",sep=""), recursive=TRUE)
      file.copy(list.files(paste(baseline_directory,"/inputs/maps/",sep=""), full.names = TRUE, recursive = TRUE), paste(climate_scenario_directory,"/inputs/maps/",sep=""), recursive=TRUE)
      
      #make a folder for WEAP results that will be linked with SWITCH for the particular climate run
      dir.create(paste(climate_scenario_directory,"/WEAP_results/",sep=""))
    }  
  }
}

#making new input folders for all the climate scnearios
for (i in 1:length(scenario_list)){  
  
  WEAP_scenario <- scenario_list[[i]]

  make_climate_scenario_dir(WEAP_scenario = WEAP_scenario)
}


#####REFERENCE HYDROPOWER GENERATION FROM WEAP BASED ON HISTORICAL CLIMATE

#read in WEAP Reference outputs
WEAP_ref_input_filename <- paste("RefLOCA", "_Hydropower Generation",".csv", sep="")
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
WEAP_data_ref_long <- gather(WEAP_data_ref, WEAP_generator_name_raw, Reference_Generation_GWh, 3:ncol(WEAP_data_ref))

#parse generator name from column
#removing GWh unit label
WEAP_generator_name <- str_replace(WEAP_data_ref_long$WEAP_generator_name_raw, ".Gigawatt.Hour.","\\.")
#replacing . with space and removing trailing space
WEAP_generator_name1 <- str_trim(str_replace_all(WEAP_generator_name, "\\."," "), side = "right")
#checking length
WEAP_generator_name1 <- cbind(WEAP_generator_name1, str_length(WEAP_generator_name1))
WEAP_generator_name1 <- as.data.frame(WEAP_generator_name1)
colnames(WEAP_generator_name1) <- c("WEAP_generator_name", "length")
WEAP_generator_name1$length <- NULL
#binding corrected name and deleting old name
WEAP_data_ref_long <- cbind(WEAP_data_ref_long, WEAP_generator_name1)
WEAP_data_ref_long$WEAP_generator_name_raw <- NULL

#dropping first years and ending years because of spin up and boundary condition years
WEAP_data_ref_long <- WEAP_data_ref_long %>% filter(Year > 2015 & Year < 2053)

#Averaging the generation for each month, for each generator for the reference data, across entire time frame (historical data repeated)
WEAP_ref_month_avg <- WEAP_data_ref_long %>% group_by(WEAP_generator_name, Month) %>% summarize(Reference_Avg_Generation_GWh = mean(Reference_Generation_GWh))

#calculating the average power for each month (generation divided by number of hours of each month) because that is the input into SWITCH
WEAP_ref_month_avg$Date <- ISOdate(2021, WEAP_ref_month_avg$Month, 1) #choosing non-leap year to have a typcial year for calculating days in month
WEAP_ref_month_avg$days_in_month <- days_in_month(WEAP_ref_month_avg$Date)

#calculate monthly Avg MW using monthly generation and capacity_limit_mw
WEAP_ref_month_avg$WEAP_ref_hydro_avg_flow_mw <- (WEAP_ref_month_avg$Reference_Avg_Generation_GWh * 1000) / (WEAP_ref_month_avg$days_in_month * 24)
WEAP_ref_month_avg$Date <- NULL
WEAP_ref_month_avg$days_in_month <- NULL

###READ IN MAPPING FILE THAT MATCHES WEAP AND SWITCH GENERATORS

#read in mapping file that matches WEAP hydropower generator names with SWITCH generator ids and load zones
WEAP_SWITCH_hydro_mapping <- read.csv(file = paste(WEAP.output.dir,"Hydropower_SWITCH_handshake/", "WEAP_SWITCH_mapping_with_avg_annual_generation.csv", sep=""), stringsAsFactors = FALSE)

WEAP_SWITCH_hydro_mapping <- rename(WEAP_SWITCH_hydro_mapping, "hydro_project" = "generation_plant_id")


#####READING IN WEAP HYDROPOWER GENERATION UNDER CLIMATE CHANGE, CALCULATING CLIMATE CHANGE TO REFERENCE RATIOS

for (i in 1:length(scenario_list)){
# for (i in 1:length(scenario_list[[1]])){
  
  WEAP_scenario <- scenario_list[[i]]
  
  if (climate_impacts_included == "Hydro_Load") {
    
    #check if a climate scenario folder has been made that copies the baseline input files 
    climate_scenario_directory <- paste(SWITCH.input.dir, SWITCH_run_id,"_",WEAP_scenario, "_Hydro_Load/", sep="")
    
  } else if (climate_impacts_included == "CDD_HDD_Hydro_Load") {
    #check if a climate scenario folder has been made that copies the baseline input files 
    climate_scenario_directory <- paste(SWITCH.input.dir, SWITCH_run_id,"_",WEAP_scenario, "_CDD_HDD_Hydro_Load/", sep="")
  }
  
    
  print(WEAP_scenario)
  # WEAP_scenario <- scenario_list[[1]]

  #read in WEAP output data, clean up column names and data formats
  calculate_WEAP_CC_ref_ratios <- function(WEAP_scenario) {
    
    #read in WEAP CC outputs
    WEAP_input_filename <- paste(WEAP_scenario, "_Hydropower Generation",".csv", sep="")
    
    WEAP_data <- read.table(paste(WEAP.output.dir, "WEAP_results/", WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/", WEAP_input_filename, sep=""), skip = 5, header = TRUE, sep =',')
    
    #output a copy of the raw WEAP output into the SWITCH folder with the run being linked with the WEAP scenario
    
    write.csv(x = WEAP_data, file = paste(climate_scenario_directory, 'WEAP_results/', WEAP_input_filename, sep=""), row.names = FALSE)
    
    #read in WEAP Reference outputs
    WEAP_ref_input_filename <- paste("RefLOCA", "_Hydropower Generation",".csv", sep="")
    WEAP_data_ref <- read.table(paste(WEAP.output.dir, "WEAP_results/", WEAP_run_id,"/data/Results/SupplyDelivered_Hydropower_EnergyUse/",WEAP_ref_input_filename, sep=""), skip = 5, header = TRUE, sep =',')
    
    #output a copy of the raw WEAP output into the SWITCH folder with the run being linked with the WEAP scenario
    write.csv(x = WEAP_data_ref, file = paste(climate_scenario_directory, 'WEAP_results/', WEAP_ref_input_filename, sep=""), row.names = FALSE)
    
    #renaming some column names
    WEAP_data <- rename(WEAP_data, "Year" = "X.Columns...Year", "Month" = "Timestep")
    WEAP_data$Max.Gigawatt.Hour. <- NULL
    WEAP_data$Min.Gigawatt.Hour. <- NULL
    WEAP_data$Sum.Gigawatt.Hour. <- NULL
    WEAP_data$Median.Gigawatt.Hour. <- NULL
    WEAP_data$SD.Gigawatt.Hour. <- NULL
    WEAP_data$Mean.Gigawatt.Hour. <- NULL
    WEAP_data$RMS.Gigawatt.Hour. <- NULL
    
    #transpose to long format
    WEAP_data_long <- gather(WEAP_data, WEAP_generator_name_raw, Generation_GWh, 3:ncol(WEAP_data))
    
    #parse generator name from column
    #removing GWh unit label
    WEAP_generator_name <- str_replace(WEAP_data_long$WEAP_generator_name_raw, ".Gigawatt.Hour.","\\.")
    #replacing . with space and removing trailing space
    WEAP_generator_name1 <- str_trim(str_replace_all(WEAP_generator_name, "\\."," "), side = "right")
    #checking length
    WEAP_generator_name1 <- cbind(WEAP_generator_name1, str_length(WEAP_generator_name1))
    WEAP_generator_name1 <- as.data.frame(WEAP_generator_name1)
    colnames(WEAP_generator_name1) <- c("WEAP_generator_name", "length")
    WEAP_generator_name1$length <- NULL
    #binding corrected name and deleting old name
    WEAP_data_long <- cbind(WEAP_data_long, WEAP_generator_name1)
    WEAP_data_long$WEAP_generator_name_raw <- NULL
    #specifying WEAP scenario name
    WEAP_data_long$WEAP_scenario <- WEAP_scenario
    
    #dropping early years for spin up
    WEAP_data_long <- WEAP_data_long %>% filter(Year > 2024 & Year < 2056)
     
    ######CALCULATING "CC ANOMOLY FACTORS" AS CLIMATE CHANGE IMPACT COMPARED TO REFERENCE SCENARIO
    
    #calculate rolling average of monthly generation +/- 5 years to have 11-year window average. Then only filter out the midpoint years which are going to be the sampled investment periods
    #list of generators
    generator_list <- list(unique(WEAP_data_long$WEAP_generator_name))
    
    WEAP_hydro_month_avg_CC <- data.frame()
    
    for (g in 1:length(generator_list[[1]])) {
      
      # g=2
      #filter out the data for each generator
      WEAP_generation_by_generator <- WEAP_data_long %>% filter(WEAP_data_long$WEAP_generator_name == generator_list[[1]][g])
      # #calculate 10-yr moving window average for each month of year
      w = 120
      
      for (j in seq_len(length(WEAP_generation_by_generator$Generation_GWh))) {
        #creates a sequence of indices of monthly observations from minus 5 years to plus 5 years with each annual observation
         ind <- seq((j - floor(w / 2)), (j + floor(w / 2)), by=12)
        #removes indices that are negative and greater than the length of the column
        ind <- ind[ind>0]
        ind <- ind[ind<=length(WEAP_generation_by_generator$Generation_GWh)]
         #average the generation for each month of year across the years of the averaging window
        WEAP_generation_by_generator$CC_Avg_Generation_GWh[j] <- mean(WEAP_generation_by_generator$Generation_GWh[ind]) 
        
        
      }
      #stacking the monthly averages of the individual generators
      WEAP_hydro_month_avg_CC <- rbind(WEAP_hydro_month_avg_CC, WEAP_generation_by_generator)  
      
    }
    
    #selecting the years that are sampled for the investment periods
    WEAP_hydro_month_avg_CC <- rename(WEAP_hydro_month_avg_CC, period = Year)

    WEAP_hydro_month_avg_CC <- WEAP_hydro_month_avg_CC %>% filter(WEAP_hydro_month_avg_CC$period %in% c(2025, 2030, 2035, 2040, 2045, 2050, 2055))
    
    #dropping the point estimates of monthly generation for the particular year to only keep the average
    
    WEAP_hydro_month_avg_CC$Generation_GWh <- NULL
    # #Calculating the average monthly generation for each generator for each period of the CC scenarios
    
    # WEAP_hydro_month_avg_CC <- WEAP_data_long %>% group_by(WEAP_scenario, WEAP_generator_name, period, Month) %>% summarize(CC_Avg_Generation_GWh = mean(Generation_GWh))
    
    #joining the monthly average generation in the reference scenario with the average monthly generation for each period of the CC scnearios
    WEAP_hydro_ratio_CC <- left_join(WEAP_hydro_month_avg_CC, WEAP_ref_month_avg, c("WEAP_generator_name" = "WEAP_generator_name", "Month" = "Month"))
    
    #calculate the climate change ratio as the ratio of the Climate change monthly period average generation divided by the monthly average generation from the historical reference scenario           
    WEAP_hydro_ratio_CC$CC_ratio <- WEAP_hydro_ratio_CC$CC_Avg_Generation_GWh / WEAP_hydro_ratio_CC$Reference_Avg_Generation_GWh
    
    #changing the ratios to 1 if Nan (both histoical generation is 0)
    WEAP_hydro_ratio_CC$CC_ratio <- ifelse(is.nan(WEAP_hydro_ratio_CC$CC_ratio), 1, WEAP_hydro_ratio_CC$CC_ratio)
    #changing the ratios to 1 if Inf (historical generation is 0)
    WEAP_hydro_ratio_CC$CC_ratio <- ifelse(is.infinite(WEAP_hydro_ratio_CC$CC_ratio), 1, WEAP_hydro_ratio_CC$CC_ratio)
    
    #join WEAP_SWITCH mapping with WEAP CC ratios
    WEAP_SWITCH_hydro_ratio_CC <- left_join(WEAP_hydro_ratio_CC, WEAP_SWITCH_hydro_mapping, c("WEAP_generator_name" = "WEAP_name"))
    
    #removing NAs (generators that don't have a matching SWITCH generator)
    WEAP_SWITCH_hydro_ratio_CC <- WEAP_SWITCH_hydro_ratio_CC[complete.cases(WEAP_SWITCH_hydro_ratio_CC[ , 9]), ]
    
    #removing pumped storage from list because not fully characterized in WEAP
    WEAP_SWITCH_hydro_ratio_CC <- WEAP_SWITCH_hydro_ratio_CC %>% filter(gen_tech == "Hydro_NonPumped")
    
    #removing the generators that aren't generating in WEAP
    WEAP_SWITCH_hydro_ratio_CC <- WEAP_SWITCH_hydro_ratio_CC %>% filter(not_generating_in_WEAP == 0)
    
    #writing csv of WEAP CC ratios that match SWITCH generators and are not pumped storage
    write.csv(x = WEAP_SWITCH_hydro_ratio_CC, file = paste(climate_scenario_directory, 'WEAP_results/', "CC_Ratios_of_",WEAP_scenario,"to_Ref.csv", sep=""), row.names = FALSE)
    
    #removing extra columns and write to csv for QC
    
    WEAP_SWITCH_hydro_ratio_CC_QC <- WEAP_SWITCH_hydro_ratio_CC %>% dplyr::select(WEAP_scenario, WEAP_generator_name, SWITCH_name, hydro_project, load_zone_id, load_zone_name, period, Month, CC_ratio, CC_Avg_Generation_GWh, Reference_Avg_Generation_GWh, capacity_limit_mw, not_generating_in_WEAP)
    
    write.csv(x = WEAP_SWITCH_hydro_ratio_CC_QC, file = paste(climate_scenario_directory, 'WEAP_results/', "CC_Ratios_by_matching_SWITCH_generator_of_",WEAP_scenario,"to_RefTEST.csv", sep=""), row.names = FALSE)
    
    return(WEAP_SWITCH_hydro_ratio_CC)
    
  }


  WEAP_SWITCH_hydro_ratio_CC <- calculate_WEAP_CC_ref_ratios(WEAP_scenario = WEAP_scenario)

########AVERAGE CC FACTOR PER SWITCH LOAD ZONE AND MONTH AND period

  WEAP_CC_load_zone_ratios <- function(WEAP_input_df){
    
    #calculating the average monthly CC ratios by load zone, to use for adjusting hydropower generators not modeled in WEAP but that are in SWITCH (<30 MW)
    WEAP_loadzone_avg_ratio_CC <- WEAP_input_df %>% group_by(period, Month, load_zone_name) %>% summarize(CC_ratio_lz_avg = mean(CC_ratio, na.rm = TRUE))
    
    #write file with average load zone CC ratios for inspection and QC
    write.csv(x = WEAP_loadzone_avg_ratio_CC, file = paste(climate_scenario_directory, 'WEAP_results/', "CC_Ratios_by_SWITCH_loadzone_of_",WEAP_scenario,"to_Ref.csv", sep=""), row.names = FALSE)
    
    #plotting ratios for each period and load zone to spot outliers
    png(paste(climate_scenario_directory, 'WEAP_results/', "Facet_Plot_CC_Ratios_by_SWITCH_loadzone_of_",WEAP_scenario,".png",sep=""), width=2250, height=1000, res=100)
    
    plot1 <- ggplot(data = WEAP_loadzone_avg_ratio_CC, aes(x=factor(Month), y = CC_ratio_lz_avg, group = period, color = period)) + geom_line() +
      ggtitle(paste("Average by SWITCH load zone of monthly ratio of hydropower generation ",WEAP_scenario," scenario / avg. hydropower generation WEAP Reference scenario", sep="")) +
      ylab("Ratio WEAP CC generation/Reference generation") +
      xlab("Month") +
      facet_wrap(~load_zone_name) +
      geom_hline(yintercept = 1, color = "red")
    
    print(plot1)
    dev.off()
    
    #plotting ratios for each period and load zone to spot outliers, limiting ratios to 5
    png(paste(climate_scenario_directory, 'WEAP_results/', "Facet_Plot_CC_Ratios_limit_5_by_SWITCH_loadzone_of_",WEAP_scenario,".png",sep=""), width=2250, height=1000, res=100)
    
    plot2<- ggplot(data = WEAP_loadzone_avg_ratio_CC, aes(x=factor(Month), y = CC_ratio_lz_avg, group = period, color = period)) + geom_line() +
      ggtitle(paste("Average by SWITCH load zone of monthly ratio of hydropower generation ",WEAP_scenario," scenario / avg. hydropower generation WEAP Reference scenario", sep="")) +
      ylab("Ratio WEAP CC generation/Reference generation") +
      xlab("Month") +
      facet_wrap(~load_zone_name) +
      geom_hline(yintercept = 1, color = "red")  +
      ylim(0,5)
    print(plot2)
    dev.off()
    
    return(WEAP_loadzone_avg_ratio_CC)
  }
  
  WEAP_SWITCH_loadzone_ratio_CC <- WEAP_CC_load_zone_ratios(WEAP_input_df = WEAP_SWITCH_hydro_ratio_CC)
  
  ####READ IN SWITCH BASELINE (NO CLIMATE CHANGE) HYDROPOWER GENERATION TIMESERIES
  
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
  
  
  ####JOIN WEAP CC FACTORS WITH SWITCH BASELINE (NO CLIMATE CHANGE) HYDROPOWER GENERATION TIMESERIES TO CALCULATE NEW SWITCH TIMESERIES
  
  adjust_SWITCH_ref_with_WEAP_CC_factors <- function(WEAP_input_df, WEAP_load_zone_df){
    
    #removing extra columns and preparing to merge with SWITCH reference time series
    WEAP_SWITCH_hydro_ratio_CC2 <- WEAP_input_df %>% dplyr::select(WEAP_scenario, WEAP_generator_name, SWITCH_name, hydro_project, load_zone_id, load_zone_name, period, Month, CC_ratio, capacity_limit_mw)
    
    #read in SWITCH reference hydrotime series

    SWITCH_ref_run <- read_in_SWITCH_data(climate_scenario_directory = climate_scenario_directory, input_file = "hydro_timeseries.csv")
    
    SWITCH_ref_run$ts_period <- as.numeric(substr(SWITCH_ref_run$timeseries, 1, 4))
    
    SWITCH_ref_run$year <- as.numeric(substr(SWITCH_ref_run$timeseries, 1, 4))
    
    SWITCH_ref_run$month <- as.numeric(substr(SWITCH_ref_run$timeseries, 7, 8))
    
    SWITCH_WEAP_CC <- left_join(SWITCH_ref_run, WEAP_SWITCH_hydro_ratio_CC2, c("hydro_project" = "hydro_project", "ts_period" = "period", "month" = "Month"))
    
    #Reading in project list to get load zone id for non-matching hydropower generators (not modeled in WEAP)
    SWITCH_projects_info <- read_in_SWITCH_data(climate_scenario_directory = climate_scenario_directory, input_file = "generation_projects_info.csv")
    
    SWITCH_projects_info <- SWITCH_projects_info %>% dplyr::select(GENERATION_PROJECT, gen_load_zone, gen_tech, gen_capacity_limit_mw, gen_scheduled_outage_rate,	gen_forced_outage_rate)
    
    #join hydropower timeseries with project info to get load zone data for generators not modeled in WEAP (for eventual merging with load zone average CC ratios)
    SWITCH_WEAP_CC <- left_join(SWITCH_WEAP_CC, SWITCH_projects_info, c("hydro_project" = "GENERATION_PROJECT"))
    
    #Generators in these load zones do not have CC avrage ratios by load zone because there are no generators in those zones modeled in WEAP:
    #AZ_APS_N CA_LADWP   CA_PGE_S CA_SCE_VLY    CA_SDGE    CAN_ALB       CO_E    CO_NW   NV_N       UT_S      WY_SW 
    
    #So, we assign load zone averages from neighboring load zones with similar climates
    
    #read in mapping file that matches SWITCH load zones that do not have WEAP hydropower generators to load zone names with WEAP generators, so those load zone averages may be used instead
    nearest_lz_mapping <- read.csv(file = paste(WEAP.output.dir,"Hydropower_SWITCH_handshake/","WEAP_to_SWITCH_nearest_load_zone.csv", sep=""), stringsAsFactors = FALSE)
    
    #joining file that maps load zones where they are missing in WEAP to nearest neighboring zones
    SWITCH_WEAP_CC <- left_join(SWITCH_WEAP_CC, nearest_lz_mapping, c("gen_load_zone" = "Load_zone_with_no_WEAP_hydro"))
    
    SWITCH_WEAP_CC$load_zone_for_CC_avg <- SWITCH_WEAP_CC$Nearest_load_zone_with_WEAP_hydro
    
    SWITCH_WEAP_CC$load_zone_for_CC_avg <- ifelse(is.na(SWITCH_WEAP_CC$Nearest_load_zone_with_WEAP_hydro), SWITCH_WEAP_CC$gen_load_zone, SWITCH_WEAP_CC$load_zone_for_CC_avg)
    
    #join hydropower timeseries with average CC ratios by load zone
    SWITCH_WEAP_CC <- left_join(SWITCH_WEAP_CC, WEAP_load_zone_df, c("ts_period" = "period", "month" = "Month","load_zone_for_CC_avg" = "load_zone_name"))
    
    #If a CC ratio exists specific to the generator, use that, otherwise use the load zone average CC ratio
    SWITCH_WEAP_CC$CC_ratio_applied <- SWITCH_WEAP_CC$CC_ratio
    SWITCH_WEAP_CC$CC_ratio_applied <-ifelse(is.na(SWITCH_WEAP_CC$CC_ratio), SWITCH_WEAP_CC$CC_ratio_lz_avg, SWITCH_WEAP_CC$CC_ratio_applied)
    
    #multiply CC ratio by the avg and min SWITCH mw 
    SWITCH_WEAP_CC$hydro_avg_flow_mw_CC <- SWITCH_WEAP_CC$hydro_avg_flow_mw * SWITCH_WEAP_CC$CC_ratio_applied 
    
    SWITCH_WEAP_CC$hydro_min_flow_mw_CC <- SWITCH_WEAP_CC$hydro_min_flow_mw * SWITCH_WEAP_CC$CC_ratio_applied 
    
    #check if capacity factor greater than 1 - outage rate, if yes, make MW equal to gen_capacity limit * 1- outage rate for that month and do same for the minimum flow
    SWITCH_WEAP_CC$gen_capacity_limit_mw <- as.numeric(SWITCH_WEAP_CC$gen_capacity_limit_mw)
    
    SWITCH_WEAP_CC$check_CC_cf <- SWITCH_WEAP_CC$hydro_avg_flow_mw_CC / SWITCH_WEAP_CC$gen_capacity_limit_mw
    SWITCH_WEAP_CC$check_CC_cf_min <- SWITCH_WEAP_CC$hydro_min_flow_mw_CC / SWITCH_WEAP_CC$gen_capacity_limit_mw
    
    #output list of generators with cf > 1 for QC later
    #reading in specific table and range
    SWITCH_WEAP_CC_gt1 <- SWITCH_WEAP_CC %>% filter(SWITCH_WEAP_CC$check_CC_cf > 1)
    SWITCH_WEAP_CC_mingt1 <- SWITCH_WEAP_CC %>% filter(SWITCH_WEAP_CC$check_CC_cf_min > 1)
    
    project_directory <- paste(climate_scenario_directory, 'inputs/', sep="")
    write.csv(SWITCH_WEAP_CC_gt1, paste(project_directory,"Archive/","Hydro_generators_with_CC_cap_factors_gt_1.csv",sep=""), row.names=FALSE)
    write.csv(SWITCH_WEAP_CC_mingt1, paste(project_directory,"Archive/","Hydro_generators_with_CC_min_cap_factors_gt_1.csv",sep=""), row.names=FALSE)
    
    #if a generator already has a cap factor >1 or the cap factor becomes greater than 1 with the CC ratios, cap it at the max capacity limit
    
    for(j in 1:length(SWITCH_WEAP_CC$hydro_avg_flow_mw)) {        
    
      if ( SWITCH_WEAP_CC$hydro_avg_flow_mw[j]/SWITCH_WEAP_CC$gen_capacity_limit_mw[j] > (1 - (SWITCH_WEAP_CC$gen_scheduled_outage_rate[j] + SWITCH_WEAP_CC$gen_forced_outage_rate[j])) && SWITCH_WEAP_CC$hydro_avg_flow_mw_CC[j]/SWITCH_WEAP_CC$gen_capacity_limit_mw[j] > (1 - (SWITCH_WEAP_CC$gen_scheduled_outage_rate[j] + SWITCH_WEAP_CC$gen_forced_outage_rate[j]))) {
        
        SWITCH_WEAP_CC$hydro_avg_flow_mw_CC[j] <- (SWITCH_WEAP_CC$hydro_avg_flow_mw[j]) * (1 - (SWITCH_WEAP_CC$gen_scheduled_outage_rate[j] + SWITCH_WEAP_CC$gen_forced_outage_rate[j]))
        SWITCH_WEAP_CC$hydro_min_flow_mw_CC[j] <- (SWITCH_WEAP_CC$hydro_min_flow_mw[j]) * (1 - (SWITCH_WEAP_CC$gen_scheduled_outage_rate[j] + SWITCH_WEAP_CC$gen_forced_outage_rate[j]))
        
      } else if ( SWITCH_WEAP_CC$hydro_avg_flow_mw_CC[j]/SWITCH_WEAP_CC$gen_capacity_limit_mw[j] > (1 - (SWITCH_WEAP_CC$gen_scheduled_outage_rate[j] + SWITCH_WEAP_CC$gen_forced_outage_rate[j]))) {
        
        SWITCH_WEAP_CC$hydro_avg_flow_mw_CC[j] <- (SWITCH_WEAP_CC$gen_capacity_limit_mw[j]) * (1 - (SWITCH_WEAP_CC$gen_scheduled_outage_rate[j] + SWITCH_WEAP_CC$gen_forced_outage_rate[j]))
        SWITCH_WEAP_CC$hydro_min_flow_mw_CC[j] <- (SWITCH_WEAP_CC$gen_capacity_limit_mw[j]/2) * (1 - (SWITCH_WEAP_CC$gen_scheduled_outage_rate[j] + SWITCH_WEAP_CC$gen_forced_outage_rate[j]))
        
      } else {
        SWITCH_WEAP_CC$hydro_avg_flow_mw_CC[j] <- SWITCH_WEAP_CC$hydro_avg_flow_mw_CC[j]
        SWITCH_WEAP_CC$hydro_min_flow_mw_CC[j] <- SWITCH_WEAP_CC$hydro_min_flow_mw_CC[j]
      }
    }
    
    #remove extra columns and write new tab file of hydropower timeseries
    SWITCH_WEAP_CC_final <- SWITCH_WEAP_CC
    
    SWITCH_WEAP_CC_final$hydro_avg_flow_mw <- NULL 
    SWITCH_WEAP_CC_final$hydro_min_flow_mw <- NULL
    
    SWITCH_WEAP_CC_final <- rename(SWITCH_WEAP_CC_final, hydro_avg_flow_mw = hydro_avg_flow_mw_CC, hydro_min_flow_mw = hydro_min_flow_mw_CC)
    
    SWITCH_WEAP_CC_final <- SWITCH_WEAP_CC_final %>% dplyr::select(hydro_project, timeseries, hydro_min_flow_mw, hydro_avg_flow_mw)
    
    #Outputing the modified SWITCH output file for the run
    
    project_directory <- paste(climate_scenario_directory, 'inputs/', sep="")
    
    SWITCH_output_file <- paste(project_directory, "hydro_timeseries.csv", sep="")
    # SWITCH_output_file <- paste(project_directory, "hydro_timeseries_CC.tab", sep="")
    
    #reading in specific table and range
    write.csv(SWITCH_WEAP_CC_final, SWITCH_output_file, row.names=FALSE, quote = F)
    
    return(SWITCH_WEAP_CC)  
  
  }
  
  SWITCH_WEAP_CC_final_CC <- adjust_SWITCH_ref_with_WEAP_CC_factors(WEAP_input_df = WEAP_SWITCH_hydro_ratio_CC, WEAP_load_zone_df = WEAP_SWITCH_loadzone_ratio_CC)
  
  ####CALCULATE RESERVE CAPACITY VALUE BASED ON CLIMATE CHANGE-ADJUSTED MONTHLY HYDROPOWER GENERERATION

  calculate_CC_reserve_cap_value <- function(WEAP_scenario){
    
    #making a copy of the reserves file before it gets modified for these cc factors
    reserve_capacity_value_orig <- read_in_SWITCH_data(climate_scenario_directory = climate_scenario_directory, input_file = "reserve_capacity_value.csv")
    
    #read in CC hydro timeseries
    hydro_timeseries <- read_in_SWITCH_data(climate_scenario_directory = climate_scenario_directory, input_file = "hydro_timeseries.csv")
    
    #read in gen project list
    generation_plant <- read_in_SWITCH_data(climate_scenario_directory = climate_scenario_directory, input_file = "generation_projects_info.csv")
    
    #join gen plant with hydro timeseries
    gen_plant_hydro <- left_join(hydro_timeseries, generation_plant %>% dplyr::select(GENERATION_PROJECT, gen_tech, gen_capacity_limit_mw, gen_energy_source, gen_scheduled_outage_rate, gen_forced_outage_rate) , by = c("hydro_project"="GENERATION_PROJECT"))
    
    #parse date
    
    gen_plant_hydro$year <- as.numeric(substr(gen_plant_hydro$timeseries, 1, 4))
    gen_plant_hydro$month <- as.numeric(substr(gen_plant_hydro$timeseries, 7, 8))
    
    
    #calc monthly cap factor
    gen_plant_hydro$gen_capacity_limit_mw <- as.numeric(gen_plant_hydro$gen_capacity_limit_mw)
    
    gen_plant_hydro$monthly_avg_cf <- (gen_plant_hydro$hydro_avg_flow_mw / gen_plant_hydro$gen_capacity_limit_mw) 
    # gen_plant_hydro$monthly_avg_cf <- (gen_plant_hydro$hydro_avg_flow_mw / gen_plant_hydro$gen_capacity_limit_mw) * (1 - (gen_plant_hydro$gen_scheduled_outage_rate + gen_plant_hydro$gen_forced_outage_rate))
    
    #remove duplicates, so there is just one row per generator and per month
    gen_plant_hydro_nodup <- gen_plant_hydro %>% group_by(hydro_project, year, month) %>% summarize(monthly_avg_cf = mean(monthly_avg_cf))
    
    #read in loads
    loads <- read_in_SWITCH_data(climate_scenario_directory = climate_scenario_directory, input_file = "loads.csv")
      
    #read in tps
    timepoints <- read_in_SWITCH_data(climate_scenario_directory = climate_scenario_directory, input_file = "timepoints.csv")
      
    #join with hourly tp, by month
    loads_tp <- left_join(loads, timepoints, c("TIMEPOINT"="timepoint_id"))
    
    #removing duplicates so just have one set of sampled time points (not by load zone)
    loads_tp_no_dup <-  loads_tp[!duplicated(loads_tp[c(2,4,5)]),]
    
    sampled_timepoints_forloads <- loads_tp_no_dup %>% dplyr::select(TIMEPOINT, timestamp, timeseries)
    
    sampled_timepoints_forloads$month <- as.numeric(substr(sampled_timepoints_forloads$timeseries, 11, 12))
    sampled_timepoints_forloads$year <- as.numeric(substr(sampled_timepoints_forloads$timeseries, 6, 9))
    
    #output ratios
    gen_plant_hydro_tp <- left_join(sampled_timepoints_forloads, gen_plant_hydro_nodup, c("month" = "month", "year"="year"))
    
    gen_plant_hydro_tp <- rename(gen_plant_hydro_tp, "GENERATION_PROJECT" = "hydro_project", "timepoint" = "TIMEPOINT", "gen_capacity_value" = "monthly_avg_cf")
    
    #output hydro capacity value
    reserve_capacity_value <- gen_plant_hydro_tp %>% dplyr::select(GENERATION_PROJECT, timepoint, gen_capacity_value)
    
    project_directory <- paste(climate_scenario_directory, 'inputs/', sep="")
    
    SWITCH_output_file <- paste(project_directory, "reserve_capacity_value.csv", sep="")
    
    #writing updated file
    write.csv(reserve_capacity_value, SWITCH_output_file, row.names=FALSE, quote = F)
    
    return(reserve_capacity_value)
  }
  
  CC_reserve_capacity_value_CC <- calculate_CC_reserve_cap_value(WEAP_scenario = WEAP_scenario)
  
}

