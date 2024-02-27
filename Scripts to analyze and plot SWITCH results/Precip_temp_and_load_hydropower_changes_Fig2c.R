#Script to calculate the change in temperature and change in precipitation by sub-region of the WECC from 15 climate scenarios compared to historical climate

#This script supports the analysis of the manuscript submitted to Nature Communications on climate impacts and adaptation of the WECC grid. It does the following: 

# 1. get list of climate input file names used by the WEAP model from all the catchments and parse the catchment name from the file name
# 2. read in all the files
# 3. create an aggregate average temp column and a total precip column across all elevations in each catchment
# 4. add a column that categorizes the catchment into a climate region, based on the state in the catchment name
# 5. rbind all the aggregated temp and precip data and the climate region across the catchments
# 6. Calculate WECC wide average temp and total precip for each year
# 7. repeat for historical data
# 8. join historical period average with annual average for each climate scenario and calculate the delta for each year
# 9. read in the hydropower and energy demand results for each climate scenario from the SWITCH outputs
# 10. create the same aggregations of the hydropower and energy demand data by sub-region as the precip and temp data
# 11. create Figure 2C as scatter plots by sub-region for 2050 average change in precip and temperatures compared to change in hydropower, cooling/heating load, and water load


library(tidyverse)
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(httr)
library(jsonlite)
library(rgdal)
library(sp)
library(sf)
library(ggrepel)
library(cowplot)
library(ggpubr)

rm(list=ls())  #clear variables in R space
#directory for inputs

#replace file path here:
dat.dir <- ""
setwd(dat.dir)

#directories for outputs

#replace file path here:
switch_output_dir <- ""
switch_run_dir <- "SWITCH_WEAP_runs/"
results_dir <- "WEAP_Nov_8_results_SWITCH_Mar_27_results/"
climate_impacts_included <- "Cooling + Hydropower and Water Load Scenarios"
weap_run_dir <- paste(results_dir, climate_impacts_included,"/", sep="")

#climate scenarios
climate_scenario_list <- c("ACCESS_1-0","bcc-csm1-1","CanESM","CCSM","CESM1-BGC","CESM1-CAM5","CMCC-CM", "CMCC-CMS", "CNRM-CM5","GFDL-CM3", "GFDL-ESM2M" ,"HadGEM2-CC", "HadGEM2-ES","MIROC5", "MPI-ESM-LR", "Hist")

#reading in the climate data that was used as inputs into WEAP
monthly_TavgPrecip_all_Scenarios <- data.frame()

for (i in 1:length(climate_scenario_list)){
  # i=1
  Scenario <- climate_scenario_list[i]
  
  print(Scenario)
  
  ############################################
  Files     <- list.files(path=paste0("./",Scenario))
  FilesLong <- list.files(path=paste0("./",Scenario),full.names=TRUE)
  
  #specifying the number of lines to skip in the input climate data files (climate scenarios have slightly different file format than the historical scenario)
  if(Scenario == "Hist"){
    SkipNum1 = 16
    SkipNum2 = 11
  }else{
    SkipNum1 = 15
    SkipNum2 = 10
  }
  
  #extract list of catchment names for each scenario
  Catchment_num_name <- str_split_fixed(Files, " - ", 2)
  Catchment_names <- as.data.frame(str_split_fixed(Catchment_num_name[,2], ".csv", 2))
  Catchment_names$V2 <- NULL
  Catchment_names[c('State','blank')] <- str_split_fixed(Catchment_names$V1, "_", 2)
  Catchment_names$blank <- NULL
  Catchment_names<- cbind(Catchment_names, Catchment_num_name[,1])
  colnames(Catchment_names) <- c("Catchment_name","State","Catchment_num")
  
  #designating each catchment into a region (SW, PacNW, MT, CAN) based on the State in the catchment name
  Catchment_names$Region <- "SW"
  Catchment_names$Region <- ifelse(Catchment_names$State %in% c("CO","UT","ID","WY","MT"),"MT", Catchment_names$Region)
  Catchment_names$Region <- ifelse(Catchment_names$State %in% c("WA","OR"),"PacNW", Catchment_names$Region)
  Catchment_names$Region <- ifelse(Catchment_names$State %in% c("CAN"),"CAN", Catchment_names$Region)
  
  monthly_TavgPrecip_all_catchments <- data.frame()
  
  for (f in 1:length(Files)) {
    
    print(FilesLong[f])
    
    #reading in the climate data
    climate_data = read.table(FilesLong[f], skip=SkipNum1, header = TRUE, sep=",")
    climate_data <- rename(climate_data,Year=X.Columns...Year)
    climate_data_long <- gather(climate_data, variable, C_or_mm, 3:ncol(climate_data))
    #split the column into the variable name and elevation
    climate_data_long[c('Var_type','Elevation')] <- str_split_fixed(climate_data_long$variable, "..Elevation.", 2)
    
    #extracting the area of each elevation band from the input file for area-weighting precip
    area_data_raw = read.csv(FilesLong[f], skip=SkipNum2, header = FALSE, sep=",")
    area_data_raw <- area_data_raw[1,]
    area_data <- as.data.frame(paste(area_data_raw, collapse=" "))
    colnames(area_data) <- c('area_data_raw')
    area_split0 <- str_split_fixed(area_data$area_data_raw, ":",2)
    
    #extracting the area of each elevation band of the catchment, in hectares
    area_split1 <- str_extract_all(area_split0[1,2],  "(?<=\\().+?(?=\\))")[[1]]
    area_ha <- str_split_fixed(area_split1," ha",2) #trimming "ha" from string
    area_ha[,1] <- str_replace_all(area_ha[,1], fixed(" "), "") #trimming space from string
    area_ha <- area_ha[,1]
    area_ha <- as.data.frame(area_ha)
    area_ha$area_ha <- as.numeric(area_ha$area_ha)
    
    #extracting the elevation band of each catchment
    area_split2 <- str_split_fixed(area_split0[1,2],  " m ", str_count(area_split0[1,2], " m "))
    area_elev<- as.data.frame(as.numeric(str_extract_all(area_split0[1,2], "\\d{4}(?= m)")[[1]]))
    colnames(area_elev) <- c('elevation_upper_limit')
    area_elev$elevation_lower_limit <- area_elev$elevation_upper_limit - 1000
    area_elev$Elevation_band <- paste(area_elev$elevation_lower_limit, "to", area_elev$elevation_upper_limit, "m", sep=".")
    #joining the area of each elevation band with the elevation bands and calculating total catchment area in km2
    area_elev_ha <- cbind(area_elev, area_ha) %>% mutate(total_catchment_area_km2 = sum(area_ha)*0.01)
    
    #filtering out just the Temp data for averaging across all elevation bands in the catchment
    T_data_long <- climate_data_long %>% filter(Var_type %in% c("tavg.C","Tavg.C"))
    Tavg_data_long <- T_data_long %>% group_by(Year, Month) %>% summarize(tavg.C =mean(C_or_mm))
    
    #filtering out just the precip data
    Precip_data_long <- climate_data_long %>% filter(Var_type == "Prcp.mm")
    #join precip data with the area of each elevation band, and multiply the depth * area to get the total volumetric precip for each elevation band
    Precip_data_long <- left_join(Precip_data_long, area_elev_ha, c("Elevation"="Elevation_band"))
    Precip_data_long$Precip_km3 <- Precip_data_long$C_or_mm * 10^(-6) * Precip_data_long$area_ha * 0.01
    #summing the volumetric precip across all elevation bands of the catchment
    Precip_total_data_long <- Precip_data_long %>% group_by(Year, Month, total_catchment_area_km2) %>% summarize(Precip_km3 = sum(Precip_km3))
    
    Tavg_Precip_total_long <- left_join(Tavg_data_long, Precip_total_data_long, c("Year"="Year","Month"="Month"))
    
    #Calculate rolling average of precip and temperature for each month, with a 10 year window (+/- 5 years) around each month
    # #calculate 10-yr moving window average for each day of year
    if(Scenario != "Hist"){
      
      #subseting the date range
      Tavg_Precip_total_long <- Tavg_Precip_total_long %>% filter(Year >= 2010 & Year <= 2060)
      
      #window of 10 years * 12 months
      w = 120
      
      Tavg_Precip_total_long$avg_Precip_km3 <- Tavg_Precip_total_long$Precip_km3
      Tavg_Precip_total_long$avg_tavg.C <- Tavg_Precip_total_long$tavg.C
      
      for (j in seq_len(length(Tavg_Precip_total_long$Precip_km3))) {
        #creates a sequence of indices of monthly observations from minus 5 years to plus 5 years with each annual observation
        ind <- seq((j - floor(w / 2)), (j + floor(w / 2)), by=12)
        
        #removes indices that are negative and greater than the length of the column
        ind <- ind[ind>0]
        
        ind <- ind[ind<length(Tavg_Precip_total_long$Precip_km3)]
        
        #average the total monthly Precip of each catchment across the years of the averaging window
        Tavg_Precip_total_long$avg_Precip_km3[j] <- mean(Tavg_Precip_total_long$Precip_km3[ind]) 
        
      }
      
      for (j in seq_len(length(Tavg_Precip_total_long$tavg.C))) {
        #creates a sequence of indices of monthly observations from minus 5 years to plus 5 years with each annual observation
        ind <- seq((j - floor(w / 2)), (j + floor(w / 2)), by=12)
        
        #removes indices that are negative and greater than the length of the column
        ind <- ind[ind>0]
        
        ind <- ind[ind<length(Tavg_Precip_total_long$tavg.C)]
        
        #average the total monthly Precip of each catchment across the years of the averaging window
        Tavg_Precip_total_long$avg_tavg.C[j] <- mean(Tavg_Precip_total_long$tavg.C[ind]) 
        
      }
    }else if(Scenario == "Hist"){
      
      #subseting the date range
      Tavg_Precip_total_long <- Tavg_Precip_total_long %>% filter(Year >= 1980 & Year <= 2010)
      #not averaging the historical data
      Tavg_Precip_total_long$avg_Precip_km3 <- Tavg_Precip_total_long$Precip_km3
      Tavg_Precip_total_long$avg_tavg.C <- Tavg_Precip_total_long$tavg.C  
      
    }
   
   #labeling the catchments 
    Tavg_Precip_total_long$Catchment_num <- Catchment_names$Catchment_num[f]
    Tavg_Precip_total_long$Catchment_name <- Catchment_names$Catchment_name[f]
    Tavg_Precip_total_long$State <- Catchment_names$State[f]
    Tavg_Precip_total_long$Region <- Catchment_names$Region[f]
    
  #adding the data to the other catchments
    monthly_TavgPrecip_all_catchments <- rbind(monthly_TavgPrecip_all_catchments, Tavg_Precip_total_long)
    
  } 
  
  monthly_TavgPrecip_all_catchments$Scenario <- Scenario
  
  #adding the data to the other scenarios
  monthly_TavgPrecip_all_Scenarios <- rbind(monthly_TavgPrecip_all_Scenarios, monthly_TavgPrecip_all_catchments)
}


write.csv(x = monthly_TavgPrecip_all_Scenarios, file = paste(switch_output_dir, weap_run_dir,climate_impacts_included, "monthly_TavgPrecip_all_Scenarios.csv",sep=""), row.names = F)
#########
#skip to here if already processed data above
monthly_TavgPrecip_all_Scenarios <- read.csv(file =  paste(switch_output_dir, weap_run_dir,climate_impacts_included, "monthly_TavgPrecip_all_Scenarios.csv",sep=""), header = TRUE)

#separate out the climate scenarios data from the historical data
monthly_TavgPrecip_CC_Scenarios <- monthly_TavgPrecip_all_Scenarios %>% filter(Scenario != "Hist")
monthly_TavgPrecip_Hist <- monthly_TavgPrecip_all_Scenarios %>% filter(Scenario == "Hist")

#calculate monthly average temp and precip for historical period 1980 - 2010
monthly_TavgPrecip_Hist_period <- monthly_TavgPrecip_Hist %>% group_by(Catchment_num, Catchment_name, State, Region, total_catchment_area_km2, Month) %>% summarize(hist_avg_tavg.C = mean(avg_tavg.C), 
                                                                                                                                                                    hist_avg_Precip_km3=mean(avg_Precip_km3))
#calculate annual average historical temp and annual average precip
annual_TavgPrecip_Hist_period <- monthly_TavgPrecip_Hist_period %>% group_by(Catchment_num, Catchment_name, State, Region, total_catchment_area_km2) %>% summarize(annual_hist_avg_tavg.C = mean(hist_avg_tavg.C), 
                                                                                                                                                                    annual_hist_avg_Precip_km3=sum(hist_avg_Precip_km3))

#calculate annual average CC temp and annual average precip
annual_TavgPrecip_CC_period <- monthly_TavgPrecip_CC_Scenarios %>% group_by(Scenario, Catchment_num, Catchment_name, State, Region, total_catchment_area_km2, Year) %>% summarize(annual_avg_tavg.C = mean(avg_tavg.C), 
                                                                                                                                                                   annual_total_Precip_km3=sum(avg_Precip_km3))

#join annual historical averages with annual CC scenario averages for temp and precip
annual_CC_hist_TavgPrecip <- left_join(annual_TavgPrecip_CC_period, annual_TavgPrecip_Hist_period, c("Catchment_num"="Catchment_num", "Catchment_name" = "Catchment_name", 
"State"="State", "Region"="Region" , "total_catchment_area_km2"="total_catchment_area_km2"))

#calculate delta in annual temp and precip by catchment
annual_CC_hist_TavgPrecip$delta_annual_avg_tavg.C <- annual_CC_hist_TavgPrecip$annual_avg_tavg.C - annual_CC_hist_TavgPrecip$annual_hist_avg_tavg.C
annual_CC_hist_TavgPrecip$delta_annual_total_Precip_km3 <- annual_CC_hist_TavgPrecip$annual_total_Precip_km3 - annual_CC_hist_TavgPrecip$annual_hist_avg_Precip_km3

#aggregate delta annual temp and precip by region
Region_annual_CC_hist_TavgPrecip <- annual_CC_hist_TavgPrecip %>% group_by(Scenario, Year, Region)%>% summarize(total_region_area_km2=sum(total_catchment_area_km2),
                                                                                                                delta_annual_avg_tavg.C = mean(delta_annual_avg_tavg.C), 
                                                                                                                delta_annual_total_Precip_km3=sum(delta_annual_total_Precip_km3),
                                                                                                                annual_avg_tavg.C = mean(annual_avg_tavg.C),
                                                                                                                annual_hist_avg_tavg.C= mean(annual_hist_avg_tavg.C),
                                                                                                                annual_total_Precip_km3=sum(annual_total_Precip_km3),
                                                                                                                annual_hist_avg_Precip_km3=sum(annual_hist_avg_Precip_km3))
##Regional analysis

#read in the delta load and hydropower generation data from SWITCH results

#plot Regional relationships for 2050 for all climate scenarios

Lz_annual_hydro_load_deltas <- read.csv(file=paste(switch_output_dir,weap_run_dir, "Cooling + Hydropower and Water Load ScenariosDELTA_lz_hydro_load_disagg.csv", sep=""), header = TRUE)

#filter data to just 2050
Lz_2050_CC_hist_TavgPrecip <- Region_annual_CC_hist_TavgPrecip %>% filter(Year == 2050)
Lz_2050_hydro_load_deltas <- Lz_annual_hydro_load_deltas %>% filter(ts_period == 2050)

#Categorize SWITCH load zones into Regions
Lz_2050_hydro_load_deltas$State <- str_split_fixed(str_sub(Lz_2050_hydro_load_deltas$load_zone, 1,3),"_",2)[,1]

#designating each State into a region (SW, PacNW, MT, CAN) 
Lz_2050_hydro_load_deltas$Region <- "SW"
Lz_2050_hydro_load_deltas$Region <- ifelse(Lz_2050_hydro_load_deltas$State %in% c("CO","UT","ID","WY","MT"),"MT", Lz_2050_hydro_load_deltas$Region)
Lz_2050_hydro_load_deltas$Region <- ifelse(Lz_2050_hydro_load_deltas$State %in% c("WA","OR"),"PacNW", Lz_2050_hydro_load_deltas$Region)
Lz_2050_hydro_load_deltas$Region <- ifelse(Lz_2050_hydro_load_deltas$State %in% c("CAN"),"CAN", Lz_2050_hydro_load_deltas$Region)

Agg_Lz_2050_hydro_load_deltas <- Lz_2050_hydro_load_deltas %>% 
  group_by(ts_period, energy_category, Scenario, Baseline_Scenario, climate_impacts_included, energy_category_disagg, Region) %>%
  summarize(Energy_GWh_typical_yr = sum(Energy_GWh_typical_yr), Energy_GWh_typical_yr_Baseline = sum(Energy_GWh_typical_yr_Baseline), 
            Delta_AnnualGen_GWh = sum(Delta_AnnualGen_GWh))

#join SWITCH data with delta climate data
Lz_2050_CC_hist_TavgPrecip$Scenario <- ifelse(Lz_2050_CC_hist_TavgPrecip$Scenario == "ACCESS_1-0", "ACCESS-1.0", Lz_2050_CC_hist_TavgPrecip$Scenario)
Lz_2050_climate_hydro_load_deltas <- left_join(Agg_Lz_2050_hydro_load_deltas, Lz_2050_CC_hist_TavgPrecip,  c("Scenario"="Scenario", "ts_period"="Year", "Region"="Region"))

#linear regression to calculate R2
fitted_models_temp_region = Lz_2050_climate_hydro_load_deltas %>% group_by(energy_category_disagg, Region) %>% do(model = lm(Delta_AnnualGen_GWh ~ delta_annual_avg_tavg.C, data = .))
fitted_models_precip_region = Lz_2050_climate_hydro_load_deltas %>% group_by( energy_category_disagg, Region) %>% do(model = lm(Delta_AnnualGen_GWh ~ delta_annual_total_Precip_km3, data = .))
fitted_models_temp_region$R2_temp <- sapply(fitted_models_temp_region$model,function(x) summary(x)$r.squared)
fitted_models_precip_region$R2_precip <- sapply(fitted_models_precip_region$model,function(x) summary(x)$r.squared)

Lz_2050_climate_hydro_load_deltas <- left_join(Lz_2050_climate_hydro_load_deltas, fitted_models_temp_region, c("energy_category_disagg"="energy_category_disagg", "Region"="Region"))
Lz_2050_climate_hydro_load_deltas <- left_join(Lz_2050_climate_hydro_load_deltas, fitted_models_precip_region, c("energy_category_disagg"="energy_category_disagg", "Region"="Region"))

#reordering factors
Lz_2050_climate_hydro_load_deltas$energy_category_disagg <- ifelse(Lz_2050_climate_hydro_load_deltas$energy_category_disagg == "Water", "Hydropower", Lz_2050_climate_hydro_load_deltas$energy_category_disagg)
Lz_2050_climate_hydro_load_deltas$energy_category_disagg <- ifelse(Lz_2050_climate_hydro_load_deltas$energy_category_disagg == "Cooling Load", "Cooling/Heating Load", Lz_2050_climate_hydro_load_deltas$energy_category_disagg)
Lz_2050_climate_hydro_load_deltas$energy_category_disagg <- factor(Lz_2050_climate_hydro_load_deltas$energy_category_disagg, levels = c("Cooling/Heating Load","Water Load", "Hydropower"))

#plot regional relationships for 2050 for all climate scenarios
#regional disaggration of hydropower and load changes compared to temp and precip changes by scenario, 2050
cbpal <- c("#999999","#004949","#009292","#ff6db6","#ffb6db",
           "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
           "#920000","#924900","#db6d00","#24ff24","#ffff6d")

Lz_2050_climate_hydro_load_deltas$Scenario <- as.factor(Lz_2050_climate_hydro_load_deltas$Scenario)

library(ggpmisc)
library(ggtext)

#FIGURE 2

# Lz_2050_climate_hydro_load_deltas2 <- Lz_2050_climate_hydro_load_deltas %>% group_by(Scenario, energy_category_disagg) %>% nest() %>% mutate(Mod = map(data, ~lm(Delta_AnnualGen_GWh ~ delta_annual_avg_tavg.C, data = .x))) %>% mutate(R2 = map_dbl(Mod, ~round(summary(.x)$r.squared, 3))) 

plot_file_name <- paste("/",climate_impacts_included,"/","Fig2C_facet_plot","freescalesClimate_Regional_DELTA_2050_load_hydro_", climate_impacts_included,".png", sep="")

png(paste(switch_output_dir, results_dir, plot_file_name, sep=""), width=2650, height=2200, res=100)

plot1 <-ggplot(data = subset(Lz_2050_climate_hydro_load_deltas, Region != "CAN"), 
               aes(x = delta_annual_avg_tavg.C, y = Delta_AnnualGen_GWh/1000, group=factor(Scenario), color=factor(Scenario))) + 
  geom_point(size=5) + 
  geom_smooth(aes(group=1), method=lm,  se=FALSE, formula = y ~ x) +  # Add linear regression lines
  geom_richtext(color = "black",aes(x=3.4, y= -Inf, group = Region, label = paste("R<sup>2</sup> = ", round(R2_temp,3),sep="")), vjust = -.3, size=7) +
  labs(x=expression("Delta temperature ("*degree*C*")")) + labs(y="Delta energy (TWh)")  +
  ggtitle(paste("Regional changes in 2050 load and hydropower vs. change in 2050 avg. temperature", sep=", ")) +
  scale_color_manual(values = cbpal, name = "Scenario") +
  geom_hline(yintercept = 0) +
  facet_wrap(Region ~ energy_category_disagg, scales="free_y") +
  theme(axis.text.y = element_text(size=28), axis.title.x = element_text(size=30), axis.text.x = element_text(size=28), axis.title.y=element_text(size=30),
        legend.text = element_text(size = 28, nrow(2)), legend.title = element_text(size=30), legend.position="none", plot.title = element_text(size=32,hjust=0.5),
        strip.text.x = element_text(size = 30), strip.text.y = element_text(size = 30), plot.margin = margin(10, 20, 20, 10))

plot2 <-ggplot(data = subset(Lz_2050_climate_hydro_load_deltas, Region != "CAN"), 
               aes(x = delta_annual_total_Precip_km3, y = Delta_AnnualGen_GWh/1000, group=factor(Scenario), color=factor(Scenario))) + 
  geom_point(size=5) + 
  geom_smooth(aes(group=1),method=lm,  se=FALSE, formula = y ~ x) +  # Add linear regression lines
  geom_richtext(color = "black",
                aes(x=80, y = -Inf, group = Region, label = paste("R<sup>2</sup> = ", round(R2_precip,3),sep="")), vjust = -.3, size=7) +
  labs(x=expression(paste("Delta precipitation (", km^{3},")",sep=""))) + labs(y="Delta energy (TWh)")  +
  ggtitle(paste("Regional changes in 2050 load and hydropower vs. change in 2050 total precipitation", sep=", ")) +
  scale_color_manual(values = cbpal, name = "Scenario") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  facet_wrap(Region ~ energy_category_disagg, scales="free_y") +
  theme(axis.text.y = element_text(size=28), axis.title.x = element_text(size=30), axis.text.x = element_text(size=28), axis.title.y=element_text(size=30),
        legend.text = element_text(size = 28, nrow(2)), legend.title = element_text(size=30), legend.position="bottom", plot.title = element_text(size=32,hjust=0.5),
        strip.text.x = element_text(size = 30), strip.text.y = element_text(size = 30), plot.margin = margin(20, 20, 10, 10))

plot_grid(plot1, plot2, nrow = 2, rel_heights=c(0.85,1))

dev.off()