###########################
#This script takes the historical HDD and CDD calculated daily for the population-weighted centroids of the SWITCH load zones from the Livneh dataset, 
#and calculates the daily delta in HDD and CDD for 15 climate scenarios (15 GCMS) based on temperature projections from 15 GCMs
#These daily HDD and CDD deltas by SWITCH load zone will be uploaded to the SWITCH database and used to calculate the hourly load differences in SWITCH
library(tidyverse)
library(lubridate)
library(stringr)
library(RColorBrewer)
library(readxl)


rm(list=ls())  #clear variables in R space

#replace file path here:
dat.dir <- ""


setwd(dat.dir)

############################################
HistoricalFiles <- list.files(path=".\\hist\\",pattern="_CDD-HDD")
HistoricalFilesLong <- list.files(path=".\\hist\\",pattern="_CDD-HDD",full.names=TRUE)

############################################
avg_daily_historical_cdd_hdd_lz <- data.frame()

for (f in 1:length(HistoricalFiles)) {
 
  historical_cdd_hdd = read.csv(HistoricalFilesLong[f], header=TRUE,sep=",")
  historical_cdd_hdd$year <- year(historical_cdd_hdd$Date)
  historical_cdd_hdd$month <- month(historical_cdd_hdd$Date)
  historical_cdd_hdd$day <- day(historical_cdd_hdd$Date)
  
  historical_cdd_hdd$load_zone <- str_sub(HistoricalFiles[f],1, str_locate(HistoricalFiles[f],"_CDD-HDD")-1)[1]
  
  #limit date range to reference time period 1980 - 2010
  historical_cdd_hdd_limit <- historical_cdd_hdd %>% filter(historical_cdd_hdd$Date < ymd("2011-01-01") & historical_cdd_hdd$Date >= ymd("1980-01-01"))
  
  #average daily HDD and CDD by load zone for reference time period
  avg_daily_historical_cdd_hdd <- historical_cdd_hdd_limit %>% group_by(month, day, load_zone) %>% summarize(HDD = mean(HDD), CDD = mean(CDD))
  
  #long_format by load zone and degree day type 
  avg_daily_historical_cdd_hdd_long <- gather(avg_daily_historical_cdd_hdd, degree_day_type, hist_avg_degree_day_C, 4:5)

  avg_daily_historical_cdd_hdd_lz  <- rbind(avg_daily_historical_cdd_hdd_lz,avg_daily_historical_cdd_hdd_long)
}

############################################
##REPEAT WITH CLIMATE SCENARIOS

climate_scenario_list <- c("CCSM","HadGEM2-ES","CanESM","CESM1-CAM5","ACCESS-1.0","CNRM-CM5",
                           "GFDL-ESM2M","bcc-csm1-1", "MPI-ESM-LR", "MIROC5", "HadGEM2-CC", "GFDL-CM3" ,
                           "CMCC-CMS"  , "CMCC-CM", "CESM1-BGC")

#compile daily CDD and HDD for each load zone and climate scenario for the future period

CC_daily_delta_CDD_lz_all_scenarios <- data.frame()
CC_daily_delta_HDD_lz_all_scenarios <- data.frame()

for (i in 1:length(climate_scenario_list)){
  # i=1
  #get list of files for each climate scenario
  ClimateScenarioFiles <- list.files(path=paste(".\\",climate_scenario_list[i],"\\",sep=""),pattern="_CDD-HDD")
  ClimateScenarioFilesLong <- list.files(path=paste(".\\",climate_scenario_list[i],"\\",sep=""),pattern="_CDD-HDD",full.names=TRUE)
  
  print(climate_scenario_list[i])
  
  CC_daily_delta_CDD_lz <- data.frame()
  CC_daily_delta_HDD_lz <- data.frame()
  
  for (f in 1:length(ClimateScenarioFiles)) {
    
    cc_cdd_hdd = read.csv(ClimateScenarioFilesLong[f], header=TRUE,sep=",")
    cc_cdd_hdd$year <- year(cc_cdd_hdd$Date)
    cc_cdd_hdd$month <- month(cc_cdd_hdd$Date)
    cc_cdd_hdd$day <- day(cc_cdd_hdd$Date)
    
    #get load zone name
    cc_cdd_hdd$load_zone <- str_sub(ClimateScenarioFiles[f],1, str_locate(ClimateScenarioFiles[f],"_CDD-HDD")-1)[1]
    
    #get scenario name
    cc_cdd_hdd$Scenario <- climate_scenario_list[i]
    
    #limit date range to future time period < 2070
    cc_cdd_hdd <- cc_cdd_hdd %>% filter(cc_cdd_hdd$Date < ymd("2071-01-01"))
    
    #long_format by load zone and degree day type 
    cc_cdd_hdd_long <- gather(cc_cdd_hdd, degree_day_type, degree_day_C, 4:5)
    
    ###JOIN HISTORICAL AVERAGE DAILY HDD AND CDD WITH PROJECTED CLIMATE SCENARIOS OF HDD AND CDD
    
    #join by load zone, day of year (ie April 1), and degree day type
    daily_delta_cdd_hdd_lz <- left_join(cc_cdd_hdd_long, avg_daily_historical_cdd_hdd_lz, by = c("month"="month", "day"="day", "load_zone"="load_zone", "degree_day_type"="degree_day_type"))
    
    #calculte delta degree day for each future day
    daily_delta_cdd_hdd_lz$delta_degree_day_C <- daily_delta_cdd_hdd_lz$degree_day_C - daily_delta_cdd_hdd_lz$hist_avg_degree_day_C
    
    ###CALCULATE 10-year moving WINDOW DAY-OF-YEAR AVERAGES OF CDD AND HDD DEGREE DAY DELTAS FROM BETWEEN CLIMATE PROJECTIONS AND HISTORICAL DATA
    
    
    ##CDD
    #10 year averaging for CDD
    
    #separate out CDD
    daily_delta_cdd <- daily_delta_cdd_hdd_lz %>% filter(degree_day_type == "CDD")
    
    # #calculate 10-yr moving window average for each day of year
    w = 3650
    
    for (j in seq_len(length(daily_delta_cdd$delta_degree_day_C))) {
      #creates a sequence of indices of daily observations from minus 5 years to plus 5 years with each annual observation
      ind <- seq((j - floor(w / 2)), (j + floor(w / 2)), by=365)
      
      #removes indices that are negative and greater than the length of the column
      ind <- ind[ind>0]
      
      ind <- ind[ind<length(daily_delta_cdd$delta_degree_day_C)]
      
      #average the delta degree days for each day of year across the years of the averaging window
      daily_delta_cdd$avg_delta_degree_day_C[j] <- mean(daily_delta_cdd$delta_degree_day_C[ind]) 
      
    }
    
    #append to CDD daily deltas from other load zones
    CC_daily_delta_CDD_lz  <- rbind(CC_daily_delta_CDD_lz,daily_delta_cdd)
    
    ####HDD
    #10 year averaging for HDD
    
    #separate out HDD
    daily_delta_hdd <- daily_delta_cdd_hdd_lz %>% filter(degree_day_type == "HDD")
    
    # #calculate 10-yr moving window average for each day of year
    w = 3650
    
    for (k in seq_len(length(daily_delta_hdd$delta_degree_day_C))) {
      #creates a sequence from minus 5 years to plus 5 years with each annual observation
      ind <- seq((k - floor(w / 2)), (k + floor(w / 2)), by=365)
      
      #removes indices that are negative and greater than the length of the column
      ind <- ind[ind>0]
      
      ind <- ind[ind<length(daily_delta_hdd$delta_degree_day_C)]
      
      #average the delta degree days for each day of year across the years of the averaging window
      daily_delta_hdd$avg_delta_degree_day_C[k] <- mean(daily_delta_hdd$delta_degree_day_C[ind]) 
      
      
    }
    
    #append to HDD daily deltas from other load zones
    CC_daily_delta_HDD_lz  <- rbind(CC_daily_delta_HDD_lz,daily_delta_hdd)
    
    
  }
  
  #output into csv for uploading into SWITCH db
  write.csv(CC_daily_delta_CDD_lz, file = paste(dat.dir,"\\Daily_delta_CDD_lz_",climate_scenario_list[i],"_scenario.csv",sep=""), row.names = FALSE)
  
  #append to CDD daily deltas from other scenarios
  CC_daily_delta_CDD_lz_all_scenarios <- rbind(CC_daily_delta_CDD_lz_all_scenarios, CC_daily_delta_CDD_lz)
  
  #output into csv for uploading into SWITCH db
  write.csv(CC_daily_delta_HDD_lz, file = paste(dat.dir,"\\Daily_delta_HDD_lz_",climate_scenario_list[i],"_scenario.csv",sep=""), row.names = FALSE)
  
  #append to HDD daily deltas from other scenarios
  CC_daily_delta_HDD_lz_all_scenarios <- rbind(CC_daily_delta_HDD_lz_all_scenarios, CC_daily_delta_HDD_lz)
}

#append CDD and HDD daily deltas to one dataframe for all scenarios
Daily_delta_CDD_HDD_lz_scenario <- rbind(CC_daily_delta_CDD_lz_all_scenarios, CC_daily_delta_HDD_lz_all_scenarios)

#output into csv for uploading into SWITCH db
Daily_delta_CDD_HDD_lz_scenario <- Daily_delta_CDD_HDD_lz_scenario %>% dplyr::select(Date,year, month, day, load_zone, Scenario, degree_day_type, degree_day_C, hist_avg_degree_day_C, delta_degree_day_C, avg_delta_degree_day_C)

#output into csv for uploading into SWITCH db
write.csv(Daily_delta_CDD_HDD_lz_scenario, file = paste(dat.dir,"\\Daily_delta_CDD_HDD_lz_scenario.csv",sep=""), row.names = FALSE)








