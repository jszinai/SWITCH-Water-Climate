##Script to process and create plots and maps of results from WEAP-SWITCH climate runs vs baseline

#This script supports the analysis of the manuscript submitted to Nature Communications on climate impacts and adaptation of the WECC grid. It does the following: 


library(tidyverse)
library(lubridate)
library(stringr)
library(RColorBrewer)
library(httr)
library(jsonlite)
library(rgdal)
library(sp)
library(sf)
library(ggrepel)
library(lubridate)


#set global parameters for reading in SWITCH run results####

#replace file path here:
switch_input_dir <- ""
#
switch_output_dir <- ""

switch_run_dir <- "SWITCH_WEAP_runs/"

results_dir <- "WEAP_Nov_8_results_SWITCH_Mar_27_results/"

output_folder_name <-"//post-process//"

day <- "Mar_27_"

setwd(switch_input_dir)

#set baseline and climate run names and other globals here:

run_name1 <- "id_202_WECC_0_carbon_baseline_5y_24_sample_barrier"
scenario_name1 <- "Baseline WECC-wide 0 carbon cap"
scenario_name_short1 <- "Baseline no CC"

# climate_impacts_included <- "Hydropower and Water Load Scenarios"
# climate_impacts_included <- "Cooling Load Scenarios"
climate_impacts_included <- "Cooling + Hydropower and Water Load Scenarios"

#color palettes for scenarios and energy sources:

cbpal <- c("#999999","#004949","#009292","#ff6db6","#ffb6db",
           "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
           "#920000","#924900","#db6d00","#24ff24","#ffff6d")

cbpal2 <- c("#999999","#004949","#009292","#ff6db6","#ffb6db",
            "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
            "#920000","#924900","#db6d00","#24ff24","#ffff6d", "#000000")

energy_source_palette <- c("Oil" = "black", "Biomass" = "springgreen4", "Coal" = "tan4", "Gas" = "#666666","Geothermal" = "#660000","Solar" = "gold","Uranium" = "blueviolet", "Water" = "#0066CC",
                           "Wind" = "deepskyblue1", "Battery Storage" = "aquamarine", "Waste Heat" = "brown", "Curtailment"="red")


#coming up with scenario and run name lists
climate_scenario_list <- c("ACCESS-1.0","bcc-csm1-1","CanESM","CCSM","CESM1-BGC","CESM1-CAM5","CMCC-CM", "CMCC-CMS", "CNRM-CM5","GFDL-CM3", "GFDL-ESM2M" ,"HadGEM2-CC", "HadGEM2-ES","MIROC5", "MPI-ESM-LR"  )

run_name_list <- list(run_name1)
scenario_list <- list(scenario_name1)
short_scenario_list <- list(scenario_name_short1)


for (i in 1:length(climate_scenario_list)){
  
  if (climate_impacts_included == "Hydropower and Water Load Scenarios") {
  new_run_name <- paste(run_name1, climate_scenario_list[i],"Hydro_Load",sep="_")
  run_name_list[[i+1]] <- new_run_name
  
  new_scenario_name <- paste(climate_scenario_list[i], "hydropower, energy for water" ,sep=" ")
  scenario_list[[i+1]] <- new_scenario_name
  
  new_short_scenario_name <- paste(climate_scenario_list[i])
  short_scenario_list[[i+1]] <- new_short_scenario_name
  
  weap_run_dir <- paste(results_dir, "Hydropower and Water Load Scenarios/", sep="")
  
  }else if (climate_impacts_included == "Cooling + Hydropower and Water Load Scenarios"){
    new_run_name <- paste(run_name1, climate_scenario_list[i],"CDD_HDD_Hydro_Load",sep="_")
    run_name_list[[i+1]] <- new_run_name
    
    new_scenario_name <- paste(climate_scenario_list[i], "cooling, hydropower, energy for water" ,sep=" ")
    scenario_list[[i+1]] <- new_scenario_name
    
    new_short_scenario_name <- paste(climate_scenario_list[i])
    short_scenario_list[[i+1]] <- new_short_scenario_name
    
    weap_run_dir <- paste(results_dir, "Cooling + Hydropower and Water Load Scenarios/", sep="")

  }else if  (climate_impacts_included == "Cooling Load Scenarios"){
    new_run_name <- paste(run_name1, climate_scenario_list[i],"CDD_HDD",sep="_")
    run_name_list[[i+1]] <- new_run_name
    
    new_scenario_name <- paste(climate_scenario_list[i], "cooling" ,sep=" ")
    scenario_list[[i+1]] <- new_scenario_name
    
    new_short_scenario_name <- paste(climate_scenario_list[i])
    short_scenario_list[[i+1]] <- new_short_scenario_name
    
    weap_run_dir <- paste(results_dir, "Cooling Load Scenarios/", sep="")
  }
}


###Generation capacity online####################

gen_cap_source_period_lz <- data.frame()

for (i in 1:length(run_name_list)){
  
  run_name <- run_name_list[[i]]
  scenario <- short_scenario_list[[i]]
  
  #read in the generation capacity builds and generation project information 
  gen_cap <- read.csv(file=paste(switch_input_dir,run_name,"/outputs/","gen_cap.csv",sep=""),stringsAsFactors=F, header = T, fill = TRUE) 
 
  # selecting only the columns of interest
  gen_capacity_cumulative <- subset(gen_cap, select=c(PERIOD, GENERATION_PROJECT, gen_load_zone, gen_tech, gen_energy_source, GenCapacity))
  
  #cleaning the data from possible reading problems ("NA")
  rows <- nrow(gen_capacity_cumulative)
  gen_capacity_cumulative <- na.omit(gen_capacity_cumulative)
  rows2 <- nrow(gen_capacity_cumulative)
  
  #checking how many rows were discarded
  rows-rows2
  
  #specifying CAES gen_tech as CAES gen_energy_source source so it doesn't get lumped into regular gas, and battery storage as gen_energy_source source
  #gen_capacity_cumulative$gen_energy_source[gen_capacity_cumulative$gen_tech == "Compressed_Air_Energy_Storage"] <- "Compressed_Air_Energy_Storage"
  gen_capacity_cumulative$gen_energy_source[gen_capacity_cumulative$gen_tech == "Battery_Storage"] <- "Battery Storage"
  gen_capacity_cumulative$gen_energy_source[gen_capacity_cumulative$gen_energy_source == "Waste_Heat"] <- "Waste Heat"
  
  #also aggregating the bio gen_energy_source categories and renaming oil
  gen_capacity_cumulative$gen_energy_source[gen_capacity_cumulative$gen_energy_source == "Bio_Gas" | gen_capacity_cumulative$gen_energy_source =="Bio_Liquid" | gen_capacity_cumulative$gen_energy_source == "Bio_Solid"] <- "Biomass"
  gen_capacity_cumulative$gen_energy_source[gen_capacity_cumulative$gen_energy_source == "DistillateFuelOil" | gen_capacity_cumulative$gen_energy_source ==  "ResidualFuelOil"] <- "Oil"
  
  #aggregate capacity by generating gen_energy_source souce (across all gen technologies) by load zone and period
  gen_cap_fuelAgg <- gen_capacity_cumulative %>% group_by(gen_load_zone, PERIOD, gen_energy_source) %>% summarize(GenCapacityCumulative_MW = sum(GenCapacity))
  
  #calculating the total cumulative generating capacity by load zone and PERIOD and joining with gen_cap aggregated for each PERIOD by source
  gen_cap_SourceAgg_st <- arrange(gen_cap_fuelAgg, gen_load_zone, PERIOD, gen_energy_source)
  
  gen_cap_PERIOD_total_lz <- gen_cap_SourceAgg_st %>% group_by(gen_load_zone, PERIOD) %>% summarize(Total_Lz_GenCap_MW = sum(GenCapacityCumulative_MW))
  
  gen_cap_SourceAgg_st <- left_join(gen_cap_SourceAgg_st,gen_cap_PERIOD_total_lz,c("gen_load_zone" = "gen_load_zone","PERIOD"="PERIOD"))
  
  gen_cap_SourceAgg_st$Scenario <- scenario
  
  #capacity online by load zone and period for each scenario
  gen_cap_source_period_lz <- rbind(gen_cap_source_period_lz, gen_cap_SourceAgg_st)
  
}

#output csv of capacity installed by load zone by period for each scenario
write.csv(x = gen_cap_source_period_lz, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, climate_impacts_included, "LZ_capacity_online.csv",sep=""), row.names = F)

#total WECC capacity online by period for each scenario
gen_cap_source_period_WECC <- gen_cap_source_period_lz %>% group_by(Scenario, PERIOD, gen_energy_source) %>% summarize(GenCapacityOnline_MW = sum(GenCapacityCumulative_MW))

#output csv of total WECC capacity online by period for each scenario
write.csv(x = gen_cap_source_period_WECC, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, climate_impacts_included,"WECCcapacity_online_LONG.csv",sep=""), row.names = F)

#FOR SI
#plot of total WECC capacity by energy source for each period BASELINE SCENARIO
baseline <- scenario_name_short1

plot_file_name <- paste("SI_WECC_installed_capacity_BASELINE",day, ".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=950, height=700, res=100)

ggplot(data=subset(gen_cap_source_period_WECC, Scenario == baseline), aes(x=PERIOD, y=GenCapacityOnline_MW/1000, fill=gen_energy_source, label=round(GenCapacityOnline_MW/1000,0))) + 
  ggtitle(paste("WECC online generating capacity, Baseline scenario", sep=", ")) +
  geom_bar(stat="identity") +
 labs(x="Investment Period") + labs(y="Capacity Online (GW)") +
  scale_fill_manual(name="Energy Source", values = energy_source_palette) +
  theme(axis.text.y = element_text(size=16), axis.title.x = element_text(size=18), axis.text.x = element_text(size=16, angle = 45), axis.title.y=element_text(size=18), 
        legend.text = element_text(size = 14), legend.title = element_text(size=16), legend.position="bottom", plot.title = element_text(size=20,hjust=0.7), 
        strip.text.x = element_text(size = 18), strip.text.y = element_text(size = 18), plot.margin = margin(10, 50, 10, 10), legend.margin = margin(0, 150, 0, 150)) + 
  guides(fill = guide_legend(nrow = 2))

dev.off()  


#making wide table of capacity online by energy source and scenario and period
gen_cap_source_period_WECC_wide <- spread(gen_cap_source_period_WECC, gen_energy_source, GenCapacityOnline_MW)

gen_cap_source_period_WECC_wide[is.na(gen_cap_source_period_WECC_wide)] <- 0

write.csv(x = gen_cap_source_period_WECC_wide, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,climate_impacts_included, "WECC_capacity_online.csv",sep=""), row.names = F)

#load zone delta between baseline and CC scenarios

baseline <- scenario_name_short1

gen_cap_source_period_lz_baseline <- gen_cap_source_period_lz %>% filter(Scenario == baseline)
gen_cap_source_period_lz_baseline <- rename(gen_cap_source_period_lz_baseline, Baseline_Scenario = Scenario, GenCapacityCumulative_MW_baseline = GenCapacityCumulative_MW, Total_Lz_GenCap_MW_baseline = Total_Lz_GenCap_MW)

gen_cap_source_period_lz_scenario_delta0 <- gen_cap_source_period_lz %>% filter(Scenario != baseline)
gen_cap_source_period_lz_scenario_delta <- left_join(gen_cap_source_period_lz_scenario_delta0, gen_cap_source_period_lz_baseline, c("PERIOD"="PERIOD", "gen_load_zone" = "gen_load_zone","gen_energy_source" = "gen_energy_source"))

gen_cap_source_period_lz_scenario_delta$Delta_GenCapacity_MW <-gen_cap_source_period_lz_scenario_delta$GenCapacityCumulative_MW - gen_cap_source_period_lz_scenario_delta$GenCapacityCumulative_MW_baseline

gen_cap_source_period_lz_scenario_delta$GenCap_perc_change_from_baseline <- round(gen_cap_source_period_lz_scenario_delta$Delta_GenCapacity_MW/gen_cap_source_period_lz_scenario_delta$GenCapacityCumulative_MW_baseline,3)

#output csv of DELTA load zone capacity online by period for each scenario
write.csv(x = gen_cap_source_period_lz_scenario_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, climate_impacts_included, "DELTA_Lz_capacity_online.csv",sep=""), row.names = F)

#WECC wide delta between baseline and CC scenarios
gen_cap_source_period_WECC_scenario_delta <- gen_cap_source_period_lz_scenario_delta %>% 
  group_by(Scenario, Baseline_Scenario, PERIOD, gen_energy_source) %>% 
  summarize(GenCapacityOnline_MW = sum(GenCapacityCumulative_MW), GenCapacityOnline_MW_baseline = sum(GenCapacityCumulative_MW_baseline))

# get percentage diff from baseline and total WECC wide delta
gen_cap_source_period_WECC_scenario_delta$Delta_GenCapacity_MW <- gen_cap_source_period_WECC_scenario_delta$GenCapacityOnline_MW - gen_cap_source_period_WECC_scenario_delta$GenCapacityOnline_MW_baseline
gen_cap_source_period_WECC_scenario_delta$GenCap_perc_change_from_baseline <- round(gen_cap_source_period_WECC_scenario_delta$Delta_GenCapacity_MW/gen_cap_source_period_WECC_scenario_delta$GenCapacityOnline_MW_baseline,3)

#output csv of DELTA WECC capacity online by period for each scenario
write.csv(x = gen_cap_source_period_WECC_scenario_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, climate_impacts_included, "DELTA_WECC_capacity_online.csv",sep=""), row.names = F)

#Plot of Deltas of capacity WECC wide compared to baseline scenario 1

#facet plot with capacity delta WECC-wide 3 x 5
plot_file_name <- paste("SI_Delta_WECC_capacity_online3x5_",climate_impacts_included, day,".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=1100, height=1000, res=100)

plot <- ggplot(data=subset(gen_cap_source_period_WECC_scenario_delta, Scenario != baseline), aes(x=PERIOD, y=Delta_GenCapacity_MW/1000, fill=gen_energy_source, label=ifelse(GenCap_perc_change_from_baseline>0,paste("+",round(GenCap_perc_change_from_baseline*100,0),"%",sep=""),paste(round(GenCap_perc_change_from_baseline*100,0),"%",sep="")))) +
  ggtitle(paste("Change in online generating capacity by period relative to baseline scenario", climate_impacts_included, sep=", ")) +
  geom_bar(stat="identity") +
  labs(x="Investment Period") + labs(y="Delta Capacity Online relative to Baseline Scenario (GW)") +
  scale_fill_manual(name="Energy Source", values = energy_source_palette) +
  facet_wrap( ~ Scenario, nrow=3) +
  geom_hline(yintercept=0, color = "black") +
  theme(axis.text.y = element_text(size=14), axis.title.x = element_text(size=16), axis.text.x = element_text(size=14, angle = 45), axis.title.y=element_text(size=16),
        legend.text = element_text(size = 12), legend.title = element_text(size=12), legend.position="bottom", plot.title = element_text(size=13,hjust=0.5),
        strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15)) +
  guides(fill = guide_legend(nrow = 2))

print(plot)

dev.off()

#FIGURE 4A

#plot of Delta WECC capacity by energy source for each period BASELINE SCENARIO
library(cowplot)
plot_file_name <- paste("Fig4A_DELTA_WECC_installed_capacity_ACCESS_BASELINE",day, ".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=1400, height=500, res=100)

plot1 <- ggplot(data=subset(gen_cap_source_period_WECC, Scenario == baseline), aes(x=PERIOD, y=GenCapacityOnline_MW/1000, fill=gen_energy_source, label=round(GenCapacityOnline_MW/1000,0))) + 
  ggtitle(paste("Online capacity, Baseline Scenario", sep=", ")) +
  geom_bar(stat="identity") +
  labs(x="Investment period") + labs(y="Capacity online (GW)") +
  ylim(0, 1000) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(name="Energy source", values = energy_source_palette) +
  theme(axis.text.y = element_text(size=18), axis.title.x = element_text(size=20), axis.text.x = element_text(size=18), axis.title.y=element_text(size=20), 
        legend.text = element_text(size = 18, nrow(2)), legend.title = element_text(size=20), legend.position="none", plot.title = element_text(size=20,hjust=0.3), 
        strip.text.x = element_text(size = 18), strip.text.y = element_text(size = 18), plot.margin = margin(10, 10, 10, 10))

plot2 <- ggplot(data=subset(gen_cap_source_period_WECC_scenario_delta, Scenario == "ACCESS-1.0"), aes(x=PERIOD, y=Delta_GenCapacity_MW/1000, fill=gen_energy_source, label=ifelse(GenCap_perc_change_from_baseline>0,paste("+",round(GenCap_perc_change_from_baseline*100,0),"%",sep=""),paste(round(GenCap_perc_change_from_baseline*100,0),"%",sep="")))) +
  ggtitle(paste("Change in online capacity, ACCESS-1.0 Scenario", sep=", ")) +
  geom_bar(stat="identity") +
  labs(x="Investment period") + labs(y="Delta capacity online (GW)") +
  scale_fill_manual(name="Energy source", values = energy_source_palette) +
  scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept=0, color = "black") +
  theme(axis.text.y = element_text(size=18), axis.title.x = element_text(size=20), axis.text.x = element_text(size=18), axis.title.y=element_text(size=20), 
        legend.text = element_text(size = 18, nrow(2)), legend.title = element_text(size=20), legend.position="right", plot.title = element_text(size=20,hjust=0.3), 
        strip.text.x = element_text(size = 18), strip.text.y = element_text(size = 18), plot.margin = margin(10, 30, 10, 10))

plot_grid(plot1, plot2, rel_widths=c(0.75,1))

dev.off()



##Generation capacity builds#####################

gen_build_source_period_lz <- data.frame()

for (i in 1:length(run_name_list)){
  
  run_name <- run_name_list[[i]]
  scenario <- short_scenario_list[[i]]
  
  #read in the generation capacity builds and generation project information 
  gen_build <- read.csv(file=paste(switch_input_dir,run_name,"/outputs/","BuildGen.csv",sep=""),stringsAsFactors=F, header = T, fill = TRUE) 
  gen_projects <- read.csv(file=paste(switch_input_dir,run_name,"/inputs/","generation_projects_info.csv",sep=""),stringsAsFactors=F, header = T, fill = TRUE) 
  
  # selecting only the columns of interest
  gen_projects <- subset(gen_projects, select=c(GENERATION_PROJECT, gen_tech, gen_energy_source, gen_load_zone, gen_tech))
  
  gen_build <- rename(gen_build, GENERATION_PROJECT = GEN_BLD_YRS_1, YEAR = GEN_BLD_YRS_2)
  #joining the generation projects info with the gen
  
  gen_build_projects <- left_join(gen_build, gen_projects, c("GENERATION_PROJECT"="GENERATION_PROJECT"))
  
  #specifying CAES gen_tech as CAES gen_energy_source source so it doesn't get lumped into regular gas, and battery storage as gen_energy_source source
  #gen_capacity_cumulative$gen_energy_source[gen_capacity_cumulative$gen_tech == "Compressed_Air_Energy_Storage"] <- "Compressed_Air_Energy_Storage"
  gen_build_projects$gen_energy_source[gen_build_projects$gen_tech == "Battery_Storage"] <- "Battery Storage"
  gen_build_projects$gen_energy_source[gen_build_projects$gen_energy_source == "Waste_Heat"] <- "Waste Heat"
  
  #also aggregating the bio gen_energy_source categories and renaming oil
  gen_build_projects$gen_energy_source[gen_build_projects$gen_energy_source == "Bio_Gas" | gen_build_projects$gen_energy_source =="Bio_Liquid" | gen_build_projects$gen_energy_source == "Bio_Solid"] <- "Biomass"
  gen_build_projects$gen_energy_source[gen_build_projects$gen_energy_source == "DistillateFuelOil" | gen_build_projects$gen_energy_source ==  "ResidualFuelOil"] <- "Oil"
  
  #aggregate capacity by generating gen_energy_source souce (across all gen technologies) by load zone and period
  gen_build_fuelAgg <- gen_build_projects %>% group_by(gen_load_zone, YEAR, gen_energy_source) %>% summarize(BuildGen_MW = sum(BuildGen))
  
  #calculating the total cumulative generating capacity by load zone and PERIOD and joining with gen_cap aggregated for each PERIOD by source
  gen_build_fuelAgg_st <- arrange(gen_build_fuelAgg, gen_load_zone, YEAR, gen_energy_source)
  
  gen_build_PERIOD_total_lz <- gen_build_fuelAgg_st %>% group_by(gen_load_zone, YEAR) %>% summarize(Total_BuildGen_MW = sum(BuildGen_MW))
  
  gen_build_fuelAgg_st <- left_join(gen_build_fuelAgg_st,gen_build_PERIOD_total_lz,c("gen_load_zone" = "gen_load_zone","YEAR"="YEAR"))
  
  gen_build_fuelAgg_st$Scenario <- scenario
  
  #capacity online by load zone and period for each scenario
  gen_build_source_period_lz <- rbind(gen_build_source_period_lz, gen_build_fuelAgg_st)
  
}

#categorize years into periods, and pre-existing
gen_build_source_period_lz$PERIOD <- "pre-2030"

gen_build_source_period_lz$PERIOD <-ifelse(gen_build_source_period_lz$YEAR  >= 2028 & gen_build_source_period_lz$YEAR  < 2033, 2030, gen_build_source_period_lz$PERIOD)
gen_build_source_period_lz$PERIOD <-ifelse(gen_build_source_period_lz$YEAR  >= 2033 & gen_build_source_period_lz$YEAR  < 2038, 2035, gen_build_source_period_lz$PERIOD)
gen_build_source_period_lz$PERIOD <-ifelse(gen_build_source_period_lz$YEAR  >= 2038 & gen_build_source_period_lz$YEAR  < 2043, 2040, gen_build_source_period_lz$PERIOD)
gen_build_source_period_lz$PERIOD <-ifelse(gen_build_source_period_lz$YEAR  >= 2043 & gen_build_source_period_lz$YEAR  < 2048, 2045, gen_build_source_period_lz$PERIOD)
gen_build_source_period_lz$PERIOD <-ifelse(gen_build_source_period_lz$YEAR  >= 2048 & gen_build_source_period_lz$YEAR  < 2053, 2050, gen_build_source_period_lz$PERIOD)
gen_build_source_period_lz$PERIOD <-ifelse(gen_build_source_period_lz$YEAR  >= 2053, 2055, gen_build_source_period_lz$PERIOD)



#output csv of capacity installed by load zone by period for each scenario
write.csv(x = gen_build_source_period_lz, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,climate_impacts_included, "WECC_load_zone_BuildGen_by_period_scenario.csv",sep=""), row.names = F)

#total WECC capacity builds by period for each scenario
gen_build_source_period_WECC <- gen_build_source_period_lz %>% group_by(Scenario, PERIOD, gen_energy_source) %>% summarize(BuildGen_MW = sum(BuildGen_MW))

#output csv of total WECC capacity builds by period for each scenario
write.csv(x = gen_build_source_period_WECC, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, climate_impacts_included,"WECC_BuildGen_by_period_scenario_LONG.csv",sep=""), row.names = F)

#plot of total WECC capacity builds by energy sourc for each period BASELINE SCENARIO
baseline <- scenario_name_short1

#reorder factors
gen_build_source_period_WECC$PERIOD <- factor(gen_build_source_period_WECC$PERIOD, levels = c("pre-2030", "2030", "2035", "2040", "2045", "2050"))

#making wide table of gen builds by energy source and scenario and period
gen_build_source_period_WECC_wide <- spread(gen_build_source_period_WECC, gen_energy_source, BuildGen_MW)

gen_build_source_period_WECC_wide[is.na(gen_build_source_period_WECC_wide)] <- 0

write.csv(x = gen_build_source_period_WECC_wide, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, climate_impacts_included,"WECC_total_gen_builds_by_period_scenario_WIDE.csv",sep=""), row.names = F)

#load zone delta between baseline and CC scenarios

baseline <- scenario_name_short1

gen_build_source_period_lz_baseline <- gen_build_source_period_lz %>% filter(Scenario == baseline)
gen_build_source_period_lz_baseline <- rename(gen_build_source_period_lz_baseline, Baseline_Scenario = Scenario, BuildGen_MW_baseline = BuildGen_MW, Total_BuildGen_MW_baseline = Total_BuildGen_MW)

gen_build_source_period_lz_scenario_delta0 <- gen_build_source_period_lz %>% filter(Scenario != baseline)
gen_build_source_period_lz_scenario_delta <- left_join(gen_build_source_period_lz_scenario_delta0, gen_build_source_period_lz_baseline, c("PERIOD"="PERIOD", "gen_load_zone" = "gen_load_zone","gen_energy_source" = "gen_energy_source"))

gen_build_source_period_lz_scenario_delta$Delta_BuildGen_MW <-gen_build_source_period_lz_scenario_delta$BuildGen_MW - gen_build_source_period_lz_scenario_delta$BuildGen_MW_baseline

gen_build_source_period_lz_scenario_delta$BuildGen_perc_change_from_baseline <- round(gen_build_source_period_lz_scenario_delta$Delta_BuildGen_MW/gen_build_source_period_lz_scenario_delta$BuildGen_MW_baseline,3)

#output csv of DELTA load zone capacity online by period for each scenario
write.csv(x = gen_build_source_period_lz_scenario_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,climate_impacts_included, "DELTA_Load_zone_build_gen_by_period_scenario.csv",sep=""), row.names = F)

#WECC wide delta between baseline and CC scenarios
gen_build_source_period_WECC_scenario_delta <- gen_build_source_period_lz_scenario_delta %>% 
  group_by(Scenario, Baseline_Scenario, PERIOD, gen_energy_source) %>% 
  summarize(BuildGen_MW = sum(BuildGen_MW), BuildGen_MW_baseline = sum(BuildGen_MW_baseline))

# get percentage diff from baseline and total WECC wide delta
gen_build_source_period_WECC_scenario_delta$Delta_BuildGen_MW <- gen_build_source_period_WECC_scenario_delta$BuildGen_MW - gen_build_source_period_WECC_scenario_delta$BuildGen_MW_baseline
gen_build_source_period_WECC_scenario_delta$BuildGen_perc_change_from_baseline <- round(gen_build_source_period_WECC_scenario_delta$Delta_BuildGen_MW/gen_build_source_period_WECC_scenario_delta$BuildGen_MW_baseline,3)

#output csv of DELTA WECC capacity online by period for each scenario
write.csv(x = gen_build_source_period_WECC_scenario_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, climate_impacts_included,"DELTA_WECC_total_build_gen_by_period_scenario.csv",sep=""), row.names = F)

#Plot of Deltas of capacity WECC wide compared to baseline scenario 1

#reorder scenario
# gen_build_source_period_WECC_scenario_delta <- ungroup(gen_build_source_period_WECC_scenario_delta) %>% mutate(Scenario = fct_reorder(Scenario, Delta_BuildGen_MW, max, .desc = FALSE))

#facet plot with gen build delta WECC-wide
plot_file_name <- paste("SI_Delta_WECC_gen_build_",climate_impacts_included, day,".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=1100, height=1000, res=100)

plot <- ggplot(data=subset(gen_build_source_period_WECC_scenario_delta, Scenario != baseline & PERIOD != "pre-2030"), aes(x=PERIOD, y=Delta_BuildGen_MW/1000, fill=gen_energy_source, label=paste(round(BuildGen_perc_change_from_baseline*100,0),"%",sep=""))) + 
  ggtitle(paste("Change in generating capacity built by period relative to baseline scenario",climate_impacts_included, sep=", ")) +
  geom_bar(stat="identity") +
  labs(x="Investment Period") + labs(y="Delta generation built relative to Baseline Scenario (GW)") +
  scale_fill_manual(name="Energy Source", values = energy_source_palette) +
  facet_wrap( ~ Scenario, nrow=3) +
  geom_hline(yintercept=0, color = "black") +
  theme(axis.text.y = element_text(size=14), axis.title.x = element_text(size=16), axis.text.x = element_text(size=14, angle = 45), axis.title.y=element_text(size=16), 
        legend.text = element_text(size = 12), legend.title = element_text(size=12), legend.position="bottom", plot.title = element_text(size=13.5,hjust=0.5), 
        strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15)) +
  guides(fill = guide_legend(nrow = 2))

print(plot)

dev.off()  


#FIGURE 3A
#calculating CUMULATIVE DELTA GEN BUILDS OVER 2030-2050
cumulative_gen_build_sourceWECC_scenario_delta <- gen_build_source_period_WECC_scenario_delta %>% filter(PERIOD != "pre-2030") %>% group_by(Scenario, Baseline_Scenario, gen_energy_source) %>% summarize(CumulativelDelta_BuildGen_MW = sum(Delta_BuildGen_MW), 
                                                                                                                                     CumulativeBuildGen_MW= sum(BuildGen_MW),
                                                                                                                                     CumulativeBaselineBuildGen_MW= sum(BuildGen_MW_baseline))

cumulative_gen_build_sourceWECC_scenario_delta <- cumulative_gen_build_sourceWECC_scenario_delta %>% group_by(Scenario, Baseline_Scenario) %>% mutate(TotalCumulativelDelta_BuildGen_MW = sum(CumulativelDelta_BuildGen_MW))


cumulative_gen_build_sourceWECC_scenario_delta <- ungroup(cumulative_gen_build_sourceWECC_scenario_delta) %>% mutate(Scenario = fct_reorder(Scenario, TotalCumulativelDelta_BuildGen_MW, max, .desc = FALSE))

write.csv(x = cumulative_gen_build_sourceWECC_scenario_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, climate_impacts_included,"DELTA_WECC_Cumulative_Build_gen_by_scenario.csv",sep=""), row.names = F)


#facet plot with cumulative capacity builds Delta WECC-wide by baseline vs. climate scenarios
library(cowplot)
plot_file_name <- paste("Fig3A_CumulativeBaseline_and_Delta_WECC_gen_build_",climate_impacts_included, day,".png", sep="")
cumulative_gen_build_sourceWECC_scenario_delta2 = cumulative_gen_build_sourceWECC_scenario_delta
cumulative_gen_build_sourceWECC_scenario_delta2$Baseline_Scenario <- ifelse(cumulative_gen_build_sourceWECC_scenario_delta2$Baseline_Scenario == "Baseline no CC", "Baseline", cumulative_gen_build_sourceWECC_scenario_delta2$Baseline_Scenario)

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""),  width=2300, height=700, res=100)

plot1 <- ggplot(data=subset(cumulative_gen_build_sourceWECC_scenario_delta2, Scenario == "ACCESS-1.0"), aes(x= Baseline_Scenario, y=CumulativeBaselineBuildGen_MW/1000, fill=gen_energy_source)) + 
  ggtitle(paste("Baseline capacity built", sep=", ")) +
  geom_bar(stat="identity") +
  labs(x="Scenario") + labs(y="Capacity built (GW)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(name="Energy source", values = energy_source_palette) +
  geom_hline(yintercept=0, color = "black") +
  theme(axis.text.y = element_text(size=22), axis.title.x = element_text(size=24), axis.text.x = element_text(size=22, angle = 90), axis.title.y=element_text(size=24), 
        legend.text = element_text(size = 22, nrow(2)), legend.title = element_text(size=24), legend.position="right", plot.title = element_text(size=26,hjust=0.1), 
        strip.text.x = element_text(size = 24), strip.text.y = element_text(size = 24), plot.margin = margin(10, 10, 10, 10))

plot2 <- ggplot(data=cumulative_gen_build_sourceWECC_scenario_delta, aes(x= Scenario, y=CumulativelDelta_BuildGen_MW/1000, fill=gen_energy_source)) + 
  ggtitle(paste("Change in cumulative capacity built relative to Baseline Scenario", sep=", ")) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::comma) +
  geom_point(size = 5, data=cumulative_gen_build_sourceWECC_scenario_delta, aes(x= Scenario, y=TotalCumulativelDelta_BuildGen_MW/1000)) + 
  labs(x="Scenario") + labs(y="Delta capacity built (GW)") +
  scale_fill_manual(name="Energy source", values = energy_source_palette) +
  geom_hline(yintercept=0, color = "black") +
  theme(axis.text.y = element_text(size=22), axis.title.x = element_text(size=24), axis.text.x = element_text(size=22, angle = 90), axis.title.y=element_text(size=24), 
        legend.text = element_text(size = 22, nrow(2)), legend.title = element_text(size=24), legend.position="none", plot.title = element_text(size=26,hjust=0.4), 
        strip.text.x = element_text(size = 24), strip.text.y = element_text(size = 24), plot.margin = margin(10, 10, 10, 10))

plot_grid(plot1, plot2, rel_widths=c(1,3), align = 'hv')

dev.off()  

#extract order of scenarios from lowest to highest cumulative built capacity
cumulative_buildgen_order <- c("Baseline no CC", levels(cumulative_gen_build_sourceWECC_scenario_delta$Scenario))

#Annual energy dispatch/annual energy generation/hydro and load change disaggregation##############################################

dispatch_annual_source_period_lz <- data.frame()

for (i in 1:length(run_name_list)){
  
  run_name <- run_name_list[[i]]
  scenario <- short_scenario_list[[i]]
  
  #read in the generation capacity builds and generation project information 
  dispatch_annual <- read.csv(file=paste(switch_input_dir,run_name,"//outputs//","dispatch_zonal_annual_summary.csv",sep=""),stringsAsFactors=F, header=TRUE)
  
  #also aggregating the bio gen_energy_source categories and renaming oil
  dispatch_annual$gen_energy_source[dispatch_annual$gen_energy_source == "Bio_Gas" | dispatch_annual$gen_energy_source =="Bio_Liquid" | dispatch_annual$gen_energy_source == "Bio_Solid"] <- "Biomass"
  dispatch_annual$gen_energy_source[dispatch_annual$gen_energy_source == "DistillateFuelOil" | dispatch_annual$gen_energy_source == "ResidualFuelOil"] <- "Oil"
  dispatch_annual$gen_energy_source[dispatch_annual$gen_energy_source == "Waste_Heat"] <- "Waste Heat"
  #specifying CAES gen_tech as CAES gen_energy_source source so it doesn't get lumped into regular gas, and battery storage as gen_energy_source source
  # dispatch_annual$gen_energy_source[dispatch_annual$gen_tech == "Compressed_Air_Energy_Storage"] <- "Compressed_Air_Energy_Storage"
  dispatch_annual$gen_energy_source[dispatch_annual$gen_tech == "Battery_Storage"] <- "Battery Storage"
  
  #aggregate by energy source
  dispatch_annual_sourceAgg <- dispatch_annual %>% group_by(gen_load_zone,period,gen_energy_source) %>% summarize(Energy_GWh_typical_yr = sum(Energy_GWh_typical_yr), VariableCost_per_yr = sum(VariableOMCost_per_yr))
  
  dispatch_annual_total <- dispatch_annual_sourceAgg %>% group_by(gen_load_zone,period) %>% summarize(Total_annual_GWh = sum(Energy_GWh_typical_yr))
  
  #joining total annual generation for each load zone with annual gen by load zone and source
  dispatch_annual_sourceAgg <- left_join(dispatch_annual_sourceAgg,dispatch_annual_total,c("gen_load_zone"="gen_load_zone","period"="period"))
  
  dispatch_annual_sourceAgg$Scenario <- scenario
  
  #generation by load zone and period for each scenario
  dispatch_annual_source_period_lz <- rbind(dispatch_annual_source_period_lz, dispatch_annual_sourceAgg)
  
}

#output csv of annual dispatch energy by load zone by period for each scenario
write.csv(x = dispatch_annual_source_period_lz, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,climate_impacts_included, "Lz_annual_dispatch.csv",sep=""), row.names = F)

#load zone delta between baseline and CC scenarios

baseline <- scenario_name_short1

dispatch_annual_source_period_lz_baseline <- dispatch_annual_source_period_lz %>% filter(Scenario == baseline)
dispatch_annual_source_period_lz_baseline <- rename(dispatch_annual_source_period_lz_baseline, Baseline_Scenario = Scenario, Energy_GWh_typical_yr_baseline = Energy_GWh_typical_yr, Total_annual_GWh_baseline = Total_annual_GWh)

dispatch_annual_source_period_lz_delta0 <- dispatch_annual_source_period_lz %>% filter(Scenario != baseline)
dispatch_annual_source_period_lz_delta <- left_join(dispatch_annual_source_period_lz_delta0, dispatch_annual_source_period_lz_baseline, c("period"="period", "gen_load_zone" = "gen_load_zone","gen_energy_source" = "gen_energy_source"))

dispatch_annual_source_period_lz_delta$Delta_Energy_GWh <-dispatch_annual_source_period_lz_delta$Energy_GWh_typical_yr - dispatch_annual_source_period_lz_delta$Energy_GWh_typical_yr_baseline

dispatch_annual_source_period_lz_delta$Energy_perc_change_from_baseline <- round(dispatch_annual_source_period_lz_delta$Delta_Energy_GWh/dispatch_annual_source_period_lz_delta$Energy_GWh_typical_yr_baseline,3)

#output csv of DELTA load zone capacity online by period for each scenario
write.csv(x = dispatch_annual_source_period_lz_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, climate_impacts_included, "DELTA_Lz_dispatch.csv",sep=""), row.names = F)

#total WECC dispatch by period for each scenario
dispatch_annual_source_period_WECC <- dispatch_annual_source_period_lz %>% group_by(Scenario, period, gen_energy_source) %>% summarize(Energy_GWh_typical_yr = sum(Energy_GWh_typical_yr))

#output csv of total WECC generation by period for each scenario
write.csv(x = dispatch_annual_source_period_WECC, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, climate_impacts_included,"WECC_annual_dispatchLONG.csv",sep=""), row.names = F)

#reading in the laod from all scenarios to join with dispatch
loads_all_scenarios <- data.frame()

for (i in 1:length(run_name_list)){
  
  run_name <- run_name_list[[i]]
  scenario <- short_scenario_list[[i]]

  #read in the load balance by load zone and timestamp
  load_balance <- read.csv(file=paste(switch_input_dir,run_name,"//outputs//","load_balance.csv",sep=""),stringsAsFactors=F, header=TRUE)
  
  #parse time points to human readable date and components
  load_balance$Month <- as.integer(substr(load_balance$timestamp, 5, 6))
  load_balance$Year <- as.integer(substr(load_balance$timestamp, 1, 4))
  load_balance$Day <- as.integer(substr(load_balance$timestamp, 7, 8))
  load_balance$Hour_UTC <- as.integer(substr(load_balance$timestamp, 9, 10))
  #constructing datetime in UTC (time zone of time points)
  load_balance$date_time_UTC <- ymd_hms(paste(paste(load_balance$Year,load_balance$Month,load_balance$Day, sep="-"), 
                                              paste(load_balance$Hour_UTC,"00","00",sep=":"),sep=" "), tz = "UTC")
  
  #read in the timestamp weighting
  timeseries <- read.csv(file=paste(switch_input_dir,run_name,"//inputs//","timeseries.csv",sep=""),stringsAsFactors=F, header=TRUE)
  
  #read in the timestamp weighting
  timepoints <- read.csv(file=paste(switch_input_dir,run_name,"//inputs//","timepoints.csv",sep=""),stringsAsFactors=F, header=TRUE)
  
  timepoints_ts <- left_join(timepoints, timeseries, c("timeseries"="TIMESERIES"))
  
  load_balance_tps <- left_join(load_balance, timepoints_ts, c("timestamp" = "timestamp"))
  
  #calculate weighted loads scaled to the year
  load_balance_tps$zone_demand_mw_scaled_to_year <- load_balance_tps$zone_demand_mw * load_balance_tps$ts_duration_of_tp * load_balance_tps$ts_scale_to_period/5
  load_balance_tps$ZoneTotalCentralDispatch_scaled_to_year <- load_balance_tps$ZoneTotalCentralDispatch * load_balance_tps$ts_duration_of_tp * load_balance_tps$ts_scale_to_period/5
  load_balance_tps$TXPowerNet_scaled_to_year <- load_balance_tps$TXPowerNet * load_balance_tps$ts_duration_of_tp * load_balance_tps$ts_scale_to_period/5
  load_balance_tps$StorageNetCharge_scaled_to_year <- load_balance_tps$StorageNetCharge * load_balance_tps$ts_duration_of_tp * load_balance_tps$ts_scale_to_period/5
  
  load_balance_tps$Scenario <- scenario
  
  #capacity online by load zone and period for each scenario
  loads_all_scenarios <- rbind(loads_all_scenarios, load_balance_tps)
  
}

#output csv of load balance by load zone and period for each scenario
write.csv(x = loads_all_scenarios, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, climate_impacts_included,"Lz_load_balance.csv",sep=""), row.names = F)

#sum load zone total annual load by period and scenario 
loads_annual_period_lz <- loads_all_scenarios %>% group_by(load_zone, Year, ts_period, Scenario) %>% summarize(zone_demand_GWh = sum(zone_demand_mw_scaled_to_year)/1000, 
                                                                                                      ZoneTotalCentralDispatch_GWh = sum(ZoneTotalCentralDispatch_scaled_to_year)/1000,
                                                                                                      TXPowerNet_GWh = sum(TXPowerNet_scaled_to_year)/1000,
                                                                                                      StorageNetCharge_GWh = sum(StorageNetCharge_scaled_to_year)/1000)

#output csv of total WECC load balance by period for each scenario
write.csv(x = loads_annual_period_lz, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,climate_impacts_included,"Lz_annual_load_balance.csv",sep=""), row.names = F)


#sum WECC total annual load by period and scenario 
loads_annual_period_WECC <- loads_all_scenarios %>% group_by(Year, ts_period, Scenario) %>% summarize(zone_demand_GWh = sum(zone_demand_mw_scaled_to_year)/1000, 
                                                                                                      ZoneTotalCentralDispatch_GWh = sum(ZoneTotalCentralDispatch_scaled_to_year)/1000,
                                                                                                      TXPowerNet_GWh = sum(TXPowerNet_scaled_to_year)/1000,
                                                                                                      StorageNetCharge_GWh = sum(StorageNetCharge_scaled_to_year)/1000)

#output csv of total WECC load balance by period for each scenario
write.csv(x = loads_annual_period_WECC, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,climate_impacts_included, "WECC_annual_load_balance.csv",sep=""), row.names = F)

#join load balance with generation for all WECC

loads_annual_period_WECC_long <- gather(loads_annual_period_WECC, energy_category, Energy_GWh_typical_yr, 4:7)
loads_annual_period_WECC_long <- loads_annual_period_WECC_long %>% dplyr::select(Scenario, ts_period, energy_category, Energy_GWh_typical_yr)
loads_annual_period_WECC_long$Year <- NULL

dispatch_annual_source_period_WECC2 <- dispatch_annual_source_period_WECC
dispatch_annual_source_period_WECC2 <- rename(dispatch_annual_source_period_WECC2, energy_category = gen_energy_source, ts_period = period)

dispatch_annual_source_period_WECC3 <- rbind(dispatch_annual_source_period_WECC2, loads_annual_period_WECC_long)

# plot of total WECC annual dispatch by energy source for each period BASELINE SCENARIO
plot_file_name <- paste("SI_WECC_annual_energyBASELINE", ".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=700, height=700, res=100)

plot <- ggplot() + geom_bar(data = subset(dispatch_annual_source_period_WECC, Scenario == baseline), 
                            aes(x = period, y = Energy_GWh_typical_yr/1000, group=gen_energy_source, fill=gen_energy_source), stat="identity") + 
  # geom_text(data=subset(dispatch_annual_source_period_WECC, Scenario == baseline & round(Energy_GWh_typical_yr/1000,0) > 5), aes(x = period, y = Energy_GWh_typical_yr/1000, group=gen_energy_source, label=round(Energy_GWh_typical_yr/1000,0)) , size = 5, position = position_stack(vjust = 0.5)) + 
  labs(x="Investment Period") + labs(y="Energy (TWh) per year")  +
  ggtitle(paste("WECC annual dispatch by period, Baseline scenario", sep="")) +
  scale_fill_manual(name="Energy Source", values = energy_source_palette) +
  theme(axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.text.x = element_text(size=12, angle = 45), axis.title.y=element_text(size=12), 
        legend.text = element_text(size = 9), legend.title = element_text(size=9), legend.position="bottom", plot.title = element_text(size=13.5,hjust=0.5), 
        strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 16)) +
  guides(fill = guide_legend(nrow = 2))

print(plot)

dev.off()  

#making wide table of generation by energy source and scenario and period
dispatch_annual_source_period_WECC_wide <- spread(dispatch_annual_source_period_WECC, gen_energy_source, Energy_GWh_typical_yr)

dispatch_annual_source_period_WECC_wide[is.na(dispatch_annual_source_period_WECC_wide)] <- 0

write.csv(x = dispatch_annual_source_period_WECC_wide, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,  climate_impacts_included, "WECC_annual_dispatch.csv",sep=""))

#load zone delta between baseline and CC scenarios

baseline <- scenario_name_short1

dispatch_source_period_lz_baseline <- dispatch_annual_source_period_lz %>% filter(Scenario == baseline)
dispatch_source_period_lz_baseline <- rename(dispatch_source_period_lz_baseline, Baseline_Scenario = Scenario, Energy_GWh_typical_yr_Baseline = Energy_GWh_typical_yr, Total_annual_GWh_baseline = Total_annual_GWh, VariableCost_per_yr_baseline = VariableCost_per_yr)

dispatch_annual_source_period_lz_delta0 <- dispatch_annual_source_period_lz %>% filter(Scenario != baseline)
dispatch_annual_source_period_lz_delta <- left_join(dispatch_annual_source_period_lz_delta0, dispatch_source_period_lz_baseline, c("period"="period", "gen_load_zone" = "gen_load_zone","gen_energy_source" = "gen_energy_source"))

dispatch_annual_source_period_lz_delta$Delta_AnnualGen_GWh <-dispatch_annual_source_period_lz_delta$Energy_GWh_typical_yr - dispatch_annual_source_period_lz_delta$Energy_GWh_typical_yr_Baseline

dispatch_annual_source_period_lz_delta$AnnualGen_perc_change_from_baseline <- round(dispatch_annual_source_period_lz_delta$Delta_AnnualGen_GWh/dispatch_annual_source_period_lz_delta$Energy_GWh_typical_yr_Baseline,3)

#output csv of DELTA load zone energy by period for each scenario
write.csv(x = dispatch_annual_source_period_lz_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,climate_impacts_included, "DELTA_Lz_annual_dispatch.csv",sep=""), row.names = F)

#WECC wide delta between baseline and CC scenarios
dispatch_annual_source_period_WECC_delta <- dispatch_annual_source_period_lz_delta %>% 
  group_by(Scenario, Baseline_Scenario, period, gen_energy_source) %>% 
  summarize(Energy_GWh_typical_yr = sum(Energy_GWh_typical_yr), Energy_GWh_typical_yr_Baseline = sum(Energy_GWh_typical_yr_Baseline))

# get percentage diff from baseline and total WECC wide delta
dispatch_annual_source_period_WECC_delta$Delta_AnnualGen_GWh <- dispatch_annual_source_period_WECC_delta$Energy_GWh_typical_yr - dispatch_annual_source_period_WECC_delta$Energy_GWh_typical_yr_Baseline
dispatch_annual_source_period_WECC_delta$AnnualGen_perc_change_from_baseline <- round(dispatch_annual_source_period_WECC_delta$Delta_AnnualGen_GWh/dispatch_annual_source_period_WECC_delta$Energy_GWh_typical_yr_Baseline,3)
dispatch_annual_source_period_WECC_delta$AnnualGen_perc_change_from_baseline[is.nan(dispatch_annual_source_period_WECC_delta$AnnualGen_perc_change_from_baseline)] <- NA
#removing NA
dispatch_annual_source_period_WECC_delta <- dispatch_annual_source_period_WECC_delta[complete.cases(dispatch_annual_source_period_WECC_delta[ , 8]),]

#output csv of DELTA WECC generation by period for each scenario
write.csv(x = dispatch_annual_source_period_WECC_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,climate_impacts_included, "DELTA_WECC_annual_dispatch.csv",sep=""), row.names = F)

#Plot of Deltas in generation by source

plot_file_name <- paste("SI_Delta_5x3_WECC_annual_dispatch_", climate_impacts_included,".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=1100, height=900, res=100)

plot <- ggplot(data=subset(dispatch_annual_source_period_WECC_delta, Scenario != baseline), aes(x=period, y=Delta_AnnualGen_GWh/1000,
                                                                                                fill=gen_energy_source,
                                                                                                label=paste(round(AnnualGen_perc_change_from_baseline*100,0),"%",sep=""))) +
  ggtitle(paste("Change in annual generation by period relative to baseline scenario",climate_impacts_included, sep=", ")) +
  geom_bar(stat="identity") +
   labs(x="Investment Period") + labs(y="Energy generation relative to Baseline scenario (TWh)") +
  scale_fill_manual(name="Energy Source", values = energy_source_palette) +
  facet_wrap( ~ Scenario, nrow=3) +
  geom_hline(yintercept=0, color = "black") +
  theme(axis.text.y = element_text(size=12), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12, angle = 45), axis.title.y=element_text(size=14),
        legend.text = element_text(size = 12), legend.title = element_text(size=12), legend.position="bottom", plot.title = element_text(size=13.5,hjust=0.5),
        strip.text.x = element_text(size = 15), strip.text.y = element_text(size = 15))

print(plot)

dev.off()

# #Delta annual generation by energy source 2050 only
dispatch_annual_source_WECC_delta2050 <- dispatch_annual_source_period_WECC_delta %>% filter(period == 2050) %>% group_by(period, Scenario) %>% mutate(TotalDelta_AnnualGen_GWh = sum(Delta_AnnualGen_GWh))

dispatch_annual_source_WECC_delta2050 <- ungroup(dispatch_annual_source_WECC_delta2050) %>% mutate(Scenario = fct_reorder(Scenario, TotalDelta_AnnualGen_GWh, max, .desc = FALSE))

##FIGURE 3B
#combined plot of 2050BASELINE and DELTA ANNUAL GENERATION all scenarios, with scenarios ordered by lowest to highest cumulative built CAPACITY (LEVELS FROM FIGURE 3A)
library(cowplot)

cumulative_buildgen_order <- c("Baseline no CC", levels(cumulative_gen_build_sourceWECC_scenario_delta$Scenario))

#reorder factors based on lowest to highest built CAPACITY levels (FIGURE 3A scenario order)
dispatch_annual_source_WECC_delta2050$Scenario <- factor(dispatch_annual_source_WECC_delta2050$Scenario, levels = cumulative_buildgen_order)

dispatch_annual_source_period_WECC2 = dispatch_annual_source_period_WECC
dispatch_annual_source_period_WECC2$Scenario <- ifelse(dispatch_annual_source_period_WECC2$Scenario == "Baseline no CC", "Baseline", dispatch_annual_source_period_WECC2$Scenario)

plot_file_name <- paste("Fig3B_2050Baseline_and_DELTA_WECC_annual_energy",climate_impacts_included,day, ".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=2300, height=700, res=100)

plot1 <- ggplot() + geom_bar(data = subset(dispatch_annual_source_period_WECC2, Scenario == "Baseline" & period == 2050), 
                             aes(x = Scenario, y = Energy_GWh_typical_yr/1000, group=gen_energy_source, fill=gen_energy_source), stat="identity") + 
  labs(x="Scenario") + labs(y="Generation (TWh)")  +
  scale_y_continuous(labels = scales::comma) +
  ggtitle(paste("2050 Baseline generation", sep="")) +
  scale_fill_manual(name="Energy source", values = energy_source_palette) +
  theme(axis.text.y = element_text(size=22), axis.title.x = element_text(size=24), axis.text.x = element_text(size=22, angle = 90), axis.title.y=element_text(size=24), 
        legend.text = element_text(size = 22, nrow(2)), legend.title = element_text(size=24), legend.position="right", plot.title = element_text(size=26,hjust=0.2), 
        strip.text.x = element_text(size = 24), strip.text.y = element_text(size = 24), plot.margin = margin(10, 10, 10, 10))

plot2 <- ggplot(data=subset(dispatch_annual_source_WECC_delta2050, Scenario != baseline & period == 2050), aes(x=Scenario, y=Delta_AnnualGen_GWh/1000,
                                                                                                               fill=gen_energy_source,
                                                                                                               label=paste(round(AnnualGen_perc_change_from_baseline*100,0),"%",sep=""))) +
  ggtitle(paste("Change in 2050 generation relative to Baseline Scenario", sep=", ")) +
  geom_hline(yintercept=0) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::comma) +
  geom_point(data=subset(dispatch_annual_source_WECC_delta2050, Scenario != baseline & period == 2050), aes(x=Scenario, y=TotalDelta_AnnualGen_GWh/1000), size =5) +
  labs(x="Scenario") + labs(y="Delta generation (TWh)") +
  scale_fill_manual(name="Energy source", values = energy_source_palette) +
  geom_hline(yintercept=0, color = "black") +
  theme(axis.text.y = element_text(size=22), axis.title.x = element_text(size=24), axis.text.x = element_text(size=22, angle = 90), axis.title.y=element_text(size=24), 
        legend.text = element_text(size = 22, nrow(2)), legend.title = element_text(size=24), legend.position="none", plot.title = element_text(size=26,hjust=0.5), 
        strip.text.x = element_text(size = 24), strip.text.y = element_text(size = 24), plot.margin = margin(10, 20, 10, 10))

plot_grid(plot1, plot2, rel_widths=c(1,3), align = 'hv')

dev.off()  


#making wide table of generation by energy source and scenario and period
gen_load_annual_source_period_WECC_wide<- spread(dispatch_annual_source_period_WECC3, energy_category, Energy_GWh_typical_yr)

gen_load_annual_source_period_WECC_wide[is.na(gen_load_annual_source_period_WECC_wide)] <- 0

write.csv(x = gen_load_annual_source_period_WECC_wide, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,climate_impacts_included, "WECC_annual_gen_load.csv",sep=""))

#WECC delta between baseline and CC scenarios
#making loads positive to be easier for interpretation
dispatch_annual_source_period_WECC4 <- dispatch_annual_source_period_WECC3
dispatch_annual_source_period_WECC4$Energy_GWh_typical_yr <- ifelse(dispatch_annual_source_period_WECC4$energy_category == "zone_demand_GWh", 
                                                                    dispatch_annual_source_period_WECC4$Energy_GWh_typical_yr * (-1), dispatch_annual_source_period_WECC4$Energy_GWh_typical_yr)

baseline <- scenario_name_short1

gen_load_annual_source_period_WECC_wide_baseline <- dispatch_annual_source_period_WECC4 %>% filter(Scenario == baseline)
gen_load_annual_source_period_WECC_wide_baseline <- rename(gen_load_annual_source_period_WECC_wide_baseline, Baseline_Scenario = Scenario, Energy_GWh_typical_yr_Baseline = Energy_GWh_typical_yr)

gen_load_annual_source_period_WECC_delta0 <- dispatch_annual_source_period_WECC4 %>% filter(Scenario != baseline)
gen_load_annual_source_period_WECC_delta <- left_join(gen_load_annual_source_period_WECC_delta0, gen_load_annual_source_period_WECC_wide_baseline, c("ts_period"="ts_period", "energy_category" = "energy_category"))

gen_load_annual_source_period_WECC_delta$Delta_AnnualGen_GWh <-gen_load_annual_source_period_WECC_delta$Energy_GWh_typical_yr - gen_load_annual_source_period_WECC_delta$Energy_GWh_typical_yr_Baseline

gen_load_annual_source_period_WECC_delta$AnnualGen_perc_change_from_baseline <- round(gen_load_annual_source_period_WECC_delta$Delta_AnnualGen_GWh/gen_load_annual_source_period_WECC_delta$Energy_GWh_typical_yr_Baseline,3)

#output csv of DELTA WECC annual dispatch by period for each scenario
write.csv(x = gen_load_annual_source_period_WECC_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day, climate_impacts_included, "DELTA_WECC_annual_dispatch.csv",sep=""), row.names = F)

load_hydro_colors <- c("#0072B2","#E69F00")

labels <- c(zone_demand_GWh = "Load", Water = "Hydropower")


###DELTA HYROPOWER AND LOAD WECC ANNUAL ENERGY PLOT
#plot the difference in load and the difference in hydropower generation by year for each scenario
plot_file_name <- paste("SI_facet_plot","DELTA_WECC_load_hydro_", climate_impacts_included,".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=2600, height=900, res=100)

plot <-ggplot(subset(gen_load_annual_source_period_WECC_delta, energy_category %in% c("zone_demand_GWh","Water")), 
              aes(x = factor(ts_period), y = Delta_AnnualGen_GWh, group=energy_category, fill=energy_category)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(x="Year") + labs(y="Delta Energy (GWh)")  +
  ggtitle(paste("Change in annual load and hydropower compared to baseline",climate_impacts_included, sep=", ")) +
  scale_fill_manual(values = load_hydro_colors) +
  facet_grid(energy_category ~ Scenario, scales="free_y", labeller = labeller(energy_category=labels)) +
  theme(axis.text.y = element_text(size=14), axis.title.x = element_text(size=16), axis.text.x = element_text(size=14, angle = 90), axis.title.y=element_text(size=16), 
        legend.text = element_text(size = 14, nrow(2)), legend.title = element_text(size=14), legend.position="bottom", plot.title = element_text(size=18,hjust=0.5), 
        strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 16))
print(plot)

dev.off()

#####DISAGGREGATION OF SUPPPLY AND DEMAND CHANGES FOR ENERGY for FIGURE 2A

if(climate_impacts_included == "Cooling + Hydropower and Water Load Scenarios"){
  ##read in the delta loads from the hydro and water load only scenarios and stack the load differences onto the combined cooling and load difference
  
  climate_impacts_list <- c("Hydropower and Water Load Scenarios","Cooling Load Scenarios","Cooling + Hydropower and Water Load Scenarios")
  
  hydro_load_delta_disagg <- data.frame()
  
  for(i in 1:length(climate_impacts_list)){
    
    climate_impacts <- climate_impacts_list[i]
    
    gen_load_annual_source_period_WECC_delta <- read.csv(file=paste(switch_output_dir,switch_run_dir, results_dir, climate_impacts,"/",day,climate_impacts,"DELTA_WECC_annual_dispatch.csv",sep=""),stringsAsFactors=F, header=TRUE)
    gen_load_annual_source_period_WECC_delta$climate_impacts_included <- climate_impacts
    
    hydro_load_delta_disagg <- rbind(hydro_load_delta_disagg,gen_load_annual_source_period_WECC_delta )  
  }
  
  hydro_load_delta_disagg$energy_category_disagg <- hydro_load_delta_disagg$energy_category
  
  hydro_load_delta_disagg$energy_category_disagg <- ifelse(hydro_load_delta_disagg$energy_category=="zone_demand_GWh" & hydro_load_delta_disagg$climate_impacts_included == "Hydropower and Water Load Scenarios", "Water Load", hydro_load_delta_disagg$energy_category)
  hydro_load_delta_disagg$energy_category_disagg <- ifelse(hydro_load_delta_disagg$energy_category=="zone_demand_GWh" & hydro_load_delta_disagg$climate_impacts_included == "Cooling Load Scenarios", "Cooling Load", hydro_load_delta_disagg$energy_category)
  hydro_load_delta_disagg$energy_category_disagg <- ifelse(hydro_load_delta_disagg$energy_category=="zone_demand_GWh" & hydro_load_delta_disagg$climate_impacts_included == "Cooling + Hydropower and Water Load Scenarios", "Cooling + Water Load", hydro_load_delta_disagg$energy_category)
  
  #just keeping the 2 load components and one hydropower generation delta
  hydro_load_delta_disagg_filter <- hydro_load_delta_disagg %>% filter(energy_category %in% c("zone_demand_GWh", "Water"))
        
  hydro_load_delta_disagg_filter2 <-hydro_load_delta_disagg_filter[!(hydro_load_delta_disagg_filter$energy_category_disagg == "Cooling + Water Load"),]
  hydro_load_delta_disagg_filter3 <-hydro_load_delta_disagg_filter2[!(hydro_load_delta_disagg_filter2$climate_impacts_included == "Hydropower and Water Load Scenarios" & hydro_load_delta_disagg_filter2$energy_category == "Water"),]
  hydro_load_delta_disagg_filter4 <-hydro_load_delta_disagg_filter3[!(hydro_load_delta_disagg_filter3$climate_impacts_included == "Cooling Load Scenarios" & hydro_load_delta_disagg_filter3$energy_category == "Water"),]
  
  hydro_load_delta_disagg_filter4$energy_category_disagg <- ifelse(hydro_load_delta_disagg_filter4$energy_category=="zone_demand_GWh" & hydro_load_delta_disagg_filter4$climate_impacts_included == "Hydropower and Water Load Scenarios", "Water Load", hydro_load_delta_disagg_filter4$energy_category_disagg )
  hydro_load_delta_disagg_filter4$energy_category_disagg <- ifelse(hydro_load_delta_disagg_filter4$energy_category=="zone_demand_GWh" & hydro_load_delta_disagg_filter4$climate_impacts_included == "Cooling Load Scenarios", "Cooling Load", hydro_load_delta_disagg_filter4$energy_category_disagg )
  
  #export for use in other plots
  write.csv(x = hydro_load_delta_disagg_filter4, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,climate_impacts_included, "DELTA_WECC_hydro_load_disagg.csv",sep=""), row.names = F)
  
  load_hydro_colors2 <- c("#D55E00","#0072B2","#E69F00")
  
  labels <- c(zone_demand_GWh = "Demand", Water = "Supply")
  hydro_load_delta_disagg_filter4$energy_category <- factor(hydro_load_delta_disagg_filter4$energy_category, levels = c("zone_demand_GWh", "Water"))
  
  
  #disaggregation of hydropower and load changes by scenario, all periods
  plot_file_name <- paste("SI_facet_plot","Disagg_DELTA_WECC_load_hydro_", climate_impacts_included,".png", sep="")
  
  png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=2600, height=900, res=100)
  
  plot <-ggplot(data = hydro_load_delta_disagg_filter4, 
                aes(x = factor(ts_period), y = Delta_AnnualGen_GWh, group=energy_category_disagg, fill=energy_category_disagg)) + 
    geom_bar(stat="identity") + 
    labs(x="Year") + labs(y="Delta Energy (GWh)")  +
    ggtitle(paste("Change in annual load and hydropower compared to Baseline Scenario", sep=", ")) +
    scale_fill_manual(values = load_hydro_colors2, name = "Energy Category", labels = c("Cooling Load", "Hydropower", "Water Load")) +
    facet_grid(energy_category ~ Scenario, scales="free_y", labeller = labeller(energy_category=labels)) +
    theme(axis.text.y = element_text(size=14), axis.title.x = element_text(size=16), axis.text.x = element_text(size=14, angle = 90), axis.title.y=element_text(size=16), 
          legend.text = element_text(size = 14, nrow(2)), legend.title = element_text(size=14), legend.position="bottom", plot.title = element_text(size=18,hjust=0.5), 
          strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 16))
  print(plot)
  
  dev.off()
  
  #FIGURE 2A - ordered from lowest to highest energy imbalance across load and hydro changes
  #disaggegation of hydropower and load changes by scenario, 2050 only
  plot_file_name <- paste("Fig2A_facet_plot","Disagg2050_DELTA_WECC_load_hydro_", climate_impacts_included,".png", sep="")
  
  hydro_load_delta_disagg_filter4$Adj_Delta_AnnualGen_GWh <- hydro_load_delta_disagg_filter4$Delta_AnnualGen_GWh
  hydro_load_delta_disagg_filter4$Adj_Delta_AnnualGen_GWh <- ifelse(hydro_load_delta_disagg_filter4$energy_category_disagg == "Water", -1*hydro_load_delta_disagg_filter4$Delta_AnnualGen_GWh, hydro_load_delta_disagg_filter4$Delta_AnnualGen_GWh)
  
  
  hydro_load_delta_disagg_filter5 <- hydro_load_delta_disagg_filter4 %>% filter(ts_period == 2050) %>% group_by(ts_period, Scenario) %>% mutate(TotalImbalance_GWh = sum(Adj_Delta_AnnualGen_GWh))
  
  
  hydro_load_delta_disagg_filter5 <- ungroup(hydro_load_delta_disagg_filter5) %>% mutate(Scenario = fct_reorder(Scenario, TotalImbalance_GWh, max, .desc = FALSE))
  
  load_hydro_colors2 <- c("#D55E00","#0072B2","#E69F00")
  
  
  png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=1800, height=700, res=100)
  
  plot <-ggplot(data = subset(hydro_load_delta_disagg_filter5, ts_period == 2050), 
                aes(x = Scenario, y = Delta_AnnualGen_GWh/1000, group=energy_category_disagg, fill=energy_category_disagg)) + 
    geom_bar(stat="identity") + 
    geom_hline(yintercept = 0) +
    labs(x="Scenario") + labs(y="Delta energy (TWh)")  +
    ggtitle(paste("Decomposition of 2050 changes in energy supply and demand relative to Baseline Scenario", sep=", ")) +
    scale_fill_manual(values = c("#0072B2","#E69F00","#D55E00"), name = "Energy category", breaks = c("Water", "Cooling Load", "Water Load"), labels = c("Hydropower","Cooling/Heating load", "Water load" )) +
    facet_grid(energy_category ~ ., scales="free_y", labeller = labeller(energy_category=labels)) +
    theme(axis.text.y = element_text(size=22), axis.title.x = element_text(size=24), axis.text.x = element_text(size=22, angle = 90), axis.title.y=element_text(size=24), 
          legend.text = element_text(size = 22, nrow(2)), legend.title = element_text(size=24), legend.position="none", plot.title = element_text(size=26,hjust=0.5), 
          strip.text.x = element_text(size = 24), strip.text.y = element_text(size = 24), plot.margin = margin(10, 20, 10, 10), panel.spacing = unit(1, "lines"))
    print(plot)
  

  
  dev.off()
  
  
  #LOAD ZONE LEVEL disaggregation of LOAD AND HYDROPOWER changes#
  
  #total WECC dispatch by period for each scenario
  dispatch_annual_source_period_WECC <- dispatch_annual_source_period_lz %>% group_by(Scenario, period, gen_energy_source) %>% summarize(Energy_GWh_typical_yr = sum(Energy_GWh_typical_yr))
  
  
  #WECC delta between baseline and CC scenarios
  #making loads positive to be easier for interpretation
  #join load balance with generation for all WECC
  
  loads_annual_period_lz_long <- gather(loads_annual_period_lz, energy_category, Energy_GWh_typical_yr, 5:8)
  loads_annual_period_lz_long <- loads_annual_period_lz_long %>% dplyr::select(load_zone, Scenario, ts_period, energy_category, Energy_GWh_typical_yr)
  loads_annual_period_lz_long$Year <- NULL
  
  dispatch_annual_source_period_lz2 <- dispatch_annual_source_period_lz
  dispatch_annual_source_period_lz2 <- rename(dispatch_annual_source_period_lz2, energy_category = gen_energy_source, ts_period = period, load_zone = gen_load_zone)
  dispatch_annual_source_period_lz2$VariableCost_per_yr <- NULL
  dispatch_annual_source_period_lz2$Total_annual_GWh <- NULL
  
  dispatch_annual_source_period_lz3 <- rbind(dispatch_annual_source_period_lz2, loads_annual_period_lz_long)
  
  dispatch_annual_source_period_lz4 <- dispatch_annual_source_period_lz3
  dispatch_annual_source_period_lz4$Energy_GWh_typical_yr <- ifelse(dispatch_annual_source_period_lz4$energy_category == "zone_demand_GWh", 
                                                                    dispatch_annual_source_period_lz4$Energy_GWh_typical_yr * (-1), dispatch_annual_source_period_lz4$Energy_GWh_typical_yr)
  
  baseline <- scenario_name_short1
  
  gen_load_annual_source_period_lz_wide_baseline <- dispatch_annual_source_period_lz4 %>% filter(Scenario == baseline)
  gen_load_annual_source_period_lz_wide_baseline <- rename(gen_load_annual_source_period_lz_wide_baseline, Baseline_Scenario = Scenario, Energy_GWh_typical_yr_Baseline = Energy_GWh_typical_yr)
  
  gen_load_annual_source_period_lz_delta0 <- dispatch_annual_source_period_lz4 %>% filter(Scenario != baseline)
  gen_load_annual_source_period_lz_delta <- left_join(gen_load_annual_source_period_lz_delta0, gen_load_annual_source_period_lz_wide_baseline, c("ts_period"="ts_period", 
                                                                                                                                                 "energy_category" = "energy_category", "load_zone"="load_zone"))
  
  gen_load_annual_source_period_lz_delta$Delta_AnnualGen_GWh <-gen_load_annual_source_period_lz_delta$Energy_GWh_typical_yr - gen_load_annual_source_period_lz_delta$Energy_GWh_typical_yr_Baseline
  
  gen_load_annual_source_period_lz_delta$AnnualGen_perc_change_from_baseline <- round(gen_load_annual_source_period_lz_delta$Delta_AnnualGen_GWh/gen_load_annual_source_period_lz_delta$Energy_GWh_typical_yr_Baseline,3)
  
  # gen_load_annual_source_period_lz_delta <- rename(gen_load_annual_source_period_lz_delta, load_zone=gen_load_zone, period = ts_period, energy_category= gen_energy_source, )
  
  #output csv of DELTA load zone capacity online by period for each scenario
  write.csv(x = gen_load_annual_source_period_lz_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day,climate_impacts_included, "DELTA_lz_annual_dispatch.csv",sep=""), row.names = F)
  
  
  ##read in the delta loads from the hydro and water load only scenarios and stack the load differences onto the combined cooling and load difference
  
  climate_impacts_list <- c("Hydropower and Water Load Scenarios","Cooling Load Scenarios","Cooling + Hydropower and Water Load Scenarios")
  
  lz_hydro_load_delta_disagg <- data.frame()
  
  for(i in 1:length(climate_impacts_list)){
    
    climate_impacts <- climate_impacts_list[i]
    
    gen_load_annual_source_period_lz_delta <- read.csv(file=paste(switch_output_dir,switch_run_dir, results_dir, climate_impacts,"/",day,climate_impacts,"DELTA_lz_annual_dispatch.csv",sep=""),stringsAsFactors=F, header=TRUE)
    gen_load_annual_source_period_lz_delta$climate_impacts_included <- climate_impacts
    
    lz_hydro_load_delta_disagg <- rbind(lz_hydro_load_delta_disagg,gen_load_annual_source_period_lz_delta )  
  }
  
  lz_hydro_load_delta_disagg$energy_category_disagg <- lz_hydro_load_delta_disagg$energy_category
  
  lz_hydro_load_delta_disagg$energy_category_disagg <- ifelse(lz_hydro_load_delta_disagg$energy_category=="zone_demand_GWh" & lz_hydro_load_delta_disagg$climate_impacts_included == "Hydropower and Water Load Scenarios", "Water Load", lz_hydro_load_delta_disagg$energy_category)
  lz_hydro_load_delta_disagg$energy_category_disagg <- ifelse(lz_hydro_load_delta_disagg$energy_category=="zone_demand_GWh" & lz_hydro_load_delta_disagg$climate_impacts_included == "Cooling Load Scenarios", "Cooling Load", lz_hydro_load_delta_disagg$energy_category)
  lz_hydro_load_delta_disagg$energy_category_disagg <- ifelse(lz_hydro_load_delta_disagg$energy_category=="zone_demand_GWh" & lz_hydro_load_delta_disagg$climate_impacts_included == "Cooling + Hydropower and Water Load Scenarios", "Cooling + Water Load", lz_hydro_load_delta_disagg$energy_category)
  
  #just keeping the 2 load components and one hydropower generation delta
  lz_hydro_load_delta_disagg_filter <- lz_hydro_load_delta_disagg %>% filter(energy_category %in% c("zone_demand_GWh", "Water"))
  
  lz_hydro_load_delta_disagg_filter2 <-lz_hydro_load_delta_disagg_filter[!(lz_hydro_load_delta_disagg_filter$energy_category_disagg == "Cooling + Water Load"),]
  lz_hydro_load_delta_disagg_filter3 <-lz_hydro_load_delta_disagg_filter2[!(lz_hydro_load_delta_disagg_filter2$climate_impacts_included == "Hydropower and Water Load Scenarios" & lz_hydro_load_delta_disagg_filter2$energy_category == "Water"),]
  lz_hydro_load_delta_disagg_filter4 <-lz_hydro_load_delta_disagg_filter3[!(lz_hydro_load_delta_disagg_filter3$climate_impacts_included == "Cooling Load Scenarios" & lz_hydro_load_delta_disagg_filter3$energy_category == "Water"),]
  
  lz_hydro_load_delta_disagg_filter4$energy_category_disagg <- ifelse(lz_hydro_load_delta_disagg_filter4$energy_category=="zone_demand_GWh" & lz_hydro_load_delta_disagg_filter4$climate_impacts_included == "Hydropower and Water Load Scenarios", "Water Load", lz_hydro_load_delta_disagg_filter4$energy_category_disagg )
  lz_hydro_load_delta_disagg_filter4$energy_category_disagg <- ifelse(lz_hydro_load_delta_disagg_filter4$energy_category=="zone_demand_GWh" & lz_hydro_load_delta_disagg_filter4$climate_impacts_included == "Cooling Load Scenarios", "Cooling Load", lz_hydro_load_delta_disagg_filter4$energy_category_disagg )
  
  #export for use in other plots
  write.csv(x = lz_hydro_load_delta_disagg_filter4, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,climate_impacts_included, "DELTA_lz_hydro_load_disagg.csv",sep=""), row.names = F)

}



## LOADS #
### load deltas by load zone

#transpose annual load summary by load zone to long format

# loads_annual_period_lz_long <- gather(loads_annual_period_lz, energy_category, Energy_GWh_typical_yr, 5:8)
# loads_annual_period_lz_long <- loads_annual_period_lz_long %>% dplyr::select(Scenario, ts_period, energy_category, Energy_GWh_typical_yr)
# loads_annual_period_lz_long$Year <- NULL

#load zone delta between baseline and CC scenarios

baseline <- scenario_name_short1

loads_annual_period_lz_baseline <- loads_annual_period_lz %>% filter(Scenario == baseline)
loads_annual_period_lz_baseline <- rename(loads_annual_period_lz_baseline, Baseline_Scenario = Scenario, zone_demand_GWh_baseline = zone_demand_GWh, ZoneTotalCentralDispatch_GWh_baseline = ZoneTotalCentralDispatch_GWh, 
                                          TXPowerNet_GWh_baseline = TXPowerNet_GWh, StorageNetCharge_GWh_baseline = StorageNetCharge_GWh)

loads_annual_period_lz_delta0 <- loads_annual_period_lz %>% filter(Scenario != baseline)
loads_annual_period_lz_delta <- left_join(loads_annual_period_lz_delta0, loads_annual_period_lz_baseline, c("load_zone"="load_zone","Year"="Year","ts_period"="ts_period"))

#load deltas and percentage differences
loads_annual_period_lz_delta$Delta_zone_demand_GWh <-abs(loads_annual_period_lz_delta$zone_demand_GWh) - abs(loads_annual_period_lz_delta$zone_demand_GWh_baseline)

loads_annual_period_lz_delta$AnnualLoad_perc_change_from_baseline <- round(loads_annual_period_lz_delta$Delta_zone_demand_GWh/abs(loads_annual_period_lz_delta$zone_demand_GWh_baseline),3)
#central dispatch deltas and percentage differences
loads_annual_period_lz_delta$Delta_ZoneTotalCentralDispatch_GWh <-loads_annual_period_lz_delta$ZoneTotalCentralDispatch_GWh - loads_annual_period_lz_delta$ZoneTotalCentralDispatch_GWh_baseline

loads_annual_period_lz_delta$AnnualCentralDispatch_perc_change_from_baseline <- round(loads_annual_period_lz_delta$Delta_ZoneTotalCentralDispatch_GWh/loads_annual_period_lz_delta$ZoneTotalCentralDispatch_GWh_baseline,3)

#storage dispatch deltas and percentage differences
loads_annual_period_lz_delta$Delta_StorageNetCharge_GWh <-loads_annual_period_lz_delta$StorageNetCharge_GWh - loads_annual_period_lz_delta$StorageNetCharge_GWh_baseline

loads_annual_period_lz_delta$AnnualStorageNetCharge_perc_change_from_baseline <- round(loads_annual_period_lz_delta$Delta_StorageNetCharge_GWh/loads_annual_period_lz_delta$StorageNetCharge_GWh_baseline,3)

#TxPower deltas and percentage differences
loads_annual_period_lz_delta$Delta_TXPowerNet_GWh <-loads_annual_period_lz_delta$TXPowerNet_GWh - loads_annual_period_lz_delta$TXPowerNet_GWh_baseline

loads_annual_period_lz_delta$AnnualTxPowerNet_perc_change_from_baseline <- round(loads_annual_period_lz_delta$Delta_TXPowerNet_GWh/loads_annual_period_lz_delta$TXPowerNet_GWh_baseline,3)

#output csv of DELTA load zone load, dispatch, TX, storage by period for each scenario
write.csv(x = loads_annual_period_lz_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day,climate_impacts_included, "DELTA_Lz_load_dispatch_tx.csv",sep=""), row.names = F)

#WECC wide delta between baseline and CC scenarios of load, dispatch, TX, storage by period for each scenario
loads_annual_period_WECC_delta <- loads_annual_period_lz_delta %>% 
  group_by(Scenario, Baseline_Scenario, Year, ts_period) %>% 
  summarize(zone_demand_GWh = sum(zone_demand_GWh), zone_demand_GWh_baseline = sum(zone_demand_GWh_baseline), ZoneTotalCentralDispatch_GWh = sum(ZoneTotalCentralDispatch_GWh), 
            ZoneTotalCentralDispatch_GWh_baseline = sum(ZoneTotalCentralDispatch_GWh_baseline), TXPowerNet_GWh = sum(TXPowerNet_GWh), TXPowerNet_GWh_baseline = sum(TXPowerNet_GWh_baseline),
            StorageNetCharge_GWh = sum(StorageNetCharge_GWh), StorageNetCharge_GWh_baseline = sum(StorageNetCharge_GWh_baseline))

# get percentage diff from baseline and total WECC wide delta
#load deltas and percentage differences
loads_annual_period_WECC_delta$Delta_zone_demand_GWh <-abs(loads_annual_period_WECC_delta$zone_demand_GWh) - abs(loads_annual_period_WECC_delta$zone_demand_GWh_baseline)

loads_annual_period_WECC_delta$AnnualLoad_perc_change_from_baseline <- round(loads_annual_period_WECC_delta$Delta_zone_demand_GWh/abs(loads_annual_period_WECC_delta$zone_demand_GWh_baseline),3)
#central dispatch deltas and percentage differences
loads_annual_period_WECC_delta$Delta_ZoneTotalCentralDispatch_GWh <-loads_annual_period_WECC_delta$ZoneTotalCentralDispatch_GWh - loads_annual_period_WECC_delta$ZoneTotalCentralDispatch_GWh_baseline

loads_annual_period_WECC_delta$AnnualCentralDispatch_perc_change_from_baseline <- round(loads_annual_period_WECC_delta$Delta_ZoneTotalCentralDispatch_GWh/loads_annual_period_WECC_delta$ZoneTotalCentralDispatch_GWh_baseline,3)

#storage dispatch deltas and percentage differences
loads_annual_period_WECC_delta$Delta_StorageNetCharge_GWh <-loads_annual_period_WECC_delta$StorageNetCharge_GWh - loads_annual_period_WECC_delta$StorageNetCharge_GWh_baseline

loads_annual_period_WECC_delta$AnnualStorageNetCharge_perc_change_from_baseline <- round(loads_annual_period_WECC_delta$Delta_StorageNetCharge_GWh/loads_annual_period_WECC_delta$StorageNetCharge_GWh_baseline,3)

#TxPower deltas and percentage differences
loads_annual_period_WECC_delta$Delta_TXPowerNet_GWh <-loads_annual_period_WECC_delta$TXPowerNet_GWh - loads_annual_period_WECC_delta$TXPowerNet_GWh_baseline

loads_annual_period_WECC_delta$AnnualTxPowerNet_perc_change_from_baseline <- round(loads_annual_period_WECC_delta$Delta_TXPowerNet_GWh/loads_annual_period_WECC_delta$TXPowerNet_GWh_baseline,3)

#output csv of DELTA WECC generation by period for each scenario
write.csv(x = loads_annual_period_WECC_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day, climate_impacts_included, "DELTA_WECC_load_dispatch_tx.csv",sep=""), row.names = F)

#plot the load zone load balance by scenario and year
#first create a long format of the absolute load balance values
loads_annual_period_lz_long <- gather(loads_annual_period_lz, energy_category, Energy_GWh_typical_yr, 5:8)

#sum across load zones to get WECC totals in long format
loads_annual_period_WECC_long <- loads_annual_period_lz_long %>% group_by(Year, ts_period, Scenario, energy_category) %>% summarize(Energy_GWh_typical_yr = sum(Energy_GWh_typical_yr))

#Load zone totals


##Western US totals annually (excluding Canadian and Mexican load zones)
#WECC wide delta between baseline and CC scenarios of load, dispatch, TX, storage by period for each scenario
WUS_loads_annual_period_lz_delta <- loads_annual_period_lz_delta %>% filter(load_zone != "CAN_ALB" & load_zone!="CAN_BC" & load_zone !="MEX_BAJA")

WUS_loads_annual_period_delta <- WUS_loads_annual_period_lz_delta %>% 
  group_by(Scenario, Baseline_Scenario, Year, ts_period) %>% 
  summarize(zone_demand_GWh = sum(zone_demand_GWh), zone_demand_GWh_baseline = sum(zone_demand_GWh_baseline), ZoneTotalCentralDispatch_GWh = sum(ZoneTotalCentralDispatch_GWh), 
            ZoneTotalCentralDispatch_GWh_baseline = sum(ZoneTotalCentralDispatch_GWh_baseline), TXPowerNet_GWh = sum(TXPowerNet_GWh), TXPowerNet_GWh_baseline = sum(TXPowerNet_GWh_baseline),
            StorageNetCharge_GWh = sum(StorageNetCharge_GWh), StorageNetCharge_GWh_baseline = sum(StorageNetCharge_GWh_baseline))

# get percentage diff from baseline and total WECC wide delta
#load deltas and percentage differences
WUS_loads_annual_period_delta$Delta_zone_demand_GWh <-abs(WUS_loads_annual_period_delta$zone_demand_GWh) - abs(WUS_loads_annual_period_delta$zone_demand_GWh_baseline)

WUS_loads_annual_period_delta$AnnualLoad_perc_change_from_baseline <- round(WUS_loads_annual_period_delta$Delta_zone_demand_GWh/abs(WUS_loads_annual_period_delta$zone_demand_GWh_baseline),3)
#central dispatch deltas and percentage differences
WUS_loads_annual_period_delta$Delta_ZoneTotalCentralDispatch_GWh <-WUS_loads_annual_period_delta$ZoneTotalCentralDispatch_GWh - WUS_loads_annual_period_delta$ZoneTotalCentralDispatch_GWh_baseline

WUS_loads_annual_period_delta$AnnualCentralDispatch_perc_change_from_baseline <- round(WUS_loads_annual_period_delta$Delta_ZoneTotalCentralDispatch_GWh/WUS_loads_annual_period_delta$ZoneTotalCentralDispatch_GWh_baseline,3)

#storage dispatch deltas and percentage differences
WUS_loads_annual_period_delta$Delta_StorageNetCharge_GWh <-WUS_loads_annual_period_delta$StorageNetCharge_GWh - WUS_loads_annual_period_delta$StorageNetCharge_GWh_baseline

WUS_loads_annual_period_delta$AnnualStorageNetCharge_perc_change_from_baseline <- round(WUS_loads_annual_period_delta$Delta_StorageNetCharge_GWh/WUS_loads_annual_period_delta$StorageNetCharge_GWh_baseline,3)

#TxPower deltas and percentage differences
WUS_loads_annual_period_delta$Delta_TXPowerNet_GWh <-WUS_loads_annual_period_delta$TXPowerNet_GWh - WUS_loads_annual_period_delta$TXPowerNet_GWh_baseline

WUS_loads_annual_period_delta$AnnualTxPowerNet_perc_change_from_baseline <- round(WUS_loads_annual_period_delta$Delta_TXPowerNet_GWh/WUS_loads_annual_period_delta$TXPowerNet_GWh_baseline,3)

#output csv of DELTA WECC generation by period for each scenario
write.csv(x = WUS_loads_annual_period_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day, climate_impacts_included, "DELTA_WUS_load_dispatch_tx.csv",sep=""), row.names = F)

#Baseline WECC load by year

plot_file_name <- paste("SI_facet_plot","Baseline_WECC_Annual_load", ".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=1000, height=900, res=100)

plot <-ggplot(subset(loads_annual_period_WECC_long, Scenario == baseline & energy_category == "zone_demand_GWh"), aes(x = ts_period, y = -1*Energy_GWh_typical_yr)) + 
  geom_bar(stat="identity") + 
  # geom_text(data=subset(loads_annual_period_WECC_delta, abs(AnnualLoad_perc_change_from_baseline) > 0.01) , size = 2.5, position = position_stack(vjust = 1.5)) +
  labs(x="Period") + labs(y="Energy (GWh)")  +
  ggtitle(paste("WECC total annual load for Baseline scenario", sep="")) +
  scale_y_continuous(labels = scales::comma) +
  # scale_fill_discrete(name = "Load balance" , labels = c("Storage Net Charge","Net Transmission Flows","Load","Generation")) +
  # facet_wrap( ~ Scenario, nrow = 5) +
  theme(axis.text.y = element_text(size=14), axis.title.x = element_text(size=16), axis.text.x = element_text(size=12, angle = 90), axis.title.y=element_text(size=16), 
        legend.text = element_text(size = 14, nrow(2)), legend.title = element_text(size=14), legend.position="bottom", plot.title = element_text(size=18,hjust=0.5), 
        strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 16))
print(plot)

dev.off()

#plot the WECC DELTA load by scenario and year 5x3
plot_file_name <- paste("SI_facet_plot","DELTA_WECC_Annual_load2",climate_impacts_included, ".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=1200, height=900, res=100)

plot <-ggplot(loads_annual_period_WECC_delta, aes(x = ts_period, y = Delta_zone_demand_GWh, label = ifelse(AnnualLoad_perc_change_from_baseline>0,paste("+",round(AnnualLoad_perc_change_from_baseline*100,1),"%",sep=""),paste(round(AnnualLoad_perc_change_from_baseline*100,0),"%",sep="")))) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(data=subset(loads_annual_period_WECC_delta, abs(AnnualLoad_perc_change_from_baseline) > 0.005) , size = 4, position = position_stack(vjust = 1.5)) +
  labs(x="Period") + labs(y="Delta Load (GWh)")  +
  ggtitle(paste("Change in WECC total annual load compared to baseline scenario",climate_impacts_included, sep=", ")) +
  #scale_fill_manual(values = cbpal) +
  facet_wrap( ~ Scenario, nrow = 5) +
  theme(axis.text.y = element_text(size=14), axis.title.x = element_text(size=16), axis.text.x = element_text(size=12, angle = 90), axis.title.y=element_text(size=16), 
        legend.text = element_text(size = 14, nrow(2)), legend.title = element_text(size=14), legend.position="bottom", plot.title = element_text(size=13.5,hjust=0.5), 
        strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 16))
print(plot)

dev.off()

###Costs############

itemized_cost_per_period <- data.frame()

for (i in 1:length(run_name_list)){
  
  run_name <- run_name_list[[i]]
  scenario <- short_scenario_list[[i]]
  
  #read in the itemized costs by period
  itemized_cost <- read.csv(file=paste(switch_input_dir,run_name,"//outputs//","costs_itemized.csv",sep=""),stringsAsFactors=F, header=TRUE)
  
  itemized_cost$Scenario <- scenario
  
  itemized_cost_per_period <- rbind(itemized_cost_per_period, itemized_cost)
  
}  


#output csv of itemized cost by period for each scenario
write.csv(x = itemized_cost_per_period, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, day, climate_impacts_included, "itemized_cost.csv",sep=""), row.names = F)

#reorder scenario
itemized_cost_per_period <- itemized_cost_per_period %>% filter(Component != "EmissionsCosts")

# itemized_cost_per_period <- ungroup(itemized_cost_per_period) %>% mutate(Scenario = fct_reorder(Scenario, AnnualCost_Real, max, .desc = FALSE))
#multiply by number of years per period to get total over the whole period
itemized_cost_per_period$PeriodCost_Real <- itemized_cost_per_period$AnnualCost_Real * 5
itemized_cost_per_period$PeriodCost_NPV <- itemized_cost_per_period$AnnualCost_NPV * 5

cbPalette <- c("#D55E00", "#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#F0E442", "#CC79A7")


#itemized cost delta for all WECC
baseline <- scenario_name_short1

itemized_cost_per_period_baseline <- itemized_cost_per_period %>% filter(Scenario == baseline)
itemized_cost_per_period_baseline <- rename(itemized_cost_per_period_baseline, Baseline_Scenario = Scenario, PeriodCost_NPV_Baseline = PeriodCost_NPV, PeriodCost_Real_Baseline = PeriodCost_Real)

itemized_cost_per_period_delta0 <- itemized_cost_per_period %>% filter(Scenario != baseline)
itemized_cost_per_period_delta <- left_join(itemized_cost_per_period_delta0, itemized_cost_per_period_baseline, c("PERIOD"="PERIOD", "Component" = "Component","Component_type" = "Component_type"))

itemized_cost_per_period_delta$Delta_PeriodCost_NPV <-itemized_cost_per_period_delta$PeriodCost_NPV - itemized_cost_per_period_delta$PeriodCost_NPV_Baseline
itemized_cost_per_period_delta$Delta_PeriodCost_Real <-itemized_cost_per_period_delta$PeriodCost_Real - itemized_cost_per_period_delta$PeriodCost_Real_Baseline

itemized_cost_per_period_delta$PeriodCost_NPV_perc_change_from_baseline <- round(itemized_cost_per_period_delta$Delta_PeriodCost_NPV/itemized_cost_per_period_delta$PeriodCost_NPV_Baseline,3)
itemized_cost_per_period_delta$PeriodCost_Real_perc_change_from_baseline <- round(itemized_cost_per_period_delta$Delta_PeriodCost_Real/itemized_cost_per_period_delta$PeriodCost_Real_Baseline,3)

#output csv of DELTA itemized costs by period for each scenario
write.csv(x = itemized_cost_per_period_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day,climate_impacts_included, "DELTA_Itemized_cost.csv",sep=""), row.names = F)

#### total costs per period (not itemized)
total_cost_per_period <- itemized_cost_per_period %>% group_by(PERIOD, Scenario) %>% summarize(PeriodCost_NPV = sum(PeriodCost_NPV), PeriodCost_Real = sum(PeriodCost_Real))
total_cost_per_period_delta <- itemized_cost_per_period_delta %>% group_by(PERIOD, Scenario, Baseline_Scenario) %>% summarize(PeriodCost_NPV = sum(PeriodCost_NPV), PeriodCost_Real = sum(PeriodCost_Real), 
                                                                                                                              PeriodCost_NPV_Baseline = sum(PeriodCost_NPV_Baseline), 
                                                                                                                              PeriodCost_Real_Baseline = sum(PeriodCost_Real_Baseline),
                                                                                                                              Delta_PeriodCost_NPV= sum(Delta_PeriodCost_NPV), 
                                                                                                                              Delta_PeriodCost_Real = sum(Delta_PeriodCost_Real))

#### total costs across all periods (not itemized)

total_cost_all_periods <- itemized_cost_per_period %>% group_by(Scenario) %>% summarize(PeriodCost_NPV = sum(PeriodCost_NPV), PeriodCost_Real = sum(PeriodCost_Real))
total_cost_all_periods_delta <- itemized_cost_per_period_delta %>% group_by(Scenario, Baseline_Scenario) %>% summarize(PeriodCost_NPV = sum(PeriodCost_NPV), PeriodCost_Real = sum(PeriodCost_Real), 
                                                                                                                       PeriodCost_NPV_Baseline = sum(PeriodCost_NPV_Baseline), 
                                                                                                                       PeriodCost_Real_Baseline = sum(PeriodCost_Real_Baseline),
                                                                                                                       Delta_PeriodCost_NPV= sum(Delta_PeriodCost_NPV), 
                                                                                                                       Delta_PeriodCost_Real = sum(Delta_PeriodCost_Real))
total_cost_all_periods_delta$TotalCost_NPV_perc_change_from_baseline <- total_cost_all_periods_delta$Delta_PeriodCost_NPV/ total_cost_all_periods_delta$PeriodCost_NPV_Baseline
total_cost_all_periods_delta$TotalCost_Real_perc_change_from_baseline <- total_cost_all_periods_delta$Delta_PeriodCost_Real/ total_cost_all_periods_delta$PeriodCost_Real_Baseline


#output csv of DELTA total cost for each scenario
write.csv(x = total_cost_all_periods_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day,climate_impacts_included, "DELTA_Total_cost.csv",sep=""), row.names = F)

#reorder scenario
total_cost_all_periods_delta <- ungroup(total_cost_all_periods_delta) %>% mutate(Scenario = fct_reorder(Scenario, Delta_PeriodCost_NPV, max, .desc = FALSE))



#FIGURE 3C

#resorted cost by cumulative capacity built
total_cost_all_periods_delta$Scenario <- factor(total_cost_all_periods_delta$Scenario, levels = cumulative_buildgen_order)
total_cost_all_periods2 = total_cost_all_periods
total_cost_all_periods2$Scenario <- ifelse(total_cost_all_periods2$Scenario == "Baseline no CC", "Baseline",total_cost_all_periods2$Scenario)
plot_file_name <- paste("Fig3C_WECC_total_cost_NPV_BASELINE_and_DELTA_cost", ".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=2300, height=700, res=100)

plot1 <- ggplot(data=subset(total_cost_all_periods2, Scenario == "Baseline"), aes(x=Scenario, y=PeriodCost_NPV/10^9, fill=PeriodCost_NPV)) + 
  ggtitle(paste("Baseline total cost", sep=", ")) +
  geom_bar(stat="identity", fill="#56B4E9") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(name="Cost", values="#56B4E9", labels = "Net Present Value") +
  labs(x="Scenario") + labs(y="Costs ($Billions, NPV)") +
  theme(axis.text.y = element_text(size=22), axis.title.x = element_text(size=24), axis.text.x = element_text(size=22, angle = 90), axis.title.y=element_text(size=24), 
        legend.text = element_text(size = 22, nrow(2)), legend.title = element_text(size=24), legend.position="none", plot.title = element_text(size=26,hjust=0.4), 
        strip.text.x = element_text(size = 24), strip.text.y = element_text(size = 24), plot.margin = margin(10, 10, 10, 10))

plot2 <-ggplot(data=total_cost_all_periods_delta, aes(x=Scenario, y=Delta_PeriodCost_NPV/10^9, label=ifelse(TotalCost_NPV_perc_change_from_baseline>0, paste("+",round(TotalCost_NPV_perc_change_from_baseline*100,1),"%",sep=""),
                                                                                                            paste("-",round(TotalCost_NPV_perc_change_from_baseline*100,1),"%",sep="")))) +
  ggtitle(paste("Change in total cost relative to Baseline Scenario", sep=", ")) +
  geom_bar(stat="identity", fill="#56B4E9") +
  geom_text( size = 7, position = position_stack(vjust = 0.5)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(x="Scenario") + labs(y="Delta costs ($Billions, NPV)") +
  theme(axis.text.y = element_text(size=22), axis.title.x = element_text(size=24), axis.text.x = element_text(size=22, angle = 90), axis.title.y=element_text(size=24), 
        legend.text = element_text(size = 22, nrow(2)), legend.title = element_text(size=24), legend.position="right", plot.title = element_text(size=26,hjust=0.5), 
        strip.text.x = element_text(size = 24), strip.text.y = element_text(size = 24), plot.margin = margin(10, 10, 10, 10))


plot_grid(plot1, plot2, rel_widths=c(1,3), align = 'hv')

dev.off()  


###Detailed load comparison between scenarios################


load_balance_all_scenarios <- data.frame()
hourly_load_balance_all_scenarios <- data.frame()

for (i in 1:length(short_scenario_list)){
  
  run_name <- run_name_list[[i]]
  scenario <- short_scenario_list[[i]]

  #read in the load balance by load zone and timestamp
  load_balance <- read.csv(file=paste(switch_input_dir,run_name,"//outputs//","load_balance.csv",sep=""),stringsAsFactors=F, header=TRUE)
  
  #parse time points to human readable date and components
  load_balance$Month <- as.integer(substr(load_balance$timestamp, 5, 6))
  load_balance$Year <- as.integer(substr(load_balance$timestamp, 1, 4))
  load_balance$Day <- as.integer(substr(load_balance$timestamp, 7, 8))
  load_balance$Hour_UTC <- as.integer(substr(load_balance$timestamp, 9, 10))
  #constructing datetime in UTC (time zone of time points)
  load_balance$date_time_UTC <- ymd_hms(paste(paste(load_balance$Year,load_balance$Month,load_balance$Day, sep="-"), 
                                                           paste(load_balance$Hour_UTC,"00","00",sep=":"),sep=" "), tz = "UTC")
  
  #read in the timestamp weighting
  timeseries <- read.csv(file=paste(switch_input_dir,run_name,"//inputs//","timeseries.csv",sep=""),stringsAsFactors=F, header=TRUE)
  
  #read in the timestamp weighting
  timepoints <- read.csv(file=paste(switch_input_dir,run_name,"//inputs//","timepoints.csv",sep=""),stringsAsFactors=F, header=TRUE)
  
  timepoints_ts <- left_join(timepoints, timeseries, c("timeseries"="TIMESERIES"))
  
  load_balance_tps <- left_join(load_balance, timepoints_ts, c("timestamp" = "timestamp"))
  
  #calculate weighted loads scaled to the year
  load_balance_tps$zone_demand_mw_scaled_to_year <- load_balance_tps$zone_demand_mw * load_balance_tps$ts_scale_to_period/5
  load_balance_tps$ZoneTotalCentralDispatch_scaled_to_year <- load_balance_tps$ZoneTotalCentralDispatch * load_balance_tps$ts_scale_to_period/5
  load_balance_tps$TXPowerNet_scaled_to_year <- load_balance_tps$TXPowerNet * load_balance_tps$ts_scale_to_period/5
  load_balance_tps$StorageNetCharge_scaled_to_year <- load_balance_tps$StorageNetCharge * load_balance_tps$ts_scale_to_period/5
  
  #aggregate by month
  load_balance_tps_Aggmonth <- load_balance_tps %>% group_by(load_zone, Month, Year, ts_period) %>% summarize(zone_demand_mw = sum(zone_demand_mw_scaled_to_year), 
                                                                                                              ZoneTotalCentralDispatch = sum(ZoneTotalCentralDispatch_scaled_to_year),
                                                                                                              TXPowerNet = sum(TXPowerNet_scaled_to_year),
                                                                                                              StorageNetCharge = sum(StorageNetCharge_scaled_to_year))
  
  load_balance_tps_Aggmonth$Scenario <- scenario
  
  #load balance by load zone and period for each scenario
  load_balance_all_scenarios <- rbind(load_balance_all_scenarios, load_balance_tps_Aggmonth)
  
  load_balance_tps$Scenario <- scenario
  
  hourly_load_balance_all_scenarios <- rbind(hourly_load_balance_all_scenarios, load_balance_tps)
}

#output csv of monthly load balance by load zone by period for each scenario
write.csv(x = load_balance_all_scenarios, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day, climate_impacts_included, "Monthly_load_lz.csv",sep=""))

#output csv of hourly load balance by load zone by period for each scenario
write.csv(x = hourly_load_balance_all_scenarios, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day, climate_impacts_included, "Hourly_load_balance_lz.csv",sep=""))

########Load duration curves with sampled hourly loads

#sum load for all wecc
WECC_hourly_load_all_scenarios <- hourly_load_balance_all_scenarios %>% group_by(Scenario, timestamp, Month, Year, Day, Hour_UTC, date_time_UTC, ts_period, ts_duration_of_tp, ts_num_tps, ts_scale_to_period) %>% 
  summarize(zone_demand_mw = sum(zone_demand_mw))
#order from highest to lowest by scenario
WECC_hourly_load_all_scenarios_st <- WECC_hourly_load_all_scenarios[with(WECC_hourly_load_all_scenarios, order(Scenario, ts_period, abs(zone_demand_mw),decreasing = TRUE)),]

#create an index for each hour sampled by scenario and period after sorting in descending order (for plotting duration curve)
WECC_hourly_load_all_scenarios_st <- WECC_hourly_load_all_scenarios_st %>% group_by(Scenario, ts_period) %>% mutate(counter = row_number())

#load zone delta between baseline and CC scenarios

baseline <- scenario_name_short1

load_balance_baseline <- load_balance_all_scenarios %>% filter(Scenario == baseline)
load_balance_baseline <- rename(load_balance_baseline, Baseline_Scenario = Scenario, zone_demand_mw_Baseline = zone_demand_mw, ZoneTotalCentralDispatch_baseline = ZoneTotalCentralDispatch,
                                TXPowerNet_baseline = TXPowerNet, StorageNetCharge_baseline = StorageNetCharge)

load_balance_lz_delta0 <- load_balance_all_scenarios %>% filter(Scenario != baseline)
load_balance_lz_delta <- left_join(load_balance_lz_delta0, load_balance_baseline, c("ts_period"="ts_period", "load_zone" = "load_zone","Month" = "Month", "Year" = "Year"))

load_balance_lz_delta$Delta_zone_demand_mw <-load_balance_lz_delta$zone_demand_mw_Baseline - load_balance_lz_delta$zone_demand_mw
load_balance_lz_delta$Delta_ZoneTotalCentralDispatch <-load_balance_lz_delta$ZoneTotalCentralDispatch_baseline - load_balance_lz_delta$ZoneTotalCentralDispatch
load_balance_lz_delta$Delta_TXPowerNet <-load_balance_lz_delta$TXPowerNet_baseline - load_balance_lz_delta$TXPowerNet
load_balance_lz_delta$Delta_StorageNetCharge <-load_balance_lz_delta$StorageNetCharge_baseline - load_balance_lz_delta$StorageNetCharge

load_balance_lz_delta$zone_demand_mw_perc_change_from_baseline <- round(load_balance_lz_delta$Delta_zone_demand_mw/load_balance_lz_delta$zone_demand_mw_Baseline,3)
load_balance_lz_delta$ZoneTotalCentralDispatch_perc_change_from_baseline <- round(load_balance_lz_delta$Delta_ZoneTotalCentralDispatch/load_balance_lz_delta$ZoneTotalCentralDispatch_baseline,3)
load_balance_lz_delta$TXPowerNet_perc_change_from_baseline <- round(load_balance_lz_delta$Delta_TXPowerNet/load_balance_lz_delta$TXPowerNet_baseline,3)
load_balance_lz_delta$StorageNetCharge_perc_change_from_baseline <- round(load_balance_lz_delta$Delta_StorageNetCharge/load_balance_lz_delta$StorageNetCharge_baseline,3)

#output csv of DELTA load zone balance by period for each scenario
write.csv(x = load_balance_lz_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day,climate_impacts_included, "DELTA_Lz_monthly_load.csv",sep=""), row.names = F)


#monthly WECC total load
#sum load for all wecc
WECC_load_balance_delta <- load_balance_lz_delta %>% group_by(Scenario, Month, Year, ts_period) %>% 
  summarize(zone_demand_mw_Baseline = -sum(zone_demand_mw_Baseline), zone_demand_mw=-sum(zone_demand_mw))

#filter out Canadian and Mexican load zones to just have WUS load
WUS_load_balance_delta <- load_balance_lz_delta %>% filter(load_zone != "CAN_BC" & load_zone != "CAN_ALB" & load_zone != "MEX_BAJA") %>% group_by(Scenario, Month, Year, ts_period) %>% 
  summarize(zone_demand_mw_Baseline = -sum(zone_demand_mw_Baseline), zone_demand_mw=-sum(zone_demand_mw))

#changes in WECC total monthly load
WECC_load_balance_delta$Delta_zone_demand_mw <- WECC_load_balance_delta$zone_demand_mw - WECC_load_balance_delta$zone_demand_mw_Baseline
WUS_load_balance_delta$Delta_zone_demand_mw <- WUS_load_balance_delta$zone_demand_mw - WUS_load_balance_delta$zone_demand_mw_Baseline

WECC_load_balance_delta$Delta_perc_zone_demand_mw <- WECC_load_balance_delta$Delta_zone_demand_mw/WECC_load_balance_delta$zone_demand_mw_Baseline
WUS_load_balance_delta$Delta_perc_zone_demand_mw <- WUS_load_balance_delta$Delta_zone_demand_mw/WUS_load_balance_delta$zone_demand_mw_Baseline

#output csv of DELTA WECC and WUS load monthly by period for each scenario
write.csv(x = WECC_load_balance_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day,climate_impacts_included, "DELTA_WECC_monthly_load.csv",sep=""), row.names = F)
write.csv(x = WUS_load_balance_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day,climate_impacts_included, "DELTA_WUS_monthly_load.csv",sep=""), row.names = F)


#read in daily dispatch delta to see if we can see the decrease in hydropower generation monthly
dispatch_hourly_lz <- read.csv(file=paste(switch_output_dir, switch_run_dir, weap_run_dir,day,climate_impacts_included, "Lz_dispatch_hourly.csv",sep=""),stringsAsFactors=F, header=TRUE)

dispatch_hourly_WECC <- dispatch_hourly_lz %>% group_by(Scenario, gen_energy_source, period, Month, Day, date_time_PST, Hour_PST_bin, day_type, tp_weight_in_year_hrs) %>% 
  summarize(DispatchGen_MW = sum(DispatchGen_MW), Curtailment_MW = sum(Curtailment_MW))

hydro_dispatch_hourly_WECC <- dispatch_hourly_WECC %>% filter(gen_energy_source == "Water")

hydro_dispatch_hourly_WECC$Monthly_DispatchGen_MW <- hydro_dispatch_hourly_WECC$tp_weight_in_year_hrs * hydro_dispatch_hourly_WECC$DispatchGen_MW

hydro_dispatch_monthly_WECC <- hydro_dispatch_hourly_WECC %>% group_by(Scenario, gen_energy_source, period, Month) %>% summarize(Monthly_DispatchGen_MW = sum(Monthly_DispatchGen_MW))
#calculating monthly hydro dispatch delta
baseline <- scenario_name_short1

hydro_dispatch_monthly_WECC_baseline <- hydro_dispatch_monthly_WECC %>% filter(Scenario == baseline)
# dispatch_hourly_baseline <- rename(dispatch_hourly_baseline, Baseline_Scenario = Scenario, DispatchGen_MW_baseline = DispatchGen_MW, Curtailment_MW_baseline = Curtailment_MW)
hydro_dispatch_monthly_WECC_baseline <- rename(hydro_dispatch_monthly_WECC_baseline, Baseline_Scenario = Scenario, Monthly_DispatchGen_MW_baseline = Monthly_DispatchGen_MW)


hydro_dispatch_monthly_WECC_delta0 <- hydro_dispatch_monthly_WECC %>% filter(Scenario != baseline)
hydro_dispatch_monthly_WECC_delta <- left_join(hydro_dispatch_monthly_WECC_delta0, hydro_dispatch_monthly_WECC_baseline, c("period"="period", "Month" = "Month", 
                                                                                                                           "gen_energy_source" = "gen_energy_source"))

hydro_dispatch_monthly_WECC_delta$Delta_Monthly_DispatchGen_MW <-hydro_dispatch_monthly_WECC_delta$Monthly_DispatchGen_MW - hydro_dispatch_monthly_WECC_delta$Monthly_DispatchGen_MW_baseline
hydro_dispatch_monthly_WECC_delta$PercDelta_Monthly_DispatchGen_MW <-hydro_dispatch_monthly_WECC_delta$Delta_Monthly_DispatchGen_MW/hydro_dispatch_monthly_WECC_delta$Monthly_DispatchGen_MW_baseline


#combined plot FIGURE 2B with monthly change in 2050 load and hydropower generation in box plots
WECC_monthly_load_hydro_delta <- left_join(WECC_load_balance_delta, hydro_dispatch_monthly_WECC_delta, c("Scenario"="Scenario", "Month"="Month", "ts_period"="period"))

#select subset of columns
WECC_monthly_load_hydro_delta2 <- WECC_monthly_load_hydro_delta %>% dplyr::select(Scenario, Month, Year, Delta_Monthly_DispatchGen_MW, Delta_zone_demand_mw )
WECC_monthly_load_hydro_delta_long <- gather(WECC_monthly_load_hydro_delta2, Demand_or_Supply, DeltaMonthly_Dispatch_or_Load_MW, 4:5)

#rename for plotting
WECC_monthly_load_hydro_delta_long$Demand_or_Supply <- ifelse(WECC_monthly_load_hydro_delta_long$Demand_or_Supply == "Delta_Monthly_DispatchGen_MW", "Supply", "Demand")

##FIGURE 2B

plot_file_name <- paste("Fig2B_freescalesBox_plot_DELTA_Monthly_hydropower_and_Load",climate_impacts_included, ".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=1800, height=700, res=100)

plot <-ggplot(subset(WECC_monthly_load_hydro_delta_long, Year == 2050), aes(x = factor(Month), y = DeltaMonthly_Dispatch_or_Load_MW/1000, group=Month, fill = Demand_or_Supply)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2","#FFCC33"), name = "Energy category", breaks = c("Supply", "Demand"), labels = c("Hydropower", "Load")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x="Month") + labs(y="Delta energy (GWh)")  +
  ggtitle(paste("2050 Change in WECC monthly hydropower generation and load compared to Baseline Scenario",sep=", ")) +
  geom_hline(yintercept=0) +
  facet_grid(Demand_or_Supply ~ ., scales="free_y") +
  theme(axis.text.y = element_text(size=22), axis.title.x = element_text(size=24), axis.text.x = element_text(size=22), axis.title.y=element_text(size=24), 
        legend.text = element_text(size = 22, nrow(2)), legend.title = element_text(size=24), legend.position="bottom", plot.title = element_text(size=26,hjust=0.5), 
        strip.text.x = element_text(size = 24), strip.text.y = element_text(size = 24), plot.margin = margin(10, 10, 10, 10), panel.spacing = unit(1, "lines"))
print(plot)

dev.off()


# ##try to only run once because it takes a long time to read in the data!
# ##Daily dispatch####################
# 
# dispatch_hourly <- data.frame()
# dispatch_hourly_lz_sourceAgg <- data.frame()
# dispatch_hourly_WECC_sourceAgg <- data.frame()
# 
# for (i in 1:length(run_name_list)){
#   # i =2
#   run_name <- run_name_list[[i]]
#   scenario <- short_scenario_list[[i]]
#   print(i)
#   #read in the generation capacity builds and generation project information 
#   dispatch <- read.csv(file=paste(switch_input_dir,run_name,"//outputs//","dispatch.csv",sep=""),stringsAsFactors=F, header=TRUE)
#   
#   #also aggregating the bio gen_energy_source categories and renaming oil
#   dispatch$gen_energy_source[dispatch$gen_energy_source == "Bio_Gas" | dispatch$gen_energy_source =="Bio_Liquid" | dispatch$gen_energy_source == "Bio_Solid"] <- "Biomass"
#   dispatch$gen_energy_source[dispatch$gen_energy_source == "DistillateFuelOil" | dispatch$gen_energy_source == "ResidualFuelOil"] <- "Oil"
#   dispatch$gen_energy_source[dispatch$gen_energy_source == "Waste_Heat"] <- "Waste Heat"
#   dispatch$gen_energy_source[dispatch$gen_tech == "Battery_Storage"] <- "Battery Storage"
#   
#   #parse the timestamp
#   #parse time points to human readable date and components
#   dispatch$Month <- as.integer(substr(dispatch$timestamp, 5, 6))
#   dispatch$Year <- as.integer(substr(dispatch$timestamp, 1, 4))
#   dispatch$Day <- as.integer(substr(dispatch$timestamp, 7, 8))
#   dispatch$Hour_UTC <- as.integer(substr(dispatch$timestamp, 9, 10))
#   
#   #constructing datetime in UTC (time zone of time points)
#   dispatch$date_time_UTC <- ymd_hms(paste(paste(dispatch$Year,dispatch$Month,dispatch$Day, sep="-"), 
#                                               paste(dispatch$Hour_UTC,"00","00",sep=":"),sep=" "), tz = "UTC")
#   
#   dispatch$date_time_PST <- with_tz(dispatch$date_time_UTC, tzone = "America/Los_Angeles")
#   dispatch$Hour_PST <- hour(dispatch$date_time_PST)
#   
#   # #standardize hours into 4-hour bins if 4-hour sampling
#   # dispatch$Hour_PST_bin <- 23
#   # dispatch$Hour_PST_bin <- ifelse(dispatch$Hour_PST <4, 3, dispatch$Hour_PST_bin)
#   # dispatch$Hour_PST_bin <- ifelse(dispatch$Hour_PST >3 & dispatch$Hour_PST< 8, 7, dispatch$Hour_PST_bin)
#   # dispatch$Hour_PST_bin <- ifelse(dispatch$Hour_PST >7 & dispatch$Hour_PST< 12, 11, dispatch$Hour_PST_bin)
#   # dispatch$Hour_PST_bin <- ifelse(dispatch$Hour_PST > 11 & dispatch$Hour_PST< 16, 15, dispatch$Hour_PST_bin)
#   # dispatch$Hour_PST_bin <- ifelse(dispatch$Hour_PST > 15 & dispatch$Hour_PST< 20, 19, dispatch$Hour_PST_bin)
#   # dispatch$Hour_PST_bin <- ifelse(dispatch$Hour_PST > 19, 23, dispatch$Hour_PST_bin)
#   # dispatch$Hour_PST_bin <- dispatch$Hour_PST_bin + 1
#   dispatch$Hour_PST_bin <- dispatch$Hour_PST + 1
#   
#   #classifying by day type
#   dispatch$day_type <- ifelse(dispatch$tp_weight_in_year_hrs == 1, "peak_day", "median_day")
#   
#   #categorizing in seasons
#   dispatch$Season <- "Winter"
#   dispatch$Season <- ifelse(dispatch$Month == 3 | dispatch$Month == 4 | dispatch$Month == 5 , "Spring", dispatch$Season)
#   dispatch$Season <- ifelse(dispatch$Month == 6 | dispatch$Month == 7 | dispatch$Month == 8 , "Summer", dispatch$Season)
#   dispatch$Season <- ifelse(dispatch$Month == 9 | dispatch$Month == 10 | dispatch$Month == 11 , "Fall", dispatch$Season)
#   
#   dispatch$Scenario <- scenario
#   
#   #aggregate by energy source for each hour across all generators
#   dispatch_hourly_sourceAgg <- dispatch %>% group_by(Scenario, gen_load_zone, gen_energy_source, period, Month, Day, Hour_UTC, 
#                                                      date_time_PST, Hour_PST_bin, day_type, tp_weight_in_year_hrs,Season) %>% summarize(DispatchGen_MW = sum(DispatchGen_MW), 
#                                                                                                                                         Curtailment_MW = sum(Curtailment_MW))
#   
#   #zeroing out curtailment for non wind and solar resources
#   dispatch_hourly_sourceAgg$Curtailment_MW <- ifelse(dispatch_hourly_sourceAgg$gen_energy_source %in% c("Solar", "Wind") , dispatch_hourly_sourceAgg$Curtailment_MW, 0) 
#   
#   #LZ hourly dispatch across scenarios
#   dispatch_hourly_lz_sourceAgg <- rbind(dispatch_hourly_lz_sourceAgg, dispatch_hourly_sourceAgg)
#   
#   #aggregate across all WECC for each hour across all generators
#   dispatch_hourly_WECCsourceAgg <- dispatch_hourly_sourceAgg %>% group_by(Scenario, gen_energy_source, period, Month, Day, Hour_UTC, 
#                                                      date_time_PST, Hour_PST_bin, day_type, tp_weight_in_year_hrs,Season) %>% summarize(DispatchGen_MW = sum(DispatchGen_MW), 
#                                                                                                                                         Curtailment_MW = sum(Curtailment_MW))
#   dispatch_hourly_WECC_sourceAgg <- rbind(dispatch_hourly_WECC_sourceAgg, dispatch_hourly_WECCsourceAgg)
#   
#   #seasonal hourly average dispatch for median and peak days
#   dispatch_season_WECCsourceAgg <- dispatch_hourly_WECCsourceAgg %>% group_by(Scenario, Season, period, Hour_PST_bin, day_type, gen_energy_source) %>% summarize(DispatchGen_MW = mean(DispatchGen_MW), 
#                                                                                                                                                                               Curtailment_MW = mean(Curtailment_MW))
#   #aggregating WECC curtailment for each hour across energy sources
#   curtailment_season_WECCsourceAgg <- dispatch_season_WECCsourceAgg %>% group_by(Scenario, Season, period, Hour_PST_bin, day_type) %>% summarize(Curtailment = sum(Curtailment_MW))
#   
#   #gather aggregate curtailment to long format for easier joining with dispatch
#   curtailment_long_season_WECCsourceAgg <- gather(curtailment_season_WECCsourceAgg, "gen_energy_source", "DispatchGen_MW", Curtailment)
#   dispatch_season_WECCsourceAgg_long <- dispatch_season_WECCsourceAgg
#   dispatch_season_WECCsourceAgg_long$Curtailment_MW <- NULL
#   
#   dispatch_season_WECCsourceAgg_long <- rbind(dispatch_season_WECCsourceAgg_long, curtailment_long_season_WECCsourceAgg)
#   
#   # dispatch_season_WECCsourceAgg_long$Scenario <- scenario
#   
#   dispatch_hourly <- rbind(dispatch_hourly, dispatch_season_WECCsourceAgg_long)
#   
# }
# 
# 
# #output csvs of dispatch results by scenario, for all WECC, LZ, season
# write.csv(x = dispatch_hourly, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, day, climate_impacts_included,"WECC_dispatch_hourlySEASONAL.csv",sep=""), row.names = F)
# write.csv(x = dispatch_hourly_WECC_sourceAgg, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day, climate_impacts_included, "WECC_dispatch_hourly.csv",sep=""), row.names = F)
# write.csv(x = dispatch_hourly_lz_sourceAgg, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day,climate_impacts_included, "Lz_dispatch_hourly.csv",sep=""), row.names = F)

dispatch_hourly <- read.csv(file = paste(switch_output_dir, switch_run_dir, weap_run_dir, day, climate_impacts_included,"WECC_dispatch_hourlySEASONAL.csv",sep=""))
dispatch_hourly_WECC_sourceAgg <- read.csv(file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day, climate_impacts_included, "WECC_dispatch_hourly.csv",sep=""))
dispatch_hourly_lz_sourceAgg <- read.csv(file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day,climate_impacts_included, "Lz_dispatch_hourly.csv",sep=""))

# dispatch_hourly$gen_energy_source <- ifelse(dispatch_hourly$gen_energy_source == "Total_Curtailment_MW", "Curtailment", dispatch_hourly$gen_energy_source)

#reorder factors for plotting
dispatch_hourly$gen_energy_source <- factor(dispatch_hourly$gen_energy_source , levels=c("Curtailment", "Battery Storage","Solar","Wind","Biomass","Geothermal","Gas", "Coal", "Oil", "Waste Heat",
                                                                                         "Uranium", "Water") )
dispatch_hourly$Season <- factor(dispatch_hourly$Season , levels=c("Winter", "Spring","Summer","Fall"))

# # #making facet plots of WECC total dispatch by season, averaged for peak and median days for each period
# # for (i in 1:length(run_name_list)){
# #    # i =1
# #   run_name <- run_name_list[[i]]
# #   scenario <- short_scenario_list[[i]]
# #   
# # 
# #   #facet plot with WECC-wide average daily dispatch per season for peak day
# #   plot_file_name <- paste("Seasonal_dispatch_",scenario,"_peak_day" ,climate_impacts_included,".png", sep="")
# #   
# #   png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=1500, height=900, res=100)
# #   
# #   plot <- ggplot(data=subset(dispatch_hourly, day_type == "peak_day" & Scenario == scenario), aes(x=Hour_PST_bin, y=DispatchGen_MW/10^3, fill=gen_energy_source)) + 
# #     ggtitle(paste("WECC average daily dispatch by season for peak days, ",scenario,climate_impacts_included,sep=" ")) +
# #     geom_area() +
# #     labs(x="Hour") + labs(y="Dispatch (GW)") +
# #     scale_fill_manual(name="Energy Source", values = energy_source_palette) +
# #     facet_grid(period ~ Season) +
# #     scale_x_continuous(limits = c(1,24)) +
# #     theme(axis.text.y = element_text(size=14), axis.title.x = element_text(size=16), axis.text.x = element_text(size=14), axis.title.y=element_text(size=16), 
# #           legend.text = element_text(size = 14), legend.title = element_text(size=14), legend.position="bottom", plot.title = element_text(size=13.5,hjust=0.5), 
# #           strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 16)) +
# #     guides(fill = guide_legend(nrow = 2)) 
# #     # +ylim(0, max(dispatch_hourly$DispatchGen_MW/1000)) 
# #   
# #   print(plot)
# #   
# #   dev.off() 
# #   
# #   #facet plot with WECC-wide average daily dispatch per season for median day
# #   plot_file_name <- paste("Seasonal_dispatch_",scenario,"_median_day" ,climate_impacts_included,".png", sep="")
# #   
# #   png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=1500, height=900, res=100)
# #   
# #   plot <- ggplot(data=subset(dispatch_hourly, day_type == "median_day" & Scenario == scenario), aes(x=Hour_PST_bin, y=DispatchGen_MW/10^3, fill=gen_energy_source)) + 
# #     ggtitle(paste("WECC average daily dispatch by season for median days, ",scenario,climate_impacts_included, sep=" ")) +
# #     geom_area() +
# #     labs(x="Hour") + labs(y="Dispatch (GW)") +
# #     scale_fill_manual(name="Energy Source", values = energy_source_palette) +
# #     facet_grid(period ~ Season) +
# #     #scale_x_continuous(limits = c(4,24), breaks=c(4,8,12,16,20,24)) +
# #     theme(axis.text.y = element_text(size=14), axis.title.x = element_text(size=16), axis.text.x = element_text(size=14), axis.title.y=element_text(size=16), 
# #           legend.text = element_text(size = 14), legend.title = element_text(size=14), legend.position="bottom", plot.title = element_text(size=13.5,hjust=0.5), 
# #           strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 16)) +
# #     guides(fill = guide_legend(nrow = 2))
# #     # guides(fill = guide_legend(nrow = 2)) +
# #     # ylim(0, 450)
# #   
# #   
# #   print(plot)
# #   
# #   dev.off() 
# # }

#Delta in daily dispatch by season, day type for each scenario vs baseline

baseline <- scenario_name_short1

dispatch_hourly_baseline <- dispatch_hourly %>% filter(Scenario == baseline)
# dispatch_hourly_baseline <- rename(dispatch_hourly_baseline, Baseline_Scenario = Scenario, DispatchGen_MW_baseline = DispatchGen_MW, Curtailment_MW_baseline = Curtailment_MW)
dispatch_hourly_baseline <- rename(dispatch_hourly_baseline, Baseline_Scenario = Scenario, DispatchGen_MW_baseline = DispatchGen_MW)


dispatch_hourly_delta0 <- dispatch_hourly %>% filter(Scenario != baseline)
dispatch_hourly_delta <- left_join(dispatch_hourly_delta0, dispatch_hourly_baseline, c("Season"="Season","period"="period", "Hour_PST_bin" = "Hour_PST_bin",
                                                                                       "day_type" = "day_type", "gen_energy_source" = "gen_energy_source"))

dispatch_hourly_delta$Delta_DispatchGen_MW <-dispatch_hourly_delta$DispatchGen_MW - dispatch_hourly_delta$DispatchGen_MW_baseline
# dispatch_hourly_delta$Delta_Curtailment_MW <-dispatch_hourly_delta$Curtailment_MW - dispatch_hourly_delta$Curtailment_MW_baseline

dispatch_hourly_delta$DispatchGen_perc_change_from_baseline <- round(dispatch_hourly_delta$Delta_DispatchGen_MW/dispatch_hourly_delta$DispatchGen_MW_baseline,3)
# dispatch_hourly_delta$Curtailment_perc_change_from_baseline <- round(dispatch_hourly_delta$Delta_Curtailment_MW/dispatch_hourly_delta$Curtailment_MW_baseline,3)

#output csv of DELTA daily dispatch by period for each scenario
write.csv(x = dispatch_hourly_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir,day,climate_impacts_included, "DELTA_hourly_dispatch_by_season_daytype_period_scenario.csv",sep=""), row.names = F)


#adding 0 rows when no generation of a type
hours <- unique(dispatch_hourly_delta$Hour_PST_bin)
gen_energy_sources <-unique(dispatch_hourly_delta$gen_energy_source)
periods <- unique(dispatch_hourly_delta$period)
seasons <- unique(dispatch_hourly_delta$Season)
scenarios <- unique(dispatch_hourly_delta$Scenario)
day_types <-unique(dispatch_hourly_delta$day_type)

combinations <- expand.grid(Hour_PST_bin = hours, gen_energy_source = gen_energy_sources, period = periods, Season = seasons,
                            Scenario = scenarios, day_type= day_types)
combinations$gen_energy_source <- as.character(combinations$gen_energy_source )
combinations$Season <- as.character(combinations$Season )
combinations$Scenario <- as.character(combinations$Scenario )
combinations$day_type <- as.character(combinations$day_type )

dispatch_hourly_delta_Full <- full_join(dispatch_hourly_delta, combinations, by = c("Hour_PST_bin" = "Hour_PST_bin", "gen_energy_source" = "gen_energy_source",
                                                                                                    "period"="period","Season"="Season","Scenario"="Scenario",
                                                                                                    "day_type"="day_type")) %>%
  mutate(Delta_DispatchGen_MW = ifelse(is.na(Delta_DispatchGen_MW) | is.nan(Delta_DispatchGen_MW) , 0, Delta_DispatchGen_MW), DispatchGen_MW = ifelse(is.na(DispatchGen_MW), 0, DispatchGen_MW))

#reorder factors
dispatch_hourly_delta_Full$Season <- factor(dispatch_hourly_delta_Full$Season , levels=c("Winter", "Spring","Summer","Fall"))


###Focus on ACCESS scenario

ACCESS_dispatch_hourly_delta_Full <- dispatch_hourly_delta_Full %>% filter(Scenario == "ACCESS-1.0" & day_type == "peak_day" & period == 2050)
ACCESS_dispatch_hourly_delta_Full<-ACCESS_dispatch_hourly_delta_Full[!(is.na(ACCESS_dispatch_hourly_delta_Full$DispatchGen_MW_baseline)),]

#adding 0 rows when no generation of a type
hours <- unique(ACCESS_dispatch_hourly_delta_Full$Hour_PST_bin)
gen_energy_sources <-unique(ACCESS_dispatch_hourly_delta_Full$gen_energy_source)
periods <- unique(ACCESS_dispatch_hourly_delta_Full$period)
seasons <- unique(ACCESS_dispatch_hourly_delta_Full$Season)
scenarios <- unique(ACCESS_dispatch_hourly_delta_Full$Scenario)
day_types <-unique(ACCESS_dispatch_hourly_delta_Full$day_type)

combinations <- expand.grid(Hour_PST_bin = hours, gen_energy_source = gen_energy_sources, period = periods, Season = seasons,
                            Scenario = scenarios, day_type= day_types)
combinations$gen_energy_source <- as.character(combinations$gen_energy_source )
combinations$Season <- as.character(combinations$Season )
combinations$Scenario <- as.character(combinations$Scenario )
combinations$day_type <- as.character(combinations$day_type )

ACCESS_dispatch_hourly_delta_Full2 <- full_join(ACCESS_dispatch_hourly_delta_Full, combinations, by = c("Hour_PST_bin" = "Hour_PST_bin", "gen_energy_source" = "gen_energy_source",
                                                                                    "period"="period","Season"="Season","Scenario"="Scenario",
                                                                                    "day_type"="day_type")) %>%
  mutate(Delta_DispatchGen_MW = ifelse(is.na(Delta_DispatchGen_MW) | is.nan(Delta_DispatchGen_MW) , 0, Delta_DispatchGen_MW), DispatchGen_MW = ifelse(is.na(DispatchGen_MW), 0, DispatchGen_MW),
         DispatchGen_MW_baseline = ifelse(is.na(DispatchGen_MW_baseline), 0, DispatchGen_MW_baseline))

#reorder factors
ACCESS_dispatch_hourly_delta_Full2$Season <- factor(ACCESS_dispatch_hourly_delta_Full2$Season , levels=c("Winter", "Spring","Summer","Fall"))

#FIGURE 4B

#facet plot with WECC-wide DELTA average daily dispatch per season for peak day
# library(gridExtra)

plot_file_name <- paste("Fig4B_2050_DELTA_Seasonal_dispatch_ACCESS_peak_day" ,climate_impacts_included,".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=1800, height=1000, res=100)

plot1 <- ggplot(data=ACCESS_dispatch_hourly_delta_Full2, aes(x=Hour_PST_bin, y=DispatchGen_MW_baseline/10^3, fill=factor(gen_energy_source))) +
  ggtitle(paste("WECC 2050 average peak day dispatch, Baseline Scenario",sep=" ")) +
  geom_area() +
  labs(x="Hour") + labs(y="Dispatch (GW)") +
  scale_fill_manual(name="Energy source", values = energy_source_palette) +
  facet_grid(. ~ Season) +
  scale_x_continuous(limits = c(0,24), breaks=c(4,8,12,16,20,24)) +
theme(axis.text.y = element_text(size=24), axis.title.x = element_text(size=26), axis.text.x = element_text(size=24), axis.title.y=element_text(size=26),
  legend.text = element_text(size = 24, nrow(2)), legend.title = element_text(size=26), legend.position="none", plot.title = element_text(size=30,hjust=0.5),
  strip.text.x = element_text(size = 24), strip.text.y = element_text(size = 24), plot.margin = margin(10, 20, 10, 10))
  theme(axis.text.y = element_text(size=14), axis.title.x = element_text(size=16), axis.text.x = element_text(size=14), axis.title.y=element_text(size=16),
        legend.position="none", plot.title = element_text(size=16,hjust=0.5),
        strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 16))

plot2 <- ggplot(data=ACCESS_dispatch_hourly_delta_Full2, aes(x=Hour_PST_bin, y=Delta_DispatchGen_MW/10^3, fill=factor(gen_energy_source))) +
  ggtitle(paste("Change in 2050 WECC peak day dispatch, ACCESS-1.0 Scenario",sep=" ")) +
  geom_area(position = "identity") +
  labs(x="Hour") + labs(y="Delta dispatch (GW)") +
  scale_fill_manual(name="Energy source", values = energy_source_palette) +
  facet_grid(. ~ Season) +
  scale_x_continuous(limits = c(0,24), breaks=c(4,8,12,16,20,24)) +
  theme(axis.text.y = element_text(size=24), axis.title.x = element_text(size=26), axis.text.x = element_text(size=24), axis.title.y=element_text(size=26),
legend.text = element_text(size = 24, nrow(2)), legend.title = element_text(size=26), legend.position="bottom", plot.title = element_text(size=30,hjust=0.5),
strip.text.x = element_text(size = 24), strip.text.y = element_text(size = 24), plot.margin = margin(10, 20, 10, 10))

plot_grid(plot1, plot2, rel_heights=c(0.8,1), nrow=2, align="v")

# grid.arrange(plot1, plot2, nrow = 2)

dev.off()

## Tx capacity buildout#########


tx_build <- data.frame()

for (i in 1:length(run_name_list)){
  
  run_name <- run_name_list[[i]]
  scenario <- short_scenario_list[[i]]
  
  #read in the transmission capacity builds and transmission project information 
  tx_build_all <- read.csv(file=paste(switch_input_dir,run_name,"/outputs/","transmission.csv",sep=""),stringsAsFactors=F, header = T, fill = TRUE) 
 
  # selecting only the columns of interest
  tx_build_all <- subset(tx_build_all, select=c(TRANSMISSION_LINE,	PERIOD,	trans_lz1,	trans_lz2,	existing_trans_cap,	BuildTx,	TxCapacityNameplate,	TxCapacityNameplateAvailable,	TotalAnnualCost))
  tx_build_all$BuildTx <- ifelse(tx_build_all$BuildTx==".", 0, tx_build_all$BuildTx) 
  
  tx_build_all$BuildTx <- as.numeric(tx_build_all$BuildTx )
  #create cumulative sum of Tx build by period and line
  tx_build_all <- tx_build_all %>%                             # Apply group_by & mutate functions
    group_by(TRANSMISSION_LINE, trans_lz1, trans_lz2) %>%
    dplyr::mutate(cumulative_BuiltTx_MW = cumsum(BuildTx))

  
  tx_build_all$Scenario <- scenario
  
  #capacity online by load zone and period for each scenario
  tx_build <- rbind(tx_build, tx_build_all)
  
}


#output csv of Tx capacity installed by load zone by period for each scenario
write.csv(x = tx_build, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, day,climate_impacts_included, "Tx_Builds_by_line.csv",sep=""), row.names = F)

#total WECC capacity builds by period for each scenario
tx_build_period_WECC <- tx_build %>% group_by(Scenario, PERIOD) %>% summarize(BuildTx = sum(BuildTx), TxCapacityNameplate = sum(TxCapacityNameplate) )
tx_build_period_WECC <- tx_build_period_WECC %>% group_by(Scenario) %>% dplyr::mutate(cumulative_BuiltTx_MW = cumsum(BuildTx))
tx_build_period_WECC$existing_Tx_MW <- tx_build_period_WECC$TxCapacityNameplate - tx_build_period_WECC$BuildTx

#output csv of WECC tx capacity installed by period for each scenario
write.csv(x = tx_build_period_WECC, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, day,climate_impacts_included, "WECC_Tx_Builds.csv",sep=""), row.names = F)

tx_build_period_WECC_long <- gather(tx_build_period_WECC, Tx_type, MW_Tx, 3:6)

tx_build_period_WECC_long$Tx_type <- ifelse(tx_build_period_WECC_long$Tx_type == "existing_Tx_MW", "Existing Tx Capacity", tx_build_period_WECC_long$Tx_type)
tx_build_period_WECC_long$Tx_type <- ifelse(tx_build_period_WECC_long$Tx_type == "BuildTx", "New Tx Capacity Built", tx_build_period_WECC_long$Tx_type)
tx_build_period_WECC_long$Tx_type <- ifelse(tx_build_period_WECC_long$Tx_type == "cumulative_BuiltTx_MW", "Cumulative New Tx Capacity Built", tx_build_period_WECC_long$Tx_type)
tx_build_period_WECC_long$Tx_type <- ifelse(tx_build_period_WECC_long$Tx_type == "TxCapacityNameplate", "Total Tx Nameplate Capacity", tx_build_period_WECC_long$Tx_type)


#plot of total WECC tx capacity builds for each period BASELINE SCENARIO
baseline <- scenario_name_short1

#reorder factors
tx_build_period_WECC_long$PERIOD <- factor(tx_build_period_WECC_long$PERIOD, levels = c( "2030", "2035", "2040", "2045", "2050"))
tx_build_period_WECC_long$Tx_type <- factor(tx_build_period_WECC_long$Tx_type, levels = c("New Tx Capacity Built","Existing Tx Capacity", "Cumulative New Tx Capacity Built", "Total Tx Nameplate Capacity"))

plot_file_name <- paste("SI_WECC_Tx_build_BASELINE_scenario",day, ".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=700, height=700, res=100)

ggplot(data=subset(tx_build_period_WECC_long, Scenario == baseline & Tx_type %in% c("Existing Tx Capacity","New Tx Capacity Built")), aes(x=PERIOD, y=MW_Tx/1000, fill = Tx_type, label=round(MW_Tx/1000,0))) +
  ggtitle(paste("WECC transmission capacity built by period, Baseline scenario", sep=", ")) +
  geom_bar(stat="identity") +
  labs(x="Investment Period") + labs(y="Tx Capacity (GW)") +
  scale_fill_manual(name="Transmission Type",  values =  c("red","pink")) +
  theme(axis.text.y = element_text(size=14), axis.title.x = element_text(size=16), axis.text.x = element_text(size=14, angle = 45), axis.title.y=element_text(size=16),
        legend.text = element_text(size = 12), legend.title = element_text(size=12), legend.position="bottom", plot.title = element_text(size=16,hjust=0.5),
        strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 16)) +
  guides(fill = guide_legend(nrow = 2))

dev.off()

#facet plot of total WECC existing and new Tx capacity for each period and scenario
#reorder scenario

#facet plot by scenario, years on x axis
plot_file_name <- paste("SI_facet_plot","_WECC_Tx_all_scenarios",climate_impacts_included,day, ".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=1100, height=1000, res=100)

plot <- ggplot(data=subset(tx_build_period_WECC_long, Scenario != baseline & Tx_type %in% c("Existing Tx Capacity","New Tx Capacity Built")), aes(x=PERIOD, y=MW_Tx/1000, fill = Tx_type, label=round(MW_Tx/1000,0))) +
  ggtitle(paste("WECC transmission capacity built by period and scenario",climate_impacts_included, sep=", ")) +
  geom_bar(stat="identity") +
  labs(x="Investment Period") + labs(y="Tx Capacity Built (GW)") +
  scale_fill_manual(name="Transmission Type",  values =  c("red","pink")) +
  facet_wrap( ~ Scenario, nrow=3) +
  theme(axis.text.y = element_text(size=14), axis.title.x = element_text(size=16), axis.text.x = element_text(size=14, angle = 90), axis.title.y=element_text(size=16),
        legend.text = element_text(size = 14), legend.title = element_text(size=14), legend.position="bottom", plot.title = element_text(size=13.4,hjust=0.5),
        strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 16)) +
  guides(fill = guide_legend(nrow = 2))

print(plot)

dev.off()


#Delta cumulative Tx between baseline and CC scenarios

baseline <- scenario_name_short1

tx_build_period_WECC_long_baseline <- tx_build_period_WECC_long %>% filter(Scenario == baseline)
tx_build_period_WECC_long_baseline <- rename(tx_build_period_WECC_long_baseline, Baseline_Scenario = Scenario, MW_Tx_baseline = MW_Tx)

tx_build_period_WECC_long_delta0 <- tx_build_period_WECC_long %>% filter(Scenario != baseline)
tx_build_period_WECC_long_delta <- left_join(tx_build_period_WECC_long_delta0, tx_build_period_WECC_long_baseline, c("PERIOD"="PERIOD", "Tx_type" = "Tx_type"))

tx_build_period_WECC_long_delta$Delta_MW_Tx <-tx_build_period_WECC_long_delta$MW_Tx - tx_build_period_WECC_long_delta$MW_Tx_baseline

tx_build_period_WECC_long_delta$Tx_perc_change_from_baseline <- round(tx_build_period_WECC_long_delta$Delta_MW_Tx/tx_build_period_WECC_long_delta$MW_Tx_baseline,3)

#output csv of DELTA load zone capacity online by period for each scenario
write.csv(x = tx_build_period_WECC_long_delta, file = paste(switch_output_dir, switch_run_dir, weap_run_dir, day,climate_impacts_included, "DELTA_WECC_Tx_by_period_scenario.csv",sep=""), row.names = F)


#FIGURE 3A

# #Plot of Deltas of capacity WECC wide compared to baseline scenario 1
library(cowplot)
#reorder scenario

tx_build_period_WECC_long_delta2050 <- tx_build_period_WECC_long_delta %>% filter(PERIOD == 2050 & Tx_type == "Cumulative New Tx Capacity Built") 
baseline_existing_tx <- tx_build_period_WECC_long_delta %>% filter(PERIOD == 2030 & Tx_type == "Existing Tx Capacity") 
baseline_cumulative_built_tx <- tx_build_period_WECC_long_delta %>% filter(PERIOD == 2050 & Tx_type ==  "Cumulative New Tx Capacity Built") 
baseline_existing_cumulative_built_tx <- rbind(baseline_existing_tx,baseline_cumulative_built_tx)
baseline_existing_cumulative_built_tx$Tx_type <- factor(baseline_existing_cumulative_built_tx$Tx_type, levels = c("Cumulative New Tx Capacity Built", "Existing Tx Capacity"))

tx_build_period_WECC_long_delta2050 <- ungroup(tx_build_period_WECC_long_delta2050) %>% mutate(Scenario = fct_reorder(Scenario, Delta_MW_Tx, max, .desc = FALSE))

#Figure 3A  resorted by cumulative gen capacity built

tx_build_period_WECC_long_delta2050$Scenario <- factor(tx_build_period_WECC_long_delta2050$Scenario, levels = cumulative_buildgen_order)
baseline_existing_cumulative_built_tx2 = baseline_existing_cumulative_built_tx
baseline_existing_cumulative_built_tx2$Baseline_Scenario <- ifelse(baseline_existing_cumulative_built_tx2$Baseline_Scenario == "Baseline no CC", "Baseline", baseline_existing_cumulative_built_tx2$Baseline_Scenario)

plot_file_name <- paste("Fig3A_Delta_WECC_Tx_cumulative_build_",climate_impacts_included, day,".png", sep="")

png(paste(switch_output_dir,switch_run_dir, weap_run_dir, plot_file_name, sep=""), width=2300, height=700, res=100)

plot1 <- ggplot(data=subset(baseline_existing_cumulative_built_tx2, Scenario == "ACCESS-1.0"), aes(x=Baseline_Scenario, y=MW_Tx_baseline/1000, fill = Tx_type)) + 
  ggtitle(paste("Baseline 2050 transmission", sep=", ")) +
  geom_bar(stat="identity") +
  labs(x="Scenario") + labs(y="Tx capacity (GW)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(name="Transmission type",  values =  c("red","pink"), labels=c("New Tx built", "Existing Tx")) +
  theme(axis.text.y = element_text(size=22), axis.title.x = element_text(size=24), axis.text.x = element_text(size=22, angle = 90), axis.title.y=element_text(size=24), 
        legend.text = element_text(size = 22, nrow(2)), legend.title = element_text(size=24), legend.position="right", plot.title = element_text(size=26,hjust=0.1), 
        strip.text.x = element_text(size = 24), strip.text.y = element_text(size = 24), plot.margin = margin(10, 10, 10, 10))

plot2 <- ggplot(data=subset(tx_build_period_WECC_long_delta2050, Tx_type == "Cumulative New Tx Capacity Built"), aes(x=Scenario, y=Delta_MW_Tx/1000)) + 
  ggtitle(paste("Change in cumulative new transmission capacity built relative to Baseline Scenario", sep=", ")) +
  geom_bar(stat="identity", fill="red") +
  scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept=0) +
  labs(x="Scenario") + labs(y="Delta Tx capacity built (GW)") +
  theme(axis.text.y = element_text(size=22), axis.title.x = element_text(size=24), axis.text.x = element_text(size=22, angle = 90), axis.title.y=element_text(size=24), 
        legend.text = element_text(size = 22, nrow(2)), legend.title = element_text(size=24), legend.position="none", plot.title = element_text(size=26,hjust=0.5), 
        strip.text.x = element_text(size = 24), strip.text.y = element_text(size = 24), plot.margin = margin(10, 20, 10, 10))

plot_grid(plot1, plot2, rel_widths=c(1,3), align = 'hv')

dev.off()  


