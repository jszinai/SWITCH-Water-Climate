# SWITCH-Water-Climate
Electricity system modeling incorporating climate impacts, adaptation, and water interactions

This repository archives the code needed to link outputs of the Western U.S. Water Systems Model (WWSM) created within the WEAP software platform with the electricity system capacity expansion model SWITCH under a number of climate scenarios, as well as the code to analyze and plot the results of the coupled models. The modeling methods are documented in the manuscript in submission:

J.K. Szinai ^1, D.Yates ^2, P.S.Pérez ^3, M.Staadecker ^4, D.Kammen ^5, A.D.Jones ^1,5, P.Hidalgo-Gonzalez ^6. Climate change and its influence on water systems increases the cost of electricity system decarbonization. Preprint at https://doi.org/10.21203/rs.3.rs-3359999/v1 (2023).

1 Lawrence Berkeley National Lab

2 National Center for Atmospheric Research

3 National Renewable Energy Lab

4 University of Toronoto

5 UC Berkeley

6 UC San Diego

# Abstract

The electricity sector faces a dual challenge: decarbonization and adaptation to climate change. In many regions, this challenge is complicated by interdependence of electricity and water systems, through hydropower and energy-intensive water resources. By coupling detailed water and electricity system models, we evaluate how climate change alters pathways to carbon-free generation across the Western Interconnect, emphasizing water interactions. We find that grid planning ignoring climate and water linkages underestimates the magnitude, type, and location of capacity needed to achieve decarbonization. By 2050, electricity use could grow by up to 2% annually but up to 8% in July from cooling and water-related electricity demand, while hydropower generation could decrease annually by 23%. Here, we show that to adapt, the region would need to build up to 139 GW of capacity between 2030 and 2050, equivalent to nearly thrice California’s peak demand, and could incur up to $150 billion (+7%) in extra costs.

# Data pre-processing and model linkages
1. \Calculating energy demand and hydropower changes and coupling with SWITCH\: This folder contains the input data and the scripts to couple the hydropower and energy demand related to water results from the WWSM model results with the SWITCH electricity system model
   
\WEAP_Nov_8_2022results\data\Results\SupplyDelivered_Hydropower_EnergyUse\: This folder contains the results on hydropower potential and energy use related to water from the WWSM water model under 15 climate scenarios and the historical reference climate.

Mapping_WEAP_energy_for_water_to_SWITCH_loadzonesOct2022_v1.xlsx: This is a mapping file used to link the names of the objects in the WWSM model with SWITCH inputs

SWITCH_WEAP_hydropower_handshake.R: This script uses the hydropower potential results from the WWSM folder above, and adjusts the hydropower input files for SWITCH for each of the 15 climate scenarios

SWITCH_WEAP_energy_demand_handshake_andFig6: This script uses the energy demand related to water results from the WWSM folder above, maps the results with the mapping file to the SWITCH load zones, and adjusts the SWITCH load input files for each of the 15 climate scenarios. The script also creates Fig. 6 for the manuscript.

2. \Calculating heating and cooling load changes and coupling with SWITCH\: This folder contains the input data and the scripts to calculate the change in heating and cooling degree days and the subsequent load changes based on load sensitivity factors, and adjust the SWITCH input data for each of the 15 climate scenarios 

\Daily Tmin and Tmax data\: This folder contains all of the raw climate model data on minimum daily temperatures and maximum daily temperatures at the centroid of each of the SWITCH load zones. There is a folder for each climate scenario and the historical climate.

HDDCDD.R: This script calculates the Heating Degree Days (HDD) and Cooling Degree Days (CDD) for each climate scenario using the Daily Tmin and Tmax data from the folder above, and outputs the time series of HDD and CDD

Pre_process_HDD_CDD_data_pc.R: This script calculates the daily delta in HDD and CDD between the historical climate data and each of the 15 climate scenarios for the population-weighted centroids of the SWITCH load zones

Daily_delta_CDD_HDD_lz_scenario_15scenarios.csv: This is the csv file of the daily delta CDD and HDD by load zone and scenario produced by the Pre_process_HDD_CDD_data_pc.R script and is uploaded to the SWITCH database

hdd_cdd_sql_queries_for_2023_CC_scenarios.sql: This script contains a set of SQL queries used to upload the daily delta of HDD and CDD into the SWITCH database, interpolate the load-temperature sensitivity factors for each SWITCH load zone and hour from the ReEDS zones and time slices, calculate the change in total hourly load for each SWITCH load zone and climate scenario, and create a new demand time series with the changed load. It also creates the scenarios for the SWITCH database that correspond to the new demand timeseries and time sampling time points for each climate scenario

\Timesample\: This folder contains the scripts and input data to calculate the peak and median day of each month for each investment period from the adjusted demand time series from each climate scenario, and produced the set of sampled time points for each scenario to be uploaded with the queries above to the SWITCH db.

# Analysis and plotting of results
1. \Scripts to analyze and plot SWITCH results\: This folder contains the scripts to analyze and plot the results of the SWITCH climate scenarios. It contains an example set of SWITCH input and output data for the ACCESS-1.0 scenario.
Comparison plots of different WEAP-SWITCH climate scenario runs_Fig2ab_3abc_4ab.R: This script reads in the SWITCH results for all climate scenarios and baseline, analyzes the results and produces Figures 2a, 2b, 3a, 3b, 3c, 4a, and 4b

Precip_temp_and_load_hydropower_changes_Fig2c.R: This scripts calculates the average change in precipitation and temperature from the climate scenario data used in the WWSM water model (and SWITCH), aggregated by sub-region of the Western Interconnect, and plots the changes with the changes in hydropower and load from the SWITCH results (from file Cooling + Hydropower and Water Load ScenariosDELTA_lz_hydro_load_disagg.csv) to make Figure 2c.

Cooling + Hydropower and Water Load ScenariosDELTA_lz_hydro_load_disagg.csv: This the input file to the above script that has the aggregated results from the SWITCH climate scenarios 

Maps of capacity and dispatch deltas between Baseline and CC scenarios_Fig1Fig5aFig5b.R: This script calculates the delta of capacity online and dispatch for the climate scenarios compared to the SWITCH baseline and produces the figures and maps of Figure 5a and Figure 5b

\id_202_WECC_0_carbon_baseline_5y_24_sample_barrier_ACCESS-1.0_CDD_HDD_Hydro_Load\: This folder contains an example set of SWITCH input and output data for the ACCESS-1.0 scenario.

2. \Manuscript figures and summary files\: This folder contains the figures for the manuscript and its Supplementary Information (SI), and also has summary Excel files used to analyze and display data for the tables in the manuscript and SI.


