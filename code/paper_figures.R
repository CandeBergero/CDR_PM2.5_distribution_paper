# Paper_figures.R
# This script is designed to create the figures for the equity and air pollution paper
# Last modified: October-November 2023
# Author: Candelaria Bergero

#--------------------------------------------------------------------------------------------------

#### Load packages ####
library(plyr)
library(readr)
library(ggplot2)
library(devtools)
devtools::load_all("/Users/mariacandelariabergero/Documents/GCAM/rgcam-1.2.0")
library(rgcam)
library(tidyr)

library(gcamdata)
library(directlabels)
library(viridisLite)
library(usmap)
library(forcats)
library(processx)
library(magrittr)
library(reticulate)
library(plotly)
library(sf)
library(ggspatial)
library(ncdf4)
library(raster)
library(RColorBrewer)
library(dplyr)

#### Load scenarios ####
#Set working directory
setwd("~/Documents/GCAM/gcam-v6.0-Mac-Release-Package/output/R_scripts_Just_transition")

#Connect to the database
conn <- localDBConn('/Users/mariacandelariabergero/Documents/GCAM/gcam-v6.0-Mac-Release-Package/output', 'database_basexdb_FINAL')

#Load the scenario
# prj <- addScenario(conn, 'dat/GCAM_analysis_V3.dat', 'GCAM-USA_REF', 'queries/queries.xml') #Reference
# prj <- addScenario(conn, 'dat/GCAM_analysis_V3.dat', 'GCAM-USA_DAC_NZUSA2050ghg_NZROW2060co2_newEFV2_allEF', 'queries/queries.xml') #Net-zero + high CDR
# prj <- addScenario(conn, 'dat/GCAM_analysis_V3.dat', 'GCAM-USA_withDAC_NZUSA2050ghg_NZROW2060co2_forest_CCSV3_allEF', 'queries/queries.xml') #Net-zero + lowCDR

#Load the project
prj <- loadProject('dat/GCAM_analysis_V3.dat')

#Get color schemes
source( "~/Documents/GCAM/gcam-v6.0-Mac-Release-Package/output/R_scripts_Just_transition/code/color_schemes.R" ) # some predefined color schemes
#### Mapping files ####
scenario_mapping <- read_csv("mappings/scenario_mapping.csv")
PE_fuel_mapping <- read_csv("mappings/PE_fuel_mapping.csv", skip = 1)
CO2_sector_mapping <- read_csv("mappings/CO2_mapping_v2.csv")
GHG_mapping <- read_csv("mappings/GHG_conv_mapping.csv", skip = 1)
air_pollution_sector_mapping <- read_csv("mappings/air_pollution_sectors_mapping_V2.csv")

us_states <- st_read("shapefiles/cb_2019_us_state_500k/cb_2019_us_state_500k.shp")
usa_boundary <- st_union(us_states) #Get outter borders

state_region_mapping <- read.csv("mappings/state_region_mapping.csv") %>%
  mutate( state_ID = gsub("^_", "", state_ID))

cbsa_shapefile <- st_read("shapefiles/cb_2019_us_cbsa_500k/cb_2019_us_cbsa_500k.shp") %>% dplyr::select(AFFGEOID, NAME, geometry)

#### Other####
#Filters 
USA_all <- c("USA", "AL","AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC","FL",
             "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
             "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
             "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
             "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
             "WY") # Include USA

GHG_filter <- c(unique(GHG_mapping$GHG))

#Order
primary_energy_order <- c( "Solar",  "Wind", "Hydro", "Biomass", "Nuclear", "Coal", "Oil","Natural Gas")

scenario_short_facet_order <- c("Net-zero, High BECCS, High DAC", "Net-zero, Low BECCS, Low DAC", "Reference")

GHG_sector_order <- c("HFCs & SF6",  "N2O", "CH4", "Other energy processing", "Refining", "Buildings", "Industry", "Transport", "Electricity", "Electricity BECCS", "DAC", "Refining BECCS",  "H2 Production","Land sink", "Industrial feedstocks")

PM_sector_order <- c("Electricity BECCS", "Refining bioliquids", "Other energy processing","Urban processes", "Refining", "Electricity", "Transportation","Buildings", "Industry")

industry_sector_order <- c("cement", "other industrial energy use", "industrial processes")

#Constants
emissions.CONV_C_CO2    <- 44 / 12 # Convert Carbon to CO2

#Figure format
figure_theme <-       theme(panel.background = element_blank(),
                            panel.grid.minor = element_blank(),
                            # panel.grid.major.x = element_line( size=.4, color="gray"),
                            # panel.grid.major.y = element_line( size=.4, color="gray"),
                            panel.spacing = unit(2, "lines"),
                            plot.title = element_text(face="bold", size=40, hjust = 0.5),
                            axis.title.x = element_text(size=10),
                            axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
                            axis.title.y = element_text(size=15),
                            axis.text.y  = element_text(size=15),
                            strip.text = element_text(size=15),
                            legend.title = element_text(size = 15),
                            legend.text = element_text(size = 10))

colors <- rev(brewer.pal(n = 11, name = "RdYlBu"))
#--------------------------------------------------------------- FIGURE 1 -------------------------------------------------------------------
#Figure 1 has 9 panels total
#Rows: primary energy consumption, CO2 emissions by sector, air pollution PM
#Columns: High CDR, Low CDR, differences
#### 1.1. Primary energy ####
    primary_energy_consumption <- getQuery(prj, "primary energy consumption by region (direct equivalent)")
    
    primary_energy_consumption %>%
      filter(year > 2015 & year < 2055,
             !fuel %in% c("traded coal", "traded natural gas", "traded oil")) %>%
      left_join(scenario_mapping, by = "scenario") -> primary_energy_consumption_states
    
    primary_energy_consumption_states %>%
      left_join(PE_fuel_mapping, by = "fuel") %>%
      filter(!region %in% c("AK", "HI")) %>% #Filter out Alaska and Hawaii
      group_by(year, Units, Scenario, agg_fuel) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      mutate(Fuel = factor(agg_fuel, levels = primary_energy_order)) %>%
      mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> primary_energy_consumption_USA
    
    #### 1.1.1. All scenarios ####
    p <- ggplot() + geom_area(data=primary_energy_consumption_USA, aes(x=year, y=sum, fill= Fuel)) +
      scale_fill_manual(values=primary_energy_color_2) +
      facet_wrap( ~ Scenario, ncol = 2, as.table = FALSE) +
      scale_x_continuous(breaks=seq(2020,2100,10)) +
      ggtitle( "Primary Energy Consumption by Fuel" ) +
      xlab("Year") +
      ylab("EJ") +
      figure_theme 
    #ggsave("figures/paper_figures/1.1.primary_energy_fuel_USA.png", dpi=600/2, width=6000/300, height=3000/300)
    #ggsave("figures/paper_figures/1.1.primary_energy_fuel_USA.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    #write.csv(primary_energy_consumption_USA, "primary_energy_consumption_USA.csv")
    #Numbers for paper
    primary_energy_consumption_USA %>%
      group_by(Scenario, year) %>%
      summarise(total = sum(sum)) %>%
      ungroup() -> total
    
    primary_energy_consumption_USA %>%
      left_join(total, by = c("Scenario", "year"))  %>%
      mutate(percentage = round((sum * 100)/ total))-> percentages
    
#### 1.2. CO2 emissions #####
    #### 1.2.1. CO2 from energy ####
    #Here we bring in the 3 files with emissions per sector and state where biomass has been allocated to each sector
    #Thus there is no "regional biomass" sector
    #This was processed in another script in folder "gcam-usa-nobio-accounting-main"
    CO2_sector_nobio_REF <- read_csv("/Users/mariacandelariabergero/Documents/GCAM/gcam-usa-nobio-accounting-main/csv/FINAL/P_co2_sector_GCAM-USA_REF.csv") #Units are MtC
    CO2_sector_nobio_lowCDR <- read_csv("/Users/mariacandelariabergero/Documents/GCAM/gcam-usa-nobio-accounting-main/csv/FINAL/P_co2_sector_GCAM-USA_withDAC_NZUSA2050ghg_NZROW2060co2_forest_CCSV3_allEF.csv")
    CO2_sector_nobio_highCDR <- read_csv("/Users/mariacandelariabergero/Documents/GCAM/gcam-usa-nobio-accounting-main/csv/FINAL/P_co2_sector_GCAM-USA_DAC_NZUSA2050ghg_NZROW2060co2_newEFV2_allEF.csv")
    
    #Merge 3 scenarios
    CO2_sector_nobio_REF %>%
      bind_rows(CO2_sector_nobio_lowCDR, CO2_sector_nobio_highCDR) %>%
      mutate(Units = "MtC")-> CO2_nobio_all
    
    CO2_nobio_all %>%
      left_join(scenario_mapping, by = "scenario") %>%
      filter(!region %in% c("AK", "HI")) %>% #Filter out Alaska and Hawaii
      group_by(Scenario, Units, year) %>%
      summarise(sum = sum(co2.emiss)) %>%
      ungroup() %>%
      mutate(sum = sum * emissions.CONV_C_CO2,
             Units = "MtCO2")-> total_CO2_energy # Total CO2 emissions from energy by year and scenario

    #Process data
    CO2_nobio_all %>%
      filter(year > 2010 & year < 2055) %>%
      filter(!region %in% c("AK", "HI")) %>% #Filter out Alaska and Hawaii
      left_join(CO2_sector_mapping, by = "sector") %>%
      left_join(scenario_mapping, by = "scenario") %>%
      #Convert from MtC to MtCO2
      mutate(value = co2.emiss * emissions.CONV_C_CO2,
             Units = "MtCO2eq") %>%
      group_by(agg_sector, year, Scenario, Units) %>%
      summarise(total_USA = sum(value)) %>%
      ungroup() %>%
      rename(GHG = agg_sector)-> CO2_nobio_all_sector #CO2 emissions from energy by Sector
    
    #### 1.2.2. LUC emissions #####
    LUC_emissions_region <- getQuery(prj, "LUC emissions by region" )
    
    LUC_emissions_region %>%
      filter(year > 2010,
             year < 2055,
             region == "USA",
             !(grepl("PacArctic|Hawaii", landleaf))) %>% #Filter out Alaska and Hawaii
      mutate(value_MtCO2 = (value * emissions.CONV_C_CO2),
             Units = "MtCO2eq",
             GHG = "CO2_LUC") %>%
      group_by(Units, scenario, year, region) %>%
      summarise(total_USA = sum(value_MtCO2)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      dplyr::select(Scenario, Units, year, total_USA) %>%
      mutate(GHG = "Land sink")-> LUC_emissions_region_final #CO2 emissions from land
    
    
    #### 1.2.3. GHG emissions #####
    GHG_emissions_region <- getQuery(prj, "nonCO2 emissions by region")
    
    GHG_emissions_region %>%
      filter(year > 2010,
             year < 2055,
             ghg %in% GHG_filter,
             ghg != "CO2") %>%
      filter(grepl("grid", region) | region %in% USA_all) %>%
      left_join(GHG_mapping, by = c("ghg" = "GHG")) %>%
      mutate(total = value * GWP,
             Units = "MtCO2eq") %>%
      mutate(GHG = if_else(ghg %in% c("CH4", "CH4_AGR", "CH4_AWB"), "CH4",
                           if_else(ghg %in% c("N2O", "N2O_AGR", "N2O_AWB"), "N2O",
                                   if_else(ghg %in% c("HFC125", "HFC134a", "HFC143a", "HFC23", "HFC32", "SF6"), "HFCs & SF6", ghg), ghg), ghg)) %>%
      filter(!region %in% c("AK", "HI")) %>% #Filter out Alaska and Hawaii
      group_by(scenario, year, Units, GHG) %>%
      summarise(total_USA = sum(total)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      dplyr::select(Scenario, Units, year, total_USA, GHG)-> GHG_emissions_region_complete #Other GHGs (CH4, N2O, HFCs)
    
    #### 1.2.4. Get negative emissions #### 
    #Get negative CO2
    #CO2_seq_tech <- getQuery(prj, "CO2 sequestration by tech")
    CO2_seq_tech_elec <- getQuery(prj, "CO2 sequestration by electricity tech (USA)")
    
    ###Here we deal with electricity BECCS
    CO2_seq_tech_elec %>%
      filter(subsector...5 == "biomass") %>%
      left_join(scenario_mapping, by = "scenario") %>%
      filter(!region %in% c("AK", "HI")) %>% #Filter out Alaska and Hawaii
      group_by(Scenario, year, Units) %>%
      summarise(value=sum(value)) %>%
      ungroup() %>%
      mutate(BECCS_electricity = value * emissions.CONV_C_CO2,
             Units = "MtCO2eq") %>%
      dplyr::select(-value)-> BECCS_electricity
    
    CO2_nobio_all_sector %>%
      filter(GHG == "Electricity") %>%
      left_join(BECCS_electricity, by = c("Scenario", "year", "Units")) %>%
    mutate(BECCS_electricity = if_else(is.na(BECCS_electricity), 0, BECCS_electricity)) %>%
      mutate(total_USA = total_USA + BECCS_electricity) %>%
      dplyr::select(-BECCS_electricity) -> CO2_nobio_all_sector_electircity
    
    #Join electricity and electricity BECCS
    BECCS_electricity %>%
      mutate(total_USA = BECCS_electricity * -1) %>%
      mutate(GHG = "Electricity BECCS") %>%
      dplyr::select(-BECCS_electricity) %>%
      bind_rows(CO2_nobio_all_sector_electircity)-> CO2_nobio_all_sector_electircity_complete
    
    #### 1.2.5. Graph all emissions #### 
    #All emissions
    CO2_nobio_all_sector %>%
      filter(GHG != "Electricity") %>%
      bind_rows(CO2_nobio_all_sector_electircity_complete) %>%
      bind_rows(LUC_emissions_region_final, GHG_emissions_region_complete) %>% 
      mutate(GHG = factor(GHG, levels = GHG_sector_order)) %>%
      mutate(total_USA = total_USA / 10^3,
             Units = "GtCO2eq")-> all_GHG_emissions

    #Net GHG emissions
    all_GHG_emissions %>%
      group_by(Scenario, year) %>%
      summarise(net = sum(total_USA)) -> all_GHG_emissions_net
    
    #Net carbon emissions
    all_GHG_emissions %>%
      filter(!GHG %in% c("CH4", "HFCs & SF6", "N2O")) %>%
      group_by(Scenario, year) %>%
      summarise (net = sum(total_USA)) -> all_GHG_emissions_net_CO2
  
    p <- ggplot() + geom_area(data=all_GHG_emissions, aes(x=year, y=total_USA, fill= GHG)) +
      geom_line(data = all_GHG_emissions_net, aes(x=year, y=net), color = "#264653", linetype = "dashed") +
      geom_line(data = all_GHG_emissions_net_CO2, aes(x=year, y=net)) +
      scale_fill_manual(values=CO2_sector_color) +
      scale_x_continuous(breaks=seq(2010,2100,10)) +
      facet_wrap( ~ Scenario, ncol = 2, as.table = F) +
      ggtitle( "Total CO2 Emissions by sector" ) +
      xlab("Year") +
      ylab("GtCO2") +
      figure_theme
    #ggsave("figures/paper_figures/1.2.CO2_emissions_sector_USA_nobio2.png", dpi=600/2, width=6000/300, height=3000/300)
    #ggsave("figures/paper_figures/1.2.CO2_emissions_sector_USA_nobio2.svg", dpi=600/2, width=6000/300, height=3000/300)
    
#### 1.3. PM2.5 Emissions ####
    air_pollution_sources <- getQuery(prj, "air pollution nonCO2 emissions by tech")
    air_pollution_sources_electricity <- getQuery(prj, "air pollution nonCO2 emissions by elec tech (USA)")
    air_pollution_resoruce_prod <- getQuery(prj, "nonCO2 emissions by resource production")
    
    #### 1.3.1. Energy sources ####
    air_pollution_sources %>%
      bind_rows(air_pollution_sources_electricity) -> air_pollution_source_all
    
    air_pollution_source_all %>%
      filter(year > 2010 & year < 2055) %>%
      left_join(air_pollution_sector_mapping, by = "sector") %>%
      left_join(scenario_mapping, by = "scenario") %>%
      filter(ghg == "PM2.5") %>%
      filter(!region %in% c("AK", "HI")) -> air_pollution_source_PM2.5
    
    #Here we want to split electricity and electricity BECCS
    air_pollution_source_PM2.5 %>%
      filter(Sector == "Electricity") %>%
      filter(grepl("CCS", technology)) %>%
      mutate(Sector = "Electricity BECCS") %>%
      group_by(Units, Scenario, ghg, year, Sector) %>%
      summarise(total_USA = sum(value)) %>%
      ungroup()-> electricity_BECCS_PM2.5
    
    air_pollution_source_PM2.5 %>%
      filter(Sector == "Electricity") %>%
      filter(!grepl("CCS", technology)) %>%
      group_by(Units, Scenario, ghg, year, Sector) %>%
      summarise(total_USA = sum(value)) %>%
      ungroup() -> electricitynoBECCS_PM2.5
      
    air_pollution_source_PM2.5 %>%
      filter(Sector != "Electricity") %>%
      group_by(Units, Scenario, ghg, year, Sector) %>%
      summarise(total_USA = sum(value)) %>%
      ungroup() %>%
      bind_rows(electricity_BECCS_PM2.5, electricitynoBECCS_PM2.5) -> air_pollution_sources_complete_PM2.5
    
    #### 1.3.2. Resource production ####
    air_pollution_resoruce_prod %>%
      filter(year > 2010 & year < 2055,
             ghg == "PM2.5") %>%
      left_join(scenario_mapping, by = "scenario") %>%
      group_by(Units, Scenario, ghg, year) %>%
      summarise(total_USA = sum(value)) %>%
      ungroup() %>%
      mutate(Sector = "Other energy processing")-> air_pollution_resoruce_prod_PM2.5
    
    #### 1.3.1. All scenarios ####
    air_pollution_sources_complete_PM2.5 %>%
      bind_rows(air_pollution_resoruce_prod_PM2.5) %>%
      mutate(Sector = factor(Sector, levels = PM_sector_order)) %>%
      filter(year != 2015)-> air_pollution_2.5_final 
    
    air_pollution_2.5_final %>%
      group_by(Scenario, year) %>%
      summarise(sum = sum(total_USA)) -> total
    
    air_pollution_2.5_final %>%
      filter(year == 2050) %>%
      left_join(total, by = c("Scenario", "year")) %>%
      mutate(percentage = round((total_USA / sum)*100, digits = 0),
             total_USA = round(total_USA, digits=2))-> percentage
    
    p <- ggplot() + geom_area(data=air_pollution_2.5_final, aes(x=year, y=total_USA, fill= Sector)) +
      scale_fill_manual(values=pollutant_sector_color_2) +
      scale_x_continuous(breaks=seq(2010,2100,10)) +
      facet_wrap( ~ Scenario, ncol = 2, as.table = F) +
      ggtitle( "Total PM2.5 Emissions by sector" ) +
      xlab("Year") +
      ylab("Tg") +
      figure_theme
    #ggsave("figures/paper_figures/1.3.PM2.5_emissions_sector_USA2.png", dpi=600/2, width=6000/300, height=3000/300)
    #ggsave("figures/paper_figures/1.3.PM2.5_emissions_sector_USA2.svg", dpi=600/2, width=6000/300, height=3000/300)
    

#--------------------------------------------------------------- FIGURE 2 -------------------------------------------------------------------
#Figure 2 is on 9 km results: three scenarios PM2.5 from WRF-CMAQ and mortality from BenMAP
#### 2.0 Prepare data ####
    #Load partitions to get pixel_ID column
    pixel_ID_table <- read.csv("output_data/BenMAP_files/9km/Date_ 2024-03-13 _9km_partitions_2019.csv")  %>% select(row, col, pixel_ID) %>% unique() #98,276 total pixels
    
    #Get pixels to keep (those that have)
    input_pollution_pixels <- read.csv("output_data/BenMAP_files/9km_V2/Date_ 2024-05-24 OutputTable_PM25_2050_REF.csv") %>%
      select(Row, Column) %>% #97,433 total pixels with pollution from Jing files
      left_join(pixel_ID_table, by = c("Row" = "row", "Column" = "col"))
    
    pixels_to_keep <- input_pollution_pixels$pixel_ID
    
    input_pollution_pixels %>% select(pixel_ID, Row, Column) -> pixel_ID
    
    #Prepare useful tables 
    partitions <- read.csv("output_data/BenMAP_files/9km/Date_ 2024-03-13 _9km_partitions_2019.csv")
    
    partitions %>%
      mutate(pixel_area = round(pixel_area)) %>%
      select(pixel_ID, row, col, pixel_area) %>%
      unique() %>%
      filter(pixel_ID %in% pixels_to_keep)-> pixel_area #97,433 instances, in m^2
    
    #Get population per pixel
    population_2019_data_pixel_age_final<- read.csv("output_data/BenMAP_files/9km/Date_ 2024-03-01 9_km_grid_NAD83_filtered_population_2019.csv")
    
    population_2019_data_pixel_age_final %>% 
      group_by(Row, Column) %>%
      summarise(Population = sum(Population)) ->population_2019_data_pixel_clean 

    #Get state pixels
    partitions %>%
      st_drop_geometry() %>%
      dplyr::select(pixel_ID, row, col, state_ID) %>%
      unique() %>%
      left_join(state_region_mapping %>% mutate(state_ID = as.numeric(state_ID)), by = "state_ID")-> pixels_by_state
    
    #Get geometry
    #Here we load BenMAP shapefiles with mortality results to create plots      
    geomtery_polygons <- st_read("output_data/shapefiles/9km/9_km_grid_NAD83_filtered_2019.shp") %>% rename(Row = row, Column = col) %>% filter(pixel_ID %in% pixels_to_keep)
    
#### 2.1. 2050 Reference Pollution ####
    grid_data_PM25_NAD_REF_2050_filtered_final <- st_read("BenMAP_results/9km_V2/2050_REF/air_quality_2050_ref_9km/Air Quality-PM2.5-Delta_20240526.shp") %>% 
      rename(Row = ROW, Column = COL, Values = D24HourMean) %>% select(-QuarterlyMe)
    
    #Get national mean (population weighted)
    grid_data_PM25_NAD_REF_2050_filtered_final %>%
      left_join(population_2019_data_pixel_clean, by = c("Row", "Column")) %>%
      left_join(pixel_ID, by = c("Row", "Column")) %>%
      filter(pixel_ID %in% pixels_to_keep)-> grid_data_PM25_NAD_REF_2050_filtered_final_pop
    
    #Population weighted PM2.5
    mean_2050_REF_popw <- with(grid_data_PM25_NAD_REF_2050_filtered_final_pop, {
      product <- Values * Population
      sum_products <- sum(product)
      total_population <- sum(Population)
      sum_products / total_population
    })
    
    #Non-population weighted mean
    mean_REF_2050 <- mean(grid_data_PM25_NAD_REF_2050_filtered_final$Values)
    
    #Set values > 9 to 9 for graphing purposes
    #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
    grid_data_PM25_NAD_REF_2050_filtered_final_pop %>%
      filter(!is.na(Values)) %>% #We use 10 for graphing purposes
      mutate(PM25_AVG_graph = if_else(Values >= 10,10, Values)) -> grid_data_PM25_NAD_REF_2050_filtered_graph
    
    #Count pixels with 9 or higher ug/m3 concentrations
    highpol_pixels_REF_2050 <- sum(grid_data_PM25_NAD_REF_2050_filtered_graph$PM25_AVG_graph >= 9)
    highpol_pixels_REF_2050_list <- grid_data_PM25_NAD_REF_2050_filtered_graph$pixel_ID[grid_data_PM25_NAD_REF_2050_filtered_graph$PM25_AVG_graph > 9]
    highpol_pixels_REF_2050_list_WHO <- grid_data_PM25_NAD_REF_2050_filtered_graph$pixel_ID[grid_data_PM25_NAD_REF_2050_filtered_graph$PM25_AVG_graph > 5]
    
    partitions %>%
      st_drop_geometry() %>%
      filter(pixel_ID %in% highpol_pixels_REF_2050_list) %>%
      dplyr::select(pixel_ID, pixel_area) %>%
      unique() %>%
      summarise(total_area = sum(pixel_area)) %>%
      #convert area from m2 to km2
      mutate(total_area_km2 = total_area / 10^6)-> area_above9_REF_2050
    
    partitions %>%
      st_drop_geometry() %>%
      filter(pixel_ID %in% highpol_pixels_REF_2050_list_WHO) %>%
      dplyr::select(pixel_ID, pixel_area) %>%
      unique() %>%
      summarise(total_area = sum(pixel_area)) %>%
      #convert area from m2 to km2
      mutate(total_area_km2 = total_area / 10^6)-> area_above5_REF_2050_WHO
    
    #Perform intersection to crop pixels within the boundary
    usa_boundary_transformed <- st_transform(usa_boundary, st_crs(grid_data_PM25_NAD_REF_2050_filtered_graph))
    grid_data_PM25_NAD_REF_2050_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_REF_2050_filtered_graph, usa_boundary_transformed)
    
    # grid_data_PM25_NAD_REF_2050_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_REF_2050_filtered_graph, usa_boundary)
    colors <- rev(brewer.pal(n = 11, name = "RdYlBu"))
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_NAD_REF_2050_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(
        colors = colors,
        values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
        limits = c(0, 10),  # Adjust limits to match your data range
        name = "PM2.5 (ug/m3)"
      ) +
      ggtitle("2050 REF US Mean 9km Yearly PM2.5 (ug/m3)")+
      geom_text(aes(label = paste("Population weighted mean:", round(mean_2050_REF_popw, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
      #annotation_scale() +
      theme_void() +
      geom_sf(data = usa_boundary, color = "black", fill = NA)
    #ggsave("figures/paper_figures/2.1.2050_REF_PM25_blue.png", dpi=600/2, width=6000/300, height=3000/300)
    #ggsave("figures/paper_figures/2.1.2050_REF_PM25_blue.svg", dpi=600/2, width=6000/300, height=3000/300)
    print(p)
    
#### 2.2. 2050 HIGH-CDR Pollution ####
    grid_data_PM25_NAD_HIGH_2050_filtered_final <- st_read("BenMAP_results/9km_V2/2050_HIGH/air_quality_2050_high_9km/Air Quality-PM2.5-Delta_20240526.shp") %>% 
      rename(Row = ROW, Column = COL, Values = D24HourMean) %>% select(-QuarterlyMe)
    
    #Get national mean (population weighted)
    grid_data_PM25_NAD_HIGH_2050_filtered_final %>%
      left_join(population_2019_data_pixel_clean, by = c("Row", "Column")) %>%
      left_join(pixel_ID, by = c("Row", "Column")) %>%
      filter(pixel_ID %in% pixels_to_keep)-> grid_data_PM25_NAD_HIGH_2050_filtered_final_pop
    
    #Populationw eighted PM2.5
    mean_2050_HIGH_popw <- with(grid_data_PM25_NAD_HIGH_2050_filtered_final_pop, {
      product <- Values * Population
      sum_products <- sum(product)
      total_population <- sum(Population)
      sum_products / total_population
    })
    
    #Non-population weighted mean
    mean_HIGH_2050 <- mean(grid_data_PM25_NAD_HIGH_2050_filtered_final$Values)
    
    #Set values > 9 to 9 for graphing purposes
    #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
    grid_data_PM25_NAD_HIGH_2050_filtered_final_pop %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_NAD_HIGH_2050_filtered_graph
    
    #Count pixels with 9 or higher ug/m3 concentrations
    highpol_pixels_HIGH_2050 <- sum(grid_data_PM25_NAD_HIGH_2050_filtered_graph$PM25_AVG_graph >= 9)
    highpol_pixels_HIGH_2050_list <- grid_data_PM25_NAD_HIGH_2050_filtered_graph$pixel_ID[grid_data_PM25_NAD_HIGH_2050_filtered_graph$PM25_AVG_graph > 9]
    highpol_pixels_HIGH_2050_list_WHO <- grid_data_PM25_NAD_HIGH_2050_filtered_graph$pixel_ID[grid_data_PM25_NAD_HIGH_2050_filtered_graph$PM25_AVG_graph > 5]
    
    partitions %>%
      st_drop_geometry() %>%
      filter(pixel_ID %in% highpol_pixels_HIGH_2050_list) %>%
      dplyr::select(pixel_ID, pixel_area) %>%
      unique() %>%
      summarise(total_area = sum(pixel_area)) %>%
      #convert area from m2 to km2
      mutate(total_area_km2 = total_area / 10^6)-> area_above9_HIGH_2050

    partitions %>%
      st_drop_geometry() %>%
      filter(pixel_ID %in% highpol_pixels_HIGH_2050_list_WHO) %>%
      dplyr::select(pixel_ID, pixel_area) %>%
      unique() %>%
      summarise(total_area = sum(pixel_area)) %>%
      #convert area from m2 to km2
      mutate(total_area_km2 = total_area / 10^6)-> area_above9_HIGH_2050_WHO
    
    #Perform intersection to crop pixels within the boundary
    usa_boundary_transformed <- st_transform(usa_boundary, st_crs(grid_data_PM25_NAD_HIGH_2050_filtered_graph))
    grid_data_PM25_NAD_HIGH_2050_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_HIGH_2050_filtered_graph, usa_boundary_transformed)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_NAD_HIGH_2050_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      #scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 9)) +  # Using the magma color scale
      scale_fill_gradientn(
        colors = colors,
        values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
        limits = c(0, 10),  # Adjust limits to match your data range
        name = "PM2.5 (ug/m3)"
      ) +
      ggtitle("2050 HIGH US Mean 9km Yearly PM2.5 (ug/m3)")+
      geom_text(aes(label = paste("Population weighted mean:", round(mean_2050_HIGH_popw, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
      #annotation_scale() +
      theme_void() +
      geom_sf(data = usa_boundary, color = "black", fill = NA)
    ggsave("figures/paper_figures/2.2.2050_HIGH_PM25_blue.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/2.2.2050_HIGH_PM25_blue.svg", dpi=600/2, width=6000/300, height=3000/300)
    print(p)
    
#### 2.3. 2050 LOW-CDR Pollution ####
    grid_data_PM25_NAD_LOW_2050_filtered_final <- st_read("BenMAP_results/9km_V2/2050_LOW/air_quality_2050_low_9km/Air Quality-PM2.5-Delta_20240526.shp") %>% 
      rename(Row = ROW, Column = COL, Values = D24HourMean) %>% select(-QuarterlyMe)
    
    #Get national mean (population weighted)
    grid_data_PM25_NAD_LOW_2050_filtered_final %>%
      left_join(population_2019_data_pixel_clean, by = c("Row", "Column")) %>%
      left_join(pixel_ID, by = c("Row", "Column")) %>%
      filter(pixel_ID %in% pixels_to_keep)-> grid_data_PM25_NAD_LOW_2050_filtered_final_pop
    
    #Populationw eighted PM2.5
    mean_2050_LOW_popw <- with(grid_data_PM25_NAD_LOW_2050_filtered_final_pop, {
      product <- Values * Population
      sum_products <- sum(product)
      total_population <- sum(Population)
      sum_products / total_population
    })
    
    #Non-population weighted mean
    mean_LOW_2050 <- mean(grid_data_PM25_NAD_LOW_2050_filtered_final$Values)
    
    #Set values > 9 to 9 for graphing purposes
    #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
    grid_data_PM25_NAD_LOW_2050_filtered_final_pop %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_NAD_LOW_2050_filtered_graph
    
    #Count pixels with 9 or higher ug/m3 concentrations
    LOWpol_pixels_LOW_2050 <- sum(grid_data_PM25_NAD_LOW_2050_filtered_graph$PM25_AVG_graph >= 9)
    LOWpol_pixels_LOW_2050_list <- grid_data_PM25_NAD_LOW_2050_filtered_graph$pixel_ID[grid_data_PM25_NAD_LOW_2050_filtered_graph$PM25_AVG_graph > 9]
    LOWpol_pixels_LOW_2050_list_WHO <- grid_data_PM25_NAD_LOW_2050_filtered_graph$pixel_ID[grid_data_PM25_NAD_LOW_2050_filtered_graph$PM25_AVG_graph > 5]
    
    partitions %>%
      st_drop_geometry() %>%
      filter(pixel_ID %in% LOWpol_pixels_LOW_2050_list) %>%
      dplyr::select(pixel_ID, pixel_area) %>%
      unique() %>%
      summarise(total_area = sum(pixel_area)) %>%
      #convert area from m2 to km2
      mutate(total_area_km2 = total_area / 10^6)-> area_above9_LOW_2050
    
    partitions %>%
      st_drop_geometry() %>%
      filter(pixel_ID %in% LOWpol_pixels_LOW_2050_list_WHO) %>%
      dplyr::select(pixel_ID, pixel_area) %>%
      unique() %>%
      summarise(total_area = sum(pixel_area)) %>%
      #convert area from m2 to km2
      mutate(total_area_km2 = total_area / 10^6)-> area_above9_LOW_2050_WHO
    
    #Perform intersection to crop pixels within the boundary
    usa_boundary_transformed <- st_transform(usa_boundary, st_crs(grid_data_PM25_NAD_LOW_2050_filtered_graph))
    grid_data_PM25_NAD_LOW_2050_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_LOW_2050_filtered_graph, usa_boundary_transformed)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_NAD_LOW_2050_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      # scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 9)) +  # Using the magma color scale
      scale_fill_gradientn(
        colors = colors,
        values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
        limits = c(0, 10),  # Adjust limits to match your data range
        name = "PM2.5 (ug/m3)"
      ) +
      ggtitle("2050 LOW US Mean 9km Yearly PM2.5 (ug/m3)")+
      geom_text(aes(label = paste("Population weighted mean:", round(mean_2050_LOW_popw, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
      #annotation_scale() +
      theme_void() +
      geom_sf(data = usa_boundary, color = "black", fill = NA)
    ggsave("figures/paper_figures/2.3.2050_LOW_PM25_blue.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/2.3.2050_LOW_PM25_blue.svg", dpi=600/2, width=6000/300, height=3000/300)
    print(p)
  
#### 2.4. 2050 Reference Mortality####  
    #Here we load BenMAP shapefiles with mortality results to create plots      
    benmap_2050_REF_9km <- st_read("BenMAP_results/9km_V2/2050_REF/health_impacts_2050_ref_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()
    
    #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
    input_pollution_pixels %>%
      dplyr::select(Row, Column)%>%
      left_join(benmap_2050_REF_9km, by = c("Row" = "ROW", "Column" = "COL")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      left_join(pixel_area, by = c("Row" = "row", "Column" = "col"))-> benmap_2050_REF_9km_complete
    
    #Prepare data 
    benmap_2050_REF_9km_complete %>% 
      mutate(deaths_per_km = Point.Estim / (pixel_area / 10^6),
             deaths_per_km_graph = if_else(deaths_per_km >= 1, 1, deaths_per_km)) %>%
      mutate(deaths_per_million = if_else(Population == 0, 0, ((Point.Estim / Population)*10^6)),
             deaths_per_million_graph = if_else(deaths_per_million >= 2000, 2000, deaths_per_million))-> benmap_2050_REF_9km_complete_death_per_km
    
    geomtery_polygons %>%
    left_join(benmap_2050_REF_9km_complete_death_per_km, by = c("pixel_ID", "Row", "Column"))->benmap_2050_REF_9km_complete_death_per_km_geometry
    
    #Calculate total
    benmap_2050_REF_9km %>%
      dplyr::summarise(mean_deaths = sum(Point.Estim), #The point estimate is the mean deaths
                       per2p5_deaths = sum(Percentile), #This is percentile 2.5
                       per97p5_deaths = sum(Percentile.19),#This is percentile 97.5
                       total_population = sum(Population)) %>% 
      mutate(Scenario = "2050 Reference 9km")-> total_mortality_2050_REF_9km
    
    #Get state values
    benmap_2050_REF_9km_complete_death_per_km %>%
      st_drop_geometry() %>%
      left_join(pixels_by_state, by = c("Row" = "row", "Column" = "col")) %>%
      group_by(state_ID, state_name, Region) %>%
      dplyr::summarise(Population_state = sum(Population),
                       Point.Estim_state = sum(Point.Estim)) %>%
      mutate(deaths_per_million = (Point.Estim_state / Population_state)*10^6)-> total_mortality_2050_REF_9km_state #deaths per million in state
    
    #Figure 
    #Perform intersection to crop pixels within the boundary
    benmap_2050_REF_9km_9km_pop_weighted_crop <- st_intersection(benmap_2050_REF_9km_complete_death_per_km_geometry, usa_boundary)
    
    #Total deaths
    p <- ggplot() +
      geom_sf(data = benmap_2050_REF_9km_9km_pop_weighted_crop, aes(fill = deaths_per_km_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0,1)) +  
      ggtitle("2050 Reference US Mortality due to Mean Yearly PM2.5")+
      geom_text(data = total_mortality_2050_REF_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/2.4.2050_REF_mortality.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/2.4.2050_REF_mortality.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    #Deaths per million
    p <- ggplot() +
      geom_sf(data = benmap_2050_REF_9km_9km_pop_weighted_crop, aes(fill = deaths_per_million_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 2000)) +
      ggtitle("2050 Reference US Mortality due to Mean Yearly PM2.5 (deaths per million)")+
      geom_text(data = total_mortality_2050_REF_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/2.4.2050_REF_mortality_per_million.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/2.4.2050_REF_mortality_per_million.svg", dpi=600/2, width=6000/300, height=3000/300)
  
#### 2.5. 2050 HIGH-CDR  Mortality####  
    #Here we load BenMAP shapefiles with mortality results to create plots      
    benmap_2050_HIGH_9km <- st_read("BenMAP_results/9km_V2/2050_HIGH/health_impact_2050_high_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()
    
    #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
    input_pollution_pixels %>%
      dplyr::select(Row, Column)%>%
      left_join(benmap_2050_HIGH_9km, by = c("Row" = "ROW", "Column" = "COL")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      left_join(pixel_area, by = c("Row" = "row", "Column" = "col"))-> benmap_2050_HIGH_9km_complete
    
    #Prepare data 
    benmap_2050_HIGH_9km_complete %>% 
      mutate(deaths_per_km = Point.Estim / (pixel_area / 10^6),
             deaths_per_km_graph = if_else(deaths_per_km >= 1, 1, deaths_per_km)) %>%
      mutate(deaths_per_million = if_else(Population == 0, 0, ((Point.Estim / Population)*10^6)),
             deaths_per_million_graph = if_else(deaths_per_million >= 2000, 2000, deaths_per_million))-> benmap_2050_HIGH_9km_complete_death_per_km
    
    geomtery_polygons %>%
      left_join(benmap_2050_HIGH_9km_complete_death_per_km, by = c("pixel_ID", "Row", "Column"))->benmap_2050_HIGH_9km_complete_death_per_km_geometry
    
    #Calculate total
    benmap_2050_HIGH_9km %>%
      dplyr::summarise(mean_deaths = sum(Point.Estim), #The point estimate is the mean deaths
                       per2p5_deaths = sum(Percentile), #This is percentile 2.5
                       per97p5_deaths = sum(Percentile.19),#This is percentile 97.5
                       total_population = sum(Population)) %>% 
      mutate(Scenario = "2050 High-CDR 9km")-> total_mortality_2050_HIGH_9km
    
    #Get state values
    benmap_2050_HIGH_9km_complete_death_per_km %>%
      st_drop_geometry() %>%
      left_join(pixels_by_state, by = c("Row" = "row", "Column" = "col")) %>%
      group_by(state_ID, state_name, Region) %>%
      dplyr::summarise(Population_state = sum(Population),
                       Point.Estim_state = sum(Point.Estim)) %>%
      mutate(deaths_per_million = (Point.Estim_state / Population_state)*10^6)-> total_mortality_2050_HIGH_9km_state #deaths per million in state
    
    #Figure 
    #Perform intersection to crop pixels within the boundary
    benmap_2050_HIGH_9km_9km_pop_weighted_crop <- st_intersection(benmap_2050_HIGH_9km_complete_death_per_km_geometry, usa_boundary)
    
    #total deaths
    p <- ggplot() +
      geom_sf(data = benmap_2050_HIGH_9km_9km_pop_weighted_crop, aes(fill = deaths_per_km_graph), color = NA, size = 0) + 
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) +
      ggtitle("2050 HIGH-CDR US Mortality due to Mean Yearly PM2.5")+
      geom_text(data = total_mortality_2050_HIGH_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/2.5.2050_HIGH_mortality.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/2.5.2050_HIGH_mortality.svg", dpi=600/2, width=6000/300, height=3000/300)  
    
    #Deaths per million
    p <- ggplot() +
      geom_sf(data = benmap_2050_HIGH_9km_9km_pop_weighted_crop, aes(fill = deaths_per_million_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 2000)) + # 
      ggtitle("2050 HIGH-CDR US Mortality due to Mean Yearly PM2.5")+
      geom_text(data = total_mortality_2050_HIGH_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/2.5.2050_HIGH_mortality_per_million.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/2.5.2050_HIGH_mortality_per_million.svg", dpi=600/2, width=6000/300, height=3000/300)  
    
#### 2.6. 2050 LOW-CDR  Mortality####  
    #Here we load BenMAP shapefiles with mortality results to create plots      
    benmap_2050_LOW_9km <- st_read("BenMAP_results/9km_V2/2050_LOW/health_impacts_2050_low_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()
    
    #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
    input_pollution_pixels %>%
      dplyr::select(Row, Column)%>%
      left_join(benmap_2050_LOW_9km, by = c("Row" = "ROW", "Column" = "COL")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      left_join(pixel_area, by = c("Row" = "row", "Column" = "col"))-> benmap_2050_LOW_9km_complete
    
    #Prepare data 
    benmap_2050_LOW_9km_complete %>% 
      mutate(deaths_per_km = Point.Estim / (pixel_area / 10^6),
             deaths_per_km_graph = if_else(deaths_per_km >= 1, 1, deaths_per_km)) %>%
      mutate(deaths_per_million = if_else(Population == 0, 0, ((Point.Estim / Population)*10^6)),
             deaths_per_million_graph = if_else(deaths_per_million >= 2000, 2000, deaths_per_million))-> benmap_2050_LOW_9km_complete_death_per_km
    
    geomtery_polygons %>%
      left_join(benmap_2050_LOW_9km_complete_death_per_km, by = c("pixel_ID", "Row", "Column"))->benmap_2050_LOW_9km_complete_death_per_km_geometry
    
    #Calculate total
    benmap_2050_LOW_9km %>%
      dplyr::summarise(mean_deaths = sum(Point.Estim), #The point estimate is the mean deaths
                       per2p5_deaths = sum(Percentile), #This is percentile 2.5
                       per97p5_deaths = sum(Percentile.19),#This is percentile 97.5
                       total_population = sum(Population)) %>% 
      mutate(Scenario = "2050 LOW-CDR 9km")-> total_mortality_2050_LOW_9km
    
    #Get state values
    benmap_2050_LOW_9km_complete_death_per_km %>%
      st_drop_geometry() %>%
      left_join(pixels_by_state, by = c("Row" = "row", "Column" = "col")) %>%
      group_by(state_ID, state_name, Region) %>%
      dplyr::summarise(Population_state = sum(Population),
                       Point.Estim_state = sum(Point.Estim)) %>%
      mutate(deaths_per_million = (Point.Estim_state / Population_state)*10^6)-> total_mortality_2050_LOW_9km_state #deaths per million in state
    
    #Figure 
    #Perform intersection to crop pixels within the boundary
    benmap_2050_LOW_9km_9km_pop_weighted_crop <- st_intersection(benmap_2050_LOW_9km_complete_death_per_km_geometry, usa_boundary)
    
    #Total deaths
    p <- ggplot() +
      geom_sf(data = benmap_2050_LOW_9km_9km_pop_weighted_crop, aes(fill = deaths_per_km_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 LOW-CDR US Mortality due to Mean Yearly PM2.5")+
      geom_text(data = total_mortality_2050_LOW_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/2.6.2050_LOW_mortality.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/2.6.2050_LOW_mortality.svg", dpi=600/2, width=6000/300, height=3000/300)  
    
    #Deaths per million
    p <- ggplot() +
      geom_sf(data = benmap_2050_LOW_9km_9km_pop_weighted_crop, aes(fill = deaths_per_million_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 2000)) + 
      ggtitle("2050 LOW-CDR US Mortality due to Mean Yearly PM2.5")+
      geom_text(data = total_mortality_2050_LOW_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/2.6.2050_LOW_mortality_per_million.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/2.6.2050_LOW_mortality_per_million.svg", dpi=600/2, width=6000/300, height=3000/300)  
    
#--------------------------------------------------------------- FIGURE 3 -------------------------------------------------------------------
    #Figure 3 is on 1 km results: mortality in the 15 most populous US cities
#### 3.1. SEATTLE ####
    population_2019_data_pixel_age_final_seattle<-read.csv("output_data/BenMAP_files/1km/Seattle/Date_ 2024-03-31 1_km_population_2019_seattle.csv")
    
    #City shapefile 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US42660")-> cbsa_shapefile_seattle
    
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_Seattle <- st_read("BenMAP_results/1km_V2/Seattle/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")

    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_Seattle <- st_read("BenMAP_results/1km_V2/Seattle/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") 
    
    #Set 811 pixels with no population to 0
    PM_difference_2050_Seattle %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_Seattle %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
     #filter_all(all_vars(!is.na(.))) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_Seattle_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_Seattle_pop_weighted_cropped <- st_transform(benmap_2050_Seattle_pop_weighted, crs = st_crs(cbsa_shapefile_seattle))
    
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_Seattle_pop_weighted_cropped <- st_intersection(benmap_2050_Seattle_pop_weighted_cropped, cbsa_shapefile_seattle)
    
    #Calculate total
    total_deaths_Seattle <- round(sum(benmap_2050_Seattle$Point.Estim))
    total_deaths_Seattle_2p5 <- round(sum(benmap_2050_Seattle$Percentile)) #This is percentile 2.5
    total_deaths_Seattle_97p5 <- round(sum(benmap_2050_Seattle$Percentile.19)) #This is percentile 97.5
    total_population_Seattle <- sum(population_2019_data_pixel_age_final_seattle$Population) #Total city population
  
    #Total deaths
    benmap_2050_Seattle_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Seattle_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_Seattle_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + #
      ggtitle("2050 Seattle Mortality due to Mean Yearly PM2.5 [High - Low]")+
      annotation_scale()+
      geom_sf(data = cbsa_shapefile_seattle, color = "black", fill = NA)+
      theme_void() +
      #theme(plot.background = element _rect(fill = "white")) +
      geom_point(data = NULL, aes(x = -122.32987465320657, y = 47.60386428408079), shape = 1, color = "purple", size = 6, stroke = 2) #Seattle city hall+
    #ggsave("figures/paper_figures/3.1.1km_PM25_2050_Benmap_Seattle_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.1.1km_PM25_2050_Benmap_Seattle_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)

#### 3.2. LOS ANGELES ####
    #Los Angeles
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US31080")-> cbsa_shapefile_LA
    
    population_2019_data_pixel_age_final_LA <- read.csv("output_data/BenMAP_files/1km/LA/Date_ 2024-03-31 1_km_population_2019_LA.csv")

    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_LA <- st_read("BenMAP_results/1km_V2/LA/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
    
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_LA <- st_read("BenMAP_results/1km_V2/LA/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") 
    #Set 302 pixels with no population to 0
    PM_difference_2050_LA %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_LA %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_LA_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_LA_pop_weighted_cropped <- st_transform(benmap_2050_LA_pop_weighted, crs = st_crs(cbsa_shapefile_LA))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_LA_pop_weighted_cropped <- st_intersection(benmap_2050_LA_pop_weighted_cropped, cbsa_shapefile_LA)
    
    #Calculate total
    total_deaths_LA <- round(sum(benmap_2050_LA$Point.Estim))
    total_deaths_LA_2p5 <- round(sum(benmap_2050_LA$Percentile)) #This is percentile 2.5
    total_deaths_LA_97p5 <- round(sum(benmap_2050_LA$Percentile.19)) #This is percentile 97.5
    total_population_LA <- sum(population_2019_data_pixel_age_final_LA$Population) #Total city population
    
    #Total deaths
    benmap_2050_LA_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_LA_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_LA_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 LA Mortality due to Mean Yearly PM2.5 [High - Low]")+
      #annotation_scale() +
      geom_sf(data = cbsa_shapefile_LA, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -118.24271554610264,  y =  34.05368499865087), shape = 1, color = "purple", size = 6, stroke = 2) #LA city hall
    #ggsave("figures/paper_figures/3.2.1km_PM25_2050_Benmap_LA_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.2.1km_PM25_2050_Benmap_LA_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    
#### 3.3. RIVERSIDE ####
    #Riverside
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US40140")-> cbsa_shapefile_riverside
    
    population_2019_data_pixel_age_final_riverside <- read.csv("output_data/BenMAP_files/1km/Riverside/Date_ 2024-03-31 1_km_population_2019_riverside.csv")
    
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_Riverside <- st_read("BenMAP_results/1km_V2/Riverside/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
    
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_Riverside <- st_read("BenMAP_results/1km_V2/Riverside/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") 
    
    #Set 2 pixels with no population to 0
    PM_difference_2050_Riverside %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_Riverside %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_Riverside_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_Riverside_pop_weighted_cropped <- st_transform(benmap_2050_Riverside_pop_weighted, crs = st_crs(cbsa_shapefile_riverside))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_Riverside_pop_weighted_cropped <- st_intersection(benmap_2050_Riverside_pop_weighted_cropped, cbsa_shapefile_riverside)
    
    #Calculate total
    total_deaths_Riverside <- sum(benmap_2050_Riverside$Point.Estim)
    total_deaths_Riverside_2p5 <- round(sum(benmap_2050_Riverside$Percentile)) #This is percentile 2.5
    total_deaths_Riverside_97p5 <- round(sum(benmap_2050_Riverside$Percentile.19)) #This is percentile 97.5
    total_population_Riverside <- sum(population_2019_data_pixel_age_final_riverside$Population) #Total city population
    
    #Total deaths
    benmap_2050_Riverside_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Riverside_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_Riverside_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 Riverside Mortality due to Mean Yearly PM2.5 [High - Low]")+
      #annotation_scale() +  
      geom_sf(data = cbsa_shapefile_riverside, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -117.37559005667082,  y =  33.980696653550375), shape = 1, color = "purple", size = 6, stroke = 2) #Riverside city hall
    #ggsave("figures/paper_figures/3.3.1km_PM25_2050_Benmap_Riverside_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.3.1km_PM25_2050_Benmap_Riverside_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    
#### 3.4. PHOENIX ####
    #Phoenix
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US38060")-> cbsa_shapefile_phoenix
    
    population_2019_data_pixel_age_final_phoenix <- read.csv("output_data/BenMAP_files/1km/Phoenix/Date_ 2024-03-31 1_km_population_2019_phoenix.csv")
    
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_Phoenix <- st_read("BenMAP_results/1km_V2/Phoenix/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
    
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_Phoenix <- st_read("BenMAP_results/1km_V2/Phoenix/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") 
    #Set 2941 pixels with no population to 0
    PM_difference_2050_Phoenix %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_Phoenix %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_Phoenix_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_Phoenix_pop_weighted_cropped <- st_transform(benmap_2050_Phoenix_pop_weighted, crs = st_crs(cbsa_shapefile_phoenix))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_Phoenix_pop_weighted_cropped <- st_intersection(benmap_2050_Phoenix_pop_weighted_cropped, cbsa_shapefile_phoenix)
    
    #Calculate total
    total_deaths_Phoenix <- sum(benmap_2050_Phoenix$Point.Estim)
    total_deaths_Phoenix_2p5 <- round(sum(benmap_2050_Phoenix$Percentile)) #This is percentile 2.5
    total_deaths_Phoenix_97p5 <- round(sum(benmap_2050_Phoenix$Percentile.19)) #This is percentile 97.5
    total_population_Phoenix <- sum(population_2019_data_pixel_age_final_phoenix$Population) #Total city population
    
    #Total deaths
    benmap_2050_Phoenix_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Phoenix_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_Phoenix_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 Phoenix Mortality due to Mean Yearly PM2.5 [High - Low]")+
      #annotation_scale() + 
      geom_sf(data = cbsa_shapefile_phoenix, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -112.07721684407342,  y =  33.448615125541835), shape = 1, color = "purple", size = 6, stroke = 2) #Phoenix city hall
    #ggsave("figures/paper_figures/3.4.1km_PM25_2050_Benmap_Phoenix_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.4.1km_PM25_2050_Benmap_Phoenix_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 3.5. DALLAS ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US19100")-> cbsa_shapefile_dallas
    
    population_2019_data_pixel_age_final_dallas <- read.csv("output_data/BenMAP_files/1km/Dallas/Date_ 2024-03-31 1_km_population_2019_dallas.csv")
    
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_Dallas <- st_read("BenMAP_results/1km_V2/Dallas/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240517.shp")
    
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_Dallas <- st_read("BenMAP_results/1km_V2/Dallas/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") 
    #Set 9 pixels with no population to 0
    PM_difference_2050_Dallas %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_Dallas %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_Dallas_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_Dallas_pop_weighted_cropped <- st_transform(benmap_2050_Dallas_pop_weighted, crs = st_crs(cbsa_shapefile_dallas))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_Dallas_pop_weighted_cropped <- st_intersection(benmap_2050_Dallas_pop_weighted_cropped, cbsa_shapefile_dallas)
    
    #Calculate total
    total_deaths_Dallas <- sum(benmap_2050_Dallas$Point.Estim)
    total_deaths_Dallas_2p5 <- round(sum(benmap_2050_Dallas$Percentile)) #This is percentile 2.5
    total_deaths_Dallas_97p5 <- round(sum(benmap_2050_Dallas$Percentile.19)) #This is percentile 97.5
    total_population_Dallas <- sum(population_2019_data_pixel_age_final_dallas$Population) #Total city population
  
    #Total deaths
    benmap_2050_Dallas_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Dallas_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_Dallas_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 Dallas Mortality due to Mean Yearly PM2.5 [High - Low]")+
      #annotation_scale() +
      geom_sf(data = cbsa_shapefile_dallas, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -96.7968013977644,  y =  32.776295519527835), shape = 1, color = "purple", size = 6, stroke = 2) #Dallas city hall
    #ggsave("figures/paper_figures/3.5.1km_PM25_2050_Benmap_Dallas_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.5.1km_PM25_2050_Benmap_Dallas_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 3.6. HOUSTON ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US26420")-> cbsa_shapefile_houston
    
    population_2019_data_pixel_age_final_houston_fix <- read.csv("output_data/BenMAP_files/1km/Houston/Date_ 2024-04-08 1_km_population_2019_houston_fixed.csv")
    
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_Houston <- st_read("BenMAP_results/1km_V2/Houston/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240517.shp")
    
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_Houston <- st_read("BenMAP_results/1km_V2/Houston/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") 
    #Set 8 pixels with no population to 0
    PM_difference_2050_Houston %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_Houston %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_Houston_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_Houston_pop_weighted_cropped <- st_transform(benmap_2050_Houston_pop_weighted, crs = st_crs(cbsa_shapefile_houston))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_Houston_pop_weighted_cropped <- st_intersection(benmap_2050_Houston_pop_weighted_cropped, cbsa_shapefile_houston)
    
    #Calculate total
    total_deaths_Houston <- sum(benmap_2050_Houston$Point.Estim)
    total_deaths_Houston_2p5 <- round(sum(benmap_2050_Houston$Percentile)) #This is percentile 2.5
    total_deaths_Houston_97p5 <- round(sum(benmap_2050_Houston$Percentile.19)) #This is percentile 97.5
    total_population_Houston <- sum(population_2019_data_pixel_age_final_houston_fix$Population) #Total city population
    
    #Total deaths
    benmap_2050_Houston_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Houston_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_Houston_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 Houston Mortality due to Mean Yearly PM2.5 [High - Low]")+
      #annotation_scale() +
      geom_sf(data = cbsa_shapefile_houston, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -95.36946640065364,  y =  29.760080695254256), shape = 1, color = "purple", size = 6, stroke = 2) #Houston city hall
    #ggsave("figures/paper_figures/3.6.1km_PM25_2050_Benmap_Houston_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.6.1km_PM25_2050_Benmap_Houston_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 3.7. MIAMI ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US33100")-> cbsa_shapefile_miami
    
    population_2019_data_pixel_age_final_miami_fix <- read.csv("output_data/BenMAP_files/1km/Miami/Date_ 2024-04-08 1_km_population_2019_miami_fixed.csv")
    
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_Miami <- st_read("BenMAP_results/1km_V2/Miami/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
  
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_Miami <- st_read("BenMAP_results/1km_V2/Miami/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") 
    #Set 3508 pixels with no population to 0
    PM_difference_2050_Miami %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_Miami %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_Miami_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_Miami_pop_weighted_cropped <- st_transform(benmap_2050_Miami_pop_weighted, crs = st_crs(cbsa_shapefile_miami))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_Miami_pop_weighted_cropped <- st_intersection(benmap_2050_Miami_pop_weighted_cropped, cbsa_shapefile_miami)
    
    #Calculate total
    total_deaths_Miami <- sum(benmap_2050_Miami$Point.Estim)
    total_deaths_Miami_2p5 <- round(sum(benmap_2050_Miami$Percentile)) #This is percentile 2.5
    total_deaths_Miami_97p5 <- round(sum(benmap_2050_Miami$Percentile.19)) #This is percentile 97.5
    total_population_Miami <- sum(population_2019_data_pixel_age_final_miami_fix$Population) #Total city population

    #Total deaths
    benmap_2050_Miami_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Miami_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_Miami_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 Miami Mortality due to Mean Yearly PM2.5 [High - Low]")+
      #annotation_scale() + 
      geom_sf(data = cbsa_shapefile_miami, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -80.23398527668328,  y =  25.72761157460114), shape = 1, color = "purple", size = 6, stroke = 2) #Miami city hall
    #ggsave("figures/paper_figures/3.7.1km_PM25_2050_Benmap_Miami_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.7.1km_PM25_2050_Benmap_Miami_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 3.8. ATLANTA ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US12060")-> cbsa_shapefile_atl
    
    population_2019_data_pixel_age_final_atl <- read.csv("output_data/BenMAP_files/1km/Atlanta/Date_ 2024-03-31 1_km_population_2019_atlanta.csv")
    
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_Atlanta <- st_read("BenMAP_results/1km_V2/Atlanta/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240517.shp")
    
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_Atlanta <- st_read("BenMAP_results/1km_V2/Atlanta/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") 
    
    #Set 9 pixels with no population to 0
    PM_difference_2050_Atlanta %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_Atlanta %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_Atlanta_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_Atlanta_pop_weighted_cropped <- st_transform(benmap_2050_Atlanta_pop_weighted, crs = st_crs(cbsa_shapefile_atl))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_Atlanta_pop_weighted_cropped <- st_intersection(benmap_2050_Atlanta_pop_weighted_cropped, cbsa_shapefile_atl)
    
    #Calculate total
    total_deaths_Atlanta <- sum(benmap_2050_Atlanta$Point.Estim)
    total_deaths_Atlanta_2p5 <- round(sum(benmap_2050_Atlanta$Percentile)) #This is percentile 2.5
    total_deaths_Atlanta_97p5 <- round(sum(benmap_2050_Atlanta$Percentile.19)) #This is percentile 97.5
    total_population_Atlanta <- sum(population_2019_data_pixel_age_final_atl$Population) #Total city population
    
    #Total deaths
    benmap_2050_Atlanta_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Atlanta_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_Atlanta_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 Atlanta Mortality due to Mean Yearly PM2.5 [High - Low]")+
      #annotation_scale() + 
      geom_sf(data = cbsa_shapefile_atl, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -84.39062010249631,  y =  33.74857740874673), shape = 1, color = "purple", size = 6, stroke = 2) #Atlanta city hall
    #ggsave("figures/paper_figures/3.8.1km_PM25_2050_Benmap_Atlanta_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.8.1km_PM25_2050_Benmap_Atlanta_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 3.9. CHICAGO ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US16980")-> cbsa_shapefile_chicago
    
    population_2019_data_pixel_age_final_chicago_fix <- read.csv("output_data/BenMAP_files/1km/Chicago/Date_ 2024-04-08 1_km_population_2019_chicago_fixed.csv")
    
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_Chicago <- st_read("BenMAP_results/1km_V2/Chicago/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240521.shp")
    
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_Chicago <- st_read("BenMAP_results/1km_V2/Chicago/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") 
    #Set 60 pixels with no population to 0
    PM_difference_2050_Chicago %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_Chicago %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_Chicago_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_Chicago_pop_weighted_cropped <- st_transform(benmap_2050_Chicago_pop_weighted, crs = st_crs(cbsa_shapefile_chicago))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_Chicago_pop_weighted_cropped <- st_intersection(benmap_2050_Chicago_pop_weighted_cropped, cbsa_shapefile_chicago)
    
    #Calculate total
    total_deaths_Chicago <- sum(benmap_2050_Chicago$Point.Estim)
    total_deaths_Chicago_2p5 <- round(sum(benmap_2050_Chicago$Percentile)) #This is percentile 2.5
    total_deaths_Chicago_97p5 <- round(sum(benmap_2050_Chicago$Percentile.19)) #This is percentile 97.5
    total_population_Chicago <- sum(population_2019_data_pixel_age_final_chicago_fix$Population) #Total city population
  
    #Total deaths
    benmap_2050_Chicago_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Chicago_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_Chicago_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 Chicago Mortality due to Mean Yearly PM2.5 [High - Low]")+
      #annotation_scale() +
      geom_sf(data = cbsa_shapefile_chicago, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -87.63207808921189,  y =  41.8837839226344), shape = 1, color = "purple", size = 6, stroke = 2) #Chicago city hall
    #ggsave("figures/paper_figures/3.9.1km_PM25_2050_Benmap_Chicago_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.9.1km_PM25_2050_Benmap_Chicago_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 3.10. WASHINGTON DC ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US47900")-> cbsa_shapefile_wash
    
    population_2019_data_pixel_age_final_wash_fix <- read.csv("output_data/BenMAP_files/1km/DC/Date_ 2024-04-08 1_km_population_2019_wash_fixed.csv")
    
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_DC <- st_read("BenMAP_results/1km_V2/DC/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240517.shp")
  
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_DC <- st_read("BenMAP_results/1km_V2/DC/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") 
    #Set 85 pixels with no population to 0
    PM_difference_2050_DC %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_DC %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_DC_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_DC_pop_weighted_cropped <- st_transform(benmap_2050_DC_pop_weighted, crs = st_crs(cbsa_shapefile_wash))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_DC_pop_weighted_cropped <- st_intersection(benmap_2050_DC_pop_weighted_cropped, cbsa_shapefile_wash)
    
    #Calculate total
    total_deaths_DC <- sum(benmap_2050_DC$Point.Estim)
    total_deaths_DC_2p5 <- round(sum(benmap_2050_DC$Percentile)) #This is percentile 2.5
    total_deaths_DC_97p5 <- round(sum(benmap_2050_DC$Percentile.19)) #This is percentile 97.5
    total_population_DC <- sum(population_2019_data_pixel_age_final_wash_fix$Population) #Total city population
    
    #Total deaths
    benmap_2050_DC_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_DC_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_DC_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 DC Mortality due to Mean Yearly PM2.5 [High - Low]")+
      #annotation_scale() +
      geom_sf(data = cbsa_shapefile_wash, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -77.03653714296149,  y =  38.89771345976116), shape = 1, color = "purple", size = 6, stroke = 2) #The White House
    #ggsave("figures/paper_figures/3.10.1km_PM25_2050_Benmap_DC_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.10.1km_PM25_2050_Benmap_DC_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 3.11. PHILADELPHIA ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US37980")-> cbsa_shapefile_phil
    
    population_2019_data_pixel_age_final_phil_fix <- read.csv("output_data/BenMAP_files/1km/Philadelphia/Date_ 2024-04-08 1_km_population_2019_phil_fixed.csv")
    
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_Philadelphia <- st_read("BenMAP_results/1km_V2/Philadelphia/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
    
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_Philadelphia <- st_read("BenMAP_results/1km_V2/Philadelphia/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") 
    #Set 56 pixels with no population to 0
    PM_difference_2050_Philadelphia %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_Philadelphia %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_Philadelphia_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_Philadelphia_pop_weighted_cropped <- st_transform(benmap_2050_Philadelphia_pop_weighted, crs = st_crs(cbsa_shapefile_phil))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_Philadelphia_pop_weighted_cropped <- st_intersection(benmap_2050_Philadelphia_pop_weighted_cropped, cbsa_shapefile_phil)
    
    #Calculate total
    total_deaths_Philadelphia <- sum(benmap_2050_Philadelphia$Point.Estim)
    total_deaths_Philadelphia_2p5 <- round(sum(benmap_2050_Philadelphia$Percentile)) #This is percentile 2.5
    total_deaths_Philadenlphia_97p5 <- round(sum(benmap_2050_Philadelphia$Percentile.19)) #This is percentile 97.5
    total_population_Philadelphia <- sum(population_2019_data_pixel_age_final_phil_fix$Population) #Total city population

    #Total deaths
    benmap_2050_Philadelphia_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Philadelphia_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_Philadelphia_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 Philadelphia Mortality due to Mean Yearly PM2.5 [High - Low]")+
      #annotation_scale() + 
      geom_sf(data = cbsa_shapefile_phil, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -75.16326872597556,  y =  39.95279997140329), shape = 1, color = "purple", size = 6, stroke = 2) #Philadelphia city hall
    #ggsave("figures/paper_figures/3.11.1km_PM25_2050_Benmap_Philadelphia_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.11.1km_PM25_2050_Benmap_Philadelphia_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 3.12. NEW YORK ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US35620")-> cbsa_shapefile_NY
    
    population_2019_data_pixel_age_final_NY_fix<-read.csv("output_data/BenMAP_files/1km/NY/Date_ 2024-04-08 1_km_population_2019_NY_fixed.csv")
    
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_NY <- st_read("BenMAP_results/1km_V2/NY/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
    
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_NY <- st_read("BenMAP_results/1km_V2/NY/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") 
    #Set 56 pixels with no population to 0
    PM_difference_2050_NY %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_NY %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_NY_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_NY_pop_weighted_cropped <- st_transform(benmap_2050_NY_pop_weighted, crs = st_crs(cbsa_shapefile_NY))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_NY_pop_weighted_cropped <- st_intersection(benmap_2050_NY_pop_weighted_cropped, cbsa_shapefile_NY)
    
    #Calculate total
    total_deaths_NY <- sum(benmap_2050_NY$Point.Estim)
    total_deaths_NY_2p5 <- round(sum(benmap_2050_NY$Percentile)) #This is percentile 2.5
    total_deaths_NY_97p5 <- round(sum(benmap_2050_NY$Percentile.19)) #This is percentile 97.5
    total_population_NY <- sum(population_2019_data_pixel_age_final_NY_fix$Population)#Total city population
  
    #Total deaths
    benmap_2050_NY_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_NY_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_NY_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 NY Mortality due to Mean Yearly PM2.5 [High - Low]")+
      #annotation_scale() +
      geom_sf(data = cbsa_shapefile_NY, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -74.00583906157757,  y =  40.71266458461421), shape = 1, color = "purple", size = 6, stroke = 2) #New York city hall
    #ggsave("figures/paper_figures/3.12.1km_PM25_2050_Benmap_NY_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.12.1km_PM25_2050_Benmap_NY_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 3.13. BOSTON ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US14460")-> cbsa_shapefile_boston
    
    population_2019_data_pixel_age_final_boston_fix<-read.csv("output_data/BenMAP_files/1km/Boston/Date_ 2024-04-08 1_km_population_2019_boston_fixed.csv")
    
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_Boston <- st_read("BenMAP_results/1km_V2/Boston/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240517.shp")
    
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_Boston <- st_read("BenMAP_results/1km_V2/Boston/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") 
    #Set 4 pixels with no population to 0
    PM_difference_2050_Boston %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_Boston %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_Boston_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_Boston_pop_weighted_cropped <- st_transform(benmap_2050_Boston_pop_weighted, crs = st_crs(cbsa_shapefile_boston))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_Boston_pop_weighted_cropped <- st_intersection(benmap_2050_Boston_pop_weighted_cropped, cbsa_shapefile_boston)
    
    #Calculate total
    total_deaths_Boston <- sum(benmap_2050_Boston$Point.Estim)
    total_deaths_Boston_2p5 <- round(sum(benmap_2050_Boston$Percentile)) #This is percentile 2.5
    total_deaths_Boston_97p5 <- round(sum(benmap_2050_Boston$Percentile.19)) #This is percentile 97.5
    total_population_Boston <- sum(population_2019_data_pixel_age_final_boston_fix$Population) #Total city population
    
    #Total deaths
    benmap_2050_Boston_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Boston_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_Boston_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 Boston Mortality due to Mean Yearly PM2.5 [High - Low]")+
      #annotation_scale() + 
      geom_sf(data = cbsa_shapefile_boston, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -71.05789998431737,  y =  42.360362270351416), shape = 1, color = "purple", size = 6, stroke = 2) #Boston city hall
    #ggsave("figures/paper_figures/3.13.1km_PM25_2050_Benmap_Boston_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.13.1km_PM25_2050_Benmap_Boston_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 3.14. DETROIT ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US19820")-> cbsa_shapefile_detroit
    
    population_2019_data_pixel_age_final_detroit_fix <-read.csv("output_data/BenMAP_files/1km/Detroit/Date_ 2024-04-08 1_km_population_2019_detroit_fixed.csv")
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_Detroit <- st_read("BenMAP_results/1km_V2/Detroit/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240517.shp")
    
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_Detroit <- st_read("BenMAP_results/1km_V2/Detroit/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") 
    #Set 18 pixels with no population to 0
    PM_difference_2050_Detroit %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_Detroit %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_Detroit_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_Detroit_pop_weighted_cropped <- st_transform(benmap_2050_Detroit_pop_weighted, crs = st_crs(cbsa_shapefile_detroit))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_Detroit_pop_weighted_cropped <- st_intersection(benmap_2050_Detroit_pop_weighted_cropped, cbsa_shapefile_detroit)
    
    #Calculate total
    total_deaths_Detroit <- sum(benmap_2050_Detroit$Point.Estim)
    total_deaths_Detroit_2p5 <- round(sum(benmap_2050_Detroit$Percentile)) #This is percentile 2.5
    total_deaths_Detroit_97p5 <- round(sum(benmap_2050_Detroit$Percentile.19)) #This is percentile 97.5
    total_population_Detroit <- sum(population_2019_data_pixel_age_final_detroit_fix$Population) #Total city population
    
    #Total deaths
    benmap_2050_Detroit_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Detroit_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_Detroit_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 Detroit Mortality due to Mean Yearly PM2.5  [High - Low]")+
      geom_sf(data = cbsa_shapefile_detroit, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -83.04362102880836,  y =  42.32974049721709), shape = 1, color = "purple", size = 6, stroke = 2) #Detroit city hall
    #ggsave("figures/paper_figures/3.14.1km_PM25_2050_Benmap_Detroit_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.14.1km_PM25_2050_Benmap_Detroit_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 3.15. SAN FRANCISCO ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US41860")-> cbsa_shapefile_SF
    
    population_2019_data_pixel_age_final_SF_fix <- read.csv("output_data/BenMAP_files/1km/SF/Date_ 2024-04-08 1_km_population_2019_SF_fixed.csv")
    
    #PM2.5 difference: High CDR - Low CDR
    PM_difference_2050_SF <- st_read("BenMAP_results/1km_V2/SF/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
    
    #Here we load BenMAP shapefiles with mortality results to create plots 
    benmap_2050_SF <- st_read("BenMAP_results/1km_V2/SF/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") 
    #Set 25 pixels with no population to 0
    PM_difference_2050_SF %>%
      dplyr::select(ROW, COL, geometry) %>%
      left_join(benmap_2050_SF %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
      mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
      mutate(Population = if_else(is.na(Population), 0, Population)) %>%
      mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*10^6), #This is the population in each pixel from 18-99 years, which is where the health impacts are applied
             deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_SF_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
    # Ensure both datasets have the same CRS
    benmap_2050_SF_pop_weighted_cropped <- st_transform(benmap_2050_SF_pop_weighted, crs = st_crs(cbsa_shapefile_SF))
    # Perform intersection to retain only pixels within the city boundary
    benmap_2050_SF_pop_weighted_cropped <- st_intersection(benmap_2050_SF_pop_weighted_cropped, cbsa_shapefile_SF)
    
    #Calculate total
    total_deaths_SF <- sum(benmap_2050_SF$Point.Estim)
    total_deaths_SF_2p5 <- round(sum(benmap_2050_SF$Percentile)) #This is percentile 2.5
    total_deaths_SF_97p5 <- round(sum(benmap_2050_SF$Percentile.19)) #This is percentile 97.5
    total_population_SF <- sum(population_2019_data_pixel_age_final_SF_fix$Population) #Total city population
    
    #Total deaths
    benmap_2050_SF_pop_weighted_cropped %>%
      mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_SF_pop_weighted_cropped_graph
    
    p <- ggplot() +
      geom_sf(data = benmap_2050_SF_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
      ggtitle("2050 SF Mortality due to Mean Yearly PM2.5  [High - Low]")+
      #annotation_scale() + 
      geom_sf(data = cbsa_shapefile_SF, color = "black", fill = NA)+
      theme_void() +
      geom_point(data = NULL, aes(x = -122.41894505158999,  y =  37.779200252025916), shape = 1, color = "purple", size = 6, stroke = 2) #San Francisco city hall
    #ggsave("figures/paper_figures/3.15.1km_PM25_2050_Benmap_SF_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/3.15.1km_PM25_2050_Benmap_SF_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#--------------------------------------------------------------- FIGURE 4 -------------------------------------------------------------------
#### FUNCTIONS ####
    #1. Weighted average pollution
    weighted_average<- function(data, impact_var, population_var) {
      # Calculate weighted average pollution
      sum_products <- sum(data[[impact_var]] * data[[population_var]], na.rm = TRUE)
      total_population <- sum(data[[population_var]], na.rm = TRUE)
      weighted_avg_impact <- sum_products / total_population
      return(round(weighted_avg_impact,2))
    }
    
    # 2. Weighted mean for bins based on income
    calculate_weighted_average_bins_income <- function(city_name, city_data) {
      # Calculate the 33rd and 66th percentiles
      percentiles <- quantile(city_data$weighted_median_income, probs = c(1/3, 2/3))
      cutoff_points <- c(0, percentiles, Inf)
      
      percentage_ranges <- list(
        list(name = "0to33percentile", min = cutoff_points[1], max = cutoff_points[2]),
        list(name = "33to66percentile", min = cutoff_points[2] + 1, max = cutoff_points[3]),
        list(name = "66to100percentile", min = cutoff_points[3] + 1, max = Inf)
      )
      
      # Create an empty data frame to store the results
      weighted_average_bins <- data.frame(
        bin = character(),  # Empty character vector
        PM25_2019 = numeric(),  # Empty numeric vector
        PM25_HIGH = numeric(),  # Empty numeric vector
        PM25_LOW = numeric(),  # Empty numeric vector
        PM25_DIFF = numeric(),  # Empty numeric vector
        deaths_2019 = numeric(),  # Empty numeric vector
        deaths_HIGH = numeric(),  # Empty numeric vector
        deaths_LOW = numeric(),  # Empty numeric vector
        deaths_DIFF = numeric(),
        region = character() # Empty character vector
      )
      
      # Loop through the percentage ranges
      for (range in percentage_ranges) {
        # Filter the data for the current range
        filtered_data <- city_data %>%
          filter(weighted_median_income >= range$min & weighted_median_income <= range$max)
        
        # Calculate the weighted average PM2.5 pollution for the current range
        result_PM_2019 <- weighted_average(filtered_data, "PM25_2019", "Population")
        result_PM_HIGH <- weighted_average(filtered_data, "PM25_HIGH", "Population")
        result_PM_LOW <- weighted_average(filtered_data, "PM25_LOW", "Population")
        result_PM_DIFF <- weighted_average(filtered_data, "PM25_DIFF", "Population")
        
        # Calculate the weighted average PM2.5 pollution for the current range
        result_death_2019 <- weighted_average(filtered_data, "deaths_2019", "Population")
        result_death_HIGH <- weighted_average(filtered_data, "deaths_HIGH", "Population")
        result_death_LOW <- weighted_average(filtered_data, "deaths_LOW", "Population")
        result_death_DIFF <- weighted_average(filtered_data, "deaths_DIFF", "Population")
        
        # Add the results to the data frame
        weighted_average_bins <- rbind(weighted_average_bins, 
                                       data.frame(bin = range$name,
                                                  PM25_2019 = result_PM_2019,
                                                  PM25_HIGH = result_PM_HIGH,
                                                  PM25_LOW = result_PM_LOW,
                                                  PM25_DIFF = result_PM_DIFF,
                                                  deaths_2019 = result_death_2019,
                                                  deaths_HIGH = result_death_HIGH,
                                                  deaths_LOW = result_death_LOW,
                                                  deaths_DIFF = result_death_DIFF,
                                                  region = city_name))
      }
      
      return(weighted_average_bins)
    }
    
    
    
    #3. Weighted means based on percent white
    calculate_weighted_average_bins_white <- function(city_name, city_data) {
      # Calculate the percentage ranges
      percentage_ranges <- list(
        list(name = "0to30white", min = 0, max = 30),
        list(name = "31to60white", min = 31, max = 60),
        list(name = "61to100white", min = 61, max = 100)
      )
      
      # Create an empty data frame to store the results
      weighted_average_bins_white <- data.frame(
        bin = character(),  # Empty character vector
        PM25_2019 = numeric(),  # Empty numeric vector
        PM25_HIGH = numeric(),  # Empty numeric vector
        PM25_LOW = numeric(),  # Empty numeric vector
        PM25_DIFF = numeric(),  # Empty numeric vector
        deaths_2019 = numeric(),  # Empty numeric vector
        deaths_HIGH = numeric(),  # Empty numeric vector
        deaths_LOW = numeric(),  # Empty numeric vector
        deaths_DIFF = numeric(),  # Empty numeric vector
        region = character() # Empty character vector
      )
      
      # Loop through the percentage ranges
      for (range in percentage_ranges) {
        # Filter the data for the current range
        filtered_data <- city_data %>%
          filter(percent_white >= range$min & percent_white <= range$max)
        
        # Calculate the weighted average PM2.5 pollution for the current range
        result_PM_2019 <- weighted_average(filtered_data, "PM25_2019", "Population")
        result_PM_HIGH <- weighted_average(filtered_data, "PM25_HIGH", "Population")
        result_PM_LOW <- weighted_average(filtered_data, "PM25_LOW", "Population")
        result_PM_DIFF <- weighted_average(filtered_data, "PM25_DIFF", "Population")
        
        # Calculate the weighted average PM2.5 pollution for the current range
        result_death_2019 <- weighted_average(filtered_data, "deaths_2019", "Population")
        result_death_HIGH <- weighted_average(filtered_data, "deaths_HIGH", "Population")
        result_death_LOW <- weighted_average(filtered_data, "deaths_LOW", "Population")
        result_death_DIFF <- weighted_average(filtered_data, "deaths_DIFF", "Population")
        
        # Add the results to the data frame
        weighted_average_bins_white <- rbind(weighted_average_bins_white, 
                                             data.frame(bin = range$name,
                                                        PM25_2019 = result_PM_2019,
                                                        PM25_HIGH = result_PM_HIGH,
                                                        PM25_LOW = result_PM_LOW,
                                                        PM25_DIFF = result_PM_DIFF,
                                                        deaths_2019 = result_death_2019,
                                                        deaths_HIGH = result_death_HIGH,
                                                        deaths_LOW = result_death_LOW,
                                                        deaths_DIFF = result_death_DIFF,
                                                        region = city_name))
      }
      
      return(weighted_average_bins_white)
    }
    
    #4. city analysis
    city_analysis <- function(city_name) {
      # Generate the table name dynamically based on the city name
      table_name <- paste0("partitions_", city_name, "_complete")
      
      table <- get(table_name)
      
      # Define mean variables and weight variables
      mean_variables <- c("PM25_2019", "PM25_HIGH", "PM25_LOW", "PM25_DIFF", 
                          "deaths_2019", "deaths_HIGH", "deaths_LOW", "deaths_DIFF")
      weight_variables <- c("Population", "White", "Black", "Hispanic", "Other")
      
      # Prepare an empty data frame to store the results
      results <- setNames(data.frame(matrix(ncol = length(mean_variables), nrow = length(weight_variables))),
                          mean_variables)
      rownames(results) <- weight_variables
      
      # Loop through each combination of mean_variable and weight_variable
      for (mean_var in mean_variables) {
        for (weight_var in weight_variables) {
          # Calculate the weighted average
          weighted_avg <- weighted_average(table, mean_var, weight_var)
          
          # Store the result in the appropriate location
          results[weight_var, mean_var] <- weighted_avg
        }
      }
      
      # Add city_name column
      results$city_name <- city_name
      
      # Create a variable name by concatenating city name and "weighted_analysis"
      result_var_name <- paste0(city_name, "_weighted_analysis")
      
      # Assign the results data frame to a variable in the global environment
      assign(result_var_name, results, envir = .GlobalEnv)
      
      # Return the results table
      return(results)
    }
    
    #5. Exposure
    calculate_exposure_percentages <- function(data, threshold_value) {
      variables <- c("PM25_2019", "PM25_HIGH", "PM25_LOW", "PM25_DIFF")
      groups <- c("Population", "White", "Black", "Other")
      
      results <- matrix(NA, nrow = length(groups), ncol = length(variables), 
                        dimnames = list(groups, variables))
      
      for (i in seq_along(variables)) {
        for (j in seq_along(groups)) {
          variable <- variables[i]
          group <- groups[j]
          
          data_filtered <- dplyr::filter(data, !!rlang::sym(variable) > threshold_value)
          
          total_population <- sum(data[[group]])
          total_exposed <- sum(data_filtered[[group]])
          
          percent_exposed <- (total_exposed / total_population) * 100
          
          results[j, i] <- round(percent_exposed, 1)
        }
      }
      
      return(results)
    }
  
    
    #6. 
    check_population_sum <- function(data) {
      # Calculate the sum of White, Hispanic, Black, and Other columns
      sum_race_columns <- rowSums(data[, c("White", "Hispanic", "Black", "Other")], na.rm = TRUE)
      
      # Round the sums
      rounded_sum_race_columns <- round(sum_race_columns)
      
      # Round the Population column
      rounded_population <- round(data$Population)
      
      # Calculate the allowed margin (.1% of Population)
      margin <- 0.001 * rounded_population
      
      # Check if the difference between sum of race columns and Population is within the margin
      if (any(abs(rounded_sum_race_columns - rounded_population) > margin)) {
        print("Warning: Population downscaling may not have been done correctly!")
      } else {
        print("Population downscaling was done correctly.")
      }
    }
    
  
#### Census  data ####
  ## Load data
    census_block <- st_read("shapefiles/cb_2019_us_bg_500k_clipped/cb_2019_us_bg_500k.shp") #From https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.2019.html#list-tab-1883739534
    
    input_ethnicity_data <- read.csv("data/ACS/ACSDT5Y2019.B03002_2024-04-13T182630/ACSDT5Y2019.B03002-Data.csv")  #From census data table B03002 on race at the census block level. Note we rename column to match census block metadata
    
    input_income_data <-read.csv("data/ACS/ACSDT5Y2019.B19013_2024-02-28T213319/ACSDT5Y2019.B19013-Data.csv")  #From census data table B19013 on race at the census block level. Note we rename column to match census block metadata
    
    input_income_data_tract <-read.csv("data/ACS/ACSDT5Y2019.B19013_2024-04-17T151300_tract/ACSDT5Y2019.B19013-Data.csv")  #From census data table B19013 on race at the census TRACT level. Note we rename column to match census block metadata
    
    input_income_data_county <-read.csv("data/ACS/ACSDT5Y2019.B19013_2024-04-17T154614_county/ACSDT5Y2019.B19013-Data.csv")  #From census data table B19013 on race at the census TRACT level. Note we rename column to match census block metadata
    
  ## Prepare census data 
    #2.1. Race-Ethnicity 
    #Here following Tessum et al 2019 we group people into:
    #All races hispanic/latino
    #Non-hispanic Black
    #Non-hispanic White
    #Non-hispanic Other
    input_ethnicity_data %>%
      dplyr::select(GEO_ID,
                    B03002_001E, #total population
                    B03002_012E, #Total:!!Hispanic or Latino
                    B03002_004E, #Total:!!Not Hispanic or Latino:!!Black or African American alone
                    B03002_003E, #Total:!!Not Hispanic or Latino:!!White alone
                    B03002_005E, #Total:!!Not Hispanic or Latino:!!American Indian and Alaska Native alone
                    B03002_006E, #Total:!!Not Hispanic or Latino:!!Asian alone
                    B03002_007E, #Total:!!Not Hispanic or Latino:!!Native Hawaiian and Other Pacific Islander alone
                    B03002_008E, #Total:!!Not Hispanic or Latino:!!Some other race alone
                    B03002_009E)%>% #Total:!!Not Hispanic or Latino:!!Two or more races:
      rename(census_block_ID = GEO_ID, total_population = B03002_001E, hispanic_latino = B03002_012E, 
             nonhisp_black = B03002_004E, nonhisp_white = B03002_003E) %>%
      mutate(nonhisp_other = B03002_005E + B03002_006E + B03002_007E + B03002_008E + B03002_009E) %>%
      dplyr::select(-B03002_005E, -B03002_006E, -B03002_007E, -B03002_008E, -B03002_009E)-> ethnicity_data 
    
    #2.2. Income 
    input_income_data %>% #In 2019 inflation-adjusted USD
      dplyr::select(GEO_ID, B19013_001E) %>%
      rename(census_block_ID = GEO_ID,
             median_house_income = B19013_001E) %>%
      mutate(median_house_income = if_else(median_house_income == "-", "", median_house_income)) %>% #Some missing values for income, NAs expected (9131 NAs)
      mutate(median_house_income = as.numeric(median_house_income))-> income_data 
    
    #Most of the cases the NAs are because information cannot be made publicly available for small units for privacy.
    #In these cases we use data at the census tract level
    #We reduce the NAs in income from 8299 to 1162
    #Note we have a total of 220,333 census block groups
    input_income_data_tract %>%
      mutate(GEO_ID = substr(GEO_ID, 8, 21)) -> input_income_data_tract
    
    income_data %>% 
      mutate(GEO_ID = substr(census_block_ID, 8, 20)) %>%
      left_join(input_income_data_tract, by = "GEO_ID") %>%
      mutate(B19013_001E = as.numeric(B19013_001E)) %>%
      mutate(median_house_income= if_else(is.na(median_house_income), B19013_001E, median_house_income)) %>%
      dplyr::select(-B19013_001E, -B19013_001M, -GEO_ID, -NAME)-> income_data_clean
    
    #Now we use county data income for the remaining 1422 NAs
    input_income_data_county %>%
      mutate(GEO_ID = substr(GEO_ID, 8, 21)) -> input_income_data_county
    
    income_data_clean %>%
      mutate(GEO_ID = substr(census_block_ID, 8, 14)) %>%
      left_join(input_income_data_county, by = "GEO_ID") %>%
      mutate(B19013_001E = as.numeric(B19013_001E)) %>%
      mutate(median_house_income = if_else(is.na(median_house_income), B19013_001E, median_house_income)) %>%
      dplyr::select(-B19013_001E, -B19013_001M, -GEO_ID, -NAME)-> income_data_clean_final
    
    # 2.3. Census block data
    #Keep relevant columns
    census_block <- census_block[, c("AFFGEOID", "geometry", "STATEFP")]
    
    #Calculate census block area
    census_block %>%
      mutate(block_area = round(st_area(census_block), digits = 5), #Area in m^2
             block_area = as.numeric(gsub("\\[m²\\]", "", block_area))) %>%
      rename(census_block_ID = AFFGEOID,
             state_ID = STATEFP) -> census_block
    
    #Merge all variables
    census_block %>%
      filter(!state_ID %in% c(66, 69)) %>% #filter out Guam (66) and Northern Mariana Islands (69)
      left_join(ethnicity_data, by = c("census_block_ID")) %>% #ethnicity-race
      left_join(income_data_clean_final, by = c("census_block_ID"))-> census_block_variables_geometry
    
    census_block_variables_geometry %>%
      st_drop_geometry() -> census_block_variables
    
#----REGION FILES----
#### 1. SEATTLE 1km ####
    #Partitions
    partitions_seattle <- read.csv("output_data/Partitions/partitions_seattle.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_Seattle_2019 <- read.csv("output_data/BenMAP_files/1km_V2/Seattle/Date_ 2024-05-10 PM25_2019_Seattle.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_Seattle_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/Seattle/Date_ 2024-05-10 PM25_2050_high_Seattle.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_Seattle_LOW <- read.csv("output_data/BenMAP_files/1km_V2/Seattle/Date_ 2024-05-10 PM25_2050_low_Seattle.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_Seattle_2019 <- st_read("BenMAP_results/1km_V2/Seattle/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    mortality_Seattle_HIGH <- st_read("BenMAP_results/1km_V2/Seattle/health_impacts_2050_1km_HIGHvsZero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_Seattle_LOW <- st_read("BenMAP_results/1km_V2/Seattle/health_impacts_2050_1km_LOWvsZero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_Seattle_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/Seattle/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_Seattle <- st_read("BenMAP_results/1km_V2/Seattle/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    
    #Race
    partitions_seattle %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_seattle_race
    
    #Income
    partitions_seattle %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_seattle_income
    
    #Join all
    partitions_seattle_race %>% #race
      left_join(partitions_seattle_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_Seattle_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Seattle_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Seattle_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_Seattle_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Seattle_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Seattle_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Seattle_HIGH_vs_LOW, by = c("row", "col")) %>%
      #Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.)))-> partitions_seattle_complete
  
    #Figure
    total_deaths_Seattle <- round(sum(partitions_seattle_complete$deaths_2019))
    total_deaths_Seattle_HIGH <- round(sum(partitions_seattle_complete$deaths_HIGH))
    total_deaths_Seattle_LOW <- round(sum(partitions_seattle_complete$deaths_LOW))
    total_deaths_Seattle_DIFF <- round(sum(partitions_seattle_complete$deaths_DIFF))
    
    #bins
    seattle_weighted_analysis <- city_analysis("seattle")
    seattle_white<-calculate_weighted_average_bins_white("seattle", partitions_seattle_complete)
    seattle_income<-calculate_weighted_average_bins_income("seattle", partitions_seattle_complete)
    
    ##Percent over EPA's threshold 
    calculate_exposure_percentages(partitions_seattle_complete, 9)
    calculate_exposure_percentages(partitions_seattle_complete, 5)
    
    #SM
    partitions_seattle_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_Seattle
    summary(partitions_seattle_complete$weighted_median_income)
    quantile(partitions_seattle_complete$weighted_median_income, probs = c(1/3, 2/3))
    
#### 2. LOS ANGELES 1km ####
    #Partitions
    partitions_LA <- read.csv("output_data/Partitions/partitions_LA.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_LA_2019 <- read.csv("output_data/BenMAP_files/1km_V2/LA/Date_ 2024-05-10 PM25_2019_LA.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    
    pollution_LA_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/LA/Date_ 2024-05-10 PM25_2050_high_LA.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_LA_LOW <- read.csv("output_data/BenMAP_files/1km_V2/LA/Date_ 2024-05-17 PM25_2050_low_LA.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_LA_2019 <- st_read("BenMAP_results/1km_V2/LA/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    
    mortality_LA_HIGH <- st_read("BenMAP_results/1km_V2/LA/health_impacts_2050_1km_HIGHvsZero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    
    mortality_LA_LOW <- st_read("BenMAP_results/1km_V2/LA/health_impacts_2050_1km_LOWvsZero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    
    mortality_LA_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/LA/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_LA <- st_read("BenMAP_results/1km_V2/LA/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US31080")-> cbsa_shapefile_LA
    
    #Race
    partitions_LA %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_LA_race
    
    # Check downscaling
    check_population_sum(partitions_LA_race)
    
    #Create table for SM
    partitions_LA_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_LA
    
    #Income
    partitions_LA %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_LA_income
    
    #Join all
    partitions_LA_race %>% #race
      left_join(partitions_LA_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_LA_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_LA_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_LA_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_LA_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_LA_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_LA_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_LA_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      #Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.)))-> partitions_LA_complete
    
    total_deaths_LA <- round(sum(partitions_LA_complete$deaths_2019))
    total_deaths_LA_HIGH <- round(sum(partitions_LA_complete$deaths_HIGH))
    total_deaths_LA_LOW <- round(sum(partitions_LA_complete$deaths_LOW))
    total_deaths_LA_DIFF <- round(sum(partitions_LA_complete$deaths_DIFF))
    
    #bins
    LA_weighted_analysis <- city_analysis("LA")
    LA_white <- calculate_weighted_average_bins_white("LA", partitions_LA_complete)
    LA_income <- calculate_weighted_average_bins_income("LA", partitions_LA_complete)
    
    #ercent over EPA's threshold
    calculate_exposure_percentages(partitions_LA_complete, 9)
    calculate_exposure_percentages(partitions_LA_complete, 5)
    
    #SM data
    partitions_LA_complete %>%
      filter(Population > 0) -> partitions_LA_complete_filtered
    
    partitions_LA_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_LA
    
    summary(partitions_LA_complete_filtered$weighted_median_income)
    quantile(partitions_LA_complete_filtered$weighted_median_income, probs = c(1/3, 2/3))
    
#### 3. RIVERSIDE1km ####
    #Partitions
    partitions_Riverside <- read.csv("output_data/Partitions/partitions_Riverside.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_Riverside_2019 <- read.csv("output_data/BenMAP_files/1km_V2/Riverside/Date_ 2024-05-10 PM25_2019_Riverside.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_Riverside_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/Riverside/Date_ 2024-05-10 PM25_2050_high_Riverside.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_Riverside_LOW <- read.csv("output_data/BenMAP_files/1km_V2/Riverside/Date_ 2024-05-10 PM25_2050_low_Riverside.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_Riverside_2019 <- st_read("BenMAP_results/1km_V2/Riverside/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    mortality_Riverside_HIGH <- st_read("BenMAP_results/1km_V2/Riverside/health_impacts_2050_1km_highvszero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_Riverside_LOW <- st_read("BenMAP_results/1km_V2/Riverside/health_impacts_2050_1km_lowvszero/Health Impacts-Pope 2019_20240521.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_Riverside_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/Riverside/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_Riverside <- st_read("BenMAP_results/1km_V2/Riverside/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US40140")-> cbsa_shapefile_Riverside
    
    #Race
    partitions_Riverside %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_Riverside_race
    
    # Check downscaling
    check_population_sum(partitions_Riverside_race)
    
    #Income
    partitions_Riverside %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_Riverside_income
    
    #Join all
    partitions_Riverside_race %>% #race
      left_join(partitions_Riverside_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_Riverside_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Riverside_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Riverside_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_Riverside_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Riverside_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Riverside_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Riverside_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      #Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.))) -> partitions_Riverside_complete
    
      #Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      #mutate_all(~replace_na(., 0))-> partitions_Riverside_complete
    
    total_deaths_Riverside <- round(sum(partitions_Riverside_complete$deaths_2019))
    total_deaths_Riverside_HIGH <- round(sum(partitions_Riverside_complete$deaths_HIGH))
    total_deaths_Riverside_LOW <- round(sum(partitions_Riverside_complete$deaths_LOW))
    total_deaths_Riverside_DIFF <- round(sum(partitions_Riverside_complete$deaths_DIFF))
    
    #Bins   
    Riverside_weighted_analysis <- city_analysis("Riverside")
    riverside_white <- calculate_weighted_average_bins_white("Riverside", partitions_Riverside_complete)
    riverside_income <-calculate_weighted_average_bins_income("Riverside", partitions_Riverside_complete)
    
    #Percent over EPA's threshold  
    calculate_exposure_percentages(partitions_Riverside_complete, 9)
    calculate_exposure_percentages(partitions_Riverside_complete, 5)
    
    #Create table for SM
    partitions_Riverside_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_Riverside
    summary(partitions_Riverside_complete$weighted_median_income)
    quantile(partitions_Riverside_complete$weighted_median_income, probs = c(1/3, 2/3))
    
#### 4 SAN FRANCISCO ####
    #Partitions
    partitions_SF <- read.csv("output_data/Partitions/partitions_SF.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_SF_2019 <- read.csv("output_data/BenMAP_files/1km_V2/SF/Date_ 2024-05-17 PM25_2019_SF.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_SF_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/SF/Date_ 2024-05-17 PM25_2050_high_SF.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_SF_LOW <- read.csv("output_data/BenMAP_files/1km_V2/SF/Date_ 2024-05-17 PM25_2050_low_SF.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_SF_2019 <- st_read("BenMAP_results/1km_V2/SF/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    mortality_SF_HIGH <- st_read("BenMAP_results/1km_V2/SF/health_impacts_2050_1km_highvszero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_SF_LOW <- st_read("BenMAP_results/1km_V2/SF/health_impacts_2050_1km_lowvszero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_SF_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/SF/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_SF <- st_read("BenMAP_results/1km_V2/SF/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US41860")-> cbsa_shapefile_SF
    
    #Race
    partitions_SF %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_SF_race
    
    # Check downscaling
    check_population_sum(partitions_SF_race)
    
    #Income
    partitions_SF %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_SF_income
    
    #Join all
    partitions_SF_race %>% #race
      left_join(partitions_SF_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_SF_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_SF_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_SF_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_SF_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_SF_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_SF_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_SF_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      ##Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.))) -> partitions_SF_complete
    
    total_deaths_SF <- round(sum(partitions_SF_complete$deaths_2019))
    total_deaths_SF_HIGH <- round(sum(partitions_SF_complete$deaths_HIGH))
    total_deaths_SF_LOW <- round(sum(partitions_SF_complete$deaths_LOW))
    total_deaths_SF_DIFF <- round(sum(partitions_SF_complete$deaths_DIFF))
    
    #Bins
    SF_weighted_analysis <- city_analysis("SF")
    SF_white <- calculate_weighted_average_bins_white("SF", partitions_SF_complete)
    SF_income <- calculate_weighted_average_bins_income("SF", partitions_SF_complete)
    
    #Percent over EPA's threshold
    calculate_exposure_percentages(partitions_SF_complete, 9)
    calculate_exposure_percentages(partitions_SF_complete, 5)
    
    #Create table for SM
    partitions_SF_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_SF
    summary(partitions_SF_complete$weighted_median_income)
    quantile(partitions_SF_complete$weighted_median_income, probs = c(1/3, 2/3))
    
#### 5. PHOENIX ####
    #Partitions
    partitions_Phoenix <- read.csv("output_data/Partitions/partitions_Phoenix.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_Phoenix_2019 <- read.csv("output_data/BenMAP_files/1km_V2/Phoenix/Date_ 2024-05-10 PM25_2019_Phoenix.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_Phoenix_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/Phoenix/Date_ 2024-05-10 PM25_2050_high_Phoenix.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_Phoenix_LOW <- read.csv("output_data/BenMAP_files/1km_V2/Phoenix/Date_ 2024-05-10 PM25_2050_low_Phoenix.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_Phoenix_2019 <- st_read("BenMAP_results/1km_V2/Phoenix/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    mortality_Phoenix_HIGH <- st_read("BenMAP_results/1km_V2/Phoenix/health_impacts_2050_1km_highvszero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_Phoenix_LOW <- st_read("BenMAP_results/1km_V2/Phoenix/health_impacts_2050_1km_lowvszero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_Phoenix_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/Phoenix/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_Phoenix <- st_read("BenMAP_results/1km_V2/Phoenix/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US38060")-> cbsa_shapefile_Phoenix
    
    #Race
    partitions_Phoenix %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_Phoenix_race
    
    # Check downscaling
    check_population_sum(partitions_Phoenix_race)
    
    #Income
    partitions_Phoenix %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_Phoenix_income
    
    #Join all
    partitions_Phoenix_race %>% #race
      left_join(partitions_Phoenix_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_Phoenix_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Phoenix_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Phoenix_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_Phoenix_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Phoenix_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Phoenix_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Phoenix_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      ##Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.)))-> partitions_Phoenix_complete
    
    total_deaths_Phoenix <- round(sum(partitions_Phoenix_complete$deaths_2019))
    total_deaths_Phoenix_HIGH <- round(sum(partitions_Phoenix_complete$deaths_HIGH))
    total_deaths_Phoenix_LOW <- round(sum(partitions_Phoenix_complete$deaths_LOW))
    total_deaths_Phoenix_DIFF <- round(sum(partitions_Phoenix_complete$deaths_DIFF))
    
    #Bins 
    Phoenix_weighted_analysis <- city_analysis("Phoenix")
    phoenix_white <- calculate_weighted_average_bins_white("Phoenix", partitions_Phoenix_complete)
    phoenix_income <- calculate_weighted_average_bins_income("Phoenix", partitions_Phoenix_complete)
    
    #Percent over EPA's threshold
    calculate_exposure_percentages(partitions_Phoenix_complete, 9)
    calculate_exposure_percentages(partitions_Phoenix_complete, 5)
    
    #Create table for SM
    partitions_Phoenix_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_Phoenix
    summary(partitions_Phoenix_complete$weighted_median_income)
    quantile(partitions_Phoenix_complete$weighted_median_income, probs = c(1/3, 2/3))
    
#### 6. DALLAS 1km ####
    #Partitions
    partitions_Dallas <- read.csv("output_data/Partitions/partitions_Dallas.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_Dallas_2019 <- read.csv("output_data/BenMAP_files/1km_V2/Dallas/Date_ 2024-05-10 PM25_2019_Dallas.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_Dallas_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/Dallas/Date_ 2024-05-10 PM25_2050_high_Dallas.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_Dallas_LOW <- read.csv("output_data/BenMAP_files/1km_V2/Dallas/Date_ 2024-05-10 PM25_2050_low_Dallas.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_Dallas_2019 <- st_read("BenMAP_results/1km_V2/Dallas/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    mortality_Dallas_HIGH <- st_read("BenMAP_results/1km_V2/Dallas/health_impacts_2050_1km_highvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_Dallas_LOW <- st_read("BenMAP_results/1km_V2/Dallas/health_impacts_2050_1km_lowvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_Dallas_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/Dallas/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_Dallas <- st_read("BenMAP_results/1km_V2/Dallas/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US19100")-> cbsa_shapefile_Dallas
    
    #Race
    partitions_Dallas %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_Dallas_race
    
    # Check downscaling
    check_population_sum(partitions_Dallas_race)
    
    #Income
    partitions_Dallas %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_Dallas_income
    
    #Join all
    partitions_Dallas_race %>% #race
      left_join(partitions_Dallas_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_Dallas_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Dallas_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Dallas_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_Dallas_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Dallas_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Dallas_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Dallas_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      ##Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.)))-> partitions_Dallas_complete
    
    total_deaths_Dallas <- round(sum(partitions_Dallas_complete$deaths_2019))
    total_deaths_Dallas_HIGH <- round(sum(partitions_Dallas_complete$deaths_HIGH))
    total_deaths_Dallas_LOW <- round(sum(partitions_Dallas_complete$deaths_LOW))
    total_deaths_Dallas_DIFF <- round(sum(partitions_Dallas_complete$deaths_DIFF))
    
    #Bins 
    Dallas_weighted_analysis<-city_analysis("Dallas")
    dallas_white <- calculate_weighted_average_bins_white("Dallas", partitions_Dallas_complete)
    dallas_income <- calculate_weighted_average_bins_income("Dallas", partitions_Dallas_complete)
    
    #Percent over EPA's threshold 
    calculate_exposure_percentages(partitions_Dallas_complete, 9)
    calculate_exposure_percentages(partitions_Dallas_complete, 5)
    
    #Create table for SM
    partitions_Dallas_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_Dallas
    
    summary(partitions_Dallas_complete$weighted_median_income)
    quantile(partitions_Dallas_complete$weighted_median_income, probs = c(1/3, 2/3))
    
#### 7. HOUSTON 1km ####
    #Partitions
    partitions_Houston <- read.csv("output_data/Partitions/partitions_Houston.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_Houston_2019 <- read.csv("output_data/BenMAP_files/1km_V2/Houston/Date_ 2024-05-10 PM25_2019_Houston.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_Houston_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/Houston/Date_ 2024-05-10 PM25_2050_high_Houston.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_Houston_LOW <- read.csv("output_data/BenMAP_files/1km_V2/Houston/Date_ 2024-05-10 PM25_2050_low_Houston.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_Houston_2019 <- st_read("BenMAP_results/1km_V2/Houston/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    mortality_Houston_HIGH <- st_read("BenMAP_results/1km_V2/Houston/health_impacts_2050_1km_highvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_Houston_LOW <- st_read("BenMAP_results/1km_V2/Houston/health_impacts_2050_1km_lowvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_Houston_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/Houston/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_Houston <- st_read("BenMAP_results/1km_V2/Houston/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US26420")-> cbsa_shapefile_Houston
    
    #Race
    partitions_Houston %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_Houston_race
    
    # Check downscaling
    check_population_sum(partitions_Houston_race)
    
    #Income
    partitions_Houston %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_Houston_income
    
    #Join all
    partitions_Houston_race %>% #race
      left_join(partitions_Houston_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_Houston_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Houston_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Houston_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_Houston_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Houston_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Houston_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Houston_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      ##Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.)))-> partitions_Houston_complete
    
    total_deaths_Houston <- round(sum(partitions_Houston_complete$deaths_2019))
    total_deaths_Houston_HIGH <- round(sum(partitions_Houston_complete$deaths_HIGH))
    total_deaths_Houston_LOW <- round(sum(partitions_Houston_complete$deaths_LOW))
    total_deaths_Houston_DIFF <- round(sum(partitions_Houston_complete$deaths_DIFF))
    
    #Bins
    Houston_weighted_analysis <- city_analysis("Houston")
    houston_white <- calculate_weighted_average_bins_white("Houston", partitions_Houston_complete)
    hosuton_income <- calculate_weighted_average_bins_income("Houston", partitions_Houston_complete)
    
    #Percent over EPA's threshold
    calculate_exposure_percentages(partitions_Houston_complete, 9)
    calculate_exposure_percentages(partitions_Houston_complete, 5)
    
    #Create table for SM
    partitions_Houston_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_Houston
    summary(partitions_Houston_complete$weighted_median_income)
    quantile(partitions_Houston_complete$weighted_median_income, probs = c(1/3, 2/3))

#### 8. MIAMI 1km ####
    #Partitions
    partitions_Miami <- read.csv("output_data/Partitions/partitions_Miami.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_Miami_2019 <- read.csv("output_data/BenMAP_files/1km_V2/Miami/Date_ 2024-05-10 PM25_2019_Miami.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_Miami_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/Miami/Date_ 2024-05-10 PM25_2050_high_Miami.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_Miami_LOW <- read.csv("output_data/BenMAP_files/1km_V2/Miami/Date_ 2024-05-10 PM25_2050_low_Miami.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_Miami_2019 <- st_read("BenMAP_results/1km_V2/Miami/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    
    mortality_Miami_HIGH <- st_read("BenMAP_results/1km_V2/Miami/health_impacts_2050_1km_highvszero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_Miami_LOW <- st_read("BenMAP_results/1km_V2/Miami/health_impacts_2050_1km_lowvszero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_Miami_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/Miami/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_Miami <- st_read("BenMAP_results/1km_V2/Miami/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US33100")-> cbsa_shapefile_Miami
    
    #Race
    partitions_Miami %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_Miami_race
    
    # Check downscaling
    check_population_sum(partitions_Miami_race)
    
    #Income
    partitions_Miami %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_Miami_income
    
    #Join all
    partitions_Miami_race %>% #race
      left_join(partitions_Miami_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_Miami_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Miami_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Miami_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_Miami_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Miami_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Miami_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Miami_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      ##Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.)))-> partitions_Miami_complete
    
    total_deaths_Miami <- round(sum(partitions_Miami_complete$deaths_2019))
    total_deaths_Miami_HIGH <- round(sum(partitions_Miami_complete$deaths_HIGH))
    total_deaths_Miami_LOW <- round(sum(partitions_Miami_complete$deaths_LOW))
    total_deaths_Miami_DIFF <- round(sum(partitions_Miami_complete$deaths_DIFF))
    
    #Bins
    Miami_weighted_analysis <- city_analysis("Miami")
    miami_white <- calculate_weighted_average_bins_white("Miami", partitions_Miami_complete)
    miami_income <- calculate_weighted_average_bins_income("Miami", partitions_Miami_complete)
    
    #Percent over EPA's threshold  
    calculate_exposure_percentages(partitions_Miami_complete, 9)
    calculate_exposure_percentages(partitions_Miami_complete, 5)
    
    #Create table for SM
    partitions_Miami_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_Miami
    summary(partitions_Miami_complete$weighted_median_income)
    quantile(partitions_Miami_complete$weighted_median_income, probs = c(1/3, 2/3))

#### 9. ATLANTA 1km ####
    #Partitions
    partitions_Atlanta <- read.csv("output_data/Partitions/partitions_Atlanta.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_Atlanta_2019 <- read.csv("output_data/BenMAP_files/1km_V2/Atlanta/Date_ 2024-05-10 PM25_2019_Atlanta.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_Atlanta_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/Atlanta/Date_ 2024-05-10 PM25_2050_high_Atlanta.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_Atlanta_LOW <- read.csv("output_data/BenMAP_files/1km_V2/Atlanta/Date_ 2024-05-10 PM25_2050_low_Atlanta.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_Atlanta_2019 <- st_read("BenMAP_results/1km_V2/Atlanta/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    mortality_Atlanta_HIGH <- st_read("BenMAP_results/1km_V2/Atlanta/health_impacts_2050_1km_highvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_Atlanta_LOW <- st_read("BenMAP_results/1km_V2/Atlanta/health_impacts_2050_1km_lowvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_Atlanta_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/Atlanta/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_Atlanta <- st_read("BenMAP_results/1km_V2/Atlanta/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US12060")-> cbsa_shapefile_Atlanta
    
    #Race
    partitions_Atlanta %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_Atlanta_race
    
    # Check downscaling
    check_population_sum(partitions_Atlanta_race)
    
    #Income
    partitions_Atlanta %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_Atlanta_income
    
    #Join all
    partitions_Atlanta_race %>% #race
      left_join(partitions_Atlanta_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_Atlanta_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Atlanta_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Atlanta_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_Atlanta_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Atlanta_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Atlanta_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Atlanta_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      ##Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.)))-> partitions_Atlanta_complete
    
    total_deaths_Atlanta <- round(sum(partitions_Atlanta_complete$deaths_2019))
    total_deaths_Atlanta_HIGH <- round(sum(partitions_Atlanta_complete$deaths_HIGH))
    total_deaths_Atlanta_LOW <- round(sum(partitions_Atlanta_complete$deaths_LOW))
    total_deaths_Atlanta_DIFF <- round(sum(partitions_Atlanta_complete$deaths_DIFF))
    
    # Bins
    Atlanta_weighted_analysis <- city_analysis("Atlanta")
    atlanta_white <- calculate_weighted_average_bins_white("Atlanta", partitions_Atlanta_complete)
    atlanta_income <- calculate_weighted_average_bins_income("Atlanta", partitions_Atlanta_complete)
    
    #Percent over EPA's threshold
    calculate_exposure_percentages(partitions_Atlanta_complete, 9)
    calculate_exposure_percentages(partitions_Atlanta_complete, 5)
    
    #Create table for SM
    partitions_Atlanta_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_Atlanta
    summary(partitions_Atlanta_complete$weighted_median_income)
    quantile(partitions_Atlanta_complete$weighted_median_income, probs = c(1/3, 2/3))
    
#### 10. CHICAGO 1km ####
    #Partitions
    partitions_Chicago <- read.csv("output_data/Partitions/partitions_Chicago.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_Chicago_2019 <- read.csv("output_data/BenMAP_files/1km_V2/Chicago/Date_ 2024-05-10 PM25_2019_Chicago.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_Chicago_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/Chicago/Date_ 2024-05-10 PM25_2050_high_Chicago.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_Chicago_LOW <- read.csv("output_data/BenMAP_files/1km_V2/Chicago/Date_ 2024-05-10 PM25_2050_low_Chicago.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_Chicago_2019 <- st_read("BenMAP_results/1km_V2/Chicago/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    mortality_Chicago_HIGH <- st_read("BenMAP_results/1km_V2/Chicago/health_impacts_2050_1km_highvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_Chicago_LOW <- st_read("BenMAP_results/1km_V2/Chicago/health_impacts_2050_1km_lowvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_Chicago_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/Chicago/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_Chicago <- st_read("BenMAP_results/1km_V2/Chicago/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US16980")-> cbsa_shapefile_Chicago
    
    #Race
    partitions_Chicago %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_Chicago_race
    
    # Check downscaling
    check_population_sum(partitions_Chicago_race)
    
    #Income
    partitions_Chicago %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_Chicago_income
    
    #Join all
    partitions_Chicago_race %>% #race
      left_join(partitions_Chicago_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_Chicago_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Chicago_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Chicago_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_Chicago_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Chicago_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Chicago_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Chicago_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      ##Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.)))-> partitions_Chicago_complete
    
    total_deaths_Chicago <- round(sum(partitions_Chicago_complete$deaths_2019))
    total_deaths_Chicago_HIGH <- round(sum(partitions_Chicago_complete$deaths_HIGH))
    total_deaths_Chicago_LOW <- round(sum(partitions_Chicago_complete$deaths_LOW))
    total_deaths_Chicago_DIFF <- round(sum(partitions_Chicago_complete$deaths_DIFF))
    
    #Bins
    Chicago_weighted_analysis <- city_analysis("Chicago")
    chicago_white <- calculate_weighted_average_bins_white("Chicago", partitions_Chicago_complete)
    chicago_income <- calculate_weighted_average_bins_income("Chicago", partitions_Chicago_complete)
    
    #Percent over EPA's threshold
    calculate_exposure_percentages(partitions_Chicago_complete, 9)
    calculate_exposure_percentages(partitions_Chicago_complete, 5)
  
    #Create table for SM
    partitions_Chicago_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_Chicago
    summary(partitions_Chicago_complete$weighted_median_income)
    quantile(partitions_Chicago_complete$weighted_median_income, probs = c(1/3, 2/3))
   
#### 11. DETROIT 1km ####
    #Partitions
    partitions_Detroit <- read.csv("output_data/Partitions/partitions_Detroit.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_Detroit_2019 <- read.csv("output_data/BenMAP_files/1km_V2/Detroit/Date_ 2024-05-10 PM25_2019_Detroit.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_Detroit_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/Detroit/Date_ 2024-05-10 PM25_2050_high_Detroit.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_Detroit_LOW <- read.csv("output_data/BenMAP_files/1km_V2/Detroit/Date_ 2024-05-10 PM25_2050_low_Detroit.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_Detroit_2019 <- st_read("BenMAP_results/1km_V2/Detroit/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    
    mortality_Detroit_HIGH <- st_read("BenMAP_results/1km_V2/Detroit/health_impacts_2050_1km_highvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_Detroit_LOW <- st_read("BenMAP_results/1km_V2/Detroit/health_impacts_2050_1km_lowvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_Detroit_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/Detroit/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_Detroit <- st_read("BenMAP_results/1km_V2/Detroit/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US19820")-> cbsa_shapefile_Detroit
    
    #Race
    partitions_Detroit %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_Detroit_race
    
    # Check downscaling
    check_population_sum(partitions_Detroit_race)
    
    #Income
    partitions_Detroit %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_Detroit_income
    
    #Join all
    partitions_Detroit_race %>% #race
      left_join(partitions_Detroit_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_Detroit_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Detroit_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Detroit_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_Detroit_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Detroit_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Detroit_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Detroit_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      ##Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.)))-> partitions_Detroit_complete
    
    total_deaths_Detroit <- round(sum(partitions_Detroit_complete$deaths_2019))
    total_deaths_Detroit_HIGH <- round(sum(partitions_Detroit_complete$deaths_HIGH))
    total_deaths_Detroit_LOW <- round(sum(partitions_Detroit_complete$deaths_LOW))
    total_deaths_Detroit_DIFF <- round(sum(partitions_Detroit_complete$deaths_DIFF))
  
    #By bins
    Detroit_weighted_analysis <- city_analysis("Detroit")
    detroit_white <- calculate_weighted_average_bins_white("Detroit", partitions_Detroit_complete)
    detroit_income <- calculate_weighted_average_bins_income("Detroit", partitions_Detroit_complete)
    
    #Percent over EPA's threshold 
    calculate_exposure_percentages(partitions_Detroit_complete, 9)
    calculate_exposure_percentages(partitions_Detroit_complete, 5)
    
    #Create table for SM
    partitions_Detroit_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_Detroit
    summary(partitions_Detroit_complete$weighted_median_income)
    quantile(partitions_Detroit_complete$weighted_median_income, probs = c(1/3, 2/3))
    
#### 12. WASHINGTON DC 1km ####
    #Partitions
    partitions_DC <- read.csv("output_data/Partitions/partitions_DC.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_DC_2019 <- read.csv("output_data/BenMAP_files/1km_V2/DC/Date_ 2024-05-10 PM25_2019_DC.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_DC_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/DC/Date_ 2024-05-10 PM25_2050_high_DC.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_DC_LOW <- read.csv("output_data/BenMAP_files/1km_V2/DC/Date_ 2024-05-10 PM25_2050_low_DC.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_DC_2019 <- st_read("BenMAP_results/1km_V2/DC/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    mortality_DC_HIGH <- st_read("BenMAP_results/1km_V2/DC/health_impacts_2050_1km_highvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_DC_LOW <- st_read("BenMAP_results/1km_V2/DC/health_impacts_2050_1km_lowvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_DC_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/DC/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_DC <- st_read("BenMAP_results/1km_V2/DC/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US47900")-> cbsa_shapefile_DC
    
    #Race
    partitions_DC %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_DC_race
    
    # Check downscaling
    check_population_sum(partitions_DC_race)
    
    #Income
    partitions_DC %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_DC_income
    
    #Join all
    partitions_DC_race %>% #race
      left_join(partitions_DC_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_DC_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_DC_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_DC_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_DC_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_DC_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_DC_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_DC_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      ##Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.)))-> partitions_DC_complete
    
    total_deaths_DC <- round(sum(partitions_DC_complete$deaths_2019))
    total_deaths_DC_HIGH <- round(sum(partitions_DC_complete$deaths_HIGH))
    total_deaths_DC_LOW <- round(sum(partitions_DC_complete$deaths_LOW))
    total_deaths_DC_DIFF <- round(sum(partitions_DC_complete$deaths_DIFF))
    
    #By bins
    DC_weighted_analysis <- city_analysis("DC")
    DC_white <- calculate_weighted_average_bins_white("DC", partitions_DC_complete)
    DC_income <- calculate_weighted_average_bins_income("DC", partitions_DC_complete)
    
    ##Percent over EPA's threshold 
    calculate_exposure_percentages(partitions_DC_complete, 9)
    calculate_exposure_percentages(partitions_DC_complete, 5)
    
    #Create table for SM
    partitions_DC_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_DC
    summary(partitions_DC_complete$weighted_median_income)
    quantile(partitions_DC_complete$weighted_median_income, probs = c(1/3, 2/3))
    
#### 13. NEW YORK 1km ####
    #Partitions
    partitions_NY <- read.csv("output_data/Partitions/partitions_NY.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_NY_2019 <- read.csv("output_data/BenMAP_files/1km_V2/NY/Date_ 2024-05-10 PM25_2019_NY.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_NY_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/NY/Date_ 2024-05-10 PM25_2050_high_NY.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_NY_LOW <- read.csv("output_data/BenMAP_files/1km_V2/NY/Date_ 2024-05-10 PM25_2050_low_NY.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_NY_2019 <- st_read("BenMAP_results/1km_V2/NY/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    mortality_NY_HIGH <- st_read("BenMAP_results/1km_V2/NY/health_impacts_2050_1km_highvszero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_NY_LOW <- st_read("BenMAP_results/1km_V2/NY/health_impacts_2050_1km_lowvszero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_NY_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/NY/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_NY <- st_read("BenMAP_results/1km_V2/NY/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US35620")-> cbsa_shapefile_NY
    
    #Race
    partitions_NY %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_NY_race
    
    # Check downscaling
    check_population_sum(partitions_NY_race)
    
    #Income
    partitions_NY %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_NY_income
    
    #Join all
    partitions_NY_race %>% #race
      left_join(partitions_NY_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_NY_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_NY_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_NY_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_NY_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_NY_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_NY_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_NY_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      ##Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.)))-> partitions_NY_complete
    
    total_deaths_NY <- round(sum(partitions_NY_complete$deaths_2019))
    total_deaths_NY_HIGH <- round(sum(partitions_NY_complete$deaths_HIGH))
    total_deaths_NY_LOW <- round(sum(partitions_NY_complete$deaths_LOW))
    total_deaths_NY_DIFF <- round(sum(partitions_NY_complete$deaths_DIFF))
    
    #By bins
    NY_weighted_analysis <- city_analysis("NY")
    NY_white <- calculate_weighted_average_bins_white("NY", partitions_NY_complete)
    NY_income <- calculate_weighted_average_bins_income("NY", partitions_NY_complete)
    
    ##ercent over EPA's threshold 
    calculate_exposure_percentages(partitions_NY_complete, 9)
    calculate_exposure_percentages(partitions_NY_complete, 5)
  
    #Create table for SM
    partitions_NY_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_NY
    summary(partitions_NY_complete$weighted_median_income)
    quantile(partitions_NY_complete$weighted_median_income, probs = c(1/3, 2/3))
    
#### 14. PHILADELPHIA 1km ####
    #Partitions
    partitions_Philadelphia <- read.csv("output_data/Partitions/partitions_phil.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_Philadelphia_2019 <- read.csv("output_data/BenMAP_files/1km_V2/Philadelphia/Date_ 2024-05-10 PM25_2019_Philadelphia.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_Philadelphia_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/Philadelphia/Date_ 2024-05-10 PM25_2050_high_Philadelphia.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_Philadelphia_LOW <- read.csv("output_data/BenMAP_files/1km_V2/Philadelphia/Date_ 2024-05-10 PM25_2050_low_Philadelphia.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_Philadelphia_2019 <- st_read("BenMAP_results/1km_V2/Philadelphia/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    mortality_Philadelphia_HIGH <- st_read("BenMAP_results/1km_V2/Philadelphia/health_impacts_2050_1km_highvszero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_Philadelphia_LOW <- st_read("BenMAP_results/1km_V2/Philadelphia/health_impacts_2050_1km_lowvszero/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_Philadelphia_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/Philadelphia/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_Philadelphia <- st_read("BenMAP_results/1km_V2/Philadelphia/health_impacts_2019_1km/Health Impacts-Pope 2019_20240520.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US37980")-> cbsa_shapefile_Philadelphia
    
    #Race
    partitions_Philadelphia %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_Philadelphia_race
    
    # Check downscaling
    check_population_sum(partitions_Philadelphia_race)
    
    #Income
    partitions_Philadelphia %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_Philadelphia_income
    
    #Join all
    partitions_Philadelphia_race %>% #race
      left_join(partitions_Philadelphia_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_Philadelphia_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Philadelphia_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Philadelphia_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_Philadelphia_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Philadelphia_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Philadelphia_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Philadelphia_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      ##Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.)))-> partitions_Philadelphia_complete
    
    total_deaths_Philadelphia <- round(sum(partitions_Philadelphia_complete$deaths_2019))
    total_deaths_Philadelphia_HIGH <- round(sum(partitions_Philadelphia_complete$deaths_HIGH))
    total_deaths_Philadelphia_LOW <- round(sum(partitions_Philadelphia_complete$deaths_LOW))
    total_deaths_Philadelphia_DIFF <- round(sum(partitions_Philadelphia_complete$deaths_DIFF))

    #By bins
    Philadelphia_weighted_analysis <- city_analysis("Philadelphia")
    philadelphia_white <- calculate_weighted_average_bins_white("Philadelphia", partitions_Philadelphia_complete)
    philadelphia_income <- calculate_weighted_average_bins_income("Philadelphia", partitions_Philadelphia_complete)
    
    #Percent over EPA's threshold
    calculate_exposure_percentages(partitions_Philadelphia_complete, 9)
    calculate_exposure_percentages(partitions_Philadelphia_complete, 5)
    
    #Create table for SM
    partitions_Philadelphia_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_Philadelphia
    summary(partitions_Philadelphia_complete$weighted_median_income)
    quantile(partitions_Philadelphia_complete$weighted_median_income, probs = c(1/3, 2/3))
    
#### 15. BOSTON 1km ####
    #Partitions
    partitions_Boston <- read.csv("output_data/Partitions/partitions_Boston.csv") %>% dplyr::select(-X)
    
    #Pollution
    pollution_Boston_2019 <- read.csv("output_data/BenMAP_files/1km_V2/Boston/Date_ 2024-05-10 PM25_2019_Boston.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_2019 = Values, row = Row, col = Column)
    pollution_Boston_HIGH <- read.csv("output_data/BenMAP_files/1km_V2/Boston/Date_ 2024-05-10 PM25_2050_high_Boston.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_HIGH = Values, row = Row, col = Column)
    pollution_Boston_LOW <- read.csv("output_data/BenMAP_files/1km_V2/Boston/Date_ 2024-05-10 PM25_2050_low_Boston.csv") %>% dplyr::select(Row, Column, Values) %>% rename(PM25_LOW = Values, row = Row, col = Column)
    
    #Mortality
    mortality_Boston_2019 <- st_read("BenMAP_results/1km_V2/Boston/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim, Population) %>% rename(row = ROW, col = COL, benmap_population = Population, deaths_2019 = Point.Estim) 
    mortality_Boston_HIGH <- st_read("BenMAP_results/1km_V2/Boston/health_impacts_2050_1km_highvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_HIGH = Point.Estim) 
    mortality_Boston_LOW <- st_read("BenMAP_results/1km_V2/Boston/health_impacts_2050_1km_lowvszero/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_LOW = Point.Estim) 
    mortality_Boston_HIGH_vs_LOW <- st_read("BenMAP_results/1km_V2/Boston/health_impacts_2050_1km/Health Impacts-Pope 2019_20240517.shp") %>% st_drop_geometry() %>% dplyr::select(ROW, COL, Point.Estim) %>% rename(row = ROW, col = COL, deaths_DIFF = Point.Estim) 
    
    #Geometry
    geometry_Boston <- st_read("BenMAP_results/1km_V2/Boston/health_impacts_2019_1km/Health Impacts-Pope 2019_20240517.shp") %>%
      dplyr::select(ROW, COL) %>% rename(row = ROW, col = COL,) 
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US14460")-> cbsa_shapefile_Boston
    
    #Race
    partitions_Boston %>%
      left_join(ethnicity_data, by = "census_block_ID") %>%
      mutate(Population = total_population * area_ratio_block,
             Black = nonhisp_black * area_ratio_block,
             White = nonhisp_white * area_ratio_block,
             Hispanic = hispanic_latino * area_ratio_block,
             Other = nonhisp_other * area_ratio_block) %>%
      group_by(pixel_ID, row, col) %>%
      summarise(Population = sum(Population),
                White = sum(White), 
                Black = sum(Black), 
                Hispanic = sum(Hispanic),
                Other = sum(Other)) %>%
      ungroup() %>%
      mutate(percent_black = if_else(Population == 0, 0, (Black * 100)/Population),
             percent_white = if_else(Population == 0, 0, (White * 100)/Population),
             percent_hispanic = if_else(Population == 0, 0, (Hispanic * 100)/Population),
             percent_other = if_else(Population == 0, 0, (Other * 100)/Population))-> partitions_Boston_race
    
    # Check downscaling
    check_population_sum(partitions_Boston_race)
    
    #Income
    partitions_Boston %>%
      left_join(income_data_clean_final, by = "census_block_ID") %>%
      group_by(pixel_ID, row, col) %>%
      mutate(weighted_median_income = (sum(median_house_income * area_ratio_pixel))/ sum(area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, weighted_median_income) %>%
      unique() -> partitions_Boston_income
    
    #Join all
    partitions_Boston_race %>% #race
      left_join(partitions_Boston_income, by = c("pixel_ID", "row", "col")) %>% #income
      left_join(pollution_Boston_2019, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Boston_HIGH, by = c("row", "col")) %>% #PM25 in ug/m3
      left_join(pollution_Boston_LOW, by = c("row", "col")) %>% #PM25 in ug/m3
      mutate(PM25_DIFF = PM25_HIGH - PM25_LOW) %>%
      left_join(mortality_Boston_2019, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Boston_HIGH, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Boston_LOW, by = c("row", "col")) %>% #Mortality 
      left_join(mortality_Boston_HIGH_vs_LOW, by = c("row", "col")) %>% #Mortality 
      ##Pixels where population = 0 have no mortality, thus NAs. We replace them with 0
      mutate(across(c("Population", "White", "Black", "Hispanic", "Other", 
                      "percent_black", "percent_white", "percent_hispanic", "percent_other", "benmap_population",
                      "deaths_2019", "deaths_HIGH","deaths_LOW" ,"deaths_DIFF"), 
                    ~ replace_na(., 0))) %>%
      #Here we filter out pixels with NA in pollution, which refer to some bordering pixels
      filter_all(all_vars(!is.na(.)))-> partitions_Boston_complete
    
    total_deaths_Boston <- round(sum(partitions_Boston_complete$deaths_2019))
    total_deaths_Boston_HIGH <- round(sum(partitions_Boston_complete$deaths_HIGH))
    total_deaths_Boston_LOW <- round(sum(partitions_Boston_complete$deaths_LOW))
    total_deaths_Boston_DIFF <- round(sum(partitions_Boston_complete$deaths_DIFF))
    
    #By bins
    Boston_weighted_analysis <- city_analysis("Boston")
    boston_white <- calculate_weighted_average_bins_white("Boston", partitions_Boston_complete)
    boston_income <- calculate_weighted_average_bins_income("Boston", partitions_Boston_complete)
    
    # Percent over EPA's threshold
    calculate_exposure_percentages(partitions_Boston_complete, 9)
    calculate_exposure_percentages(partitions_Boston_complete, 5)
    
    #Create table for SM
    partitions_Boston_race %>%
      summarise(population = sum(Population), White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_Boston
    summary(partitions_Boston_complete$weighted_median_income)
    quantile(partitions_Boston_complete$weighted_median_income, probs = c(1/3, 2/3))
    
  
#### 16. ALL CITIES TOGETHER ####
  #Partitions
    partitions_Atlanta_complete$region <- "Atlanta"
    partitions_Boston_complete$region <- "Boston"
    partitions_Chicago_complete$region <-"Chicago"
    partitions_Dallas_complete$region <- "Dallas"
    partitions_DC_complete$region <- "DC"
    partitions_Detroit_complete$region <- "Detroit"
    partitions_Houston_complete$region <- "Houston"
    partitions_LA_complete$region <- "LA"
    partitions_Miami_complete$region <- "Miami"
    partitions_NY_complete$region <- "NY"
    partitions_Philadelphia_complete$region <- "Philadelphia"
    partitions_Phoenix_complete$region <- "Phoenix"
    partitions_Riverside_complete$region <- "Riverside"
    partitions_seattle_complete$region <- "Seattle"
    partitions_SF_complete$region <- "SF"
    
    partitions_Atlanta_complete %>%
      bind_rows(partitions_Boston_complete, partitions_Chicago_complete, partitions_Dallas_complete,
                partitions_DC_complete, partitions_Detroit_complete, partitions_Houston_complete,
                partitions_LA_complete, partitions_Miami_complete, partitions_NY_complete,
                partitions_Philadelphia_complete, partitions_Phoenix_complete,
                partitions_Riverside_complete, partitions_seattle_complete,partitions_SF_complete) -> partitions_all_complete
    
    summary(partitions_all_complete$weighted_median_income)
    quantile(partitions_all_complete$weighted_median_income, probs = c(1/3, 2/3))
    
    total_deaths <- round(sum(partitions_all_complete$deaths_2019))
    
  #Create table for SM
    partitions_all_complete %>%
      summarise(population = sum(Population),  White = sum(White), Black = sum(Black),
                Hispanic = sum(Hispanic), Other = sum(Other)) -> total_race_all 
    
  #Population weighted
    pop_weighted_all <- city_analysis("all")
    pop_weighted_per_white <- calculate_weighted_average_bins_white("All", partitions_all_complete)
    pop_weighted_income <- calculate_weighted_average_bins_income("All", partitions_all_complete)
    
  #Percent over EPA's threshold
    calculate_exposure_percentages(partitions_all_complete, 9) #EPA
    calculate_exposure_percentages(partitions_all_complete, 5) #WHO
    
#--------------------------------------------------------------- FIGURE 5 -------------------------------------------------------------------
#### 5e-f. Analyze all data ####
    ##### 17.1. Merge all tables with weighted averages ####
    ### Cities ####
    #1. This is for cities by races: All, White, Black, Hispanic, Other
    # List to store all result tables
    result_tables <- list()
    
    # Loop through each city and generate the result table
    for (city_name in c("seattle", "LA", "Riverside", "SF", "Phoenix", "Dallas", "Houston", "Miami",
                        "Atlanta", "Chicago", "Detroit", "DC", "NY", "Philadelphia", "Boston")) {
      result_table_name <- paste0(city_name, "_weighted_analysis")
      
      # Check if the result table exists in the global environment
      if (exists(result_table_name, envir = .GlobalEnv)) {
        result_tables[[city_name]] <- get(result_table_name, envir = .GlobalEnv)
      } else {
        print(paste("Warning: Result table for", city_name, "does not exist."))
      }
    }
    
    # Combine all result tables into one master table
    master_table_city <- do.call(rbind, result_tables)
    #write.csv(master_table_city, "output_data/results/master_table_weighted_averages_city.csv")
    
    #2. This is for bins
    bin_white <- bind_rows(seattle_white, LA_white, riverside_white, SF_white, phoenix_white, dallas_white, houston_white, miami_white, atlanta_white, chicago_white, detroit_white, DC_white, NY_white, philadelphia_white, boston_white)
    
    bin_income <- bind_rows(seattle_income, LA_income, riverside_income, SF_income, phoenix_income, dallas_income, hosuton_income, miami_income, atlanta_income, chicago_income, detroit_income, DC_income, NY_income, philadelphia_income, boston_income)
    
    master_table_weighted_averages_bins_city <- bind_rows(bin_white, bin_income)
    #write.csv(master_table_weighted_averages_bins_city, "output_data/results/master_table_weighted_averages_bins_city.csv")
    
#### 17.2. Scatterplots based on race ####
    ### PM2.5 and race ####
    bin_white_filtered_high <- bin_white %>% #HIGH CDR
      filter(bin != "31to60white") %>%
      # Select the columns you want to spread
      dplyr::select(bin, PM25_HIGH, region) %>%
      # Spread the values across columns based on the 'bin' column
      spread(key = bin, value = PM25_HIGH) %>%
      mutate(scenario = "HIGH")
    
    bin_white_filtered_low <- bin_white %>% #HIGH CDR
      filter(bin != "31to60white") %>%
      # Select the columns you want to spread
      dplyr::select(bin, PM25_LOW, region) %>%
      # Spread the values across columns based on the 'bin' column
      spread(key = bin, value = PM25_LOW) %>%
      mutate(scenario = "LOW")
    
    bin_white_graph <- bind_rows(bin_white_filtered_high, bin_white_filtered_low)
    
    # Create the scatterplot
    p<-ggplot(bin_white_graph, aes(x = `0to30white`, y = `61to100white`, label = region,  color = scenario, shape = scenario)) +
      geom_point() +  # Add points for each city
      geom_text(hjust = 0, vjust = 0) +  # Add city labels
      geom_line(aes(group = region), color = "grey") +  # Connect the dots by city
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Add the 1:1 line
      labs(x = "Population-weighted PM2.5 for 0-30% white",
           y = "Population-weighted PM2.5 for 61-100% white",
           title = "Scatterplot of Population-weighted PM2.5")
    ggsave("figures/paper_figures/4.5.scatterplot_cities_white_PM.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/4.5.scatterplot_cities_white_PM.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    bin_white_graph %>%
      filter(region %in% c("Riverside", "Boston", "Phoenix", "Miami", "Detroit")) -> bin_white_graph_top5
    
    p<-ggplot(bin_white_graph_top5, aes(x = `0to30white`, y = `61to100white`, label = region,  color = scenario, shape = scenario)) +
      geom_point() +  # Add points for each city
      geom_text(hjust = 0, vjust = 0) +  # Add city labels
      geom_line(aes(group = region), color = "grey") +  # Connect the dots by city
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Add the 1:1 line
      labs(x = "Population-weighted PM2.5 for 0-30% white",
           y = "Population-weighted PM2.5 for 61-100% white",
           title = "Scatterplot of Population-weighted PM2.5") +
      xlim(3, 8.2) +  # Set x-axis limit
      ylim(3, 7.68)    # Set y-axis limit
    ggsave("figures/paper_figures/4.5.scatterplot_cities_white_PM_top5.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/4.5.scatterplot_cities_white_PM_top5.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    ### Deaths and race ####
    bin_white_filtered_high <- bin_white %>% #HIGH CDR
      filter(bin != "31to60white") %>%
      # Select the columns you want to spread
      dplyr::select(bin, deaths_HIGH, region) %>%
      # Spread the values across columns based on the 'bin' column
      spread(key = bin, value = deaths_HIGH) %>%
      mutate(scenario = "HIGH")
    
    bin_white_filtered_low <- bin_white %>% #HIGH CDR
      filter(bin != "31to60white") %>%
      # Select the columns you want to spread
      dplyr::select(bin, deaths_LOW, region) %>%
      # Spread the values across columns based on the 'bin' column
      spread(key = bin, value = deaths_LOW) %>%
      mutate(scenario = "LOW")
    
    bin_white_graph <- bind_rows(bin_white_filtered_high, bin_white_filtered_low)
    
    # Create the scatterplot
    p<-ggplot(bin_white_graph, aes(x = `0to30white`, y = `61to100white`, label = region,  color = scenario, shape = scenario)) +
      geom_point() +  # Add points for each city
      geom_text(hjust = 0, vjust = 0) +  # Add city labels
      geom_line(aes(group = region), color = "grey") +  # Connect the dots by city
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Add the 1:1 line
      labs(x = "Population-weighted deaths for 0-30% white",
           y = "Population-weighted deaths for 61-100% white",
           title = "Scatterplot of Population-weighted deaths")
    ggsave("figures/paper_figures/4.6.scatterplot_cities_white_deaths.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/4.6.scatterplot_cities_white_deaths.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #ZOOM
    bin_white_graph %>%
      #The cities with the lowest slope are Philadelphia (0.3), Boston (0.32), NY (0.44). all cities combined (0.5),
      #Riverside (0.56), Detroit (0.56)
      filter(region %in% c("Philadelphia", "Boston", "NY", "Riverside", "Detroit")) -> bin_white_graph_top5
   
    # Create the scatterplot
    p<-ggplot(bin_white_graph_top5, aes(x = `0to30white`, y = `61to100white`, label = region,  color = scenario, shape = scenario)) +
      geom_point() +  # Add points for each city
      geom_text(hjust = 0, vjust = 0) +  # Add city labels
      geom_line(aes(group = region), color = "grey") +  # Connect the dots by city
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Add the 1:1 line
      labs(x = "Population-weighted deaths for 0-30% white",
           y = "Population-weighted deaths for 61-100% white",
           title = "Scatterplot of Population-weighted deaths") +
      xlim(0.5, 5.1) +  # Set x-axis limit
      ylim(0.3, 2.3)    # Set y-axis limit
    ggsave("figures/paper_figures/4.6.scatterplot_cities_white_deaths_zoom_top5.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/4.6.scatterplot_cities_white_deaths_zoom_top5.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 17.3. scatter based on income ####
    ### PM2.5 and income ####
    bin_income_filtered_high <- bin_income %>% #HIGH CDR
      filter(bin != "33to66percentile") %>%
      # Select the columns you want to spread
      dplyr::select(bin, PM25_HIGH, region) %>%
      # Spread the values across columns based on the 'bin' column
      spread(key = bin, value = PM25_HIGH) %>%
      mutate(scenario = "HIGH")
    
    bin_income_filtered_low <- bin_income %>% #HIGH CDR
      filter(bin != "33to66percentile") %>%
      # Select the columns you want to spread
      dplyr::select(bin, PM25_LOW, region) %>%
      # Spread the values across columns based on the 'bin' column
      spread(key = bin, value = PM25_LOW) %>%
      mutate(scenario = "LOW")
    
    bin_income_graph <- bind_rows(bin_income_filtered_high, bin_income_filtered_low)
    
    # Create the scatterplot
    p<-ggplot(bin_income_graph, aes(x = `0to33percentile`, y = `66to100percentile`, label = region,  color = scenario, shape = scenario)) +
      geom_point() +  # Add points for each city
      geom_text(hjust = 0, vjust = 0) +  # Add city labels
      geom_line(aes(group = region), color = "grey") +  # Connect the dots by city
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Add the 1:1 line
      labs(x = "Population-weighted PM2.5 for 0-33 income percentile",
           y = "Population-weighted PM2.5 for 66-100 income percentile",
           title = "Scatterplot of Population-weighted PM2.5")
    ggsave("figures/paper_figures/4.7.scatterplot_cities_income_PM.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/4.7.scatterplot_cities_income_PM.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #ZOOM
    bin_income_graph %>%
      filter(region %in% c("Miami", "seattle", "LA", "Detroit", "SF")) -> bin_income_graph_top5
    
    p<-ggplot(bin_income_graph_top5, aes(x = `0to33percentile`, y = `66to100percentile`, label = region,  color = scenario, shape = scenario)) +
      geom_point() +  # Add points for each city
      geom_text(hjust = 0, vjust = 0) +  # Add city labels
      geom_line(aes(group = region), color = "grey") +  # Connect the dots by city
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Add the 1:1 line
      labs(x = "Population-weighted PM2.5 for 0-33 income percentile",
           y = "Population-weighted PM2.5 for 66-100 income percentile",
           title = "Scatterplot of Population-weighted PM2.5") +
      xlim(3, 8.2) +  # Set x-axis limit
      ylim(3, 7.68)    # Set y-axis limit
    ggsave("figures/paper_figures/4.7.scatterplot_cities_income_PM_top5.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/4.7.scatterplot_cities_income_PM_top5.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    ### Deaths and income ####
    bin_income_filtered_high <- bin_income %>% #HIGH CDR
      filter(bin != "33to66percentile") %>%
      # Select the columns you want to spread
      dplyr::select(bin, deaths_HIGH, region) %>%
      # Spread the values across columns based on the 'bin' column
      spread(key = bin, value = deaths_HIGH) %>%
      mutate(scenario = "HIGH")
    
    bin_income_filtered_low <- bin_income %>% #HIGH CDR
      filter(bin != "33to66percentile") %>%
      # Select the columns you want to spread
      dplyr::select(bin, deaths_LOW, region) %>%
      # Spread the values across columns based on the 'bin' column
      spread(key = bin, value = deaths_LOW) %>%
      mutate(scenario = "LOW")
    
    bin_income_graph <- bind_rows(bin_income_filtered_high, bin_income_filtered_low)
    
    # Create the scatterplot
    p<-ggplot(bin_income_graph, aes(x = `0to33percentile`, y = `66to100percentile`, label = region,  color = scenario, shape = scenario)) +
      geom_point() +  # Add points for each city
      geom_text(hjust = 0, vjust = 0) +  # Add city labels
      geom_line(aes(group = region), color = "grey") +  # Connect the dots by city
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Add the 1:1 line
      labs(x = "Population-weighted mortality for 0-33 income percentile",
           y = "Population-weighted mortality for 66-100 income percentile",
           title = "Scatterplot of Population-weighted deaths")
    ggsave("figures/paper_figures/4.8.scatterplot_cities_income_deaths.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/4.8.scatterplot_cities_income_deaths.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    bin_income_graph %>%
      filter(region %in% c("Philadelphia", "Boston", "Detroit", "Miami", "NY")) -> bin_income_graph_zoom
    
    # Create the scatterplot
    p<-ggplot(bin_income_graph_zoom, aes(x = `0to33percentile`, y = `66to100percentile`, label = region,  color = scenario, shape = scenario)) +
      geom_point() +  # Add points for each city
      geom_text(hjust = 0, vjust = 0) +  # Add city labels
      geom_line(aes(group = region), color = "grey") +  # Connect the dots by city
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Add the 1:1 line
      labs(x = "Population-weighted mortality for 0-33 income percentile",
           y = "Population-weighted mortality for 66-100 income percentile",
           title = "ZOOM Scatterplot of Population-weighted deaths")+
      xlim(0.5, 5.1) +  # Set x-axis limit
      ylim(0.3, 2.3)    # Set y-axis limit
    ggsave("figures/paper_figures/4.8.scatterplot_cities_income_deaths_zoom_top5.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/4.8.scatterplot_cities_income_deaths_zoom_top5.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
 
#--------------------------------------------------------------- POLICY COSTS -------------------------------------------------------------------
    #Connect to the database
      conn <- localDBConn('/Users/mariacandelariabergero/Documents/GCAM/gcam-v6.0-Mac-Release-Package/output', 'database_basexdb_FINAL_policy_cost')
    
    # #Load the scenario
    # prj_cost <- addScenario(conn, 'dat/GCAM_analysis_V3_policy_cost.dat', 'GCAM-USA_DAC_NZUSA2050ghg_NZROW2060co2_newEFV2_allEF_0percent', 'queries/queries_cost.xml') #High CDR (0% of tax)
    #   prj_cost <- addScenario(conn, 'dat/GCAM_analysis_V3_policy_cost.dat', 'GCAM-USA_DAC_NZUSA2050ghg_NZROW2060co2_newEFV2_allEF_25percent', 'queries/queries_cost.xml') #High CDR (25% of tax)
    #   prj_cost <- addScenario(conn, 'dat/GCAM_analysis_V3_policy_cost.dat', 'GCAM-USA_DAC_NZUSA2050ghg_NZROW2060co2_newEFV2_allEF_50percent', 'queries/queries_cost.xml') #High CDR (50% of tax)
    #   prj_cost <- addScenario(conn, 'dat/GCAM_analysis_V3_policy_cost.dat', 'GCAM-USA_DAC_NZUSA2050ghg_NZROW2060co2_newEFV2_allEF_75percent', 'queries/queries_cost.xml') #High CDR (75% of tax)
    #    
    #   prj_cost <- addScenario(conn, 'dat/GCAM_analysis_V3_policy_cost.dat', 'GCAM-USA_withDAC_NZUSA2050ghg_NZROW2060co2_forest_CCSV3_allEF_0percent', 'queries/queries_cost.xml') #Low CDR (0% of tax)
    #   prj_cost <- addScenario(conn, 'dat/GCAM_analysis_V3_policy_cost.dat', 'GCAM-USA_withDAC_NZUSA2050ghg_NZROW2060co2_forest_CCSV3_allEF_25percent', 'queries/queries_cost.xml') #Low CDR (25% of tax)
    #   prj_cost <- addScenario(conn, 'dat/GCAM_analysis_V3_policy_cost.dat', 'GCAM-USA_withDAC_NZUSA2050ghg_NZROW2060co2_forest_CCSV3_allEF_50percent', 'queries/queries_cost.xml') #Low CDR (50% of tax)
    #   prj_cost <- addScenario(conn, 'dat/GCAM_analysis_V3_policy_cost.dat', 'GCAM-USA_withDAC_NZUSA2050ghg_NZROW2060co2_forest_CCSV3_allEF_75percent', 'queries/queries_cost.xml') #Low CDR (75% of tax)
    
    #Load the project
      prj_cost <- loadProject('dat/GCAM_analysis_V3_policy_cost.dat')  
    
    #Get query on CO2 prices
      CO2_prices_MAC <- getQuery(prj_cost, "CO2 prices")
    
      CO2_prices_MAC %>%
        filter(market == "USACO2",
               year == 2050) -> CO2_prices_MAC_final
      
    #Get query on CO2 emissions
      CO2_emissions_MAC <- getQuery(prj_cost, "CO2 emissions by region")
      
      CO2_emissions_MAC %>%
        filter(year == 2050,
               region %in% USA_all) %>%
        filter(!region %in% c("AK", "HI")) %>% #Filter out Alaska and Hawaii
        group_by(Units, scenario, year) %>%
        summarise(USA_emissions = sum(value)) %>%
        ungroup() -> CO2_emissions_MAC_final
      
      #Now bring original scenarios (REF, High-CDR, Low-CDR)
      #Get query on CO2 prices
      CO2_prices <- getQuery(prj, "CO2 prices")
      CO2_prices %>%
        filter(market == "USACO2",
               year == 2050) %>%
        mutate(scenario = paste0(scenario, "_100percent"))-> CO2_prices_final
      
      CO2_emissions <- getQuery(prj, "CO2 emissions by region")
      
      CO2_emissions %>%
        filter(year == 2050,
               region %in% USA_all) %>%
        filter(!region %in% c("AK", "HI")) %>% #Filter out Alaska and Hawaii
        group_by(Units, scenario, year) %>%
        summarise(USA_emissions = sum(value)) %>%
        ungroup() %>%
        mutate(scenario = paste0(scenario, "_100percent"))-> CO2_emissions_final #Reference CO2 emissions = 1204.30174 MTC
      
      CO2_emissions_final %>%
        filter(scenario != "GCAM-USA_REF_100percent") -> CO2_emissions_final_filtered
      
      #Merge all
      CO2_prices_MAC_all <- bind_rows(CO2_prices_MAC_final, CO2_prices_final)
      CO2_emissions_MAC_all <- bind_rows(CO2_emissions_MAC_final, CO2_emissions_final_filtered)
      zero_tax_HIGH <- CO2_emissions_MAC_all %>% filter(scenario == "GCAM-USA_DAC_NZUSA2050ghg_NZROW2060co2_newEFV2_allEF_0percent") %>% dplyr::select(USA_emissions) %>% as.numeric()
      zero_tax_LOW <- CO2_emissions_MAC_all %>% filter(scenario == "GCAM-USA_withDAC_NZUSA2050ghg_NZROW2060co2_forest_CCSV3_allEF_0percent") %>% dplyr::select(USA_emissions) %>% as.numeric()
      
      CO2_emissions_MAC_all %>%
        rename(units_emissions = Units) %>%
        mutate(CDR = ifelse(grepl("GCAM-USA_DAC_NZUSA2050ghg_NZROW2060co2_newEFV2_allEF", scenario), "HIGH-CDR", "LOW-CDR"))%>%
        mutate(abatement_quantity = if_else(CDR == "HIGH-CDR", zero_tax_HIGH - USA_emissions, zero_tax_LOW- USA_emissions)) %>%
        mutate(abatement_quantity_tC = abatement_quantity * 10^6) %>%
        left_join(CO2_prices_MAC_all, by = c("scenario", "year")) %>%
        rename(carbon_price = value) %>%
        mutate(carbon_price = replace(carbon_price, is.na(carbon_price), 0),
               #1 USD1990 = 2.33 USD2023
               carbon_price_2023USD = carbon_price * 2.33)-> MAC_curves_final

      # Create the scatter plot
      
      p <- ggplot(MAC_curves_final, aes(x = abatement_quantity_tC, y = carbon_price_2023USD, color = CDR)) +
        geom_point() +
        geom_line() +
        facet_wrap(~ CDR, ncol = 1) +
        ggtitle("Abatement Quantity vs CO2 Price") +
        xlab("Abatement Quantity (tC)") +
        ylab("CO2 Price (2023$/tC)") +
        theme_minimal()
      #ggsave("figures/paper_figures/abatement_cost.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/paper_figures/abatement_cost.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Now calculate trapezoid area
      calculate_trapezoid_area <- function(df) {
        df <- df %>% arrange(abatement_quantity_tC)
        areas <- 0.5 * (df$carbon_price[-1] + df$carbon_price[-n()]) * diff(df$abatement_quantity_tC)
        sum(areas, na.rm = TRUE)
      }
      
      trapezoid_areas <- MAC_curves_final %>%
        group_by(CDR) %>%
        dplyr::summarize(trapezoid_area = calculate_trapezoid_area(cur_data()))
      
      #Now convert to 2023$ -> https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator
      trapezoid_areas %>%
        mutate(trapezoid_area_2023USD = trapezoid_area * 2.33) -> trapezoid_areas
      # View the result
      print(trapezoid_areas)
      
#--------------------------------------------------------------- SUPPLEMENTARY TABLE 1 -------------------------------------------------------------------
#### SM Table 1: Carbon storage costs #####
    carbon_storage_cost <- getQuery(prj, "price of a specific market")
    
    carbon_storage_cost %>%
      filter(year > 2010 & year < 2055) %>%
      #Origina units in 1990$/tC, convert to 2023$/tCO2
      mutate(price_2020_C = value * 2.33) %>% # multiplier from BLS https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1.00&year1=199012&year2=202012
      mutate(price_2020_CO2 = price_2020_C / emissions.CONV_C_CO2) %>%
      left_join(scenario_mapping, by = "scenario") %>%
      mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) %>%
      select(Scenario, year, market, price_2020_CO2) %>%
      mutate(Units = "2023$/tCO2") %>%
      group_by(Scenario, year, Units) %>%
      summarise(mean_price = mean(price_2020_CO2)) %>%
      ungroup()-> carbon_storage_cost_final
    
    p <- ggplot() + geom_line(data=carbon_storage_cost_final, aes(x=year, y=mean_price, colour = Scenario, group = Scenario), size = 1) +
      geom_text(data = carbon_storage_cost_final %>% filter(year %in% c(2030, 2050)),
                aes(x = year, y = mean_price, label = round(mean_price)),
                hjust = -0.2, vjust = 0.2, size = 5) + 
      scale_colour_manual(values = scenario_short_color) +
      ggtitle( "Mean National Carbon Storage Cost" ) +
      xlab("Year") +
      ylab("2023$ / tCO2") +
      figure_theme
    ggsave("figures/paper_figures/SM_CO2_storage_cost.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM_CO2_storage_cost.svg", dpi=600/2, width=6000/300, height=3000/300)
    
#--------------------------------------------------------------- SUPPLEMENTARY FIGURE 2 -------------------------------------------------------------------
#### SM Figure 2: GCAM 2015 values ####
    #### SM2.1. Primary energy ####
    primary_energy_consumption %>%
      filter(year == 2015,
             !fuel %in% c("traded coal", "traded natural gas", "traded oil")) %>%
      left_join(scenario_mapping, by = "scenario") -> primary_energy_consumption_states_2015
    
    primary_energy_consumption_states_2015 %>%
      left_join(PE_fuel_mapping, by = "fuel") %>%
      filter(!region %in% c("AK", "HI")) %>% #Filter out Alaska and Hawaii
      group_by(year, Units, Scenario, agg_fuel) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      mutate(Fuel = factor(agg_fuel, levels = primary_energy_order)) %>%
      mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> primary_energy_consumption_states_2015_USA
    
    p <- ggplot(data = primary_energy_consumption_states_2015_USA, aes(x = year, y = sum, fill = Fuel)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = primary_energy_color_2) +
      facet_wrap(~ Scenario, ncol = 2, as.table = FALSE) +
      ggtitle("2015 Primary Energy Consumption by Fuel") +
      xlab("Year") +
      ylab("EJ") +
      figure_theme
    ggsave("figures/paper_figures/SM2_primary_energy_fuel_USA_2015.png", plot = p, dpi = 600/2, width = 6000/300, height = 3000/300)
    ggsave("figures/paper_figures/SM2.primary_energy_fuel_USA_2015.svg", plot = p, dpi = 600/2, width = 6000/300, height = 3000/300)
    
    
    #### SM2.2. CO2 emissions #####
    #All emissions
    CO2_nobio_all_sector %>%
      filter(GHG != "Electricity") %>%
      bind_rows(CO2_nobio_all_sector_electircity_complete) %>%
      bind_rows(LUC_emissions_region_final, GHG_emissions_region_complete) %>% 
      mutate(GHG = factor(GHG, levels = GHG_sector_order)) %>%
      mutate(total_USA = total_USA / 10^3,
             Units = "GtCO2eq") %>%
      filter(year == 2015)-> all_GHG_emissions_2015
    
    #Net GHG emissions
    all_GHG_emissions_2015 %>%
      group_by(Scenario, year) %>%
      summarise(net = sum(total_USA)) -> all_GHG_emissions_net_2015
    
    #Net carbon emissions
    all_GHG_emissions_2015 %>%
      filter(!GHG %in% c("CH4", "HFCs & SF6", "N2O")) %>%
      group_by(Scenario, year) %>%
      summarise (net = sum(total_USA)) -> all_GHG_emissions_net_CO2_net_2015
    
    p <- ggplot() + 
      geom_col(data = all_GHG_emissions_2015, aes(x = year, y = total_USA, fill = GHG), position = "stack") +
      geom_point(data = all_GHG_emissions_net_2015, aes(x = year, y = net), color = "#264653") +
      geom_point(data = all_GHG_emissions_net_CO2_net_2015, aes(x = year, y = net)) +
      scale_fill_manual(values = CO2_sector_color) +
      scale_x_continuous(breaks = seq(2010, 2100, 10)) +
      facet_wrap(~ Scenario, ncol = 2, as.table = FALSE) +
      ggtitle("Total CO2 Emissions by sector") +
      xlab("Year") +
      ylab("GtCO2") +
      figure_theme
    ggsave("figures/paper_figures/SM2.CO2_emissions_sector_USA_nobio_2015.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM2.CO2_emissions_sector_USA_nobio_2015.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    #### SM2.3. PM2.5 Emissions ####
    air_pollution_sources_complete_PM2.5 %>%
      bind_rows(air_pollution_resoruce_prod_PM2.5) %>%
      mutate(Sector = factor(Sector, levels = PM_sector_order)) %>%
      filter(year == 2015)-> air_pollution_2.5_final_2015
    
    p <- ggplot() + 
      geom_col(data = air_pollution_2.5_final_2015, aes(x = year, y = total_USA, fill = Sector), position = "stack") +
      scale_fill_manual(values=pollutant_sector_color_2) +
      facet_wrap(~ Scenario, ncol = 2, as.table = FALSE) +
      ggtitle("Total CO2 Emissions by sector") +
      xlab("Year") +
      ylab("GtCO2") +
      figure_theme
    ggsave("figures/paper_figures/SM2.PM2.5_emissions_sector_USA_2015.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM2.PM2.5_emissions_sector_USA_2015.svg", dpi=600/2, width=6000/300, height=3000/300)
    
#--------------------------------------------------------------- SUPPLEMENTARY FIGURE 3 -------------------------------------------------------------------
#### SM Figure 3: energy CO2 emissions ####
    #### SM3.1 downscale national sectors ####
    #Here we want to downscale emissions at the US level from:  backup_electricity, csp_backup, gas processing, and gas pipeline
    backup_electricity_states <- getQuery(prj, "inputs by sector (backup_elec, csp_backup)") #Query has backup_electricity and CSP_backup
    gas_states <- getQuery(prj, "inputs by sector (gas)") # Query has delivered gas and wholesale gas demand by state
    
    #DOWNSCALING GAS EMISSIONS
    CO2_nobio_all %>%
      filter(region == "USA",
             grepl("gas", sector)) %>%
      group_by(year, scenario, Units) %>%
      summarise(emissions_USA = sum(co2.emiss)) %>%
      ungroup() -> CO2_nobio_all_gas
    
    gas_states %>%
      filter(year > 2010 & year < 2055) %>%
      #Here input doesn't matter because flow is: inputs -> gas processing -> gas pipeline -> delivered gas & wholesale gas
      group_by(Units, scenario, year, region) %>%
      summarise(sum_state = sum(value)) %>%
      ungroup() -> gas_states_final
    
    gas_states %>%
      filter(year > 2010 & year < 2055) %>%
      group_by(Units, scenario, year) %>%
      summarise(sum_USA = sum(value)) %>%
      ungroup() -> gas_USA_final
    
    gas_states_final %>%
      left_join(gas_USA_final, by = c("Units", "scenario", "year")) %>%
      mutate(fraction = sum_state / sum_USA) %>%
      rename(Units_EJ = Units) %>%
      left_join(CO2_nobio_all_gas, by = c("scenario", "year")) %>%
      #Here we calculate the emissions in each state based on EJ of gas and emissions at uSA national level
      mutate(co2.emiss = emissions_USA * fraction,
             # We will put these emissions under the "industry" category
             sector = "gas pipeline / gas processing") %>%
      select(scenario, co2.emiss, sector, region, year, Units)-> CO2_nobio_all_gas_final #Table has total emissions by state from gas processing and gas pipeline
    
    #DOWNSCALING BACKUP ELECTRICITY EMISSIONS
    CO2_nobio_all %>%
      filter(region == "USA",
             grepl("backup", sector)) %>%
      group_by(year, scenario, Units, sector) %>%
      summarise(emissions_USA = sum(co2.emiss)) %>%
      ungroup() -> CO2_nobio_all_electricity
    
    backup_electricity_states %>%
      filter(year > 2010 & year < 2055) %>%
      #Here the input does matter, because the demand for each CSP or electricity backup is modeled separately
      group_by(Units, scenario, year, region, input) %>%
      summarise(sum_state = sum(value)) %>%
      ungroup() -> backup_electricity_states_final
    
    backup_electricity_states %>%
      filter(year > 2010 & year < 2055) %>%
      group_by(Units, scenario, year, input) %>%
      summarise(sum_USA = sum(value)) %>%
      ungroup() -> backup_electricity_USA_final
    
    backup_electricity_states_final %>%
      left_join(backup_electricity_USA_final, by = c("Units", "scenario", "year", "input")) %>%
      mutate(fraction = sum_state / sum_USA) %>%
      rename(Units_EJ = Units) %>%
      left_join(CO2_nobio_all_electricity, by = c("scenario", "year", "input" = "sector")) %>%
      #Here we calculate the emissions in each state based on EJ of gas and emissions at uSA national level
      mutate(co2.emiss = emissions_USA * fraction) %>%
      rename(sector = input) %>%
      select(scenario, co2.emiss, sector, region, year, Units)-> CO2_nobio_all_electricity_final #Table has emissions by state for csp_backup and backup_electricity
    
    #### SM3.2 merge tables ####
    ### Now we go back to original table
    CO2_nobio_all %>%
      filter(year > 2010 & year < 2055,
             #Now take out USA
             region != "USA") %>%
      bind_rows(CO2_nobio_all_gas_final) %>%
      bind_rows(CO2_nobio_all_electricity_final) %>%
      left_join(CO2_sector_mapping, by = "sector") %>%
      left_join(scenario_mapping, by = "scenario") %>%
      #Convert from MtC to MtCO2
      mutate(value = co2.emiss * emissions.CONV_C_CO2,
             Units = "MtCO2")-> CO2_nobio_all_sector
    
    #### SM3.3 create maps ####
    CO2_nobio_all_sector %>%
      filter(year == 2050) %>%
      group_by(Units, year, Scenario, region) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) %>%
      rename(state = region) -> CO2_nobio_all_sector_USA_2050_map
    
    CO2_nobio_all_sector_USA_2050_map %>%
      filter(Scenario != "Reference") -> CO2_nobio_all_sector_USA_2050_map
    
    ## Net emissions
    p <- plot_usmap(region = c("states"),
                    data = CO2_nobio_all_sector_USA_2050_map,
                    values = "sum") +
      ggtitle( "Net Energy CO2 Emissions USA in 2050 in MtCO2") +
      facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
      scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) +
      figure_theme
    ggsave("figures/paper_figures/SM3.CO2_emissions_USA_MAPS_2050.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM3.CO2_emissions_USA_MAPS_2050.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    ## Positive emissions
    CO2_nobio_all_sector %>%
      filter(value >= 0) %>%
      group_by(Units, region, year, Scenario) %>%
      summarise(sum_positive = sum(value)) %>%
      ungroup() %>%
      rename(state = region) %>%
      filter(Scenario != "Reference",
             year == 2050)-> positive_emissions
    
    p <- plot_usmap(region = c("states"),
                    data = positive_emissions,
                    values = "sum_positive") +
      ggtitle( "Positive Energy CO2 Emissions USA in 2050 in MtCO2") +
      facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
      scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) +
      figure_theme
    ggsave("figures/paper_figures/SM3.CO2_emissions_USA_MAPS_2050_positive.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM3.CO2_emissions_USA_MAPS_2050_positive.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    ## Negative emissions
    CO2_nobio_all_sector %>%
      filter(value < 0) %>%
      group_by(Units, region, year, Scenario) %>%
      summarise(sum_negative = sum(value)) %>%
      ungroup() %>%
      rename(state = region) %>%
      filter(Scenario != "Reference",
             year == 2050)-> negative_emissions
    
    p <- plot_usmap(region = c("states"),
                    data = negative_emissions,
                    values = "sum_negative") +
      ggtitle( "Negative Energy CO2 Emissions USA in 2050 in MtCO2") +
      facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
      scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) +
      figure_theme
    ggsave("figures/paper_figures/SM3.CO2_emissions_USA_MAPS_2050_negative.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM3.CO2_emissions_USA_MAPS_2050_negative.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    ## Ratio
    negative_emissions %>%
      left_join(positive_emissions, by = c("year", "state", "Units", "Scenario")) %>%
      filter(year == 2050,
             Scenario != "Reference") %>%
      mutate(ratio = (sum_negative / sum_positive)*-1) %>%
      mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> ratio_emissions
    
    p <- plot_usmap(region = c("states"),
                    data = ratio_emissions,
                    values = "ratio") +
      ggtitle( "Ratio Energy CO2 Emissions USA in 2050 in MtCO2 (negative / positive)") +
      facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +      
      scale_fill_gradient2(low = "#08A045", mid = "#FAF0CA", high = "#007EA7", midpoint = 0) +
      figure_theme
    ggsave("figures/paper_figures/SM3.CO2_emissions_USA_MAPS_2050_ratio.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM3.CO2_emissions_USA_MAPS_2050_ratio.svg", dpi=600/2, width=6000/300, height=3000/300)

#--------------------------------------------------------------- SUPPLEMENTARY FIGURE 4 & 5 -------------------------------------------------------------------
#### SM Figure 4 and 5: PM2.5 in states ####
    #### SM4.1 Sectors from energy ####
    air_pollution_source_PM2.5 %>%
      filter(Sector == "Electricity") %>%
      filter(grepl("CCS", technology)) %>%
      mutate(Sector = "Electricity BECCS") %>%
      group_by(Units, Scenario, ghg, year, Sector, region) %>%
      summarise(total_USA = sum(value)) %>%
      ungroup()-> electricity_BECCS_PM2.5_region
    
    air_pollution_source_PM2.5 %>%
      filter(Sector == "Electricity") %>%
      filter(!grepl("CCS", technology)) %>%
      group_by(Units, Scenario, ghg, year, Sector, region) %>%
      summarise(total_USA = sum(value)) %>%
      ungroup() -> electricitynoBECCS_PM2.5_region
    
    air_pollution_source_PM2.5 %>%
      filter(Sector != "Electricity") %>%
      group_by(Units, Scenario, ghg, year, Sector, region) %>%
      summarise(total_USA = sum(value)) %>%
      ungroup() %>%
      bind_rows(electricity_BECCS_PM2.5_region, electricitynoBECCS_PM2.5_region) -> air_pollution_sources_complete_PM2.5_region
    
    #### SM4.2 Resource production ####
    air_pollution_resoruce_prod %>%
      filter(year > 2010 & year < 2055,
             ghg == "PM2.5") %>%
      left_join(scenario_mapping, by = "scenario") %>%
      group_by(Units, Scenario, ghg, year, region) %>%
      summarise(total_USA = sum(value)) %>%
      ungroup() %>%
      mutate(Sector = "Other energy processing")-> air_pollution_resoruce_prod_PM2.5_region
    
    #### SM4.3 Join both tables ####
    air_pollution_sources_complete_PM2.5_region %>%
      bind_rows(air_pollution_resoruce_prod_PM2.5_region) -> air_pollution_sources_complete_PM2.5_region_final
    
    #Here filter the top 3 states with PM2.5 in 2050
    air_pollution_sources_complete_PM2.5_region_final %>%
      filter(region %in% c("TX", "CA", "IL")) %>%
      filter(Scenario != "Reference") %>%
      mutate(Sector = factor(Sector, levels = PM_sector_order))-> air_pollution_sources_complete_PM2.5_region_final_top3
    
    p <- ggplot() + geom_area(data=air_pollution_sources_complete_PM2.5_region_final_top3, aes(x=year, y=total_USA, fill= Sector)) +
      scale_fill_manual(values=pollutant_sector_color_2) +
      scale_x_continuous(breaks=seq(2010,2100,10)) +
      facet_wrap( region ~ Scenario, ncol = 2, as.table = F) +
      ggtitle( "Total PM2.5 Emissions by sector" ) +
      xlab("Year") +
      ylab("Tg") +
      figure_theme
    ggsave("figures/paper_figures/SM5.PM2.5_top3.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM5.PM2.5_top3.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    air_pollution_sources_complete_PM2.5_region_final_top3 %>%
      group_by(Scenario, year, region) %>%
      summarise(total_USA = sum(total_USA)) -> total
    
    #### SM4.4 Maps####
    #Here get totals by state
    air_pollution_sources_complete_PM2.5_region_final %>%
      filter(!region %in% c("AK", "HI")) %>% 
      group_by(Units, Scenario, ghg, year, region) %>%
      summarise(total_state = sum(total_USA)) %>%
      ungroup() %>%
      rename(state = region) %>%
      filter(Scenario != "Reference",
             year == 2050)-> air_pollution_sources_complete_PM2.5_region_totals
    
    p <- plot_usmap(region = c("state"),
                    data = air_pollution_sources_complete_PM2.5_region_totals,
                    values = "total_state") +
      ggtitle( "PM2.5 Emissions in the US in 2050 in Tg") +
      facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
      scale_fill_gradientn(
        colors = colors,
        name = "PM2.5 (ug/m3)"
      ) +
      figure_theme
    ggsave("figures/paper_figures/SM4.PM2.5_map.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM4.PM2.5_map.svg", dpi=600/2, width=6000/300, height=3000/300)
    
#--------------------------------------------------------------- SUPPLEMENTARY FIGURE 6 -------------------------------------------------------------------
#### SM Figure 6: 2019 Pollution and mortality ####
    #### SM6.1. Pollution ####
    grid_data_PM25_NAD_2019_filtered_final <- st_read("BenMAP_results/9km_V2/2019/air_quality_2019_9km/Air Quality-PM2.5-Delta_20240526.shp") %>% 
      rename(Row = ROW, Column = COL, Values = D24HourMean) %>% select(-QuarterlyMe)
    
    #Get national mean (population weighted)
    grid_data_PM25_NAD_2019_filtered_final %>%
      left_join(population_2019_data_pixel_clean, by = c("Row", "Column")) %>%
      left_join(pixel_ID, by = c("Row" = "row", "Column" = "col")) %>%
      filter(pixel_ID %in% pixels_to_keep)-> grid_data_PM25_NAD_2019_filtered_final_pop
    
    #Population weighted PM2.5
    mean_2019_popw <- with(grid_data_PM25_NAD_2019_filtered_final_pop, {
      product <- Values * Population
      sum_products <- sum(product)
      total_population <- sum(Population)
      sum_products / total_population
    })
    
    #Non-population weighted mean
    mean_2019 <- mean(grid_data_PM25_NAD_2019_filtered_final$Values)
    
    #Set values > 9 to 9 for graphing purposes
    #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
    grid_data_PM25_NAD_2019_filtered_final_pop %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_NAD_2019_filtered_graph
    
    #Count pixels with 9 or higher ug/m3 concentrations
    pol_pixels_2019 <- sum(grid_data_PM25_NAD_2019_filtered_graph$PM25_AVG_graph >= 9)
    pol_pixels_2019_list <- grid_data_PM25_NAD_2019_filtered_graph$pixel_ID[grid_data_PM25_NAD_2019_filtered_graph$PM25_AVG_graph >= 9]
    
    partitions %>%
      st_drop_geometry() %>%
      filter(pixel_ID %in% pol_pixels_2019_list) %>%
      dplyr::select(pixel_ID, pixel_area) %>%
      unique() %>%
      summarise(total_area = sum(pixel_area)) %>%
      #convert area from m2 to km2
      mutate(total_area_km2 = total_area / 10^6)-> area_above9_2019
    
    #Perform intersection to crop pixels within the boundary
    usa_boundary_transformed <- st_transform(usa_boundary, st_crs(grid_data_PM25_NAD_2019_filtered_graph))
    grid_data_PM25_NAD_2019_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_2019_filtered_graph, usa_boundary_transformed)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_NAD_2019_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(
        colors = colors,
        values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
        limits = c(0, 10),  # Adjust limits to match your data range
        name = "PM2.5 (ug/m3)"
      ) +
      ggtitle("2019 US Mean 9km Yearly PM2.5 (ug/m3)")+
      geom_text(aes(label = paste("Population weighted mean:", round(mean_2019_popw, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
      #annotation_scale() +
      theme_void() +
      geom_sf(data = usa_boundary, color = "black", fill = NA)
    ggsave("figures/paper_figures/SM6.2019_PM25_magma.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM6.2019_PM25_magma.svg", dpi=600/2, width=6000/300, height=3000/300)
    print(p)
    
    #### SM6.2. Mortality ####
    #Here we load BenMAP shapefiles with mortality results to create plots      
    benmap_2019_9km <- st_read("BenMAP_results/9km_V2/2019/health_impacts_2019_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()
    
    #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
    input_pollution_pixels %>%
      dplyr::select(Row, Column)%>%
      left_join(benmap_2019_9km, by = c("Row" = "ROW", "Column" = "COL")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      left_join(pixel_area, by = c("Row" = "row", "Column" = "col"))-> benmap_2019_9km_complete
    
    #Prepare data 
    benmap_2019_9km_complete %>% 
      mutate(deaths_per_km = Point.Estim / (pixel_area / 10^6),
             deaths_per_km_graph = if_else(deaths_per_km >= 1, 1, deaths_per_km)) %>%
      mutate(deaths_per_million = if_else(Population == 0, 0, ((Point.Estim / Population)*10^6)),
             deaths_per_million_graph = if_else(deaths_per_million >= 2000, 2000, deaths_per_million))-> benmap_2019_9km_complete_death_per_km
    
    geomtery_polygons %>%
      left_join(benmap_2019_9km_complete_death_per_km, by = c("pixel_ID", "Row", "Column"))->benmap_2019_9km_complete_death_per_km_geometry
    
    #Calculate total
    benmap_2019_9km %>%
      dplyr::summarise(mean_deaths = sum(Point.Estim), #The point estimate is the mean deaths
                       per2p5_deaths = sum(Percentile), #This is percentile 2.5
                       per97p5_deaths = sum(Percentile.19),#This is percentile 97.5
                       total_population = sum(Population)) %>% 
      mutate(Scenario = "2019 Reference 9km")-> total_mortality_2019_9km
    
    #Get state values
    benmap_2019_9km_complete_death_per_km %>%
      st_drop_geometry() %>%
      left_join(pixels_by_state, by = c("Row" = "row", "Column" = "col")) %>%
      group_by(state_ID, state_name, Region) %>%
      dplyr::summarise(Population_state = sum(Population),
                       Point.Estim_state = sum(Point.Estim)) %>%
      mutate(deaths_per_million = (Point.Estim_state / Population_state)*10^6)-> total_mortality_2019_9km_state #deaths per million in state
    
    #Figure 
    #Perform intersection to crop pixels within the boundary
    benmap_2019_9km_9km_pop_weighted_crop <- st_intersection(benmap_2019_9km_complete_death_per_km_geometry, usa_boundary)
    
    #Total deaths
    p <- ggplot() +
      geom_sf(data = benmap_2019_9km_9km_pop_weighted_crop, aes(fill = deaths_per_km_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0,1)) +  
      ggtitle("2019 Baseline US Mortality due to Mean Yearly PM2.5")+
      geom_text(data = total_mortality_2019_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/SM6.2019_mortality.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM6.2019_mortality.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    #Deaths per million
    p <- ggplot() +
      geom_sf(data = benmap_2019_9km_9km_pop_weighted_crop, aes(fill = deaths_per_million_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 2000)) +
      ggtitle("2019 Baseline US Mortality due to Mean Yearly PM2.5 (deaths per million)")+
      geom_text(data = total_mortality_2019_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/SM6.2019_mortality_per_million.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM6.2019_mortality_per_million.svg", dpi=600/2, width=6000/300, height=3000/300)
    
#--------------------------------------------------------------- SUPPLEMENTARY FIGURE 7 -------------------------------------------------------------------
#### SM Figure 7: 2050 Pollution and mortality ####
    #### SM7.1. High vs Low ####
    #POLLUTION
    grid_data_PM25_NAD_highVSlow_filtered_final <- st_read("BenMAP_results/9km_V2/2050_HIGHvsLOW/air_quality_2050_highVSlow_9km/Air Quality-PM2.5-Delta_20240526.shp") %>% 
      rename(Row = ROW, Column = COL, Values = D24HourMean) %>% select(-QuarterlyMe)
    
    #Get national mean (population weighted)
    grid_data_PM25_NAD_highVSlow_filtered_final %>%
      left_join(population_2019_data_pixel_clean, by = c("Row", "Column")) %>%
      left_join(pixel_ID, by = c("Row" = "row", "Column" = "col")) %>%
      filter(pixel_ID %in% pixels_to_keep)-> grid_data_PM25_NAD_highVSlow_filtered_final_pop
    
    #Population weighted PM2.5
    mean_highVSlow_popw <- with(grid_data_PM25_NAD_highVSlow_filtered_final_pop, {
      product <- Values * Population
      sum_products <- sum(product)
      total_population <- sum(Population)
      sum_products / total_population
    })
    
    mean_highVslow <- mean(grid_data_PM25_NAD_highVSlow_filtered_final$Values)
    
    grid_data_PM25_NAD_highVSlow_filtered_final_pop %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 2, 2, Values)) -> grid_data_PM25_NAD_highVSlow_filtered_graph
    
    #Perform intersection to crop pixels within the boundary
    usa_boundary_transformed <- st_transform(usa_boundary, st_crs(grid_data_PM25_NAD_highVSlow_filtered_graph))
    grid_data_PM25_NAD_highVSlow_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_highVSlow_filtered_graph, usa_boundary_transformed)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_NAD_highVSlow_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(
        colors = colors,
        values = scales::rescale(c(0, 1, 2)),  # Set the midpoint to 5
        limits = c(0, 2),  # Adjust limits to match your data range
        name = "PM2.5 (ug/m3)") +
      ggtitle("High VS Low US Mean 9km Yearly PM2.5 (ug/m3)")+
      geom_text(aes(label = paste("Population weighted mean:", round(mean_highVSlow_popw, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
      #annotation_scale() +
      theme_void() +
      geom_sf(data = usa_boundary, color = "black", fill = NA)
    ggsave("figures/paper_figures/SM7.highVSlow_PM25_magma.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM7.highVSlow_PM25_magma.svg", dpi=600/2, width=6000/300, height=3000/300)
    print(p)
    
    ###MORTALITY     
    benmap_highVSlow_9km <- st_read("BenMAP_results/9km_V2/2050_HIGHvsLOW/health_impact_2050_highVSlow_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()
    
    #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
    input_pollution_pixels %>%
      dplyr::select(Row, Column)%>%
      left_join(benmap_highVSlow_9km, by = c("Row" = "ROW", "Column" = "COL")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      left_join(pixel_area, by = c("Row" = "row", "Column" = "col"))-> benmap_highVSlow_9km_complete
    
    #Prepare data 
    benmap_highVSlow_9km_complete %>% 
      mutate(deaths_per_km = Point.Estim / (pixel_area / 10^6),
             deaths_per_km_graph = if_else(deaths_per_km >= 1, 1, deaths_per_km)) %>%
      mutate(deaths_per_million = if_else(Population == 0, 0, ((Point.Estim / Population)*10^6)),
             deaths_per_million_graph = if_else(deaths_per_million >= 1000, 1000, deaths_per_million))-> benmap_highVSlow_9km_complete_death_per_km
    
    geomtery_polygons %>%
      left_join(benmap_highVSlow_9km_complete_death_per_km, by = c("pixel_ID", "Row", "Column"))->benmap_highVSlow_9km_complete_death_per_km_geometry
    
    #Calculate total
    benmap_highVSlow_9km %>%
      dplyr::summarise(mean_deaths = sum(Point.Estim), #The point estimate is the mean deaths
                       per2p5_deaths = sum(Percentile), #This is percentile 2.5
                       per97p5_deaths = sum(Percentile.19),#This is percentile 97.5
                       total_population = sum(Population)) %>% 
      mutate(Scenario = "2050 High VS Low 9km")-> total_mortality_highVSlow_9km
    
    #Get state values
    benmap_highVSlow_9km_complete_death_per_km %>%
      st_drop_geometry() %>%
      left_join(pixels_by_state, by = c("Row" = "row", "Column" = "col")) %>%
      group_by(state_ID, state_name, Region) %>%
      dplyr::summarise(Population_state = sum(Population),
                       Point.Estim_state = sum(Point.Estim)) %>%
      mutate(deaths_per_million = (Point.Estim_state / Population_state)*10^6)-> total_mortality_highVSlow_9km_state #deaths per million in state
    
    #Figure 
    #Perform intersection to crop pixels within the boundary
    benmap_highVSlow_9km_9km_pop_weighted_crop <- st_intersection(benmap_highVSlow_9km_complete_death_per_km_geometry, usa_boundary)
    
    #Total deaths
    p <- ggplot() +
      geom_sf(data = benmap_highVSlow_9km_9km_pop_weighted_crop, aes(fill = deaths_per_km_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0,1)) +  
      ggtitle("High VS Low Baseline US Mortality due to Mean Yearly PM2.5")+
      geom_text(data = total_mortality_highVSlow_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/SM7.highVSlow_mortality.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM7.highVSlow_mortality.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    #Deaths per million
    p <- ggplot() +
      geom_sf(data = benmap_highVSlow_9km_9km_pop_weighted_crop, aes(fill = deaths_per_million_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1000)) +
      ggtitle("High VS Low Baseline US Mortality due to Mean Yearly PM2.5 (deaths per million)")+
      geom_text(data = total_mortality_highVSlow_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/SM7.highVSlow_mortality_per_million.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM7.highVSlow_mortality_per_million.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    #### SM7.2. Ref vs High ####
    #POLLUTION
    grid_data_PM25_NAD_refVShigh_filtered_final <- st_read("BenMAP_results/9km_V2/2050_REFvsHIGH/air_quality_2050_REFvsHIGH_9km/Air Quality-PM2.5-Delta_20240526.shp") %>% 
      rename(Row = ROW, Column = COL, Values = D24HourMean) %>% select(-QuarterlyMe)
    
    #Get national mean (population weighted)
    grid_data_PM25_NAD_refVShigh_filtered_final %>%
      left_join(population_2019_data_pixel_clean, by = c("Row", "Column")) %>%
      left_join(pixel_ID, by = c("Row" = "row", "Column" = "col")) %>%
      filter(pixel_ID %in% pixels_to_keep)-> grid_data_PM25_NAD_refVShigh_filtered_final_pop
    
    #Population weighted PM2.5
    mean_refVShigh_popw <- with(grid_data_PM25_NAD_refVShigh_filtered_final_pop, {
      product <- Values * Population
      sum_products <- sum(product)
      total_population <- sum(Population)
      sum_products / total_population
    })
    
    mean_refVShigh <- mean(grid_data_PM25_NAD_refVShigh_filtered_final$Values)
    
    grid_data_PM25_NAD_refVShigh_filtered_final_pop %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 2, 2, Values)) -> grid_data_PM25_NAD_refVShigh_filtered_graph
    
    #Perform intersection to crop pixels within the boundary
    usa_boundary_transformed <- st_transform(usa_boundary, st_crs(grid_data_PM25_NAD_refVShigh_filtered_graph))
    grid_data_PM25_NAD_refVShigh_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_refVShigh_filtered_graph, usa_boundary_transformed)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_NAD_refVShigh_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(
        colors = colors,
        values = scales::rescale(c(0, 1, 2)),  # Set the midpoint to 5
        limits = c(0, 2),  # Adjust limits to match your data range
        name = "PM2.5 (ug/m3)") +
      ggtitle("Ref VS High US Mean 9km Yearly PM2.5 (ug/m3)")+
      geom_text(aes(label = paste("Population weighted mean:", round(mean_refVShigh_popw, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
      #annotation_scale() +
      theme_void() +
      geom_sf(data = usa_boundary, color = "black", fill = NA)
    ggsave("figures/paper_figures/SM7.refVShigh_PM25_magma.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM7.refVShigh_PM25_magma.svg", dpi=600/2, width=6000/300, height=3000/300)
    print(p)
    
    ###MORTALITY     
    benmap_refVShigh_9km <- st_read("BenMAP_results/9km_V2/2050_REFvsHIGH/health_impacts_2050_REFvsHIGH_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()
    
    #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
    input_pollution_pixels %>%
      dplyr::select(Row, Column)%>%
      left_join(benmap_refVShigh_9km, by = c("Row" = "ROW", "Column" = "COL")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      left_join(pixel_area, by = c("Row" = "row", "Column" = "col"))-> benmap_refVShigh_9km_complete
    
    #Prepare data 
    benmap_refVShigh_9km_complete %>% 
      mutate(deaths_per_km = Point.Estim / (pixel_area / 10^6),
             deaths_per_km_graph = if_else(deaths_per_km >= 1, 1, deaths_per_km)) %>%
      mutate(deaths_per_million = if_else(Population == 0, 0, ((Point.Estim / Population)*10^6)),
             deaths_per_million_graph = if_else(deaths_per_million >= 1000, 1000, deaths_per_million))-> benmap_refVShigh_9km_complete_death_per_km
    
    geomtery_polygons %>%
      left_join(benmap_refVShigh_9km_complete_death_per_km, by = c("pixel_ID", "Row", "Column"))->benmap_refVShigh_9km_complete_death_per_km_geometry
    
    #Calculate total
    benmap_refVShigh_9km %>%
      dplyr::summarise(mean_deaths = sum(Point.Estim), #The point estimate is the mean deaths
                       per2p5_deaths = sum(Percentile), #This is percentile 2.5
                       per97p5_deaths = sum(Percentile.19),#This is percentile 97.5
                       total_population = sum(Population)) %>% 
      mutate(Scenario = "2050 REF VS HIGH 9km")-> total_mortality_refVShigh_9km
    
    #Get state values
    benmap_refVShigh_9km_complete_death_per_km %>%
      st_drop_geometry() %>%
      left_join(pixels_by_state, by = c("Row" = "row", "Column" = "col")) %>%
      group_by(state_ID, state_name, Region) %>%
      dplyr::summarise(Population_state = sum(Population),
                       Point.Estim_state = sum(Point.Estim)) %>%
      mutate(deaths_per_million = (Point.Estim_state / Population_state)*10^6)-> total_mortality_refVShigh_9km_state #deaths per million in state
    
    #Figure 
    #Perform intersection to crop pixels within the boundary
    benmap_refVShigh_9km_9km_pop_weighted_crop <- st_intersection(benmap_refVShigh_9km_complete_death_per_km_geometry, usa_boundary)
    
    #Total deaths
    p <- ggplot() +
      geom_sf(data = benmap_refVShigh_9km_9km_pop_weighted_crop, aes(fill = deaths_per_km_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0,1)) +  
      ggtitle("REF VS HIGH Baseline US Mortality due to Mean Yearly PM2.5")+
      geom_text(data = total_mortality_refVShigh_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/SM7.refVShigh_mortality.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM7.refVShigh_mortality.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    #Deaths per million
    p <- ggplot() +
      geom_sf(data = benmap_refVShigh_9km_9km_pop_weighted_crop, aes(fill = deaths_per_million_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1000)) +
      ggtitle("REF VS HIGH Baseline US Mortality due to Mean Yearly PM2.5 (deaths per million)")+
      geom_text(data = total_mortality_refVShigh_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/SM7.refVShigh_mortality_per_million.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM7.refVShigh_mortality_per_million.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    #### SM7.3. Ref vs Low ####
    #POLLUTION
    grid_data_PM25_NAD_refVSlow_filtered_final <- st_read("BenMAP_results/9km_V2/2050_REFvsLOW/air_quality_2050_REFvsLOW_9km/Air Quality-PM2.5-Delta_20240526.shp") %>% 
      rename(Row = ROW, Column = COL, Values = D24HourMean) %>% select(-QuarterlyMe)
    
    #Get national mean (population weighted)
    grid_data_PM25_NAD_refVSlow_filtered_final %>%
      left_join(population_2019_data_pixel_clean, by = c("Row", "Column")) %>%
      left_join(pixel_ID, by = c("Row" = "row", "Column" = "col")) %>%
      filter(pixel_ID %in% pixels_to_keep)-> grid_data_PM25_NAD_refVSlow_filtered_final_pop
    
    #Population weighted PM2.5
    mean_refVSlow_popw <- with(grid_data_PM25_NAD_refVSlow_filtered_final_pop, {
      product <- Values * Population
      sum_products <- sum(product)
      total_population <- sum(Population)
      sum_products / total_population
    })
    
    mean_refVSlow <- mean(grid_data_PM25_NAD_refVSlow_filtered_final$Values)
    
    grid_data_PM25_NAD_refVSlow_filtered_final_pop %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 2, 2, Values)) -> grid_data_PM25_NAD_refVSlow_filtered_graph
    
    #Perform intersection to crop pixels within the boundary
    usa_boundary_transformed <- st_transform(usa_boundary, st_crs(grid_data_PM25_NAD_refVSlow_filtered_graph))
    grid_data_PM25_NAD_refVSlow_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_refVSlow_filtered_graph, usa_boundary_transformed)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_NAD_refVSlow_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(
        colors = colors,
        values = scales::rescale(c(0, 1, 2)),  # Set the midpoint to 5
        limits = c(0, 2),  # Adjust limits to match your data range
        name = "PM2.5 (ug/m3)") +
      ggtitle("Ref VS Low US Mean 9km Yearly PM2.5 (ug/m3)")+
      geom_text(aes(label = paste("Population weighted mean:", round(mean_refVSlow_popw, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
      #annotation_scale() +
      theme_void() +
      geom_sf(data = usa_boundary, color = "black", fill = NA)
    ggsave("figures/paper_figures/SM7.refVSlow_PM25.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM7.refVSlow_PM25.svg", dpi=600/2, width=6000/300, height=3000/300)
    print(p)
    
    ###MORTALITY     
    benmap_refVSlow_9km <- st_read("BenMAP_results/9km_V2/2050_REFvsLOW/health_impacts_2050_REFvsLOW_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()
    
    #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
    input_pollution_pixels %>%
      dplyr::select(Row, Column)%>%
      left_join(benmap_refVSlow_9km, by = c("Row" = "ROW", "Column" = "COL")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      left_join(pixel_area, by = c("Row" = "row", "Column" = "col"))-> benmap_refVSlow_9km_complete
    
    #Prepare data 
    benmap_refVSlow_9km_complete %>% 
      mutate(deaths_per_km = Point.Estim / (pixel_area / 10^6),
             deaths_per_km_graph = if_else(deaths_per_km >= 1, 1, deaths_per_km)) %>%
      mutate(deaths_per_million = if_else(Population == 0, 0, ((Point.Estim / Population)*10^6)),
             deaths_per_million_graph = if_else(deaths_per_million >= 1000, 1000, deaths_per_million))-> benmap_refVSlow_9km_complete_death_per_km
    
    geomtery_polygons %>%
      left_join(benmap_refVSlow_9km_complete_death_per_km, by = c("pixel_ID", "Row", "Column"))->benmap_refVSlow_9km_complete_death_per_km_geometry
    
    #Calculate total
    benmap_refVSlow_9km %>%
      dplyr::summarise(mean_deaths = sum(Point.Estim), #The point estimate is the mean deaths
                       per2p5_deaths = sum(Percentile), #This is percentile 2.5
                       per97p5_deaths = sum(Percentile.19),#This is percentile 97.5
                       total_population = sum(Population)) %>% 
      mutate(Scenario = "2050 REF VS LOW 9km")-> total_mortality_refVSlow_9km
    
    #Get state values
    benmap_refVSlow_9km_complete_death_per_km %>%
      st_drop_geometry() %>%
      left_join(pixels_by_state, by = c("Row" = "row", "Column" = "col")) %>%
      group_by(state_ID, state_name, Region) %>%
      dplyr::summarise(Population_state = sum(Population),
                       Point.Estim_state = sum(Point.Estim)) %>%
      mutate(deaths_per_million = (Point.Estim_state / Population_state)*10^6)-> total_mortality_refVSlow_9km_state #deaths per million in state
    
    #Figure 
    #Perform intersection to crop pixels within the boundary
    benmap_refVSlow_9km_9km_pop_weighted_crop <- st_intersection(benmap_refVSlow_9km_complete_death_per_km_geometry, usa_boundary)
    
    #Total deaths
    p <- ggplot() +
      geom_sf(data = benmap_refVSlow_9km_9km_pop_weighted_crop, aes(fill = deaths_per_km_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0,1)) +  
      ggtitle("REF VS LOW Baseline US Mortality due to Mean Yearly PM2.5")+
      geom_text(data = total_mortality_refVSlow_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/SM7.refVSlow_mortality.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM7.refVSlow_mortality.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    #Deaths per million
    p <- ggplot() +
      geom_sf(data = benmap_refVSlow_9km_9km_pop_weighted_crop, aes(fill = deaths_per_million_graph), color = NA, size = 0) +
      scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1000)) +
      ggtitle("REF VS LOW Baseline US Mortality due to Mean Yearly PM2.5 (deaths per million)")+
      geom_text(data = total_mortality_refVSlow_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
      theme_void() +
      #annotation_scale() +
      geom_sf(data = usa_boundary, color = "black", fill = NA) 
    ggsave("figures/paper_figures/SM7.refVSlow_mortality_per_million.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM7.refVSlow_mortality_per_million.svg", dpi=600/2, width=6000/300, height=3000/300)
    
#--------------------------------------------------------------- SUPPLEMENTARY FIGURE 8, 9 & 10 -------------------------------------------------------------------
    #Figures with pollution in each city
#### 1. SEATTLE ####
    #### 1.1. 2019 ####
    grid_data_PM25_Seattle_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/Seattle/1_km_PM25_2019_seattle.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    #Note that there are NAs in latest file provided by Jing because some bordering pixels were removed. 
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_seattle %>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> seattle_pop_total
    
    grid_data_PM25_Seattle_NAD_filtered_final  %>%
      left_join(seattle_pop_total, by = c("Row", "Column"))-> seattle_pop_PM25_total
    
    # Sum up products and total population
    Seattle_mean_BASE_2019 <- weighted_average(seattle_pop_PM25_total, "Values", "population")
  
    total_population <- sum(seattle_pop_PM25_total$population) 
    mean(seattle_pop_PM25_total$Values)
    
    grid_data_PM25_Seattle_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Seattle_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Seattle_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_Seattle_NAD_filtered_graph, cbsa_shapefile_seattle)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Seattle_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 Seattle Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Seattle_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_seattle, color = "black", fill = NA)+
      theme_void() +
      ##annotation_scale() +
      geom_point(data = NULL, aes(x = -122.32987465320657, y = 47.60386428408079), shape = 1, color = "#95C623", size = 6, stroke = 2) #Seattle city hall+
    #ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Seattle.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Seattle.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 1.2. 2050 HIGH CDR ####
    grid_data_PM25_Seattle_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/Seattle/1_km_PM25_2050_HIGH_seattle.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    seattle_pop_total %>%
      left_join(grid_data_PM25_Seattle_NAD_2050_high, by = c("Row", "Column")) %>%
      filter(!is.na(Values))-> seattle_pop_PM25_total_2050_high
    
    seattle_pop_PM25_total_2050_high$product <- seattle_pop_PM25_total_2050_high$Values * seattle_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(seattle_pop_PM25_total_2050_high$product)
    total_population <- sum(seattle_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    Seattle_mean_HIGH_2050 <- sum_products / total_population 
    mean(seattle_pop_PM25_total_2050_high$Values)
    
    grid_data_PM25_Seattle_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Seattle_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Seattle_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_Seattle_NAD_2050_high_graph, cbsa_shapefile_seattle)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Seattle_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High Seattle Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Seattle_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_seattle, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -122.32987465320657, y = 47.60386428408079), shape = 1, color = "#95C623", size = 6, stroke = 2) #Seattle city hall+
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Seattle.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Seattle.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 1.3. 2050 LOW CDR ####
    grid_data_PM25_Seattle_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/Seattle/1_km_PM25_2050_LOW_seattle.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Seattle_NAD_2050_LOW %>%
      left_join(seattle_pop_total, by = c("Row", "Column")) -> seattle_pop_PM25_total_2050_LOW
    
    seattle_pop_PM25_total_2050_LOW$product <- seattle_pop_PM25_total_2050_LOW$Values * seattle_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(seattle_pop_PM25_total_2050_LOW$product)
    total_population <- sum(seattle_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    Seattle_mean_LOW_2050 <- sum_products / total_population 
    mean(seattle_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_Seattle_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Seattle_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Seattle_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_Seattle_NAD_2050_LOW_graph, cbsa_shapefile_seattle)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Seattle_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW Seattle Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Seattle_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_seattle, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -122.32987465320657, y = 47.60386428408079), shape = 1, color = "#95C623", size = 6, stroke = 2) #Seattle city hall+
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Seattle.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Seattle.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 2. LOS ANGELES ####
    #### 2.1. 2019 ####
    grid_data_PM25_LA_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/LA/1_km_PM25_2019_LA.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_LA %>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> LA_pop_total
    
    grid_data_PM25_LA_NAD_filtered_final %>%
      left_join(LA_pop_total, by = c("Row", "Column")) -> LA_pop_PM25_total
    
    LA_pop_PM25_total$product <- LA_pop_PM25_total$Values * LA_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(LA_pop_PM25_total$product)
    total_population <- sum(LA_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    LA_mean_BASE_2019 <- sum_products / total_population 
    mean(LA_pop_PM25_total$Values)
    
    grid_data_PM25_LA_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_LA_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_LA_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_LA_NAD_filtered_graph, cbsa_shapefile_LA)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_LA_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 LA Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(LA_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_LA, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -118.24271554610264,  y =  34.05368499865087), shape = 1, color = "#95C623", size = 6, stroke = 2) #LA city hall
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_LA.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_LA.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 2.2. 2050 HIGH CDR ####
    grid_data_PM25_LA_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/LA/1_km_PM25_2050_HIGH_LA.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_LA_NAD_2050_high %>%
      left_join(LA_pop_total, by = c("Row", "Column")) -> LA_pop_PM25_total_2050_high
    
    LA_pop_PM25_total_2050_high$product <- LA_pop_PM25_total_2050_high$Values * LA_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(LA_pop_PM25_total_2050_high$product)
    total_population <- sum(LA_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    LA_mean_HIGH_2050 <- sum_products / total_population 
    mean(LA_pop_PM25_total_2050_high$Values)
    
    grid_data_PM25_LA_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_LA_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_LA_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_LA_NAD_2050_high_graph, cbsa_shapefile_LA)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_LA_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High LA Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(LA_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_LA, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -118.24271554610264,  y =  34.05368499865087), shape = 1, color = "#95C623", size = 6, stroke = 2) #LA city hall
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_LA.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_LA.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 2.3. 2050 LOW CDR ####
    grid_data_PM25_LA_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/LA/1_km_PM25_2050_LOW_LA.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_LA_NAD_2050_LOW %>%
      left_join(LA_pop_total, by = c("Row", "Column")) -> LA_pop_PM25_total_2050_LOW
    
    LA_pop_PM25_total_2050_LOW$product <- LA_pop_PM25_total_2050_LOW$Values * LA_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(LA_pop_PM25_total_2050_LOW$product)
    total_population <- sum(LA_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    LA_mean_LOW_2050 <- sum_products / total_population 
    mean(LA_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_LA_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_LA_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_LA_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_LA_NAD_2050_LOW_graph, cbsa_shapefile_LA)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_LA_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW LA Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(LA_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_LA, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -118.24271554610264,  y =  34.05368499865087), shape = 1, color = "#95C623", size = 6, stroke = 2) #LA city hall
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_LA.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_LA.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 3. RIVERSIDE ####
    #### 3.1. 2019 ####
    grid_data_PM25_Riverside_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/Riverside/1_km_PM25_2019_Riverside.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_riverside %>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> Riverside_pop_total
    
    grid_data_PM25_Riverside_NAD_filtered_final %>%
      left_join(Riverside_pop_total, by = c("Row", "Column")) -> Riverside_pop_PM25_total
    
    Riverside_pop_PM25_total$product <- Riverside_pop_PM25_total$Values * Riverside_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(Riverside_pop_PM25_total$product)
    total_population <- sum(Riverside_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    Riverside_mean_BASE_2019 <- sum_products / total_population 
    mean(Riverside_pop_PM25_total$Values)
    
    grid_data_PM25_Riverside_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Riverside_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Riverside_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_Riverside_NAD_filtered_graph, cbsa_shapefile_Riverside)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Riverside_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 Riverside Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Riverside_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Riverside, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -117.37559005667082,  y =  33.980696653550375), shape = 1, color = "#95C623", size = 6, stroke = 2) #Riverside city hall
    #ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Riverside.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Riverside.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 3.2. 2050 HIGH CDR ####
    grid_data_PM25_Riverside_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/Riverside/1_km_PM25_2050_HIGH_Riverside.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
  
    grid_data_PM25_Riverside_NAD_2050_high %>%
      left_join(Riverside_pop_total, by = c("Row", "Column")) -> Riverside_pop_PM25_total_2050_high
    
    Riverside_pop_PM25_total_2050_high$product <- Riverside_pop_PM25_total_2050_high$Values * Riverside_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(Riverside_pop_PM25_total_2050_high$product)
    total_population <- sum(Riverside_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    Riverside_mean_HIGH_2050 <- sum_products / total_population 
    mean(Riverside_pop_PM25_total_2050_high$Values)
    
    grid_data_PM25_Riverside_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Riverside_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Riverside_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_Riverside_NAD_2050_high_graph, cbsa_shapefile_Riverside)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Riverside_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High Riverside Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Riverside_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Riverside, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -117.37559005667082,  y =  33.980696653550375), shape = 1, color = "#95C623", size = 6, stroke = 2) #Riverside city hall
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Riverside.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Riverside.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 3.3. 2050 LOW CDR ####
    grid_data_PM25_Riverside_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/Riverside/1_km_PM25_2050_LOW_Riverside.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))

    grid_data_PM25_Riverside_NAD_2050_LOW %>%
      left_join(Riverside_pop_total, by = c("Row", "Column")) -> Riverside_pop_PM25_total_2050_LOW
    
    Riverside_pop_PM25_total_2050_LOW$product <- Riverside_pop_PM25_total_2050_LOW$Values * Riverside_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(Riverside_pop_PM25_total_2050_LOW$product)
    total_population <- sum(Riverside_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    Riverside_mean_LOW_2050 <- sum_products / total_population 
    mean(Riverside_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_Riverside_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Riverside_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Riverside_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_Riverside_NAD_2050_LOW_graph, cbsa_shapefile_Riverside)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Riverside_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW Riverside Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Riverside_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Riverside, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -117.37559005667082,  y =  33.980696653550375), shape = 1, color = "#95C623", size = 6, stroke = 2) #Riverside city hall
    #ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Riverside.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Riverside.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### JOIN HIGH AND LOW TO CALCULATE DIFF
    grid_data_PM25_Riverside_NAD_2050_LOW %>%
      select(-Values) -> grid
    
    grid_data_PM25_Riverside_NAD_2050_high %>%
      st_drop_geometry() %>%
      rename(highCDR_PM = Values) %>%
      left_join(grid_data_PM25_Riverside_NAD_2050_LOW %>% st_drop_geometry(), by = c("pixel_ID", "Row", "Column")) %>%
      rename(lowCDR_PM = Values) %>%
      mutate(difference = highCDR_PM - lowCDR_PM) -> riverside_difference
    
    grid %>%
      left_join(riverside_difference, by = c("pixel_ID", "Row", "Column"))-> riverside_difference_geometry
    
    p <- ggplot() +
      geom_sf(data = riverside_difference_geometry, aes(fill = difference), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,
                           values = scales::rescale(c(0, 1.2, 2.5)),  # Set the midpoint to 5
                           limits = c(0, 2.5),  # Adjust limits to match your data range
                           name = "PM2.5 difference (ug/m3)"       ) +
      ggtitle("2050 HIGH minus LOW Riverside Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_sf(data = cbsa_shapefile_Riverside, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -117.37559005667082,  y =  33.980696653550375), shape = 1, color = "#95C623", size = 6, stroke = 2) #Riverside city hall
    #ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Riverside.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Riverside.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
      
    partitions_Riverside_race %>%
      mutate(percent_minority = percent_black + percent_hispanic +percent_other) %>%
      select(pixel_ID, row, col, percent_white, percent_minority)-> percent_race
    
    grid %>%
      left_join(percent_race, by = c("pixel_ID", "Row" = "row", "Column" = "col"))-> riverside_race_geometry
    
    p <- ggplot() +
      geom_sf(data = riverside_race_geometry, aes(fill = percent_minority), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,
                           values = scales::rescale(c(0, 50, 100)),  # Set the midpoint to 5
                           limits = c(0, 100),  # Adjust limits to match your data range
                           name = "Percent minority (black + hispanic + other)"       ) +
      ggtitle("Percent minority") +
      geom_sf(data = cbsa_shapefile_Riverside, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -117.37559005667082,  y =  33.980696653550375), shape = 1, color = "#95C623", size = 6, stroke = 2) #Riverside city hall
    #ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Riverside.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Riverside.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #scatter plot
    riverside_difference %>%
      left_join(percent_race, by = c("pixel_ID", "Row" = "row", "Column" = "col")) -> scatterplot_riverside
    
    ggplot(data = scatterplot_riverside, aes(x = percent_minority, y = difference)) +
      geom_point() +  # Create scatter plot
      labs(x = "Percent Minority", y = "Difference", title = "Scatterplot of Percent Minority vs Difference") +
      theme_minimal()  # Optional: Apply a clean theme
    
    # Create the scatterplot with a trendline
    ggplot(data = scatterplot_riverside, aes(x = percent_minority, y = difference)) +
      geom_point() +  # Scatter plot
      labs(x = "Percent Minority", y = "Reduction in PM2.5 (highCDR - lowCDR)", title = "Decrease in pop-weighted PM2.5 and percent minority") +
      theme_minimal()  # Apply a clean theme

    
#### 4. PHOENIX ####
    #### 4.1. 2019 ####
    grid_data_PM25_Phoenix_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/Phoenix/1_km_PM25_2019_Phoenix.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_phoenix %>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> Phoenix_pop_total
    
    grid_data_PM25_Phoenix_NAD_filtered_final %>%
      left_join(Phoenix_pop_total, by = c("Row", "Column")) -> Phoenix_pop_PM25_total
    
    Phoenix_pop_PM25_total$product <- Phoenix_pop_PM25_total$Values * Phoenix_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(Phoenix_pop_PM25_total$product)
    total_population <- sum(Phoenix_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    Phoenix_mean_BASE_2019 <- sum_products / total_population 
    mean(Phoenix_pop_PM25_total$Values)
    
    grid_data_PM25_Phoenix_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Phoenix_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Phoenix_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_Phoenix_NAD_filtered_graph, cbsa_shapefile_Phoenix)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Phoenix_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 Phoenix Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Phoenix_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Phoenix, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -112.07721684407342,  y =  33.448615125541835), shape = 1, color = "#95C623", size = 6, stroke = 2) #Phoenix city hall
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Phoenix.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Phoenix.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 4.2. 2050 HIGH CDR ####
    grid_data_PM25_Phoenix_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/Phoenix/1_km_PM25_2050_HIGH_Phoenix.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Phoenix_NAD_2050_high %>%
      left_join(Phoenix_pop_total, by = c("Row", "Column")) -> Phoenix_pop_PM25_total_2050_high
    
    Phoenix_pop_PM25_total_2050_high$product <- Phoenix_pop_PM25_total_2050_high$Values * Phoenix_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(Phoenix_pop_PM25_total_2050_high$product)
    total_population <- sum(Phoenix_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    Phoenix_mean_HIGH_2050 <- sum_products / total_population 
    mean(Phoenix_pop_PM25_total_2050_high$Values)
    
    #Set values > 9 to 9 for graphing purposes
    #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
    grid_data_PM25_Phoenix_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Phoenix_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Phoenix_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_Phoenix_NAD_2050_high_graph, cbsa_shapefile_Phoenix)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Phoenix_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High Phoenix Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Phoenix_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Phoenix, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -112.07721684407342,  y =  33.448615125541835), shape = 1, color = "#95C623", size = 6, stroke = 2) #Phoenix city hall
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Phoenix.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Phoenix.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 4.3. 2050 LOW CDR ####
    grid_data_PM25_Phoenix_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/Phoenix/1_km_PM25_2050_LOW_Phoenix.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Phoenix_NAD_2050_LOW %>%
      left_join(Phoenix_pop_total, by = c("Row", "Column")) -> Phoenix_pop_PM25_total_2050_LOW
    
    Phoenix_pop_PM25_total_2050_LOW$product <- Phoenix_pop_PM25_total_2050_LOW$Values * Phoenix_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(Phoenix_pop_PM25_total_2050_LOW$product)
    total_population <- sum(Phoenix_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    Phoenix_mean_LOW_2050 <- sum_products / total_population 
    mean(Phoenix_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_Phoenix_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Phoenix_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Phoenix_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_Phoenix_NAD_2050_LOW_graph, cbsa_shapefile_Phoenix)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Phoenix_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW Phoenix Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Phoenix_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Phoenix, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -112.07721684407342,  y =  33.448615125541835), shape = 1, color = "#95C623", size = 6, stroke = 2) #Phoenix city hall
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Phoenix.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Phoenix.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 5. DALLAS ####
    #### 5.1. 2019 ####
    grid_data_PM25_Dallas_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/Dallas/1_km_PM25_2019_Dallas.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_dallas %>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> Dallas_pop_total
    
    grid_data_PM25_Dallas_NAD_filtered_final %>%
      left_join(Dallas_pop_total, by = c("Row", "Column")) -> Dallas_pop_PM25_total
    
    Dallas_pop_PM25_total$product <- Dallas_pop_PM25_total$Values * Dallas_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(Dallas_pop_PM25_total$product)
    total_population <- sum(Dallas_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    Dallas_mean_BASE_2019 <- sum_products / total_population 
    mean(Dallas_pop_PM25_total$Values)
    
    grid_data_PM25_Dallas_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Dallas_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Dallas_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_Dallas_NAD_filtered_graph, cbsa_shapefile_Dallas)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Dallas_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 Dallas Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Dallas_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Dallas, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -96.7968013977644,  y =  32.776295519527835), shape = 1, color = "#95C623", size = 6, stroke = 2) #Dallas city hall
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Dallas.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Dallas.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 5.2. 2050 HIGH CDR ####
    grid_data_PM25_Dallas_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/Dallas/1_km_PM25_2050_HIGH_Dallas.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Dallas_NAD_2050_high %>%
      left_join(Dallas_pop_total, by = c("Row", "Column")) -> Dallas_pop_PM25_total_2050_high
    
    Dallas_pop_PM25_total_2050_high$product <- Dallas_pop_PM25_total_2050_high$Values * Dallas_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(Dallas_pop_PM25_total_2050_high$product)
    total_population <- sum(Dallas_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    Dallas_mean_HIGH_2050 <- sum_products / total_population 
    mean(Dallas_pop_PM25_total_2050_high$Values)
    
    grid_data_PM25_Dallas_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Dallas_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Dallas_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_Dallas_NAD_2050_high_graph, cbsa_shapefile_Dallas)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Dallas_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High Dallas Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Dallas_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Dallas, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -96.7968013977644,  y =  32.776295519527835), shape = 1, color = "#95C623", size = 6, stroke = 2) #Dallas city hall
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Dallas.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Dallas.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 5.3. 2050 LOW CDR ####
    grid_data_PM25_Dallas_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/Dallas/1_km_PM25_2050_LOW_Dallas.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Dallas_NAD_2050_LOW %>%
      left_join(Dallas_pop_total, by = c("Row", "Column")) -> Dallas_pop_PM25_total_2050_LOW
    
    Dallas_pop_PM25_total_2050_LOW$product <- Dallas_pop_PM25_total_2050_LOW$Values * Dallas_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(Dallas_pop_PM25_total_2050_LOW$product)
    total_population <- sum(Dallas_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    Dallas_mean_LOW_2050 <- sum_products / total_population 
    mean(Dallas_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_Dallas_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Dallas_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Dallas_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_Dallas_NAD_2050_LOW_graph, cbsa_shapefile_Dallas)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Dallas_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW Dallas Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Dallas_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Dallas, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -96.7968013977644,  y =  32.776295519527835), shape = 1, color = "#95C623", size = 6, stroke = 2) #Dallas city hall
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Dallas.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Dallas.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)

#### 6. HOUSTON ####
    #### 6.1. 2019 ####
    grid_data_PM25_Houston_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/Houston/1_km_PM25_2019_Houston.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_houston %>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> Houston_pop_total
    
    grid_data_PM25_Houston_NAD_filtered_final %>%
      left_join(Houston_pop_total, by = c("Row", "Column")) -> Houston_pop_PM25_total
    
    Houston_pop_PM25_total$product <- Houston_pop_PM25_total$Values * Houston_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(Houston_pop_PM25_total$product)
    total_population <- sum(Houston_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    Houston_mean_BASE_2019 <- sum_products / total_population 
    mean(Houston_pop_PM25_total$Values)
    
    grid_data_PM25_Houston_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Houston_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Houston_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_Houston_NAD_filtered_graph, cbsa_shapefile_Houston)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Houston_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 Houston Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Houston_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Houston, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -95.36946640065364,  y =  29.760080695254256), shape = 1, color = "#95C623", size = 6, stroke = 2) #Houston city hall
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Houston.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Houston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 6.2. 2050 HIGH CDR ####
    grid_data_PM25_Houston_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/Houston/1_km_PM25_2050_HIGH_Houston.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Houston_NAD_2050_high %>%
      left_join(Houston_pop_total, by = c("Row", "Column")) -> Houston_pop_PM25_total_2050_high
    
    Houston_pop_PM25_total_2050_high$product <- Houston_pop_PM25_total_2050_high$Values * Houston_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(Houston_pop_PM25_total_2050_high$product)
    total_population <- sum(Houston_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    Houston_mean_HIGH_2050 <- sum_products / total_population 
    mean(Houston_pop_PM25_total_2050_high$Values)
    
    grid_data_PM25_Houston_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Houston_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Houston_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_Houston_NAD_2050_high_graph, cbsa_shapefile_Houston)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Houston_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High Houston Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Houston_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Houston, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -95.36946640065364,  y =  29.760080695254256), shape = 1, color = "#95C623", size = 6, stroke = 2) #Houston city hall
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Houston.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Houston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 6.3. 2050 LOW CDR ####
    grid_data_PM25_Houston_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/Houston/1_km_PM25_2050_LOW_Houston.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Houston_NAD_2050_LOW %>%
      left_join(Houston_pop_total, by = c("Row", "Column")) -> Houston_pop_PM25_total_2050_LOW
    
    Houston_pop_PM25_total_2050_LOW$product <- Houston_pop_PM25_total_2050_LOW$Values * Houston_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(Houston_pop_PM25_total_2050_LOW$product)
    total_population <- sum(Houston_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    Houston_mean_LOW_2050 <- sum_products / total_population 
    mean(Houston_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_Houston_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Houston_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Houston_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_Houston_NAD_2050_LOW_graph, cbsa_shapefile_Houston)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Houston_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW Houston Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Houston_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Houston, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -95.36946640065364,  y =  29.760080695254256), shape = 1, color = "#95C623", size = 6, stroke = 2) #Houston city hall
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Houston.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Houston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)  
    
#### 7. MIAMI ####
    #### 7.1. 2019 ####
    grid_data_PM25_Miami_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/Miami/1_km_PM25_2019_Miami.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_miami %>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> Miami_pop_total
    
    grid_data_PM25_Miami_NAD_filtered_final %>%
      left_join(Miami_pop_total, by = c("Row", "Column")) -> Miami_pop_PM25_total
    
    Miami_pop_PM25_total$product <- Miami_pop_PM25_total$Values * Miami_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(Miami_pop_PM25_total$product)
    total_population <- sum(Miami_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    Miami_mean_BASE_2019 <- sum_products / total_population 
    mean(Miami_pop_PM25_total$Values)
    
    grid_data_PM25_Miami_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Miami_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Miami_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_Miami_NAD_filtered_graph, cbsa_shapefile_Miami)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Miami_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 Miami Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Miami_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Miami, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -80.23398527668328,  y =  25.72761157460114), shape = 1, color = "#95C623", size = 6, stroke = 2) #Miami city hall
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Miami.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Miami.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 7.2. 2050 HIGH CDR ####
    grid_data_PM25_Miami_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/Miami/1_km_PM25_2050_HIGH_Miami.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Miami_NAD_2050_high %>%
      left_join(Miami_pop_total, by = c("Row", "Column")) -> Miami_pop_PM25_total_2050_high
    
    Miami_pop_PM25_total_2050_high$product <- Miami_pop_PM25_total_2050_high$Values * Miami_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(Miami_pop_PM25_total_2050_high$product)
    total_population <- sum(Miami_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    Miami_mean_HIGH_2050 <- sum_products / total_population 
    mean(Miami_pop_PM25_total_2050_high$Values)
    
    grid_data_PM25_Miami_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Miami_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Miami_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_Miami_NAD_2050_high_graph, cbsa_shapefile_Miami)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Miami_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High Miami Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Miami_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Miami, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -80.23398527668328,  y =  25.72761157460114), shape = 1, color = "#95C623", size = 6, stroke = 2) #Miami city hall
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Miami.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Miami.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 7.3. 2050 LOW CDR ####
    grid_data_PM25_Miami_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/Miami/1_km_PM25_2050_LOW_Miami.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Miami_NAD_2050_LOW %>%
      left_join(Miami_pop_total, by = c("Row", "Column")) -> Miami_pop_PM25_total_2050_LOW
    
    Miami_pop_PM25_total_2050_LOW$product <- Miami_pop_PM25_total_2050_LOW$Values * Miami_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(Miami_pop_PM25_total_2050_LOW$product)
    total_population <- sum(Miami_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    Miami_mean_LOW_2050 <- sum_products / total_population 
    mean(Miami_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_Miami_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Miami_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Miami_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_Miami_NAD_2050_LOW_graph, cbsa_shapefile_Miami)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Miami_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW Miami Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Miami_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Miami, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -80.23398527668328,  y =  25.72761157460114), shape = 1, color = "#95C623", size = 6, stroke = 2) #Miami city hall
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Miami.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Miami.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
#### 8. ATLANTA ####
    #### 8.1. 2019 ####
    grid_data_PM25_Atlanta_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/Atlanta/1_km_PM25_2019_Atlanta.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_atl%>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> Atlanta_pop_total
    
    grid_data_PM25_Atlanta_NAD_filtered_final %>%
      left_join(Atlanta_pop_total, by = c("Row", "Column")) -> Atlanta_pop_PM25_total
    
    Atlanta_pop_PM25_total$product <- Atlanta_pop_PM25_total$Values * Atlanta_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(Atlanta_pop_PM25_total$product)
    total_population <- sum(Atlanta_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    Atlanta_mean_BASE_2019 <- sum_products / total_population 
    mean(Atlanta_pop_PM25_total$Values)
    
    grid_data_PM25_Atlanta_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Atlanta_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Atlanta_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_Atlanta_NAD_filtered_graph, cbsa_shapefile_Atlanta)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Atlanta_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 Atlanta Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Atlanta_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Atlanta, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -84.39062010249631,  y =  33.74857740874673), shape = 1, color = "#95C623", size = 6, stroke = 2) #Atlanta city hall
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Atlanta.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Atlanta.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 8.2. 2050 HIGH CDR ####
    grid_data_PM25_Atlanta_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/Atlanta/1_km_PM25_2050_HIGH_Atlanta.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Atlanta_NAD_2050_high %>%
      left_join(Atlanta_pop_total, by = c("Row", "Column")) -> Atlanta_pop_PM25_total_2050_high
    
    Atlanta_pop_PM25_total_2050_high$product <- Atlanta_pop_PM25_total_2050_high$Values * Atlanta_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(Atlanta_pop_PM25_total_2050_high$product)
    total_population <- sum(Atlanta_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    Atlanta_mean_HIGH_2050 <- sum_products / total_population 
    mean(Atlanta_pop_PM25_total_2050_high$Values)
    
    grid_data_PM25_Atlanta_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Atlanta_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Atlanta_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_Atlanta_NAD_2050_high_graph, cbsa_shapefile_Atlanta)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Atlanta_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High Atlanta Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Atlanta_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Atlanta, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -84.39062010249631,  y =  33.74857740874673), shape = 1, color = "#95C623", size = 6, stroke = 2) #Atlanta city hall
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Atlanta.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Atlanta.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 8.3. 2050 LOW CDR ####
    grid_data_PM25_Atlanta_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/Atlanta/1_km_PM25_2050_LOW_Atlanta.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
  
    grid_data_PM25_Atlanta_NAD_2050_LOW %>%
      left_join(Atlanta_pop_total, by = c("Row", "Column")) -> Atlanta_pop_PM25_total_2050_LOW
    
    Atlanta_pop_PM25_total_2050_LOW$product <- Atlanta_pop_PM25_total_2050_LOW$Values * Atlanta_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(Atlanta_pop_PM25_total_2050_LOW$product)
    total_population <- sum(Atlanta_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    Atlanta_mean_LOW_2050 <- sum_products / total_population 
    mean(Atlanta_pop_PM25_total_2050_LOW$Values)
  
    grid_data_PM25_Atlanta_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Atlanta_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Atlanta_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_Atlanta_NAD_2050_LOW_graph, cbsa_shapefile_Atlanta)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Atlanta_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW Atlanta Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Atlanta_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Atlanta, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -84.39062010249631,  y =  33.74857740874673), shape = 1, color = "#95C623", size = 6, stroke = 2) #Atlanta city hall
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Atlanta.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Atlanta.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)  

#### 9. CHICAGO ####
    #### 9.1. 2019 ####
    grid_data_PM25_Chicago_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/Chicago/1_km_PM25_2019_Chicago.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_chicago%>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> Chicago_pop_total
    
    grid_data_PM25_Chicago_NAD_filtered_final %>%
      left_join(Chicago_pop_total, by = c("Row", "Column")) -> Chicago_pop_PM25_total
    
    Chicago_pop_PM25_total$product <- Chicago_pop_PM25_total$Values * Chicago_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(Chicago_pop_PM25_total$product)
    total_population <- sum(Chicago_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    Chicago_mean_BASE_2019 <- sum_products / total_population 
    mean(Chicago_pop_PM25_total$Values)
    
    grid_data_PM25_Chicago_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Chicago_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Chicago_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_Chicago_NAD_filtered_graph, cbsa_shapefile_Chicago)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Chicago_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 Chicago Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Chicago_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Chicago, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -87.63207808921189,  y =  41.8837839226344), shape = 1, color = "#95C623", size = 6, stroke = 2) #Chicago city hall
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Chicago.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Chicago.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 9.2. 2050 HIGH CDR ####
    grid_data_PM25_Chicago_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/Chicago/1_km_PM25_2050_HIGH_Chicago.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Chicago_NAD_2050_high %>%
      left_join(Chicago_pop_total, by = c("Row", "Column")) -> Chicago_pop_PM25_total_2050_high
    
    Chicago_pop_PM25_total_2050_high$product <- Chicago_pop_PM25_total_2050_high$Values * Chicago_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(Chicago_pop_PM25_total_2050_high$product)
    total_population <- sum(Chicago_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    Chicago_mean_HIGH_2050 <- sum_products / total_population 
    mean(Chicago_pop_PM25_total_2050_high$Values)
  
    grid_data_PM25_Chicago_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Chicago_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Chicago_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_Chicago_NAD_2050_high_graph, cbsa_shapefile_Chicago)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Chicago_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High Chicago Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Chicago_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Chicago, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -87.63207808921189,  y =  41.8837839226344), shape = 1, color = "#95C623", size = 6, stroke = 2) #Chicago city hall
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Chicago.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Chicago.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 9.3. 2050 LOW CDR ####
    grid_data_PM25_Chicago_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/Chicago/1_km_PM25_2050_LOW_Chicago.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Chicago_NAD_2050_LOW %>%
      left_join(Chicago_pop_total, by = c("Row", "Column")) -> Chicago_pop_PM25_total_2050_LOW
    
    Chicago_pop_PM25_total_2050_LOW$product <- Chicago_pop_PM25_total_2050_LOW$Values * Chicago_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(Chicago_pop_PM25_total_2050_LOW$product)
    total_population <- sum(Chicago_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    Chicago_mean_LOW_2050 <- sum_products / total_population 
    mean(Chicago_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_Chicago_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Chicago_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Chicago_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_Chicago_NAD_2050_LOW_graph, cbsa_shapefile_Chicago)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Chicago_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW Chicago Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Chicago_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Chicago, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -87.63207808921189,  y =  41.8837839226344), shape = 1, color = "#95C623", size = 6, stroke = 2) #Chicago city hall
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Chicago.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Chicago.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)  
   
#### 10. DC ####
    #### 10.1. 2019 ####
    grid_data_PM25_DC_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/DC/1_km_PM25_2019_wash.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_wash_fix%>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> DC_pop_total
    
    grid_data_PM25_DC_NAD_filtered_final %>%
      left_join(DC_pop_total, by = c("Row", "Column")) -> DC_pop_PM25_total
    
    DC_pop_PM25_total$product <- DC_pop_PM25_total$Values * DC_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(DC_pop_PM25_total$product)
    total_population <- sum(DC_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    DC_mean_BASE_2019 <- sum_products / total_population 
    mean(DC_pop_PM25_total$Values)
    
    grid_data_PM25_DC_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_DC_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_DC_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_DC_NAD_filtered_graph, cbsa_shapefile_DC)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_DC_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 DC Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(DC_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_DC, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -77.03653714296149,  y =  38.89771345976116), shape = 1, color = "#95C623", size = 6, stroke = 2) #The White House
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_DC.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_DC.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 10.2. 2050 HIGH CDR ####
    grid_data_PM25_DC_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/DC/1_km_PM25_2050_high_wash.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_DC_NAD_2050_high %>%
      left_join(DC_pop_total, by = c("Row", "Column")) -> DC_pop_PM25_total_2050_high
    
    DC_pop_PM25_total_2050_high$product <- DC_pop_PM25_total_2050_high$Values * DC_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(DC_pop_PM25_total_2050_high$product)
    total_population <- sum(DC_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    DC_mean_HIGH_2050 <- sum_products / total_population 
    mean(DC_pop_PM25_total_2050_high$Values)
    
    grid_data_PM25_DC_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_DC_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_DC_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_DC_NAD_2050_high_graph, cbsa_shapefile_DC)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_DC_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High DC Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(DC_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_DC, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -77.03653714296149,  y =  38.89771345976116), shape = 1, color = "#95C623", size = 6, stroke = 2) #The White House
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_DC.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_DC.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 10.3. 2050 LOW CDR ####
    grid_data_PM25_DC_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/DC/1_km_PM25_2050_low_wash.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_DC_NAD_2050_LOW %>%
      left_join(DC_pop_total, by = c("Row", "Column")) -> DC_pop_PM25_total_2050_LOW
    
    DC_pop_PM25_total_2050_LOW$product <- DC_pop_PM25_total_2050_LOW$Values * DC_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(DC_pop_PM25_total_2050_LOW$product)
    total_population <- sum(DC_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    DC_mean_LOW_2050 <- sum_products / total_population 
    mean(DC_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_DC_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_DC_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_DC_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_DC_NAD_2050_LOW_graph, cbsa_shapefile_DC)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_DC_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW DC Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(DC_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_DC, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -77.03653714296149,  y =  38.89771345976116), shape = 1, color = "#95C623", size = 6, stroke = 2) #The White House
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_DC.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_DC.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)   
    
#### 11. Philadelphia ####
    #### 11.1. 2019 ####
    grid_data_PM25_Philadelphia_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/Philadelphia/1_km_PM25_2019_phil.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_phil_fix%>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> Philadelphia_pop_total
    
    grid_data_PM25_Philadelphia_NAD_filtered_final %>%
      left_join(Philadelphia_pop_total, by = c("Row", "Column")) -> Philadelphia_pop_PM25_total
    
    Philadelphia_pop_PM25_total$product <- Philadelphia_pop_PM25_total$Values * Philadelphia_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(Philadelphia_pop_PM25_total$product)
    total_population <- sum(Philadelphia_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    Philadelphia_mean_BASE_2019 <- sum_products / total_population 
    mean(Philadelphia_pop_PM25_total$Values)
    
    grid_data_PM25_Philadelphia_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Philadelphia_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Philadelphia_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_Philadelphia_NAD_filtered_graph, cbsa_shapefile_Philadelphia)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Philadelphia_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 Philadelphia Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Philadelphia_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Philadelphia, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -75.16326872597556,  y =  39.95279997140329), shape = 1, color = "#95C623", size = 6, stroke = 2) #Philadelphia city hall
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Philadelphia.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Philadelphia.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 11.2. 2050 HIGH CDR ####
    grid_data_PM25_Philadelphia_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/Philadelphia/1_km_PM25_2050_HIGH_phil.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Philadelphia_NAD_2050_high %>%
      left_join(Philadelphia_pop_total, by = c("Row", "Column")) -> Philadelphia_pop_PM25_total_2050_high
    
    Philadelphia_pop_PM25_total_2050_high$product <- Philadelphia_pop_PM25_total_2050_high$Values * Philadelphia_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(Philadelphia_pop_PM25_total_2050_high$product)
    total_population <- sum(Philadelphia_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    Philadelphia_mean_HIGH_2050 <- sum_products / total_population 
    mean(Philadelphia_pop_PM25_total_2050_high$Values)
    
    grid_data_PM25_Philadelphia_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Philadelphia_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Philadelphia_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_Philadelphia_NAD_2050_high_graph, cbsa_shapefile_Philadelphia)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Philadelphia_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High Philadelphia Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Philadelphia_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Philadelphia, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -75.16326872597556,  y =  39.95279997140329), shape = 1, color = "#95C623", size = 6, stroke = 2) #Philadelphia city hall
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Philadelphia.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Philadelphia.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 11.3. 2050 LOW CDR ####
    grid_data_PM25_Philadelphia_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/Philadelphia/1_km_PM25_2050_LOW_phil.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Philadelphia_NAD_2050_LOW %>%
      left_join(Philadelphia_pop_total, by = c("Row", "Column")) -> Philadelphia_pop_PM25_total_2050_LOW
    
    Philadelphia_pop_PM25_total_2050_LOW$product <- Philadelphia_pop_PM25_total_2050_LOW$Values * Philadelphia_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(Philadelphia_pop_PM25_total_2050_LOW$product)
    total_population <- sum(Philadelphia_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    Philadelphia_mean_LOW_2050 <- sum_products / total_population 
    mean(Philadelphia_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_Philadelphia_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Philadelphia_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Philadelphia_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_Philadelphia_NAD_2050_LOW_graph, cbsa_shapefile_Philadelphia)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Philadelphia_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW Philadelphia Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Philadelphia_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Philadelphia, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -75.16326872597556,  y =  39.95279997140329), shape = 1, color = "#95C623", size = 6, stroke = 2) #Philadelphia city hall
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Philadelphia.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Philadelphia.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)  
  
#### 12. NY ####
    #### 12.1. 2019 ####
    grid_data_PM25_NY_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/NY/1_km_PM25_2019_NY.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_NY_fix%>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> NY_pop_total
    
    grid_data_PM25_NY_NAD_filtered_final %>%
      left_join(NY_pop_total, by = c("Row", "Column")) %>%
      filter(!is.na(population))-> NY_pop_PM25_total
    
    NY_pop_PM25_total$product <- NY_pop_PM25_total$Values * NY_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(NY_pop_PM25_total$product)
    total_population <- sum(NY_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    NY_mean_BASE_2019 <- sum_products / total_population 
    mean(NY_pop_PM25_total$Values)
    
    grid_data_PM25_NY_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_NY_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_NY_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_NY_NAD_filtered_graph, cbsa_shapefile_NY)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_NY_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 NY Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(NY_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_NY, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -74.00583906157757,  y =  40.71266458461421), shape = 1, color = "#95C623", size = 6, stroke = 2) #New York city hall
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_NY.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_NY.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 12.2. 2050 HIGH CDR ####
    grid_data_PM25_NY_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/NY/1_km_PM25_2050_HIGH_NY.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_NY_NAD_2050_high %>%
      left_join(NY_pop_total, by = c("Row", "Column")) %>%
      filter(!is.na(population))-> NY_pop_PM25_total_2050_high
    
    NY_pop_PM25_total_2050_high$product <- NY_pop_PM25_total_2050_high$Values * NY_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(NY_pop_PM25_total_2050_high$product)
    total_population <- sum(NY_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    NY_mean_HIGH_2050 <- sum_products / total_population 
    mean(NY_pop_PM25_total_2050_high$Values)
    
    grid_data_PM25_NY_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_NY_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_NY_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_NY_NAD_2050_high_graph, cbsa_shapefile_NY)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_NY_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High NY Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(NY_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_NY, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -74.00583906157757,  y =  40.71266458461421), shape = 1, color = "#95C623", size = 6, stroke = 2) #New York city hall
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_NY.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_NY.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 12.3. 2050 LOW CDR ####
    grid_data_PM25_NY_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/NY/1_km_PM25_2050_LOW_NY.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_NY_NAD_2050_LOW %>%
      left_join(NY_pop_total, by = c("Row", "Column")) %>%
      filter(!is.na(population))-> NY_pop_PM25_total_2050_LOW
    
    NY_pop_PM25_total_2050_LOW$product <- NY_pop_PM25_total_2050_LOW$Values * NY_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(NY_pop_PM25_total_2050_LOW$product)
    total_population <- sum(NY_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    NY_mean_LOW_2050 <- sum_products / total_population 
    mean(NY_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_NY_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_NY_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_NY_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_NY_NAD_2050_LOW_graph, cbsa_shapefile_NY)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_NY_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW NY Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(NY_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_NY, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -74.00583906157757,  y =  40.71266458461421), shape = 1, color = "#95C623", size = 6, stroke = 2) #New York city hall
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_NY.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_NY.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)    
   
#### 13. BOSTON ####
    #### 13.1. 2019 ####
    grid_data_PM25_Boston_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/Boston/1_km_PM25_2019_Boston.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_boston_fix%>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> Boston_pop_total
    
    grid_data_PM25_Boston_NAD_filtered_final %>%
      left_join(Boston_pop_total, by = c("Row", "Column")) -> Boston_pop_PM25_total
    
    Boston_pop_PM25_total$product <- Boston_pop_PM25_total$Values * Boston_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(Boston_pop_PM25_total$product)
    total_population <- sum(Boston_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    Boston_mean_BASE_2019 <- sum_products / total_population 
    mean(Boston_pop_PM25_total$Values)
    
    grid_data_PM25_Boston_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Boston_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Boston_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_Boston_NAD_filtered_graph, cbsa_shapefile_Boston)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Boston_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 Boston Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Boston_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Boston, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -71.05789998431737,  y =  42.360362270351416), shape = 1, color = "#95C623", size = 6, stroke = 2) #Boston city hall
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Boston.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Boston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 13.2. 2050 HIGH CDR ####
    grid_data_PM25_Boston_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/Boston/1_km_PM25_2050_HIGH_Boston.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Boston_NAD_2050_high %>%
      left_join(Boston_pop_total, by = c("Row", "Column")) -> Boston_pop_PM25_total_2050_high
    
    Boston_pop_PM25_total_2050_high$product <- Boston_pop_PM25_total_2050_high$Values * Boston_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(Boston_pop_PM25_total_2050_high$product)
    total_population <- sum(Boston_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    Boston_mean_HIGH_2050 <- sum_products / total_population 
    mean(Boston_pop_PM25_total_2050_high$Values)
    
    grid_data_PM25_Boston_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Boston_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Boston_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_Boston_NAD_2050_high_graph, cbsa_shapefile_Boston)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Boston_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High Boston Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Boston_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Boston, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -71.05789998431737,  y =  42.360362270351416), shape = 1, color = "#95C623", size = 6, stroke = 2) #Boston city hall
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Boston.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Boston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 13.3. 2050 LOW CDR ####
    grid_data_PM25_Boston_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/Boston/1_km_PM25_2050_LOW_Boston.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Boston_NAD_2050_LOW %>%
      left_join(Boston_pop_total, by = c("Row", "Column")) -> Boston_pop_PM25_total_2050_LOW
    
    Boston_pop_PM25_total_2050_LOW$product <- Boston_pop_PM25_total_2050_LOW$Values * Boston_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(Boston_pop_PM25_total_2050_LOW$product)
    total_population <- sum(Boston_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    Boston_mean_LOW_2050 <- sum_products / total_population 
    mean(Boston_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_Boston_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Boston_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Boston_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_Boston_NAD_2050_LOW_graph, cbsa_shapefile_Boston)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Boston_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW Boston Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Boston_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Boston, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -71.05789998431737,  y =  42.360362270351416), shape = 1, color = "#95C623", size = 6, stroke = 2) #Boston city hall
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Boston.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Boston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)    
  
#### 14. DETROIT ####
    #### 14.1. 2019 ####
    grid_data_PM25_Detroit_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/Detroit/1_km_PM25_2019_Detroit.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_detroit_fix%>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> Detroit_pop_total
    
    grid_data_PM25_Detroit_NAD_filtered_final %>%
      left_join(Detroit_pop_total, by = c("Row", "Column")) -> Detroit_pop_PM25_total
    
    Detroit_pop_PM25_total$product <- Detroit_pop_PM25_total$Values * Detroit_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(Detroit_pop_PM25_total$product)
    total_population <- sum(Detroit_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    Detroit_mean_BASE_2019 <- sum_products / total_population 
    mean(Detroit_pop_PM25_total$Values)
    
    grid_data_PM25_Detroit_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Detroit_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Detroit_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_Detroit_NAD_filtered_graph, cbsa_shapefile_Detroit)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Detroit_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 Detroit Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Detroit_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Detroit, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -83.04362102880836,  y =  42.32974049721709), shape = 1, color = "#95C623", size = 6, stroke = 2) #Detroit city hall
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Detroit.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_Detroit.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 14.2. 2050 HIGH CDR ####
    grid_data_PM25_Detroit_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/Detroit/1_km_PM25_2050_HIGH_Detroit.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Detroit_NAD_2050_high %>%
      left_join(Detroit_pop_total, by = c("Row", "Column")) -> Detroit_pop_PM25_total_2050_high
    
    Detroit_pop_PM25_total_2050_high$product <- Detroit_pop_PM25_total_2050_high$Values * Detroit_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(Detroit_pop_PM25_total_2050_high$product)
    total_population <- sum(Detroit_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    Detroit_mean_HIGH_2050 <- sum_products / total_population 
    mean(Detroit_pop_PM25_total_2050_high$Values)
    
    grid_data_PM25_Detroit_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Detroit_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Detroit_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_Detroit_NAD_2050_high_graph, cbsa_shapefile_Detroit)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Detroit_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High Detroit Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Detroit_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Detroit, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -83.04362102880836,  y =  42.32974049721709), shape = 1, color = "#95C623", size = 6, stroke = 2) #Detroit city hall
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Detroit.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_Detroit.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 14.3. 2050 LOW CDR ####
    grid_data_PM25_Detroit_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/Detroit/1_km_PM25_2050_LOW_Detroit.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_Detroit_NAD_2050_LOW %>%
      left_join(Detroit_pop_total, by = c("Row", "Column")) -> Detroit_pop_PM25_total_2050_LOW
    
    Detroit_pop_PM25_total_2050_LOW$product <- Detroit_pop_PM25_total_2050_LOW$Values * Detroit_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(Detroit_pop_PM25_total_2050_LOW$product)
    total_population <- sum(Detroit_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    Detroit_mean_LOW_2050 <- sum_products / total_population 
    mean(Detroit_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_Detroit_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_Detroit_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_Detroit_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_Detroit_NAD_2050_LOW_graph, cbsa_shapefile_Detroit)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_Detroit_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW Detroit Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(Detroit_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_Detroit, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -83.04362102880836,  y =  42.32974049721709), shape = 1, color = "#95C623", size = 6, stroke = 2) #Detroit city hall
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Detroit.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_Detroit.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p) 
 
#### 15. SAN FRANCISCO ####
    #### 15.1. 2019 ####
    grid_data_PM25_SF_NAD_filtered_final<- st_read("output_data/shapefiles/1km_V2/SF/1_km_PM25_2019_SF.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    #Get national mean (population weighted)
    population_2019_data_pixel_age_final_SF_fix%>%
      group_by(Row, Column) %>%
      summarise(population = sum(Population)) %>%
      ungroup() -> SF_pop_total
    
    grid_data_PM25_SF_NAD_filtered_final %>%
      left_join(SF_pop_total, by = c("Row", "Column")) %>%
      filter(!is.na(population))-> SF_pop_PM25_total
    
    SF_pop_PM25_total$product <- SF_pop_PM25_total$Values * SF_pop_PM25_total$population
    # Sum up products and total population
    sum_products <- sum(SF_pop_PM25_total$product)
    total_population <- sum(SF_pop_PM25_total$population)
    # Calculate population-weighted average pollution
    SF_mean_BASE_2019 <- sum_products / total_population 
    mean(SF_pop_PM25_total$Values)
    
    grid_data_PM25_SF_NAD_filtered_final %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_SF_NAD_filtered_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_SF_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_SF_NAD_filtered_graph, cbsa_shapefile_SF)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_SF_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2019 SF Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(SF_mean_BASE_2019, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_SF, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -122.41894505158999,  y =  37.779200252025916), shape = 1, color = "#95C623", size = 6, stroke = 2) #San Francisco city hall
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_SF.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM8.1km_PM25_2019_plot_SF.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 15.2. 2050 HIGH CDR ####
    grid_data_PM25_SF_NAD_2050_high<- st_read("output_data/shapefiles/1km_V2/SF/1_km_PM25_2050_HIGH_SF.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_SF_NAD_2050_high %>%
      left_join(SF_pop_total, by = c("Row", "Column")) %>%
      filter(!is.na(population))-> SF_pop_PM25_total_2050_high
    
    SF_pop_PM25_total_2050_high$product <- SF_pop_PM25_total_2050_high$Values * SF_pop_PM25_total_2050_high$population
    # Sum up products and total population
    sum_products <- sum(SF_pop_PM25_total_2050_high$product)
    total_population <- sum(SF_pop_PM25_total_2050_high$population)
    # Calculate population-weighted average pollution
    SF_mean_HIGH_2050 <- sum_products / total_population 
    mean(SF_pop_PM25_total_2050_high$Values)
    
    grid_data_PM25_SF_NAD_2050_high %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_SF_NAD_2050_high_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_SF_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_SF_NAD_2050_high_graph, cbsa_shapefile_SF)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_SF_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 High SF Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(SF_mean_HIGH_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_SF, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -122.41894505158999,  y =  37.779200252025916), shape = 1, color = "#95C623", size = 6, stroke = 2) #San Francisco city hall
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_SF.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM9.1km_PM25_2050_HIGH_plot_SF.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #### 15.3. 2050 LOW CDR ####
    grid_data_PM25_SF_NAD_2050_LOW<- st_read("output_data/shapefiles/1km_V2/SF/1_km_PM25_2050_LOW_SF.shp") %>% rename(Row = row, Column = col, Values = PM25_AVG) %>% filter(!is.na(Values))
    
    grid_data_PM25_SF_NAD_2050_LOW %>%
      left_join(SF_pop_total, by = c("Row", "Column")) %>%
      filter(!is.na(population))-> SF_pop_PM25_total_2050_LOW
    
    SF_pop_PM25_total_2050_LOW$product <- SF_pop_PM25_total_2050_LOW$Values * SF_pop_PM25_total_2050_LOW$population
    # Sum up products and total population
    sum_products <- sum(SF_pop_PM25_total_2050_LOW$product)
    total_population <- sum(SF_pop_PM25_total_2050_LOW$population)
    # Calculate population-weighted average pollution
    SF_mean_LOW_2050 <- sum_products / total_population 
    mean(SF_pop_PM25_total_2050_LOW$Values)
    
    grid_data_PM25_SF_NAD_2050_LOW %>%
      filter(!is.na(Values)) %>%
      mutate(PM25_AVG_graph = if_else(Values >= 10, 10, Values)) -> grid_data_PM25_SF_NAD_2050_LOW_graph
    
    # Perform intersection to retain only pixels within the city boundary
    grid_data_PM25_SF_NAD_2050_LOW_graph_crop <- st_intersection(grid_data_PM25_SF_NAD_2050_LOW_graph, cbsa_shapefile_SF)
    
    p <- ggplot() +
      geom_sf(data = grid_data_PM25_SF_NAD_2050_LOW_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
      scale_fill_gradientn(colors = colors,         
                           values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                           limits = c(0, 10),  # Adjust limits to match your data range
                           name = "PM2.5 (ug/m3)"       ) +
      ggtitle("2050 LOW SF Mean 1km Yearly PM2.5 (ug/m3)") +
      geom_text(aes(label = paste("Population weighted mean:", round(SF_mean_LOW_2050, 2), "ug/m3")),
                x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
      geom_sf(data = cbsa_shapefile_SF, color = "black", fill = NA)+
      theme_void() +
      #annotation_scale() +
      geom_point(data = NULL, aes(x = -122.41894505158999,  y =  37.779200252025916), shape = 1, color = "#95C623", size = 6, stroke = 2) #San Francisco city hall
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_SF.png", p, width = 16, height = 12, units = "in", dpi = 600)
    ggsave("figures/paper_figures/SM10.1km_PM25_2050_LOW_plot_SF.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)    
    

#--------------------------------------------------------------- SUPPLEMENTARY FIGURE 14 -------------------------------------------------------------------
##### SM Figure 14: CO2 prices #### 
    co2_prices <- getQuery(prj, "CO2 prices")
    
    co2_prices %>%
      filter(year > 2010 & year < 2055,
             market %in% c("USACO2", "USACO2_LUC")) %>% #"USACO2_LUC", 
      mutate(price_2023_C = value * 2.33) %>% # multiplier from BLS https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1.00&year1=199012&year2=202012
      mutate(price_2023_CO2 = price_2023_C / emissions.CONV_C_CO2) %>%
      left_join(scenario_mapping, by = "scenario") %>%
      unite(Market, c("market", "Scenario"), sep = " & ", remove = FALSE) %>%
      mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> co2_prices_final
    
    p <- ggplot() + geom_line(data=co2_prices_final, aes(x=year, y=price_2023_CO2, colour = Market, group = Market), size = 1) +
      geom_text(data = co2_prices_final %>% filter(year %in% c(2030, 2050)),
                aes(x = year, y = price_2023_CO2, label = round(price_2023_CO2)),
                hjust = -0.2, vjust = 0.2, size = 5) + 
      scale_colour_manual(values = scenario_short_color) +
      ggtitle( "CO2 Prices" ) +
      xlab("Year") +
      ylab("2023$ / tCO2") +
      figure_theme
    ggsave("figures/paper_figures/SM14._CO2_price.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM14._CO2_price.svg", dpi=600/2, width=6000/300, height=3000/300)

#--------------------------------------------------------------- SUPPLEMENTARY FIGURE 15 -------------------------------------------------------------------
    USA_states <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC","FL",
                    "GA", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                    "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
                    "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
                    "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
                    "WY") #Do not include USA, AK, HI
    
    elec_prices <- getQuery(prj, "elec prices by sector")
    
    elec_prices %>%
      filter(year > 2010 & year < 2055,
             region %in% USA_states) %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario, -BECCS, -DAC, -`Policy USA`, -`Policy Global`) %>%
      mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> elec_prices_states
    
    elec_prices_states %>%
      filter(fuel == "elect_td_bld",
             year == 2050) %>%
      #Convert 1975 USD to 2023 USD -> https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator
      mutate(value = value *  2.33,
             Units = "2020$/GJ") %>%
      rename(state = region) %>%
      filter(Scenario != "Reference")-> elec_prices_states_2023USD
    
    p <- plot_usmap(region = c("states"),
                    data = elec_prices_states_2023USD,
                    values = "value") +
      ggtitle( "Electircity prices to buildings in 2050 in 2023$/GJ") +
      scale_fill_gradientn(colors = c("#edf8e9", "#bae4b3", "#74c476", "#31a354", "#006d2c"), limits = c(25, 45)) + 
      facet_wrap(~Scenario) +
      theme(panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(face="bold", size=24, hjust = 0.5),
            strip.text = element_text(size=16),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.position = "right")
    ggsave("figures/paper_figures/SM15.electricity_prices.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM15.electricity_prices.svg", dpi=600/2, width=6000/300, height=3000/300)
    
    #Now plot percent increase in Low CDR vs High CDR
    elec_prices_states %>%
      filter(fuel == "elect_td_bld",
             year == 2050) %>%
      spread(Scenario, value) %>%
      rename(state = region) %>%
      mutate(percent_increase = (`Net-zero, Low BECCS, Low DAC` - `Net-zero, High BECCS, High DAC`) / `Net-zero, High BECCS, High DAC` *100)-> buildings_percent_increase_2050
    
    p <- plot_usmap(region = c("states"),
                    data = buildings_percent_increase_2050,
                    values = "percent_increase") +
      ggtitle( "Percent increase in electircity prices to buildings in 2050") +
      scale_fill_gradientn(colors = c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"), limits = c(10, 25)) +
      theme(panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(face="bold", size=24, hjust = 0.5),
            strip.text = element_text(size=16),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.position = "right")
    ggsave("figures/paper_figures/SM15.electricity_price_increase.png", dpi=600/2, width=6000/300, height=3000/300)
    ggsave("figures/paper_figures/SM15.electricity_price_increase.svg", dpi=600/2, width=6000/300, height=3000/300)
    