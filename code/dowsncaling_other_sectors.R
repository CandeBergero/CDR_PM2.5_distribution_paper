# downscaling_other_sectors.R
# Data downscaling for other sectors
# This script is designed to downscale GCAM data for non-electricity sectors based on the National Emissions Inventory
# Date: September-October 2023 
# Author: Candelaria Bergero

#------------------------------------------------- SECTION 1: LOADING DATA -------------------------------------------------
#### 1.1. Load packages ####
library(plyr)
library(readr)
library(ggplot2)
library(devtools)
devtools::load_all("/Users/mariacandelariabergero/Documents/GCAM/rgcam-1.2.0")
library(rgcam)
library(tidyr)
library(dplyr)
library(gcamdata)
library(directlabels)
library(viridisLite)
library(usmap)
library(forcats)

#### 1.2. Load GCAM scenarios ####
#Load the project
prj <- loadProject('data/GCAM/GCAM_analysis.dat')
listScenarios(prj)
listQueries(prj)

#Get color schemes
source( "code/color_schemes.R" ) # some predefined color schemes

#### 1.3. Load mapping files ####
NEI_GCAM_mapping_nonpoint_mobile <- read_csv("mappings/NEI_GCAM_mapping_nonpoint_mobile.csv") %>% distinct
NEI_GCAM_mapping_point_source <- read_csv("mappings/NEI_GCAM_mapping_point.csv") %>% distinct 
scenario_mapping <- read_csv("mappings/scenario_mapping.csv") %>% select(scenario, Scenario)
basin_mapping <- read_csv("mappings/state_and_basin.csv") %>% rename(region = state_abbr) #File from GCAM
states_mapping <- read_csv("mappings/state_and_basin.csv") %>% select(state_abbr) %>% mutate(region ="USA") %>% unique() #File from GCAM
air_pollutant_NEI_mapping <- read_csv("mappings/mapping_air_pollution_NEI_v2.csv")
NEI_sectors_short_mapping <- read_csv("mappings/NEI_sectors_short.csv")
NEI_GCAM_mapping_point_sector <- read_csv("mappings/NEI_GCAM_mapping_point_sector.csv")
GCAM_sector_mapping <- read_csv("mappings/GCAM_sector_mapping.csv")
states_abb_mapping <- read_csv("mappings/states_abbreviation.csv")

#### 1.4. Load data files ####
#This corresponds to mobile and non-point data
NEI_CO_2020 <- read_csv("data/downscaling/NEI_CO_2020.csv")
NEI_NH3_2020 <- read_csv("data/downscaling/NEI_NH3_2020.csv")
NEI_NOx_2020 <- read_csv("data/downscaling/NEI_NOx_2020.csv")
NEI_PM_2020 <- read_csv("data/downscaling/NEI_PM_2020.csv")
NEI_SO2_2020 <- read_csv("data/downscaling/NEI_SO2_2020.csv")
NEI_VOC_2020 <- read_csv("data/downscaling/NEI_VOC_2020.csv")

NEI_nonpoint_mobile_2020 <- bind_rows(NEI_CO_2020, NEI_NH3_2020, NEI_NOx_2020, 
                                      NEI_PM_2020, NEI_SO2_2020, NEI_VOC_2020)

#This corresponds to NEI point-source data
NEI_point_source_2020 <- read_csv("data/downscaling/NEI_2020_point.csv")

#### 1.5. General parameters ####
# Conversions
EJ_MWh <- 277777777.78 #From EJ to MWh hour
# Order
scenario_short_order <- c("Reference",
                          "Net-zero, High BECCS, High DAC",
                          "Net-zero, Low BECCS, Low DAC")
#Filters
air_pollutants <- c("BC", "BC_AWB", "CO", "CO_AWB", "NH3", "NH3_AGR", "NH3_AWB", "NMVOC", "NMVOC_AGR", "NMVOC_AWB",
                    "NOx", "NOx_AGR", "NOx_AWB", "OC", "OC_AWB", "PM10", "PM2.5", "SO2_1", "SO2_1_AWB", 
                    "SO2_2", "SO2_2_AWB", "SO2_3", "SO2_3_AWB", "SO2_4", "SO2_4_AWB")

crops_filter <- c("Corn", "FiberCrop", "FodderGrass", "FodderHerb", "Fruits", "Legumes", "MiscCrop", "NutsSeeds", "OilCrop", 
                  "OtherGrain",  "Rice", "RootTuber", "Soybean", "SugarCrop", "UnmanagedLand", "Vegetables", "Wheat")

animal_filter <- c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")

point_source_GCAM_sectors_unique <- c("trn_aviation_intl", "cement", "biomass liquids") #These are three sectors that appear on our GCAM emissions that are at the point source level in NEI

point_source_GCAM_sectors_NEI <- c("trn_aviation_intl", "trn_pass", "biomass liquids", "cement", "industrial processes",
                                   "other industrial energy use", "resource production", "oil refining", "comm cooking", "comm heating",
                                   "comm hot water", "comm other", "comm cooling", "urban processes", "trn_freight",
                                   "trn_shipping_intl", "Beef", "Dairy", "Pork", "Poultry", "SheepGoat") #These are the sectors that map to NEI poit source 

PM_agg_filter <- c("BC", "OC", "PM10", "PM2.5", "BC_AWB", "OC_AWB")

coal_production_ghg <- c("CO", "NH3", "NMVOC")

animal_ghg <- c("NH3", "NMVOC", "NOx")

NEI_mobile_excluded <- c("Agriculture - Crops & Livestock Dust", "Biogenics - Vegetation and Soil", "Fires - Prescribed Fires",
                         "Dust - Paved Road Dust", "Dust - Unpaved Road Dust", "Fires - Wildfires" ) #These are the 6 EIS Sectors that we are excluding at the mobile and non-point source level. 45 remaining (of 51)

NEI_pointsource_excluded <- c("Crematory - Animal", "Crematory - Human", "Electricity Generation Geothermal", "Electricity Generation via Combustion", "Military Base") # These are the 5 sectors that are excluded. 79 remaining (of 84)

#Graph theme
my_theme <- theme(panel.background = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_line( size=.1, color="gray"),
                  panel.grid.major.y = element_line( size=.1, color="gray"),
                  plot.title = element_text(face="bold", size=24, hjust = 0.5),
                  axis.title.x = element_text(size=20),
                  axis.text.x  = element_text(size=16, vjust = 0.5, angle = 45),
                  axis.title.y = element_text(size=20),
                  axis.text.y  = element_text(size=16),
                  strip.text = element_text(size=16),
                  legend.title = element_text(size = 20),
                  legend.text = element_text(size = 20),
                  legend.position = "bottom")  

#------------------------------------------------ SECTION 2: MOBILE AND NON-POINT DATA -------------------------------------
  #In this section we will create the tables for the mobile and non-point data (at the county level)
  #We will apply GCAM's emission changes to NEI sectors.
  #Same emission changes are applied to each county.
  #Steps:
    #1. Calculate emissions by NEI sector and gas
    #2. Deal with BC and OC emissions
    #3. Calculate multipliers

#### 2.1. Here we bring in GCAM data ####
    #### 2.1.1. Non-electricity energy technology (state level) ####
    air_pollution_tech <- getQuery(prj, "air pollution nonCO2 emissions by tech") 
    #Table has air pollution in states and USA level. It includes all sectors except for resource production and electricity.
    #Resource production is dealt with below and electricity (excluded here) in downscaling_electricity.R code

    air_pollution_tech %>%
      filter(ghg %in% air_pollutants, #Keep air pollutants only
             !grepl("intl", sector)) %>% #Filter out international aviation and shipping, which are not included in NEI
      filter(region != "USA") %>% # here we take out land emissions, which we deal with later on
      filter(year > 2010 & year < 2055) %>% #Keep only 2015-2050
      filter(!sector %in% point_source_GCAM_sectors_unique) %>% #filter out sectors that are point source in NEI
      filter(subsector != "Domestic Aviation") %>% #filter out domestic aviation, which is point source subsector
      left_join(NEI_GCAM_mapping_nonpoint_mobile, by = c("sector", "subsector", "technology", "ghg"), relationship = "many-to-many") %>%
      filter(GCAM_Sector != "Transportation") %>% #Here we filter out transportation because we use a 2017 base, as opposed to 2020, because of the pandemic
      group_by(scenario, region, year, ghg, Units, `EIS Sector`, `Source Description`, `SCC LEVEL 1`, `SCC LEVEL 2`) %>%
      summarise(sum = sum(value)) %>% #This sums by EIS sector
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>% #Bring in short scenario names
      select(-scenario) -> air_pollution_sector_energy
    
    #Here we want to group BC and OC with PM2.5 and PM10 to create PM2.5_AGG and PM10_AGG
    air_pollution_sector_energy %>%
      filter(ghg %in% PM_agg_filter) %>%
      spread(ghg, sum) %>%
      #Replace NA with 0
      mutate(BC = ifelse(is.na(BC), 0, BC),
             OC = ifelse(is.na(OC), 0, OC),
             PM2.5 = ifelse(is.na(PM2.5), 0, PM2.5),
             PM10 = ifelse(is.na(PM10), 0, PM10)) %>%
      mutate(PM2.5_AGG = BC + OC + PM2.5, #Add PM2.5 + BC + OC
             PM10_AGG = BC + OC + PM10) %>% #Add PM10 + BC + OC
      select(-BC, -OC, -PM10, -PM2.5)-> air_pollution_sector_energy_PM
    
    air_pollution_sector_energy_PM %>%
      select(-PM10_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -`EIS Sector`, -`Source Description`, -`SCC LEVEL 1`, -`SCC LEVEL 2`, -Scenario) -> PM2.5_AGG
    
    air_pollution_sector_energy_PM %>%
      select(-PM2.5_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -`EIS Sector`, -`Source Description`, -`SCC LEVEL 1`, -`SCC LEVEL 2`, -Scenario) -> PM10_AGG
    
    air_pollution_sector_energy %>%
      filter(!ghg %in% c("BC", "OC")) %>%
      bind_rows(PM2.5_AGG, PM10_AGG) -> air_pollution_sector_energy_agg
    
    #Here we want to calculate multipliers, which refer to change between 2020 and 2030, and 2020 and 2050
    air_pollution_sector_energy_agg %>%
      filter(year == 2020 | year == 2030 | year == 2050) %>% #Filter base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2020` = if_else(is.na(`2020`), 1, `2020`)) %>% #Replace NAs with 1, since it is the baseline
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2030_multiplier` = `2030` / `2020`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2020`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2020`, -`2030`, -`2050`)-> air_pollution_sector_energy_final ## 34 NEI sectors
    
    #### 2.1.2. Transportation (state level) #### 
    #The COVID pandemic largely affected transportation in 2020, so here we want to use NEI 2017 base
    #We first extrapolate from GCAM from 2015 to 2020 to get 2017, and use this value for the multipliers
    air_pollution_tech %>%
      filter(ghg %in% air_pollutants,
             !grepl("intl", sector)) %>%
      filter(region != "USA") %>% 
      filter(!sector %in% point_source_GCAM_sectors_unique) %>% 
      filter(subsector != "Domestic Aviation") %>% 
      left_join(NEI_GCAM_mapping_nonpoint_mobile, by = c("sector", "subsector", "technology", "ghg"), relationship = "many-to-many") %>%
      #Here we filter for transportation because we use a 2017 base, as opposed to 2020, because of the pandemic
      filter(GCAM_Sector == "Transportation") %>%
      group_by(scenario, region, year, ghg, Units, `EIS Sector`, `Source Description`, `SCC LEVEL 1`, `SCC LEVEL 2`) %>%
      summarise(sum = sum(value)) %>% #This sums by EIS sector
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario) -> air_pollution_sector_transport
    
    #Here we want to group BC and OC with PM2.5 and PM10 to create PM2.5_AGG and PM10_AGG
    air_pollution_sector_transport %>%
      filter(ghg %in% PM_agg_filter) %>%
      spread(ghg, sum) %>%
      #Replace NA with 0
      mutate(BC = ifelse(is.na(BC), 0, BC),
             OC = ifelse(is.na(OC), 0, OC),
             PM2.5 = ifelse(is.na(PM2.5), 0, PM2.5),
             PM10 = ifelse(is.na(PM10), 0, PM10)) %>%
      mutate(PM2.5_AGG = BC + OC + PM2.5,
             PM10_AGG = BC + OC + PM10) %>%
      select(-BC, -OC, -PM10, -PM2.5)-> air_pollution_sector_transport_PM
    
    air_pollution_sector_transport_PM %>%
      select(-PM10_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -`EIS Sector`, -`Source Description`, -`SCC LEVEL 1`, -`SCC LEVEL 2`, -Scenario) -> PM2.5_AGG_transport
    
    air_pollution_sector_transport_PM %>%
      select(-PM2.5_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -`EIS Sector`, -`Source Description`, -`SCC LEVEL 1`, -`SCC LEVEL 2`, -Scenario) -> PM10_AGG_transport
    
    air_pollution_sector_transport %>%
      filter(!ghg %in% c("BC", "OC")) %>%
      bind_rows(PM2.5_AGG_transport, PM10_AGG_transport) -> air_pollution_sector_transport_agg
    
    #Create multipliers
    air_pollution_sector_transport_agg %>%
      filter(year == 2015 | year == 2020 | year == 2030 | year == 2050) %>% #Filter 2015, base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2015` = if_else(is.na(`2015`), 0, `2015`)) %>% #Replace NAs with 0
      mutate(`2020` = if_else(is.na(`2020`), 0, `2020`)) %>% #Replace NAs with 0
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2017` = `2015` + (2 * ((`2020` - `2015`) / 5))) %>% #Interpolate 2017 values
      mutate(`2017` = if_else(`2017` == 0, 1, `2017`)) %>% #Replace NAs with 1
      mutate(`2030_multiplier` = `2030` / `2017`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2017`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2015`, -`2017`, -`2020`, -`2030`, -`2050`) -> air_pollution_sector_transport_final ## 6 NEI sectors
    
    
    #### 2.1.3. Resource production (USA level) ####
    #Here we deal with air pollution form resource production (coal, gas, oil) at US level
    air_pollution_resource <- getQuery(prj, "nonCO2 emissions by resource production")
    
    air_pollution_resource %>%
      filter(ghg %in% air_pollutants,
             year > 2010 & year < 2055) %>% 
      mutate(sector = "resource production") %>%
      rename(subsector = resource, technology = subresource) %>%
      filter(subsector != "coal") %>% #We filter out coal, since it is not in NEI resource production (it is in the point soruce data)
      left_join(NEI_GCAM_mapping_nonpoint_mobile, by = c("sector", "subsector", "technology", "ghg")) %>%
      group_by(scenario, region, year, ghg, Units, `EIS Sector`, `Source Description`, `SCC LEVEL 1`, `SCC LEVEL 2`) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario) -> air_pollution_resource_pre
      
    #Here we want to group BC and OC with PM2.5 and PM10 to create PM2.5_AGG and PM10_AGG
    air_pollution_resource_pre %>%
      filter(ghg %in% PM_agg_filter) %>%
      spread(ghg, sum) %>%
      #Replace NA with 0
      mutate(BC = ifelse(is.na(BC), 0, BC),
             OC = ifelse(is.na(OC), 0, OC),
             PM2.5 = ifelse(is.na(PM2.5), 0, PM2.5),
             PM10 = ifelse(is.na(PM10), 0, PM10)) %>%
      mutate(PM2.5_AGG = BC + OC + PM2.5,
             PM10_AGG = BC + OC + PM10) %>%
      select(-BC, -OC, -PM10, -PM2.5)-> air_pollution_resource_PM
    
    air_pollution_resource_PM %>%
      select(-PM10_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -`EIS Sector`, -`Source Description`, -`SCC LEVEL 1`, -`SCC LEVEL 2`, -Scenario) -> PM2.5_AGG_resource
    
    air_pollution_resource_PM %>%
      select(-PM2.5_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -`EIS Sector`, -`Source Description`, -`SCC LEVEL 1`, -`SCC LEVEL 2`, -Scenario) -> PM10_AGG_resource
    
    air_pollution_resource_pre %>%
      filter(!ghg %in% c("BC", "OC")) %>%
      bind_rows(PM2.5_AGG_resource, PM10_AGG_resource) -> air_pollution_resource_pre_agg
    
    # Here we calculate multipliers
    air_pollution_resource_pre_agg %>%
      filter(year == 2020 | year == 2030 | year == 2050) %>% #Filter base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2020` = if_else(is.na(`2020`), 1, `2020`)) %>% #Replace NAs with 1, since it is the baseline
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2030_multiplier` = `2030` / `2020`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2020`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2020`, -`2030`, -`2050`) %>%
      #Here we rewrite this information to each state
      left_join(states_mapping, by = "region", relationship = "many-to-many") %>%
      select(-region) %>%
      rename(region = state_abbr)-> air_pollution_resource_final ## 1 NEI sectors
    
    
    #### 2.1.4. Cropland (USA / basin) ####
    air_pollution_tech %>%
      filter(ghg %in% air_pollutants,
             !grepl("intl", sector)) %>%
      filter(region == "USA",
             year > 2010 & year < 2055,
             sector != "UnmanagedLand",
             sector != "Unmanaged land")  %>% # here we deal with agriculture emissions, but we exclude land
      left_join(NEI_GCAM_mapping_nonpoint_mobile, by = c("sector", "subsector", "technology", "ghg"), relationship = "many-to-many") %>%
      filter(sector %in% crops_filter) %>% #Here we keep crops only. We deal with animals later on
      select(-region) %>%
      separate(technology, c("crop", "basin_name", "irrigation", "technology"), sep = "_") %>% #decompose basin names. 
      left_join(basin_mapping, by = "basin_name", relationship = "many-to-many") %>% #Bring in states per basin
      #Here we will add emissions per gas in the same state, which would mean double-counting of emissions. 
      #Since we do not care about total emissions, but their growth, this is fine 
      group_by(scenario, region, year, ghg, Units, `EIS Sector`, `Source Description`, `SCC LEVEL 1`, `SCC LEVEL 2`) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario) -> air_pollution_land
    
    #Note: there are no PM emissions in the land system, so we create aggregate as "PM", which equals BC + OC
    air_pollution_land %>%
      filter(ghg %in% PM_agg_filter) %>%
      spread(ghg, sum) %>%
      #Replace NA with 0
      mutate(BC_AWB = ifelse(is.na(BC_AWB), 0, BC_AWB),
             OC_AWB = ifelse(is.na(OC_AWB), 0, OC_AWB)) %>%
      mutate(PM = BC_AWB + OC_AWB) %>%
      select(-BC_AWB, -OC_AWB)-> air_pollution_land_PM
    
    air_pollution_land_PM %>%
      gather(ghg, sum, -year, -region, -Units, -`EIS Sector`, -`Source Description`, -`SCC LEVEL 1`, -`SCC LEVEL 2`, -Scenario) -> PM_AGG_land
    
    air_pollution_land %>%
      filter(!ghg %in% c("BC_AWB", "OC_AWB")) %>%
      bind_rows(PM_AGG_land) -> air_pollution_land_agg
    
    #Here we calculate multipliers
    air_pollution_land_agg %>%
      filter(year == 2020 | year == 2030 | year == 2050) %>% #Filter base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2020` = if_else(is.na(`2020`), 1, `2020`)) %>% #Replace NAs with 1, since it is the baseline
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2030_multiplier` = `2030` / `2020`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2020`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2020`, -`2030`, -`2050`)-> air_pollution_land_final ## 3 sectors
    
    #### 2.1.5. Animals (USA / basin) ####
    air_pollution_tech %>%
      filter(ghg %in% air_pollutants,
             !grepl("intl", sector)) %>%
      filter(region == "USA",
             year > 2010 & year < 2055,
             sector %in% animal_filter) %>% #Here we keep animals only
      left_join(NEI_GCAM_mapping_nonpoint_mobile, by = c("sector", "subsector", "technology", "ghg"), relationship = "many-to-many") %>%
      group_by(scenario, region, year, ghg, Units, `EIS Sector`, `Source Description`, `SCC LEVEL 1`, `SCC LEVEL 2`) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario) -> air_pollution_animal
    
    #Note: there are no PM, BC, OC emissions for animals in GCAM, so we do not create an aggregate
    #Here we calculate multipliers
    air_pollution_animal %>%
      filter(year == 2020 | year == 2030 | year == 2050) %>% #Filter base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2020` = if_else(is.na(`2020`), 1, `2020`)) %>% #Replace NAs with 1, since it is the baseline
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2030_multiplier` = `2030` / `2020`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2020`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2020`, -`2030`, -`2050`) %>%
      #Here we rewrite this information to each state
      left_join(states_mapping, by = "region", relationship = "many-to-many") %>%
      select(-region) %>%
      rename(region = state_abbr)-> air_pollution_animal_final ## 2 sectors
    

    #### 2.1.6. Residential other ####
    #Bring in residential sector other, which refers to coal. This happens in GCAM until 2005, and then disappears.
    #We use multipliers = 0
    air_pollution_sector_energy_final %>%
      select(region, Scenario) %>%
      unique() %>%
      mutate(`EIS Sector` = "Fuel Comb - Residential - Other")-> region_scenario_ghg
    
    NEI_GCAM_mapping_nonpoint_mobile %>%
      filter(`EIS Sector` == "Fuel Comb - Residential - Other") %>%
      mutate(`2030_multiplier` = 0,
             `2050_multiplier` = 0) %>%
      select(-sector, -subsector, -technology, -GCAM_Sector, -GCAM_Sector_1) %>%
      left_join(region_scenario_ghg, by = "EIS Sector", relationship = "many-to-many") %>%
      #Filter out BC and OC, which are not in NEI
      filter(!ghg %in% c("BC", "OC"))-> air_pollution_residential_other ## 1 sector
    
#### 2.2. Here we bring in sectors that are kept constant ####
    #For mobile and non-point sources these sectors are:
    #1.	Agriculture crops and livestock dust
    #2.	Biogenics – vegetation / soil
    #3.	Fires – prescribed
    #4.	Fires – Wildfires (here we will keep 2017 constant)
    #5.	Dust - Paved Road Dust
    #6.	Dust - Unpaved Road Dust
    air_pollution_sector_energy_final %>%
      left_join(air_pollutant_NEI_mapping, by = "ghg") %>%
      select(Scenario, POLLUTANT, Pollutant) %>%
      unique () -> scenario_pollutant
    
    NEI_nonpoint_mobile_2020 %>%
      filter(State != "Puerto Rico",
             State != "Virgin Islands",
             State != "-") %>%
      filter(`EIS Sector` %in% NEI_mobile_excluded) %>%
      select(State, POLLUTANT, `EIS Sector`, `Source Description`, `SCC LEVEL 1`, `SCC LEVEL 2`) %>%
      unique() %>%
      left_join(scenario_pollutant, by = "POLLUTANT", relationship = "many-to-many") %>%
      select(-POLLUTANT) %>%
      mutate(`2030_multiplier` = 1,
             `2050_multiplier` = 1)-> air_pollution_multipliers_mobile_nonpoint_final_missing ## 6 sectors
    
#### 2.3. Merge all tables ####
    air_pollution_sector_energy_final %>%
      bind_rows(air_pollution_sector_transport_final, air_pollution_resource_final, air_pollution_land_final, air_pollution_animal_final) %>% #Here we include all the other sectors (44 sectors)
      select(-Units) %>%
      bind_rows(air_pollution_residential_other) %>% #Bring in residential other (1 sector)
      left_join(air_pollutant_NEI_mapping, by = "ghg") %>%
      left_join(states_abb_mapping, by = "region") %>%
      select(-ghg, -region) %>%
      bind_rows(air_pollution_multipliers_mobile_nonpoint_final_missing) %>% #Bring in sector held constant (6 sectors)
      select(State, Scenario, Pollutant, `EIS Sector`, `Source Description`, `SCC LEVEL 1`, `SCC LEVEL 2`, `2030_multiplier`, `2050_multiplier`)-> air_pollution_multipliers_mobile_nonpoint_final # FINAL TABLE (51/51 sectors)

#### 2.4. Here we print output tables ####
    air_pollution_multipliers_mobile_nonpoint_final %>%
      select(-`2050_multiplier`) -> air_pollution_multipliers_mobile_nonpoint_final_2030
    
    air_pollution_multipliers_mobile_nonpoint_final %>%
      select(-`2030_multiplier`) -> air_pollution_multipliers_mobile_nonpoint_final_2050
    
    # Get unique scenarios
    scenarios <- unique(air_pollution_multipliers_mobile_nonpoint_final$Scenario)
    
    # # Loop through each scenario for 2030
    # for (scenario in scenarios) {
    #   # Filter data for the current scenario
    #   filtered_data <- air_pollution_multipliers_mobile_nonpoint_final_2030 %>%
    #     filter(Scenario == scenario)
    #   
    #   # Define the file name
    #   file_name <- paste("output_data/downscaling/mobile_nonpoint_2030_", scenario, ".csv", sep = "")
    #   
    #   # Save the filtered data as a CSV file
    #   write.csv(filtered_data, file_name, row.names = FALSE)
    # }
    
    # Loop through each scenario for 2050
    for (scenario in scenarios) {
      # Filter data for the current scenario
      filtered_data <- air_pollution_multipliers_mobile_nonpoint_final_2050 %>%
        filter(Scenario == scenario)
      
      # Define the file name
      file_name <- paste("output_data/downscaling/mobile_nonpoint_2050_", scenario, ".csv", sep = "")
      
      # Save the filtered data as a CSV file
      write.csv(filtered_data, file_name, row.names = FALSE)
    }
    

#----------------------------------------------- SECTION 3: PROCESSING NEI POINT SOURCE DATA ----------------------------------
  #In this section we will create the tables for point source data (at the facility level)
  #We will apply GCAM's emission changes to NEI sectors.
  #Same emission changes are applied to each longitude and latitude.
  #Steps:
    #1. Calculate emissions by NEI sector and gas
    #2. Deal with BC and OC emissions
    #3. Calculate multipliers
    
#### 3.1. Here we deal with each sector individually ####
    #### 3.1.1. Industry: energy use + processes ####
    air_pollution_tech %>%
      filter(year > 2010 & year < 2055) %>% #filter years of interest
      filter(sector %in% c("other industrial energy use", "industrial processes")) %>%
      mutate(Sector = "Industry (energy + processes)") %>%
      group_by(Units, scenario,region, Sector, ghg, year) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario)-> air_pollution_tech_industrial
    
    #Here we aggregate BC and OC to create PMagg
    air_pollution_tech_industrial %>%
      filter(ghg %in% PM_agg_filter) %>%
      spread(ghg, sum) %>%
      #Replace NA with 0
      mutate(BC = ifelse(is.na(BC), 0, BC),
             OC = ifelse(is.na(OC), 0, OC),
             PM2.5 = ifelse(is.na(PM2.5), 0, PM2.5),
             PM10 = ifelse(is.na(PM10), 0, PM10)) %>%
      mutate(PM2.5_AGG = BC + OC + PM2.5, #Add PM2.5 + BC + OC
             PM10_AGG = BC + OC + PM10) %>% #Add PM10 + BC + OC
      select(-BC, -OC, -PM10, -PM2.5)-> air_pollution_tech_industrial_PMagg
    
    air_pollution_tech_industrial_PMagg %>%
      select(-PM10_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM2.5_AGG_industry
    
    air_pollution_tech_industrial_PMagg %>%
      select(-PM2.5_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM10_AGG_industry
    
    air_pollution_tech_industrial %>%
      filter(!ghg %in% c("BC", "OC")) %>%
      bind_rows(PM2.5_AGG_industry, PM10_AGG_industry) %>%
      left_join(NEI_GCAM_mapping_point_source, by = c("Sector", "ghg"), relationship = "many-to-many")-> air_pollution_tech_industrial_PM
    
    #Now calculate multipliers
    air_pollution_tech_industrial_PM %>%
      filter(year == 2020 | year == 2030 | year == 2050) %>% #Filter base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2020` = if_else(is.na(`2020`), 1, `2020`)) %>% #Replace NAs with 1, since it is the baseline
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2030_multiplier` = `2030` / `2020`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2020`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2020`, -`2030`, -`2050`)-> air_pollution_tech_industrial_final #FINAL (58 facility types)
      
    #### 3.1.2. Aviation ####
    air_pollution_tech %>%
      filter(year > 2010 & year < 2055) %>% 
      filter(subsector == "Domestic Aviation") %>% #We exclude international aviation because NEI does not have it
      mutate(Sector = "Aviation") %>%
      group_by(Units, scenario,region, Sector, ghg, year) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario)-> air_pollution_tech_airport
    
    #Here we aggregate BC and OC to create PMagg
    air_pollution_tech_airport %>%
      filter(ghg %in% PM_agg_filter) %>%
      spread(ghg, sum) %>%
      #Replace NA with 0
      mutate(BC = ifelse(is.na(BC), 0, BC),
             OC = ifelse(is.na(OC), 0, OC),
             PM2.5 = ifelse(is.na(PM2.5), 0, PM2.5),
             PM10 = ifelse(is.na(PM10), 0, PM10)) %>%
      mutate(PM2.5_AGG = BC + OC + PM2.5, #Add PM2.5 + BC + OC
             PM10_AGG = BC + OC + PM10) %>% #Add PM10 + BC + OC
      select(-BC, -OC, -PM10, -PM2.5)-> air_pollution_tech_airport_PMagg
    
    air_pollution_tech_airport_PMagg %>%
      select(-PM10_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM2.5_AGG_airport
    
    air_pollution_tech_airport_PMagg %>%
      select(-PM2.5_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM10_AGG_airport
    
    air_pollution_tech_airport %>%
      filter(!ghg %in% c("BC", "OC")) %>%
      bind_rows(PM2.5_AGG_airport, PM10_AGG_airport) %>%
      left_join(NEI_GCAM_mapping_point_source, by = c("Sector", "ghg"), relationship = "many-to-many")-> air_pollution_tech_airport_PM
    
    #Now calculate multipliers. Transportation is based in 2017 because of the pandemic
    air_pollution_tech_airport_PM %>%
      filter(year == 2015 | year == 2020 | year == 2030 | year == 2050) %>% #Filter 2015, base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2015` = if_else(is.na(`2015`), 0, `2015`)) %>% #Replace NAs with 0
      mutate(`2020` = if_else(is.na(`2020`), 0, `2020`)) %>% #Replace NAs with 0
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2017` = `2015` + (2 * ((`2020` - `2015`) / 5))) %>% #Interpolate 2017 values
      mutate(`2017` = if_else(`2017` == 0, 1, `2017`)) %>% #Replace NAs with 1
      mutate(`2030_multiplier` = `2030` / `2017`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2017`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2015`, -`2017`, -`2020`, -`2030`, -`2050`) -> air_pollution_tech_airport_final #FINAL (1 facility type)
    
    #### 3.1.3. Cement ####
    air_pollution_tech %>%
      filter(year > 2010 & year < 2055) %>% #filter years of interest
      filter(sector %in% c("cement")) %>%
      mutate(Sector = "Cement") %>%
      group_by(Units, scenario,region, Sector, ghg, year) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario) %>%
      left_join(NEI_GCAM_mapping_point_source, by = c("Sector", "ghg"), relationship = "many-to-many")-> air_pollution_tech_cement
    
    #Now calculate multipliers
    air_pollution_tech_cement %>%
      filter(year == 2020 | year == 2030 | year == 2050) %>% #Filter base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2020` = if_else(is.na(`2020`), 1, `2020`)) %>% #Replace NAs with 1, since it is the baseline
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2030_multiplier` = `2030` / `2020`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2020`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2020`, -`2030`, -`2050`)-> air_pollution_tech_cement_final #FINAL (3 facility types)
    
    #### 3.1.4. Refining (biomass) ####
    air_pollution_tech %>%
      filter(year > 2010 & year < 2055) %>% #filter years of interest
      filter(sector == "biomass liquids") %>%
      mutate(Sector = "Refining (biomass)") %>%
      group_by(Units, scenario,region, Sector, ghg, year) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario) %>%
      left_join(NEI_GCAM_mapping_point_source, by = c("Sector", "ghg"), relationship = "many-to-many")-> air_pollution_tech_bioliquids
    
    #Now calculate multipliers
    air_pollution_tech_bioliquids %>%
      filter(year == 2020 | year == 2030 | year == 2050) %>% #Filter base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2020` = if_else(is.na(`2020`), 1, `2020`)) %>% #Replace NAs with 1, since it is the baseline
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2030_multiplier` = `2030` / `2020`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2020`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2020`, -`2030`, -`2050`)-> air_pollution_tech_bioliquids_final #FINAL (1 facility type)
    
    #### 3.1.5. Resource production ####
    #Natural gas only
    air_pollution_resource %>%  
      filter(resource == "natural gas") %>%
      mutate(Sector ="Resource production (gas)")-> air_pollution_resource_gas
    
    #Natural gas + oil
    air_pollution_resource %>%
      filter(resource %in% c("natural gas", "crude oil")) %>%
      mutate(Sector = "Resource production (gas and oil)")-> air_pollution_resource_gas_oil
    
    #Coal
    #Note that coal resource production only has CO, NH3 and NMVOC. For missing pollutants we use 
    air_pollution_resource %>%
      filter(resource == "coal") %>%
      mutate(Sector ="Resource production (coal)")-> air_pollution_resource_coal
    
    #Join tables
    air_pollution_resource_gas %>%
      bind_rows(air_pollution_resource_gas_oil, air_pollution_resource_coal) %>%
      filter(year > 2010 & year < 2055,
             ghg %in% air_pollutants) %>%
      group_by(Units, scenario, region, ghg, year, Sector) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario) -> air_pollution_resource_complete
    
    #Here we aggregate BC and OC to create PMagg
    air_pollution_resource_complete %>%
      filter(ghg %in% PM_agg_filter) %>%
      spread(ghg, sum) %>%
      #Replace NA with 0
      mutate(BC = ifelse(is.na(BC), 0, BC),
             OC = ifelse(is.na(OC), 0, OC),
             PM2.5 = ifelse(is.na(PM2.5), 0, PM2.5),
             PM10 = ifelse(is.na(PM10), 0, PM10)) %>%
      mutate(PM2.5_AGG = BC + OC + PM2.5, #Add PM2.5 + BC + OC
             PM10_AGG = BC + OC + PM10) %>% #Add PM10 + BC + OC
      select(-BC, -OC, -PM10, -PM2.5)-> air_pollution_resource_complete_PMagg
    
    air_pollution_resource_complete_PMagg %>%
      select(-PM10_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM2.5_AGG_resource_point
    
    air_pollution_resource_complete_PMagg %>%
      select(-PM2.5_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM10_AGG_resource_point
    
    air_pollution_resource_complete %>%
      filter(!ghg %in% c("BC", "OC")) %>%
      bind_rows(PM2.5_AGG_resource_point, PM10_AGG_resource_point) %>%
      left_join(NEI_GCAM_mapping_point_source, by = c("Sector", "ghg"), relationship = "many-to-many")-> air_pollution_resource_complete_PM
    
    #Now calculate multipliers
    air_pollution_resource_complete_PM %>%
      filter(year == 2020 | year == 2030 | year == 2050) %>% #Filter base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2020` = if_else(is.na(`2020`), 1, `2020`)) %>% #Replace NAs with 1, since it is the baseline
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2030_multiplier` = `2030` / `2020`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2020`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2020`, -`2030`, -`2050`)-> air_pollution_resource_complete_final
    
    #Coal multipliers are almost identical for all three gases, so we apply multipliers from one of them to the missing ones
    air_pollution_resource_complete_final %>%
      select(ghg, Scenario) %>%
      filter(!ghg %in% coal_production_ghg) %>%
      unique() -> pollutants
    
    air_pollution_resource_complete_final %>%
      filter(Sector == "Resource production (coal)",
             ghg == "CO") %>%
      select(-ghg) %>%
      unique()%>%
      left_join(pollutants, by = "Scenario") %>%
      bind_rows(air_pollution_resource_complete_final) %>%
      left_join(states_mapping, by = "region", relationship = "many-to-many") %>%
      select(-region) %>%
      rename(region = state_abbr) -> air_pollution_resource_complete_final_corrected #FINAL (5 facility types)
    
    #### 3.1.6. Refining (oil) ####
    air_pollution_tech %>%
      filter(year > 2010 & year < 2055) %>% #filter years of interest
      filter(sector %in% c("oil refining")) %>%
      mutate(Sector = "Refining (oil)") %>%
      group_by(Units, scenario,region, Sector, ghg, year) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario) %>%
      left_join(NEI_GCAM_mapping_point_source, by = c("Sector", "ghg"), relationship = "many-to-many")-> air_pollution_tech_refining
    
    #Now calculate multipliers
    air_pollution_tech_refining %>%
      filter(year == 2020 | year == 2030 | year == 2050) %>% #Filter base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2020` = if_else(is.na(`2020`), 1, `2020`)) %>% #Replace NAs with 1, since it is the baseline
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2030_multiplier` = `2030` / `2020`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2020`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2020`, -`2030`, -`2050`)-> air_pollution_tech_refining_final #FINAL (3 faility types)
    
    #### 3.1.7. Commercial ####
    air_pollution_tech %>%
      filter(year > 2010 & year < 2055) %>% #filter years of interest
      filter(sector %in% c("comm cooking", "comm cooling", "comm heating", "comm hot water", "comm other" )) %>%
      mutate(Sector = "Commercial") %>%
      group_by(Units, scenario,region, Sector, ghg, year) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario) -> air_pollution_tech_commercial

    #Here we aggregate BC and OC to create PMagg
    air_pollution_tech_commercial %>%
      filter(ghg %in% PM_agg_filter) %>%
      spread(ghg, sum) %>%
      #Replace NA with 0
      mutate(BC = ifelse(is.na(BC), 0, BC),
             OC = ifelse(is.na(OC), 0, OC),
             PM2.5 = ifelse(is.na(PM2.5), 0, PM2.5),
             PM10 = ifelse(is.na(PM10), 0, PM10)) %>%
      mutate(PM2.5_AGG = BC + OC + PM2.5, #Add PM2.5 + BC + OC
             PM10_AGG = BC + OC + PM10) %>% #Add PM10 + BC + OC
      select(-BC, -OC, -PM10, -PM2.5)-> air_pollution_tech_commercial_PMagg
    
    air_pollution_tech_commercial_PMagg %>%
      select(-PM10_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM2.5_AGG_commercial
    
    air_pollution_tech_commercial_PMagg %>%
      select(-PM2.5_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM10_AGG_commercial
    
    air_pollution_tech_commercial %>%
      filter(!ghg %in% c("BC", "OC")) %>%
      bind_rows(PM2.5_AGG_commercial, PM10_AGG_commercial) %>%
      left_join(NEI_GCAM_mapping_point_source, by = c("Sector", "ghg"), relationship = "many-to-many")-> air_pollution_tech_commercial_PM
    
    #Now calculate multipliers
    air_pollution_tech_commercial_PM %>%
      filter(year == 2020 | year == 2030 | year == 2050) %>% #Filter base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2020` = if_else(is.na(`2020`), 1, `2020`)) %>% #Replace NAs with 1, since it is the baseline
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2030_multiplier` = `2030` / `2020`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2020`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2020`, -`2030`, -`2050`)-> air_pollution_tech_commercial_final #FINAL (1 facility type)
      
    #### 3.1.8. Urban Processes ####
    air_pollution_tech %>%
      filter(year > 2010 & year < 2055) %>% #filter years of interest
      filter(sector %in% c("urban processes")) %>%
      mutate(Sector = "Urban Processes") %>%
      group_by(Units, scenario,region, Sector, ghg, year) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario) -> air_pollution_tech_urban_processes
    
    #Here we aggregate BC and OC to create PMagg
    air_pollution_tech_urban_processes %>%
      filter(ghg %in% PM_agg_filter) %>%
      spread(ghg, sum) %>%
      #Replace NA with 0
      mutate(BC = ifelse(is.na(BC), 0, BC),
             OC = ifelse(is.na(OC), 0, OC),
             PM2.5 = ifelse(is.na(PM2.5), 0, PM2.5),
             PM10 = ifelse(is.na(PM10), 0, PM10)) %>%
      mutate(PM2.5_AGG = BC + OC + PM2.5, #Add PM2.5 + BC + OC
             PM10_AGG = BC + OC + PM10) %>% #Add PM10 + BC + OC
      select(-BC, -OC, -PM10, -PM2.5)-> air_pollution_tech_urban_processes_PMagg
    
    air_pollution_tech_urban_processes_PMagg %>%
      select(-PM10_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM2.5_AGG_urban
    
    air_pollution_tech_urban_processes_PMagg %>%
      select(-PM2.5_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM10_AGG_urban
    
    air_pollution_tech_urban_processes %>%
      filter(!ghg %in% c("BC", "OC")) %>%
      bind_rows(PM2.5_AGG_urban, PM10_AGG_urban) %>%
      left_join(NEI_GCAM_mapping_point_source, by = c("Sector", "ghg"), relationship = "many-to-many")-> air_pollution_tech_urban_processes_PM
    
    #Now calculate multipliers
    air_pollution_tech_urban_processes_PM %>%
      filter(year == 2020 | year == 2030 | year == 2050) %>% #Filter base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2020` = if_else(is.na(`2020`), 1, `2020`)) %>% #Replace NAs with 1, since it is the baseline
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2030_multiplier` = `2030` / `2020`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2020`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2020`, -`2030`, -`2050`)-> air_pollution_tech_urban_processes_final #FINAL (3 facility types)
    
    #### 3.1.9. Ship ####
    air_pollution_tech %>%
      filter(year > 2010 & year < 2055) %>% #filter years of interest
      filter(subsector == "Domestic Ship") %>% #We exclude international ship because NEI does not have it
      mutate(Sector = "Ships") %>%
      group_by(Units, scenario,region, Sector, ghg, year) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario)-> air_pollution_tech_ship
    
    #Here we aggregate BC and OC to create PMagg
    air_pollution_tech_ship %>%
      filter(ghg %in% PM_agg_filter) %>%
      spread(ghg, sum) %>%
      #Replace NA with 0
      mutate(BC = ifelse(is.na(BC), 0, BC),
             OC = ifelse(is.na(OC), 0, OC),
             PM2.5 = ifelse(is.na(PM2.5), 0, PM2.5),
             PM10 = ifelse(is.na(PM10), 0, PM10)) %>%
      mutate(PM2.5_AGG = BC + OC + PM2.5, #Add PM2.5 + BC + OC
             PM10_AGG = BC + OC + PM10) %>% #Add PM10 + BC + OC
      select(-BC, -OC, -PM10, -PM2.5)-> air_pollution_tech_ship_PMagg
    
    air_pollution_tech_ship_PMagg %>%
      select(-PM10_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM2.5_AGG_ship
    
    air_pollution_tech_ship_PMagg %>%
      select(-PM2.5_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM10_AGG_ship
    
    air_pollution_tech_ship %>%
      filter(!ghg %in% c("BC", "OC")) %>%
      bind_rows(PM2.5_AGG_ship, PM10_AGG_ship) %>%
      left_join(NEI_GCAM_mapping_point_source, by = c("Sector", "ghg"), relationship = "many-to-many")-> air_pollution_tech_ship_PM
    
    #Now calculate multipliers. Transportation is based in 2017 because of the pandemic
    air_pollution_tech_ship_PM %>%
      filter(year == 2015 | year == 2020 | year == 2030 | year == 2050) %>% #Filter 2015, base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2015` = if_else(is.na(`2015`), 0, `2015`)) %>% #Replace NAs with 0
      mutate(`2020` = if_else(is.na(`2020`), 0, `2020`)) %>% #Replace NAs with 0
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2017` = `2015` + (2 * ((`2020` - `2015`) / 5))) %>% #Interpolate 2017 values
      mutate(`2017` = if_else(`2017` == 0, 1, `2017`)) %>% #Replace NAs with 1
      mutate(`2030_multiplier` = `2030` / `2017`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2017`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2015`, -`2017`, -`2020`, -`2030`, -`2050`) -> air_pollution_tech_ship_final #FINAL (1 facility type)
    
    #### 3.1.10. Rail ####
    air_pollution_tech %>%
      filter(year > 2010 & year < 2055) %>% #filter years of interest
      filter(subsector %in% c("Freight Rail", "Passenger Rail")) %>%
      mutate(Sector = "Rail") %>%
      group_by(Units, scenario,region, Sector, ghg, year) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario)-> air_pollution_tech_rail
    
    #Here we aggregate BC and OC to create PMagg
    air_pollution_tech_rail %>%
      filter(ghg %in% PM_agg_filter) %>%
      spread(ghg, sum) %>%
      #Replace NA with 0
      mutate(BC = ifelse(is.na(BC), 0, BC),
             OC = ifelse(is.na(OC), 0, OC),
             PM2.5 = ifelse(is.na(PM2.5), 0, PM2.5),
             PM10 = ifelse(is.na(PM10), 0, PM10)) %>%
      mutate(PM2.5_AGG = BC + OC + PM2.5, #Add PM2.5 + BC + OC
             PM10_AGG = BC + OC + PM10) %>% #Add PM10 + BC + OC
      select(-BC, -OC, -PM10, -PM2.5)-> air_pollution_tech_rail_PMagg
    
    air_pollution_tech_rail_PMagg %>%
      select(-PM10_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM2.5_AGG_rail
    
    air_pollution_tech_rail_PMagg %>%
      select(-PM2.5_AGG) %>%
      gather(ghg, sum, -year, -region, -Units, -Sector, -Scenario) -> PM10_AGG_rail
    
    air_pollution_tech_rail %>%
      filter(!ghg %in% c("BC", "OC")) %>%
      bind_rows(PM2.5_AGG_rail, PM10_AGG_rail) %>%
      left_join(NEI_GCAM_mapping_point_source, by = c("Sector", "ghg"), relationship = "many-to-many")-> air_pollution_tech_rail_PM
    
    #Now calculate multipliers. Transportation is based in 2017 because of the pandemic
    air_pollution_tech_rail_PM %>%
      filter(year == 2015 | year == 2020 | year == 2030 | year == 2050) %>% #Filter 2015, base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2015` = if_else(is.na(`2015`), 0, `2015`)) %>% #Replace NAs with 0
      mutate(`2020` = if_else(is.na(`2020`), 0, `2020`)) %>% #Replace NAs with 0
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2017` = `2015` + (2 * ((`2020` - `2015`) / 5))) %>% #Interpolate 2017 values
      mutate(`2017` = if_else(`2017` == 0, 1, `2017`)) %>% #Replace NAs with 1
      mutate(`2030_multiplier` = `2030` / `2017`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2017`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2015`, -`2017`, -`2020`, -`2030`, -`2050`) -> air_pollution_tech_rail_final #FINAL (1 facility type)
    
    #### 3.1.11. Animals ####
    air_pollution_tech %>%
      filter(year > 2010 & year < 2055) %>% #filter years of interest
      filter(sector %in% animal_filter) %>%
      mutate(Sector = "Animals") %>%
      group_by(Units, scenario,region, Sector, ghg, year) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-scenario) %>%
      left_join(NEI_GCAM_mapping_point_source, by = c("Sector", "ghg"), relationship = "many-to-many")-> air_pollution_tech_animal
    
    #Now calculate multipliers
    air_pollution_tech_animal %>%
      filter(year == 2020 | year == 2030 | year == 2050) %>% #Filter base year (2020), and two future periods of interest (2030 and 2050).
      spread(year, sum) %>%
      mutate(`2020` = if_else(is.na(`2020`), 1, `2020`)) %>% #Replace NAs with 1, since it is the baseline
      mutate(`2030` = if_else(is.na(`2030`), 0, `2030`)) %>% #Replace NAs with 0
      mutate(`2050` = if_else(is.na(`2050`), 0, `2050`)) %>% #Replace NAs with 0
      mutate(`2030_multiplier` = `2030` / `2020`) %>% #Here we will have to multiply NEI 2020 values times 2030_multiplier to get 2030 emissions
      mutate(`2050_multiplier` = `2050` / `2020`) %>%
      mutate(`2030_multiplier` = if_else(is.nan(`2030_multiplier`), 0, `2030_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      mutate(`2050_multiplier` = if_else(is.nan(`2050_multiplier`), 0, `2050_multiplier`)) %>% # 0/0 yields NaN, so we replace with 0
      select(-`2020`, -`2030`, -`2050`)-> air_pollution_tech_animal_final 
    
    #Animal multipliers are almost identical for all three gases, so we apply multipliers from one of them to the missing ones
    air_pollution_tech_rail_final %>%
      select(ghg, Scenario) %>%
      filter(!ghg %in% animal_ghg) %>%
      unique() -> pollutants_animal
    
    air_pollution_tech_animal_final %>%
      filter(ghg == "NOx_AGR") %>%
      select(-ghg) %>%
      unique() %>%
      left_join(pollutants_animal, by = "Scenario") %>%
      bind_rows(air_pollution_tech_animal_final) %>%
      left_join(states_mapping, by = "region", relationship = "many-to-many") %>%
      select(-region) %>%
      rename(region = state_abbr) -> air_pollution_tech_animal_final_corrected #FINAL (1 facility type)
    
    #### 3.1.12. Coal gasification ####
    #Here we create this sector with multipliers = 0
    scenarios <- unique(air_pollution_tech_industrial_final$Scenario)
    air_pollutants_small <- unique(air_pollution_tech_industrial_final$ghg)
    
    # Create a data frame using the matrix and add row and column names
    df <- data.frame(Scenario = rep(scenarios, each = length(air_pollutants_small)),
                     ghg = rep(air_pollutants_small, times = length(scenarios)))
    
    df %>%
      mutate(`Facility Type` = "Coal Gasification Plant",
             Sector = "Coal gasification",
             `2030_multiplier` = 0,
             `2050_multiplier` = 0,
             region = "USA") %>%
      left_join(states_mapping, by = "region", relationship = "many-to-many") %>%
      select(-region) %>%
      rename(region = state_abbr)-> air_pollution_tech_coal_gasification_final #FINAL (1 facility type)
                     
    
#### 3.2. Here we bring in sectors that are kept constant ####
    #For point sources these sectors are:
    #1.Crematory - Animal                  
    #2.Crematory - Human (here we will keep 2017 constant)                     
    #3.Electricity Generation Geothermal (excluded because we downscale somewhere else)     
    #4.Electricity Generation via Combustion (excluded because we downscale somewhere else)     
    #5. Military Base 
    NEI_point_source_2020 %>%
      filter(STATE != "Puerto Rico",
             STATE != "Virgin Islands",
             STATE != "-") %>%
      filter(`Facility Type` %in% NEI_pointsource_excluded,
             !grepl("Electricity", `Facility Type`)) %>% #Exclude electricity sectors since we deal with them individually
      rename(State = STATE) %>%
      select(State, Pollutant, `Facility Type`) %>%
      unique() %>%
      rename(POLLUTANT = Pollutant)%>%
      left_join(scenario_pollutant, by = "POLLUTANT", relationship = "many-to-many") %>%
      mutate(`2030_multiplier` = 1,
             `2050_multiplier` = 1) %>%
      select(-POLLUTANT)-> air_pollution_multipliers_point_final_missing ## 3 facility types
    
    
#### 3.3. Here we merge all tables ####
    air_pollution_tech_industrial_final %>%
      bind_rows(air_pollution_tech_airport_final, air_pollution_tech_cement_final, air_pollution_tech_bioliquids_final,
                air_pollution_resource_complete_final_corrected, air_pollution_tech_refining_final, air_pollution_tech_commercial_final,
                air_pollution_tech_urban_processes_final, air_pollution_tech_ship_final, air_pollution_tech_rail_final, 
                air_pollution_tech_animal_final_corrected, air_pollution_tech_coal_gasification_final) %>%
      left_join(air_pollutant_NEI_mapping, by = "ghg") %>%
      left_join(states_abb_mapping, by = "region") %>%
      select(-Sector, -ghg, -POLLUTANT, -Units, -region) %>%
      bind_rows(air_pollution_multipliers_point_final_missing)-> air_pollution_multipliers_point_final ## FINAL 82/84 facility types
    #Note: the 2 missing facility types refer to electricity generation and we have calculated those emissions in downscaling_electricity.R
    
#### 3.4. Here we print output tables ####
    air_pollution_multipliers_point_final %>%
      select(-`2050_multiplier`) -> air_pollution_multipliers_point_final_2030
    
    air_pollution_multipliers_point_final %>%
      select(-`2030_multiplier`) -> air_pollution_multipliers_point_final_2050
    
    # Get unique scenarios
    scenarios <- unique(air_pollution_multipliers_point_final$Scenario)
    
    # Loop through each scenario for 2030
    # for (scenario in scenarios) {
    #   # Filter data for the current scenario
    #   filtered_data <- air_pollution_multipliers_point_final_2030 %>%
    #     filter(Scenario == scenario)
    #   
    #   # Define the file name
    #   file_name <- paste("output_data/downscaling/point_2030_", scenario, ".csv", sep = "")
    #   
    #   # Save the filtered data as a CSV file
    #   write.csv(filtered_data, file_name, row.names = FALSE)
    # }
    
    # Loop through each scenario for 2050
    for (scenario in scenarios) {
      # Filter data for the current scenario
      filtered_data <- air_pollution_multipliers_point_final_2050 %>%
        filter(Scenario == scenario)
      
      # Define the file name
      file_name <- paste("output_data/downscaling/point_2050_", scenario, ".csv", sep = "")
      
      # Save the filtered data as a CSV file
      write.csv(filtered_data, file_name, row.names = FALSE)
    }
  
#----------------------------------------------- SECTION 4: PREPARE NEI DATA ------------------------------------------------
#### 4.1. Mobile and point source ####
    #### 4.1.1. All sectors ####
    #Here we process the data for mobile and non-point emissions from the county level dataset
    #https://awsedap.epa.gov/public/single/?appid=20230c40-026d-494e-903f-3f112761a208&sheet=5d3fdda7-14bc-4284-a9bb-cfd856b9348d&opt=ctxmenu,currsel
    NEI_nonpoint_mobile_2020 %>%
      #Filter out regions not included in GCAM
      filter(State != "-",
             State != "Puerto Rico",
             State != "Virgin Islands") -> NEI_pollutants_2020_mobile_nonpoint
    
    NEI_pollutants_2020_mobile_nonpoint %>%
      mutate(`Emissions (Tons)` = if_else(is.na(`Emissions (Tons)`), 0, `Emissions (Tons)`)) %>% #Replace few NAs in SO2 with 0
      group_by(POLLUTANT, `EIS Sector`) %>%
      summarise(USA_total = sum(`Emissions (Tons)`)) %>%
      ungroup() %>%
      #Pollution is in short tons -> 1 US ton = 0.9071847 metric ton
      mutate(USA_total = (USA_total * 0.9071847) / 10^6,
             Units = "Tg") %>%
      spread(POLLUTANT, USA_total)-> NEI_pollutants_2020_USA_sector #51 EIS sectors 
    
    NEI_pollutants_2020_mobile_nonpoint %>%
      mutate(`Emissions (Tons)` = if_else(is.na(`Emissions (Tons)`), 0, `Emissions (Tons)`)) %>% #Replace few NAs in SO2 with 0
      group_by(POLLUTANT) %>%
      summarise(USA_total = sum(`Emissions (Tons)`)) %>%
      ungroup() %>%
      mutate(USA_total = (USA_total * 0.9071847) / 10^6,
             Units = "Tg") -> NEI_pollutants_2020_USA_total #Total at US level
    
    #### 4.1.2. Filtered sectors ####
    #Here we calculate emissions after filtering out sectors not included in GCAM
    NEI_GCAM_mapping_nonpoint_mobile %>%
      select(GCAM_Sector, `EIS Sector`) %>%
      unique() %>%
      #Filter out mobile non-road equipment diesel because it creates duplicates here, and we do not want that
      filter(!(GCAM_Sector == "Agriculture" & `EIS Sector` == "Mobile - Non-Road Equipment - Diesel"))-> NEI_GCAM_mapping_nonpoint_mobile_short
    
    NEI_pollutants_2020_mobile_nonpoint %>%
      filter(!`EIS Sector` %in% NEI_mobile_excluded) %>%
      group_by(State, POLLUTANT, `EIS Sector`) %>%
      summarise(emissions = sum(`Emissions (Tons)`)) %>%
      ungroup() %>%
      mutate(emissions = (emissions * 0.9071847) / 10^6,
             Units = "Tg") %>%
      left_join(NEI_GCAM_mapping_nonpoint_mobile_short, by = c("EIS Sector")) %>%
      group_by(State, POLLUTANT, Units, GCAM_Sector) %>%
      summarise(emissions = sum(emissions)) %>%
      ungroup()-> NEI_pollutants_2020_mobile_nonpoint_states #By GCAM sector and state, without excluded sectors
    
    NEI_pollutants_2020_mobile_nonpoint_states %>%
      group_by(POLLUTANT, Units, GCAM_Sector) %>%
      summarise(emissions = sum(emissions)) %>%
      ungroup() %>%
      rename(Pollutant = POLLUTANT) -> NEI_pollutants_2020_mobile_nonpoint_USA #FINAL filtered
    
    #Double check values are consistent with totals
    NEI_pollutants_2020_mobile_nonpoint_USA %>%
      group_by(Pollutant) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> double_check #Consistent
    
    
#### 4.2. Point source emissions ####
    #### 4.2.1. Excluded ####
    #Here we process NEI data at the facility level, which refers to point source emissions
    #https://awsedap.epa.gov/public/single/?appid=20230c40-026d-494e-903f-3f112761a208&sheet=5d3fdda7-14bc-4284-a9bb-cfd856b9348d&opt=ctxmenu,currsel 
    NEI_point_source_2020 %>%
      filter(STATE != "-",
             STATE != "Puerto Rico",
             STATE != "Virgin Islands") -> NEI_pollutants_2020_mobile
    
    #Point source emissions: NEI_pollutants_2020_mobile
    NEI_pollutants_2020_mobile %>%
      filter(!`Facility Type` %in% NEI_pointsource_excluded) %>%
      group_by(STATE, Pollutant, `Facility Type`) %>%
      summarise(emissions = sum(`Emissions (Tons)`)) %>%
      ungroup() %>%
      mutate(emissions = (emissions * 0.9071847) / 10^6,
             Units = "Tg") %>%
      left_join(NEI_GCAM_mapping_point_sector, by = "Facility Type")-> NEI_pollutants_2020_point_states
      
    NEI_pollutants_2020_point_states %>%
      group_by(Pollutant, Units, GCAM_Sector) %>%
      summarise(emissions = sum(emissions)) %>%
      ungroup() -> NEI_pollutants_2020_point_USA #FINAL filtered
    
    #Double check numbers are correct
    NEI_pollutants_2020_point_USA %>%
      group_by(Pollutant) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> double_check #Consistent
    
#### 4.3. Merge both tables at national level ####
    NEI_pollutants_2020_mobile_nonpoint_USA %>%
      bind_rows(NEI_pollutants_2020_point_USA) %>%
      group_by(Pollutant, GCAM_Sector, Units) %>%
      summarise(emissions = sum(emissions)) %>%
      ungroup() %>%
      mutate(year = 2020)-> NEI_2020_emissions_complete_USA ### FINAL (complete without excluded sectors)
    
    NEI_2020_emissions_complete_USA %>%
      group_by(Pollutant) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> double_check #Consistent
    
#### 4.4. Graph NEI emissions by our sectors ####
    p <- ggplot(NEI_2020_emissions_complete_USA, aes(x = Pollutant, y = emissions)) +
      geom_col(aes(fill = GCAM_Sector, group = GCAM_Sector)) +
      scale_fill_manual(values = GCAM_sectors_colors) +
      ggtitle(" 2020 NEI Emissions") +
      xlab("Sector") +
      ylab("Tg")+
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/0.national_emissions_NEI.png", dpi=600/2, width=6000/300, height=3000/300)
    
#----------------------------------------------- SECTION 5: GRAPHS  -------------------------------------------------------      
#### 5.1. Prepare GCAM data ####
    #Prepare GCAM data with emissions
    air_pollution_tech %>%
      filter(year > 2010 & year < 2055,
             sector != "UnmanagedLand",
             sector != "Unmanaged land",
             !grepl("intl", sector)) %>% #Filter out unmanaged land, which has emissions from deforestation and forest fires. Filter put international aviation and international shipping
      group_by(Units, sector, ghg, scenario, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      left_join(GCAM_sector_mapping, by = c("sector", "ghg")) -> air_pollution_tech_graphs 
    
    
    #Here we create PM-agg 
    air_pollution_tech_graphs %>%
      filter(ghg %in% c("BC", "OC", "BC_AWB", "OC_AWB")) %>%
      spread(ghg, value) %>%
      #Replace NA with 0
      mutate(BC = ifelse(is.na(BC), 0, BC),
             OC = ifelse(is.na(OC), 0, OC),
             BC_AWB = ifelse(is.na(BC_AWB), 0, BC_AWB),
             OC_AWB = ifelse(is.na(OC_AWB), 0, OC_AWB)) %>%
      mutate(PM_AGG = BC + OC + BC_AWB + OC_AWB) %>% #BC + OC
      select(-BC, -OC, -BC_AWB, -OC_AWB) %>%
      gather(ghg, value, -year, -Units, -sector, -scenario, -GCAM_Sector) -> air_pollution_tech_graphs_PMagg
    
    air_pollution_tech_graphs %>%
      filter(!ghg %in% c("BC", "OC", "BC_AWB", "OC_AWB")) %>%
      bind_rows(air_pollution_tech_graphs_PMagg) -> air_pollution_tech_graphs_complete #11 sectors
    
    #Here we add the resource production sectors, which is in another query
    air_pollution_resource %>%
      filter(year > 2010 & year < 2055,
             ghg %in% air_pollutants) %>%
      mutate(GCAM_Sector = "Resource production") %>%
      rename(sector = resource) %>%
      select(Units, scenario, ghg, year, value, GCAM_Sector, sector) -> air_pollution_resource_graphs
    
    air_pollution_resource_graphs %>%
      filter(ghg %in% c("BC", "OC")) %>%
      spread(ghg, value) %>%
      #Replace NA with 0
      mutate(BC = ifelse(is.na(BC), 0, BC),
             OC = ifelse(is.na(OC), 0, OC)) %>%
      mutate(PM_AGG = BC + OC) %>% #Add PM10 + BC + OC
      select(-BC, -OC,) %>%
      gather(ghg, value, -year, -Units, -sector, -scenario, -GCAM_Sector) -> air_pollution_resource_graphs_PMagg
    
    air_pollution_resource_graphs %>%
      filter(!ghg %in% c("BC", "OC")) %>%
      bind_rows(air_pollution_resource_graphs_PMagg, air_pollution_tech_graphs_complete) %>%
      left_join(scenario_mapping, by = "scenario") %>%
      left_join(air_pollutant_NEI_mapping, by = "ghg") %>%
      mutate(Pollutant = if_else(ghg == "PM_AGG", "BC + OC", POLLUTANT)) %>%
      select(-scenario, -ghg)  -> air_pollution_tech_graphs_final #12 sectors
    
    #Each sector will be graphed below
#### 5.2. National level by sector 2020 ####
    air_pollution_tech_graphs_final %>%
      filter(year == 2020,
             Scenario == "Reference") %>% 
      group_by(Units, Scenario, Pollutant, GCAM_Sector, year) %>%
      summarise(emissions = sum(value)) %>%
      ungroup() -> air_pollution_tech_graphs_final_sector

    p <- ggplot(air_pollution_tech_graphs_final_sector, aes(x = Pollutant, y = emissions)) +
      geom_col(aes(fill = GCAM_Sector, group = GCAM_Sector)) +
      scale_fill_manual(values = GCAM_sectors_colors) +
      ggtitle("2020 GCAM Reference Emissions") +
      xlab("Sector") +
      ylab("Tg")+
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/5.2.national_emissions_GCAM.png", dpi=600/2, width=6000/300, height=3000/300)
    
    #Here we compare NEI and GCAM side by side, by gas and sector
    air_pollution_tech_graphs_final_sector %>%
      select(-Scenario) %>%
      mutate(Data = "GCAM") %>%
      bind_rows(NEI_2020_emissions_complete_USA %>% mutate(Data = "NEI")) -> pollution_2020_GCAM_NEI
    
    p <- ggplot(pollution_2020_GCAM_NEI, aes(x = Data, y = emissions)) +
      geom_col(aes(fill = GCAM_Sector, group = GCAM_Sector)) +
      scale_fill_manual(values = GCAM_sectors_colors) +
      facet_wrap(facets = ~fct_reorder(Pollutant, emissions, .desc = TRUE), scales = "free_y") +
      ggtitle("2020 GCAM & NEI Emissions") +
      xlab("Sector") +
      ylab("Tg")+
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/5.2.national_emissions_GCAM_NEI_comparison.png", dpi=600/2, width=6000/300, height=3000/300)
    
#### 5.3. National level ####
    air_pollution_tech_graphs_final %>%
      group_by(Units, Scenario, Pollutant, year) %>%
      summarise(total = sum(value)) %>%
      ungroup() -> air_pollution_tech_graphs_final_total
    
    NEI_2020_emissions_complete_USA %>%
      group_by(Pollutant, Units, year) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> NEI_2020_emissions_complete_USA_total
    
    #Line graph per gas
    p <- ggplot() + geom_line(data=air_pollution_tech_graphs_final_total, aes(x=year, y=total, colour = Scenario, group = Scenario), size = 1.5) +
      geom_point(data = NEI_2020_emissions_complete_USA_total, aes(x = year, y = total)) +
      scale_colour_manual(values = scenario_short_color) +
      expand_limits(y = 0) +
      facet_wrap(facets = ~fct_reorder(Pollutant, total, .desc = TRUE), scales = "free_y") +
      ggtitle( "Air Pollution by Gas in the US" ) +
      theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
      xlab("Year") +
      ylab("Tg") +
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/5.3.national_gases_NEI_GCAM.png", dpi=600/2, width=6000/300, height=3000/300)
    
#### 5.4. Resource production ####
    air_pollution_tech_graphs_final %>%
      filter(GCAM_Sector == "Resource production") %>%
      group_by(Units, year, GCAM_Sector, Scenario, Pollutant) %>%
      summarise(total = sum(value)) %>%
      ungroup() -> graph_resource_production
    
    NEI_2020_emissions_complete_USA %>%
      filter(GCAM_Sector == "Resource production") %>%
      group_by(Units, year, GCAM_Sector, Pollutant) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> graph_resource_production_NEI
    
    p <- ggplot() + geom_line(data=graph_resource_production, aes(x=year, y=total, colour = Scenario, group = Scenario), size = 1.5) +
      geom_point(data = graph_resource_production_NEI, aes(x = year, y = total)) +
      scale_colour_manual(values = scenario_short_color) +
      expand_limits(y = 0) +
      facet_wrap(facets = ~fct_reorder(Pollutant, total, .desc = TRUE), scales = "free_y") +
      ggtitle( "Air Pollution by Gas in the US (Resource Production)" ) +
      theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
      xlab("Year") +
      ylab("Tg") +
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/5.4.resource_prod.png", dpi=600/2, width=6000/300, height=3000/300)
    
#### 5.5. Agriculture ####
    air_pollution_tech_graphs_final %>%
      filter(GCAM_Sector == "Agriculture" ) %>%
      group_by(Units, year, GCAM_Sector, Scenario, Pollutant) %>%
      summarise(total = sum(value)) %>%
      ungroup() -> graph_Agriculture
    
    NEI_2020_emissions_complete_USA %>%
      filter(GCAM_Sector == "Agriculture") %>%
      group_by(Units, year, GCAM_Sector, Pollutant) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> graph_Agriculture_NEI
    
    p <- ggplot() + geom_line(data=graph_Agriculture, aes(x=year, y=total, colour = Scenario, group = Scenario), size = 1.5) +
      geom_point(data = graph_Agriculture_NEI, aes(x = year, y = total)) +
      scale_colour_manual(values = scenario_short_color) +
      expand_limits(y = 0) +
      facet_wrap(facets = ~fct_reorder(Pollutant, total, .desc = TRUE), scales = "free_y") +
      ggtitle( "Air Pollution by Gas in the US (Agriculture)" ) +
      theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
      xlab("Year") +
      ylab("Tg") +
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/5.5.agriculture.png", dpi=600/2, width=6000/300, height=3000/300)
    
#### 5.6. Refining (biomass) ####
    air_pollution_tech_graphs_final %>%
      filter(GCAM_Sector == "Refining (biomass)" ) %>%
      group_by(Units, year, GCAM_Sector, Scenario, Pollutant) %>%
      summarise(total = sum(value)) %>%
      ungroup() -> graph_refining_biomass
    
    NEI_2020_emissions_complete_USA %>%
      filter(GCAM_Sector == "Refining (biomass)") %>%
      group_by(Units, year, GCAM_Sector, Pollutant) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> graph_refining_biomass_NEI
    
    p <- ggplot() + geom_line(data=graph_refining_biomass, aes(x=year, y=total, colour = Scenario, group = Scenario), size = 1.5) +
      geom_point(data = graph_refining_biomass_NEI, aes(x = year, y = total)) +
      scale_colour_manual(values = scenario_short_color) +
      expand_limits(y = 0) +
      facet_wrap(facets = ~fct_reorder(Pollutant, total, .desc = TRUE), scales = "free_y") +
      ggtitle( "Air Pollution by Gas in the US (Refining biomass)" ) +
      theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
      xlab("Year") +
      ylab("Tg") +
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/5.6.refining_biomass.png", dpi=600/2, width=6000/300, height=3000/300)
    
#### 5.7. Refining (oil) ####
    air_pollution_tech_graphs_final %>%
      filter(GCAM_Sector == "Refining (oil)" ) %>%
      group_by(Units, year, GCAM_Sector, Scenario, Pollutant) %>%
      summarise(total = sum(value)) %>%
      ungroup() -> graph_refining_oil
    
    NEI_2020_emissions_complete_USA %>%
      filter(GCAM_Sector == "Refining (oil)") %>%
      group_by(Units, year, GCAM_Sector, Pollutant) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> graph_refining_oil_NEI
    
    p <- ggplot() + geom_line(data=graph_refining_oil, aes(x=year, y=total, colour = Scenario, group = Scenario), size = 1.5) +
      geom_point(data = graph_refining_oil_NEI, aes(x = year, y = total)) +
      scale_colour_manual(values = scenario_short_color) +
      expand_limits(y = 0) +
      facet_wrap(facets = ~fct_reorder(Pollutant, total, .desc = TRUE), scales = "free_y") +
      ggtitle( "Air Pollution by Gas in the US (Refining oil)" ) +
      theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
      xlab("Year") +
      ylab("Tg") +
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/5.7.refining_oil.png", dpi=600/2, width=6000/300, height=3000/300)

#### 5.8. Cement ####
    air_pollution_tech_graphs_final %>%
      filter(GCAM_Sector == "Cement" ) %>%
      group_by(Units, year, GCAM_Sector, Scenario, Pollutant) %>%
      summarise(total = sum(value)) %>%
      ungroup() -> graph_cement
    
    NEI_2020_emissions_complete_USA %>%
      filter(GCAM_Sector == "Cement") %>%
      group_by(Units, year, GCAM_Sector, Pollutant) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> graph_cement_NEI
    
    p <- ggplot() + geom_line(data=graph_cement, aes(x=year, y=total, colour = Scenario, group = Scenario), size = 1.5) +
      geom_point(data = graph_cement_NEI, aes(x = year, y = total)) +
      scale_colour_manual(values = scenario_short_color) +
      expand_limits(y = 0) +
      facet_wrap(facets = ~fct_reorder(Pollutant, total, .desc = TRUE), scales = "free_y") +
      ggtitle( "Air Pollution by Gas in the US (Cement)" ) +
      theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
      xlab("Year") +
      ylab("Tg") +
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/5.8.cement.png", dpi=600/2, width=6000/300, height=3000/300)
    
#### 5.9. Commercial ####
    air_pollution_tech_graphs_final %>%
      filter(GCAM_Sector == "Commercial" ) %>%
      group_by(Units, year, GCAM_Sector, Scenario, Pollutant) %>%
      summarise(total = sum(value)) %>%
      ungroup() -> graph_commercial
    
    NEI_2020_emissions_complete_USA %>%
      filter(GCAM_Sector == "Commercial") %>%
      group_by(Units, year, GCAM_Sector, Pollutant) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> graph_commercial_NEI
    
    p <- ggplot() + geom_line(data=graph_commercial, aes(x=year, y=total, colour = Scenario, group = Scenario), size = 1.5) +
      geom_point(data = graph_commercial_NEI, aes(x = year, y = total)) +
      scale_colour_manual(values = scenario_short_color) +
      expand_limits(y = 0) +
      facet_wrap(facets = ~fct_reorder(Pollutant, total, .desc = TRUE), scales = "free_y") +
      ggtitle( "Air Pollution by Gas in the US (Commercial)" ) +
      theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
      xlab("Year") +
      ylab("Tg") +
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/5.9.Commercial.png", dpi=600/2, width=6000/300, height=3000/300)
    
#### 5.10. Industrial ####
    air_pollution_tech_graphs_final %>%
      filter(GCAM_Sector == "Industrial" ) %>%
      group_by(Units, year, GCAM_Sector, Scenario, Pollutant) %>%
      summarise(total = sum(value)) %>%
      ungroup() -> graph_industrial
    
    NEI_2020_emissions_complete_USA %>%
      filter(GCAM_Sector == "Industrial") %>%
      group_by(Units, year, GCAM_Sector, Pollutant) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> graph_industrial_NEI
    
    p <- ggplot() + geom_line(data=graph_industrial, aes(x=year, y=total, colour = Scenario, group = Scenario), size = 1.5) +
      geom_point(data = graph_industrial_NEI, aes(x = year, y = total)) +
      scale_colour_manual(values = scenario_short_color) +
      expand_limits(y = 0) +
      facet_wrap(facets = ~fct_reorder(Pollutant, total, .desc = TRUE), scales = "free_y") +
      ggtitle( "Air Pollution by Gas in the US (Industrial)" ) +
      theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
      xlab("Year") +
      ylab("Tg") +
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/5.10.Industrial.png", dpi=600/2, width=6000/300, height=3000/300)
    
#### 5.11. Residential ####
    air_pollution_tech_graphs_final %>%
      filter(GCAM_Sector == "Residential" ) %>%
      group_by(Units, year, GCAM_Sector, Scenario, Pollutant) %>%
      summarise(total = sum(value)) %>%
      ungroup() -> graph_residential
    
    NEI_2020_emissions_complete_USA %>%
      filter(GCAM_Sector == "Residential") %>%
      group_by(Units, year, GCAM_Sector, Pollutant) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> graph_residential_NEI
    
    p <- ggplot() + geom_line(data=graph_residential, aes(x=year, y=total, colour = Scenario, group = Scenario), size = 1.5) +
      geom_point(data = graph_residential_NEI, aes(x = year, y = total)) +
      scale_colour_manual(values = scenario_short_color) +
      expand_limits(y = 0) +
      facet_wrap(facets = ~fct_reorder(Pollutant, total, .desc = TRUE), scales = "free_y") +
      ggtitle( "Air Pollution by Gas in the US (Residential)" ) +
      theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
      xlab("Year") +
      ylab("Tg") +
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/5.11.Residential.png", dpi=600/2, width=6000/300, height=3000/300)
    
#### 5.12. Transportation ####
    air_pollution_tech_graphs_final %>%
      filter(GCAM_Sector == "Transportation" ) %>%
      group_by(Units, year, GCAM_Sector, Scenario, Pollutant) %>%
      summarise(total = sum(value)) %>%
      ungroup() -> graph_transport
    
    NEI_2020_emissions_complete_USA %>%
      filter(GCAM_Sector == "Transportation") %>%
      group_by(Units, year, GCAM_Sector, Pollutant) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> graph_transport_NEI
    
    p <- ggplot() + geom_line(data=graph_transport, aes(x=year, y=total, colour = Scenario, group = Scenario), size = 1.5) +
      geom_point(data = graph_transport_NEI, aes(x = year, y = total)) +
      scale_colour_manual(values = scenario_short_color) +
      expand_limits(y = 0) +
      facet_wrap(facets = ~fct_reorder(Pollutant, total, .desc = TRUE), scales = "free_y") +
      ggtitle( "Air Pollution by Gas in the US (Transportation)" ) +
      theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
      xlab("Year") +
      ylab("Tg") +
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/5.12.Transportation.png", dpi=600/2, width=6000/300, height=3000/300)
    
#### 5.13. Urban ####
    air_pollution_tech_graphs_final %>%
      filter(GCAM_Sector == "Urban" ) %>%
      group_by(Units, year, GCAM_Sector, Scenario, Pollutant) %>%
      summarise(total = sum(value)) %>%
      ungroup() -> graph_urban
    
    NEI_2020_emissions_complete_USA %>%
      filter(GCAM_Sector == "Urban") %>%
      group_by(Units, year, GCAM_Sector, Pollutant) %>%
      summarise(total = sum(emissions)) %>%
      ungroup() -> graph_urban_NEI
    
    p <- ggplot() + geom_line(data=graph_urban, aes(x=year, y=total, colour = Scenario, group = Scenario), size = 1.5) +
      geom_point(data = graph_urban_NEI, aes(x = year, y = total)) +
      scale_colour_manual(values = scenario_short_color) +
      expand_limits(y = 0) +
      facet_wrap(facets = ~fct_reorder(Pollutant, total, .desc = TRUE), scales = "free_y") +
      ggtitle( "Air Pollution by Gas in the US (Urban)" ) +
      theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
      xlab("Year") +
      ylab("Tg") +
      my_theme
    #ggsave("figures/downscaling_nonpoint_mobile/5.13.Urban.png", dpi=600/2, width=6000/300, height=3000/300)
    
#----------------------------------------------- SECTION 6: NEIGHBOR REGIONS ------------------------------------------------
  #To run WRF-CMAQ we will need emission changes in neighboring regions. We will apply multipiers from GCAM at a pollutant level, per scenairo and peirod.
#### 6.1 Process GCAM data ####
    neighbor_pollutant <- read_csv("data/downscaling/GCAM_neighbor_emissions.csv", skip = 1)
    
    neighbor_pollutant%>%
      gather(year, value, -scenario, -region, -GHG, -Units) %>%
      filter(year > 2010 & year < 2055,
             GHG %in% air_pollutants) %>%
      separate(scenario, c("scenario", "date"), sep = ",", remove = TRUE) %>%
      left_join(scenario_mapping, by = "scenario") %>%
      select(-date, -scenario) %>%
      left_join(air_pollutant_NEI_mapping, by = c("GHG" = "ghg")) %>%
      #Fix missing pollutants
      mutate(Pollutant = if_else(GHG == "BC" | GHG == "BC_AWB", "BC", Pollutant))%>%
      mutate(Pollutant = if_else(GHG == "OC" | GHG == "OC_AWB", "OC", Pollutant)) %>%
      mutate(Pollutant = if_else(GHG == "SO2_3" | GHG == "SO2_3_AWB", "SO2", Pollutant)) %>%
      #Add pollutants
      group_by(Scenario, region, year, Pollutant, Units) %>%
      summarise(total = sum(value)) %>%
      ungroup() %>%
      filter(year == 2020 | year == 2030 |year == 2050)-> neighbor_pollutant_final
    
    #Here we will group BC = OC
    neighbor_pollutant_final %>%
      filter(Pollutant %in% PM_agg_filter) %>%
      spread(Pollutant, total) %>%
      mutate(AGG = BC + OC) %>%
      select(-BC, -OC) %>%
      gather(Pollutant, total, -Scenario, -region, -year, -Units)-> neighbor_pollutant_final_AGG
    
    neighbor_pollutant_final %>%
      filter(!Pollutant %in% PM_agg_filter) %>%
      bind_rows(neighbor_pollutant_final_AGG) %>%
      spread(year, total) %>%
      mutate(`2030_multiplier` = `2030` / `2020`) %>% 
      mutate(`2050_multiplier` = `2050` / `2020`) -> neighbor_pollutant_final_multiplier
    
    neighbor_pollutant_final_multiplier %>%
      select(Scenario, region, Pollutant, `2030_multiplier`) %>%
      mutate(period = "2030") -> neighbor_pollutant_final_multiplier_2030
    
    neighbor_pollutant_final_multiplier %>%
      select(Scenario, region, Pollutant, `2050_multiplier`) %>%
      mutate(period = "2050") -> neighbor_pollutant_final_multiplier_2050
    
    
#### 6.2. Here we print output tables ####
    # Get unique scenarios
    scenarios <- unique(neighbor_pollutant_final_multiplier$Scenario)
    
    # # Loop through each scenario for 2030
    # for (scenario in scenarios) {
    #   # Filter data for the current scenario
    #   filtered_data <- neighbor_pollutant_final_multiplier_2030 %>%
    #     filter(Scenario == scenario)
    #   
    #   # Define the file name
    #   file_name <- paste("output_data/downscaling/neighbor_multiplier_2030_", scenario, ".csv", sep = "")
    #   
    #   # Save the filtered data as a CSV file
    #   write.csv(filtered_data, file_name, row.names = FALSE)
    # }
    
    # Loop through each scenario for 2050
    for (scenario in scenarios) {
      # Filter data for the current scenario
      filtered_data <- neighbor_pollutant_final_multiplier_2050 %>%
        filter(Scenario == scenario)
      
      # Define the file name
      file_name <- paste("output_data/downscaling/neighbor_multiplier_2050_", scenario, ".csv", sep = "")
      
      # Save the filtered data as a CSV file
      write.csv(filtered_data, file_name, row.names = FALSE)
    }
        