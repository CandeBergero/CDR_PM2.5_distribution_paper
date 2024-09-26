# downscaling_electricity.R
# Data downscaling for electricity sectors (point source emissions) 
# This script is designed to downscale GCAM data for the electricity sector based on eGRID data
# Date: July-September 2023 
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
  library(sf)

#### 1.2. Load GCAM scenarios ####
#Set working directory
  setwd("~/Documents/GCAM/gcam-v6.0-Mac-Release-Package/output/R_scripts_Just_transition")


#Connect to the database
  #conn <- localDBConn('/Users/mariacandelariabergero/Documents/GCAM/gcam-v6.0-Mac-Release-Package/output', 'database_basexdb_FINAL')
  
#Load the scenario
  #prj <- addScenario(conn, 'dat/GCAM_analysis.dat', 'GCAM-USA_REF', 'queries/queries.xml') #Reference
  #prj <- addScenario(conn, 'dat/GCAM_analysis.dat', 'GCAM-USA_DAC_NZUSA2050ghg_NZROW2060co2_newEFV2_allEF', 'queries/queries.xml') #Net-zero + high CDR
  #prj <- addScenario(conn, 'dat/GCAM_analysis.dat', 'GCAM-USA_withDAC_NZUSA2050ghg_NZROW2060co2_forest_CCSV3_allEF', 'queries/queries.xml') #Net-zero + lowCDR
  
#Load the project
  prj <- loadProject('dat/GCAM_analysis.dat')
  listScenarios(prj)
  listQueries(prj)

#Get color schemes
  source( "~/Documents/GCAM/gcam-v6.0-Mac-Release-Package/output/R_scripts_Just_transition/code/color_schemes.R" ) # some predefined color schemes
  
#### 1.3. Load mapping files ####
  scenario_mapping <- read_csv("mappings/scenario_mapping.csv")
  tech_mapping <- read_csv("mappings/agg_tech_mapping.csv", skip = 1)
  egrid_tech_mapping <- read_csv("mappings/egrid_tech_mapping.csv")
  GENSTAT_mapping <- read_csv("mappings/GENSTAT_mapping.csv")
  GCAM_elec_tech_mapping <- read_csv("mappings/GCAM_elec_tech_mapping.csv")
  generator_prime_mover <- read_csv("mappings/prime_mover_map.csv")
  pollutant_acronym <- read_csv("mappings/pollutant_acronym.csv")
  coal_IGCC_EF <- read_csv("mappings/coal_IGCC_EF.csv")
  state_grids <- read_csv("mappings/states_subregions.csv")
  states_abb_mapping <- read_csv("mappings/states_abbreviation.csv")


#### 1.4. General parameters ####
# Conversions
  EJ_MWh <- 277777777.78 #From EJ to MWh hour

# Order
  scenario_short_facet_order <- c("Net-zero, High BECCS, High DAC",
                                  "Net-zero, Low BECCS, Low DAC",
                                  "Reference")
  
  scenario_short_order <- c("Reference",
                            "Net-zero, High BECCS, High DAC",
                            "Net-zero, Low BECCS, Low DAC")
  
  comparison_fuel_order <- c("gas", "coal", "biomass", "refined liquids", "other_fossil",
                             "nuclear", "wind", "hydro", "solar", "geothermal")


#Filters
  combustion_fuels <- c("Coal", "Coal CCS", "Gas", "Gas CCS", "Oil", "Oil CCS", "Biomass", "Biomass CCS",
                        "coal", "coal CCS", "gas", "gas CCS", "oil", "oil CCS", "biomass", "biomass CCS",
                        "refined liquids")
  
  generators_to_keep <- c("OP", "P", "T", "TS", "L", "OA", "U", "V") #These are the status we will keep
  # page 51 from https://www.epa.gov/system/files/documents/2023-01/eGRID2021_technical_guide.pdf

  #Graph theme
  my_theme <-         theme(panel.background = element_rect(fill = "white"),
                            plot.background = element_rect(fill = "white"),
                            plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
                            panel.grid.minor = element_blank(),
                            axis.title.x = element_blank(),
                            axis.text.x  = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.y  = element_blank(),
                            legend.title = element_text(size = 20),
                            legend.text = element_text(size = 15))

#------------------------------------------------- SECTION 2: GCAM DATA ----------------------------------------------------
  #In this section we prepare and synthesize GCAM data to calculate retirements
  #The outcome of this section is a table with Scenario, electricity generation technology, period and state with additions and retirements
#### 2.1. Electricity generation by technology (CCS vs no CCS) ####
      elec_gen_tech <- getQuery(prj, "elec gen by gen tech and cooling tech (incl cogen)")
      
      elec_gen_tech %>%
        mutate(subsector = gsub(",depth=1","", subsector...6)) %>%
        filter(year > 2010) %>%
        dplyr::select(-sector, -subsector...5, -subsector...6) %>%
        left_join(tech_mapping, by = "technology") %>%
        group_by(Units, scenario, region, agg_tech, year) %>%
        dplyr::summarise(sum_EJ= sum(value)) %>%
        ungroup() %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> elec_gen_tech_final_state_EJ
      
      elec_gen_tech_final_state_EJ %>%
        group_by(Units, Scenario, agg_tech, year) %>%
        dplyr::summarise(sum_EJ_USA = sum(sum_EJ)) %>%
        ungroup() %>%
        dplyr::rename(Technology = agg_tech) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> elec_gen_tech_final_USA_EJ

      elec_gen_tech_final_USA_EJ %>%
        group_by(Units, Scenario, year) %>%
        dplyr::summarise(sum = sum(sum_EJ_USA)) %>%
        ungroup() -> elec_gen_tech_final_total_USA_EJ
      
#### 2.2. Electricity generation by technology detail ####
      #Here we want to keep the technology detail
      elec_gen_tech %>%
        mutate(subsector = gsub(",depth=1","", subsector...6)) %>%
        filter(year > 2010) %>%
        dplyr::select(-sector, -subsector...5, -subsector...6) %>%
        left_join(tech_mapping, by = "technology") %>% #Here we bring in fuels
        left_join(GCAM_elec_tech_mapping, by = "technology") %>% #Here we want to keep technology detail, but without water info
        group_by(Units, scenario, year, agg_tech, Technology, region) %>% #Now get USA total by technology and fuel
        dplyr::summarise(generation_EJ = sum(value)) %>%
        ungroup() %>%
        separate(agg_tech, c("Fuel", "CCS"), sep = " ") %>%
        left_join(scenario_mapping, by = "scenario")-> elec_gen_tech_final_states_EJ_detailed
      
      #Here we want to make sure that technologies that disappear into teh future (such as conventional coal in Oregon) have subsequent values for 0, as opposed to disappearing. This will matter in retirements
      elec_gen_tech_final_states_EJ_detailed %>%
        dplyr::select(Units, year, Fuel, Technology, region, generation_EJ, Scenario) %>%
        spread(year, generation_EJ) %>%
        #Here we replace nas with 0
        replace(is.na(.), 0) %>%
        gather(year, generation_EJ, -Units, -Fuel, -Technology, -region, -Scenario) %>%
        mutate(year = as.numeric(year))-> elec_gen_tech_final_states_EJ_detailed
      
      elec_gen_tech_final_states_EJ_detailed %>%
        group_by(Units, Scenario, year, Fuel, Technology) %>%
        dplyr::summarise(generation_EJ_USA = sum(generation_EJ)) %>%
        ungroup() -> elec_gen_tech_final_USA_EJ_detailed

      elec_gen_tech_final_USA_EJ_detailed %>%
        filter(Fuel %in% combustion_fuels) -> elec_gen_tech_final_USA_EJ_detailed_combustion
      
      elec_gen_tech_final_USA_EJ_detailed_combustion %>%
        group_by(Units, Scenario, year) %>%
        dplyr::summarise(sum = sum(generation_EJ_USA)) %>%
        ungroup() -> total

#### 2.3. ADDITIONS: Electricity generation by technology & vintage ####
      elec_gen_tech_vintages <- getQuery(prj, "elec gen by gen tech and cooling tech and vintage (incl cogen)")
      
      elec_gen_tech_vintages %>%
        dplyr::rename(subsector = subsector...5,
               subsector.1 = subsector...6) %>%
        dplyr::select(scenario, region, subsector, subsector.1, technology, year, value, Units) %>%
        mutate(subsector = gsub(",depth=1", "", subsector.1)) %>%
        separate(technology, c("technology", "temp"), sep = ",") %>%
        separate(temp, c("temp", "vintage"), sep = "=") %>%
        dplyr::select(-temp) %>%
        filter(year > 2010 & year < 2055,
               year == vintage) %>% #Vintage = year represents new capacity
        mutate(vintage = as.numeric(vintage)) %>%
        left_join(tech_mapping, by = "technology")  %>%
        left_join(GCAM_elec_tech_mapping, by = "technology")%>% #Here we want to keep technology detail, but without water info
        group_by(scenario, region, Technology, agg_tech, year, vintage, Units) %>%
        dplyr::summarise(sum_additions = sum(value)) %>%
        ungroup() %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) %>%
        dplyr::select(-`Policy USA`, -`Policy Global`, -BECCS, -DAC, -scenario)-> elec_gen_tech_vintages_EJ
      
      elec_gen_tech_vintages_EJ %>%
        group_by(Scenario, Technology, agg_tech, year, vintage, Units) %>%
        dplyr::summarise(sum_additions_USA = sum(sum_additions)) %>%
        ungroup() %>%
        filter(year > 2020,
               #Filter out hydro, since it is held constant, so additions = retirements
               agg_tech != "Hydro")-> elec_gen_tech_vintages_EJ_USA

#### 2.4. RETIREMENTS ####
      #Generation(year) = generation(year-1) + additions(year) + retirements(year)
      #Therefore here retirements = generation(year-1) + additions - generation(year)
      elec_gen_tech_final_states_EJ_detailed %>%
        dplyr::select(Scenario, year, region, Fuel, Technology, generation_EJ, Units) %>%
        group_by(region, Scenario, Fuel, Technology, Units) %>%
        dplyr::mutate(prior_generation =  lag(generation_EJ, n = 1L, order_by = year)) %>% #Prior generation is generation(t-1)
        ungroup() %>%
        replace(is.na(.), 0) %>% #Missing values are 2015, since there is no prior year. We set to 0
        left_join(elec_gen_tech_vintages_EJ, by = c("Scenario", "year", "region", "Technology", "Units")) %>% #Bring in new additions
        dplyr::select(-agg_tech, -vintage) %>%
        replace(is.na(.), 0) %>% #Here NAs happen when there is no new addition, so we set those values to 0
        #Calculate retirements
        dplyr::mutate(retirements = prior_generation + sum_additions - generation_EJ,
               retirements = retirements * -1) %>%
        dplyr::rename(additions = sum_additions)-> elec_gen_tech_retirements_EJ
      
      elec_gen_tech_retirements_EJ %>%
        group_by(Scenario, year, Technology) %>%
        dplyr::summarise(sum_retirements = sum(retirements)) %>%
        ungroup() %>%
        filter(year > 2015,
               #Filter out hydro, since it is held constant, so additions = retirements
               Technology != "hydropower")-> elec_gen_tech_retirements_EJ_USA

#### 2.5. Gross changes: Now both additions and retirements ####
      elec_gen_tech_retirements_EJ %>%
        dplyr::select(-generation_EJ, -prior_generation, -additions) %>%
        mutate(status = "retirements") %>%
        dplyr::rename(value = retirements) -> retirements
      
      elec_gen_tech_retirements_EJ %>%
        dplyr::select(-generation_EJ, -prior_generation, -retirements) %>%
        mutate(status = "additions") %>%
        dplyr::rename(value = additions) -> additions
      
      retirements %>%
        bind_rows(additions) -> total_generation_add_ret ### FINAL: Table with additions and retirements at state level
      
      total_generation_add_ret %>%
        group_by(Scenario, year, Fuel, Technology, Units, status) %>%
        dplyr::summarise(sum = sum(value)) %>%
        ungroup() %>%
        filter(year > 2015,
               Technology != "hydropower")-> total_generation_add_ret_USA

      #Get combustion fuels only
      total_generation_add_ret_USA %>%
        filter(Fuel %in% combustion_fuels) -> total_generation_add_ret_USA_combustion

      total_generation_add_ret_USA_combustion %>%
        group_by(Scenario, year, Fuel, status) %>%
        dplyr::summarise(sum = sum(sum)) %>%
        ungroup() -> total_generation_add_ret_USA_combustion_fuel

#### 2.6. Net changes: for Electricity Generation ####
      # Here we create vintages ourselves
      # First for all years
      total_generation_add_ret %>%
        group_by(region, Scenario, year, Technology, Fuel, Units) %>%
        dplyr::summarise(net_changes = sum(value)) %>%
        ungroup() -> net_changes_states
      
      net_changes_states %>%
        group_by(Scenario, year, Technology, Fuel, Units) %>%
        dplyr::summarise(net_changes_USA = sum(net_changes)) %>%
        ungroup()  %>%
        filter(year > 2015)-> net_changes_states_USA

#### 2.7. Here we want to group values into 2020, 2030 and 2050 ####
      # 2020 represents 2020
      # 2030 represents 2025 and 2030
      # 2050 represents 2035 to 2050
      total_generation_add_ret %>%
        filter(year > 2015 & year < 2035) %>% #Keep GCAM periods: 2020, 2025, 2030
        mutate(Period = if_else(year == 2020, "2020", "2021-2030")) %>%
        group_by(Scenario, Technology, Period, status, Units) %>%
        dplyr::summarise(sum_gross_change = sum(value)) %>%
        ungroup() %>%
        mutate(Period = as.character(Period))-> total_generation_add_ret_gross_changes_periods_EJ_2020_2030
      
      total_generation_add_ret %>%
        filter(year > 2020) %>% #Keep GCAM periods: 2025, 2030, 2035, 2040, 2045, 2050
        mutate(Period =  "2021-2050") %>%
        group_by(Scenario, Technology, Period, status, Units) %>%
        dplyr::summarise(sum_gross_change = sum(value)) %>%
        ungroup() %>%
        mutate(Period = as.character(Period)) %>%
        bind_rows(total_generation_add_ret_gross_changes_periods_EJ_2020_2030) %>%
        filter(Technology != "hydropower")-> total_generation_add_ret_gross_changes_periods_EJ #Table with gross changes USA level

    #Combustion only
    total_generation_add_ret_gross_changes_periods_EJ %>%
      left_join(tech_mapping, by = c("Technology" = "technology")) %>%
      filter(agg_tech %in% combustion_fuels)-> total_generation_add_ret_gross_changes_periods_EJ_combustion


#------------------------------------------------- SECTION 3: eGRID DATA --------------------------------------------------
  #In this section we filter eGRID datasets to keep only relevant generators and plant
#### 3.1. eGRID 2020 data for generators ####
      egrid_data_generator_2020_metric <- read_csv("data/egrid_2020_data_generation_generator.csv", skip = 1)

      #Here we want to see each generator, and then take out those that will retire
      #This data is from 2020 EPA egrid https://www.epa.gov/egrid/data-explorer
      #Note: we will center on generation, and not capacity, since GCAM gives use generation
      #Variable GENNTAN2 is generator annual net generation (GJ)
    
    egrid_data_generator_2020_metric %>% #N = 30,193
      filter(PSTATABB != "PR") %>%  #We filter out Puerto Rico. N = 30,047
      left_join(GENSTAT_mapping, by = "GENSTAT") %>%
      left_join(generator_prime_mover, by = c( "FUELG1","PRMVR")) %>%
      filter(gcam_fuel %in% combustion_fuels, # Keep combustion fuels. N = 16,941
             GENSTAT %in% generators_to_keep) %>% # Keep relevant status only. N = 11,105
      mutate(GENNTAN2 = if_else(is.na(GENNTAN2), 0, GENNTAN2)) %>% #Set NAs = 0 generation for the 357 cases
      #remove 398 negative cases
      filter(GENNTAN2 >= 0) -> egrid_data_generator_2020_metric_final # N = 10,707
    
    egrid_data_generator_2020_metric_final %>% # N = 10,707
      group_by(PSTATABB, gcam_fuel, Technology) %>%
      dplyr::summarise(eGRID_2020_total = sum(GENNTAN2)) %>%
      ungroup() %>%
      mutate(eGRID_2020_state_EJ = eGRID_2020_total / 10^9) %>%
      dplyr::select(-eGRID_2020_total) -> eGRID_2020_EJ_state #Totals at a state level by technology
    
#### 3.2. eGRID at the plant level ####
      #Here we want to have a table with 2020 actual generation
      egrid_data_plant_2020_metric <- read_csv("data/egrid_2020_data_generation_plant.csv", skip = 1)

      generator_prime_mover %>%
        dplyr::select(FUELG1, gcam_fuel) %>%
        distinct() -> generator_prime_mover_fuel
      
      egrid_data_plant_2020_metric %>%
        filter(!is.na(NAMEPCAP)) %>% # Filter out 1 plant without nameplate capacity. N = 12,667
        filter(PSTATABB != "PR") %>% # Filter out Puerto Rico. N = 12,618
        dplyr::select(YEAR, PNAME, NAMEPCAP, NUMGEN, PSTATABB, ORISPL, LAT, LON, NUMGEN, PLFUELCT, PLPRMFL,
               PLNGENAN2, PLNOXRTA2, PLSO2RTA2,	PLCO2AN,	PLCH4AN,	PLN2OAN, PLHTRT) %>%
        left_join(generator_prime_mover_fuel, by = c("PLPRMFL" = "FUELG1")) %>%
        mutate(gcam_fuel = if_else(PLPRMFL == "COG", "coal", gcam_fuel)) %>% #Here we add COG fuel to the table, since it was missing
        #Filter to keep only combustion fuels, go from N = 12,618 to 4,235 plants
        filter(gcam_fuel %in% combustion_fuels) -> egrid_data_plant_2020_metric_fuels
      

#------------------------------------------------- SECTION 4: DATA PREPARATION ---------------------------------------------
#### 4.1. Calculate cumulative GCAM quotas by period (2030 and 2050) #####
    #### 4.1.1. Here we want to get total 2020 electricity generation per state at a technology level ####
      elec_gen_tech_final_states_EJ_detailed %>%
        filter(year == 2020,
               Fuel %in% combustion_fuels) %>%
        group_by(Scenario, region, Fuel, Technology) %>%
        dplyr::summarise(sum_tech_2020_EJ_GCAM = sum(generation_EJ)) %>%
        ungroup()  -> elec_gen_tech_final_states_EJ_detailed_2020
      
    #### 4.1.2. Now we want to create two groups for additions and retirements: 2021-2030 (GCAM 2025-2030), and 2021-2050 (GCAM 2025-2050) ####
      total_generation_add_ret %>%
        filter(Fuel %in% combustion_fuels) %>%
        filter(year > 2020 & year < 2035) %>%
        mutate(Period = 2030) %>% #This is 2030 compared to 2020, so it represents 2021-2030
        group_by(Period, Scenario, region, Fuel, Technology, status, Units) %>%
        dplyr::summarise(value = sum(value)) %>%
        ungroup()-> total_generation_add_ret_period_1
      
      total_generation_add_ret %>%
        filter(Fuel %in% combustion_fuels) %>%
        filter(year > 2020 & year <= 2050) %>%
        mutate(Period = 2050) %>% #This is 2050 compared 2020, so it represents 2021-2050
        group_by(Period, Scenario, region, Fuel, Technology, status, Units) %>%
        dplyr::summarise(value = sum(value)) %>%
        ungroup()-> total_generation_add_ret_period_2
      
    #### 4.1.3. Join both tables, but keep additions and retirements in different columns ####
      total_generation_add_ret_period_1 %>%
        bind_rows(total_generation_add_ret_period_2) %>%
        spread(status, value)-> total_generation_add_ret_period_complete
  
#### 4.2. Scale cumulative GCAM quotas #####
    # calculate scaled additions and retirements
    #### 4.2.1. First we get master table ####
      # Create master table -> 3 scenarios, 2 periods, 51 regions, 13 technologies & fuel = 3,978 rows
      total_generation_add_ret_period_complete %>%
        dplyr::select(Scenario, Period) %>%
        unique()-> part_a
      
      total_generation_add_ret_period_complete %>%
        dplyr::select(Fuel, Technology, Period) %>%
        unique()-> part_b
      
      total_generation_add_ret_period_complete %>%
        dplyr::select(region, Period) %>%
        unique()-> part_c
      
      part_a %>%
        left_join(part_b, by = "Period", relationship = "many-to-many") %>%
        left_join(part_c, by = "Period", relationship = "many-to-many") -> master_table #N = 3978
      
      #Here we want a table with information on additions, retirements, 2020 GCAM generation, and 2020 eGRID generation
      master_table %>%
        #Bring in 2020 electricity generation GCAM
        left_join(elec_gen_tech_final_states_EJ_detailed_2020, by = c("region", "Scenario", "Fuel", "Technology")) %>% #1,214 NA in GCAM_2020_tech_EJ
        dplyr::rename(GCAM_2020_tech_EJ = sum_tech_2020_EJ_GCAM) %>%
        #Bring in addition and retirements from GCAM
        left_join(total_generation_add_ret_period_complete, by = c("region", "Scenario", "Fuel", "Technology", "Period")) %>% #1,200 NA additions, 1,200 NAs in retirements
        dplyr::select(-Units) %>%
        #Note: there are 24 cases with a positive retirement quota. 5 of them are because of rounding error, since positive retirement quota is #e-18. But 19 are because of GCAM. 15/19 are in the Reference case. In reality, retirement should be 0, and the positive value should go to additions. We fix this below:
        mutate(additions = if_else(retirements > 0, additions + retirements, additions)) %>%
        mutate(retirements = if_else(retirements > 0, 0, retirements)) %>%
        mutate(net_change = additions + retirements) %>% #Calculate net changes from GCAM
        #Bring in 2020 generation from eGRID
        left_join(eGRID_2020_EJ_state, by = c("region" = "PSTATABB", #eGRID_2020_tech_EJ has 2,634 NAs
                                              "Fuel" = "gcam_fuel",
                                              "Technology"))-> master_table_final # 3978 rows
      
      
    #### 4.2.2. Apply rules ####
      #Here we readjust quotas so that we apply changes from GCAM to eGRID
      #But there are some cases where this would not work, because of NAs
      #Cases
        #1. If there is generation in GCAM, eGRID and quotas -> 1302 cases:
            #then we apply percentage change from GCAM to eGRID generation to each addition and retirement
            
        #GCAM yes, quotas yes, but no eGRID -> 1500 cases:
        # 2a. If net quota is positive
            #then we assume eGRID has 0 and add absolute net change value, and 0 retirement value
              #This means we are creating a new technology
            #1401 cases
        # 2b. If net quota is negative
            #then set addition quota to 0, retirement quota to 0 
            #99 cases
      
        # GCAM no, quota yes, eGRID no -> 0 cases:
        # 3a. If net quota is positive
            #then we add net change absolute and assume value in eGRID was 0, retirement = 0
              #This is another way of creating the technology
            # 0 cases
        # 3b. If net quota is negative
            # 0 cases, so we ignore
      
        # 4. instances where there is no quota
            # this means there is no change, so we ignore
            # 1176 cases
      
        # 5. No GCAM, yes quota, yes eGRID
            # 0 cases
      
      master_table_final %>%
        #Calculate fraction change in GCAM
        mutate(per_change_add = additions / GCAM_2020_tech_EJ,
               per_change_ret = retirements / GCAM_2020_tech_EJ) -> master_table_final
      
      #Create new table that stores final quotas
      GCAM_quotas <- data.frame(matrix(ncol = 0, nrow = 0))

  for (i in 1:nrow(master_table_final)) {
    scenario <- as.character(master_table_final[i, "Scenario"])
    period <- as.numeric(master_table_final[i, "Period"])
    state <- as.character(master_table_final[i, "region"])
    fuel <- as.character(master_table_final[i, "Fuel"])
    technology <- as.character(master_table_final[i, "Technology"])
    GCAM_generation <- as.numeric(master_table_final[i, "GCAM_2020_tech_EJ"])
    eGRID_generation <- as.numeric(master_table_final[i, "eGRID_2020_state_EJ"])
    additions <- as.numeric(master_table_final[i, "additions"])
    retirements <- as.numeric(master_table_final[i, "retirements"])
    net_change <- as.numeric(master_table_final[i, "net_change"])
    per_change_add <- as.numeric(master_table_final[i, "per_change_add"])
    per_change_ret <- as.numeric(master_table_final[i, "per_change_ret"])
    
    ## Rule 1 (1302 cases)
      if (!is.na(GCAM_generation) & !is.na(net_change) & !is.na(eGRID_generation)) {
        new_addition = per_change_add * eGRID_generation
        new_retirement = per_change_ret * eGRID_generation
        #Create new row
        new_row <- data.frame(
          Scenario = as.character(master_table_final[i, "Scenario"]), 
          Period = as.numeric(master_table_final[i, "Period"]),
          PSTATABB = as.character(master_table_final[i, "region"]), 
          gcam_fuel = as.character(master_table_final[i, "Fuel"]),
          gcam_technology = as.character(master_table_final[i, "Technology"]), 
          addition_quota = as.numeric(new_addition), 
          retirement_quota = as.numeric(new_retirement))
        #Add row to final table
        GCAM_quotas <- bind_rows(GCAM_quotas, new_row)

    
    ## Rule 2a (1401 cases)
      } else if (!is.na(GCAM_generation) & !is.na(net_change) & (net_change >= 0) & is.na(eGRID_generation)) {
        new_addition = net_change
        new_retirement = 0
        #Create new row
        new_row <- data.frame(
          Scenario = as.character(master_table_final[i, "Scenario"]), 
          Period = as.numeric(master_table_final[i, "Period"]),
          PSTATABB = as.character(master_table_final[i, "region"]), 
          gcam_fuel = as.character(master_table_final[i, "Fuel"]),
          gcam_technology = as.character(master_table_final[i, "Technology"]), 
          addition_quota = as.numeric(new_addition), 
          retirement_quota = as.numeric(new_retirement))
        #Add row to final table
        GCAM_quotas <- bind_rows(GCAM_quotas, new_row)
        
        
    ## Rule 2b (99 cases)
      } else if (!is.na(GCAM_generation) & !is.na(net_change) & (net_change < 0) & is.na(eGRID_generation)) {
        new_addition = 0
        new_retirement = 0
        #Create new row
        new_row <- data.frame(
          Scenario = as.character(master_table_final[i, "Scenario"]), 
          Period = as.numeric(master_table_final[i, "Period"]),
          PSTATABB = as.character(master_table_final[i, "region"]), 
          gcam_fuel = as.character(master_table_final[i, "Fuel"]),
          gcam_technology = as.character(master_table_final[i, "Technology"]), 
          addition_quota = as.numeric(new_addition), 
          retirement_quota = as.numeric(new_retirement))
        #Add row to final table
        GCAM_quotas <- bind_rows(GCAM_quotas, new_row)

        
    ## Rule 3a (0 cases)
      } else if (is.na(GCAM_generation) & !is.na(net_change) & (net_change > 0) & is.na(eGRID_generation)) {
        new_addition = net_change
        new_retirement = 0
        #Create new row
        new_row <- data.frame(
          Scenario = as.character(master_table_final[i, "Scenario"]), 
          Period = as.numeric(master_table_final[i, "Period"]),
          PSTATABB = as.character(master_table_final[i, "region"]), 
          gcam_fuel = as.character(master_table_final[i, "Fuel"]),
          gcam_technology = as.character(master_table_final[i, "Technology"]), 
          addition_quota = as.numeric(new_addition), 
          retirement_quota = as.numeric(new_retirement))
        #Add row to final table
        GCAM_quotas <- bind_rows(GCAM_quotas, new_row)
    
        
    ## All other cases we keep additions and retirements as 0
      } else {
    }
  }
      
    #### 4.2.3. Calculate totals ####
    #These tables analyze total additions and retirements per scenario and period just to double check outputs
      #original GCAM
      master_table_final %>%
        replace(is.na(.), 0) %>%
        group_by(Scenario, Period) %>%
        dplyr::summarise(GCAM_sum_additions = sum(additions),
                  GCAM_sum_retirements = sum(retirements)) %>%
        ungroup() %>%
        mutate(GCAM_net = GCAM_sum_additions + GCAM_sum_retirements)-> total_GCAM
      
      #scaled 
      GCAM_quotas %>%
        group_by(Period, Scenario) %>%
        dplyr::summarise(scaled_sum_additions = sum(addition_quota),
                  scaled_sum_retirements = sum(retirement_quota)) %>%
        ungroup() %>%
        mutate(scaled_net = scaled_sum_additions + scaled_sum_retirements) -> total_scaled
      
      total_GCAM %>%
        left_join(total_scaled, by = c("Scenario", "Period")) -> total_comparison #This table has comparison of additions, retirements, and net from original GCAM and scaled based on these rules0

#### 4.3. eGRID_generators #####
    #### 4.3.1. Retirements ####
      # eGRID
      egrid_data_generator_2020_metric_final %>%
        filter(GENYRRET > 2020 & GENYRRET < 2051) -> retire_eGRID # 442 generators
      
      # eia
      # Here we calculate how many generators is eia retiring between 2021-2050
      scheduled_retirements <- read_csv("data/Retirements.csv")
      #These are scheduled retirements from https://epa.maps.arcgis.com/apps/dashboards/591b44aa8dd144719e059a39cb625c99
      
      scheduled_retirements %>%
        dplyr::select(-Technology) %>%
        filter(`Retirement Year` > 2020 & `Retirement Year` < 2051) -> retire_eia # 788 generators
      
      #Conclusion: there is missing data in eGRID on retirement for generators. Complete the list.
      retire_eia %>%
        right_join(egrid_data_generator_2020_metric_final %>%
                     dplyr::select(PSTATABB, ORISPL, GENID, NAMEPCAP,
                            GENYRONL, GENYRRET, gcam_fuel, Technology), by = c("Plant State" = "PSTATABB",
                                                                               "Plant ID" = "ORISPL",
                                                                               "GENID")) %>%
        #Here retire year has the data of eGRID, plus the data in the eia
        #Note: if values from eia and eGRID are different, we follow eia since it has the latest values (eia 2023 compared to 2020 eGRID)
        mutate(retire_year = if_else(GENYRRET != `Retirement Year`, `Retirement Year`, GENYRRET)) %>%
        mutate(retire_year = if_else(is.na(GENYRRET), `Retirement Year`, retire_year)) %>%
        dplyr::select(`Plant State`,`Plant ID`, GENID, retire_year) %>%
        #Here we keep distinct retirements only, since the eia has some duplicate data
        filter(!is.na(retire_year)) %>%
        distinct() -> generators_to_retire # 597 generators have retirement scheduled date
      
    #### 4.3.2. Prepare final table #### 
    #Here we prepare the final table for eGRID_generators based on egrid_data_generator_2020_metric_final
      egrid_data_generator_2020_metric_final %>%
        #Set birth year as 2030 if NA, which applies to the 304 generators that are not yet operating, but under construction or such status
        mutate(GENYRONL = if_else(is.na(GENYRONL), 2030, GENYRONL),
               #place holders 
               GR_index = GENYRONL, #This will be the retirement index
               CFACT = if_else(is.na(CFACT), 0, CFACT), #Here we set NAs in capacity factor as 0
               max_CFACT = if_else(CFACT < 0.8500, 0.8500, CFACT), #This is the assumed maximum capacity factor
               max_GENNTAN2_MWh = (NAMEPCAP * 8760 * max_CFACT), #Here we calculate maximum potential generation
               #And here we convert that maximum from MWh to EJ
               #1 EJ = 277777777.78 MWh
               max_GENNTAN2_EJ = max_GENNTAN2_MWh / 277777777.78) %>%
        # Here we create unique identifier 
        unite(unique_ID, c(ORISPL, GENID), sep = "_", remove = FALSE) %>%
        #Here we bring in scheduled retirements -> year when generator will retire
        left_join(generators_to_retire, by = c("PSTATABB" = "Plant State",
                                               "ORISPL" = "Plant ID",
                                               "GENID")) %>%
        #Here we set retirement year to 2100 if NA, meaning plants do not retire in this study
        mutate(retire_year = if_else(is.na(retire_year), 2100, retire_year)) %>%
        # Convert generation from GJ to EJ
        mutate(GENNTAN2_EJ = GENNTAN2 / 10^9) %>%
        #Here we want to make sure that max_GENNTAN2_EJ is actually higher that GENNTAN2, if not we set them equal
        mutate(max_GENNTAN2_EJ = if_else(max_GENNTAN2_EJ < GENNTAN2_EJ, GENNTAN2_EJ, max_GENNTAN2_EJ)) %>%
        #keep relevant columns
        # Note: for index we will need to add size, PLHTRT, PLNOXRTA2, PLSO2RTA2
        dplyr::select(PSTATABB, unique_ID, ORISPL, GENID, GENSTAT, gcam_fuel, Technology,
               NAMEPCAP, CFACT, GENYRONL, retire_year, GR_index, GENNTAN2_EJ, max_GENNTAN2_EJ) %>%
        dplyr::rename(gcam_technology = Technology)-> eGRID_generators # FINAL TABLE
      
      descrptive_statistics <- summary(eGRID_generators)
      
#------------------------------------------------- SECTION 5: DATA DOWNSCALING ---------------------------------------------
#### 5.1. Requirements ####
      #requirement 1: complete eGRID_generators table, so each generator has unique-ID, state, gcam_fuel, retirement year, GR index, positive 2020 electricity generation
      #requirement 2: complete GCAM_quotas table with scaled NEGATIVE retirement quotas and POSITIVE addition quotas. 
      # Note: table is missing some state-fuel combination when they do not exist
      
#### 5.2. Assumptions ####
      #A1: GCAM quotas are unique combinations of scenario, period, state & fuel
      #A2: generator can be retired, reduced, increased, or created (in this order)
      #A3: each generator has a unique fuel & state combination
      #A4: we are keeping combustion (oil, gas, biomass, coal) generators under status: OP, P, T, TS, L, OA, U, V (total of 11,105 generators)
      #A5: generator with a value of 0 in GENNTAN2_EJ is INCLUDED in this analysis, as new gen can be added to these (357 generators stay)
      #A6: generator with a negative value in GENNTAN2_EJ is excluded from this analysis (398 removed, 10,707 remaining)
      #A7: generator with no retirement year has been assigned a retirement year of 2100
      #A8: always retire according to eia schedule until GCAM quota is met. Do not allow overshoots from eia. If eia retires too much, do not retire.
      
#### 5.3. Prepare code ####
      #create tables for results and table for error log
      results <- data.frame()
      column_names <- c('scenario', 'period', 'state', 'fuel', 'technology', 'error_type', 'GCAM_retirement_quota', 'retire_quota','GCAM_add_quota', 'add_quota', 'full_retirement')
      error_log <- data.frame(matrix(ncol = length(column_names), nrow = 0))
      colnames(error_log) <- column_names

#### 5.4. Apply retirements and additions rules ####
      #cycle through each row of GCAM_quotas, which has unique combinations of scenario, period, state, fuel & technology
    for (i in 1:nrow(GCAM_quotas)) {
        #create variables
        scenario <- as.character(GCAM_quotas[i, "Scenario"])
        period <- as.numeric(GCAM_quotas[i, "Period"])
        state <- as.character(GCAM_quotas[i, "PSTATABB"])
        fuel <- as.character(GCAM_quotas[i, "gcam_fuel"])
        technology <- as.character(GCAM_quotas[i, "gcam_technology"])
        retire_quota <- as.numeric(GCAM_quotas[i, "retirement_quota"])
        add_quota <- as.numeric(GCAM_quotas[i, "addition_quota"])
        
        GCAM_retire_quota <- retire_quota #store original retirement quota
        GCAM_add_quota <- add_quota #store original addition quota
        
        relevant_generators <- eGRID_generators[eGRID_generators$PSTATABB == state & eGRID_generators$gcam_technology == technology,] #query with all generators that match state & technology 
        
      if (nrow(relevant_generators) > 0) { #if there exist generators in eGRID for this technology/state GCAM combination
          
          #create new columns
          relevant_generators$new_gen <- relevant_generators$GENNTAN2_EJ #duplicate GENNTAN2_EJ as new_gen (new electricity generation)
          relevant_generators$scenario <- scenario #add scenario column, fill with GCAM scenario
          relevant_generators$period <- period #add period column, fill with GCAM period
          relevant_generators$gen_status <- "active" #generator status is active, by default
          
          if(retire_quota < 0) {
            
      ##STEP 1: RETIRE BASED ON EIA SCHEDULE
        #Note: do not allow overshoot. If overshoot, then do not retire. GCAM has priority
            relevant_generators <- relevant_generators[order(relevant_generators$retire_year, relevant_generators$GENYRONL, decreasing=FALSE),] #order by retirement year first, then by year online, this way if the retirement year is the same, we will retire those generators that are older first
            
            for (a in 1:nrow(relevant_generators)){ #cycle through all generators that matched
              new_gen <- as.numeric(relevant_generators[a, "new_gen"])
              retirement_year <- as.numeric(relevant_generators[a, "retire_year"])
              
              if(retirement_year <= period){ #if generator was retired by EIA in this period
                if(retire_quota + new_gen <= 0){#if retirement does not derive on overshoot
                  relevant_generators$new_gen[a] <- 0 #set electricity generation to 0
                  relevant_generators$gen_status[a] <- "retired" #retire the generator
                  retire_quota <- retire_quota + new_gen #update quota
                }else{ #if retirement would cause overshoot, only retire part of generator
                  relevant_generators$new_gen[a] <- new_gen + retire_quota #generator will operate at reduced capacity, to meet remaining quota
                  retire_quota <- retire_quota + (new_gen - relevant_generators$new_gen[a]) #update retire quota
                }
              }
            }
            
            #after retiring all in EIA schedule, check how many generators have retired
            EIA_retirement_count <- sum(relevant_generators$gen_status == "retired") #count of all retired generators
            EIA_full_retirement <- EIA_retirement_count == nrow(relevant_generators) #true if all generators are retired
            
      ##STEP 2: RETIRE BASED ON GR INDEX
          #continue to retire generators using GR index if quota is still negative AND if active generators remain
            if(retire_quota < 0 && !EIA_full_retirement){
              
              relevant_generators <- relevant_generators[order(relevant_generators$GR_index,decreasing=FALSE),] #order by GR index
              stop <- FALSE #break out of nested loops
              
              for (b in 1:nrow(relevant_generators)){ #retire based on GRI to meet current quota
                if(stop == FALSE){
                  if(as.character(relevant_generators[b, "gen_status"]) == "active"){ #Filter for active generators
                    new_gen <- as.numeric(relevant_generators[b, "new_gen"]) #store generator's generation in a variable
                    if(new_gen != 0){ #only continue to retire generators that are active AND have positive generation.
                      if(retire_quota + new_gen <= 0){ #This is for full retirements, i.e. all generator is shutdown
                        relevant_generators$new_gen[b] <- 0 #set new_gen to 0
                        relevant_generators$gen_status[b] <- "retired" #change status to retired
                        retire_quota <- retire_quota + new_gen  #update quota
                      } else { #this is for the remainder of the quota, which is reduced from a generator, i.e. generator operates at reduced capacity
                        relevant_generators$new_gen[b] <- new_gen + retire_quota #generator will operate at reduced capacity, to meet remaining quota
                        stop <- TRUE
                        retire_quota <- retire_quota + (new_gen - relevant_generators$new_gen[b]) #update retire quota
                      }
                    }
                  }
                }
              }
            }
            
            #after retiring with GR index, check how many generators have retired
            retirement_count <- sum(relevant_generators$gen_status == "retired") #count of all retired generators
            full_retirement <- retirement_count == nrow(relevant_generators) #true if all generators have been retired
            
      ##STEP 3: FULL RETIREMENT LOGS
          #identify who is responsible for retiring all generators
            full_retirement_case <- ''
            if(EIA_full_retirement){ #if all have been retired by EIA schedule
              full_retirement_case <- 'EIA'
            } else if(full_retirement && EIA_retirement_count == 0){ #if none retired by EIA and all by GCAM quota
              full_retirement_case <- 'GCAM'
            } else if(full_retirement){
              full_retirement_case <- 'GCAM & EIA'
            }
            
          } else if (retire_quota > 0){ #If retire quota > 0, it's an error. If it is 0, then we do not really care
          error_log[nrow(error_log)+1,] <- c(scenario,period,state,fuel,technology,'positive_retirement_quota',GCAM_retire_quota,retire_quota,GCAM_add_quota,add_quota,NA)
          }## END OF RETIREMENTS
        
          
      ##STEP 4: CHECK FOR (UNMET) NEGATIVE RETIREMENT QUOTA
          #error_log if all generators were retired, or if overshoot occurred (either due to EIA schedule overshoot, or starting positive quota)
          retire_quota <- round(retire_quota, 17) #We use this to avoid errors appearing in the log that are insignificant and due to rounding
          if (retire_quota > 0) { #overshoot - although now we do not allow to overshoot
            error_log[nrow(error_log)+1,] <- c(scenario,period,state,fuel,technology,'EIA overshoot',GCAM_retire_quota,retire_quota,GCAM_add_quota,add_quota,NA)
          } else if (retire_quota < 0) { #if retire_quota is not met
            if (full_retirement) { # We could not meet the quota, even after retiring all generators
              error_log[nrow(error_log)+1,] <- c(scenario,period,state,fuel,technology,'retire quota unmet due to full retirement',GCAM_retire_quota,retire_quota,GCAM_add_quota,add_quota, full_retirement_case)
            } else { #This happens because we are not retiring generators with 0 generation. Thus you retire all generation, but not all generators, and you have still not met the quota.
              error_log[nrow(error_log)+1,] <- c(scenario,period,state,fuel,technology,'retire quota unmet but not full retirement (0 generation generators)',GCAM_retire_quota,retire_quota,GCAM_add_quota,add_quota,full_retirement_case)
            }
          }
          
      ##STEP 5: ADDRESSING (UNMET) NEGATIVE RETIREMENT QUOTA
          # Here we reduce the addition quota when we could not retire enough
          if (retire_quota < 0) { #If retire quota is still negative, then reduce add-quota. Then we do additions based on this new quota
            add_quota = add_quota + retire_quota
          } else {
            add_quota = add_quota
          }
          
          
      ##STEP 6: ADDITION - maxing out generators
          #grow active generators first, then build new generators if needed
          if(add_quota > 0) {
            
            #distribute growth to active generators until max generation is met, until add_quota is met
            active_gen_count <- sum(relevant_generators$gen_status == "active") #count all active generators
            if(active_gen_count > 0){ #if there are active generators remaining, we'll max them out their generation to try and meet quota
              relevant_generators <- relevant_generators[order(relevant_generators$GR_index,decreasing=TRUE),] #order by reverse of GR index, meaning we will first add to newer generators
              stop <- FALSE #don't know how to break out of nested loops, so using this quick fix for now...
              for (c in 1:nrow(relevant_generators)){
                if(stop == FALSE){
                  if(as.character(relevant_generators[c, "gen_status"]) == "active"){#filter for active generators
                    new_gen <- as.numeric(relevant_generators[c, "new_gen"])
                    max_gen <- as.numeric(relevant_generators[c, "max_GENNTAN2_EJ"])
                    delta_gen <- max_gen - new_gen #delta_gen will be either 0, or positive
                    if(delta_gen < add_quota){ #if quota won't be met, max out the generator. REVISE: do I add to 0 generators which had NA?
                      relevant_generators$new_gen[c] <- max_gen #set new_gen to max, redundant for delta_gen = 0
                      add_quota <- add_quota - delta_gen
                    } else { #if we meet the quota
                      relevant_generators$new_gen[c] <- new_gen + add_quota #add the last bit of the add_quota, and we're done!
                      add_quota <- 0
                      stop <- TRUE #exit the loop
                    }
                  }
                }
              }
            }
            
      ##STEP 7: ADDITION - creating new generators
            if(add_quota > 0){ #if quota still unmet after maxing out active generators, we must build new generators
              relevant_plants <- eGRID_generators[eGRID_generators$PSTATABB == state & eGRID_generators$gcam_technology == technology,] #This may be problem
              relevant_plants %>%
                group_by(ORISPL) %>%
                dplyr::summarise(plant_gen = sum(GENNTAN2_EJ)) %>%
                ungroup() -> relevant_plants #here we group by unique plant ID and sum GENNTAN2_EJ per plant
              state_gen <- sum(relevant_plants$plant_gen) #sum of generation for state & technology
              
              for (d in 1:nrow(relevant_plants)){ #for each unique plant
                plant_id <- as.double(relevant_plants[d, "ORISPL"])
                plant_gen <- as.numeric(relevant_plants[d, "plant_gen"])
                plant_add <- ifelse(state_gen == 0, add_quota/nrow(relevant_plants), add_quota*plant_gen/state_gen) #weighted distribution of remaining addition quota
                new_row <- data.frame(
                  PSTATABB = as.character(GCAM_quotas[i, "PSTATABB"]),
                  ORISPL = as.double(plant_id),
                  gcam_fuel = as.character(GCAM_quotas[i, "gcam_fuel"]),
                  gcam_technology = as.character(GCAM_quotas[i, "gcam_technology"]),
                  scenario = as.character(GCAM_quotas[i, "Scenario"]),
                  period = as.numeric(GCAM_quotas[i, "Period"]),
                  new_gen = plant_add,
                  gen_status = "active"
                )
                relevant_generators <- bind_rows(relevant_generators,new_row)
              }
              add_quota <- 0
            }
            
          add_quota <- round(add_quota, 18) #We use this to avoid errors appearing in the log that are insignificant and due to rounding
            
          } else if (add_quota < 0) { #if quota is negative, write it here
            #This can happen when the unmet retirement is not compensated with the add_quota. This is: you still need to retire more, even after using your add_quota as retirement
            error_log[nrow(error_log)+1,] <- c(scenario,period,state,fuel,technology,'negative addition quota',GCAM_retire_quota,retire_quota,GCAM_add_quota,add_quota,NA)
          }
          
          
      ##STEP 8: WRITE RESULTS
          results <- rbind(results, relevant_generators) #Here we write results for the first half, which has generation for existing technologies, i.e. there were generators found in eGRID in a given state
          
          
      ##STEP 9: FOR NEW TECHNOLOGIES
      } else { #if no generators found in eGRID for technology/state combination, we place them based on same fuel
          relevant_fuel <- eGRID_generators[eGRID_generators$PSTATABB == state & eGRID_generators$gcam_fuel == fuel,] #list of all generators in state that use fuel
          
          if(nrow(relevant_fuel) > 0){ #if there are generators of the current fuel (although not technology)
            
            net_quota <- add_quota + retire_quota
            
            if(net_quota > 0){ # If net quota is positive, thus addition
              relevant_fuel %>%
                group_by(ORISPL) %>%
                dplyr::summarise(fuel_gen = sum(GENNTAN2_EJ)) %>%
                ungroup() -> relevant_fuel #here we group by unique plant ID and sum GENNTAN2_EJ of specific fuel, per plant
              state_gen <- sum(relevant_fuel$fuel_gen) #sum of fuel generation for state
              
              for (e in 1:nrow(relevant_fuel)){ #build technology generators in same-fuel plants, with fuel-weighted distribution of net quota (addition - retirement)
                plant_id <- as.double(relevant_fuel[e, "ORISPL"])
                plant_gen <- as.numeric(relevant_fuel[e, "fuel_gen"])
                plant_add <-net_quota*plant_gen/state_gen #weighted distribution of remaining addition quota
                
                new_row <- data.frame(
                  PSTATABB = as.character(GCAM_quotas[i, "PSTATABB"]),
                  ORISPL = as.double(plant_id),
                  gcam_fuel = as.character(GCAM_quotas[i, "gcam_fuel"]),
                  gcam_technology = as.character(GCAM_quotas[i, "gcam_technology"]),
                  scenario = as.character(GCAM_quotas[i, "Scenario"]),
                  period = as.numeric(GCAM_quotas[i, "Period"]),
                  new_gen = plant_add,
                  gen_status = "active")
                relevant_generators <- bind_rows(relevant_generators,new_row)
              }
            } else if (net_quota == 0) { #if retire is equal to addition - this error does not matter
              error_log[nrow(error_log)+1,] <- c(scenario,period,state,fuel,technology,'tech nonexist, but net quota is 0',GCAM_retire_quota,retire_quota,GCAM_add_quota,add_quota,NA)
            } else if (net_quota < 0) { #if retire is greater than addition
              error_log[nrow(error_log)+1,] <- c(scenario,period,state,fuel,technology,'tech nonexist, and net quota is negative',GCAM_retire_quota,retire_quota,GCAM_add_quota,add_quota,NA)
            }
            
          } else { #if fuel does not exist in state, log the error and proceed with a different approach
            error_log[nrow(error_log)+1,] <- c(scenario,period,state,fuel,technology,'tech nonexist & fuel nonexist',GCAM_retire_quota,retire_quota,GCAM_add_quota,add_quota,NA)
            
      ##STEP 10: allocate fuels that do not exist
          #A. We assume that this generation is added to plants that have natural gas
            relevant_fuel <- eGRID_generators[eGRID_generators$PSTATABB == state & eGRID_generators$gcam_fuel == "Gas",] #list of all generators in state that use natural gas
            
            if(nrow(relevant_fuel) > 0){ #if there are generators with natural gas
              
              net_quota <- add_quota + retire_quota
            
              if(net_quota > 0){
                relevant_fuel %>%
                  group_by(ORISPL) %>%
                  dplyr::summarise(fuel_gen = sum(GENNTAN2_EJ)) %>%
                  ungroup() -> relevant_fuel #here we group by unique plant ID and sum GENNTAN2_EJ of specific fuel, per plant
                state_gen <- sum(relevant_fuel$fuel_gen) #sum of gas generation for state
                
                for (e in 1:nrow(relevant_fuel)){ #build technology generators in same-fuel plants, with fuel-weighted distribution of net quota (addition - retirement)
                  plant_id <- as.double(relevant_fuel[e, "ORISPL"])
                  plant_gen <- as.numeric(relevant_fuel[e, "fuel_gen"])
                  plant_add <-net_quota*plant_gen/state_gen #weighted distribution of remaining addition quota
                  new_row <- data.frame(
                    PSTATABB = as.character(GCAM_quotas[i, "PSTATABB"]),
                    ORISPL = as.double(plant_id),
                    gcam_fuel = as.character(GCAM_quotas[i, "gcam_fuel"]),
                    gcam_technology = as.character(GCAM_quotas[i, "gcam_technology"]),
                    scenario = as.character(GCAM_quotas[i, "Scenario"]),
                    period = as.numeric(GCAM_quotas[i, "Period"]),
                    new_gen = plant_add,
                    gen_status = "active")
                  relevant_generators <- bind_rows(relevant_generators,new_row)
                }
                
              }else if (net_quota == 0) { # If no change is expected
                error_log[nrow(error_log)+1,] <- c(scenario,period,state,fuel,technology,'tech nonexist & fuel nonexist & gas exist, but net quota is 0',GCAM_retire_quota,retire_quota,GCAM_add_quota,add_quota,NA)
              }else if (net_quota < 0) { #if retire is greater than addition
                error_log[nrow(error_log)+1,] <- c(scenario,period,state,fuel,technology,'tech nonexist & fuel nonexist & gas exist, and net quota is negative',GCAM_retire_quota,retire_quota,GCAM_add_quota,add_quota,NA)
              }
              
              #B. We record the error
            } else {
              error_log[nrow(error_log)+1,] <- c(scenario,period,state,fuel,technology,'tech nonexist & fuel nonexist & gas nonexist',GCAM_retire_quota,retire_quota,GCAM_add_quota,add_quota,NA)
            }
          }
          results <- rbind(results, relevant_generators)
      }
    }

      
#### 5.5. Analyze outputs ####
    #### 5.5.1. Error log ####
      #Note: we were recording 10 types of errors, only 5 ended up happening after our code changes. These 5 are:
      #1. retire quota unmet due to full retirement -> for these cases we have reduced the addition quota. Since we cannot retire enough, then we do not add that much.
      #2. retire quota unmet but not full retirement (0 generation generators) -> these happens because there are some generators with 0 generation in 2020, so they do not retire, but because they exist there is not a full retirement of generators. The solution is also to reduce the addition quota.
      #3. tech nonexist, but net quota is 0 -> this does not really matter, as net quota is 0
      #4.	tech nonexist & fuel nonexist-> technology and fuel are new for that state, for these cases we add the quota to plants with natural gas generators, as plants with natural gas generators tend to have other fuels more often than any other fuel
      #5. tech nonexist & fuel nonexist & gas exist, but net quota is 0 -> when net quota is 0, we record it, but it does not matter
      error_log %>%
        group_by(scenario, period, error_type) %>%
        count()  %>%
        ungroup() -> error_log_counts
      
    #### 5.5.2. US level ####  
      #GCAM generation in 2030 and 2050
      elec_gen_tech_final_states_EJ_detailed %>%
        filter(Fuel %in% combustion_fuels) %>%
        filter(year == 2020 | year == 2030 | year == 2050) %>%
        group_by(year, Scenario) %>%
        dplyr::summarise(GCAM_generation = sum(generation_EJ)) %>%
        ungroup() -> GCAM_elec_generation_total
      
      results %>%
        group_by(scenario, period) %>%
        dplyr::summarise(eGRID_generation = sum(new_gen)) %>%
        ungroup() -> eGRID_elec_generatio_total #Totals by scenario and period
      
      eGRID_elec_generatio_total %>%
        left_join(GCAM_elec_generation_total, by = c("scenario" = "Scenario", "period" = "year")) -> total_reuslts_comparison
      
    #### 5.5.3. state level ####
      #GCAM generation in 2030 and 2050
      elec_gen_tech_final_states_EJ_detailed %>%
        filter(Fuel %in% combustion_fuels) %>%
        filter(year == 2020 | year == 2030 | year == 2050) %>%
        group_by(year, Scenario, region) %>%
        dplyr::summarise(GCAM_generation = sum(generation_EJ)) %>%
        ungroup() -> GCAM_elec_generation_total_states
      
      results %>%
        group_by(scenario, period, PSTATABB) %>%
        dplyr::summarise(eGRID_generation = sum(new_gen)) %>%
        ungroup() -> eGRID_elec_generatio_total_states #Totals by scenario and period
      
      eGRID_elec_generatio_total_states %>%
        left_join(GCAM_elec_generation_total_states, by = c("scenario" = "Scenario", 
                                                            "period" = "year", 
                                                            "PSTATABB" = "region")) %>%
        mutate(per_difference = ((GCAM_generation - eGRID_generation ) / GCAM_generation) * 100)-> total_reuslts_comparison_states
      
      scatter_plot <- ggplot() +
        geom_point(data = total_reuslts_comparison_states, aes(x = eGRID_generation, y = GCAM_generation)) +
        facet_grid(scenario ~ period, switch="y") +
        ggtitle("Electricity Generation Comparison: eGRID vs. GCAM") +
        xlab("eGRID") +
        ylab("GCAM") +
        theme_minimal()
      
      # Print the scatter plot
      print(scatter_plot)
      
    #### 5.5.4. state level 2020 ####
      elec_gen_tech_final_states_EJ_detailed %>%
        filter(Fuel %in% combustion_fuels) %>%
        filter(year == 2020) %>%
        group_by(year, Scenario, region) %>%
        dplyr::summarise(GCAM_generation = sum(generation_EJ)) %>%
        ungroup() -> GCAM_elec_generation_total_states_2020
      
      eGRID_generators %>%
        group_by(PSTATABB) %>%
        dplyr::summarise(eGRID_generation = sum(GENNTAN2_EJ)) %>%
        ungroup() -> eGRID_elec_generatio_total_states_2020
      
      eGRID_elec_generatio_total_states_2020 %>%
        left_join(GCAM_elec_generation_total_states_2020, by = c("PSTATABB" = "region")) %>%
        mutate(per_difference = ((GCAM_generation - eGRID_generation ) / GCAM_generation) * 100)-> total_reuslts_comparison_states_2020
        
      scatter_plot_2020 <- ggplot() +
        geom_point(data = total_reuslts_comparison_states_2020, aes(x = eGRID_generation, y = GCAM_generation)) +
        facet_grid(Scenario ~ ., switch="y") +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Add 1:1 line
        ggtitle("2020 Electricity Generation Comparison: eGRID vs. GCAM") +
        xlab("eGRID") +
        ylab("GCAM") +
        theme_minimal()
      # Print the scatter plot
      print(scatter_plot_2020)
      
#### 5.6. Map results ####
    #### 5.6.1. general ####
      #Order
      scenario_order <- c("Reference",
                          "Net-zero, High BECCS, High DAC",
                          "Net-zero, Low BECCS, Low DAC")
      
      #Import data
      egrid_data_plant_2020_metric <- read_csv("data/egrid_2020_data_generation_plant.csv", skip = 1) %>%
        dplyr::select(PSTATABB, ORISPL, LAT, LON)
      
      # Get map data for the US
      us_map <- map_data("state")
      
    #### 5.6.2. Manipulate data 2020 ####
      states_abb_mapping_small <- data.frame(State = tolower(state.name), Abbreviation = state.abb) 
      
      eGRID_generators %>%
        left_join(egrid_data_plant_2020_metric, by = c("PSTATABB", "ORISPL")) %>%
        filter(!PSTATABB %in% c("HI", "AK")) %>%
        left_join(states_abb_mapping_small, by = c("PSTATABB" = "Abbreviation")) %>%
        mutate(State = if_else(PSTATABB == "DC", "district of columbia", State))-> generators_2020
      
      # Create the map plot
      map_plot_2020 <- ggplot() +
        geom_polygon(data = generators_2020, aes(x = LON, y = LAT, group = unique_ID), fill = "lightgray") +
        geom_point(data = generators_2020, aes(x = LON, y = LAT, size = GENNTAN2_EJ, color = gcam_technology), shape = 16) +
        geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "black", size = 0.5) +
        ggtitle( "Electricity Generation by Fuel and Size by 2020") +
        scale_color_manual(values = elec_gen_combustion_color) +
        scale_size_continuous(name = "Generation Size (EJ)", breaks = c(0, 0.01, 0.02, 0.03, 0.04), labels = c("0", "0.01", "0.02", "0.03", "0.04"),
                              range = c(1, 5)) +  # Adjust the range according to your data
        coord_fixed(1.3) +
        my_theme
      ggsave("figures/downscaling_electricity/2020/US_downscaled_electricity_2020.png", dpi=600/2, width=6000/300, height=3000/300)
      ggsave("figures/downscaling_electricity/2020/US_downscaled_electricity_2020.svg", dpi=600/2, width=6000/300, height=3000/300)
      # Display the map plot
      print(map_plot_2020)
      
      
      # Loop through each state
      for (state_name in unique(generators_2020$State)) {
        # Filter data for the current state
        state_data <- generators_2020 %>%
          filter(State == state_name)
        
        # Filter us_map for the current state
        state_map <- us_map %>%
          filter(region == tolower(state_name))
        
        # Create the map plot for the current state
        map_plot <- ggplot() +
          geom_polygon(data = state_data, aes(x = LON, y = LAT, group = unique_ID), fill = "lightgray") +
          geom_point(data = state_data, aes(x = LON, y = LAT, size = GENNTAN2_EJ, color = gcam_technology), shape = 16) +
          geom_path(data = state_map, aes(x = long, y = lat, group = group), color = "darkgray", size = 0.75) +
          ggtitle(paste("Electricity Generation by Fuel and Size by 2020 in", state_name)) +
          scale_color_manual(values = elec_gen_combustion_color) +
          scale_size_continuous(name = "Generation Size (EJ)", breaks = c(0, 0.01, 0.02, 0.03, 0.04), labels = c("0", "0.01", "0.02", "0.03", "0.04"),
                                range = c(1, 5)) +  # Adjust the range according to your data
          coord_fixed(1.3) +
          my_theme 
        
        # Save the map plot for the current state
        ggsave(paste("figures/downscaling_electricity/2020/downscaled_electricity_2020_", state_name, ".png", sep = ""), 
               plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
        ggsave(paste("figures/downscaling_electricity/2020/downscaled_electricity_2020_", state_name, ".svg", sep = ""), 
               plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
      }
    
    #### 5.6.3. Manipulate data future ####
      results %>%
        left_join(egrid_data_plant_2020_metric, by = c("PSTATABB", "ORISPL"))  %>%
        mutate(scenario = factor(scenario, levels = scenario_order)) %>%
        filter(!PSTATABB %in% c("HI", "AK"),
               gen_status != "retired",
               period == 2050) %>%
        dplyr::rename(`Generation (EJ)` = new_gen,
               Fuel = gcam_fuel) %>%
        mutate(GENID = if_else(is.na(GENID), "new", GENID)) %>%
        mutate(unique_ID = if_else(is.na(unique_ID), paste(ORISPL, GENID, sep = "_"), unique_ID)) -> generators_future #Active generators in 2050
     
      #Figure for all US per scenario
       ## Define the scenarios
      scenarios <- unique(generators_future$scenario)
      
      # Loop through each scenario
      for (scenario in scenarios) {
        # Filter the table for the current scenario
        filtered_data <- generators_future %>%
          filter(scenario == !!scenario)
        
        # Create the map plot for the current scenario
        map_plot <- ggplot() +
          geom_polygon(data = filtered_data, aes(x = LON, y = LAT, group = unique_ID), fill = "lightgray") +
          geom_point(data = filtered_data, aes(x = LON, y = LAT, size = `Generation (EJ)`, color = gcam_technology), shape = 16) +
          geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "darkgray", size = 0.5) +
          ggtitle(paste("2050 Electricity Generation by Fuel and Size - Scenario:", scenario)) +
          scale_color_manual(values = elec_gen_combustion_color) +
          scale_size(name = "Generation Size", breaks = c(0, 0.05, 0.1, 0.15, 0.20, 0.25), labels = c("0", "0.05", "0.1", "0.15", "0.2", "0.25")) +  # Adjust the range according to your data
          coord_fixed(1.3) +
          my_theme
        
        # Save the map plot for the current scenario
        ggsave(paste("figures/downscaling_electricity/2050/elec_gen/2050_national_downscaled_electricity_", scenario, ".png", sep = ""), plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
        ggsave(paste("figures/downscaling_electricity/2050/elec_gen/2050_national_downscaled_electricity_", scenario, ".svg", sep = ""), plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
      }
      
      
      #Figure for each state
      us_map %>% 
        left_join(states_abb_mapping_small, by = c("region" = "State")) %>%
        mutate(Abbreviation = if_else(region == "district of columbia", "DC", Abbreviation))-> us_map_abb
      
      # Loop through each state
      for (state in unique(generators_future$PSTATABB)) {
        # Filter the table for the current state
        filtered_data <- generators_future %>%
          filter(PSTATABB == !!state) %>%
          filter(scenario %in% c("Net-zero, High BECCS, High DAC", "Net-zero, Low BECCS, Low DAC"))
        
        # Filter us_map for the current state
        state_map <- us_map_abb %>%
          filter(Abbreviation == state)
        
          # Create the map plot for the current scenario
          map_plot <- ggplot() +
            geom_polygon(data = filtered_data, aes(x = LON, y = LAT, group = unique_ID), fill = "lightgray") +
            geom_point(data = filtered_data, aes(x = LON, y = LAT, size = `Generation (EJ)`, color = gcam_technology), shape = 16) +
            geom_path(data = state_map, aes(x = long, y = lat, group = group), color = "darkgray", size = 0.5) +
            ggtitle(paste("2050 Electricity Generation by Fuel and Size - State:", state)) +
            scale_color_manual(values = elec_gen_combustion_color) +
            scale_size(name = "Generation Size", breaks = c(0, 0.01, 0.02, 0.03, 0.04), labels = c("0", "0.01", "0.02", "0.03", "0.04")) +  # Adjust the range according to your data, 
            #for scale sixe: breaks = c(0, 0.05, 0.1, 0.15, 0.20, 0.25), labels = c("0", "0.05", "0.1", "0.15", "0.2", "0.25")
            coord_fixed(1.3) +
            facet_wrap(~scenario, nrow = 1) +  # Facet by scenario
            my_theme
          
          # Save the map plot for the current scenario and state
          ggsave(paste("figures/downscaling_electricity/2050/elec_gen/2050_state_downscaled_electricity_", state, ".png", sep = ""), 
                 plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
          ggsave(paste("figures/downscaling_electricity/2050/elec_gen/2050_state_downscaled_electricity_", state, ".svg", sep = ""), 
                 plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
      }
    
    #### 5.6.6. Histogram ####
      generators_future %>%
        group_by(Fuel, scenario, period, gcam_technology) %>%
        dplyr::summarise(Generation = sum(`Generation (EJ)`)) %>%
        ungroup() -> generators_future_hist
      hist_plot_future <- ggplot(generators_future_hist, aes(x = gcam_technology, y = Generation)) +
        geom_col(aes(fill = gcam_technology, group = gcam_technology)) +
        scale_fill_manual(values = elec_gen_combustion_color) +
        facet_grid(period ~ scenario, switch="y") +
        ggtitle("Total Generation by Technology") +
        xlab("Technology") +
        ylab("Total Generation (EJ)")+
        my_theme
      ggsave("figures/downscaling_electricity/downscaled_electricity_fuels.png", dpi=600/2, width=6000/300, height=3000/300)
      ggsave("figures/downscaling_electricity/downscaled_electricity_fuels.svg", dpi=600/2, width=6000/300, height=3000/300)
      print(hist_plot_future)
      
    #### 5.6.7. Retired ####
      results %>%
        left_join(egrid_data_plant_2020_metric, by = c("PSTATABB", "ORISPL"))  %>%
        mutate(scenario = factor(scenario, levels = scenario_order)) %>%
        filter(!PSTATABB %in% c("HI", "AK"),
               gen_status == "retired",
               period == 2050) %>%
        dplyr::rename(`Generation (EJ)` = new_gen,
               Fuel = gcam_fuel) -> generators_future_retired
      
      #Figure for all US per scenairo
      ## Define the scenarios
      scenarios <- unique(generators_future_retired$scenario)
      
      # Loop through each scenario
      for (scenario in scenarios) {
        # Filter the table for the current scenario
        filtered_data <- generators_future_retired %>%
          filter(scenario == !!scenario)
        
        # Create the map plot for the current scenario
        map_plot <- ggplot() +
          geom_polygon(data = filtered_data, aes(x = LON, y = LAT, group = unique_ID), fill = "lightgray") +
          geom_point(data = filtered_data, aes(x = LON, y = LAT, size = `Generation (EJ)`, color = gcam_technology), shape = 16) +
          geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "darkgray", size = 0.5) +
          ggtitle(paste("2050 Retired Electricity Generation by Fuel and Size - Scenario:", scenario)) +
          scale_color_manual(values = elec_gen_combustion_color) +
          scale_size(name = "Generation Size") +  # Adjust the range according to your data
          coord_fixed(1.3) +
          my_theme
        
        # Save the map plot for the current scenario
        ggsave(paste("figures/downscaling_electricity/2050/elec_gen/2050_national_retired_downscaled_electricity_", scenario, ".png", sep = ""), 
               plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
        ggsave(paste("figures/downscaling_electricity/2050/elec_gen/2050_national_retired_downscaled_electricity_", scenario, ".svg", sep = ""), 
               plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
      }
      
      #Here we want to compare High vs low
      generators_future %>%
        filter(period == 2050) %>%
        spread(scenario, `Generation (EJ)`) %>%
        mutate(`Net-zero, Low BECCS, Low DAC` = if_else(is.na(`Net-zero, Low BECCS, Low DAC`), 0, `Net-zero, Low BECCS, Low DAC`),
               `Net-zero, High BECCS, High DAC` = if_else(is.na(`Net-zero, High BECCS, High DAC`), 0, `Net-zero, High BECCS, High DAC`)) -> generators_future_ggplot
      
      scatter_plot <- ggplot() +
        geom_point(data = generators_future_ggplot, aes(x = `Net-zero, High BECCS, High DAC`, y = `Net-zero, Low BECCS, Low DAC`, color = gcam_technology)) +
        scale_color_manual(values = elec_gen_combustion_color) +
        ggtitle("Electricity Generation Comparison: Scenario A vs. Scenario B") +
        xlab("High CDR") +
        ylab("Low CDR") +
        theme_minimal()
      
      # Print the scatter plot
      print(scatter_plot)

    #### 5.6.8. Histogram capacity factors ####
      eGRID_generators %>%
        filter(CFACT <= 1) -> eGRID_generators_CFACT # N = 10,670 (we lost 28 generators)
      
      hist_cap_fact <- ggplot(eGRID_generators_CFACT, aes(x = CFACT)) +
        geom_histogram(binwidth = 0.05, position = "dodge", color = "black", alpha = 0.7) +
        facet_wrap(~gcam_technology, scales = "free") +
        ggtitle("Total Generation by Technology") +
        xlab("Capacity Factor") +
        ylab("Frequency")+
        scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
        my_theme
      ggsave("figures/downscaling_electricity/hist_CFACT.png", dpi=600/2, width=6000/300, height=3000/300)
      ggsave("figures/downscaling_electricity/hist_CFACT.svg", dpi=600/2, width=6000/300, height=3000/300)
      print(hist_cap_fact)
      
#------------------------------------------------- SECTION 6: CALCULATE EMISSION FACTORS --------------------------------------
##### 6.1. Get electricity emission factors from eGRID####  
      egrid_data_plant_2020_metric_fuels %>%
        group_by(PSTATABB, gcam_fuel) %>%
        mutate(average_NOx = mean(PLNOXRTA2, na.rm = TRUE)) %>% #kg/GJ
        mutate(average_SO2 = mean(PLSO2RTA2, na.rm = TRUE)) %>% #kg/GJ
        mutate(median_NOx = median(PLNOXRTA2, na.rm = TRUE)) %>% #kg/GJ
        mutate(median_SO2 = median(PLSO2RTA2, na.rm = TRUE)) %>% #kg/GJ
        ungroup() %>%
        #keep unique values
        dplyr::select(PSTATABB, gcam_fuel, average_NOx, median_NOx, average_SO2, median_SO2) %>%
        distinct()-> eGRID_average_EF
      
      descriptive_statistics <- summary(egrid_data_plant_2020_metric_fuels)
      
      
##### 6.2. Calculate electricity emission factors GCAM #### 
      air_pollution_sources_electricity <- getQuery(prj, "air pollution nonCO2 emissions by elec tech (USA)")
      
      air_pollution_sources_electricity %>%
        mutate(subsector = gsub(",depth=1","", subsector...6)) %>%
        filter(year > 2010) %>%
        dplyr::select(-sector, -subsector...5, -subsector...6) %>%
        #Here we bring in fuels
        left_join(tech_mapping, by = "technology") %>%
        #Here we want to keep technology detail, but without water info
        left_join(GCAM_elec_tech_mapping, by = "technology") %>%
        #Now get state total by technology and fuel, ignoring water info 
        group_by(Units, scenario, year, agg_tech, Technology, region, ghg) %>%
        dplyr::summarise(emissions_Tg = sum(value)) %>%
        ungroup() %>%
        separate(agg_tech, c("Fuel", "CCS"), sep = " ") %>%
        #Note: NAs appear in the CCS column when tech has no CCS. We ignore
        left_join(scenario_mapping, by = "scenario") %>%
        dplyr::select(-CCS, -scenario, -`Policy USA`, -`Policy Global`, -`BECCS`, -`DAC`, -Units) -> air_pollution_electricity_final
      
      elec_gen_tech_final_states_EJ_detailed %>%
        filter(Fuel %in% combustion_fuels) %>%
        dplyr::select(year, Scenario, region, Fuel, Technology, generation_EJ) %>%
        left_join(air_pollution_electricity_final, by = c("year", "Fuel", "Technology", "region", "Scenario")) %>%
        #There are 4 instances for OR and VT oil where there is small generation in 2045 and 2050, but no emissions, so we take them out
        filter(!is.na(emissions_Tg)) %>%
        #Here we want emission factors in kg/GJ, which is the same as Tg/EJ
        mutate(EF = emissions_Tg / generation_EJ) %>%
        #filter(year == 2030 | year == 2050) %>%
        dplyr::select(-generation_EJ, -emissions_Tg) -> emission_factors_GCAM
      
        #Graph the EF means at a US level
        emission_factors_GCAM %>%
          group_by(year, Scenario, Fuel, Technology, ghg) %>%
          dplyr::summarise(naitonal_mean_EF = mean(EF)) %>%
          ungroup() -> emission_factors_GCAM_mean_complete
        
        p <- ggplot() + geom_line(data=emission_factors_GCAM_mean_complete, aes(x=year, y=naitonal_mean_EF, colour = Scenario, group = Scenario), size = 1) +
          facet_grid(Technology ~ ghg, scales = "free_y") +
          theme(legend.position = "bottom",
                strip.text = element_text(size=8))
        ggsave("figures/downscaling_electricity/EF_means_historical.png", dpi=600/2, width=6000/300, height=3000/300)
        ggsave("figures/downscaling_electricity/EF_means_historical.svg", dpi=600/2, width=6000/300, height=3000/300)
        
        emission_factors_GCAM %>%
          #Keep only 2020, 2030 and 2050 EF in Tg/EJ
          filter(year == 2020 | year == 2030 | year == 2050)-> emission_factors_GCAM_filtered #OUTPUT TABLE
        
        
#------------------------------------------------- SECTION 7: CALCULATE EMISSIONS --------------------------------------
#### 7.1. Calculate emissions ####
    #### 7.1.1. Get total future emissions ####
      #Filter for active generators only
      results %>% 
        filter(gen_status == "active") -> results_active #Active generators in eGRID for 2030 and 2050
      
      #Here calculate emissions
      results_active %>%
        #join table with emission factors from GCAM
        left_join(emission_factors_GCAM_filtered, by = c("PSTATABB" = "region",
                                                "period" = "year",
                                                "scenario" = "Scenario",
                                                "gcam_fuel" = "Fuel",
                                                "gcam_technology" = "Technology"),
                  relationship = "many-to-many") %>%
        #here calculate total emissions, represented in Tg
        mutate(emissions_Tg = new_gen * EF)-> results_emissions #Total emissions in future with GCAM EF
      
      #There 1 row where there are NAs: VT 2050 refined liquids (steam/CT) Low scenario
      #This is because there is no generation or pollution in GCAM for these cases, but based on eGRID and GCAM changes there is some small.
      #To solve these cases we bring in EF means from US for that period, scenario, fuel, technology, and pollutant
      emission_factors_GCAM_filtered %>%
        group_by(year, Scenario, Fuel, Technology, ghg) %>%
        dplyr::summarise(EF = mean(EF)) %>%
        ungroup() -> emission_factors_GCAM_mean # this table has the national average EF by year, scenario, fuel, technology, and ghg
      
      results_emissions %>%
        filter(is.na(ghg)) %>%
        dplyr::select(-ghg, -EF) %>%
        left_join(emission_factors_GCAM_mean, by = c("scenario" = "Scenario",
                                                    "gcam_fuel" = "Fuel",
                                                    "gcam_technology" = "Technology",
                                                    "period" = "year")) %>%
        mutate(emissions_Tg = new_gen * EF)-> results_emissions_NA
      
      results_emissions %>%
        filter(!is.na(ghg)) %>%
        bind_rows(results_emissions_NA)-> results_emissions_final #This table has the final future emissions based on eGRID generation and GCAM EF (without any NAs)

      results_emissions_final %>%
        group_by(PSTATABB, scenario, period, ghg) %>%
        dplyr::summarise(eGRID_sum_emissions_Tg = sum(emissions_Tg)) %>%
        ungroup() -> results_emissions_totals #Totals at a US level
      
      #NOx
      results_emissions_totals %>%
        filter(scenario != "Reference",
               period == 2050,
               ghg == "NOx") %>%
        spread(scenario, eGRID_sum_emissions_Tg) %>%
        mutate(difference = `Net-zero, High BECCS, High DAC` - `Net-zero, Low BECCS, Low DAC`,
               relative = difference * 100 / `Net-zero, High BECCS, High DAC`)-> high_vs_low_difference_NOx 
      
      #SO2
      results_emissions_totals %>%
        filter(scenario != "Reference",
               period == 2050,
               ghg == "SO2_1") %>%
        spread(scenario, eGRID_sum_emissions_Tg) %>%
        mutate(difference = `Net-zero, High BECCS, High DAC` - `Net-zero, Low BECCS, Low DAC`,
               relative = difference * 100 / `Net-zero, High BECCS, High DAC`)-> high_vs_low_difference_SO2 
      
      results_emissions_totals %>%
        filter(scenario != "Reference",
               period == 2050,
               ghg == "PM2.5") %>%
        spread(scenario, eGRID_sum_emissions_Tg) %>%
        mutate(difference = `Net-zero, High BECCS, High DAC` - `Net-zero, Low BECCS, Low DAC`,
               relative = difference * 100 / `Net-zero, High BECCS, High DAC`)-> high_vs_low_difference 
      
    #### 7.1.2. Graph emissions ####
      results_emissions_final %>%
        filter(ghg =="PM2.5", period == 2050)%>% #Here we can change to get each individual gas
        filter(!PSTATABB %in% c("HI", "AK")) %>%
        left_join(egrid_data_plant_2020_metric, by = c("PSTATABB", "ORISPL")) %>%
        mutate(scenario = factor(scenario, levels = scenario_order)) %>%
        mutate(unique_ID = if_else(is.na(unique_ID), paste(ORISPL, GENID, sep = "_"), unique_ID))-> results_emissions_filtered
      
      # Define the scenarios
      scenarios <- unique(results_emissions_filtered$scenario)
      
      # Loop through each scenario
      for (scenario in scenarios) {
        # Filter the table for the current scenario
        filtered_data <- results_emissions_filtered %>%
          filter(scenario == !!scenario)
        
        # Create the map plot for the current scenario
        map_plot <- ggplot() +
          geom_polygon(data = filtered_data, aes(x = LON, y = LAT, group = unique_ID), fill = "lightgray") +
          geom_point(data = filtered_data, aes(x = LON, y = LAT, size = emissions_Tg, color = gcam_technology), shape = 16) +
          geom_path(data = us_map, aes(x = long, y = lat, group = group), color = "darkgray", size = 0.5) +
          ggtitle(paste("PM2.5 Pollution from Electricity Generation in 2050 - Scenario:", scenario)) +
          scale_color_manual(values = elec_gen_combustion_color) +
          scale_size(name = "Pollution (Tg)", breaks = c(0, 0.001, 0.002, 0.003), labels = c("0", "0.001", "0.002", "0.003")) +  # Adjust the range according to your data
          coord_fixed(1.3) +
          theme(panel.background = element_rect(fill = "white"),
                plot.background = element_rect(fill = "white"),
                plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
                panel.grid.minor = element_blank(),
                axis.title.x = element_blank(),
                axis.text.x  = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y  = element_blank(),
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 15))
        
        # Save the map plot for the current scenario
        ggsave(paste("figures/downscaling_electricity/2050/PM25_emissions/national_downscaled_pollution_PM25_", scenario, ".png", sep = ""), plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
        ggsave(paste("figures/downscaling_electricity/2050/PM25_emissions/national_downscaled_pollution_PM25_", scenario, ".svg", sep = ""), plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
      }
      
      
      ### Loop through each state
      for (state in unique(results_emissions_filtered$PSTATABB)) {
        # Filter the table for the current state
        filtered_data <- results_emissions_filtered %>%
          filter(PSTATABB == !!state) %>%
          filter(scenario %in% c("Net-zero, High BECCS, High DAC", "Net-zero, Low BECCS, Low DAC"))
        
        # Filter us_map for the current state
        state_map <- us_map_abb %>%
          filter(Abbreviation == state)
        
        # Create the map plot for the current scenario
        map_plot <- ggplot() +
          geom_polygon(data = filtered_data, aes(x = LON, y = LAT, group = unique_ID), fill = "lightgray") +
          geom_point(data = filtered_data, aes(x = LON, y = LAT, size = emissions_Tg, color = gcam_technology), shape = 16) +
          geom_path(data = state_map, aes(x = long, y = lat, group = group), color = "darkgray", size = 0.5) +
          ggtitle(paste("2050 PM2.5 from Electricity Generation - State:", state)) +
          scale_color_manual(values = elec_gen_combustion_color) +
          scale_size(name = "Emissions (Tg)") +  # Adjust the range according to your data, 
          #for scale sixe: breaks = c(0, 0.05, 0.1, 0.15, 0.20, 0.25), labels = c("0", "0.05", "0.1", "0.15", "0.2", "0.25")
          coord_fixed(1.3) +
          facet_wrap(~scenario, nrow = 1) +  # Facet by scenario
          my_theme
        
        # Save the map plot for the current scenario and state
        ggsave(paste("figures/downscaling_electricity/2050/PM25_emissions/2050_state_PM25_emissions_electricity_", state, ".png", sep = ""),  plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
        ggsave(paste("figures/downscaling_electricity/2050/PM25_emissions/2050_state_PM25_emissions_electricity_", state, ".svg", sep = ""),  plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
      }
      
      #Get Texas only, which has the largest PM2.5
      cbsa_shapefile <- st_read("shapefiles/cb_2019_us_cbsa_500k/cb_2019_us_cbsa_500k.shp") %>% dplyr::select(AFFGEOID, NAME, geometry)
      cbsa_shapefile %>%
        filter(AFFGEOID == "310M500US26420")-> cbsa_shapefile_Houston
      
      cbsa_shapefile %>%
        filter(AFFGEOID == "310M500US19100")-> cbsa_shapefile_Dallas
      
      results_emissions_filtered %>%
        filter(PSTATABB == "TX") -> results_emissions_filtered_TX
      
      # Filter us_map for the current state
      TX_map <- us_map_abb %>%
        filter(Abbreviation == results_emissions_filtered_TX$PSTATABB)

      for (current_scenario in unique(results_emissions_filtered_TX$scenario)) {
        results_emissions_filtered_TX %>%
        filter(scenario == current_scenario) -> results_emissions_filtered_TX_scenario
      
      # Create the map plot for the current scenario
      map_plot <- ggplot() +
        geom_polygon(data = results_emissions_filtered_TX_scenario, aes(x = LON, y = LAT, group = unique_ID), fill = "lightgray") +
        geom_point(data = results_emissions_filtered_TX_scenario, aes(x = LON, y = LAT, size = emissions_Tg, color = gcam_technology), shape = 16) +
        geom_path(data = TX_map, aes(x = long, y = lat, group = group), color = "darkgray", size = 0.5) +
        geom_sf(data = cbsa_shapefile_Dallas, color = "black", fill = NA)+ 
        geom_sf(data = cbsa_shapefile_Houston, color = "black", fill = NA)+
        ggtitle(paste("2050 PM2.5 from Electricity Generation - State: TX", current_scenario)) +
        scale_color_manual(values = elec_gen_combustion_color) +
        scale_size(name = "Emissions (Tg)") +  # Adjust the range according to your data
        coord_sf() +  # Set the coordinate system for geom_sf()
        my_theme
      ggsave(paste("figures/downscaling_electricity/2050/PM25_emissions/2050_state_PM25_emissions_electricity_TEXAS_cities_", current_scenario, ".png"), 
             plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
      ggsave(paste("figures/downscaling_electricity/2050/PM25_emissions/2050_state_PM25_emissions_electricity_TEXAS_cities_", current_scenario, ".svg"), 
             plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
      }
      
      
      results_emissions_filtered_TX %>%
        filter(scenario != "Reference") %>%
        dplyr::select(PSTATABB, unique_ID, scenario, period, gcam_technology, emissions_Tg, LON, LAT) %>%
        spread(scenario, emissions_Tg) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        mutate(difference = `Net-zero, High BECCS, High DAC` - `Net-zero, Low BECCS, Low DAC`,
               percent_decrease = difference * 100 /`Net-zero, High BECCS, High DAC`,
               percent_decrease = replace(percent_decrease, is.na(percent_decrease), 0))-> results_emissions_filtered_TX_diff
      
      map_plot <- ggplot() +
        geom_polygon(data = results_emissions_filtered_TX_diff, aes(x = LON, y = LAT, group = unique_ID), fill = "lightgray") +
        geom_point(data = results_emissions_filtered_TX_diff, aes(x = LON, y = LAT, size = percent_decrease, color = gcam_technology), shape = 16) +
        geom_path(data = TX_map, aes(x = long, y = lat, group = group), color = "darkgray", size = 0.5) +
        geom_sf(data = cbsa_shapefile_Dallas, color = "black", fill = NA)+ 
        geom_sf(data = cbsa_shapefile_Houston, color = "black", fill = NA)+
        ggtitle(paste("2050 percent_decrease in PM2.5 from Electricity Generation in TX (PM2.5 from high - low)")) +
        scale_color_manual(values = elec_gen_combustion_color) +
        scale_size(name = "Percent decrease") +  # Adjust the range according to your data
        coord_sf() +  # Set the coordinate system for geom_sf()
        my_theme
      ggsave(paste("figures/downscaling_electricity/2050/PM25_emissions/2050_state_PM25_emissions_electricity_TEXAS_cities_DIFF_percent.png"), 
             plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
      ggsave(paste("figures/downscaling_electricity/2050/PM25_emissions/2050_state_PM25_emissions_electricity_TEXAS_cities_DIFF_percent.svg"), 
             plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
      
      results_emissions_filtered_TX_diff %>%
        filter(LON < -94 & LON > -97, 
               LAT > 28.5 & LAT < 31) -> results_emissions_filtered_TX_diff_houston
      
      map_plot <- ggplot() +
        geom_polygon(data = results_emissions_filtered_TX_diff_houston, aes(x = LON, y = LAT, group = unique_ID), fill = "lightgray") +
        geom_point(data = results_emissions_filtered_TX_diff_houston, aes(x = LON, y = LAT, size = difference, color = gcam_technology), shape = 16) +
        #geom_path(data = TX_map, aes(x = long, y = lat, group = group), color = "darkgray", size = 0.5) +
        geom_sf(data = cbsa_shapefile_Houston, color = "black", fill = NA)+
        ggtitle(paste("2050 difference in PM2.5 from Electricity Generation in TX (PM2.5 from high - low)")) +
        scale_color_manual(values = elec_gen_combustion_color) +
        scale_size(name = "Difference in Tg") +  # Adjust the range according to your data
        coord_sf() +  # Set the coordinate system for geom_sf()
        my_theme
      ggsave(paste("figures/downscaling_electricity/2050/PM25_emissions/2050_state_PM25_emissions_electricity_TEXAS_cities_DIFF_percent_HOUSTON.png"), 
             plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
      ggsave(paste("figures/downscaling_electricity/2050/PM25_emissions/2050_state_PM25_emissions_electricity_TEXAS_cities_DIFF_percent_HOUSTON.svg"), 
             plot = map_plot, dpi = 600/2, width = 6000/300, height = 3000/300)
    
      #### 7.1.3. Compare to GCAM ####
      #Here at a state level
      results_emissions_final %>%
        group_by(PSTATABB, gcam_fuel, gcam_technology, scenario, period, ghg) %>%
        dplyr::summarise(emissions_eGRID = sum(emissions_Tg)) %>%
        ungroup() %>%
        dplyr::rename(year = period,
               Fuel = gcam_fuel,
               Technology = gcam_technology,
               region = PSTATABB,
               Scenario = scenario) -> results_emissions_final_state
        
      air_pollution_electricity_final %>%
        filter(year %in% c(2030, 2050)) %>%
        left_join(results_emissions_final_state, by = c("region", "year", "Scenario", "Fuel", "Technology", "ghg")) %>%
        #Here we replace 706 cases where there is NA in eGRID: this means GCAM has emissions, but after adjusting the quotas, this technology does not happen in that state
        mutate(emissions_eGRID = if_else(is.na(emissions_eGRID), 0, emissions_eGRID))%>%
        dplyr::rename(emissions_GCAM = emissions_Tg)-> results_emissions_final_state_comparison
      
      scatter_plot <- ggplot() +
        geom_point(data = results_emissions_final_state_comparison, aes(x = emissions_GCAM, y = emissions_eGRID, color = Technology)) +
        facet_grid(ghg ~ Scenario, scales = "free_y") +
        scale_color_manual(values = elec_gen_combustion_color) +
        ggtitle("Total State Emissions comparison: GCAM vs. eGRID") +
        xlab("GCAM") +
        ylab("eGRID") +
        theme_minimal()
      print(scatter_plot)
      
      #Here at a national level
      results_emissions_final_state_comparison %>%
        group_by(year, Fuel, Technology, ghg, Scenario) %>%
        dplyr::summarise(national_emissions_GCAM = sum(emissions_GCAM)) %>%
        ungroup() -> GCAM
      
      results_emissions_final_state_comparison %>%
        group_by(year, Fuel, Technology, ghg, Scenario) %>%
        dplyr::summarise(national_emissions_eGRID = sum(emissions_eGRID)) %>%
        ungroup() -> eGRID
      
      GCAM %>%
        left_join(eGRID, by = c("year", "Fuel", "Technology", "ghg", "Scenario")) ->results_emissions_final_national_comparison
      
      scatter_plot <- ggplot() +
        geom_point(data = results_emissions_final_national_comparison, aes(x = national_emissions_GCAM, y = national_emissions_eGRID, color = Technology)) +
        facet_grid(ghg ~ Scenario, scales = "free_y") +
        scale_color_manual(values = elec_gen_combustion_color) +
        ggtitle("Total National Emissions comparison: GCAM vs. eGRID") +
        xlab("GCAM") +
        ylab("eGRID") +
        theme_minimal() +
        xlim(0, 1) +
        ylim(0, 1)
      print(scatter_plot)
      
      #Here without technology detail
      #Here at a national level
      results_emissions_final_state_comparison %>%
        group_by(year, ghg, Scenario) %>%
        dplyr::summarise(national_emissions_GCAM = sum(emissions_GCAM)) %>%
        ungroup() -> GCAM_general
      
      results_emissions_final_state_comparison %>%
        group_by(year, ghg, Scenario) %>%
        dplyr::summarise(national_emissions_eGRID = sum(emissions_eGRID)) %>%
        ungroup() -> eGRID_general
      
      GCAM_general %>%
        left_join(eGRID_general, by = c("year", "ghg", "Scenario")) ->results_emissions_final_national_comparison_general
    
      
#### 7.2. Prepare final table ####   
      results_emissions_final %>%
        left_join(egrid_data_plant_2020_metric, by = c("PSTATABB", "ORISPL")) %>%
        group_by(PSTATABB, scenario, period, ghg, LAT, LON) %>%
        dplyr::summarise(total_emissions = sum(emissions_Tg)) %>%
        ungroup() %>%
        left_join(states_abb_mapping, by = c("PSTATABB" = "region")) %>%
        mutate(Units = "Tg") %>%
        dplyr::select(-PSTATABB) %>%
        dplyr::rename(Scenario = scenario,
               Pollutant = ghg) %>%
        mutate(Pollutant = if_else(Pollutant == "SO2_1", "SO2", Pollutant))-> results_emissions_final_format
      
      #Here we create PM2.5_AGG and PM10_AGG
      results_emissions_final_format %>%
        filter(Pollutant %in% c("BC", "OC", "PM2.5", "PM10")) %>%
        spread(Pollutant, total_emissions) %>%
        mutate(BC = ifelse(is.na(BC), 0, BC),
               OC = ifelse(is.na(OC), 0, OC),
               PM2.5 = ifelse(is.na(PM2.5), 0, PM2.5),
               PM10 = ifelse(is.na(PM10), 0, PM10)) %>%
        mutate(PM2.5_AGG = BC + OC + PM2.5, #Add PM2.5 + BC + OC
               PM10_AGG = BC + OC + PM10) %>% #Add PM10 + BC + OC
        dplyr::select(-BC, -OC, -PM10, -PM2.5)-> results_emissions_final_format_PMagg
      
      results_emissions_final_format_PMagg %>%
        dplyr::select(-PM10_AGG) %>%
        gather(Pollutant, total_emissions, -Scenario, -State, -period, -LON, -LAT, -Units) -> PM2.5_AGG
      
      results_emissions_final_format_PMagg %>%
        dplyr::select(-PM2.5_AGG) %>%
        gather(Pollutant, total_emissions, -Scenario, -State, -period, -LON, -LAT, -Units) -> PM10_AGG
      
      #Join tables
      results_emissions_final_format %>%
        filter(!Pollutant %in% c("BC", "OC")) %>%
        bind_rows(PM2.5_AGG, PM10_AGG)-> results_emissions_final_format_complete
      
#### 7.3. Here we print output tables ####
      # Get unique scenarios
      scenarios <- unique(results_emissions_final_format_complete$Scenario)
      periods <- unique(results_emissions_final_format_complete$period)
      
      # Loop through each scenario for 2030
      for (scenario in scenarios) {
        for (period in periods) {
        # Filter data for the current scenario
        filtered_data <- results_emissions_final_format_complete %>%
          filter(Scenario == scenario, period == period)
        
        # Define the file name
        file_name <- paste("output_data/downscaling/electricity_", period, "_", scenario, ".csv", sep = "")
        
        # Save the filtered data as a CSV file
        write.csv(filtered_data, file_name, row.names = FALSE)
        }
      }
