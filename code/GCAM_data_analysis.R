# GCAM_data_analysis.R
# This script is designed to access the database for scenarios run in GCAM for the just transition paper
# We want to create the .dat file with necessary output data
# Here we have a Reference scenario, and then 2 net-zero scenarios. One NZ scenario has unlimited CDR. The other NZ scenario has very expensive CDR, so there is less of it. Both scenarios have modified emission factors for CCS electricity technologies.
# Last modified: June-July 2023
# Author: Candelaria Bergero

#--------------------------------------------------------------------------------------------------

#### Load packages ####
library(plyr)
library(readr)
library(ggplot2)
library(devtools)
#devtools::load_all("/Users/mariacandelariabergero/Documents/GCAM/rgcam-1.2.0")
library(rgcam)
library(tidyr)
library(dplyr)
library(gcamdata)
library(directlabels)
library(viridisLite)
library(usmap)
library(forcats)

#### Load scenarios ####
#Set working directory
#setwd("~/Documents/GCAM/gcam-v6.0-Mac-Release-Package/output/R_scripts_Just_transition")

#Connect to the database
#conn <- localDBConn('/Users/mariacandelariabergero/Documents/GCAM/gcam-v6.0-Mac-Release-Package/output', 'database_basexdb_FINAL')

#Load the scenario
#prj <- addScenario(conn, 'dat/GCAM_analysis_V2.dat', 'GCAM-USA_REF', 'queries/queries.xml') #Reference
#prj <- addScenario(conn, 'dat/GCAM_analysis_V2.dat', 'GCAM-USA_DAC_NZUSA2050ghg_NZROW2060co2_newEFV2_allEF', 'queries/queries.xml') #Net-zero + high CDR
#prj <- addScenario(conn, 'dat/GCAM_analysis_V2.dat', 'GCAM-USA_withDAC_NZUSA2050ghg_NZROW2060co2_forest_CCSV3_allEF', 'queries/queries.xml') #Net-zero + lowCDR

#Load the project
prj <- loadProject('data/GCAM/GCAM_analysis.dat')
listScenarios(prj)
listQueries(prj)

#Get color schemes
source( "code/color_schemes.R" ) # some predefined color schemes

####Filters####
USA_states <- c("AL","AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC","FL",
                "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
                "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
                "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
                "WY") #Do not include USA

USA_all <- c("USA", "AL","AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC","FL",
              "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
              "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
              "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
              "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
              "WY") # Include USA

ROW_A1 <- c("Canada", "EU-12", "EU-15", "Australia_NZ", "Europe_Eastern", "Europe_Non_EU",
            "European Free Trade Association", "Japan", "Russia")

ROW_non_A1 <- c("Brazil", "China", "Africa_Eastern", "Africa_Northern", "Africa_Southern", "Africa_Western",
                "Argentina", "Central America and Caribbean", "Central Asia", "Colombia", "Middle East",
                "Pakistan", "South Africa", "South America_Northern", "South America_Southern", "South Asia",
                "Southeast Asia", "India", "Indonesia", "Mexico", "South Korea", "Taiwan")

ROW <- c("Canada", "EU-12", "EU-15", "Australia_NZ", "Europe_Eastern", "Europe_Non_EU",
         "European Free Trade Association", "Japan", "Russia", "Brazil", "China", "Africa_Eastern",
         "Africa_Northern", "Africa_Southern", "Africa_Western", "Argentina", "Central America and Caribbean", "Central Asia",
         "Colombia", "Middle East", "Pakistan", "South Africa", "South America_Northern", "South America_Southern",
         "South Asia", "Southeast Asia", "India", "Indonesia", "Mexico", "South Korea", "Taiwan")

air_pollution <- c("BC", "BC_AWB", "CO", "CO_AWB", "NH3", "NH3_AGR", "NH3_AWB",
                   "NMVOC", "NMVOC_AGR", "NMVOC_AWB", "NOx", "NOx_AGR", "NOx_AWB", 
                   "OC", "OC_AWB", "PM10", "PM2.5", "SO2_1", "SO2_1_AWB")

####Constants####
emissions.CONV_C_CO2    <- 44 / 12 # Convert Carbon to CO2

####Variable order ####
scenario_order <- c("GCAM-USA_REF",
                    "GCAM-USA_DAC_NZUSA2050ghg_NZROW2060co2",
                    "GCAM-USA_noDAC_NZUSA2050ghg_NZROW2060co2",
                    "GCAM-USA_DAC_noBECCS_NZUSA2050ghg_NZROW2060co2",
                    "GCAM-USA_noDAC_noBECCS_NZUSA2050ghg_NZROW2060co2",
                    "GCAM-USA_withDAC_NZUSA2050ghg_NZROW2060co2_forest_CCSV3",
                    "GCAM-USA_littleDAC_littleBECCS_NZUSA2050ghg_NZROW2060co2_test1_opoo5")

scenario_short_order <- c("Reference",
                          "Net-zero, High BECCS, High DAC",
                          "Net-zero, BECCS, no DAC",
                          "Net-zero, no BECCS, DAC",
                          "Net-zero, no BECCS, no DAC",
                          "Net-zero, Low BECCS, Low DAC")

scenario_short_facet_order <- c("Net-zero, High BECCS, High DAC",
                                "Net-zero, Low BECCS, Low DAC",
                                "Net-zero, no BECCS, no DAC",
                                "Reference")

CO2_sector_order <- c("Buildings","Industry", "Transport", "Electricity", "Backup Electricity", "Refining", "H2 Production", "Regional Biomass", "DAC")

CO2_sequestration_order <- c("Industry", "Feedstocks", "Electricity", "Refining", "H2 Production", "DAC", "DAC heat")

CO2_sequestration_tech_order <- c("Industry", "Feedstocks",  "Electricity BECCS", "Electricity CCS", "Refining BECCS", "Refining CCS", "Hydrogen BECCS",
                                  "Hydrogen CCS", "Cement CCS", "Fertilizer CCS", "DAC", "DAC heat")

CO2_sequestration_tech_order_2 <- c("Refining BECCS", "Electricity BECCS", "DAC", "Hydrogen BECCS")

GHG_gas_order <- c("HFCs",
                   "N2O Energy", "N2O Resource Production", "N2O Agriculture", "N2O Unmanaged Land",
                   "CH4 Energy", "CH4 Resource Production", "CH4 Agriculture", "CH4 Unmanaged Land",
                   "CO2 Energy", "CO2 Resource Production", "CO2 Land")

primary_energy_order <- c("Oil", "Natural Gas", "Coal", "Biomass", "Nuclear", "Hydro", "Wind", "Solar")

final_energy_order <- c("building", "industry", "transportation", "CO2 removal", "process heat dac")

final_energy_fuel_order <- c("Liquids", "Gas", "Coal", "Biomass", "Hydrogen", "Electricity")

air_pollution_source_order <- c("Buildings", "Industry", "Transportation", "Electricity", "Refining", "Agriculture", "Unmanaged Land", "Urban processes")

refining_tech_order <- c("Oil (no CCS)", "Gas (no CCS)", "Coal (with CCS)",  "Biodiesel", "Corn Ethanol (no CCS)", "Cellulosic ethanol (with CCS)",
                         "Cellulosic ethanol (no CCS)", "FT biofuel (with CCS)", "FT biofuel (no CCS)", "BTL with H2 (no CCS)")

land_alloc_order <- c("Agriculture", "Soybean",  "Oil Crop",  "Sugar Crop", "Other arable",  "Biomass", "Forest" , "Fodder", "Grassland", "Pasture", "Shrubland", "Protected Land")


####Load mapping files####
GHG_mapping <- read_csv("mappings/GHG_conv_mapping.csv", skip = 1)
region_mapping <- read_csv("mappings/region_mapping.csv")
CO2_sector_mapping <- read_csv("mappings/CO2_mapping.csv")
CO2_seq_sector_mapping <- read_csv("mappings/CO2_seq_mapping.csv")
CO2_seq_technology_mapping <- read_csv("mappings/CO2_seq_technology_mapping.csv")
GHG_gas_states_mapping <- read_csv("mappings/non_CO2_gas_mapping_states.csv")
GHG_gas_USA_mapping <- read_csv("mappings/non_CO2_gas_mapping_USA.csv")
scenario_mapping <- read_csv("mappings/scenario_mapping.csv")
PE_fuel_mapping <- read_csv("mappings/PE_fuel_mapping.csv", skip = 1)
FE_fuel_mapping <- read_csv("mappings/FE_fuel_mapping.csv")
air_pollution_sector_mapping <- read_csv("mappings/air_pollution_sectors_mapping.csv")
state_grids <- read_csv("mappings/states_subregions.csv")
refined_liquids_mapping <- read_csv("mappings/refined_liquids_mapping.csv")
agg_land_mapping <- read_csv("mappings/agg_land_mapping.csv")

# ============================================================================
# Generate graphs

##### 1. Carbon dioxide #####
      #### a. emissions in the US ####
      CO2_emissions_region <- getQuery(prj, "CO2 emissions by region")

      CO2_emissions_region %>%
        #Keep US states only, and 2015-2050 data
        filter(region %in% USA_all,
               year > 2010,
               year < 2055) %>%
        # convert MTC to Mt CO2
        mutate(value = value*emissions.CONV_C_CO2,
               Units = "MtCO2") %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order))-> CO2_emissions_region_USA

      #One graph, one line per scenario
      CO2_emissions_region_USA %>%
        group_by(Units, scenario, year, Scenario, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order))-> CO2_emissions_region_USA_total

      p <- ggplot() + geom_line(data=CO2_emissions_region_USA_total, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_grid(policy_name ~ .) +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Energy CO2 Emissions USA" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2") +
        theme(panel.background = element_blank(),
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
              legend.text = element_text(size = 20))
      #ggsave("figures/GCAM_analysis/1a.CO2_emissions_USA.png", dpi=600/2, width=6000/300, height=3000/300)

      #### b. CO2 emissions by region #####
      CO2_emissions_region %>%
        #Keep US states only, and 2015-2050 data
        filter(year > 2010,
               year < 2055) %>%
        mutate(Region = (if_else(region %in% USA_all, "USA", "ROW")))%>%
        group_by(Region, scenario, Units, year) %>%
        summarise(region_total = sum(value)) %>%
        ungroup() %>%
        # convert MTC to GtCO2 (only graph in Giga, since it has ROW)
        mutate(region_total = (region_total*emissions.CONV_C_CO2)/1000,
               Units = "GtCO2")%>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order)) -> CO2_emissions_region_final

      p <- ggplot() + geom_line(data=CO2_emissions_region_final, aes(x=year, y=region_total, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(Region ~ .) +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Energy CO2 Emissions by Region" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("GtCO2") +
        theme(panel.background = element_blank(),
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
              legend.text = element_text(size = 20))
      #ggsave("figures/GCAM_analysis/1b.CO2_emissions_region.png", dpi=600/2, width=6000/300, height=3000/300)


      #### c. CO2 emissions by sector ####
      CO2_emissions_sector <- getQuery (prj, "CO2 emissions by sector")

      CO2_emissions_sector %>%
        mutate(Region = (if_else(region %in% USA_all, "USA", "ROW"))) %>%
        filter(Region == "USA",
               year > 2010 & year < 2055) %>%
        left_join(CO2_sector_mapping, by = "sector") %>%
        group_by(Units, scenario, agg_sector, year, Region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        # convert MTC to Mt CO2
        mutate(USA_total = (sum * emissions.CONV_C_CO2),
               Units_total = "MtCO2") %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) %>%
        mutate(Sector = factor(agg_sector, levels = CO2_sector_order)) -> CO2_emissions_sector_USA

      p <- ggplot() + geom_col(data=CO2_emissions_sector_USA, aes(x=year, y=USA_total, fill= Sector)) +
        scale_fill_manual(values=CO2_sector_color) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "Total CO2 emissions by sector (WITH BIO)" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/1c.CO2_emissions_sector_USA.png", dpi=600/2, width=6000/300, height=3000/300)

      #### c. Emissions by sector NO BIO equivalent ####
      #Here we bring in the 3 files with emissions per sector and state where biomass has been allocated to each sector
      #Thus there is no "regional biomass" sector
      #This was processed in another script in folder "gcam-usa-nobio-accounting-main"
      CO2_sector_nobio_REF <- read_csv("data/GCAM/P_co2_sector_GCAM-USA_REF.csv") #Units are MtC
      CO2_sector_nobio_lowCDR <- read_csv("data/GCAM/P_co2_sector_GCAM-USA_withDAC_NZUSA2050ghg_NZROW2060co2_forest_CCSV3_allEF.csv")
      CO2_sector_nobio_highCDR <- read_csv("data/GCAM/P_co2_sector_GCAM-USA_DAC_NZUSA2050ghg_NZROW2060co2_newEFV2_allEF.csv")

      #Merge 3 scenarios
      CO2_sector_nobio_REF %>%
        bind_rows(CO2_sector_nobio_lowCDR, CO2_sector_nobio_highCDR) %>%
        mutate(Units = "MtC")-> CO2_nobio_all

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
        dplyr::select(scenario, co2.emiss, sector, region, year, Units)-> CO2_nobio_all_gas_final #Table has total emissions by state from gas processing and gas pipeline

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
        dplyr::select(scenario, co2.emiss, sector, region, year, Units)-> CO2_nobio_all_electricity_final #Table has emissions by state for csp_backup and backup_electricity


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

      #Compare that sum of CO2 emissions by sector add up to total from CO2 emissions by region
      CO2_nobio_all_sector %>%
        group_by(year, Scenario) %>%
        summarise(sum_is = sum(value)) %>%
        ungroup %>%
        left_join(CO2_emissions_region_USA_total, by = c("Scenario", "year")) %>%
        rename(sum_region = sum) %>%
        mutate(difference_percentage = ((sum_region - sum_is)/sum_region)*100)-> CO2_emissions_comparison_bio_nobio_USA

      CO2_nobio_all_sector %>%
        group_by(year, Scenario, region) %>%
        summarise(sum_is = sum(value)) %>%
        ungroup %>%
        left_join(CO2_emissions_region_USA, by = c("region", "Scenario", "year")) %>%
        rename(sum_region = value) %>%
        mutate(difference = ((sum_region - sum_is)/sum_region)*100)-> CO2_emissions_comparison_bio_nobio_states #NOTE: downscaling emissions by state does not mean that states should equal what the co2 by region query says

      #Now analyze data
      CO2_nobio_all_sector %>%
        group_by(Units, year, Scenario, agg_sector) %>%
        summarise(USA_total = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) %>%
        rename(Sector = agg_sector) %>%
        mutate(Sector = factor(Sector, levels = CO2_sector_order))-> CO2_nobio_all_sector_USA

      p <- ggplot() + geom_col(data=CO2_nobio_all_sector_USA, aes(x=year, y=USA_total, fill= Sector)) +
        scale_fill_manual(values=CO2_sector_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        ggtitle( "Total CO2 Emissions by sector" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/1c.CO2_emissions_sector_USA_nobio.png", dpi=600/2, width=6000/300, height=3000/300)

      #Map
      CO2_nobio_all_sector %>%
        filter(year == 2050) %>%
        group_by(Units, year, Scenario, region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) %>%
        rename(state = region) -> CO2_nobio_all_sector_USA_2050_map
      
      CO2_nobio_all_sector_USA_2050_map %>%
        filter(Scenario != "Reference") -> CO2_nobio_all_sector_USA_2050_map

      p <- plot_usmap(region = c("states"),
                      data = CO2_nobio_all_sector_USA_2050_map,
                      values = "sum") +
        ggtitle( "Net Energy CO2 Emissions USA in 2050 in MtCO2") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/1c.CO2_emissions_USA_MAPS_2050_V2.png", dpi=600/2, width=6000/300, height=3000/300)

      # Get lines per state
      CO2_nobio_all_sector %>%
        group_by(Units, year, Scenario, region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order)) -> CO2_nobio_all_sector_states

      p <- ggplot() + geom_line(data=CO2_nobio_all_sector_states, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_wrap(region ~ ., scales = "free_y") +
        facet_wrap(facets = ~fct_reorder(region, sum, .desc = TRUE), scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Net Energy CO2 Emissions by State" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/1c.CO2_emissions_states_nobio.png", dpi=600/2, width=6000/300, height=3000/300)

      #### c. Positive to negative ratio ####
      CO2_nobio_all_sector %>%
        filter(value >= 0) %>%
        group_by(Units, region, year, Scenario) %>%
        summarise(sum_positive = sum(value)) %>%
        ungroup() %>%
        rename(state = region)-> positive_emissions

      CO2_nobio_all_sector %>%
        filter(value < 0) %>%
        group_by(Units, region, year, Scenario) %>%
        summarise(sum_negative = sum(value)) %>%
        ungroup() %>%
        rename(state = region)-> negative_emissions

      negative_emissions %>%
        left_join(positive_emissions, by = c("year", "state", "Units", "Scenario")) %>%
        filter(year == 2050,
               Scenario != "Reference") %>%
        mutate(ratio = (sum_negative / sum_positive)*-1) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> ratio_emissions

      # Positive emissions
      p <- plot_usmap(region = c("states"),
                      data = ratio_emissions,
                      values = "sum_positive") +
        ggtitle( "Positive Energy CO2 Emissions USA in 2050 in MtCO2") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/1c.CO2_emissions_USA_MAPS_positive_2050.png", dpi=600/2, width=6000/300, height=3000/300)

      #Negative emissions
      p <- plot_usmap(region = c("states"),
                      data = ratio_emissions,
                      values = "sum_negative") +
        ggtitle( "Negative Energy CO2 Emissions USA in 2050 in MtCO2") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/1c.CO2_emissions_USA_MAPS_negative_2050.png", dpi=600/2, width=6000/300, height=3000/300)

      #Ratio
      p <- plot_usmap(region = c("states"),
                      data = ratio_emissions,
                      values = "ratio") +
        ggtitle( "Ratio Energy CO2 Emissions USA in 2050 in MtCO2 (negative / positive)") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#08A045", mid = "#FAF0CA", high = "#007EA7", midpoint = 0) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/1c.CO2_emissions_USA_MAPS_ratio_2050.png", dpi=600/2, width=6000/300, height=3000/300)

      #### c. Regional distribution of sectors ####
      CO2_nobio_all_sector %>%
        filter(agg_sector == "Electricity",
               year == 2050) %>%
        group_by(region, year, Units, Scenario, agg_sector) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) %>%
        rename(state = region)-> CO2_nobio_all_sector_2050_electricity

      p <- plot_usmap(region = c("states"),
                      data = CO2_nobio_all_sector_2050_electricity,
                      values = "sum") +
        ggtitle( "Electricity CO2 Emissions USA in 2050 in MtCO2") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/1c.CO2_emissions_USA_MAPS_electricity_2050.png", dpi=600/2, width=6000/300, height=3000/300)


      #### d. CO2 sequestration by sector####
      CO2_seq_sector <- getQuery(prj, "CO2 sequestration by sector")

      #At US level
      CO2_seq_sector %>%
        mutate(Region = (if_else(region %in% USA_all, "USA", "ROW"))) %>%
        filter(Region == "USA",
               year > 2010 & year < 2055) %>%
        left_join(CO2_seq_sector_mapping, by = "sector") %>%
        group_by(Units, scenario, agg_sector, year, Region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(USA_total = sum*emissions.CONV_C_CO2,
               Units_total = "MtCO2")%>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) %>%
        mutate(Sector = factor(agg_sector, levels = CO2_sequestration_order)) -> CO2_seq_sector_USA

      p <- ggplot() + geom_col(data=CO2_seq_sector_USA, aes(x=year, y=USA_total, fill= Sector)) +
        scale_fill_manual(values=CO2_sequestration_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        ggtitle( "Total CO2 sequestration by sector" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/1d.CO2_sequestration_sector_USA.png", dpi=600/2, width=6000/300, height=3000/300)

      #### d. MAPS ####
      # Map total sequestration by state
      CO2_seq_sector %>%
        filter(region %in% USA_states,
               year == 2050) %>%
        left_join(CO2_seq_sector_mapping, by = "sector") %>%
        group_by(Units, scenario, year, region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(total = sum*emissions.CONV_C_CO2,
               Units = "MtCO2")%>%
        rename(state = region) %>%
        mutate(total = total * (-1)) %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> CO2_seq_sector_USA_2050

      p <- plot_usmap(region = c("states"),
                      data = CO2_seq_sector_USA_2050,
                      values = "total") +
        ggtitle( "Total Carbon Sequestration USA in 2050 in MtCO2") +
        facet_wrap( ~ Scenario, ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) + #limits=c(-250,700)
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/1d.CO2_sequestration_USA_MAPS_2050.png", dpi=600/2, width=6000/300, height=3000/300)


      #### e. CO2 sequestration by technology####
      CO2_seq_tech <- getQuery(prj, "CO2 sequestration by tech")
      CO2_seq_tech_elec <- getQuery(prj, "CO2 sequestration by electricity tech (USA)")
      CO2_seq_tech %>%
        bind_rows(CO2_seq_tech_elec) -> CO2_seq_all

      # In the US
      CO2_seq_all %>%
        mutate(Region = (if_else(region %in% USA_all, "USA", "ROW"))) %>%
        filter(Region == "USA",
               year > 2010 & year < 2055) %>%
        left_join(CO2_seq_technology_mapping, by = c("sector", "technology")) %>%
        group_by(Units, scenario, agg_tech, year, Region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(USA_total = sum*emissions.CONV_C_CO2,
               USA_total = USA_total,
               Units_total = "MtCO2")%>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) %>%
        mutate(Technology = factor(agg_tech, levels = CO2_sequestration_tech_order)) -> CO2_seq_tech_final_USA


      p <- ggplot() + geom_col(data=CO2_seq_tech_final_USA, aes(x=year, y=USA_total, fill= Technology)) +
        scale_fill_manual(values=CO2_sequestration_tech_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS ) +
        ggtitle( "Total CO2 sequestration by technology" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/1e.CO2_sequestration_technology_USA.png", dpi=600/2, width=6000/300, height=3000/300)

      # Get amount of negative emissions -> DAC + BECCS (hydrogen + electricity + refining)
      CO2_seq_all %>%
        mutate(Region = (if_else(region %in% USA_all, "USA", "ROW"))) %>%
        filter(Region == "USA",
               year > 2015 & year < 2055) %>%
        left_join(CO2_seq_technology_mapping, by = c("sector", "technology")) %>%
        filter(agg_tech %in% c("Hydrogen BECCS", "Refining BECCS", "Electricity BECCS", "DAC")) %>%
        group_by(Units, scenario, agg_tech, year, Region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(USA_total = sum*emissions.CONV_C_CO2,
               USA_total = USA_total,
               Units_total = "MtCO2")%>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order)) %>%
        mutate(Technology = factor(agg_tech, levels = CO2_sequestration_tech_order_2)) -> CO2_seq_tech_final_USA_negative_emissions_techs

      p <- ggplot() + geom_line(data=CO2_seq_tech_final_USA_negative_emissions_techs, aes(x=year, y=USA_total, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(Technology ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Total CO2 sequestration by technology" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12))
      #ggsave("figures/GCAM_analysis/1e.CO2_sequestration_technology_USA_negative.png", dpi=600/2, width=6000/300, height=3000/300)
  
      CO2_seq_tech_final_USA_negative_emissions_techs %>%
        group_by(Units_total, year, Region, Scenario) %>%
        summarise(sum = sum(USA_total)) %>%
        ungroup()  -> CO2_seq_tech_final_USA_negative_emissions_techs_total

      p <- ggplot() + geom_line(data=CO2_seq_tech_final_USA_negative_emissions_techs_total, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Total CO2 sequestration by BECCS & DAC" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12))
      #ggsave("figures/GCAM_analysis/1e.CO2_sequestration_technology_USA_negative_TOT.png", dpi=600/2, width=6000/300, height=3000/300)

      #Here we do a plot with columns
      p <- ggplot() + geom_col(data=CO2_seq_tech_final_USA_negative_emissions_techs, aes(x=year, y=USA_total, fill= Technology)) +
        geom_line(data=CO2_seq_tech_final_USA_negative_emissions_techs_total, aes(x=year, y=sum), size = 1) +
        scale_fill_manual(values=CO2_sequestration_tech_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS ) +
        ggtitle( "Total CO2 sequestration by technology" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/1e.CO2_sequestration_technology_USA_negative_columns.png", dpi=600/2, width=6000/300, height=3000/300)
      
      #### e. MAPS ####
      #Sequestration per state
      CO2_seq_all %>%
        mutate(Region = (if_else(region %in% USA_all, "USA", "ROW"))) %>%
        filter(Region == "USA",
               year == 2050) %>%
        left_join(CO2_seq_technology_mapping, by = c("sector", "technology")) %>%
        filter(agg_tech %in% c("Hydrogen BECCS", "Refining BECCS", "Electricity BECCS", "DAC")) %>%
        group_by(Units, scenario, year, region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(USA_total = (sum*emissions.CONV_C_CO2)*-1,
               USA_total = USA_total,
               Units_total = "MtCO2")%>%
        rename(state = region) %>%
        left_join(scenario_mapping, by = "scenario") -> CO2_seq_all_MAP

      p <- plot_usmap(region = c("states"),
                      data = CO2_seq_all_MAP,
                      values = "USA_total") +
        ggtitle( "Total BECCS & DAC Sequestration USA in 2050 in MtCO2") +
        facet_wrap(Scenario ~ ., ncol = 2) +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) + #limits=c(-250,700)
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/1e.CO2_sequestration_USA_MAPS_2050_all.png", dpi=600/2, width=6000/300, height=3000/300)

      # Collect states
      CO2_seq_all_MAP %>%
        dplyr::select(state, year, scenario) %>%
        unique()-> USA_states_list

      #1. DAC
      CO2_seq_all %>%
        mutate(Region = (if_else(region %in% USA_all, "USA", "ROW"))) %>%
        filter(Region == "USA",
               year == 2050) %>%
        left_join(CO2_seq_technology_mapping, by = c("sector", "technology")) %>%
        filter(agg_tech == "DAC") %>%
        group_by(Units, scenario, agg_tech, year, region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(USA_total = (sum*emissions.CONV_C_CO2)*-1,
               USA_total = USA_total,
               Units_total = "MtCO2")%>%
        rename(state = region) %>%
        #Bring in missing states and assign value of 0
        right_join(USA_states_list, by = c("year", "scenario", "state")) %>%
        mutate(USA_total = ifelse(is.na(USA_total), 0, USA_total))%>%
        left_join(scenario_mapping, by = "scenario") -> CO2_seq_DAC_2050

      p <- plot_usmap(region = c("states"),
                      data = CO2_seq_DAC_2050,
                      values = "USA_total") +
        ggtitle( "Total DAC Sequestration USA in 2050 in MtCO2") +
        facet_wrap(Scenario ~ ., ncol = 2) +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) + #limits=c(-250,700)
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/1e.CO2_sequestration_USA_MAPS_2050_DAC.png", dpi=600/2, width=6000/300, height=3000/300)

      #2. Electricity BECCS
      CO2_seq_all %>%
        mutate(Region = (if_else(region %in% USA_all, "USA", "ROW"))) %>%
        filter(Region == "USA",
               year == 2050) %>%
        left_join(CO2_seq_technology_mapping, by = c("sector", "technology")) %>%
        filter(agg_tech == "Electricity BECCS") %>%
        group_by(Units, scenario, agg_tech, year, region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(USA_total = (sum*emissions.CONV_C_CO2)*-1,
               USA_total = USA_total,
               Units_total = "MtCO2")%>%
        rename(state = region) %>%
        #Bring in missing states and assign value of 0
        right_join(USA_states_list, by = c("year", "scenario", "state")) %>%
        mutate(USA_total = ifelse(is.na(USA_total), 0, USA_total))%>%
        left_join(scenario_mapping, by = "scenario") -> CO2_seq_ElecBECCS_2050

      p <- plot_usmap(region = c("states"),
                      data = CO2_seq_ElecBECCS_2050,
                      values = "USA_total") +
        ggtitle( "Total Electircity BECCS Sequestration USA in 2050 in MtCO2") +
        facet_wrap(Scenario ~ ., ncol = 2) +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) + #limits=c(-250,700)
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/1e.CO2_sequestration_USA_MAPS_2050_ElecBECCS.png", dpi=600/2, width=6000/300, height=3000/300)

      #3. Refining BECCS
      CO2_seq_all %>%
        mutate(Region = (if_else(region %in% USA_all, "USA", "ROW"))) %>%
        filter(Region == "USA",
               year == 2050) %>%
        left_join(CO2_seq_technology_mapping, by = c("sector", "technology")) %>%
        filter(agg_tech == "Refining BECCS") %>%
        group_by(Units, scenario, agg_tech, year, region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(USA_total = (sum*emissions.CONV_C_CO2)*-1,
               USA_total = USA_total,
               Units_total = "MtCO2")%>%
        rename(state = region) %>%
        #Bring in missing states and assign value of 0
        right_join(USA_states_list, by = c("year", "scenario", "state")) %>%
        mutate(USA_total = ifelse(is.na(USA_total), 0, USA_total))%>%
        left_join(scenario_mapping, by = "scenario") -> CO2_seq_RefBECCS_2050

      p <- plot_usmap(region = c("states"),
                      data = CO2_seq_RefBECCS_2050,
                      values = "USA_total") +
        ggtitle( "Total Refining BECCS Sequestration USA in 2050 in MtCO2") +
        facet_wrap(Scenario ~ ., ncol = 2) +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) + #limits=c(-250,700)
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/1e.CO2_sequestration_USA_MAPS_2050_RefBECCS.png", dpi=600/2, width=6000/300, height=3000/300)

      #4. Hydrogen BECCS
      CO2_seq_all %>%
        mutate(Region = (if_else(region %in% USA_all, "USA", "ROW"))) %>%
        filter(Region == "USA",
               year == 2050) %>%
        left_join(CO2_seq_technology_mapping, by = c("sector", "technology")) %>%
        filter(agg_tech == "Hydrogen BECCS") %>%
        group_by(Units, scenario, agg_tech, year, region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(USA_total = (sum*emissions.CONV_C_CO2)*-1,
               USA_total = USA_total,
               Units_total = "MtCO2")%>%
        rename(state = region) %>%
        #Bring in missing states and assign value of 0
        right_join(USA_states_list, by = c("year", "scenario", "state")) %>%
        mutate(USA_total = ifelse(is.na(USA_total), 0, USA_total))%>%
        left_join(scenario_mapping, by = "scenario") -> CO2_seq_H2BECCS_2050

      p <- plot_usmap(region = c("states"),
                      data = CO2_seq_H2BECCS_2050,
                      values = "USA_total") +
        ggtitle( "Total H2 BECCS Sequestration USA in 2050 in MtCO2") +
        facet_wrap(Scenario ~ ., ncol = 2) +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) + #limits=c(-250,700)
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/1e.CO2_sequestration_USA_MAPS_2050_H2BECCS.png", dpi=600/2, width=6000/300, height=3000/300)


      #### f. Carbon storage per region ####
      carbon_storage <- getQuery(prj, "resource supply curves (onshore carbon)")

      #First we get graph with grades
      carbon_storage %>%
        # Storage does not change per year, nor scenario
        filter(year == 2015,
               scenario == "GCAM-USA_REF") %>%
        dplyr::select(-year) %>%
        left_join(scenario_mapping, by = "scenario")-> carbon_storage_final

      p <- ggplot() + geom_line(data=carbon_storage_final, aes(x=grade, y=value, colour = region, group = region), size = 1) +
        #scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_wrap(Scenario ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Total Carbon Storage by Grade" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtC") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12))
      #ggsave("figures/GCAM_analysis/1f.carbon_sequestration.png", dpi=600/2, width=6000/300, height=3000/300)

      #Here we get extraction costs and availability
      #Table from outputs
      carbon_storage_costs <- read.csv("/Users/mariacandelariabergero/Documents/GCAM/gcam-v6.0-Mac-Release-Package/output/R_scripts_Just_transition/data/resource_curves_C_storage.csv") #add_units("MtC and 1990USDtC") %>%

      carbon_storage_costs %>%
        #convert from 1990 USD to 2022 USD from https://www.officialdata.org/us/inflation/1990?endYear=2022&amount=1
        mutate(cost = extractioncost * 2.24) -> carbon_storage_costs_final

      p <- ggplot() + geom_line(data=carbon_storage_costs_final, aes(x=cost, y=available, colour = region, group = region), size = 1) +
        #scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(region, available, .desc = TRUE), scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Total Carbon Storage by Grade" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Extraction Cost (2022$/tC)") +
        ylab("Storage Availability (MtC)") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_blank(),
              legend.text = element_blank())
      #ggsave("figures/GCAM_analysis/1f.carbon_sequestration_grades.png", dpi=600/2, width=6000/300, height=3000/300)

      #Now we want to know totals
      carbon_storage_final %>%
        group_by(Units, region, scenario) %>%
        summarise(storage = sum(value)) %>%
        ungroup() %>%
        left_join(state_grids, by = c("region" = "grid_region")) -> carbon_storage_final_sum

      p <- plot_usmap(region = c("states"),
                      data = carbon_storage_final_sum,
                      values = "storage") +
        ggtitle( "Total Carbon Storage Availability by Grid Region in MtC") +
        #facet_wrap(Scenario ~ ., ncol = 2) +
        scale_fill_gradient2(low = "#08A045", mid = "#FAF0CA", high = "#007EA7", midpoint = 0) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/1f.carbon_sequestration_MAP.png", dpi=600/2, width=6000/300, height=3000/300)


##### 2. LUC emissions #####
      #### a. total line plot ####
      LUC_emissions_region <- getQuery(prj, "LUC emissions by region" )

      LUC_emissions_region %>%
        filter(year > 2010,
               year < 2055) %>%
        # Bring in mapping file with multipliers
        #left_join(region_mapping, by = "region") %>%
        mutate(Region = (if_else(region %in% USA_all, "USA", "ROW"))) %>%
        filter(Region == "USA") %>%
        mutate(value_MtCO2 = (value * emissions.CONV_C_CO2),
               Units = "MtCO2eq",
               ghg = "CO2_LUC") %>%
        group_by(Units, scenario, year, Region) %>%
        summarise(total_CO2_LUC = sum(value_MtCO2)) %>%
        ungroup() %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order)) -> LUC_emissions_region_final

      p <- ggplot() + geom_line(data=LUC_emissions_region_final, aes(x=year, y=total_CO2_LUC, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_grid(policy_name ~ .) +
        #facet_wrap(Region ~ .) +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "LUC emissions by region" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2") +
        theme(panel.background = element_blank(),
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
      #ggsave("figures/GCAM_analysis/2a.LUC_emissions.png", dpi=600/2, width=6000/300, height=3000/300)

##### 3. GHG emissions #####
      #### a. GHG emissions in the US ####
      GHG_emissions_region <- getQuery(prj, "nonCO2 emissions by region")

      GHG_emissions_region %>%
        #Keep 2015-2050 data
        filter(year > 2010,
               year < 2055) %>%
        # Bring in mapping file with multipliers
        left_join(GHG_mapping, by = c("ghg" = "GHG")) %>%
        mutate(total = value * GWP,
               Units = "MtCO2eq") %>%
        mutate(Region = (if_else(region %in% USA_all, "USA", "ROW")))%>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order)) -> GHG_emissions_region_complete

      #Here we want to calculate total GHG in USA (adding CO2_LUC)
      GHG_emissions_region_complete %>%
        filter(total != is.na(total)) %>%
        filter(Region == "USA") %>%
        group_by(Scenario, year, Region, Units, BECCS, DAC) %>%
        summarise(total = sum(total)) %>%
        ungroup() %>%
        #Now we want to add LUC CO2
        left_join(LUC_emissions_region_final, by = c("year", "Region", "Units", "BECCS", "DAC", "Scenario")) %>%
        mutate(total_GHG = total + total_CO2_LUC)-> GHG_emissions_USA

      p <- ggplot() + geom_line(data=GHG_emissions_USA, aes(x=year, y=total_GHG, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "GHG emissions by scenario" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2eq") +
        theme(panel.background = element_blank(),
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
              legend.text = element_text(size = 20))
      #ggsave("figures/GCAM_analysis/3a.GHG_emissions_USA.png", dpi=600/2, width=6000/300, height=3000/300)

      #### b. GHG emissions per state ####
      GHG_emissions_region_complete %>%
        filter(Region == "USA") %>%
        filter(total != is.na(total)) %>%
        group_by(Scenario, year, region, Units, scenario, DAC, BECCS) %>%
        summarise(GHG_eq_value_region = sum(total)) %>%
        ungroup()-> GHG_emissions_region_complete_states

      p <- ggplot() + geom_line(data=GHG_emissions_region_complete_states, aes(x=year, y=GHG_eq_value_region, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_wrap(region ~ ., scales = "free_y") +
        facet_wrap(facets = ~fct_reorder(region, GHG_eq_value_region, .desc = TRUE), scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "GHG Emissions by State" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2eq") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/3b.GHG_emissions_states.png", dpi=600/2, width=6000/300, height=3000/300)

      #### b. MAPS ####
      GHG_emissions_region_complete_states %>%
        filter(region %in% USA_states,
               year == 2050) %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> GHG_emissions_region_complete_states_2050

      p <- plot_usmap(region = c("state"),
                      data = GHG_emissions_region_complete_states_2050,
                      values = "GHG_eq_value_region") +
        ggtitle( "Total Energy GHG USA in 2050 in MtCO2eq") +
        #facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0, limits=c(-300,710)) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/3b.GHG_emissions_states_MAPS_2050.png", dpi=600/2, width=6000/300, height=3000/300)

      #### c. GHG emissions per gas ####
      #Note: states have GHGs from energy, but there are GHGs at USA level from resource production, backup energy, land, and agriculture
      #Here we get USA level CO2 from land
      LUC_emissions_region_final %>%
        filter(Region == "USA") %>%
        mutate(Gas = "CO2 Land") %>%
        rename(total = total_CO2_LUC) %>%
        dplyr::select(-scenario, -`Policy USA`, -`Policy Global`)-> LUC_emissions_region_final_merge

      #Here we get USA level
      non_CO2s_USA_level <- getQuery(prj, "nonCO2 emissions by sector")
      non_CO2s_USA_level_resource <- getQuery(prj, "nonCO2 emissions by resource production")

      non_CO2s_USA_level %>%
        filter(year > 2010 & year < 2055,
               region == "USA")  %>%
        left_join(GHG_gas_USA_mapping, by = c("ghg" = "GHG")) %>%
        left_join(GHG_mapping, by = c("ghg" = "GHG")) %>%
        mutate(total = value * GWP,
               Units = "MtCO2eq") %>%
        mutate(Region = (if_else(region %in% USA_all, "USA", "ROW")))%>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order))-> non_CO2s_USA_level_complete

      non_CO2s_USA_level_resource %>%
        filter(year > 2010 & year < 2055,
               region == "USA") %>%
        mutate(Sector = "Resource Production",
               Region = region) %>%
        left_join(GHG_mapping, by = c("ghg" = "GHG")) %>%
        mutate(total = value * GWP,
               Units = "MtCO2eq") %>%
        unite(Gas, c("ghg", "Sector"), remove = FALSE, sep = " ") %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order))-> non_CO2s_resource_USA_level_complete

      #Here we add state level + USA level tables
      GHG_emissions_region_complete %>%
        filter(region %in% USA_states) %>%
        left_join(GHG_gas_states_mapping, by = c("ghg" = "GHG")) %>%
        #Bring USA emissions
        bind_rows(non_CO2s_USA_level_complete) %>%
        #Bring USA resource production emissions
        bind_rows(non_CO2s_resource_USA_level_complete) %>%
        filter(total != is.na(total)) %>%
        bind_rows(LUC_emissions_region_final_merge) %>%
        group_by(Scenario, Gas, year, Region, Units, BECCS, DAC) %>%
        summarise(total = sum(total)) %>%
        ungroup() %>%
        #Bring USA LUC emissions
        mutate(Gas = factor(Gas, levels = GHG_gas_order)) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> GHG_emissions_region_by_gas

      p <- ggplot() + geom_col(data=GHG_emissions_region_by_gas, aes(x=year, y=total, fill= Gas)) +
        scale_fill_manual(values=GHG_gas_color) +
        #facet_grid( DAC ~ BECCS) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "Total GHG emissions in the US by gas" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2eq") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/3c.GHG_emissions_gas.png", dpi=600/2, width=6000/300, height=3000/300)

      #Now focus on 2050
      GHG_emissions_region_by_gas %>%
        filter(year == 2050) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order))-> GHG_emissions_region_by_gas_2050

      GHG_emissions_region_by_gas_2050 %>%
        filter(grepl("HFC", Gas) | grepl("N2O", Gas)) -> GHG_emissions_region_by_gas_2050_top

      GHG_emissions_region_by_gas_2050 %>%
        filter(grepl("CH4", Gas) | grepl("CO2", Gas)) -> GHG_emissions_region_by_gas_2050_bottom

      p <- ggplot() + geom_col(data=GHG_emissions_region_by_gas_2050_bottom, aes(x=year, y=total, fill= Gas)) +
        scale_fill_manual(values=GHG_gas_color) +
        facet_grid(Gas ~ Scenario, scale = "free_y") +
        #facet_wrap(Gas ~ ., ncol = 2) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(1, "lines")) +
        ggtitle( "Total GHG emissions in the US by gas in 2050" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("MtCO2eq") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_blank(),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=12),
              strip.text.y = element_blank(),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/3c.GHG_emissions_gas_2050_bot.png", dpi=600/2, width=6000/300, height=3000/300)

##### 4.Air pollution ####
      #### a. air pollution in the US ####
      GHG_emissions_region_complete %>%
        filter(ghg %in% air_pollution,
               #NOTE: USA level air pollution is from the land sector. State level is from energy sector
               region %in% USA_states) -> air_pollutants_complete

      # Here we add all US states to get a national value for energy
      air_pollutants_complete %>%
        filter(year < 2055) %>%
        group_by(Units, Scenario, ghg, Region, year, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollutants_USA

      p <- ggplot() + geom_line(data=air_pollutants_USA, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(ghg, sum, .desc = TRUE), scales = "free_y") +
        #facet_wrap(ghg ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Air Pollutants in the US from the energy sector" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
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
      #ggsave("figures/GCAM_analysis/4a.air_pollution_energy.png", dpi=600/2, width=6000/300, height=3000/300)
      
      #Here just do PM, SO2 and NOx
      air_pollutants_USA %>%
        filter(ghg %in% c("PM2.5", "PM10", "SO2_1", "NOx")) -> air_pollutants_USA_main4
      
      p <- ggplot() + geom_line(data=air_pollutants_USA_main4, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(ghg, sum, .desc = TRUE), scales = "free_y") +
        #facet_wrap(ghg ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Air Pollutants in the US from the energy sector" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
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
      #ggsave("figures/GCAM_analysis/4a.air_pollution_energy)main4.png", dpi=600/2, width=6000/300, height=3000/300)

      #Here we do bar graph
      air_pollutants_USA %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollutants_USA_bar

      p <- ggplot() + geom_col(data=air_pollutants_USA_bar, aes(x=year, y=sum, fill= ghg)) +
        scale_fill_manual(values=air_pollution_color) +
        #facet_grid( DAC ~ BECCS) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "Air Pollutants in the US from the energy sector" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4a.air_pollution_energy_column.png", dpi=600/2, width=6000/300, height=3000/300)

      #Here we do a map with all air pollution in 2050 per state (all pollutants)
      air_pollutants_complete %>%
        filter(year == 2050) %>%
        group_by(Units, Scenario, region, year, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollutants_USA_MAP

      p <- plot_usmap(region = c("states"),
                      data = air_pollutants_USA_MAP,
                      values = "sum") +
        ggtitle( "2050 Air Pollution in the US from the energy sector in Tg" ) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/4a.air_pollution_energy_column_MAPS_2050.png", dpi=600/2, width=6000/300, height=3000/300)

      # Here we just do USA region: resource production and land
      non_CO2s_USA_level %>% #"nonCO2 emissions by sector")
        filter(year > 2010 & year < 2055,
               ghg %in% air_pollution,
               #NOTE: USA level air pollution is from the land sector. State level is from energy sector
               region == "USA") %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order))%>%
        group_by(Units, Scenario, ghg, region, year, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollutants_USA_National_ag

      p <- ggplot() + geom_line(data=air_pollutants_USA_National_ag, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(ghg, sum, .desc = TRUE), scales = "free_y") +
        #facet_wrap(ghg ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Air Pollutants in the US from the land/ag sector" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
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
      #ggsave("figures/GCAM_analysis/4a.air_pollution_land.png", dpi=600/2, width=6000/300, height=3000/300)

      #National US level resource production air pollution
      non_CO2s_USA_level_resource %>% #"nonCO2 emissions by resource production")
        filter(year > 2010 & year < 2055,
               ghg %in% air_pollution,
               #NOTE: USA level air pollution is from the land sector. State level is from energy sector
               region == "USA") %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order))%>%
        group_by(Units, Scenario, ghg, region, year, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollutants_USA_National_res_prod

      p <- ggplot() + geom_line(data=air_pollutants_USA_National_res_prod, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(ghg, sum, .desc = TRUE), scales = "free_y") +
        #facet_wrap(ghg ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Air Pollutants in the US from resource production" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
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
      #ggsave("figures/GCAM_analysis/4a.air_pollution_resource_prod.png", dpi=600/2, width=6000/300, height=3000/300)


      #### b. air pollution in states ####
      #####CO####
      air_pollutants_complete %>% #NOTE: this table does not have national US (i.e. land and resource production pollution)
        filter(ghg %in% c("CO", "CO_AWB"),
               year < 2055) %>%
        group_by(Units, Scenario, region, year, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollutants_complete_CO

      p <- ggplot() + geom_line(data=air_pollutants_complete_CO, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(region, sum, .desc = TRUE), scales = "free_y") +
        #facet_wrap(region ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Carbon Monoxide Emissions in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/4b.CO_emissions_states.png", dpi=600/2, width=6000/300, height=3000/300)

      #### MAPS
      air_pollutants_complete_CO %>%
        filter(region %in% USA_states,
               year == 2050) %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollutants_complete_CO_2050

      p <- plot_usmap(region = c("state"),
                      data = air_pollutants_complete_CO_2050,
                      values = "sum") +
        ggtitle( "Carbon Monoxide Emissions in the US in 2050 in Tg") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y", as.table = F) +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0, limits=c(0,1.6)) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/4b.CO_emissions_states_MAP.png", dpi=600/2, width=6000/300, height=3000/300)


      ####NOx####
      air_pollutants_complete %>% #NOTE: this table does not have national US (i.e. land and resource production pollution)
        filter(ghg %in% c("NOx", "NOx_AGR", "NOx_AWB"),
               year < 2055) %>%
        group_by(Units, Scenario, region, year, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollutants_complete_NOx

      p <- ggplot() + geom_line(data=air_pollutants_complete_NOx, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_wrap(region ~ ., scales = "free_y") +
        facet_wrap(facets = ~fct_reorder(region, sum, .desc = TRUE), scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Nitrous Oxide Emissions in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/4b.NOx_emissions_states.png", dpi=600/2, width=6000/300, height=3000/300)

      #### MAPS
      air_pollutants_complete_NOx %>%
        filter(region %in% USA_states,
               year == 2050) %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollutants_complete_NOx_2050

      p <- plot_usmap(region = c("state"),
                      data = air_pollutants_complete_NOx_2050,
                      values = "sum") +
        ggtitle( "NOx Emissions in the US in 2050 in Tg") +
        #facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0, limits=c(0,1)) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/4b.NOx_emissions_states_MAP.png", dpi=600/2, width=6000/300, height=3000/300)

      ####PM10####
      air_pollutants_complete %>% #NOTE: this table does not have national US (i.e. land and resource production pollution)
        filter(ghg %in% c("PM10"),
               year < 2055) %>%
        group_by(Units, Scenario, region, year, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollutants_complete_PM10

      p <- ggplot() + geom_line(data=air_pollutants_complete_PM10, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_wrap(region ~ ., scales = "free_y") +
        facet_wrap(facets = ~fct_reorder(region, sum, .desc = TRUE), scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "PM10 Emissions in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/4b.PM10_emissions_states.png", dpi=600/2, width=6000/300, height=3000/300)

      #### MAPS
      air_pollutants_complete_PM10 %>%
        filter(region %in% USA_states,
               year == 2050) %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollutants_complete_PM10_2050

      p <- plot_usmap(region = c("state"),
                      data = air_pollutants_complete_PM10_2050,
                      values = "sum") +
        ggtitle( "PM10 Emissions in the US in 2050 in Tg") +
        #facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0, limits=c(0,1)) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/4b.PM10_emissions_states_MAP.png", dpi=600/2, width=6000/300, height=3000/300)


      ####PM2.5####
      air_pollutants_complete %>% #NOTE: this table does not have national US (i.e. land and resource production pollution)
        filter(ghg %in% c("PM2.5"),
               year < 2055) %>%
        group_by(Units, Scenario, region, year, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollutants_complete_PM2.5

      p <- ggplot() + geom_line(data=air_pollutants_complete_PM2.5, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_wrap(region ~ ., scales = "free_y") +
        facet_wrap(facets = ~fct_reorder(region, sum, .desc = TRUE), scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "PM2.5 Emissions in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/4b.PM2.5_emissions_states.png", dpi=600/2, width=6000/300, height=3000/300)

      #### MAPS
      air_pollutants_complete_PM2.5 %>%
        filter(region %in% USA_states,
               year == 2050) %>%
        filter(Scenario != "Reference") %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollutants_complete_PM2.5_2050

      p <- plot_usmap(region = c("state"),
                      data = air_pollutants_complete_PM2.5_2050,
                      values = "sum") +
        ggtitle( "PM2.5 Emissions in the US in 2050 in Tg") +
        #facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0, limits=c(0,0.2)) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/4b.PM2.5_emissions_states_MAP_V2.png", dpi=600/2, width=6000/300, height=3000/300)

      ####SO2####
      air_pollutants_complete %>% #NOTE: this table does not have national US (i.e. land and resource production pollution)
        filter(ghg %in% c("SO2_1", "SO2_1_AWB"),
               year < 2055) %>%
        group_by(Units, Scenario, region, year, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollutants_complete_SO2

      p <- ggplot() + geom_line(data=air_pollutants_complete_SO2, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_wrap(region ~ ., scales = "free_y") +
        facet_wrap(facets = ~fct_reorder(region, sum, .desc = TRUE), scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Sulfur Dioxide Emissions in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/4b.SO2_emissions_states.png", dpi=600/2, width=6000/300, height=3000/300)

      #### MAPS
      air_pollutants_complete_SO2 %>%
        filter(region %in% USA_states,
               year == 2050) %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollutants_complete_SO2_2050

      p <- plot_usmap(region = c("state"),
                      data = air_pollutants_complete_SO2_2050,
                      values = "sum") +
        ggtitle( "SO2 Emissions in the US in 2050 in Tg") +
        #facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0, limits=c(0,0.4)) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/4b.SO2_emissions_states_MAP.png", dpi=600/2, width=6000/300, height=3000/300)

      ####BC####
      air_pollutants_complete %>% #NOTE: this table does not have national US (i.e. land and resource production pollution)
        filter(ghg %in% c("BC"),
               year < 2055) %>%
        group_by(Units, Scenario, region, year, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollutants_complete_BC

      p <- ggplot() + geom_line(data=air_pollutants_complete_BC, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_wrap(region ~ ., scales = "free_y") +
        facet_wrap(facets = ~fct_reorder(region, sum, .desc = TRUE), scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Black Carbon Emissions in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/4b.BC_emissions_states.png", dpi=600/2, width=6000/300, height=3000/300)

      #### MAPS
      air_pollutants_complete_BC %>%
        filter(region %in% USA_states,
               year == 2050) %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollutants_complete_BC_2050

      p <- plot_usmap(region = c("state"),
                      data = air_pollutants_complete_BC_2050,
                      values = "sum") +
        ggtitle( "BC Emissions in the US in 2050 in Tg") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0, limits=c(0,0.03)) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/4b.BC_emissions_states_MAP.png", dpi=600/2, width=6000/300, height=3000/300)

      ####NH3####
      air_pollutants_complete %>% #NOTE: this table does not have national US (i.e. land and resource production pollution)
        filter(ghg %in% c("NH3"),
               year < 2055) %>%
        group_by(Units, Scenario, region, year, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollutants_complete_NH3

      p <- ggplot() + geom_line(data=air_pollutants_complete_NH3, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_wrap(region ~ ., scales = "free_y") +
        facet_wrap(facets = ~fct_reorder(region, sum, .desc = TRUE), scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Ammonia Emissions in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/4b.NH3_emissions_states.png", dpi=600/2, width=6000/300, height=3000/300)

      #### MAPS
      air_pollutants_complete_NH3 %>%
        filter(region %in% USA_states,
               year == 2050) %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollutants_complete_NH3_2050

      p <- plot_usmap(region = c("state"),
                      data = air_pollutants_complete_NH3_2050,
                      values = "sum") +
        ggtitle( "NH3 Emissions in the US in 2050 in Tg") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0, limits=c(0,0.05)) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/4b.NH3_emissions_states_MAP.png", dpi=600/2, width=6000/300, height=3000/300)

      ####NMVOC####
      air_pollutants_complete %>% #NOTE: this table does not have national US (i.e. land and resource production pollution)
        filter(ghg %in% c("NMVOC"),
               year < 2055) %>%
        group_by(Units, Scenario, region, year, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollutants_complete_NMVOC

      p <- ggplot() + geom_line(data=air_pollutants_complete_NMVOC, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_wrap(region ~ ., scales = "free_y") +
        facet_wrap(facets = ~fct_reorder(region, sum, .desc = TRUE), scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Non-methane volatile organic compounds in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/4b.NMVOC_emissions_states.png", dpi=600/2, width=6000/300, height=3000/300)

      #### MAPS
      air_pollutants_complete_NMVOC %>%
        filter(region %in% USA_states,
               year == 2050) %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollutants_complete_NMVOC_2050

      p <- plot_usmap(region = c("state"),
                      data = air_pollutants_complete_NMVOC_2050,
                      values = "sum") +
        ggtitle( "NMVOC Emissions in the US in 2050 in Tg") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0, limits=c(0,1.2)) + #limits=c(-250,700)
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/4b.NMVOC_emissions_states_MAP.png", dpi=600/2, width=6000/300, height=3000/300)

      ####OC####
      air_pollutants_complete %>% #NOTE: this table does not have national US (i.e. land and resource production pollution)
        filter(ghg %in% c("OC"),
               year < 2055) %>%
        group_by(Units, Scenario, region, year, DAC, BECCS) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollutants_complete_OC

      p <- ggplot() + geom_line(data=air_pollutants_complete_OC, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_wrap(region ~ ., scales = "free_y") +
        facet_wrap(facets = ~fct_reorder(region, sum, .desc = TRUE), scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Organic carbon emissions in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/4b.OC_emissions_states.png", dpi=600/2, width=6000/300, height=3000/300)

      #### MAPS
      air_pollutants_complete_OC %>%
        filter(region %in% USA_states,
               year == 2050) %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollutants_complete_OC_2050

      p <- plot_usmap(region = c("state"),
                      data = air_pollutants_complete_OC_2050,
                      values = "sum") +
        ggtitle( "OC Emissions in the US in 2050 in Tg") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0, limits=c(0,0.03)) + #limits=c(-250,700)
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/4b.OC_emissions_states_MAP.png", dpi=600/2, width=6000/300, height=3000/300)

      #### d. air pollution by source####
      air_pollution_sources <- getQuery(prj, "air pollution nonCO2 emissions by tech")
      air_pollution_sources_electricity <- getQuery(prj, "air pollution nonCO2 emissions by elec tech (USA)")

      air_pollution_sources %>%
        bind_rows(air_pollution_sources_electricity) -> air_pollution_source_all

      air_pollution_source_all %>%
        filter(year > 2010 & year < 2055,
               region %in% USA_states) %>% #Get rid of USA national level air pollution (land and resource production)
        left_join(air_pollution_sector_mapping, by = "sector") %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order)) %>%
        mutate(Sector = factor(Sector, levels = air_pollution_source_order))-> air_pollution_sources_complete

      #BC
      air_pollution_sources_complete %>%
        filter(ghg == "BC") %>%
        group_by(Units, ghg, year, Sector, Scenario, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_BC

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_BC, aes(x=year, y=sum, fill= Sector)) +
        scale_fill_manual(values=pollution_BC_color) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "BC Air Pollution by Source in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4d.air_pollution_source_BC.png", dpi=600/2, width=6000/300, height=3000/300)

      #CO
      air_pollution_sources_complete %>%
        filter(ghg %in% c("CO", "CO_AWB")) %>%
        group_by(Units, ghg, year, Sector, Scenario, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_CO

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_CO, aes(x=year, y=sum, fill= Sector)) +
        scale_fill_manual(values=pollution_BC_color) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "CO Air Pollution by Source in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4d.air_pollution_source_CO.png", dpi=600/2, width=6000/300, height=3000/300)

      #NH3
      air_pollution_sources_complete %>%
        filter(ghg == "NH3") %>%
        group_by(Units, ghg, year, Sector, Scenario, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_NH3

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_NH3, aes(x=year, y=sum, fill= Sector)) +
        scale_fill_manual(values=pollution_BC_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "NH3 Air Pollution by Source in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4d.air_pollution_source_NH3.png", dpi=600/2, width=6000/300, height=3000/300)

      #NMVOC
      air_pollution_sources_complete %>%
        filter(ghg == "NMVOC") %>%
        group_by(Units, ghg, year, Sector, Scenario, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_NMVOC

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_NMVOC, aes(x=year, y=sum, fill= Sector)) +
        scale_fill_manual(values=pollution_BC_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "NMVOC Air Pollution by Source in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4d.air_pollution_source_NMVOC.png", dpi=600/2, width=6000/300, height=3000/300)


      #NOx
      air_pollution_sources_complete %>%
        filter(ghg %in% c("NOx", "NOx_AGR", "NOx_AWB")) %>%
        group_by(Units, ghg, year, Sector, Scenario, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_NOx

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_NOx, aes(x=year, y=sum, fill= Sector)) +
        scale_fill_manual(values=pollution_BC_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        # facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "NOx Air Pollution by Source in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4d.air_pollution_source_NOx.png", dpi=600/2, width=6000/300, height=3000/300)

      #OC
      air_pollution_sources_complete %>%
        filter(ghg == "OC") %>%
        group_by(Units, ghg, year, Sector, Scenario, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_OC

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_OC, aes(x=year, y=sum, fill= Sector)) +
        scale_fill_manual(values=pollution_BC_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        # facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "OC Air Pollution by Source in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4d.air_pollution_source_OC.png", dpi=600/2, width=6000/300, height=3000/300)

      #PM2.5
      air_pollution_sources_complete %>%
        filter(ghg == "PM2.5") %>%
        group_by(Units, ghg, year, Sector, Scenario, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_PM2p5

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_PM2p5, aes(x=year, y=sum, fill= Sector)) +
        scale_fill_manual(values=pollution_BC_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        # facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "PM2.5 Air Pollution by Source in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4d.air_pollution_source_PM2p5.png", dpi=600/2, width=6000/300, height=3000/300)

      #PM10
      air_pollution_sources_complete %>%
        filter(ghg == "PM10") %>%
        group_by(Units, ghg, year, Sector, Scenario, DAC, BECCS) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_PM10

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_PM10, aes(x=year, y=sum, fill= Sector)) +
        scale_fill_manual(values=pollution_BC_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        # facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "PM10 Air Pollution by Source in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4d.air_pollution_source_PM10.png", dpi=600/2, width=6000/300, height=3000/300)


      #SO2
      air_pollution_sources_complete %>%
        filter(ghg%in% c("SO2_1", "SO2_1_AWB")) %>%
        group_by(Units, ghg, year, Sector, Scenario, BECCS, DAC) %>%
        summarise(sum = sum(value)) %>%
        ungroup()%>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_SO2

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_SO2, aes(x=year, y=sum, fill= Sector)) +
        scale_fill_manual(values=pollution_BC_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        # facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        ggtitle( "SO2 Air Pollution by Source in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4d.air_pollution_source_SO2.png", dpi=600/2, width=6000/300, height=3000/300)

      #### e. Now by sector ####
      #Split data based on gases "BC"    "CO"    "NH3"   "NMVOC" "NOx"   "OC"    "PM10"  "PM2.5" "SO2_1"
      air_pollution_sources_complete %>% #Excludes national USA level air pollution (resource production / ag and land)
        group_by(region,Units, year, ghg, Sector, BECCS, DAC, Scenario) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollution_sources_complete_USA
      
      #1. Buildings
      air_pollution_sources_complete_USA %>%
        filter(Sector == "Buildings") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_USA_buildings
      
      p <- ggplot() + geom_line(data=air_pollution_sources_complete_USA_buildings, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(ghg, sum, .desc = TRUE), scales = "free_y") +
        #facet_wrap(ghg ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Air Pollution from Buildings by Gas in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
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
      #ggsave("figures/GCAM_analysis/4e.air_pollution_buildings.png", dpi=600/2, width=6000/300, height=3000/300)

      #2. Industry
      air_pollution_sources_complete_USA %>%
        filter(Sector == "Industry")  %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_USA_Industry
      
      p <- ggplot() + geom_line(data=air_pollution_sources_complete_USA_Industry, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(ghg, sum, .desc = TRUE), scales = "free_y") +
        #facet_wrap(ghg ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Air Pollution from Industry by Gas in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
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
      #ggsave("figures/GCAM_analysis/4e.air_pollution_Industry.png", dpi=600/2, width=6000/300, height=3000/300)

      #3. Transportation
      air_pollution_sources_complete_USA %>%
        filter(Sector == "Transportation") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollution_sources_complete_USA_Transportation
      
      p <- ggplot() + geom_line(data=air_pollution_sources_complete_USA_Transportation, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(ghg, sum, .desc = TRUE), scales = "free_y") +
        #facet_wrap(ghg ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Air Pollution from Transportation by Gas in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
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
      #ggsave("figures/GCAM_analysis/4e.air_pollution_Transportation.png", dpi=600/2, width=6000/300, height=3000/300)

      #4. Electricity
      air_pollution_sources_complete_USA %>%
        filter(Sector == "Electricity") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollution_sources_complete_USA_Electricity

      air_pollution_sources_complete_USA_Electricity%>%
        group_by(Units, Scenario, ghg, year, Sector) %>%
        summarise(sum = sum(sum)) %>%
        ungroup()-> air_pollution_sources_complete_USA_Electricity_graph

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_USA_Electricity_graph, aes(x=year, y=sum, fill= ghg)) +
        scale_fill_manual(values=air_pollution_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        ggtitle( "Air Pollution from Electricity by Gas in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4e.air_pollution_Electricity.png", dpi=600/2, width=6000/300, height=3000/300)

      p <- ggplot() + geom_line(data=air_pollution_sources_complete_USA_Electricity_graph, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(ghg, sum, .desc = TRUE), scales = "free_y") +
        #facet_wrap(ghg ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Air Pollution from Electricity by Gas in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
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
      #ggsave("figures/GCAM_analysis/4e.air_pollution_Electricity_lines.png", dpi=600/2, width=6000/300, height=3000/300)
      
      #Double check Vermont
      air_pollution_sources_complete_USA_Electricity %>%
        filter(region == "CA") -> air_pollution_sources_complete_USA_Electricity_VT
      
      p <- ggplot() + geom_line(data=air_pollution_sources_complete_USA_Electricity_VT, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(ghg, sum, .desc = TRUE), scales = "free_y") +
        facet_wrap(ghg ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Air Pollution from Electricity by Gas in CA" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
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
      #ggsave("figures/GCAM_analysis/4e.air_pollution_Electricity_lines_CA.png", dpi=600/2, width=6000/300, height=3000/300)
      
      
      #Here focus on 4
      air_pollution_sources_complete_USA_Electricity_graph %>%
        filter(ghg %in% c("PM2.5", "PM10", "SO2_1", "NOx")) -> air_pollution_sources_complete_USA_Electricity_graph_main4

      p <- ggplot() + geom_line(data=air_pollution_sources_complete_USA_Electricity_graph_main4, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(ghg, sum, .desc = TRUE), scales = "free_y") +
        #facet_wrap(ghg ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Air Pollution from Electricity by Gas in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
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
      #ggsave("figures/GCAM_analysis/4e.air_pollution_Electricity_lines_main4.png", dpi=600/2, width=6000/300, height=3000/300)
      
      #Here we focus on PM10, which increases in high scenario
      air_pollution_sources_complete %>% #Excludes national USA level air pollution (resource production / ag and land)
        group_by(Units, year, ghg, Sector, sector, technology, BECCS, DAC, Scenario) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollution_sources_complete_USA_all_sectors
      
      air_pollution_sources_complete_USA_all_sectors %>%
        filter(ghg == "PM10",
               Sector == "Electricity") %>%
        separate(technology, c("Fuel", "tech", "water"), sep = " ", remove = FALSE) %>%
        mutate(Fuel = if_else(Fuel == "refined", "refined liquids", Fuel)) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> air_pollution_sources_complete_USA_PM10_elec
  
                   
      p <- ggplot() + geom_col(data=air_pollution_sources_complete_USA_PM10_elec, aes(x=year, y=sum, fill= Fuel)) +
        scale_fill_manual(values=fuel_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        # facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        ggtitle( "PM10 Air Pollution from Electricity in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4e.air_pollution_Electricity_col_main_PM10.png", dpi=600/2, width=6000/300, height=3000/300)
      
      #6. Urban processes
      air_pollution_sources_complete_USA %>%
        filter(Sector == "Urban processes") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_USA_urban

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_USA_urban, aes(x=year, y=sum, fill= ghg)) +
        scale_fill_manual(values=air_pollution_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        ggtitle( "Air Pollution from urban processes by Gas in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4e.air_pollution_urban.png", dpi=600/2, width=6000/300, height=3000/300)

      #7. Refining
      air_pollution_sources_complete_USA %>%
        filter(Sector == "Refining") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_USA_Refining

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_USA_Refining, aes(x=year, y=sum, fill= ghg)) +
        scale_fill_manual(values=air_pollution_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        ggtitle( "Air Pollution from Refining by Gas in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4e.air_pollution_Refining.png", dpi=600/2, width=6000/300, height=3000/300)

      #### f. Dive into electricity air pollution, which increases with policy scenarios ####
      air_pollution_sources_complete %>%
        filter(Sector == "Electricity") %>%
        rename(Fuel = subsector...5)-> air_pollution_sources_complete_Electricity

      air_pollution_sources_complete_Electricity %>%
        group_by(Units, Scenario, ghg, year, Fuel, Sector) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> air_pollution_sources_complete_Electricity_fuel

      #Check CO
      air_pollution_sources_complete_Electricity_fuel %>%
        filter(ghg == "CO") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_Electricity_fuel_CO

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_Electricity_fuel_CO, aes(x=year, y=sum, fill= Fuel)) +
        scale_fill_manual(values=fuel_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        ggtitle( "CO Air Pollution from Electricity by fuel in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4f.air_pollution_electricity_CO.png", dpi=600/2, width=6000/300, height=3000/300)

      air_pollution_sources_complete_Electricity %>%
        filter(ghg == "CO",
               year == 2050,
               Fuel == "biomass") %>%
        group_by(Units, Scenario, ghg, year, Fuel, Sector, region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_Electricity_fuel_CO_MAP

      p <- plot_usmap(region = c("states"),
                      data = air_pollution_sources_complete_Electricity_fuel_CO_MAP,
                      values = "sum") +
        ggtitle( "2050 CO Air Pollution from Biomass Electricity in the US in Tg") +
        facet_wrap( ~ Scenario, ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) + #limits=c(-250,700)
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/4f.air_pollution_electricity_CO_MAP.png", dpi=600/2, width=6000/300, height=3000/300)

      #Check PM10
      air_pollution_sources_complete_Electricity_fuel %>%
        filter(ghg == "PM10") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_Electricity_fuel_PM10

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_Electricity_fuel_PM10, aes(x=year, y=sum, fill= Fuel)) +
        scale_fill_manual(values=fuel_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        ggtitle( "PM10 Air Pollution from Electricity by fuel in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4f.air_pollution_electricity_PM10.png", dpi=600/2, width=6000/300, height=3000/300)

      air_pollution_sources_complete_Electricity %>%
        mutate(CCS = ifelse(grepl("CCS", technology), "CCS", "no CCS")) %>%
        unite(Source, c("Fuel", "CCS"), remove = FALSE, sep = " ") %>%
        filter(Fuel == "coal",
               ghg == "PM10") %>%
        group_by(Units, Scenario, ghg, year, Source, Sector) %>%
        summarise(sum = sum(value)) %>%
        ungroup()-> air_pollution_sources_complete_Electricity_PM10_coal

      p <- ggplot() + geom_line(data=air_pollution_sources_complete_Electricity_PM10_coal, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(Source, sum, .desc = TRUE)) +
        #facet_wrap(ghg ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "PM10 Air Pollution from Coal Electricity by fuel in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
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
      #ggsave("figures/GCAM_analysis/4f.air_pollution_electricity_PM10_coal.png", dpi=600/2, width=6000/300, height=3000/300)

      air_pollution_sources_complete_Electricity %>%
        filter(ghg == "PM10",
               year == 2050,
               Fuel == "coal") %>%
        group_by(Units, Scenario, ghg, year, Fuel, Sector, region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_Electricity_fuel_PM10_MAP

      p <- plot_usmap(region = c("states"),
                      data = air_pollution_sources_complete_Electricity_fuel_PM10_MAP,
                      values = "sum") +
        ggtitle( "2050 PM10 Air Pollution from Coal Electricity in the US in Tg") +
        facet_wrap( ~ Scenario, ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) + #limits=c(-250,700)
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/4f.air_pollution_electricity_PM10_MAP.png", dpi=600/2, width=6000/300, height=3000/300)

      #Check PM2.5
      air_pollution_sources_complete_Electricity_fuel %>%
        filter(ghg == "PM2.5") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_Electricity_fuel_PM2.5

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_Electricity_fuel_PM2.5, aes(x=year, y=sum, fill= Fuel)) +
        scale_fill_manual(values=fuel_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        ggtitle( "PM2.5 Air Pollution from Electricity by fuel in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4f.air_pollution_electricity_PM2.5.png", dpi=600/2, width=6000/300, height=3000/300)

      #Check OC
      air_pollution_sources_complete_Electricity_fuel %>%
        filter(ghg == "OC") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_Electricity_fuel_OC

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_Electricity_fuel_OC, aes(x=year, y=sum, fill= Fuel)) +
        scale_fill_manual(values=fuel_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        ggtitle( "OC Air Pollution from Electricity by fuel in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4f.air_pollution_electricity_OC.png", dpi=600/2, width=6000/300, height=3000/300)

      #Check BC
      air_pollution_sources_complete_Electricity_fuel %>%
        filter(ghg == "BC") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_Electricity_fuel_BC

      p <- ggplot() + geom_col(data=air_pollution_sources_complete_Electricity_fuel_BC, aes(x=year, y=sum, fill= Fuel)) +
        scale_fill_manual(values=fuel_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        ggtitle( "BC Air Pollution from Electricity by fuel in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4f.air_pollution_electricity_BC.png", dpi=600/2, width=6000/300, height=3000/300)

      #Check PM2.5
      air_pollution_sources_complete_Electricity_fuel %>%
        filter(ghg == "PM2.5") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> air_pollution_sources_complete_Electricity_fuel_PM2.5
      
      p <- ggplot() + geom_col(data=air_pollution_sources_complete_Electricity_fuel_PM2.5, aes(x=year, y=sum, fill= Fuel)) +
        scale_fill_manual(values=fuel_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        ggtitle( "PM2.5 Air Pollution from Electricity by fuel in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/4f.air_pollution_electricity_pm2.5.png", dpi=600/2, width=6000/300, height=3000/300)
      
      
#####5. Primary Energy consumption #####
      #### a. in the US ####
      primary_energy_consumption <- getQuery(prj, "primary energy consumption by region (direct equivalent)")

      primary_energy_consumption %>%
        filter(region %in% USA_states | region == "USA",
               year > 2010 & year < 2055,
               !fuel %in% c("traded coal", "traded natural gas", "traded oil")) %>%
        left_join(scenario_mapping, by = "scenario") -> primary_energy_consumption_states

      primary_energy_consumption_states %>%
        left_join(PE_fuel_mapping, by = "fuel") %>%
        group_by(year, Units, Scenario, agg_fuel) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Fuel = factor(agg_fuel, levels = primary_energy_order)) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> primary_energy_consumption_USA

      p <- ggplot() + geom_col(data=primary_energy_consumption_USA, aes(x=year, y=sum, fill= Fuel)) +
        scale_fill_manual(values=primary_energy_color) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = FALSE) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "Primary Energy Consumption by Fuel" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/5a.primary_energy_fuel_USA.png", dpi=600/2, width=6000/300, height=3000/300)

      #### b. in each state ####
      #Texas
      primary_energy_consumption_states %>%
        filter(region == "TX") %>%
        left_join(PE_fuel_mapping, by = "fuel") %>%
        group_by(year, Units, Scenario, agg_fuel, region) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Fuel = factor(agg_fuel, levels = primary_energy_order))%>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> primary_energy_consumption_states_TX

      p <- ggplot() + geom_col(data=primary_energy_consumption_states_TX, aes(x=year, y=sum, fill= Fuel)) +
        scale_fill_manual(values=primary_energy_color) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = FALSE) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "Primary Energy Consumption by Fuel in Texas" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/5a.primary_energy_fuel_states_TX.png", dpi=600/2, width=6000/300, height=3000/300)

      primary_energy_consumption_USA %>%
        group_by(Units, year, Scenario) %>%
        mutate(total_energy = sum(sum)) %>%
        ungroup() %>%
        mutate(percent = round((sum * 100) / total_energy))-> primary_energy_consumption_USA_percentages
      
##### 6. final energy by sector#####
      #### a. final energy in the US ####
      final_energy_sector <- getQuery(prj, "total final energy by aggregate sector")

      final_energy_sector %>%
        filter(region %in% USA_states,
               year > 2010 & year < 2055) %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Sector = factor(sector, levels = final_energy_order)) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> final_energy_sector_states

      final_energy_sector_states %>%
        group_by(year, Units, Scenario, sector) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Sector = factor(sector, levels = final_energy_order)) -> final_energy_sector_USA

      p <- ggplot() + geom_col(data=final_energy_sector_USA, aes(x=year, y=sum, fill= Sector)) +
        scale_fill_manual(values=final_energy_sectors_color) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = FALSE) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "Final Energy by Sector" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/6a.final_energy_sector_USA.png", dpi=600/2, width=6000/300, height=3000/300)

      #### b. final energy by state ####
      #Here we want to see the energy per state: TX, LA, CA, FL
      #Texas
      final_energy_sector_states %>%
        filter(region == "TX") -> final_energy_sector_states_TX

      p <- ggplot() + geom_col(data=final_energy_sector_states_TX, aes(x=year, y=value, fill= Sector)) +
        scale_fill_manual(values=final_energy_sectors_color) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = FALSE) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "Final Energy by Sector in Texas" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/6b.final_energy_sector_TX.png", dpi=600/2, width=6000/300, height=3000/300)

      #Louisiana
      final_energy_sector_states %>%
        filter(region == "LA") -> final_energy_sector_states_LA

      p <- ggplot() + geom_col(data=final_energy_sector_states_LA, aes(x=year, y=value, fill= Sector)) +
        scale_fill_manual(values=final_energy_sectors_color) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = FALSE) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "Final Energy by Sector in Louisiana" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/6b.final_energy_sector_LA.png", dpi=600/2, width=6000/300, height=3000/300)

      #California
      final_energy_sector_states %>%
        filter(region == "CA") -> final_energy_sector_states_CA

      p <- ggplot() + geom_col(data=final_energy_sector_states_CA, aes(x=year, y=value, fill= Sector)) +
        scale_fill_manual(values=final_energy_sectors_color) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = FALSE) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "Final Energy by Sector in California" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/6b.final_energy_sector_CA.png", dpi=600/2, width=6000/300, height=3000/300)

      #Florida
      final_energy_sector_states %>%
        filter(region == "FL") -> final_energy_sector_states_FL

      p <- ggplot() + geom_col(data=final_energy_sector_states_FL, aes(x=year, y=value, fill= Sector)) +
        scale_fill_manual(values=final_energy_sectors_color) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = FALSE) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "Final Energy by Sector in Florida" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/6b.final_energy_sector_FL.png", dpi=600/2, width=6000/300, height=3000/300)


##### 7. Final energy buildings by fuel #####
      #### a. Final energy buildings in the US ####
      buildings_final_energy <- getQuery(prj, "building final energy by fuel")

      buildings_final_energy %>%
        filter(region %in% USA_states,
               year > 2010 & year < 2055) %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) %>%
        left_join(FE_fuel_mapping, by = "input") %>%
        mutate(Fuel = factor(Fuel, levels = final_energy_fuel_order))-> buildings_final_energy_states

      buildings_final_energy_states %>%
        group_by(Units, Scenario, Fuel, year) %>%
        summarise(sum = sum(value)) %>%
        ungroup() -> buildings_final_energy_USA

      p <- ggplot() + geom_col(data=buildings_final_energy_USA, aes(x=year, y=sum, fill= Fuel)) +
        scale_fill_manual(values=final_energy_fuel_color) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = FALSE) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "Final Energy in Buildings USA" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/7a.final_energy_buildings_USA.png", dpi=600/2, width=6000/300, height=3000/300)


##### 8. Final energy industry by fuel #####
      #### a. Final energy industry in the US ####
      industry_final_energy <- getQuery(prj, "industry final energy by fuel")

      industry_final_energy %>%
      filter(region %in% USA_states,
            year > 2010 & year < 2055) %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))%>%
        left_join(FE_fuel_mapping, by = "input") %>%
        mutate(Fuel = factor(Fuel, levels = final_energy_fuel_order))-> industry_final_energy_states

      industry_final_energy_states %>%
        group_by(Units, Scenario, Fuel, year) %>%
        summarise(sum = sum(value)) %>%
        ungroup()  -> industry_final_energy_USA

      p <- ggplot() + geom_col(data=industry_final_energy_USA, aes(x=year, y=sum, fill= Fuel)) +
        scale_fill_manual(values=final_energy_fuel_color) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = FALSE) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "Final Energy in Industry USA" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/8a.final_energy_industry_USA.png", dpi=600/2, width=6000/300, height=3000/300)


##### 9. Final energy transport by fuel #####
      #### a. Final energy transport in the US ####
      transport_final_energy <- getQuery(prj, "transport final energy by fuel")

      transport_final_energy %>%
        filter(region %in% USA_states,
               year > 2010 & year < 2055) %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))%>%
        left_join(FE_fuel_mapping, by = "input") %>%
        mutate(Fuel = factor(Fuel, levels = final_energy_fuel_order))-> transport_final_energy_states

      transport_final_energy_states %>%
        group_by(Units, Scenario, Fuel, year) %>%
        summarise(sum = sum(value)) %>%
        ungroup()  -> transport_final_energy_USA

      p <- ggplot() + geom_col(data=transport_final_energy_USA, aes(x=year, y=sum, fill= Fuel)) +
        scale_fill_manual(values=final_energy_fuel_color) +
        facet_wrap( ~ Scenario, ncol = 2, as.table = FALSE) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        ggtitle( "Final Energy in Transport USA" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/9a.final_energy_transport_USA.png", dpi=600/2, width=6000/300, height=3000/300)


##### 10. Electricity generation #####
      #### a. Electricity generation in the US ####
    elec_gen <- getQuery(prj, "elec gen by region (no CHP)")

      elec_gen %>%
        filter(year > 2010 & year < 2055,
               region %in% USA_states | region == "USA") %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> elec_gen_final

      #MAP
      elec_gen_final %>%
        filter(year == 2050) %>%
        rename(state = region) -> elec_gen_final_MAP

      p <- plot_usmap(region = c("state"),
                      data = elec_gen_final_MAP,
                      values = "value") +
        ggtitle( "Electricity Generation in the US in 2050 (EJ)") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#08A045", mid = "#FAF0CA", high = "#007EA7", midpoint = 0) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/10a.elec_gen_states_MAP.png", dpi=600/2, width=6000/300, height=3000/300)


      #### b. electricity generation by subsector ####
      elec_gen_subsector <- getQuery(prj, "elec gen by subsector")

      elec_gen_subsector %>%
        filter(year > 2010 & year < 2055) %>%
        group_by(Units, scenario, subsector...4, year) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        left_join(scenario_mapping, by = "scenario") %>%
        rename(Source = subsector...4) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> elec_gen_subsector_fuel

      p <- ggplot() + geom_col(data=elec_gen_subsector_fuel, aes(x=year, y=sum, fill= Source)) +
        scale_fill_manual(values=elec_gen_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        ggtitle( "Electricity Generation by Fuel in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/10b.elec_gen_fuel.png", dpi=600/2, width=6000/300, height=3000/300)

      # Want to see biomass electricity and coal electricity
      elec_gen_subsector %>%
        filter(year > 2010 & year < 2055) %>%
        mutate(CCS = ifelse(grepl("CCS", subsector...5), "CCS", "no CCS")) %>%
        unite(Source, c("subsector...4", "CCS"), remove = FALSE, sep = " ") %>%
        group_by(Units, scenario, Source, year) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order)) %>%
        filter(grepl("coal", Source) | grepl("biomass", Source)) -> elec_gen_subsector_fuel_coal_biomass

      p <- ggplot() + geom_line(data=elec_gen_subsector_fuel_coal_biomass, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        facet_wrap(facets = ~fct_reorder(Source, sum, .desc = TRUE), scales = "free_y") +
        #facet_wrap(ghg ~ ., scales = "free_y") +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Electricity Generation by Fuel in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
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
      #ggsave("figures/GCAM_analysis/10b.elec_gen_fuel_coal_biomass.png", dpi=600/2, width=6000/300, height=3000/300)


##### 11. Refined liquids #####
      refined_liquids_tech <- getQuery (prj, "refined liquids production by tech")

      refined_liquids_tech %>%
        filter(year > 2010 & year < 2055) %>%
        left_join(refined_liquids_mapping, by = "technology") %>%
        left_join(scenario_mapping, by = "scenario") %>%
        group_by(Units, region, year, Scenario, Fuel) %>%
        summarise(sum = sum(value)) %>%
        ungroup()-> refined_liquids_tech_final

      refined_liquids_tech_final %>%
        group_by(Units, year, Scenario, Fuel) %>%
        summarise(sum = sum(sum)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) %>%
        mutate(Fuel = factor(Fuel, levels = refining_tech_order))-> refined_liquids_tech_final_USA

      p <- ggplot() + geom_col(data=refined_liquids_tech_final_USA, aes(x=year, y=sum, fill= Fuel)) +
        scale_fill_manual(values=refining_tech_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        ggtitle( "Refined Liquids by Fuel in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/11.refined_liquids_tech.png", dpi=600/2, width=6000/300, height=3000/300)


##### 12. Biomass #####
      #### a. purpose-grown biomass production ####
      biomass_production <- getQuery(prj, "purpose-grown biomass production")

      biomass_production %>%
        filter(year > 2010 & year < 2055) %>%
        mutate(Region = (if_else(region == "USA", "USA", "ROW"))) %>%
        left_join(scenario_mapping, by = "scenario") %>%
        group_by(Units, Scenario, Region, year) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order)) %>%
        filter(Region == "USA")-> biomass_production_final

      p <- ggplot() + geom_line(data=biomass_production_final, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        #facet_wrap(Region ~ ., scales = "free_y") +
        expand_limits(y = 0) +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Purpose-grown biomass" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
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
              legend.text = element_text(size = 20))
      #ggsave("figures/GCAM_analysis/12a.purpose_grown_biomass_USA.png", dpi=600/2, width=6000/300, height=3000/300)


      #### b. regional biomass consumption ####
      biomass_consumption <- getQuery(prj, "regional biomass consumption")

      # Get USA total
      biomass_consumption %>%
        #Filter out USA for double counting
        filter(year > 2010 & year < 2055,
               region %in% USA_states) %>%
        left_join(scenario_mapping, by = "scenario") %>%
        group_by(Units, Scenario, year) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order))  -> biomass_consumption_final_USA

      p <- ggplot() + geom_line(data=biomass_consumption_final_USA, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1.5) +
        scale_colour_manual(values = scenario_short_color) +
        #facet_wrap(Region ~ ., scales = "free_y") +
        expand_limits(y = 0) +
        theme(strip.text.x = element_text(size = 22)) +
        theme(strip.text.y = element_text(size = 22)) +
        ggtitle( "Total Biomass Consumption" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("EJ") +
        theme(panel.background = element_blank(),
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
              legend.text = element_text(size = 20))
      #ggsave("figures/GCAM_analysis/12b.biomass_consumption_USA.png", dpi=600/2, width=6000/300, height=3000/300)

      # Get by state
      biomass_consumption %>%
        filter(year > 2010 & year < 2055,
               region %in% USA_states) %>%
        left_join(scenario_mapping, by = "scenario") %>%
        group_by(Units, Scenario, region, year) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_order)) -> biomass_consumption_final

      p <- ggplot() + geom_line(data=biomass_consumption_final, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_wrap(region ~ ., scales = "free_y") +
        facet_wrap(facets = ~fct_reorder(region, sum, .desc = TRUE), scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Sulfur Dioxide Emissions in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("Tg") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/12b.biomass_consumption_states.png", dpi=600/2, width=6000/300, height=3000/300)

      #MAP
      biomass_consumption_final %>%
        filter(year == 2050) %>%
        rename(state = region) %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) -> biomass_consumption_final_MAP

        p <- plot_usmap(region = c("state"),
                        data = biomass_consumption_final_MAP,
                        values = "sum") +
        ggtitle( "Biomass Consumption in the US in 2050 (EJ)") +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        scale_fill_gradient2(low = "#08A045", mid = "#FAF0CA", high = "#007EA7", midpoint = 0) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/12b.biomass_consumption_MAP.png", dpi=600/2, width=6000/300, height=3000/300)


##### 13. Land allocation #####
      land_allocation <- getQuery(prj, "Land Allocation")

      land_allocation %>%
        filter(year > 2010 & year < 2055) %>%
        separate(`land-allocation`, c("land-allocation", "basin", "irrigation", "technology"), sep = "_") %>%
        dplyr::select(-basin, -irrigation, -technology) %>%
        left_join(scenario_mapping, by = "scenario") %>%
        left_join(agg_land_mapping, by = "land-allocation") %>%
        group_by(Units, Scenario, region, aggregated, year) %>%
        summarise(sum = sum(value)) %>%
        ungroup() %>%
        filter(!aggregated %in% c("RockIceDesert", "Tundra", "Urban land"))%>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) %>%
        mutate(`Land Type` = factor(aggregated, levels = land_alloc_order))-> land_allocation_final

      p <- ggplot() + geom_col(data=land_allocation_final, aes(x=year, y=sum, fill= aggregated)) +
        scale_fill_manual(values=land_alloc_color) +
        scale_x_continuous(breaks=seq(2010,2100,10)) +
        theme(panel.spacing = unit(2, "lines")) +
        facet_wrap(Scenario ~ ., ncol = 2, as.table = F) +
        #facet_grid(DAC ~ BECCS, switch="y") +
        ggtitle( "Land Allocation by Type in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("thous km^2") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.4, color="gray"),
              panel.grid.major.y = element_line( size=.4, color="gray"),
              plot.title = element_text(face="bold", size=40, hjust = 0.5),
              axis.title.x = element_text(size=10),
              axis.text.x  = element_text(size=10, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=15),
              axis.text.y  = element_text(size=15),
              strip.text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 10))
      #ggsave("figures/GCAM_analysis/13a.land_allocation.png", dpi=600/2, width=6000/300, height=3000/300)

      #Now get line plot
      p <- ggplot() + geom_line(data=land_allocation_final, aes(x=year, y=sum, colour = Scenario, group = Scenario), size = 1) +
        scale_colour_manual(values = scenario_short_color) +
        expand_limits(y = 0) +
        #facet_wrap(region ~ ., scales = "free_y") +
        facet_wrap(facets = ~fct_reorder(`Land Type`, sum, .desc = TRUE), scales = "free_y") +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Land Allocation by Type in the US" ) +
        theme(plot.title = element_text(face="bold", size=14, hjust = 0.5)) +
        xlab("Year") +
        ylab("thous km^2") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/13a.land_allocation_lines.png", dpi=600/2, width=6000/300, height=3000/300)

      
##### 14. CO2 prices ####
      co2_prices <- getQuery(prj, "CO2 prices")
      
      co2_prices %>%
        filter(year > 2010 & year < 2055,
               market %in% c("USACO2")) %>% #"USACO2_LUC", 
        mutate(price_2020_C = value * 1.95) %>% # multiplier from BLS https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1.00&year1=199012&year2=202012
        mutate(price_2020_CO2 = price_2020_C / emissions.CONV_C_CO2) %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order))-> co2_prices_final
      
      p <- ggplot() + geom_line(data=co2_prices_final, aes(x=year, y=price_2020_CO2, colour = Scenario, group = Scenario), size = 1) +
        geom_text(data = co2_prices_final %>% filter(year %in% c(2030, 2050)),
                  aes(x = year, y = price_2020_CO2, label = round(price_2020_CO2)),
                  hjust = -0.2, vjust = 0.2, size = 5) + 
        scale_colour_manual(values = scenario_short_color) +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "CO2 Prices" ) +
        xlab("Year") +
        ylab("2020$ / tCO2") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/14a.CO2_price.png", dpi=600/2, width=6000/300, height=3000/300)

##### 15. Carbon storage costs #####
      carbon_storage_cost <- getQuery(prj, "price of a specific market" )
      
      carbon_storage_cost %>%
        filter(year > 2010 & year < 2055) %>%
        #Origina units in 1990$/tC, convert to 2020$/tCO2
        mutate(price_2020_C = value * 1.95) %>% # multiplier from BLS https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1.00&year1=199012&year2=202012
        mutate(price_2020_CO2 = price_2020_C / emissions.CONV_C_CO2) %>%
        left_join(scenario_mapping, by = "scenario") %>%
        mutate(Scenario = factor(Scenario, levels = scenario_short_facet_order)) %>%
        dplyr::select(Scenario, year, market, price_2020_CO2) %>%
        mutate(Units = "2020$/tCO2") %>%
        group_by(Scenario, year, Units) %>%
        summarise(mean_price = mean(price_2020_CO2)) %>%
        ungroup()-> carbon_storage_cost_final
      
      p <- ggplot() + geom_line(data=carbon_storage_cost_final, aes(x=year, y=mean_price, colour = Scenario, group = Scenario), size = 1) +
        geom_text(data = carbon_storage_cost_final %>% filter(year %in% c(2030, 2050)),
                  aes(x = year, y = mean_price, label = round(mean_price)),
                  hjust = -0.2, vjust = 0.2, size = 5) + 
        scale_colour_manual(values = scenario_short_color) +
        theme(strip.text.x = element_text(size = 10)) +
        theme(strip.text.y = element_text(size = 10)) +
        ggtitle( "Mean National Carbon Storage Cost" ) +
        xlab("Year") +
        ylab("2020$ / tCO2") +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_line( size=.1, color="gray"),
              panel.grid.major.y = element_line( size=.1, color="gray"),
              plot.title = element_text(face="bold", size=16, hjust = 0.5),
              axis.title.x = element_text(size=12),
              axis.text.x  = element_text(size=12, vjust = 0.5, angle = 45),
              axis.title.y = element_text(size=12),
              axis.text.y  = element_text(size=12),
              strip.text = element_text(size=12),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.position = "bottom")
      #ggsave("figures/GCAM_analysis/15a.CO2_storage_cost.png", dpi=600/2, width=6000/300, height=3000/300)
      
      #Create table for supplementary material
      carbon_storage_cost_final %>%
        spread(year, mean_price) -> carbon_storage_cost_final_table
      
      #write_csv(carbon_storage_cost_final_table, "output_data/GCAM/carbon_storage_cost_final_table.csv")
      
##### 16. Electricity prices ####
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
        spread(Scenario, value) %>%
        rename(state = region) %>%
        mutate(percent_increase = (`Net-zero, Low BECCS, Low DAC` - `Net-zero, High BECCS, High DAC`) / `Net-zero, High BECCS, High DAC` *100)-> buildings_percent_increase_2050
        
      p <- plot_usmap(region = c("states"),
                      data = buildings_percent_increase_2050,
                      values = "percent_increase") +
        ggtitle( "Percent increase in electircity prices to buildings in 2050") +
        scale_fill_gradient2(low = "#06BA63", mid = "#FAF0CA", high = "#E63946", midpoint = 0) +
        theme(panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face="bold", size=24, hjust = 0.5),
              strip.text = element_text(size=16),
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              legend.position = "right")
      #ggsave("figures/GCAM_analysis/16.electricity_price_increase.png", dpi=600/2, width=6000/300, height=3000/300)
      #ggsave("figures/GCAM_analysis/16.electricity_price_increase.svg", dpi=600/2, width=6000/300, height=3000/300)
      
