# Data downscaling for other sectors
# This script is designed to save color schemes for GCAM data
# Date: September  2023 
# -----------------------------------------------------------------------------
# Various color schemes to try to standardize figures.

####Colors ####
region_color <- c("USA" = "#1B4079",
                  "ROW" = "#E3B505")

scenario_short_color <- c("Reference" = "#5E6973",
                          "Net-zero, High BECCS, High DAC" = "#AB2346",
                          "Net-zero, Low BECCS, Low DAC" = "#00B295",
                          "USACO2 & Net-zero, High BECCS, High DAC" = "#08A045",
                          "USACO2_LUC & Net-zero, High BECCS, High DAC" = "#AB2346",
                          "USACO2 & Net-zero, Low BECCS, Low DAC" = "#1C6E8C",
                          "USACO2_LUC & Net-zero, Low BECCS, Low DAC" = "#00B295")

CO2_sector_color <- c("HFCs & SF6" = "#FEF0C3",
                      "N2O" = "#FCEAAE",
                      "CH4" = "#FCDF86",
                      "Other energy processing" = "#FEC286",
                      "Refining" = "#FAAE61",
                      "Buildings" = "#F36D45",
                      "Industry" = "#D93127",
                      "Transport" = "#A41D2A",
                      "Electricity" = "#660915",
                      "Industrial feedstocks" = "#144630",
                      "Land sink" = "#1B9A52",
                      "Refining BECCS" = "#67BE64",
                      "DAC" = "#A6D16C",
                      "Electricity BECCS" = "#D8E58E")

primary_energy_color <- c("Solar" = "#F0F6E3",
                          "Wind" = "#CDE6C4",
                          "Hydro" = "#AAD8B5",
                          "Biomass" = "#7ACCC4",
                          "Nuclear" = "#48B3D3",
                          "Coal" = "#278DBF",
                          "Oil" = "#186FA4", 
                          "Natural Gas" = "#237EA2")

primary_energy_color_2 <- c("Solar" = "#F7C9AAFF",
                            "Wind" = "#F6A47BFF",
                            "Hydro" = "#F47C56FF",
                            "Biomass" = "#B91657FF", 
                            "Nuclear" = "#921C5BFF", 
                            "Coal" = "#6A1F56FF",
                            "Oil" = "#451C47FF",
                            "Natural Gas" = "#221331FF"#140e36
)

# difference_plot_color <- c("#F0F6E3", "#CDE6C4", "#AAD8B5", "#7ACCC4", 
#                           "#48B3D3", "#278DBF", "#186FA4", "#237EA2") #Use same color palette in difference plots of Figure 2
# 
# mortality_plot_color <- c("#FCEAAE", "#FCCC9C", "#FAAE61",
#                           "#F36D45", "#D93127", "#A41D2A", "#660915", "#144630")

pollutant_sector_color <- c("Electricity BECCS" = "#fcfdbf", 
                            "Refining bioliquids" = "#fecf92",
                            "Other energy processing" = "#fe9f6d",
                            "Refining" = "#f7705c",
                            "Urban processes" = "#de4968", 
                            "Electricity" = "#b73779", 
                            "Transportation" = "#8c2981",
                            "Buildings" = "#641a80",
                            "Industry" = "#3b0f70"#140e36
                            )

pollutant_sector_color_2 <- c("Electricity BECCS" = "#4575B4", 
                            #"Refining bioliquids" = "#",
                            "Other energy processing" = "#74ADD1",
                            "Urban processes" = "#74ADD1", 
                            "Refining" = "#ABD9E9",
                            "Electricity" = "#E0F3F8", 
                            "Transportation" = "#FFFFBF",
                            "Buildings" = "#FEE090",
                            "Industry" = "#FDAE61")

pollutant_industry_color <- c("industrial processes" = "#481579", 
                            "other industrial energy use" = "#A12F7F",
                            "cement" = "#EE5C5E")
 
CO2_sequestration_color <- c("Industry" = "#D6FAFF",
                             "Feedstocks" = "black",
                             "Electricity" = "#1C6E8C",
                             "Refining"  = "#7A126C",
                             "H2 Production"   = "#73BA9B",
                             "DAC" = "#5E6973",
                             "DAC heat" = "#B0B8BF")

CO2_sequestration_tech_color <- c("Industry" = "#D6FAFF",
                                  "Feedstocks" = "black",
                                  "Electricity BECCS" = "#1C6E8C",
                                  "Electricity CCS" = "#99D5EB",
                                  "Refining BECCS" = "#7A126C",
                                  "Refining CCS" = "#F4B8EC",
                                  "Hydrogen BECCS" = "#73BA9B",
                                  "Hydrogen CCS" = "#c9e4d8",
                                  "Cement CCS" = "#F95738",
                                  "Fertilizer CCS" = "#08A045",
                                  "DAC" = "#5E6973",
                                  "DAC heat" = "#DDE1E3")



elec_gen_color <- c("refined liquids" = "#7A126C",
                    "gas" = "#F95738",
                    "coal" = "black",
                    "biomass" = "#08A045",
                    "nuclear" = "#D6FAFF",
                    "hydro" = "#1C6E8C",
                    "wind" = "#73BA9B",
                    "solar" = "#FFCF56",
                    "rooftop_pv" = "#FAF0CA",
                    "geothermal" = "#5E6973")

final_energy_sectors_color <- c("building" = "#D52941",
                                "industry" = "#D6FAFF",
                                "transportation" = "#F6AE2D",
                                "CO2 removal" = "#5E6973",
                                "process heat dac" = "black")

final_energy_fuel_color <- c("Liquids" = "#7A126C",
                             "Gas" = "#F95738",
                             "Coal" = "black",
                             "Biomass" = "#08A045",
                             "Hydrogen" = "#73BA9B",
                             "Electricity" = "#1C6E8C")

fuel_color <- c("refined liquids" = "#7A126C",
                "gas" = "#F95738",
                "coal" = "black",
                "biomass" = "#08A045",
                "Hydrogen" = "#73BA9B",
                "Electricity" = "#1C6E8C")

GHG_gas_color <- c("HFCs" = "#950952",
                   "N2O Energy" = "#895B06",
                   "N2O Resource Production" = "#C48208",
                   "N2O Agriculture" = "#F6AE2D",
                   "N2O Unmanaged Land" = "#FADA9E",
                   "CH4 Energy" = "#005C69",
                   "CH4 Resource Production" = "#00A2B8" ,
                   "CH4 Agriculture" = "#85F1FF",
                   "CH4 Unmanaged Land" = "#D6FAFF",
                   "CO2 Energy" = "#08A045",
                   "CO2 Resource Production" = "#0BD55C",
                   "CO2 Land" = "#8BF9B7")

pollution_BC_color <- c("Buildings" = "#D52941",
                        "Industry" = "#D6FAFF",
                        "Transportation" = "#F6AE2D",
                        "Electricity" = "#1C6E8C",
                        "Refining" = "#7A126C",
                        "Agriculture" = "#73BA9B",
                        "Unmanaged Land" = "#08A045",
                        "Urban processes" ="#5E6973")

air_pollution_color <- c("BC" = "#D52941",
                         #"BC_AWB" = ,
                         "CO" = "#D6FAFF",
                         "CO_AWB" = "#E4D9FF",
                         "NH3" = "#F6AE2D",
                         #"NH3_AGR = ,
                         #"NH3_AWB" = ,
                         "NMVOC" = "#1C6E8C",
                         #"NMVOC_AGR" = ,
                         #"NMVOC_AWB" = ,
                         "NOx" = "#7A126C",
                         "NOx_AGR" = "#918EF4",
                         "NOx_AWB" = "#330F0A",
                         "OC" = "#73BA9B",
                         #"OC_AWB" = ,
                         "PM10" = "#08A045",
                         "PM2.5" ="#5E6973",
                         "SO2_1" = "#EB8258",
                         "SO2_1_AWB" = "#EFDD8D")

refining_tech_color <- c("Oil (no CCS)" = "#7A126C",
                         "Gas (no CCS)"= "#F95738",
                         "Coal (with CCS)" = "black",
                         "Biodiesel" = "#005C69",
                         "Corn Ethanol (no CCS)" = "#F4E87C",
                         "Cellulosic ethanol (with CCS)" = "#08A045",
                         "Cellulosic ethanol (no CCS)" = "#0BD55C",
                         "FT biofuel (with CCS)" = "#C48208",
                         "FT biofuel (no CCS)" = "#F6AE2D",
                         "BTL with H2 (no CCS)" = "#73BA9B")

land_alloc_color <- c("Agriculture" = "#950952",
                      "Soybean" = "#895B06",
                      "Oil Crop" = "#C48208",
                      "Sugar Crop" = "#F6AE2D",
                      "N2O Unmanaged Land" = "#FADA9E",
                      "Other arable" = "#005C69",
                      "Biomass" = "#00A2B8" ,
                      "Forest" = "#85F1FF",
                      "Fodder" = "#D6FAFF",
                      "Grassland" = "#08A045",
                      "Pasture" = "#0BD55C",
                      "Shrubland" = "#8BF9B7",
                      "Protected Land" = "#98838F")

fuel_colors <- c("Oil" = "#7A126C",
                 "Gas" = "#F95738",
                 "Coal" = "black",
                 "Biomass" = "#08A045")

elec_gen_combustion_color <- c("biomass (conv)" = "#08A045",
                               "biomass (conv CCS)" = "#45DF83",
                               "biomass (IGCC)" = "#05612A",
                               "biomass (IGCC CCS)" = "#64F79F",
                               "coal (conv pul)" = "black",
                               "coal (conv pul CCS)" = "#666666",
                               "coal (IGCC CCS)" = "#ADADAD",
                               "gas (CC)"  = "#F95738",
                               "gas (CC CCS)" = "#FA8B75",
                               "gas (steam/CT)" = "#EB2B09",
                               "refined liquids (CC)" = "#7A126C",
                               "refined liquids (CC CCS)" = "#F4B8EC",
                               "refined liquids (steam/CT)" = "#EB95E1")

GCAM_sectors_colors <- c("Commercial" = "#88D9E6",
                         "Residential" = "#C5FFFD",
                         "Industrial" = "#E34F4F",
                         "Transportation" = "#F6AE2D",
                         "Cement" = "#1C6E8C",
                         "Refining (oil)" = "#53599A",
                         "Refining (biomass)" = "#AFB2D4",
                         "Agriculture" = "#285943",
                         "Agriculture (AWB)" = "#8CD790",
                         "Unmanaged Land" = "#08A045",
                         "Urban" ="#4E0110",
                         "Resource production" = "#5E6973")

  