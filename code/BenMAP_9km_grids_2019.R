# BenMAP_9km_grids.R
# Code for re-scaling variables at 9 km grid: population and baseline incidence, and for getting mean for PM2.5
# This script is designed to re-scale demographic and socioeconomic data from census block level to 9 km grid
# It is the version where we are using the modified pollution data based on Wei et al (https://www.nature.com/articles/s41467-023-43862-3)
# Date: May 2024
# Author: Candelaria Bergero

#------------------------------------------------- LOAD LIBRARIES -------------------------------------------------
#Load all packages
library(sp)
library(rgdal)

library(raster)
library(purrr)
library(ggplot2)
library(viridis)
library(data.table)

library(sf)
library(ncdf4)
library(rgdal)
library(raster)
library(future)
library(furrr)
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggspatial)

#General files
source( "code/color_schemes.R" ) # some predefined color schemes
state_region_mapping <- read.csv("mappings/state_region_mapping.csv") %>%
  mutate( state_ID = gsub("^_", "", state_ID)) #mapping


#------------------------------------------------- BENMAP INPUTS --------------------------------------------------

#------------------------------------------------- SECTION 1: 9 KM GRID -------------------------------------------------
#Work with 9 km grid from air pollution results
#### 1. Load 9 km grid data ####
  grid_data <- st_read("shapefiles/d01_9km_projv2/D01_9km_proj_v2.shp") #Provided by Jing Cheng based on WRF-CMAQ resolution

#### 2. Prepare 9 km grids ####
  #### 2.1. Set grid numbers ####
    grid_data$pixel_ID <- seq_along(grid_data$geometry)
    grid_data <- grid_data[, c("geometry", "pixel_ID")]
  
  #### 2.2. Extract bounding box ####
    bbox <- st_bbox(grid_data$geometry)
  
  #### 2.3. Calculate the number of columns and rows ####
    num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 9000)
    num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 9000)
  
  #### 2.4. Extract information ####
  # Extract centroids from the geometry column
    centroids <- st_centroid(grid_data$geometry)
  
  # Extract x and y coordinates from the centroid geometry
    x_coords <- st_coordinates(centroids)[, "X"]
    y_coords <- st_coordinates(centroids)[, "Y"]
  
  #### 2.5. Add rows and columns ####
    grid_data$row <- num_rows - floor((y_coords - bbox["ymin"]) / 9000)
    grid_data$col <- ceiling((x_coords - bbox["xmin"]) / 9000)
  
  #### 2.6. Convert CRS ####
    grid_data_NAD83 <- st_transform(grid_data, crs = st_crs("+proj=longlat +datum=NAD83"))


#------------------------------------------------- SECTION 2: 9 KM POPULATION -------------------------------------------------
#Here we re-scale population data by age from Census bureau at the census block group level to our 9km grids. 
#We will use the same 9 km 2019 population grid for our 3 BenMAP runs.
    #We use 2019 population data because our air pollution modeling if for 2019
#### 1. Load census data ####
    census_block <- st_read("shapefiles/cb_2019_us_bg_500k_clipped/cb_2019_us_bg_500k.shp") #From https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.2019.html#list-tab-1883739534
    
    input_population_data <- read.csv("data/BenMAP/ACSDT5Y2019.B01003_2024-02-28T212808/ACSDT5Y2019.B01003-Data.csv") #From census data table B01003 on total population at the census block level. Note we rename column to match census block metadata
    
    input_race_data <- read.csv("data/BenMAP/ACSDT5Y2019.B02001_2024-02-28T213117/ACSDT5Y2019.B02001-Data.csv")  #From census data table B02001 on race at the census block level. Note we rename column to match census block metadata
    
    input_gender_age_data <- read.csv("data/BenMAP/ACSDT5Y2019.B01001_2024-02-28T213030/ACSDT5Y2019.B01001-Data.csv")  #From census data table B01001 on race at the census block level. Note we rename column to match census block metadata
    
    input_income_data <-read.csv("data/BenMAP/ACSDT5Y2019.B19013_2024-02-28T213319/ACSDT5Y2019.B19013-Data.csv")  #From census data table B19013 on race at the census block level. Note we rename column to match census block metadata
    
#### 2 Prepare census data ####
  #### 2.1. Total population ####
    input_population_data %>%
      dplyr::select(GEO_ID, NAME, B01003_001E) %>%
      dplyr::rename(census_block_ID = GEO_ID,
             population_in = B01003_001E) -> population_data
    
  #### 2.2. Race ####
    input_race_data %>%
      dplyr::select(GEO_ID, B02001_002E, B02001_003E, B02001_004E, 
                    B02001_005E, B02001_006E, B02001_007E, B02001_008E) %>%
      dplyr::rename(census_block_ID = GEO_ID,
             WHITE_in = B02001_002E,
             BLACK_in = B02001_003E,
             NATAMER_in = B02001_004E,
             ASIAN_in = B02001_005E) %>%
      #Adding up: Hawaiian native or Pacific Islander + some other race alone + two or more races
      mutate(Other_in = B02001_006E + B02001_007E + B02001_008E) %>%
      dplyr::select(-B02001_006E, -B02001_007E, -B02001_008E)-> race_data
    
  #### 2.3. Age ####
    #Note: these categories have to match our incidence rate
    input_gender_age_data %>%
      dplyr::select(GEO_ID, NAME, B01001_003E, B01001_004E, B01001_005E, B01001_006E,
                    B01001_007E, B01001_008E, B01001_009E, B01001_010E, B01001_011E,
                    B01001_012E, B01001_013E, B01001_014E, B01001_015E, B01001_016E,
                    B01001_017E, B01001_018E, B01001_019E, B01001_020E, B01001_021E,
                    B01001_022E, B01001_023E, B01001_024E, B01001_025E,
                    B01001_027E, B01001_028E, B01001_029E, B01001_030E, B01001_031E,
                    B01001_032E, B01001_033E, B01001_034E, B01001_035E, B01001_036E,
                    B01001_037E, B01001_038E, B01001_039E, B01001_040E, B01001_041E,
                    B01001_042E, B01001_043E, B01001_044E, B01001_045E, B01001_046E,
                    B01001_047E, B01001_048E, B01001_049E) %>%
      #We want to divide "under 5 years" so that I get 0, and then 1 to 5. We assume population is evenly distributed
      mutate(`0_in` = (B01001_003E + B01001_027E) / 5) %>%
      mutate(`1TO17_in` = B01001_003E + B01001_004E + B01001_005E + B01001_006E + #male
                          B01001_027E + B01001_028E + B01001_029E + B01001_030E - #female
               `0_in`, #subtract the newbor category
             `18TO24_in` = B01001_007E + B01001_008E + B01001_009E + B01001_010E + #male
                           B01001_031E + B01001_032E + B01001_033E + B01001_034E, #female
             `25TO34_in` = B01001_011E + B01001_012E + #male
                           B01001_035E + B01001_036E, #female
             `35TO44_in` = B01001_013E + B01001_014E + #male
                           B01001_037E + B01001_038E, #female
             `45TO54_in` = B01001_015E + B01001_016E + #male
                           B01001_039E + B01001_040E, #female
             `55TO64_in` = B01001_017E + B01001_018E + B01001_019E + #male
                           B01001_041E + B01001_042E + B01001_043E, #female
             `65TO74_in` = B01001_020E + B01001_021E + B01001_022E + #male
                           B01001_044E + B01001_045E + B01001_046E, #female
             `75TO84_in` = B01001_023E + B01001_024E + #male
                           B01001_047E + B01001_048E, #female
             `85TO99_in` = B01001_025E + #male
                           B01001_049E #female
            ) %>%
      dplyr::select(GEO_ID, `0_in`, `1TO17_in`, `18TO24_in`, `25TO34_in`, `35TO44_in`, `45TO54_in`,
             `55TO64_in`, `65TO74_in`, `75TO84_in`, `85TO99_in`) %>%
      dplyr::rename(census_block_ID = GEO_ID) -> age_data
    
  #### 2.4. Income ####
    input_income_data %>% #In 2020 inflation-adjusted USD
      dplyr::select(GEO_ID, B19013_001E) %>%
      dplyr::rename(census_block_ID = GEO_ID,
             median_house_income = B19013_001E) %>%
      mutate(median_house_income = if_else(median_house_income == "-", "", median_house_income)) %>% #Some missing values for income, NAs expected
      mutate(median_house_income = as.numeric(median_house_income))-> income_data

#### 3. Census block data ####
    #Keep relevant columns
    census_block <- census_block[, c("AFFGEOID", "geometry", "STATEFP")]
    
    #Calculate census block area
    census_block %>%
      dplyr::mutate(block_area = round(st_area(census_block), digits = 5), #Area in m^2
             block_area = as.numeric(gsub("\\[m²\\]", "", block_area))) %>%
      dplyr::rename(census_block_ID = AFFGEOID,
             state_ID = STATEFP) -> census_block
    
    #Calculate pixel area
    grid_data_NAD83 %>% 
      mutate(pixel_area = round(st_area(grid_data_NAD83), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_NAD83_area
    
#### 4. Join our shapefiles (census data + 9km grid) ####
    # Here we join our grids to our census blocks to get number of partitions 
    #A partition is the piece that remains after joining the 9km grid to the census block
    #join pixels & blocks to create a list of all partitions
    system.time({joined_data_original <- st_join(grid_data_NAD83_area, census_block, join = st_intersects)}) #30 seconds
    
      # Remove NAs in the census_block_ID column, keep NAs in income
      joined_data_original <- joined_data_original[complete.cases(joined_data_original$census_block_ID), , drop = FALSE]#From 661,940 to 516,028. Note we do not care to keep intersections where no people live, meaning middle of the ocean.
 
#### 5. Create the function for calculating intersecting area ####
    # Function to calculate intersection area between pixels and blocks
    calculate_intersection_area <- function(pixel_ID, census_block_ID) {
      pixel_geometry <- st_geometry(grid_data_NAD83_area[grid_data_NAD83_area$pixel_ID == pixel_ID, ])
      block_geometry <- st_geometry(census_block[census_block$census_block_ID == census_block_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
      return(intersection_area)
    }

#### 6. Run the function ####
  #### 6.1. Run ####
    #partitions <- joined_data_original[1:5000, ] #this is for testing
    partitions <- joined_data_original
    
      timing_result <- system.time({
        partitions$intersection_area <- mapply(calculate_intersection_area, 
                                              pixel_ID = partitions$pixel_ID, 
                                              census_block_ID = partitions$census_block_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #8895.7 = ~2.47 hours
    
  #### 6.2. Save partitions ####
    #Write as shapefile
    st_write(partitions, "output_data/shapefiles/9km/9km_partitions_shapefile_2019_03152024.shp")
    
    #Write as csv
    write.csv(partitions, row.names = FALSE, "output_data/BenMAP_files/9km/9km_partitions_2019.csv")
    partitions %>%
       st_drop_geometry() -> partitions_clean
    write.csv(partitions_clean, row.names = FALSE, "output_data/BenMAP_files/9km/9km_partitions_nogeometry_2019.csv")
    #NOTE: If we just need the intersection area and the ratio we can load this csv
    
#### 7. Outputs ####
  #### 7.1 Get area ratios ####
    #This is how much of the census block is in the pixel, group by census block should equal 1
    partitions$area_ratio <- partitions$intersection_area / partitions$block_area
    
    #This is how much of the pixel is in each census block, group by pixel should equal 1
    partitions$area_ratio_pixel <- partitions$intersection_area / partitions$pixel_area
        #small number means the pixel has a lot of census blocks 
        #1 means the pixel is fully contained within a census bloc
    #NOTE: pixels that are on the continental coast are larger than continental border, which affects later weighted averages
    #These happens to 4043 pixels
    #To fix this we set the pixel area for these pixels = the sum of the intersection area with the census blocks contained in that pixel, since the rest is ocean.
    partitions %>%
      group_by(pixel_ID) %>%
      summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
      ungroup() %>%
      st_drop_geometry()-> partitions_area_pixel
    
    partitions_area_pixel_border <- partitions_area_pixel$pixel_ID[partitions_area_pixel$sum_area_ratio_pixel < 0.9999] #4043
    
    #Now we change the pixel_area for these 4043 pixels to be their intersection area with the census block, given that the rest is ocean
    for (i in seq_along(partitions$pixel_ID)) {
      current_pixel_ID <- partitions$pixel_ID[i]
      
      # Check if the current pixel_ID is in partitions_area_pixel_border
      if (current_pixel_ID %in% partitions_area_pixel_border) {
        
        # Filter partitions to get the rows matching the current pixel_ID
        matching_rows <- partitions[partitions$pixel_ID == current_pixel_ID, ]
        
        # Recalculate pixel_area as the sum of intersection_area for the matching rows
        new_pixel_area <- sum(matching_rows$intersection_area)
        
        # Update the pixel_area in the 'partitions' table
        partitions$pixel_area[i] <- new_pixel_area
    
      } 
    }
   
    #Now write the fixed partitions table 
    st_write(partitions, "output_data/shapefiles/9km/9km_partitions_shapefile_2019_03152024_fixed.shp")
  
  #### 7.2. Merge variables ####
    census_block %>%
      mutate(state_ID = as.numeric(state_ID)) %>%
      left_join(population_data, by = "census_block_ID") %>% #total population
      filter(!state_ID %in% c(66, 69)) %>% #filter out Guam (66) and Northern Mariana Islands (69)
      left_join(race_data, by = c("census_block_ID")) %>% #race
      left_join(income_data, by = c("census_block_ID")) %>% #income
      left_join(age_data, by = c("census_block_ID")) -> census_block_variables_geometry
    
    census_block_variables_geometry %>%
      st_drop_geometry() -> census_block_variables
    
    #Merge partitions with variables
    partitions %>%
      mutate(state_ID = as.numeric(state_ID)) %>%
      select(-block_area) %>%
      left_join(census_block_variables, by = c("census_block_ID", "state_ID")) -> partitions_complete

  #### 7.3. Calculate population totals per partition ####
    partitions_complete %>%
      mutate(Population = population_in * area_ratio,
             WHITE = WHITE_in * area_ratio,
             BLACK = BLACK_in * area_ratio,
             NATAMER = NATAMER_in * area_ratio,
             ASIAN = ASIAN_in * area_ratio,
             Other = Other_in * area_ratio,
             `0TO0` = `0_in` * area_ratio,
             `1TO17` = `1TO17_in` * area_ratio,
             `18TO24` = `18TO24_in` * area_ratio,
             `25TO34` = `25TO34_in` * area_ratio,
             `35TO44` = `35TO44_in` * area_ratio,
             `45TO54` = `45TO54_in` * area_ratio,
             `55TO64` = `55TO64_in` * area_ratio,
             `65TO74` = `65TO74_in` * area_ratio,
             `75TO84` = `75TO84_in` * area_ratio,
             `85TO99` = `85TO99_in` * area_ratio) %>%
      dplyr::rename(Row = row,
             Column = col) %>%
      dplyr::select(pixel_ID, Row, Column, Population, 
             WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, `18TO24`, `25TO34`,
             `35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data
    
  #### 7.4. Calculate population totals per pixel ####
    population_2019_data %>%
      group_by(pixel_ID, Row, Column) %>%
      dplyr::summarise(Population = sum(Population),
                WHITE = sum(WHITE), 
                BLACK = sum(BLACK), 
                NATAMER = sum(NATAMER), 
                ASIAN = sum(ASIAN), 
                Other = sum(Other), 
                `0TO0` = sum(`0TO0`),
                `1TO17` = sum(`1TO17`), 
                `18TO24` = sum(`18TO24`), 
                `25TO34` = sum(`25TO34`),
                `35TO44` = sum(`35TO44`),
                `45TO54` = sum(`45TO54`),
                `55TO64` = sum(`55TO64`),
                `65TO74` = sum(`65TO74`),
                `75TO84` = sum(`75TO84`),
                `85TO99` = sum(`85TO99`)) %>%
      ungroup()-> population_2019_data_pixel
    
    #Create filter with pixels that end up with population
    pixels_to_keep <- population_2019_data_pixel$pixel_ID 
    
  #### 7.5. Population by age ####
    population_2019_data_pixel %>%
      st_drop_geometry() %>%
      dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
             `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
      gather(AgeRange, Population, -Row, -Column) %>%
      mutate(Year = 2019,
             Race = "ALL",
             Ethnicity = "ALL",
             Gender = "ALL")-> population_2019_data_pixel_age_final
      
    # Print the output table
    st_write(population_2019_data_pixel, "output_data/shapefiles/9km/9_km_grid_NAD83_filtered_population_2019.shp") #File with geometry, row, col, pixel ID, ages and races
    
    write.csv(population_2019_data_pixel_age_final, row.names = FALSE, "output_data/BenMAP_files/9km/9_km_grid_NAD83_filtered_population_2019.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
    
    #Print the shapefile without unused pixels
    grid_data_NAD83 %>%
      filter(pixel_ID %in% pixels_to_keep) -> grid_data_NAD83_filtered
    st_write(grid_data_NAD83_filtered, "output_data/shapefiles/9km/9_km_grid_NAD83_filtered_2019.shp")
    
  #### 7.6. Figures ####
    #Figure
    population_2019_data_pixel %>%
      mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_log
    
    #Plot 9 km grid data
    p<- ggplot() +
      geom_sf(data = population_2019_data_pixel_log, aes(fill = log_pixel_population), color = NA, size = 0) +
      #geom_sf(data = population_2019_data_pixel, aes(fill = Population), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("United States 9km Rescaled Population")
      #ggsave("figures/BenMAP/9km/population/9km_rescaled_population_USA_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/population/9km_rescaled_population_USA_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
    
    #Plot census block data
    census_block_variables_geometry %>%
      mutate(log_population = log(population_in + 1))-> census_block_filtered

    p<- ggplot() +
      geom_sf(data = census_block_filtered, aes(fill = log_population), color = NA, size = 0) +
      #geom_sf(data = census_block_filtered, aes(fill = population_in), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("United States Census Block Population")
    #ggsave("figures/BenMAP/9km/population/census_block_USA_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
  print(p)

#### 8. Checks ####
  #### 8.1. Total population ####
    #Input
    check_national_population_in <- sum(census_block_filtered$population_in) #322,538,633 people
    
    #Output (Partitions)
    check_national_population_out <- round(sum(population_2019_data_pixel$Population)) #322,538,633
    
  #### 8.2. State population ####
    #Input data state
    check_state_population_in <- census_block_variables %>%
      na.omit(census_block_variables) %>%
      group_by(state_ID) %>%
      summarise(state_population = sum(population_in)) %>%
      ungroup() %>%
      mutate(data = "input")
    
    # Output (partitions)    
    check_state_population_out <- population_2019_data_pixel %>%
      group_by(state_ID) %>%
      summarise(state_population = sum(Population)) %>%
      ungroup()  %>%
      mutate(data = "output")
    
    #Merge
    check_state_population <- bind_rows(check_state_population_in, check_state_population_out)
    
  #### 8.3. Area ratio sum ####
    partitions_complete %>%
      group_by(census_block_ID) %>%
      summarise(sum_area_ratio = sum(area_ratio)) -> check_census_ratio #should = 1
    
#------------------------------------------------- SECTION 3: 9 KM INCIDENCE RATE -------------------------------------------------
#### 1. Incidence rates ####
  #### 1.1. Load files ####
    #BenMAP census file does not match the Census counties, which causes issues with our rows and columns designation. In order for our analysis to have the same map as a baseline, we use census bureau county shapefiles for teh geometry, but BenMAP for the actual incidence
    #from census
    grid_data_county_census <- st_read("shapefiles/cb_2019_us_county_500k/cb_2019_us_county_500k.shp") %>% 
      filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78" )) %>% #filter out states we don't need
      dplyr::select(STATEFP, NAME, COUNTYFP, GEOID, geometry) #keep relevant columns
    
    #fromm BenMAP
    grid_data_county_benmap <- st_read("data/BenMAP/incidence_rates_BenMAP/County.shp") #From BenMAP
    
    incidence_2015 <- read.csv("data/BenMAP/incidence_rates_BenMAP/Mortality Incidence (2015).csv") #From BenMAP
    incidence_2020 <- read.csv("data/BenMAP/incidence_rates_BenMAP/Mortality Incidence (2020).csv") #From BenMAP
    
    grid_data_county_benmap %>%
      mutate(FIPS = if_else(FIPS == "46113", "46102", FIPS)) %>% #County Oglala Lakota was previously known as Shannon, so we rename it before the join
      #This FIPS is for Bedford city in Virginia, which merged to Bedford county FIPS 51019 in July 2013. We remove it from our analysis because it is not in our 2019 census data, and its geometry is included in FIPS 51019
      filter(FIPS != "51515") %>%
      st_drop_geometry()-> grid_data_county_benmap_filtered
    
    grid_data_county_census %>%
      left_join (grid_data_county_benmap_filtered, by = c("GEOID" = "FIPS", "STATEFP" = "STATE_FIPS")) %>%
      dplyr::rename(county_name = NAME.x,
             county_ID = GEOID,
             benmap_ROW = ROW,
             benmap_COL = COL) %>%
      dplyr::select(-NAME.y, -CNTY_FIPS, -COUNTYFP)-> grid_data_county
    
    #So now grid_data_county has each county in the US with the geometry from the census data 2020.
    #We will bring in incidence later
    
  #### 1.2. Process files ####
    #Calculate county area
    grid_data_county %>%
      mutate(county_area = round(st_area(grid_data_county), digits = 5), #Area in m^2
             county_area = as.numeric(gsub("\\[m²\\]", "", county_area)))-> grid_data_county
    #Calculated pixel area ->  we use grid_data_NAD83_area
    
#### 2. Join files ####
    system.time({joined_data_original_mortality <- st_join(grid_data_NAD83_area, grid_data_county, join = st_intersects)}) #8 seconds
    
    # Remove NAs in the county ID column
    joined_data_original_mortality <- joined_data_original_mortality[complete.cases(joined_data_original_mortality$county_ID), , drop = FALSE]#From 287,038 to 141,126 Note we do not care to keep intersections where no county exists, meaning middle of the ocean.
    
#### 3. Area of intersection ####
  #### 3.1. create function ####
    # Function to calculate intersection area between pixels and blocks
    calculate_intersection_area <- function(pixel_ID, county_ID) {
      pixel_geometry <- st_geometry(grid_data_NAD83_area[grid_data_NAD83_area$pixel_ID == pixel_ID, ])
      county_geometry <- st_geometry(grid_data_county[grid_data_county$county_ID == county_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
      return(intersection_area)
    }
    
  #### 3.2. Run the function #### 
    #partitions_mortality <- joined_data_original_mortality[1:1000, ]
    partitions_mortality <- joined_data_original_mortality
    
    timing_result <- system.time({
      partitions_mortality$intersection_area <- mapply(calculate_intersection_area, 
                                                       pixel_ID = partitions_mortality$pixel_ID, 
                                                       county_ID = partitions_mortality$county_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #2047 = ~34 minutes
    
  #### 3.3. Calculate ratios ####  
    #This is how much of the county is in the pixel, group by county should equal 1
    partitions_mortality$area_ratio <- partitions_mortality$intersection_area / partitions_mortality$county_area 
    
    #This is how much of the pixel is in each county, group by pixel should equal 1
    partitions_mortality$area_ratio_pixel <- partitions_mortality$intersection_area / partitions_mortality$pixel_area
    
  #### 3.4. Fix border pixels ####
    #NOTE: pixels that are on the continental coast are larger than continental border, which affects later weighted averages
    #These happens to 4043 pixels
    #To fix this we set the pixel area for these pixels = the sum of the intersection area with the census blocks contained in that pixel
    partitions_mortality %>%
      group_by(pixel_ID) %>%
      summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
      ungroup() %>%
      st_drop_geometry()-> partitions_mortality_area_pixel
    
    partitions_mortality_area_pixel_border <- partitions_mortality_area_pixel$pixel_ID[partitions_mortality_area_pixel$sum_area_ratio_pixel < 0.9999] #4043
    
    #Now we change the pixel_area for these 4043 pixels
    for (i in seq_along(partitions_mortality$pixel_ID)) {
      current_pixel_ID <- partitions_mortality$pixel_ID[i]
      
      # Check if the current pixel_ID is in partitions_mortality_area_pixel_border
      if (current_pixel_ID %in% partitions_mortality_area_pixel_border) {
        
        # Filter partitions_mortality to get the rows matching the current pixel_ID
        matching_rows <- partitions_mortality[partitions_mortality$pixel_ID == current_pixel_ID, ]
        
        # Recalculate pixel_area as the sum of intersection_area for the matching rows
        new_pixel_area <- sum(matching_rows$intersection_area)
        
        # Update the pixel_area in the 'partitions_mortality' table
        partitions_mortality$pixel_area[i] <- new_pixel_area
        
      } 
    }
  
  #### 3.5. Recalculate area pixel ratio ####
    #This is how much of the pixel is in each county, group by pixel should equal 1
    partitions_mortality$area_ratio_pixel <- partitions_mortality$intersection_area / partitions_mortality$pixel_area
    
#### 4. Bring in the incidence rates for each row and column ####
    #First analyze 2015 vs 2020 incidence rates, since we need to interpolate for 2019
    incidence_2020 %>%
      dplyr::rename(Value_2020 = Value) %>%
      left_join(incidence_2015, by = c("Endpoint.Group", "Endpoint", "Race", "Gender", "Ethnicity", "Start.Age", "End.Age",
                                       "Type", "Column", "Row")) %>%
      dplyr::rename(Value_2015 = Value) %>%
      filter(Endpoint == "Mortality, All Cause") %>%
      mutate(age = as.character(End.Age))-> incidence_rates_2015_2020
    
    p <- ggplot(incidence_rates_2015_2020, aes(x = Value_2015, y = Value_2020, color = age)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Add 1-to-1 line
      labs(title = "Scatter Plot of Incidence Rates (2015 vs 2020)",
           x = "Value_2015",
           y = "Value_2020") + 
      facet_wrap(~ age, scales = "free")
    #ggsave("figures/BenMAP/9km/incidence/incidence_rates_2015vs2020.png", p, width = 16, height = 12, units = "in", dpi = 600)
    (p)
    
    #Now linearly interpolate 2015 to 2020
    incidence_rates_2015_2020 %>% 
      mutate(Value_2019 = Value_2020 + (Value_2015 - Value_2020) / 5) %>%
      dplyr::select(-Value_2020, -Value_2015)-> incidence_2019
    
    partitions_mortality %>%
      #benmap_ROW and benmap_COL are from BenMAP county,
      #Row and Column are from BenMAP incidence rates, 
      #row and col are from my grid.
      #Value is the incidence rate in the county
      left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
                relationship = "many-to-many") %>%#We expect a many-to-many relationship because there are 11 endpoints and 10 age buckets
      dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate
    
#### 5. Weighted mortality rate ####
    partitions_rate %>%
      group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
      #Here we want to get the weighted incidence value between counties that intersect with the pixel, but based on the pixel's area, not on the county, so we use area_ratio_pixel, which is how much of the pixel intersects with the county
      mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
      ungroup() %>%
      #Now we select the columns we want to keep
      dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
             Start.Age, End.Age, Type, weighted_incidence) %>%
      #And we get rid of duplicate rows. This happens when a pixel is shared by several counties. 
      #We used this information to calculate the weighted incidence rates between different counties in the same pixel.
      #Now we can get rid of these duplicate rows
      unique()-> partitions_rate_final
    
#### 6. Print output table####
    #csv
    partitions_rate_final %>%
      dplyr::select(-pixel_ID) %>%
      dplyr::rename(Row = row, Column = col, Value = weighted_incidence) %>%
      st_drop_geometry()-> partitions_rate_final_clean
    
    write.csv(partitions_rate_final_clean, row.names = FALSE, "output_data/BenMAP_files/9km/Incidence_9km_2019.csv")
  
    #shapefiles
    partitions_rate_final %>%
      dplyr::select(-pixel_ID) %>%
      dplyr::rename(Row = row, Column = col, Value = weighted_incidence) %>%
      filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry
    
    st_write(partitions_rate_final_clean_allcauses_geometry, "output_data/shapefiles/9km/9_km_grid_NAD83_filtered_mortality_geometry_2019.shp")
    
#------------------------------------------------- SECTION 4: 9 KM POLLUTION -------------------------------------------------
#Here we prepare pollution files to csv for BenMAP. We need to get the mean of 365 days, and we need to assign exact row and columns to our data. We will have one file per period and per scenario (4 total: 2019, 2050 REF, 2050 HIH, 2050 LOW) 
#WRF-CMAQ files form Jing for daily average PM2.5
    us_states <- st_read("shapefiles/cb_2019_us_state_500k/cb_2019_us_state_500k.shp")
    usa_boundary <- st_union(us_states) #Get outter borders
    
#### 1. 2019 (9km resolution) ####
  #### 1.1. Get mean value ####
      #Read in netcdf file with daily-average PM2.5 concentrations (ug/m3) for 2019
      #365 days, 399 rows, 612 columns, pixel size is 9000m2
      # Read the NetCDF file using stack
      nc_9km_stack_2019 <- stack("data/WRF-CMAQ/9km_V2/Dailymean_9km_BASE_2019_PM25.nc", varname = "PM25_AVG")
      # Set the CRS explicitly based on the information you obtained from gdalinfo
      crs(nc_9km_stack_2019)
      
      # Calculate the mean across all bands (365 days) for each cell
      mean_raster_2019 <- calc(nc_9km_stack_2019, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      
      # Plot the mean raster
      plot(mean_raster_2019, col = rev(magma(20)), main = "2019 Yearly Mean PM2.5 Concentrations (ug/m3)")
      
  #### 1.2. Get rows and columns matching####
      #1. make sure geometries align
        # 1. Check the CRS
        st_crs(grid_data)
        crs(mean_raster_2019)
        
        #2. Print extent information
        print(extent(mean_raster_2019))
        print(st_bbox(grid_data))
        
        #3. Check resolution
        res(mean_raster_2019)
        # Get the bounding box of the grid_data
        bbox <- st_bbox(grid_data)
        # Calculate the resolution
        resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                        (bbox$ymax - bbox$ymin) / num_rows)
        # Print the resolution
        print(resolution)
      
      # 2. Set the CRS for mean_raster_2019
      crs(mean_raster_2019) <- st_crs(grid_data)$proj4string
      
      # 3. Set the extent of mean_raster_aligned to match grid_data
      extent(mean_raster_2019) <- extent(grid_data)
      
  #### 1.3. Get values from raster and assign them to my grid ####
      # 1. Extract values using st_extract
      system.time({values <- raster::extract(mean_raster_2019, grid_data)}) #1728 s = ~28.8 min
      grid_data_PM25 <- grid_data
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25$PM25_AVG <- values
      grid_data_PM25$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25$PM25_AVG))
  
  #### 1.4. Prepare output table#### 
      # 1. Convert to NAD83
      grid_data_PM25_NAD <- st_transform(grid_data_PM25, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_NAD %>%
        filter(pixel_ID %in% pixels_to_keep) -> grid_data_PM25_NAD_filtered
      
      #Get right columns 
      grid_data_PM25_NAD_filtered %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        dplyr::rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        filter(!is.na(Values)) %>% #filter out bordering pixels
        dplyr::mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_NAD_filtered_final
        
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_NAD_filtered_final, row.names = FALSE, "output_data/BenMAP_files/9km/OutputTable_PM25_2019.csv")
      
      #Shapefile
      st_write(grid_data_PM25_NAD_filtered, "output_data/shapefiles/9km/9_km_grid_NAD83_filtered_PM25_2019.shp")
      
  #### 1.5. Prepare figure #### 
      #Get national mean (population weighted mean)
      population_2019_data_pixel %>%
        dplyr::select(Row, Column, Population) %>%
        st_drop_geometry() -> population_2019_data_pixel_clean
      
      grid_data_PM25_NAD_filtered_final %>%
        left_join(population_2019_data_pixel_clean, by = c("Row", "Column")) -> grid_data_PM25_NAD_filtered_final_pop
      
      grid_data_PM25_NAD_filtered_final_pop$product <- grid_data_PM25_NAD_filtered_final_pop$Values * grid_data_PM25_NAD_filtered_final_pop$Population
      sum_products <- sum(grid_data_PM25_NAD_filtered_final_pop$product)
      total_population <- sum(grid_data_PM25_NAD_filtered_final_pop$Population)
      mean_2019 <- sum_products / total_population #Population weighted mean PM2.5
      
      grid_data_PM25_NAD_filtered_final %>%
        summarise(mean_PM25 = mean(Values)) -> mean_BASE_2019
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_NAD_filtered %>%
        filter(!is.na(PM25_AVG)) %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_NAD_filtered_graph
      
      #Perform intersection to crop pixels within the boundary
      grid_data_PM25_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_filtered_graph, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 9)) +  # Using the magma color scale
        ggtitle("2019 US Mean 9km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(mean_2019, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        #annotation_scale() +
        theme_void() +
        geom_sf(data = usa_boundary, color = "black", fill = NA)
      #ggsave("figures/BenMAP/9km/pollution/9km_PM25_2019_plot.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/pollution/9km_PM25_2019_plot.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
#### 2. 2050 Reference (9km resolution) ####
  #### 2.1. Get mean value ####
      #Read in netcdf file with daily-average annual PM2.5 concentrations (ug/m3) for 2Reference 2050
      #365 days, 399 rows, 612 columns, pixel size is 9000m2
      # Read the NetCDF file using stack
      nc_9km_stack_2050_REF <- stack("data/WRF-CMAQ/9km_V2/Dailymean_9km_Reference_2050_PM25.nc", varname = "PM25_AVG")
      # Set the CRS explicitly based on the information you obtained from gdalinfo
      crs(nc_9km_stack_2050_REF)
      
      # Calculate the mean across all bands (365 days) for each cell
      mean_raster_2050_REF <- calc(nc_9km_stack_2050_REF, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      
      # Plot the mean raster
      plot(mean_raster_2050_REF, col = rev(magma(20)), main = "2050 Yearly Mean PM2.5 Concentrations (ug/m3) in Reference case")
      
  #### 2.2. Get rows and columns matching####
      #1. make sure geometries align
      # 1. Check the CRS
      st_crs(grid_data)
      crs(mean_raster_2050_REF)
      
      #2. Print extent information
      print(extent(mean_raster_2050_REF))
      print(st_bbox(grid_data))
      
      #3. Check resolution
      res(mean_raster_2050_REF)
      # Get the bounding box of the grid_data
      bbox <- st_bbox(grid_data)
      # Calculate the resolution
      resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                      (bbox$ymax - bbox$ymin) / num_rows)
      # Print the resolution
      print(resolution)
      
      # 2. Set the CRS for mean_raster_2050_REF
      crs(mean_raster_2050_REF) <- st_crs(grid_data)$proj4string
      
      # 3. Set the extent of mean_raster_aligned to match grid_data
      extent(mean_raster_2050_REF) <- extent(grid_data)
      
  #### 2.3. Get values from raster and assign them to my grid ####
      # 1. Extract values using st_extract
      system.time({values <- raster::extract(mean_raster_2050_REF, grid_data)}) #1817 s = ~30 min
      grid_data_PM25_REF_2050 <- grid_data
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_REF_2050$PM25_AVG <- values
      grid_data_PM25_REF_2050$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_REF_2050$PM25_AVG))
      
  #### 2.4. Prepare output table #### 
      # 1. Convert to NAD83
      grid_data_PM25_NAD_REF_2050 <- st_transform(grid_data_PM25_REF_2050, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_NAD_REF_2050 %>%
        filter(pixel_ID %in% pixels_to_keep) -> grid_data_PM25_NAD_REF_2050_filtered
      
      #Get right columns 
      grid_data_PM25_NAD_REF_2050_filtered %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        dplyr::rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        filter(!is.na(Values)) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_NAD_REF_2050_filtered_final
      
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_NAD_REF_2050_filtered_final, row.names = FALSE, "output_data/BenMAP_files/9km/OutputTable_PM25_2050_REF.csv")
      
      #Shapefile
      st_write(grid_data_PM25_NAD_REF_2050_filtered, "output_data/shapefiles/9km/9_km_grid_NAD83_filtered_PM25_2050_REF.shp")
      
  #### 2.5. Prepare figure #### 
      #Get national mean (population weighted)
      grid_data_PM25_NAD_REF_2050_filtered_final %>%
        left_join(population_2019_data_pixel_clean, by = c("Row", "Column")) -> grid_data_PM25_NAD_REF_2050_filtered_final_pop
      
      grid_data_PM25_NAD_REF_2050_filtered_final_pop$product <- grid_data_PM25_NAD_REF_2050_filtered_final_pop$Values * grid_data_PM25_NAD_REF_2050_filtered_final_pop$Population
      sum_products <- sum(grid_data_PM25_NAD_REF_2050_filtered_final_pop$product)
      total_population <- sum(grid_data_PM25_NAD_REF_2050_filtered_final_pop$Population)
      mean_2050_REF <- sum_products / total_population #Population weighted mean PM2.5
      
      #Non-population weighted mean
      grid_data_PM25_NAD_REF_2050_filtered_final %>%
        summarise(mean_PM25 = mean(Values)) -> mean_REF_2050
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_NAD_REF_2050_filtered %>%
        filter(!is.na(PM25_AVG)) %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_NAD_REF_2050_filtered_graph
      
      #Count pixels with 9 or higher ug/m3 concentrations
      highpol_pixels_REF_2050 <- sum(grid_data_PM25_NAD_REF_2050_filtered_graph$PM25_AVG_graph >= 9)
      highpol_pixels_REF_2050_list <- grid_data_PM25_NAD_REF_2050_filtered_graph$pixel_ID[grid_data_PM25_NAD_REF_2050_filtered_graph$PM25_AVG_graph >= 9]
      
      partitions %>%
        st_drop_geometry() %>%
        filter(pixel_ID %in% highpol_pixels_REF_2050_list) %>%
        dplyr::select(pixel_ID, pixel_area) %>%
        unique() %>%
        summarise(total_area = sum(pixel_area)) %>%
        #convert area from m2 to km2
        mutate(total_area_km2 = total_area / 10^6)-> area_above9_REF_2050
      
      #Perform intersection to crop pixels within the boundary
      grid_data_PM25_NAD_REF_2050_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_REF_2050_filtered_graph, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_NAD_REF_2050_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 9)) +  # Using the magma color scale
        ggtitle("2050 REF US Mean 9km Yearly PM2.5 (ug/m3)")+
        geom_text(aes(label = paste("Population weighted mean:", round(mean_2050_REF, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
        annotation_scale() +
        theme_void() +
        geom_sf(data = usa_boundary, color = "black", fill = NA)
      #ggsave("figures/BenMAP/9km/pollution/9km_PM25_2050_REF_plot.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/pollution/9km_PM25_2050_REF_plot.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      
#### 3. 2050 High CDR (9km resolution) ####
  #### 3.1. Get mean value ####
      #Read in netcdf file with daily-average annual PM2.5 concentrations (ug/m3) for 2Reference 2050
      #365 days, 399 rows, 612 columns, pixel size is 9000m2
      # Read the NetCDF file using stack
      nc_9km_stack_2050_HIGH <- stack("data/WRF-CMAQ/9km_V2/Dailymean_9km_HighCDR_2050_PM25.nc", varname = "PM25_AVG")
      # Set the CRS explicitly based on the information you obtained from gdalinfo
      crs(nc_9km_stack_2050_HIGH)
      
      # Calculate the mean across all bands (365 days) for each cell
      mean_raster_2050_HIGH <- calc(nc_9km_stack_2050_HIGH, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      
      # Plot the mean raster
      plot(mean_raster_2050_HIGH, col = rev(magma(20)), main = "2050 Yearly Mean PM2.5 Concentrations (ug/m3) in HIGH CDR case")
      
  #### 3.2. Get rows and columns matching ####
      #1. make sure geometries align
      # 1. Check the CRS
      st_crs(grid_data)
      crs(mean_raster_2050_HIGH)
      
      #2. Print extent information
      print(extent(mean_raster_2050_HIGH))
      print(st_bbox(grid_data))
      
      #3. Check resolution
      res(mean_raster_2050_HIGH)
      # Get the bounding box of the grid_data
      bbox <- st_bbox(grid_data)
      # Calculate the resolution
      resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                      (bbox$ymax - bbox$ymin) / num_rows)
      # Print the resolution
      print(resolution)
      
      # 2. Set the CRS for mean_raster_2050_HIGH
      crs(mean_raster_2050_HIGH) <- st_crs(grid_data)$proj4string
      
      # 3. Set the extent of mean_raster_aligned to match grid_data
      extent(mean_raster_2050_HIGH) <- extent(grid_data)
      
  #### 3.3. Get values from raster and assign them to my grid ####
      # 1. Extract values using st_extract
      system.time({values <- raster::extract(mean_raster_2050_HIGH, grid_data)}) #1728 s = ~28.8 min
      grid_data_PM25_HIGH_2050 <- grid_data
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_HIGH_2050$PM25_AVG <- values
      grid_data_PM25_HIGH_2050$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_HIGH_2050$PM25_AVG))
      
  #### 3.4. Prepare output table #### 
      # 1. Convert to NAD83
      grid_data_PM25_NAD_HIGH_2050 <- st_transform(grid_data_PM25_HIGH_2050, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_NAD_HIGH_2050 %>%
        filter(pixel_ID %in% pixels_to_keep) -> grid_data_PM25_NAD_HIGH_2050_filtered
      
      #Get right columns 
      grid_data_PM25_NAD_HIGH_2050_filtered %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        dplyr::rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        filter(!is.na(Values)) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_NAD_HIGH_2050_filtered_final

      
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_NAD_HIGH_2050_filtered_final, row.names = FALSE, "output_data/BenMAP_files/9km/OutputTable_PM25_2050_HIGH.csv")
      
      #Shapefile
      st_write(grid_data_PM25_NAD_HIGH_2050_filtered, "output_data/shapefiles/9km/9_km_grid_NAD83_filtered_PM25_2050_HIGH.shp")
  
  #### 3.5. Prepare figure #### 
      #Get national mean (population weighted)
      grid_data_PM25_NAD_HIGH_2050_filtered_final %>%
        left_join(population_2019_data_pixel_clean, by = c("Row", "Column")) -> grid_data_PM25_NAD_HIGH_2050_filtered_final_pop
      
      grid_data_PM25_NAD_HIGH_2050_filtered_final_pop$product <- grid_data_PM25_NAD_HIGH_2050_filtered_final_pop$Values * grid_data_PM25_NAD_HIGH_2050_filtered_final_pop$Population
      sum_products <- sum(grid_data_PM25_NAD_HIGH_2050_filtered_final_pop$product)
      total_population <- sum(grid_data_PM25_NAD_HIGH_2050_filtered_final_pop$Population)
      mean_2050_HIGH <- sum_products / total_population #Population weighted mean PM2.5
      
      grid_data_PM25_NAD_HIGH_2050_filtered_final %>%
        summarise(mean_PM25 = mean(Values)) -> mean_HIGH_2050 #national mean (not pop weighted)
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_NAD_HIGH_2050_filtered %>%
        filter(!is.na(PM25_AVG)) %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_NAD_HIGH_2050_filtered_graph
      
      #Get number of pixels with high pollution
      highpol_pixels_HIGH_2050 <- sum(grid_data_PM25_NAD_HIGH_2050_filtered_graph$PM25_AVG_graph >= 9)
      highpol_pixels_HIGH_2050_list <- grid_data_PM25_NAD_HIGH_2050_filtered_graph$pixel_ID[grid_data_PM25_NAD_HIGH_2050_filtered_graph$PM25_AVG_graph >= 9]
      
      partitions %>%
        st_drop_geometry() %>%
        filter(pixel_ID %in% highpol_pixels_HIGH_2050_list) %>%
        dplyr::select(pixel_ID, pixel_area) %>%
        unique() %>%
        summarise(total_area = sum(pixel_area)) %>%
        mutate(total_area_km2 = total_area / 10^6)-> area_above9_HIGH_2050
      
      #Perform intersection to crop pixels within the boundary
      grid_data_PM25_NAD_HIGH_2050_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_HIGH_2050_filtered_graph, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_NAD_HIGH_2050_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 9)) +  # Using the magma color scale
        ggtitle("2050 HIGH US Mean 9km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(mean_2050_HIGH, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        theme_void() +
        geom_sf(data = usa_boundary, color = "black", fill = NA)
      #ggsave("figures/BenMAP/9km/pollution/9km_PM25_2050_HIGH_plot.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/pollution/9km_PM25_2050_HIGH_plot.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
#### 4. 2050 Low CDR (9km resolution) ####
  #### 4.1. Get mean value ####
      #Read in netcdf file with daily-average annual PM2.5 concentrations (ug/m3) for 2Reference 2050
      #365 days, 399 rows, 612 columns, pixel size is 9000m2
      # Read the NetCDF file using stack
      nc_9km_stack_2050_LOW <- stack("data/WRF-CMAQ/9km_V2/Dailymean_9km_LowCDR_2050_PM25.nc", varname = "PM25_AVG")
      # Set the CRS explicitly based on the information you obtained from gdalinfo
      crs(nc_9km_stack_2050_LOW)
      
      # Calculate the mean across all bands (365 days) for each cell
      mean_raster_2050_LOW <- calc(nc_9km_stack_2050_LOW, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      
      # Plot the mean raster
      plot(mean_raster_2050_LOW, col = rev(magma(20)), main = "WEI 2050 Yearly Mean PM2.5 Concentrations (ug/m3) in LOW CDR case")
      
  #### 4.2. Get rows and columns matching####
      #1. make sure geometries align
      # 1. Check the CRS
      st_crs(grid_data)
      crs(mean_raster_2050_LOW)
      
      #2. Print extent information
      print(extent(mean_raster_2050_LOW))
      print(st_bbox(grid_data))
      
      #3. Check resolution
      res(mean_raster_2050_LOW)
      # Get the bounding box of the grid_data
      bbox <- st_bbox(grid_data)
      # Calculate the resolution
      resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                      (bbox$ymax - bbox$ymin) / num_rows)
      # Print the resolution
      print(resolution)
      
      # 2. Set the CRS for mean_raster_2050_LOW
      crs(mean_raster_2050_LOW) <- st_crs(grid_data)$proj4string
      
      # 3. Set the extent of mean_raster_aligned to match grid_data
      extent(mean_raster_2050_LOW) <- extent(grid_data)
      
  #### 4.3. Get values from raster and assign them to my grid ####
      # 1. Extract values using st_extract
      system.time({values <- raster::extract(mean_raster_2050_LOW, grid_data)}) #1737 s = ~28.8 min
      grid_data_PM25_LOW_2050 <- grid_data
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_LOW_2050$PM25_AVG <- values
      grid_data_PM25_LOW_2050$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_LOW_2050$PM25_AVG))
      
  #### 4.4. Prepare output table #### 
      # 1. Convert to NAD83
      grid_data_PM25_NAD_LOW_2050 <- st_transform(grid_data_PM25_LOW_2050, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_NAD_LOW_2050 %>%
        filter(pixel_ID %in% pixels_to_keep) -> grid_data_PM25_NAD_LOW_2050_filtered
      
      #Get right columns 
      grid_data_PM25_NAD_LOW_2050_filtered %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        dplyr::rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        filter(!is.na(Values)) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_NAD_LOW_2050_filtered_final
      
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_NAD_LOW_2050_filtered_final, row.names = FALSE, "output_data/BenMAP_files/9km/OutputTable_PM25_2050_LOW.csv")
      
      #Shapefile
     st_write(grid_data_PM25_NAD_LOW_2050_filtered, "output_data/shapefiles/9km/9_km_grid_NAD83_filtered_PM25_2050_LOW.shp")
      
  #### 4.5 Prepare figure ####
      #Get national mean (population weighted)
      grid_data_PM25_NAD_LOW_2050_filtered_final %>%
        left_join(population_2019_data_pixel_clean, by = c("Row", "Column")) -> grid_data_PM25_NAD_LOW_2050_filtered_final_pop
      
      grid_data_PM25_NAD_LOW_2050_filtered_final_pop$product <- grid_data_PM25_NAD_LOW_2050_filtered_final_pop$Values * grid_data_PM25_NAD_LOW_2050_filtered_final_pop$Population
      sum_products <- sum(grid_data_PM25_NAD_LOW_2050_filtered_final_pop$product)
      total_population <- sum(grid_data_PM25_NAD_LOW_2050_filtered_final_pop$Population)
      mean_2050_LOW <- sum_products / total_population #Population weighted mean PM2.5
      
      grid_data_PM25_NAD_LOW_2050_filtered_final %>%
        summarise(mean_PM25 = mean(Values)) -> mean_LOW_2050
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_NAD_LOW_2050_filtered %>%
        filter(!is.na(PM25_AVG)) %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_NAD_LOW_2050_filtered_graph
      
      #Get number of pixels with high pollution
      highpol_pixels_LOW_2050 <- sum(grid_data_PM25_NAD_LOW_2050_filtered_graph$PM25_AVG_graph >= 9)
      highpol_pixels_LOW_2050_list <- grid_data_PM25_NAD_LOW_2050_filtered_graph$pixel_ID[grid_data_PM25_NAD_LOW_2050_filtered_graph$PM25_AVG_graph >= 9]
      
      partitions %>%
        st_drop_geometry() %>%
        filter(pixel_ID %in% highpol_pixels_LOW_2050_list) %>%
        dplyr::select(pixel_ID, pixel_area) %>%
        unique() %>%
        summarise(total_area = sum(pixel_area)) %>%
        mutate(total_area_km2 = total_area / 10^6)-> area_above9_LOW_2050
      
      #Perform intersection to crop pixels within the boundary
      grid_data_PM25_NAD_LOW_2050_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_LOW_2050_filtered_graph, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_NAD_LOW_2050_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 9)) +  # Using the magma color scale
        ggtitle("2050 LOW US Mean 9km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(mean_2050_LOW, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        theme_void() +
        geom_sf(data = usa_boundary, color = "black", fill = NA)
      #ggsave("figures/BenMAP/9km/pollution/9km_PM25_2050_LOW_plot.png", p, width = 16, height = 12, units = "in", dpi = 600)
      # ggsave("figures/BenMAP/9km/pollution/9km_PM25_2050_LOW_plot.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
##### 5. 2050 High CDR vs Low CDR (9km resolution) ####
      #Here we calculate the difference of high - low in each pixel.
      #This is the same that BenMAP does in each pixel. We could just load BenMAP shapefile, or calculate here
      grid_data_PM25_NAD_LOW_2050_filtered_graph %>%
        st_drop_geometry() %>%
        dplyr::rename(PM25_AVG_LOW = PM25_AVG,
               PM25_AVG_LOW_graph = PM25_AVG_graph) -> grid_data_PM25_NAD_LOW_2050_filtered_graph_join
      
      grid_data_PM25_NAD_HIGH_2050_filtered_graph %>%
        dplyr::rename(PM25_AVG_HIGH = PM25_AVG,
               PM25_AVG_HIGH_graph = PM25_AVG_graph) %>%
        left_join(grid_data_PM25_NAD_LOW_2050_filtered_graph_join, by = c("col", "row", "pixel_ID")) -> grid_data_PM25_NAD_HIGHvsLOW_2050_filtered
      
      grid_data_PM25_NAD_HIGHvsLOW_2050_filtered %>%
        mutate(HIGH_minus_LOW = round(PM25_AVG_HIGH - PM25_AVG_LOW, 2),
               HIGH_minus_LOW_graph = if_else(HIGH_minus_LOW >= 2, 2, HIGH_minus_LOW)) %>%
        filter(!is.na(HIGH_minus_LOW)) %>%
        filter(!is.na(HIGH_minus_LOW_graph)) -> grid_data_PM25_NAD_HIGHvsLOW_2050_filtered_graph
      
      #Get national mean (population weighted)
      grid_data_PM25_NAD_HIGHvsLOW_2050_filtered_graph %>%
        left_join(population_2019_data_pixel_clean, by = c("row" ="Row", "col" = "Column")) -> grid_data_PM25_NAD_HIGHvsLOW_2050_filtered_graph_pop
      
      grid_data_PM25_NAD_HIGHvsLOW_2050_filtered_graph_pop$product <- grid_data_PM25_NAD_HIGHvsLOW_2050_filtered_graph_pop$HIGH_minus_LOW * grid_data_PM25_NAD_HIGHvsLOW_2050_filtered_graph_pop$Population
      sum_products <- sum(grid_data_PM25_NAD_HIGHvsLOW_2050_filtered_graph_pop$product)
      total_population <- sum(grid_data_PM25_NAD_HIGHvsLOW_2050_filtered_graph_pop$Population)
      mean_2050_HIGHvsLOW <- sum_products / total_population #Population weighted mean PM2.5
      
      grid_data_PM25_NAD_HIGHvsLOW_2050_filtered_graph %>%
        st_drop_geometry() %>%
        summarise(mean_PM25 = mean(HIGH_minus_LOW)) -> mean_HIGHvsLOW_2050 #Normal mean
      
      #Perform intersection to crop pixels within the boundary
      grid_data_PM25_NAD_HIGHvsLOW_2050_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_HIGHvsLOW_2050_filtered_graph, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_NAD_HIGHvsLOW_2050_filtered_graph_crop, aes(fill = HIGH_minus_LOW_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 2)) +  # Using the magma color scale
        ggtitle("2050 HIGH minus LOW US Mean 9km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(mean_2050_HIGHvsLOW, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        theme_void() +
        geom_sf(data = usa_boundary, color = "black", fill = NA) 
      #ggsave("figures/BenMAP/9km/pollution/9km_PM25_2050_HIGHvsLOW_plot.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/pollution/9km_PM25_2050_HIGHvsLOW_plot.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
##### 6. 2050 REF vs High CDR (9km resolution) ####
      #Here we calculate the difference of high - low in each pixel.
      #This is the same that BenMAP does in each pixel. We could just load BenMAP shapefile, or calculate here
      grid_data_PM25_NAD_REF_2050_filtered_graph %>%
        st_drop_geometry() %>%
        dplyr::rename(PM25_AVG_REF = PM25_AVG,
               PM25_AVG_REF_graph = PM25_AVG_graph) -> grid_data_PM25_NAD_REF_2050_filtered_graph_join
      
      grid_data_PM25_NAD_HIGH_2050_filtered_graph %>%
        dplyr::rename(PM25_AVG_HIGH = PM25_AVG,
               PM25_AVG_HIGH_graph = PM25_AVG_graph) %>%
        left_join(grid_data_PM25_NAD_REF_2050_filtered_graph_join, by = c("col", "row", "pixel_ID")) -> grid_data_PM25_NAD_REFvsHIGH_2050_filtered
      
      grid_data_PM25_NAD_REFvsHIGH_2050_filtered %>%
        mutate(REF_minus_HIGH = round(PM25_AVG_REF - PM25_AVG_HIGH, 2),
               REF_minus_HIGH_graph = if_else(REF_minus_HIGH >= 2, 2, REF_minus_HIGH)) %>%
        filter(!is.na(REF_minus_HIGH)) %>%
        filter(!is.na(REF_minus_HIGH_graph))-> grid_data_PM25_NAD_REFvsHIGH_2050_filtered_graph
      
      #Get national mean (population weighted)
      grid_data_PM25_NAD_REFvsHIGH_2050_filtered_graph %>%
        left_join(population_2019_data_pixel_clean, by = c("row" ="Row", "col" = "Column")) -> grid_data_PM25_NAD_REFvsHIGH_2050_filtered_graph_pop
      
      grid_data_PM25_NAD_REFvsHIGH_2050_filtered_graph_pop$product <- grid_data_PM25_NAD_REFvsHIGH_2050_filtered_graph_pop$REF_minus_HIGH * grid_data_PM25_NAD_REFvsHIGH_2050_filtered_graph_pop$Population
      sum_products <- sum(grid_data_PM25_NAD_REFvsHIGH_2050_filtered_graph_pop$product)
      total_population <- sum(grid_data_PM25_NAD_REFvsHIGH_2050_filtered_graph_pop$Population)
      mean_2050_REFvsHIGH <- sum_products / total_population #Population weighted mean PM2.5
      
      grid_data_PM25_NAD_REFvsHIGH_2050_filtered_graph %>%
        st_drop_geometry() %>%
        summarise(mean_PM25 = mean(REF_minus_HIGH)) -> mean_REFvsHIGH_2050
      
      #Perform intersection to crop pixels within the boundary
      grid_data_PM25_NAD_REFvsHIGH_2050_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_REFvsHIGH_2050_filtered_graph, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_NAD_REFvsHIGH_2050_filtered_graph_crop, aes(fill = REF_minus_HIGH_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 2)) +  # Using the magma color scale
        ggtitle("2050 REF minus HIGH US Mean 9km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(mean_2050_REFvsHIGH, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
        annotation_scale() +
        theme_void() +
        geom_sf(data = usa_boundary, color = "black", fill = NA) 
      #ggsave("figures/BenMAP/9km/pollution/9km_PM25_2050_REFvsHIGH_plot.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/pollution/9km_PM25_2050_REFvsHIGH_plot.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)

##### 7. 2050 REF vs LOW CDR (9km resolution) ####
      #Here we calculate the difference of high - low in each pixel.
      #This is the same that BenMAP does in each pixel. We could just load BenMAP shapefile, or calculate here
      grid_data_PM25_NAD_REF_2050_filtered_graph %>%
        dplyr::rename(PM25_AVG_REF = PM25_AVG,
              PM25_AVG_REF_graph = PM25_AVG_graph) %>%
        left_join(grid_data_PM25_NAD_LOW_2050_filtered_graph_join, 
                  by = c("col", "row", "pixel_ID")) -> grid_data_PM25_NAD_REFvsLOW_2050_filtered

      grid_data_PM25_NAD_REFvsLOW_2050_filtered %>%
        mutate(REF_minus_LOW = round(PM25_AVG_REF - PM25_AVG_LOW, 2),
               REF_minus_LOW_graph = if_else(REF_minus_LOW >= 2, 2, REF_minus_LOW)) %>%
        filter(!is.na(REF_minus_LOW)) %>%
        filter(!is.na(REF_minus_LOW_graph))-> grid_data_PM25_NAD_REFvsLOW_2050_filtered_graph
      
      #Get national mean (population weighted)
      grid_data_PM25_NAD_REFvsLOW_2050_filtered_graph %>%
        left_join(population_2019_data_pixel_clean, by = c("row" ="Row", "col" = "Column")) -> grid_data_PM25_NAD_REFvsLOW_2050_filtered_graph_pop
      
      grid_data_PM25_NAD_REFvsLOW_2050_filtered_graph_pop$product <- grid_data_PM25_NAD_REFvsLOW_2050_filtered_graph_pop$REF_minus_LOW * grid_data_PM25_NAD_REFvsLOW_2050_filtered_graph_pop$Population
      sum_products <- sum(grid_data_PM25_NAD_REFvsLOW_2050_filtered_graph_pop$product)
      total_population <- sum(grid_data_PM25_NAD_REFvsLOW_2050_filtered_graph_pop$Population)
      mean_2050_REFvsLOW <- sum_products / total_population #Population weighted mean PM2.5
      
      grid_data_PM25_NAD_REFvsLOW_2050_filtered_graph %>%
        summarise(mean_PM25 = mean(REF_minus_LOW)) -> mean_REFvsLOW_2050
      
      #Perform intersection to crop pixels within the boundary
      grid_data_PM25_NAD_REFvsLOW_2050_filtered_graph_crop <- st_intersection(grid_data_PM25_NAD_REFvsLOW_2050_filtered_graph, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_NAD_REFvsLOW_2050_filtered_graph_crop, aes(fill = REF_minus_LOW_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 2)) +  # Using the magma color scale
        ggtitle("2050 REF minus LOW US Mean 9km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(mean_2050_REFvsLOW, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
        annotation_scale() +
        theme_void() +
        geom_sf(data = usa_boundary, color = "black", fill = NA) 
      #ggsave("figures/BenMAP/9km/pollution/9km_PM25_2050_REFvsLOW_plot.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/pollution_/9km_PM25_2050_REFvsLOW_plot.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      

#------------------------------------------------- BENMAP OUTPUTS ---------------------------------------------------------------------------
      
#------------------------------------------------- SECTION 1: 9 KM MORTALITY BENMAP RESULTS -------------------------------------------------
#### 1. 2019 results Baseline ####
  #### 1.1. Load data ####
    #Here we load BenMAP shapefiles with mortality results to create plots 
    #These are results from a 2019 run, vs a 0 pollution case
        benmap_2019_9km <- st_read("BenMAP_results/9km_V2/2019/health_impacts_2019_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()
        #From BenMAP run of 2019 pollution vs 0 values. So it estimates number of people that died because of 2019 PM2.5 pollution in the US
      
      #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
      grid_data_PM25_NAD_filtered %>%
        dplyr::select(row, col)%>%
        left_join(benmap_2019_9km, by = c("row" = "ROW", "col" = "COL")) %>%
        mutate_all(~replace(., is.na(.), 0))-> benmap_2019_9km
  
  #### 1.2. Prepare data ####
        benmap_2019_9km %>% #This is the population in each pixel from 18-99, which is where the health impacts are applied
          mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000),
                 deaths_per_1000_graph = if_else(deaths_per_1000 >= 2, 2,deaths_per_1000)) -> benmap_2019_9km_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
    
  #### 1.3 Analyze output ####    
      #Calculate total
      benmap_2019_9km %>%
        st_drop_geometry() %>%
        summarise(mean_deaths = sum(Point.Estim), #The point esitmate is the mean
                  per2p5_deaths = sum(Percentile), #This is percentile 2.5
                  per97p5_deaths = sum(Percentile.19),#This is percentile 97.5
                  total_population = sum(Population)) %>% 
        mutate(Scenario = "2019 9km")-> total_mortality_2019
      
      #Get state values
      partitions %>%
        mutate(state_ID = as.numeric(state_ID)) %>%
        st_drop_geometry() %>%
        dplyr::select(pixel_ID, row, col, state_ID) %>%
        unique() %>%
        left_join(state_region_mapping %>% mutate(state_ID = as.numeric(state_ID)), by = "state_ID")-> pixels_by_state
      
      benmap_2019_9km_pop_weighted %>%
        st_drop_geometry() %>%
        left_join (pixels_by_state, by = c("row", "col")) %>%
        group_by(state_ID, state_name, Region) %>%
        dplyr::summarise(Population_state = sum(Population),
                  Point.Estim_state = sum(Point.Estim)) %>%
        mutate(deaths_per_1000 = (Point.Estim_state / Population_state)*1000,
               deaths_per_million = (Point.Estim_state / Population_state)*10^6)-> total_mortality_2019_state
  
  #### 1.4 Graph ####
      #Perform intersection to crop pixels within the boundary
      benmap_2019_9km_pop_weighted_crop <- st_intersection(benmap_2019_9km_pop_weighted, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = benmap_2019_9km_pop_weighted_crop, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 2)) + 
        ggtitle("2019 US Mortality due to Mean Yearly PM2.5 (ug/m3)") +
        geom_text(data = total_mortality_2019, aes(label = paste("Total Deaths:", round(mean_deaths))),
                  x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
        theme_void() +
        annotation_scale() +
        geom_sf(data = usa_boundary, color = "black", fill = NA) 
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2019_plot_BENMAP_result.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2019_plot_BENMAP_result.svg", p, width = 16, height = 12, units = "in", dpi = 600)

#### 2. 2050 High CDR (9km resolution) #### 
  #### 2.1. Load data ####
      #Here we load BenMAP shapefiles with mortality results to create plots
      #These are results from High CDR run, vs a 0 pollution case
      benmap_2050_HIGH_9km <- st_read("BenMAP_results/9km_V2/2050_HIGH/health_impact_2050_high_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()
      #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
      grid_data_PM25_NAD_filtered %>%
        dplyr::select(row, col)%>%
        left_join(benmap_2050_HIGH_9km, by = c("row" = "ROW", "col" = "COL")) %>%
        mutate_all(~replace(., is.na(.), 0))-> benmap_2050_HIGH_9km
      
  #### 2.2. Prepare data ####
      benmap_2050_HIGH_9km %>% #This is the population in each pixel from 18-99, which is where the health impacts are applied
        mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000),
               deaths_per_1000_v2 = if_else(deaths_per_1000 >= 2, 2,deaths_per_1000)) -> benmap_2050_HIGH_9km_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
  #### 2.3. Analyze output ####
      #Calculate total
      benmap_2050_HIGH_9km %>%
        st_drop_geometry() %>%
        summarise(mean_deaths = sum(Point.Estim), #The point esitmate is the mean
                  per2p5_deaths = sum(Percentile), #This is percentile 2.5
                  per97p5_deaths = sum(Percentile.19),#This is percentile 97.5
                  total_population = sum(Population)) %>% 
        mutate(Scenario = "2050 High 9km")-> total_mortality_2050_HIGH_9km
    
      #Get state values
      benmap_2050_HIGH_9km_pop_weighted %>%
        st_drop_geometry() %>%
        left_join (pixels_by_state, by = c("row", "col")) %>%
        group_by(state_ID, state_name, Region) %>%
        dplyr::summarise(Population_state = sum(Population),
                  Point.Estim_state = sum(Point.Estim)) %>%
        mutate(deaths_per_1000 = (Point.Estim_state / Population_state)*1000,
               deaths_per_million = (Point.Estim_state / Population_state)*10^6)-> total_mortality_2050_HIGH_9km_state
  
  #### 2.4 Figure ####
      #Perform intersection to crop pixels within the boundary
      benmap_2050_HIGH_9km_pop_weighted_crop <- st_intersection(benmap_2050_HIGH_9km_pop_weighted, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_HIGH_9km_pop_weighted_crop, aes(fill = deaths_per_1000_v2), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 2)) + 
        ggtitle("2050 HIGH CDR US Mortality due to Mean Yearly PM2.5 (per 1000 people)") +
        geom_text(data = total_mortality_2050_HIGH_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                  x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
        theme_void() +
        annotation_scale() +
        geom_sf(data = usa_boundary, color = "black", fill = NA) 
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2050_HIGH_plot_BENMAP_result.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2050_HIGH_plot_BENMAP_result.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      
#### 3. 2050 Low CDR (9km resolution) ####
  #### 3.1. Load data ####
      #Here we load BenMAP shapefiles with mortality results to create plots
      #These are results from Low CDR run, vs a 0 pollution case
      benmap_2050_LOW_9km <- st_read("BenMAP_results/9km_V2/2050_LOW/health_impacts_2050_low_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()
      
      #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
      grid_data_PM25_NAD_filtered %>%
        dplyr::select(row, col)%>%
        left_join(benmap_2050_LOW_9km, by = c("row" = "ROW", "col" = "COL")) %>%
        mutate_all(~replace(., is.na(.), 0))-> benmap_2050_LOW_9km
      
  #### 3.2. Prepare data ####
      benmap_2050_LOW_9km %>% #This is the population in each pixel from 18-99, which is where the health impacts are applied
        mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000)) %>%
        mutate(deaths_per_1000_v2 = if_else(deaths_per_1000 >= 2, 2,deaths_per_1000))-> benmap_2050_LOW_9km_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
      
  #### 3.3. Analyze output ####
      #Calculate total
      benmap_2050_LOW_9km %>%
        st_drop_geometry() %>%
        summarise(mean_deaths = sum(Point.Estim), #The point esitmate is the mean
                  per2p5_deaths = sum(Percentile), #This is percentile 2.5
                  per97p5_deaths = sum(Percentile.19), #This is percentile 97.5
                  total_population = sum(Population)) %>% 
        mutate(Scenario = "2050 Low 9km")-> total_mortality_2050_LOW_9km
      
      #Get state values
      benmap_2050_LOW_9km_pop_weighted %>%
        st_drop_geometry() %>%
        left_join (pixels_by_state, by = c("row", "col")) %>%
        group_by(state_ID, state_name, Region) %>%
        dplyr::summarise(Population_state = sum(Population),
                  Point.Estim_state = sum(Point.Estim)) %>%
        mutate(deaths_per_1000 = (Point.Estim_state / Population_state)*1000,
               deaths_per_million = (Point.Estim_state / Population_state)*10^6)-> total_mortality_2050_LOW_9km_state
      
  #### 3.4. Figure ####
      #Perform intersection to crop pixels within the boundary
      benmap_2050_LOW_9km_pop_weighted_crop <- st_intersection(benmap_2050_LOW_9km_pop_weighted, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_LOW_9km_pop_weighted_crop, aes(fill = deaths_per_1000_v2), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 2)) + 
        ggtitle("2050 LOW CDR US Mortality due to Mean Yearly PM2.5 (per 1000 people)")+
        geom_text(data = total_mortality_2050_LOW_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                  x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
        theme_void() +
        annotation_scale() +
        geom_sf(data = usa_boundary, color = "black", fill = NA) 
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2050_LOW_plot_BENMAP_result.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2050_LOW_plot_BENMAP_result.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      
#### 4. 2050 Reference (9km resolution) ####  
  #### 4.1. Load data ####
      #Here we load BenMAP shapefiles with mortality results to create plots      
      benmap_2050_REF_9km <- st_read("BenMAP_results/9km_V2/2050_REF/health_impacts_2050_ref_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()#From BenMAP run of 2019 pollution vs 0 values. So it estimates number of people that died because of 2019 PM2.5 pollution in the US
      
      #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
      grid_data_PM25_NAD_filtered %>%
        dplyr::select(row, col)%>%
        left_join(benmap_2050_REF_9km, by = c("row" = "ROW", "col" = "COL")) %>%
        mutate_all(~replace(., is.na(.), 0))-> benmap_2050_REF_9km
      
  #### 4.2. Prepare data ####
      benmap_2050_REF_9km %>% #This is the population in each pixel from 18-99, which is where the health impacts are applied
        mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000),
               deaths_per_1000_GRAPH = if_else(deaths_per_1000 >= 2, 2,deaths_per_1000)) -> benmap_2050_REF_9km_9km_pop_weighted 
      #This has number of people dying per pixel for every 1000 inhabitants age 18-99
  
  #### 4.3. Analyze output ####
      #Calculate total
      benmap_2050_REF_9km %>%
        st_drop_geometry() %>%
        dplyr::summarise(mean_deaths = sum(Point.Estim), #The point esitmate is the mean
                  per2p5_deaths = sum(Percentile), #This is percentile 2.5
                  per97p5_deaths = sum(Percentile.19),#This is percentile 97.5
                  total_population = sum(Population)) %>% 
        mutate(Scenario = "2050 Reference 9km")-> total_mortality_2050_REF_9km
    
      #Get state values
      benmap_2050_REF_9km_9km_pop_weighted %>%
        st_drop_geometry() %>%
        left_join (pixels_by_state, by = c("row", "col")) %>%
        group_by(state_ID, state_name, Region) %>%
        dplyr::summarise(Population_state = sum(Population),
                  Point.Estim_state = sum(Point.Estim)) %>%
        mutate(deaths_per_1000 = (Point.Estim_state / Population_state)*1000)-> total_mortality_2050_REF_9km_state 
      
  #### 4.4. Figure ####
      #Perform intersection to crop pixels within the boundary
      benmap_2050_REF_9km_9km_pop_weighted_crop <- st_intersection(benmap_2050_REF_9km_9km_pop_weighted, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_REF_9km_9km_pop_weighted_crop, aes(fill = deaths_per_1000_GRAPH), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 2)) + 
        ggtitle("2050 Reference US Mortality due to Mean Yearly PM2.5 (per 1000 people)")+
        geom_text(data = total_mortality_2050_REF_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                  x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
        theme_void() +
        annotation_scale() +
        geom_sf(data = usa_boundary, color = "black", fill = NA) 
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2050_REF_plot_BENMAP_result.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2050_REF_plot_BENMAP_result.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      
#### 5. 2050 High minus Low CDR (9km resolution) ####
  #### 5.1. Load data ####
      #Here we load BenMAP shapefiles with mortality results to create plots
      #These are results from High CDR run inus a Low CDR run
      benmap_2050_HIGHvsLOW_9km <- st_read("BenMAP_results/9km_V2/2050_HIGHvsLOW/health_impact_2050_highVSlow_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()
      
      #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
      grid_data_PM25_NAD_filtered %>%
        dplyr::select(row, col)%>%
        left_join(benmap_2050_HIGHvsLOW_9km, by = c("row" = "ROW", "col" = "COL")) %>%
        mutate_all(~replace(., is.na(.), 0))-> benmap_2050_HIGHvsLOW_9km
      
  #### 5.2. Prepare data ####
      benmap_2050_HIGHvsLOW_9km %>% #This is the population in each pixel from 18-99, which is where the health impacts are applied
        mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000),
               deaths_per_1000_GRAPH = if_else(deaths_per_1000 >= 0.5, 0.5, deaths_per_1000)) -> benmap_2050_HIGHvsLOW_9km_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
      
  #### 5.3. Analyze output ####
      #Calculate total
      benmap_2050_HIGHvsLOW_9km %>%
        st_drop_geometry() %>%
        summarise(mean_deaths = sum(Point.Estim), #The point esitmate is the mean
                  per2p5_deaths = sum(Percentile), #This is percentile 2.5
                  per97p5_deaths = sum(Percentile.19),#This is percentile 97.5
                  total_population = sum(Population)) %>% 
        mutate(Scenario = "2050 High vs Low 9km")-> total_mortality_2050_HIGHvsLOW_9km
      
  #### 5.4. Figure ####
      #Perform intersection to crop pixels within the boundary
      benmap_2050_HIGHvsLOW_9km_pop_weighted_crop <- st_intersection(benmap_2050_HIGHvsLOW_9km_pop_weighted, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_HIGHvsLOW_9km_pop_weighted_crop, aes(fill = deaths_per_1000_GRAPH), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 0.5)) + 
        ggtitle("2050 HIGH minus LOW CDR US Mortality due to Mean Yearly PM2.5 (per 1000 people)")+
        geom_text(data = total_mortality_2050_HIGHvsLOW_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                  x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
        theme_void() +
        annotation_scale() +
        geom_sf(data = usa_boundary, color = "black", fill = NA) 
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2050_HIGHvsLOW_plot_BENMAP_result.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2050_HIGHvsLOW_plot_BENMAP_result.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      
#### 6. 2050 Reference minus High CDR (9km resolution) ####  
  #### 6.1. Load data ####
      #Here we load BenMAP shapefiles with mortality results to create plots
      benmap_2050_REFvsHIGH_9km <- st_read("BenMAP_results/9km_V2/2050_REFvsHIGH/health_impacts_2050_REFvsHIGH_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()
      #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
      grid_data_PM25_NAD_filtered %>%
        dplyr::select(row, col)%>%
        left_join(benmap_2050_REFvsHIGH_9km, by = c("row" = "ROW", "col" = "COL")) %>%
        mutate_all(~replace(., is.na(.), 0))-> benmap_2050_REFvsHIGH_9km
      
  #### 6.2. Prepare data ####
      benmap_2050_REFvsHIGH_9km %>% #This is the population in each pixel from 18-99, which is where the health impacts are applied
        mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000),
               deaths_per_1000_GRAPH = if_else(deaths_per_1000 >= 0.5, 0.5,deaths_per_1000)) -> benmap_2050_REFvsHIGH_9km_pop_weighted 
      #This has number of people dying per pixel for every 1000 inhabitants age 18-99
      
  #### 6.3. Analyze the data #### 
      #Calculate total
      benmap_2050_REFvsHIGH_9km %>%
        st_drop_geometry() %>%
        summarise(mean_deaths = sum(Point.Estim), #The point esitmate is the mean
                  per2p5_deaths = sum(Percentile), #This is percentile 2.5
                  per97p5_deaths = sum(Percentile.19),#This is percentile 97.5
                  total_population = sum(Population)) %>% 
        mutate(Scenario = "2050 Reference vs High 9km")-> total_mortality_2050_REFvsHIGH_9km
  
  #### 6.4. Figure ####
      #Perform intersection to crop pixels within the boundary
      benmap_2050_REFvsHIGH_9km_pop_weighted_crop <- st_intersection(benmap_2050_REFvsHIGH_9km_pop_weighted, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_REFvsHIGH_9km_pop_weighted_crop, aes(fill = deaths_per_1000_GRAPH), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 0.5)) + 
        ggtitle("2050 REF vs HIGH CDR US Mortality due to Mean Yearly PM2.5 (per 1000 people)")+
        geom_text(data = total_mortality_2050_REFvsHIGH_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                  x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
        theme_void() +
        annotation_scale() +
        geom_sf(data = usa_boundary, color = "black", fill = NA) 
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2050_REFvsHIGH_plot_BENMAP_result.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2050_REFvsHIGH_plot_BENMAP_result.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      
#### 7. 2050 Reference minus Low CDR (9km resolution) ####  
      #### 7.1. Load data ####
      #Here we load BenMAP shapefiles with mortality results to create plots
      benmap_2050_REFvsLOW_9km <- st_read("BenMAP_results/9km_V2/2050_REFvsLOW/health_impacts_2050_REFvsLOW_9km/Health Impacts-Pope 2019_20240526.shp") %>% st_drop_geometry()
      
      #Set pixels with 0 population to 0 mortality. Currently they are not here (316 pixels)
      grid_data_PM25_NAD_filtered %>%
        dplyr::select(row, col)%>%
        left_join(benmap_2050_REFvsLOW_9km, by = c("row" = "ROW", "col" = "COL")) %>%
        mutate_all(~replace(., is.na(.), 0))-> benmap_2050_REFvsLOW_9km
      
      #### 7.2. Prepare data ####
      benmap_2050_REFvsLOW_9km %>% #This is the population in each pixel from 18-99, which is where the health impacts are applied
        mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000),
               deaths_per_1000_GRAPH = if_else(deaths_per_1000 >= 0.5, 0.5,deaths_per_1000)) -> benmap_2050_REFvsLOW_9km_pop_weighted 
      #This has number of people dying per pixel for every 1000 inhabitants age 18-99
      
      #### 7.3. Analyze the data #### 
      #Calculate total
      benmap_2050_REFvsLOW_9km %>%
        st_drop_geometry() %>%
        summarise(mean_deaths = sum(Point.Estim), #The point esitmate is the mean
                  per2p5_deaths = sum(Percentile), #This is percentile 2.5
                  per97p5_deaths = sum(Percentile.19),#This is percentile 97.5
                  total_population = sum(Population)) %>% 
        mutate(Scenario = "2050 Reference vs High 9km")-> total_mortality_2050_REFvsLOW_9km
      
      #### 7.4. Figure ####
      #Perform intersection to crop pixels within the boundary
      benmap_2050_REFvsLOW_9km_pop_weighted_crop <- st_intersection(benmap_2050_REFvsLOW_9km_pop_weighted, usa_boundary)
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_REFvsLOW_9km_pop_weighted_crop, aes(fill = deaths_per_1000_GRAPH), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 0.5)) + 
        ggtitle("2050 REF vs LOW CDR US Mortality due to Mean Yearly PM2.5 (per 1000 people)")+
        geom_text(data = total_mortality_2050_REFvsLOW_9km, aes(label = paste("Total Deaths:", round(mean_deaths))),
                  x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 5)+
        theme_void() +
        annotation_scale() +
        geom_sf(data = usa_boundary, color = "black", fill = NA) 
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2050_REFvsLOW_plot_BENMAP_result.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/9km/mortality/9km_mortality_2050_REFvsLOW_plot_BENMAP_result.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      
      
      p <- ggplot() +
        geom_sf(data = usa_boundary, color = "black", fill = NA, size = 2) 
      #ggsave("figures/BenMAP/9km/US_MAP2.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      