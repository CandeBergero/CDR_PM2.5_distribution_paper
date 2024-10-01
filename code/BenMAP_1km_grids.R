# BenMAP_1km_grids.R
# Code for re-scaling variables at 1 km grid: population and mortlaity, and for getting mean for PM2.5
# This script is designed to re-scale demographic and socioeconomic data from census block level to 1km grid
# Date: March-April 2024
# Author: Candelaria Bergero

#------------------------------------------------- LOAD LIBRARIES -------------------------------------------------
#Load all packages
library(sp)
library(rgdal)

library(purrr)
library(ggplot2)
library(viridis)
library(data.table)

library(sf)
library(ncdf4)
library(raster)
library(future)
library(furrr)
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggspatial)

# Get today's date
today_date <- Sys.Date()

#General files
source( "code/color_schemes.R" ) # some predefined color schemes
state_region_mapping <- read.csv("mappings/state_region_mapping.csv") %>%
  mutate( state_ID = gsub("^_", "", state_ID))
cbsa_shapefile <- st_read("shapefiles/cb_2019_us_cbsa_500k/cb_2019_us_cbsa_500k.shp") %>% dplyr::select(AFFGEOID, NAME, geometry)
colors <- rev(brewer.pal(n = 11, name = "RdYlBu"))

#------------------------------------------------- A. BENMAP INPUTS --------------------------------------------------

#------------------------------------------------- SECTION A1: 1 KM GRID -------------------------------------------------
#Work with 1 km grid from air pollution results
#### 1. SEATTLE ####
  #### 1.1. Assign numbers ####
    #Load 1 km grid data 
    grid_data_Seattle <- st_read("shapefiles/1km-1_seattle/fishnet_1km_1.shp") #Provided by Jing Cheng based on WRF-CMAQ resolution
  
    #Set grid numbers 
    grid_data_Seattle$pixel_ID <- seq_along(grid_data_Seattle$geometry)
    grid_data_Seattle <- grid_data_Seattle[, c("geometry", "pixel_ID")]
        
    #Extract bounding box
    bbox <- st_bbox(grid_data_Seattle$geometry)
  
    #Calculate the number of columns and rows 
    num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
    num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
    
    # Extract centroids from the geometry column
    centroids <- st_centroid(grid_data_Seattle$geometry)
    
    # Extract x and y coordinates from the centroid geometry
    x_coords <- st_coordinates(centroids)[, "X"]
    y_coords <- st_coordinates(centroids)[, "Y"]
    
    #Add rows and columns
    grid_data_Seattle$row <- num_rows - floor((y_coords - bbox["ymin"]) / 1000)
    grid_data_Seattle$col <- ceiling((x_coords - bbox["xmin"]) / 1000)
    
  #### 1.2. Convert CRS ####
      grid_data_Seattle_NAD83 <- st_transform(grid_data_Seattle, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_Seattle_NAD83 %>%
        dplyr::select(-pixel_ID) -> grid_data_Seattle_NAD83_benmap #This has all pixels
  
  #### 1.3. Crop to MSA ####
      cbsa_shapefile %>%
        filter(AFFGEOID == "310M500US42660")-> cbsa_shapefile_seattle
      
      intersection_seattle_NAD83 <- st_intersection(grid_data_Seattle_NAD83, cbsa_shapefile_seattle)
      seattle_pixels <- intersection_seattle_NAD83$pixel_ID #These are the pixels from my grid in the MSA
      
      grid_data_Seattle_NAD83 %>%
        filter(pixel_ID %in% seattle_pixels) -> grid_data_Seattle_NAD83_filtered
      
      st_write(grid_data_Seattle_NAD83_filtered, "output_data/shapefiles/1km/Seattle/1km_shapefile_2019_03252024_seattle.shp") 
      #Has total pixels 16,364 we use as grid in BenMAP. 
      

#### 2-4. SOUTH-WEST ####
      #This includes 3 MSAs: 
        #1. Los Angeles–Long Beach–Anaheim, CA MSA
        #2. Riverside–San Bernardino–Ontario, CA MSA
        #3. Phoenix–Mesa–Chandler, AZ MSA
  #### 2.1. Assign numbers ####
    #Load 1 km grid data 
    grid_data_south_west <- st_read("shapefiles/1km-2_southwest/fishnet_1km_2.shp") #Provided by Jing Cheng based on WRF-CMAQ resolution
      
    #Set grid numbers 
    grid_data_south_west$pixel_ID <- seq_along(grid_data_south_west$geometry)
    grid_data_south_west <- grid_data_south_west[, c("geometry", "pixel_ID")]
  
    #Extract bounding box
    bbox <- st_bbox(grid_data_south_west$geometry)
      
    #Calculate the number of columns and rows
    num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
    num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)

    # Extract centroids from the geometry column
    centroids <- st_centroid(grid_data_south_west$geometry)
    
    # Extract x and y coordinates from the centroid geometry
    x_coords <- st_coordinates(centroids)[, "X"]
    y_coords <- st_coordinates(centroids)[, "Y"]
    
    #Add rows and columns
    grid_data_south_west$row <- num_rows - floor((y_coords - bbox["ymin"]) / 1000)
    grid_data_south_west$col <- ceiling((x_coords - bbox["xmin"]) / 1000)
    
  #### 2.2. Convert CRS ####
    grid_data_south_west_NAD83 <- st_transform(grid_data_south_west, crs = st_crs("+proj=longlat +datum=NAD83"))
    
    grid_data_south_west_NAD83 %>%
      dplyr::select(-pixel_ID) -> grid_data_south_west_NAD83_benmap
    
  #### 2.3. Crop to each MSA ####
    #Los Angeles
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US31080")-> cbsa_shapefile_LA
    
    intersection_LA_NAD83 <- st_intersection(grid_data_south_west_NAD83, cbsa_shapefile_LA)
    LA_pixels <- intersection_LA_NAD83$pixel_ID #These are the pixels from my grid in the MSA

    grid_data_south_west_NAD83 %>%
      filter(pixel_ID %in% LA_pixels) -> grid_data_south_west_NAD83_LA
    
    st_write(grid_data_south_west_NAD83_LA, "output_data/shapefiles/1km/LA/1km_shapefile_2019_03252024_LA.shp") 
    #Has total pixels 13,121 we use as grid in BenMAP.
    
    #Riverside
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US40140")-> cbsa_shapefile_riverside
    
    intersection_riverside_NAD83 <- st_intersection(grid_data_south_west_NAD83, cbsa_shapefile_riverside)
    riverside_pixels <- intersection_riverside_NAD83$pixel_ID #These are the pixels from my grid in the MSA
    
    grid_data_south_west_NAD83 %>%
      filter(pixel_ID %in% riverside_pixels) -> grid_data_south_west_NAD83_riverside
    
    st_write(grid_data_south_west_NAD83_riverside, "output_data/shapefiles/1km/Riverside/1km_shapefile_2019_03252024_riverside.shp")
    #Has total pixels 71,352 we use as grid in BenMAP.
    
    #Phoenix
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US38060")-> cbsa_shapefile_phoenix
    
    intersection_phoenix_NAD83 <- st_intersection(grid_data_south_west_NAD83, cbsa_shapefile_phoenix)
    phoenix_pixels <- intersection_phoenix_NAD83$pixel_ID #These are the pixels from my grid in the MSA
    
    grid_data_south_west_NAD83 %>%
      filter(pixel_ID %in% phoenix_pixels) -> grid_data_south_west_NAD83_phoenix
    
    st_write(grid_data_south_west_NAD83_phoenix, "output_data/shapefiles/1km/Phoenix/1km_shapefile_2019_03252024_phoenix.shp")
    #Has total pixels 38,341 we use as grid in BenMAP.
    
#### 5. DALLAS ####
  #### 5.1. Load 1 km grid data ####
    grid_data_dallas <- st_read("shapefiles/1km-3_dallas/fishnet_1km_3.shp") #Provided by Jing Cheng based on WRF-CMAQ resolution
    
    #Set grid numbers 
    grid_data_dallas$pixel_ID <- seq_along(grid_data_dallas$geometry)
    grid_data_dallas <- grid_data_dallas[, c("geometry", "pixel_ID")]
    
    #Extract bounding box
    bbox <- st_bbox(grid_data_dallas$geometry)
    
    #Calculate the number of columns and rows
    num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
    num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
    
    # Extract centroids from the geometry column
    centroids <- st_centroid(grid_data_dallas$geometry)
    
    # Extract x and y coordinates from the centroid geometry
    x_coords <- st_coordinates(centroids)[, "X"]
    y_coords <- st_coordinates(centroids)[, "Y"]
    
    #Add rows and columns
    grid_data_dallas$row <- num_rows - floor((y_coords - bbox["ymin"]) / 1000)
    grid_data_dallas$col <- ceiling((x_coords - bbox["xmin"]) / 1000)
    
  #### 5.2. Convert CRS ####
    grid_data_dallas_NAD83 <- st_transform(grid_data_dallas, crs = st_crs("+proj=longlat +datum=NAD83"))
    
    grid_data_dallas_NAD83 %>%
      dplyr::select(-pixel_ID) -> grid_data_dallas_NAD83_benmap #This has all pixels
    
  #### 5.3. Crop to MSA ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US19100")-> cbsa_shapefile_dallas
    
    intersection_dallas_NAD83 <- st_intersection(grid_data_dallas_NAD83, cbsa_shapefile_dallas)
    dallas_pixels <- intersection_dallas_NAD83$pixel_ID #These are the pixels from my grid in the MSA
    
    grid_data_dallas_NAD83 %>%
      filter(pixel_ID %in% dallas_pixels) -> grid_data_dallas_NAD83_filtered
    
    st_write(grid_data_dallas_NAD83_filtered, "output_data/shapefiles/1km/Dallas/1km_shapefile_2019_03252024_dallas.shp") 
    #Has total pixels 23,747 we use as grid in BenMAP. 

#### 6. Houston ####
  #### 6.1. Load 1 km grid data ####
    grid_data_houston <- st_read("shapefiles/1km-4_houston/fishnet_1km_4.shp") #Provided by Jing Cheng based on WRF-CMAQ resolution
    
    #Set grid numbers
    grid_data_houston$pixel_ID <- seq_along(grid_data_houston$geometry)
    grid_data_houston <- grid_data_houston[, c("geometry", "pixel_ID")]
    
    #Extract bounding box 
    bbox <- st_bbox(grid_data_houston$geometry)
    
    #Calculate the number of columns and rows
    num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
    num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
 
    # Extract centroids from the geometry column
    centroids <- st_centroid(grid_data_houston$geometry)
    
    # Extract x and y coordinates from the centroid geometry
    x_coords <- st_coordinates(centroids)[, "X"]
    y_coords <- st_coordinates(centroids)[, "Y"]
    
    #Add rows and columns
    grid_data_houston$row <- num_rows - floor((y_coords - bbox["ymin"]) / 1000)
    grid_data_houston$col <- ceiling((x_coords - bbox["xmin"]) / 1000)
    
  #### 6.2. Convert CRS ####
    grid_data_houston_NAD83 <- st_transform(grid_data_houston, crs = st_crs("+proj=longlat +datum=NAD83"))
    
    grid_data_houston_NAD83 %>%
      dplyr::select(-pixel_ID) -> grid_data_houston_NAD83_benmap #This has all pixels
    
  #### 6.3. Crop to MSA ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US26420")-> cbsa_shapefile_houston
    
    intersection_houston_NAD83 <- st_intersection(grid_data_houston_NAD83, cbsa_shapefile_houston)
    houston_pixels <- intersection_houston_NAD83$pixel_ID #These are the pixels from my grid in the MSA
    
    grid_data_houston_NAD83 %>%
      filter(pixel_ID %in% houston_pixels) -> grid_data_houston_NAD83_filtered
    
    st_write(grid_data_houston_NAD83_filtered, "output_data/shapefiles/1km/Houston/1km_shapefile_2019_03252024_houston.shp") 
    #Has total pixels 23,340 we use as grid in BenMAP. 
    
#### 7. MIAMI ####
  #### 7.1. Load 1 km grid data ####
    grid_data_miami <- st_read("shapefiles/1km-5_miami/fishnet_1km_5.shp") #Provided by Jing Cheng based on WRF-CMAQ resolution
    
    #Set grid numbers
    grid_data_miami$pixel_ID <- seq_along(grid_data_miami$geometry)
    grid_data_miami <- grid_data_miami[, c("geometry", "pixel_ID")]
    
    #Extract bounding box
    bbox <- st_bbox(grid_data_miami$geometry)
    
    #Calculate the number of columns and rows
    num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
    num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)

    # Extract centroids from the geometry column
    centroids <- st_centroid(grid_data_miami$geometry)
    
    # Extract x and y coordinates from the centroid geometry
    x_coords <- st_coordinates(centroids)[, "X"]
    y_coords <- st_coordinates(centroids)[, "Y"]
    
    #Add rows and columns
    grid_data_miami$row <- num_rows - floor((y_coords - bbox["ymin"]) / 1000)
    grid_data_miami$col <- ceiling((x_coords - bbox["xmin"]) / 1000)
    
  #### 7.2. Convert CRS ####
    grid_data_miami_NAD83 <- st_transform(grid_data_miami, crs = st_crs("+proj=longlat +datum=NAD83"))
    
    grid_data_miami_NAD83 %>%
      dplyr::select(-pixel_ID) -> grid_data_miami_NAD83_benmap #This has all pixels
    
  #### 7.3. Crop to MSA ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US33100")-> cbsa_shapefile_miami
    
    intersection_miami_NAD83 <- st_intersection(grid_data_miami_NAD83, cbsa_shapefile_miami)
    miami_pixels <- intersection_miami_NAD83$pixel_ID #These are the pixels from my grid in the MSA
    
    grid_data_miami_NAD83 %>%
      filter(pixel_ID %in% miami_pixels) -> grid_data_miami_NAD83_filtered
    
    st_write(grid_data_miami_NAD83_filtered, "output_data/shapefiles/1km/Miami/1km_shapefile_2019_03252024_miami.shp") 
    #Has total pixels 15,126 we use as grid in BenMAP. 
    
#### 8. ATLANTA ####
  #### 8.1. Load 1 km grid data ####
    grid_data_atl <- st_read("shapefiles/1km-6_atlanta/fishnet_1km_6.shp") #Provided by Jing Cheng based on WRF-CMAQ resolution
    
    #Set grid numbers
    grid_data_atl$pixel_ID <- seq_along(grid_data_atl$geometry)
    grid_data_atl <- grid_data_atl[, c("geometry", "pixel_ID")]
    
    # Extract bounding box 
    bbox <- st_bbox(grid_data_atl$geometry)
    
    #Calculate the number of columns and rows
    num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
    num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)

    # Extract centroids from the geometry column
    centroids <- st_centroid(grid_data_atl$geometry)
    
    # Extract x and y coordinates from the centroid geometry
    x_coords <- st_coordinates(centroids)[, "X"]
    y_coords <- st_coordinates(centroids)[, "Y"]
    
    #Add rows and columns
    grid_data_atl$row <- num_rows - floor((y_coords - bbox["ymin"]) / 1000)
    grid_data_atl$col <- ceiling((x_coords - bbox["xmin"]) / 1000)
    
  #### 8.2. Convert CRS ####
    grid_data_atl_NAD83 <- st_transform(grid_data_atl, crs = st_crs("+proj=longlat +datum=NAD83"))
    
    grid_data_atl_NAD83 %>%
      dplyr::select(-pixel_ID) -> grid_data_atl_NAD83_benmap #This has all pixels
    
  #### 8.3. Crop to MSA ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US12060")-> cbsa_shapefile_atl
    
    intersection_atl_NAD83 <- st_intersection(grid_data_atl_NAD83, cbsa_shapefile_atl)
    atl_pixels <- intersection_atl_NAD83$pixel_ID #These are the pixels from my grid in the MSA
    
    grid_data_atl_NAD83 %>%
      filter(pixel_ID %in% atl_pixels) -> grid_data_atl_NAD83_filtered
    
    st_write(grid_data_atl_NAD83_filtered, "output_data/shapefiles/1km/Atlanta/1km_shapefile_2019_03252024_atl.shp") 
    #Has total pixels 23,341 we use as grid in BenMAP. 

#### 9. CHICAGO ####
  #### 9.1. Load 1 km grid data ####
    grid_data_chicago <- st_read("shapefiles/1km-7_chicago/fishnet_1km_7.shp") #Provided by Jing Cheng based on WRF-CMAQ resolution
    
    #Set grid numbers
    grid_data_chicago$pixel_ID <- seq_along(grid_data_chicago$geometry)
    grid_data_chicago <- grid_data_chicago[, c("geometry", "pixel_ID")]
    
    #Extract bounding box
    bbox <- st_bbox(grid_data_chicago$geometry)
    
    #Calculate the number of columns and rows
    num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
    num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
    
    # Extract centroids from the geometry column
    centroids <- st_centroid(grid_data_chicago$geometry)
    
    # Extract x and y coordinates from the centroid geometry
    x_coords <- st_coordinates(centroids)[, "X"]
    y_coords <- st_coordinates(centroids)[, "Y"]
    
    #Add rows and columns
    grid_data_chicago$row <- num_rows - floor((y_coords - bbox["ymin"]) / 1000)
    grid_data_chicago$col <- ceiling((x_coords - bbox["xmin"]) / 1000)
    
  #### 9.2. Convert CRS ####
    grid_data_chicago_NAD83 <- st_transform(grid_data_chicago, crs = st_crs("+proj=longlat +datum=NAD83"))
    
    grid_data_chicago_NAD83 %>%
      dplyr::select(-pixel_ID) -> grid_data_chicago_NAD83_benmap #This has all pixels
    
  #### 9.3. Crop to MSA ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US16980")-> cbsa_shapefile_chicago
    
    intersection_chicago_NAD83 <- st_intersection(grid_data_chicago_NAD83, cbsa_shapefile_chicago)
    chicago_pixels <- intersection_chicago_NAD83$pixel_ID #These are the pixels from my grid in the MSA
    
    grid_data_chicago_NAD83 %>%
      filter(pixel_ID %in% chicago_pixels) -> grid_data_chicago_NAD83_filtered
    
    st_write(grid_data_chicago_NAD83_filtered, "output_data/shapefiles/1km/Chicago/1km_shapefile_2019_03252024_chicago.shp") 
    #Has total pixels 19,209 we use as grid in BenMAP.     
 
#### 10. WASHINGTON ####
  #### 10.1. Load 1 km grid data ####
    grid_data_wash <- st_read("shapefiles/1km-8_washington/fishnet_1km_8.shp") #Provided by Jing Cheng based on WRF-CMAQ resolution
    
    #Set grid numbers
    grid_data_wash$pixel_ID <- seq_along(grid_data_wash$geometry)
    grid_data_wash <- grid_data_wash[, c("geometry", "pixel_ID")]
    
    #Extract bounding box
    bbox <- st_bbox(grid_data_wash$geometry)
    
    #Calculate the number of columns and rows
    num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
    num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)

    # Extract centroids from the geometry column
    centroids <- st_centroid(grid_data_wash$geometry)
    
    # Extract x and y coordinates from the centroid geometry
    x_coords <- st_coordinates(centroids)[, "X"]
    y_coords <- st_coordinates(centroids)[, "Y"]
    
    #Add rows and columns
    grid_data_wash$row <- num_rows - floor((y_coords - bbox["ymin"]) / 1000)
    grid_data_wash$col <- ceiling((x_coords - bbox["xmin"]) / 1000)
    
  #### 10.2. Convert CRS ####
    grid_data_wash_NAD83 <- st_transform(grid_data_wash, crs = st_crs("+proj=longlat +datum=NAD83"))
    
    grid_data_wash_NAD83 %>%
      dplyr::select(-pixel_ID) -> grid_data_wash_NAD83_benmap #This has all pixels
    
  #### 10.3. Crop to MSA ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US47900")-> cbsa_shapefile_wash
    
    intersection_wash_NAD83 <- st_intersection(grid_data_wash_NAD83, cbsa_shapefile_wash)
    wash_pixels <- intersection_wash_NAD83$pixel_ID #These are the pixels from my grid in the MSA
    
    grid_data_wash_NAD83 %>%
      filter(pixel_ID %in% wash_pixels) -> grid_data_wash_NAD83_filtered
    
    st_write(grid_data_wash_NAD83_filtered, "output_data/shapefiles/1km/DC/1km_shapefile_2019_03252024_wash.shp") 
    #Has total pixels 17,877 we use as grid in BenMAP.    

#### 11-12. NORTH-EAST ####
    #This file contains 2 MSAs:
      #Philadelphia
      #New York
  #### 11.1. Load 1 km grid data ####
    grid_data_northeast <- st_read("shapefiles/1km-9_phil_ny/fishnet_1km_9.shp") #Provided by Jing Cheng based on WRF-CMAQ resolution
    
    #Set grid numbers
    grid_data_northeast$pixel_ID <- seq_along(grid_data_northeast$geometry)
    grid_data_northeast <- grid_data_northeast[, c("geometry", "pixel_ID")]
    
    #Extract bounding box 
    bbox <- st_bbox(grid_data_northeast$geometry)
    
    #Calculate the number of columns and rows 
    num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
    num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
  
    # Extract centroids from the geometry column
    centroids <- st_centroid(grid_data_northeast$geometry)
    
    # Extract x and y coordinates from the centroid geometry
    x_coords <- st_coordinates(centroids)[, "X"]
    y_coords <- st_coordinates(centroids)[, "Y"]
    
    #Add rows and columns 
    grid_data_northeast$row <- num_rows - floor((y_coords - bbox["ymin"]) / 1000)
    grid_data_northeast$col <- ceiling((x_coords - bbox["xmin"]) / 1000)
    
  #### 11.2. Convert CRS ####
    grid_data_northeast_NAD83 <- st_transform(grid_data_northeast, crs = st_crs("+proj=longlat +datum=NAD83"))
    
    grid_data_northeast_NAD83 %>%
      dplyr::select(-pixel_ID) -> grid_data_northeast_NAD83_benmap #This has all pixels
    
  #### 11.3. Crop to MSA ####
    #Philadelphia
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US37980")-> cbsa_shapefile_phil
    
    intersection_phil_NAD83 <- st_intersection(grid_data_northeast_NAD83, cbsa_shapefile_phil)
    phil_pixels <- intersection_phil_NAD83$pixel_ID #These are the pixels from my grid in the MSA
    
    grid_data_northeast_NAD83 %>%
      filter(pixel_ID %in% phil_pixels) -> grid_data_phil_NAD83_filtered
    
    st_write(grid_data_phil_NAD83_filtered, "output_data/shapefiles/1km/Philadelphia/1km_shapefile_2019_03252024_phil.shp") 
    #Has total pixels 12,652 we use as grid in BenMAP.     
    
    #New York
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US35620")-> cbsa_shapefile_NY
    
    intersection_NY_NAD83 <- st_intersection(grid_data_northeast_NAD83, cbsa_shapefile_NY)
    NY_pixels <- intersection_NY_NAD83$pixel_ID #These are the pixels from my grid in the MSA
    
    grid_data_northeast_NAD83 %>%
      filter(pixel_ID %in% NY_pixels) -> grid_data_NY_NAD83_filtered
    
    st_write(grid_data_NY_NAD83_filtered, "output_data/shapefiles/1km/NY/1km_shapefile_2019_03252024_NY.shp") 
    #Has total pixels 19,841 we use as grid in BenMAP.     

#### 13. BOSTON ####
  #### 13.1. Load 1 km grid data ####
    grid_data_boston <- st_read("shapefiles/1km-10_boston/fishnet_1km_10.shp") #Provided by Jing Cheng based on WRF-CMAQ resolution
    
    #Set grid numbers
      grid_data_boston$pixel_ID <- seq_along(grid_data_boston$geometry)
      grid_data_boston <- grid_data_boston[, c("geometry", "pixel_ID")]
      
    #Extract bounding box
      bbox <- st_bbox(grid_data_boston$geometry)
      
    #Calculate the number of columns and rows
      num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
      num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
      
    # Extract centroids from the geometry column
      centroids <- st_centroid(grid_data_boston$geometry)
      
    # Extract x and y coordinates from the centroid geometry
      x_coords <- st_coordinates(centroids)[, "X"]
      y_coords <- st_coordinates(centroids)[, "Y"]
      
    #Add rows and columns
      grid_data_boston$row <- num_rows - floor((y_coords - bbox["ymin"]) / 1000)
      grid_data_boston$col <- ceiling((x_coords - bbox["xmin"]) / 1000)
      
  #### 13.2. Convert CRS ####
    grid_data_boston_NAD83 <- st_transform(grid_data_boston, crs = st_crs("+proj=longlat +datum=NAD83"))
    
    grid_data_boston_NAD83 %>%
      dplyr::select(-pixel_ID) -> grid_data_boston_NAD83_benmap #This has all pixels
    
  #### 13.3. Crop to MSA ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US14460")-> cbsa_shapefile_boston
    
    intersection_boston_NAD83 <- st_intersection(grid_data_boston_NAD83, cbsa_shapefile_boston)
    boston_pixels <- intersection_boston_NAD83$pixel_ID #These are the pixels from my grid in the MSA
    
    grid_data_boston_NAD83 %>%
      filter(pixel_ID %in% boston_pixels) -> grid_data_boston_NAD83_filtered
    
    st_write(grid_data_boston_NAD83_filtered, "output_data/shapefiles/1km/Boston/1km_shapefile_2019_03252024_boston.shp") 
    #Has total pixels 10,087 we use as grid in BenMAP.   
    
#### 14. DETROIT ####
  #### 14.1. Load 1 km grid data ####
    grid_data_detroit <- st_read("shapefiles/1km-11_detroit/fishnet_1km_11.shp") #Provided by Jing Cheng based on WRF-CMAQ resolution
    
    #Set grid numbers 
    grid_data_detroit$pixel_ID <- seq_along(grid_data_detroit$geometry)
    grid_data_detroit <- grid_data_detroit[, c("geometry", "pixel_ID")]
    
    #Extract bounding box
    bbox <- st_bbox(grid_data_detroit$geometry)
    
    #Calculate the number of columns and rows 
    num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
    num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
    
    # Extract centroids from the geometry column
    centroids <- st_centroid(grid_data_detroit$geometry)
    
    # Extract x and y coordinates from the centroid geometry
    x_coords <- st_coordinates(centroids)[, "X"]
    y_coords <- st_coordinates(centroids)[, "Y"]
    
    #Add rows and columns
    grid_data_detroit$row <- num_rows - floor((y_coords - bbox["ymin"]) / 1000)
    grid_data_detroit$col <- ceiling((x_coords - bbox["xmin"]) / 1000)
    
  #### 14.2. Convert CRS ####
    grid_data_detroit_NAD83 <- st_transform(grid_data_detroit, crs = st_crs("+proj=longlat +datum=NAD83"))
    
    grid_data_detroit_NAD83 %>%
      dplyr::select(-pixel_ID) -> grid_data_detroit_NAD83_benmap #This has all pixels
    
  #### 14.3. Crop to MSA ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US19820")-> cbsa_shapefile_detroit
    
    intersection_detroit_NAD83 <- st_intersection(grid_data_detroit_NAD83, cbsa_shapefile_detroit)
    detroit_pixels <- intersection_detroit_NAD83$pixel_ID #These are the pixels from my grid in the MSA
    
    grid_data_detroit_NAD83 %>%
      filter(pixel_ID %in% detroit_pixels) -> grid_data_detroit_NAD83_filtered
    
    st_write(grid_data_detroit_NAD83_filtered, "output_data/shapefiles/1km/Detroit/1km_shapefile_2019_03252024_detroit.shp")
    #Has total pixels 10,711 we use as grid in BenMAP. 
  
#### 15. SAN FRANCISCO ####
  #### 15.1. Load 1 km grid data ####
    grid_data_SF <- st_read("shapefiles/1km-12_sanfrancisco/fishnet_1km_12.shp") #Provided by Jing Cheng based on WRF-CMAQ resolution
    
    #Set grid numbers
    grid_data_SF$pixel_ID <- seq_along(grid_data_SF$geometry)
    grid_data_SF <- grid_data_SF[, c("geometry", "pixel_ID")]
    
    #Extract bounding box
    bbox <- st_bbox(grid_data_SF$geometry)
    
    #Calculate the number of columns and rows
    num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
    num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
    
    # Extract centroids from the geometry column
    centroids <- st_centroid(grid_data_SF$geometry)
    
    # Extract x and y coordinates from the centroid geometry
    x_coords <- st_coordinates(centroids)[, "X"]
    y_coords <- st_coordinates(centroids)[, "Y"]
    
    #Add rows and columns
    grid_data_SF$row <- num_rows - floor((y_coords - bbox["ymin"]) / 1000)
    grid_data_SF$col <- ceiling((x_coords - bbox["xmin"]) / 1000)
    
  #### 15.2. Convert CRS ####
    grid_data_SF_NAD83 <- st_transform(grid_data_SF, crs = st_crs("+proj=longlat +datum=NAD83"))
    
    grid_data_SF_NAD83 %>%
      dplyr::select(-pixel_ID) -> grid_data_SF_NAD83_benmap #This has all pixels
    
  #### 15.3. Crop to MSA ####
    cbsa_shapefile %>%
      filter(AFFGEOID == "310M500US41860")-> cbsa_shapefile_SF
    
    intersection_SF_NAD83 <- st_intersection(grid_data_SF_NAD83, cbsa_shapefile_SF)
    SF_pixels <- intersection_SF_NAD83$pixel_ID #These are the pixels from my grid in the MSA
    
    grid_data_SF_NAD83 %>%
      filter(pixel_ID %in% SF_pixels) -> grid_data_SF_NAD83_filtered
    
    st_write(grid_data_SF_NAD83_filtered, "output_data/shapefiles/1km/SF/1km_shapefile_2019_03252024_SF.shp")
    #Has total pixels 7,072 we use as grid in BenMAP. 
    
    
    
#------------------------------------------------- SECTION A2: 1 KM POPULATION -------------------------------------------------
#Here we re-scale population data by age from Census bureau at the census block group level to our 1km grids. We will use the same 1 km 2019 population grid for our 3 BenMAP runs.
#----GENERAL FILES----
#### 1. Load census data ####
    census_block <- st_read("shapefiles/cb_2019_us_bg_500k_clipped/cb_2019_us_bg_500k.shp") #From https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.2019.html#list-tab-1883739534
    
    input_population_data <- read.csv("data/BenMAP/ACSDT5Y2019.B01003_2024-02-28T212808/ACSDT5Y2019.B01003-Data.csv") #From census data table B01003 on total population at the census block level. Note we rename column to match census block metadata
    
    input_race_data <- read.csv("data/BenMAP/ACSDT5Y2019.B02001_2024-02-28T213117/ACSDT5Y2019.B02001-Data.csv")  #From census data table B02001 on race at the census block level. Note we rename column to match census block metadata
    
    input_gender_age_data <- read.csv("data/BenMAP/ACSDT5Y2019.B01001_2024-02-28T213030/ACSDT5Y2019.B01001-Data.csv")  #From census data table B01001 on race at the census block level. Note we rename column to match census block metadata
    
    input_income_data <-read.csv("data/BenMAP/ACSDT5Y2019.B19013_2024-02-28T213319/ACSDT5Y2019.B19013-Data.csv")  #From census data table B19013 on race at the census block level. Note we rename column to match census block metadata
    
#### 2. Prepare census data ####
  #### 2.1. Total population ####
    input_population_data %>%
      dplyr::select(GEO_ID, NAME, B01003_001E) %>%
      rename(census_block_ID = GEO_ID,
             population_in = B01003_001E) -> population_data
    
  #### 2.2. Race ####
    input_race_data %>%
      dplyr::select(GEO_ID, B02001_002E, B02001_003E, B02001_004E, 
                    B02001_005E, B02001_006E, B02001_007E, B02001_008E) %>%
      rename(census_block_ID = GEO_ID,
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
      rename(census_block_ID = GEO_ID) -> age_data
    
  #### 2.4. Income ####
    input_income_data %>% #In 2020 inflation-adjusted USD
      dplyr::select(GEO_ID, B19013_001E) %>%
      rename(census_block_ID = GEO_ID,
             median_house_income = B19013_001E) %>%
      mutate(median_house_income = if_else(median_house_income == "-", "", median_house_income)) %>% #Some missing values for income, NAs expected
      mutate(median_house_income = as.numeric(median_house_income))-> income_data

#### 3. Census block data ####
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
      left_join(population_data, by = "census_block_ID") %>% #total population
      filter(!state_ID %in% c(66, 69)) %>% #filter out Guam (66) and Northern Mariana Islands (69)
      left_join(race_data, by = c("census_block_ID")) %>% #race
      left_join(income_data, by = c("census_block_ID")) %>% #income
      left_join(age_data, by = c("census_block_ID")) -> census_block_variables_geometry
    
    census_block_variables_geometry %>%
      st_drop_geometry() -> census_block_variables

#---- REGIONAL FILES ----
#### 1. SEATTLE ####
  #### 1.1. Prepare census block and grids ####
    #Filter for WA
    census_block %>%
      filter(state_ID == "53")-> census_block_WA
    
    #Intersect census and MSA
    intersection_seattle_NAD83_bg <- st_intersection(census_block_WA, cbsa_shapefile_seattle)
    
    #Here we want to exclude census block groups that ar ein the borders, since they are not part of the MSA, even though the st_intersection function included them. This happens because we need to keep our pixels as squares (polygons), instead of irregular shapes (multipolygons). So by doing this the bordering pixels intersect with block groups outside our MSA
    #For Seattle the MSA has 3 counties: King (033), Pierce (053), and Snohomish (061)
    intersection_seattle_NAD83_bg %>%
      filter(grepl("53033", census_block_ID) |
             grepl("53053", census_block_ID) |
             grepl("53061", census_block_ID)) -> intersection_seattle_NAD83_bg
    
    seattle_blocks <- intersection_seattle_NAD83_bg$census_block_ID #These are the census block groups in the MSA
    
    #Calculate pixel area
    grid_data_Seattle_NAD83_filtered %>% 
      mutate(pixel_area = round(st_area(grid_data_Seattle_NAD83_filtered), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_Seattle_NAD83_filtered_area
    
  #### 1.2. Join our shapefiles (census data + 1km grid) ####
    # Here we join our grids to our census blocks to get number of partitions 
    #A partition is the piece that remains after joining the 9km grid to the census block
    #join pixels & blocks to create a list of all partitions
    system.time({joined_data_original_seattle <- st_join(grid_data_Seattle_NAD83_filtered_area, census_block_WA, join = st_intersects)}) #0.9 seconds
   
  #### 1.3. Create the function for calculating intersecting area ####
    # Function to calculate intersection area between pixels and blocks
    calculate_intersection_area <- function(pixel_ID, census_block_ID) {
      pixel_geometry <- st_geometry(grid_data_Seattle_NAD83_filtered_area[grid_data_Seattle_NAD83_filtered_area$pixel_ID == pixel_ID, ])
      block_geometry <- st_geometry(census_block_WA[census_block_WA$census_block_ID == census_block_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
      return(intersection_area)
    }

  #### 1.4. Run the function ####
    partitions_seattle <- joined_data_original_seattle
    
      timing_result <- system.time({
        partitions_seattle$intersection_area <- mapply(calculate_intersection_area, 
                                              pixel_ID = partitions_seattle$pixel_ID, 
                                              census_block_ID = partitions_seattle$census_block_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #389 = ~6.5 minutes

  #### 1.5. Outputs ####
    ### 1.5.1. Get area ratios ###
    partitions_seattle %>%
      filter(census_block_ID %in% seattle_blocks) -> partitions_seattle #Keep census blocks in Seattle MSA
        
    #This is how much of the census block is in the pixel, group by census block should equal 1
    partitions_seattle$area_ratio_block <- partitions_seattle$intersection_area / partitions_seattle$block_area
        
    #This is how much of the pixel is in each census block, group by pixel should equal 1
    partitions_seattle$area_ratio_pixel <- partitions_seattle$intersection_area / partitions_seattle$pixel_area
            #small number means the pixel has a lot of census blocks 
            #1 means the pixel is fully contained within a census block
    
    ### 1.5.2. Reset pixel area when needed###
      #NOTE: pixels that are on the border of city have an area not contained by the city, which affects later weighted averages
        #To fix this we set the pixel area for these pixels as the sum of the intersection area with the census blocks contained in that pixel, since the rest is ocean.
    partitions_seattle %>%
      group_by(pixel_ID) %>%
      summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
      ungroup() %>%
      st_drop_geometry()-> partitions_seattle_area_pixel
        
    partitions_seattle_area_pixel_border <- partitions_seattle_area_pixel$pixel_ID[partitions_seattle_area_pixel$sum_area_ratio_pixel < 0.99] #1167
        
        #Now we change the pixel_area for these 4043 pixels to be their intersection area with the census block, given that the rest is ocean
        for (i in seq_along(partitions_seattle$pixel_ID)) {
          current_pixel_ID <- partitions_seattle$pixel_ID[i]
          
          # Check if the current pixel_ID is in partitions_area_pixel_border
          if (current_pixel_ID %in% partitions_seattle_area_pixel_border) {
            
            # Filter partitions to get the rows matching the current pixel_ID
            matching_rows <- partitions_seattle[partitions_seattle$pixel_ID == current_pixel_ID, ]
            
            # Recalculate pixel_area as the sum of intersection_area for the matching rows
            new_pixel_area <- sum(matching_rows$intersection_area)
            
            # Update the pixel_area in the 'partitions' table
            partitions_seattle$pixel_area[i] <- new_pixel_area
        
          } 
        }

      #write csv
      partitions_seattle %>%
        st_drop_geometry() -> partitions_seattle_list
      write.csv(partitions_seattle_list, "output_data/BenMAP_files/1km/Seattle/partitions_seattle.csv")
      
    ### 1.5.3. Merge variables ###
        #Merge partitions with variables
        partitions_seattle %>%
          left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_seattle_complete
    
    ### 1.5.4. Calculate population totals per partition ###
        partitions_seattle_complete %>%
          mutate(Population = population_in * area_ratio_block,
                 WHITE = WHITE_in * area_ratio_block,
                 BLACK = BLACK_in * area_ratio_block,
                 NATAMER = NATAMER_in * area_ratio_block,
                 ASIAN = ASIAN_in * area_ratio_block,
                 Other = Other_in * area_ratio_block,
                 `0TO0` = `0_in` * area_ratio_block,
                 `1TO17` = `1TO17_in` * area_ratio_block,
                 `18TO24` = `18TO24_in` * area_ratio_block,
                 `25TO34` = `25TO34_in` * area_ratio_block,
                 `35TO44` = `35TO44_in` * area_ratio_block,
                 `45TO54` = `45TO54_in` * area_ratio_block,
                 `55TO64` = `55TO64_in` * area_ratio_block,
                 `65TO74` = `65TO74_in` * area_ratio_block,
                 `75TO84` = `75TO84_in` * area_ratio_block,
                 `85TO99` = `85TO99_in` * area_ratio_block) %>%
          rename(Row = row,
                 Column = col) %>%
          dplyr::select(pixel_ID, Row, Column, Population, 
                 WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, `18TO24`, `25TO34`,
                 `35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_seattle
        
    ### 1.5.5. Calculate population totals per pixel ###
        population_2019_data_seattle %>%
          group_by(pixel_ID, Row, Column) %>%
          summarise(Population = sum(Population),
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
          ungroup()-> population_2019_data_pixel_seattle
        
    ### 1.5.6. Population by age ###
        population_2019_data_pixel_seattle %>%
          st_drop_geometry() %>%
          dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
                 `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
          gather(AgeRange, Population, -Row, -Column) %>%
          mutate(Year = 2020,
                 Race = "ALL",
                 Ethnicity = "ALL",
                 Gender = "ALL")-> population_2019_data_pixel_age_final_seattle
        
  #### 1.6. Write outputs ####
        # Print the output table with today's date
        st_write(population_2019_data_pixel_seattle, "output_data/shapefiles/1km/Seattle/1_km_population_2019_seattle.shp") 
        #Has total pixels 16,364 we use as grid in BenMAP. 
        #File with geometry, row, col, pixel ID, ages and races
        
        write.csv(population_2019_data_pixel_age_final_seattle, row.names = FALSE,"output_data/BenMAP_files/1km/Seattle/1_km_population_2019_seattle.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
    
  #### 1.7. Figures ####
        #Figure
        population_2019_data_pixel_seattle %>%
          mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_seattle_log
        
        #Plot 1 km grid data
        p<- ggplot() +
          geom_sf(data = population_2019_data_pixel_seattle_log, aes(fill = log_pixel_population), color = NA, size = 0) +
          scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
          ggtitle("Seattle 1km Rescaled Population")
          #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_Seattle_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
          #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_Seattle_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
       
    
  #### 1.8. Checks ####
    #Output (Partitions)
      check_seattle_population_out <- round(sum(population_2019_data_pixel_seattle$Population)) #3,871,323
        
    #Area ratio sum
      partitions_seattle_complete %>%
        group_by(census_block_ID) %>%
        summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_seattle #should = 1

#### 2. LOS ANGELES ####
  #### 2.1. Prepare census block and grids ####
    #Filter for CA
    census_block %>%
      filter(state_ID == "06")-> census_block_CA
        
    #Intersect census and MSA
    intersection_LA_NAD83_bg <- st_intersection(census_block_CA, cbsa_shapefile_LA)
    
    #The intersection includes some bordering block groups that should be excluded
    #For Los Angeles the MSA has 2 counties: Los Angeles (037), Orange (059)
    intersection_LA_NAD83_bg %>%
      filter(grepl("06037", census_block_ID) |
             grepl("06059", census_block_ID)) -> intersection_LA_NAD83_bg
    
    LA_blocks <- intersection_LA_NAD83_bg$census_block_ID #These are the census block groups in the MSA
        
    #Calculate pixel area
      grid_data_south_west_NAD83_LA %>% 
        mutate(pixel_area = round(st_area(grid_data_south_west_NAD83_LA), digits = 5), #Area in m^2
               pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_south_west_NAD83_LA_area
      
  #### 2.2. Join our shapefiles (census data + 1km grid) ####
        # Here we join our grids to our census blocks to get number of partitions 
        #A partition is the piece that remains after joining the 9km grid to the census block
        #join pixels & blocks to create a list of all partitions
        joined_data_original_LA <- st_join(grid_data_south_west_NAD83_LA_area, census_block_CA, join = st_intersects)
        
  #### 2.3. Create the function for calculating intersecting area ####
        # Function to calculate intersection area between pixels and blocks
        calculate_intersection_area <- function(pixel_ID, census_block_ID) {
          pixel_geometry <- st_geometry(grid_data_south_west_NAD83_LA_area[grid_data_south_west_NAD83_LA_area$pixel_ID == pixel_ID, ])
          block_geometry <- st_geometry(census_block_CA[census_block_CA$census_block_ID == census_block_ID, ])
          
          intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
          return(intersection_area)
        }
        
  #### 2.4. Run the function ####
        partitions_LA <- joined_data_original_LA
        
        timing_result <- system.time({
          partitions_LA$intersection_area <- mapply(calculate_intersection_area, 
                                                      pixel_ID = partitions_LA$pixel_ID, 
                                                      census_block_ID = partitions_LA$census_block_ID)
        })
        cat("Elapsed Time:", timing_result["elapsed"], "\n") #537 = ~9.5 minutes
        
  #### 2.5. Outputs ####
    ### 2.5.1. Get area ratios ###
        partitions_LA %>%
          filter(census_block_ID %in% LA_blocks) -> partitions_LA #Keep census blocks in LA MSA. From 39,944 to 39,306
        
        #This is how much of the census block is in the pixel, group by census block should equal 1
        partitions_LA$area_ratio_block <- partitions_LA$intersection_area / partitions_LA$block_area
        
        #This is how much of the pixel is in each census block, group by pixel should equal 1
        partitions_LA$area_ratio_pixel <- partitions_LA$intersection_area / partitions_LA$pixel_area
        
    ### 2.5.2. Reset pixel area when needed###
        partitions_LA %>%
          group_by(pixel_ID) %>%
          summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
          ungroup() %>%
          st_drop_geometry()-> partitions_LA_area_pixel
        
        partitions_LA_area_pixel_border <- partitions_LA_area_pixel$pixel_ID[partitions_LA_area_pixel$sum_area_ratio_pixel < 0.99] #898
        
        #Now we change the pixel_area for these 4043 pixels to be their intersection area with the census block, given that the rest is ocean
        for (i in seq_along(partitions_LA$pixel_ID)) {
          current_pixel_ID <- partitions_LA$pixel_ID[i]
          
          # Check if the current pixel_ID is in partitions_area_pixel_border
          if (current_pixel_ID %in% partitions_LA_area_pixel_border) {
            
            # Filter partitions to get the rows matching the current pixel_ID
            matching_rows <- partitions_LA[partitions_LA$pixel_ID == current_pixel_ID, ]
            
            # Recalculate pixel_area as the sum of intersection_area for the matching rows
            new_pixel_area <- sum(matching_rows$intersection_area)
            
            # Update the pixel_area in the 'partitions' table
            partitions_LA$pixel_area[i] <- new_pixel_area
            
          } 
        }
        
        #write csv
        partitions_LA %>%
          st_drop_geometry() -> partitions_LA_list
        write.csv(partitions_LA_list, "output_data/BenMAP_files/1km/LA/partitions_LA.csv")
        
    ### 2.5.3. Merge variables ###
        #Merge partitions with variables
        partitions_LA %>%
          left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_LA_complete
        
    ### 2.5.4. Calculate population totals per partition ###
        partitions_LA_complete %>%
          mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
                 BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
                 ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
                 `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
                 `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
                 `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
                 `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
                 `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
          rename(Row = row, Column = col) %>%
          dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
                 `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_LA
        
    ### 2.5.5. Calculate population totals per pixel ###
        population_2019_data_LA %>%
          group_by(pixel_ID, Row, Column) %>%
          summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
                    ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
                    `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
                    `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
          ungroup()-> population_2019_data_pixel_LA
        
    ### 2.5.6. Population by age ###
        population_2019_data_pixel_LA %>%
          st_drop_geometry() %>%
          dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
                 `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
          gather(AgeRange, Population, -Row, -Column) %>%
          mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_LA
     
  #### 2.6. Write outputs ####   
        # Print the output table with today's date
        st_write(population_2019_data_pixel_LA, "output_data/shapefiles/1km/LA/1_km_population_2019_LA.shp") 
        #Has total pixels 16,097 we use as grid in BenMAP. 
        #File with geometry, row, col, pixel ID, ages and races
        
        write.csv(population_2019_data_pixel_age_final_LA, row.names = FALSE, "output_data/BenMAP_files/1km/LA/1_km_population_2019_LA.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
        
  #### 2.7. Figures ####
        #Figure
        population_2019_data_pixel_LA %>%
          mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_LA_log
        
        #Plot 1 km grid data
        p<- ggplot() +
          geom_sf(data = population_2019_data_pixel_LA_log, aes(fill = log_pixel_population), color = NA, size = 0) +
          scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
          ggtitle("LA 1km Rescaled Population")
        #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_LA_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
        #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_LA_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
        print(p)
        
  #### 2.8. Checks ####
        #Output (Partitions)
        check_LA_population_out <- round(sum(population_2019_data_pixel_LA$Population)) #13,249,614
        
    #Area ratio sum 
        partitions_LA_complete %>%
          group_by(census_block_ID) %>%
          summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_LA #should = 1

#### 3. RIVERSIDE ####
  #### 3.1. Prepare census block and grids ####
    #Intersect census and MSA
      intersection_riverside_NAD83_bg <- st_intersection(census_block_CA, cbsa_shapefile_riverside)
    
    #The intersection includes some bordering block groups that should be excluded
    #For Riverside the MSA has 2 counties: San Bernardino (071), Riverside (065)
      intersection_riverside_NAD83_bg %>%
      filter(grepl("06071", census_block_ID) |
               grepl("06065", census_block_ID)) -> intersection_riverside_NAD83_bg
    
    riverside_blocks <- intersection_riverside_NAD83_bg$census_block_ID #These are the census block groups in the MSA
    
    #Calculate pixel area
    grid_data_south_west_NAD83_riverside %>% 
      mutate(pixel_area = round(st_area(grid_data_south_west_NAD83_riverside), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_south_west_NAD83_riverside_area
  
  #### 3.2. Join our shapefiles (census data + 1km grid) ####
  joined_data_original_riverside <- st_join(grid_data_south_west_NAD83_riverside_area, census_block_CA, join = st_intersects)
  
  #### 3.3. Create the function for calculating intersecting area ####
  # Function to calculate intersection area between pixels and blocks
  calculate_intersection_area <- function(pixel_ID, census_block_ID) {
    pixel_geometry <- st_geometry(grid_data_south_west_NAD83_riverside_area[grid_data_south_west_NAD83_riverside_area$pixel_ID == pixel_ID, ])
    block_geometry <- st_geometry(census_block_CA[census_block_CA$census_block_ID == census_block_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #### 3.4. Run the function ####
  partitions_riverside <- joined_data_original_riverside
  
  timing_result <- system.time({
    partitions_riverside$intersection_area <- mapply(calculate_intersection_area, 
                                              pixel_ID = partitions_riverside$pixel_ID, 
                                              census_block_ID = partitions_riverside$census_block_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #1368 = ~23 minutes
  
  #### 3.5. Outputs ####
  ### 3.5.1. Get area ratios ###
  partitions_riverside %>%
    filter(census_block_ID %in% riverside_blocks) -> partitions_riverside #From 87,895 to 86,881

  #This is how much of the census block is in the pixel, group by census block should equal 1
  partitions_riverside$area_ratio_block <- partitions_riverside$intersection_area / partitions_riverside$block_area
  
  #This is how much of the pixel is in each census block, group by pixel should equal 1
  partitions_riverside$area_ratio_pixel <- partitions_riverside$intersection_area / partitions_riverside$pixel_area
  
  ### 3.5.2. Reset pixel area when needed###
  partitions_riverside %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_riverside_area_pixel
  
  partitions_riverside_area_pixel_border <- partitions_riverside_area_pixel$pixel_ID[partitions_riverside_area_pixel$sum_area_ratio_pixel < 0.99] #1306
  
  for (i in seq_along(partitions_riverside$pixel_ID)) {
    current_pixel_ID <- partitions_riverside$pixel_ID[i]
    
    # Check if the current pixel_ID is in partitions_area_pixel_border
    if (current_pixel_ID %in% partitions_riverside_area_pixel_border) {
      
      # Filter partitions to get the rows matching the current pixel_ID
      matching_rows <- partitions_riverside[partitions_riverside$pixel_ID == current_pixel_ID, ]
      
      # Recalculate pixel_area as the sum of intersection_area for the matching rows
      new_pixel_area <- sum(matching_rows$intersection_area)
      
      # Update the pixel_area in the 'partitions' table
      partitions_riverside$pixel_area[i] <- new_pixel_area
      
    } 
  }
  
  #write csv
  partitions_riverside %>%
    st_drop_geometry() -> partitions_riverside_list
  write.csv(partitions_riverside_list, "output_data/BenMAP_files/1km/Riverside/partitions_riverside.csv")
  
  ### 3.5.3. Merge variables ###
  #Merge partitions with variables
  partitions_riverside %>%
    left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_riverside_complete
  
  ### 3.5.4. Calculate population totals per partition ###
  partitions_riverside_complete %>%
    mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
           BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
           ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
           `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
           `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
           `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
           `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
           `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
    rename(Row = row, Column = col) %>%
    dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
           `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_riverside
  
  ### 3.5.5. Calculate population totals per pixel ###
  population_2019_data_riverside %>%
    group_by(pixel_ID, Row, Column) %>%
    summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
              ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
              `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
              `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
    ungroup()-> population_2019_data_pixel_riverside
  
  ### 3.5.6. Population by age ###
  population_2019_data_pixel_riverside %>%
    st_drop_geometry() %>%
    dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
           `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
    gather(AgeRange, Population, -Row, -Column) %>%
    mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_riverside
  
  #### 3.6. Write outputs ####   
  # Print the output table with today's date
  st_write(population_2019_data_pixel_riverside, "output_data/shapefiles/1km/Riverside/1_km_population_2019_riverside.shp") 
  #Has total pixels 71,352 we use as grid in BenMAP. 
  #File with geometry, row, col, pixel ID, ages and races
  
  write.csv(population_2019_data_pixel_age_final_riverside, row.names = FALSE, "output_data/BenMAP_files/1km/Riverside/1_km_population_2019_riverside.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
  
  #### 3.7. Figures ####
  #Figure
  population_2019_data_pixel_riverside %>%
    mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_riverside_log
  
  #Plot 1 km grid data
  p<- ggplot() +
    geom_sf(data = population_2019_data_pixel_riverside_log, aes(fill = log_pixel_population), color = NA, size = 0) +
    scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
    ggtitle("Riverside 1km Rescaled Population")
  #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_riverside_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
  #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_riverside_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
  print(p)
  
  #### 3.8. Checks ####
  #Output (Partitions)
  check_riverside_population_out <- round(sum(population_2019_data_pixel_riverside$Population)) #4,560,470
  
  #Area ratio sum 
  partitions_riverside_complete %>%
    group_by(census_block_ID) %>%
    summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_Riverside #should = 1

#### 4. PHOENIX ####
  #### 4.1. Prepare census block and grids ####
  census_block %>%
    filter(state_ID == "04")-> census_block_AZ
  
  #Intersect census and MSA
  intersection_phoenix_NAD83_bg <- st_intersection(census_block_AZ, cbsa_shapefile_phoenix)
  
  #The intersection includes some bordering block groups that should be excluded
  #For Phoenix the MSA has 2 counties: Maricopa (013), Pinal (021)
  intersection_phoenix_NAD83_bg %>%
    filter(grepl("04013", census_block_ID) |
             grepl("04021", census_block_ID)) -> intersection_phoenix_NAD83_bg
  
  phoenix_blocks <- intersection_phoenix_NAD83_bg$census_block_ID #These are the census block groups in the MSA
  
  #Calculate pixel area
  grid_data_south_west_NAD83_phoenix %>% 
    mutate(pixel_area = round(st_area(grid_data_south_west_NAD83_phoenix), digits = 5), #Area in m^2
           pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_south_west_NAD83_phoenix_area
  
  #### 4.2. Join our shapefiles (census data + 1km grid) ####
  joined_data_original_phoenix <- st_join(grid_data_south_west_NAD83_phoenix_area, census_block_AZ, join = st_intersects)
  
  #### 4.3. Create the function for calculating intersecting area ####
  calculate_intersection_area <- function(pixel_ID, census_block_ID) {
    pixel_geometry <- st_geometry(grid_data_south_west_NAD83_phoenix_area[grid_data_south_west_NAD83_phoenix_area$pixel_ID == pixel_ID, ])
    block_geometry <- st_geometry(census_block_AZ[census_block_AZ$census_block_ID == census_block_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #### 4.4. Run the function ####
  partitions_phoenix <- joined_data_original_phoenix
  
  timing_result <- system.time({
    partitions_phoenix$intersection_area <- mapply(calculate_intersection_area, 
                                                     pixel_ID = partitions_phoenix$pixel_ID, 
                                                     census_block_ID = partitions_phoenix$census_block_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #708s= ~12 minutes
  
  #### 4.5. Outputs ####
    ### 4.5.1. Get area ratios ###
      partitions_phoenix %>%
        filter(census_block_ID %in% phoenix_blocks) -> partitions_phoenix #From 53,867 to 52,730
      
      #This is how much of the census block is in the pixel, group by census block should equal 1
      partitions_phoenix$area_ratio_block <- partitions_phoenix$intersection_area / partitions_phoenix$block_area
      
      #This is how much of the pixel is in each census block, group by pixel should equal 1
      partitions_phoenix$area_ratio_pixel <- partitions_phoenix$intersection_area / partitions_phoenix$pixel_area
    
    ### 4.5.2. Reset pixel area when needed###
      partitions_phoenix %>%
        group_by(pixel_ID) %>%
        summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
        ungroup() %>%
        st_drop_geometry()-> partitions_phoenix_area_pixel
      
      partitions_phoenix_area_pixel_border <- partitions_phoenix_area_pixel$pixel_ID[partitions_phoenix_area_pixel$sum_area_ratio_pixel < 0.99] #1041
      
      #Now we change the pixel_area for these 4043 pixels to be their intersection area with the census block, given that the rest is ocean
      for (i in seq_along(partitions_phoenix$pixel_ID)) {
        current_pixel_ID <- partitions_phoenix$pixel_ID[i]
        
        # Check if the current pixel_ID is in partitions_area_pixel_border
        if (current_pixel_ID %in% partitions_phoenix_area_pixel_border) {
          
          # Filter partitions to get the rows matching the current pixel_ID
          matching_rows <- partitions_phoenix[partitions_phoenix$pixel_ID == current_pixel_ID, ]
          
          # Recalculate pixel_area as the sum of intersection_area for the matching rows
          new_pixel_area <- sum(matching_rows$intersection_area)
          
          # Update the pixel_area in the 'partitions' table
          partitions_phoenix$pixel_area[i] <- new_pixel_area
          
        } 
      }
      
      #write csv
      partitions_phoenix %>%
        st_drop_geometry() -> partitions_phoenix_list
      write.csv(partitions_phoenix_list, "output_data/BenMAP_files/1km/Phoenix/partitions_phoenix.csv")
      
    ### 4.5.3. Merge variables ###
      #Merge partitions with variables
      partitions_phoenix %>%
        left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_phoenix_complete
  
    ### 4.5.4. Calculate population totals per partition ###
      partitions_phoenix_complete %>%
        mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
               BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
               ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
               `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
               `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
               `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
               `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
               `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
        rename(Row = row, Column = col) %>%
        dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
               `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_phoenix
  
    ### 4.5.5. Calculate population totals per pixel ###
      population_2019_data_phoenix %>%
        group_by(pixel_ID, Row, Column) %>%
        summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
                  ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
                  `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
                  `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
        ungroup()-> population_2019_data_pixel_phoenix
  
    ### 4.5.6. Population by age ###
      population_2019_data_pixel_phoenix %>%
        st_drop_geometry() %>%
        dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
               `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
        gather(AgeRange, Population, -Row, -Column) %>%
        mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_phoenix
  
  #### 4.6. Write outputs ####   
    # Print the output table with today's date
    st_write(population_2019_data_pixel_phoenix, "output_data/shapefiles/1km/Phoenix/1_km_population_2019_phoenix.shp") 
    #Has total pixels 38,341 we use as grid in BenMAP. 
    #File with geometry, row, col, pixel ID, ages and races
    
    write.csv(population_2019_data_pixel_age_final_phoenix, row.names = FALSE, "output_data/BenMAP_files/1km/Phoenix/1_km_population_2019_phoenix.csv") #BenMAP file with row, col, age range, population, all races, all ethnicity, all gender
    
  #### 4.7. Figures ####
    #Figure
    population_2019_data_pixel_phoenix %>%
      mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_phoenix_log
    
    #Plot 1 km grid data
    p<- ggplot() +
      geom_sf(data = population_2019_data_pixel_phoenix_log, aes(fill = log_pixel_population), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("Phoenix 1km Rescaled Population")
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_phoenix_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_phoenix_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
  
  #### 4.8. Checks ####
    #Output (Partitions)
    check_phoenix_population_out <- round(sum(population_2019_data_pixel_phoenix$Population)) #4,761,603
    
    #Area ratio sum 
    partitions_phoenix_complete %>%
      group_by(census_block_ID) %>%
      summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_phoenix #should = 1

#### 5. DALLAS ####
  #### 5.1. Prepare census block and grids ####
    census_block %>%
      filter(state_ID == "48")-> census_block_TX
    
    #Intersect census and MSA
    intersection_dallas_NAD83_bg <- st_intersection(census_block_TX, cbsa_shapefile_dallas)
    
    #The intersection includes some bordering block groups that should be excluded
    #For Dallas the MSA has 11 counties: Wise (497), Denton (121), Collin (085), Hunt (231), Parker (367),
    #Tarrant (439), Dallas (113), Rockwall (397), Johnson (251), Ellis (139), Kaufman (257)
    intersection_dallas_NAD83_bg %>%
      filter(grepl("48497", census_block_ID) | grepl("48121", census_block_ID) | grepl("48085", census_block_ID) |
               grepl("48231", census_block_ID) | grepl("48367", census_block_ID) | grepl("48439", census_block_ID) |
               grepl("48113", census_block_ID) | grepl("48397", census_block_ID) | grepl("48251", census_block_ID) |
               grepl("48139", census_block_ID) | grepl("48257", census_block_ID)) -> intersection_dallas_NAD83_bg
    
    dallas_blocks <- intersection_dallas_NAD83_bg$census_block_ID #These are the census block groups in the MSA
    
    #Calculate pixel area
    grid_data_dallas_NAD83_filtered %>% 
      mutate(pixel_area = round(st_area(grid_data_dallas_NAD83_filtered), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_dallas_NAD83_filtered_area
  
  #### 5.2. Join our shapefiles (census data + 1km grid) ####
    joined_data_original_dallas <- st_join(grid_data_dallas_NAD83_filtered_area, census_block_TX, join = st_intersects)
  
  #### 5.3. Create the function for calculating intersecting area ####
    # Function to calculate intersection area between pixels and blocks
    calculate_intersection_area <- function(pixel_ID, census_block_ID) {
      pixel_geometry <- st_geometry(grid_data_dallas_NAD83_filtered_area[grid_data_dallas_NAD83_filtered_area$pixel_ID == pixel_ID, ])
      block_geometry <- st_geometry(census_block_TX[census_block_TX$census_block_ID == census_block_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
      return(intersection_area)
    }
  
  #### 5.4. Run the function ####
    partitions_dallas <- joined_data_original_dallas
    
    timing_result <- system.time({
      partitions_dallas$intersection_area <- mapply(calculate_intersection_area, 
                                                     pixel_ID = partitions_dallas$pixel_ID, 
                                                     census_block_ID = partitions_dallas$census_block_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #621s= ~10 minutes
  
  #### 5.5. Outputs ####
    ### 5.5.1. Get area ratios ###
      partitions_dallas %>%
        filter(census_block_ID %in% dallas_blocks) -> partitions_dallas #From 46,053 to 45,171
      
      #This is how much of the census block is in the pixel, group by census block should equal 1
      partitions_dallas$area_ratio_block <- partitions_dallas$intersection_area / partitions_dallas$block_area
      
      #This is how much of the pixel is in each census block, group by pixel should equal 1
      partitions_dallas$area_ratio_pixel <- partitions_dallas$intersection_area / partitions_dallas$pixel_area
      
    ### 5.5.2. Reset pixel area when needed###
      partitions_dallas %>%
        group_by(pixel_ID) %>%
        summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
        ungroup() %>%
        st_drop_geometry()-> partitions_dallas_area_pixel
      
      partitions_dallas_area_pixel_border <- partitions_dallas_area_pixel$pixel_ID[partitions_dallas_area_pixel$sum_area_ratio_pixel < 0.99] #772
      
      for (i in seq_along(partitions_dallas$pixel_ID)) {
        current_pixel_ID <- partitions_dallas$pixel_ID[i]
        
        # Check if the current pixel_ID is in partitions_area_pixel_border
        if (current_pixel_ID %in% partitions_dallas_area_pixel_border) {
          
          # Filter partitions to get the rows matching the current pixel_ID
          matching_rows <- partitions_dallas[partitions_dallas$pixel_ID == current_pixel_ID, ]
          
          # Recalculate pixel_area as the sum of intersection_area for the matching rows
          new_pixel_area <- sum(matching_rows$intersection_area)
          
          # Update the pixel_area in the 'partitions' table
          partitions_dallas$pixel_area[i] <- new_pixel_area
          
        } 
      }
      
      #write csv
      partitions_dallas %>%
        st_drop_geometry() -> partitions_dallas_list
      write.csv(partitions_dallas_list, "output_data/BenMAP_files/1km/Dallas/partitions_dallas.csv")
      
    ### 5.5.3. Merge variables ###
      #Merge partitions with variables
      partitions_dallas %>%
        left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_dallas_complete
      
    ### 5.5.4. Calculate population totals per partition ###
      partitions_dallas_complete %>%
        mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
               BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
               ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
               `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
               `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
               `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
               `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
               `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
        rename(Row = row, Column = col) %>%
        dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
               `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_dallas
      
    ### 5.5.5. Calculate population totals per pixel ###
      population_2019_data_dallas %>%
        group_by(pixel_ID, Row, Column) %>%
        summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
                  ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
                  `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
                  `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
        ungroup()-> population_2019_data_pixel_dallas
      
    ### 5.5.6. Population by age ###
      population_2019_data_pixel_dallas %>%
        st_drop_geometry() %>%
        dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
               `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
        gather(AgeRange, Population, -Row, -Column) %>%
        mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_dallas
  
  #### 5.6. Write outputs ####   
    # Print the output table with today's date
    st_write(population_2019_data_pixel_dallas, "output_data/shapefiles/1km/Dallas/1_km_population_2019_dallas.shp") 
    #Has total pixels 23,747 we use as grid in BenMAP. 
    #File with geometry, row, col, pixel ID, ages and races
    
    write.csv(population_2019_data_pixel_age_final_dallas, row.names = FALSE, "output_data/BenMAP_files/1km/Dallas/1_km_population_2019_dallas.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
    
  #### 5.7. Figures ####
    #Figure
    population_2019_data_pixel_dallas %>%
      mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_dallas_log
    
    #Plot 1 km grid data
    p<- ggplot() +
      geom_sf(data = population_2019_data_pixel_dallas_log, aes(fill = log_pixel_population), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("Dallas 1km Rescaled Population")
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_dallas_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_dallas_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
  
  #### 5.8. Checks ####
    #Output (Partitions)
    check_dallas_population_out <- round(sum(population_2019_data_pixel_dallas$Population)) #7,320,663
    
    #Area ratio sum 
    partitions_dallas_complete %>%
      group_by(census_block_ID) %>%
      summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_dallas #should = 1  

#### 6. HOUSTON ####
  #### 6.1. Prepare census block and grids ####
    #Intersect census and MSA
    intersection_houston_NAD83_bg <- st_intersection(census_block_TX, cbsa_shapefile_houston)
    
    #The intersection includes some bordering block groups that should be excluded
    #For Houston the MSA has 9 counties: Brazoria (039), Fort Bend (157), Austin (015),
    #Galveston (167), Harris (201), Waller (473), Chambers (071), Liberty (291),
    #Montgomery (339)
    intersection_houston_NAD83_bg %>%
      filter(grepl("48039", census_block_ID) | grepl("48157", census_block_ID) | grepl("48015", census_block_ID) |
               grepl("48167", census_block_ID) | grepl("48201", census_block_ID) | grepl("48473", census_block_ID) |
               grepl("48071", census_block_ID) | grepl("48291", census_block_ID) | grepl("48339", census_block_ID)) -> intersection_houston_NAD83_bg
    
    houston_blocks <- intersection_houston_NAD83_bg$census_block_ID #These are the census block groups in the MSA
    
    #Calculate pixel area
    grid_data_houston_NAD83_filtered %>% 
      mutate(pixel_area = round(st_area(grid_data_houston_NAD83_filtered), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_houston_NAD83_filtered_area
    
  #### 6.2. Join our shapefiles (census data + 1km grid) ####
    joined_data_original_houston <- st_join(grid_data_houston_NAD83_filtered_area, census_block_TX, join = st_intersects)
  
  #### 6.3. Create the function for calculating intersecting area ####
    calculate_intersection_area <- function(pixel_ID, census_block_ID) {
      pixel_geometry <- st_geometry(grid_data_houston_NAD83_filtered_area[grid_data_houston_NAD83_filtered_area$pixel_ID == pixel_ID, ])
      block_geometry <- st_geometry(census_block_TX[census_block_TX$census_block_ID == census_block_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
      return(intersection_area)
    }
  
  #### 6.4. Run the function ####
    partitions_houston <- joined_data_original_houston
    
    timing_result <- system.time({
      partitions_houston$intersection_area <- mapply(calculate_intersection_area, 
                                                    pixel_ID = partitions_houston$pixel_ID, 
                                                    census_block_ID = partitions_houston$census_block_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #578s= ~9.6 minutes
  
  #### 6.5. Outputs ####
    ### 6.5.1. Get area ratios ###
      partitions_houston %>%
        filter(census_block_ID %in% houston_blocks) -> partitions_houston #From 42,116 to 41,248
      
      #This is how much of the census block is in the pixel, group by census block should equal 1
      partitions_houston$area_ratio_block <- partitions_houston$intersection_area / partitions_houston$block_area
      
      #This is how much of the pixel is in each census block, group by pixel should equal 1
      partitions_houston$area_ratio_pixel <- partitions_houston$intersection_area / partitions_houston$pixel_area
      
    ### 6.5.2. Reset pixel area when needed###
      partitions_houston %>%
        group_by(pixel_ID) %>%
        summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
        ungroup() %>%
        st_drop_geometry()-> partitions_houston_area_pixel
      
      partitions_houston_area_pixel_border <- partitions_houston_area_pixel$pixel_ID[partitions_houston_area_pixel$sum_area_ratio_pixel < 0.99] #1405
      
      for (i in seq_along(partitions_houston$pixel_ID)) {
        current_pixel_ID <- partitions_houston$pixel_ID[i]
        
        # Check if the current pixel_ID is in partitions_area_pixel_border
        if (current_pixel_ID %in% partitions_houston_area_pixel_border) {
          
          # Filter partitions to get the rows matching the current pixel_ID
          matching_rows <- partitions_houston[partitions_houston$pixel_ID == current_pixel_ID, ]
          
          # Recalculate pixel_area as the sum of intersection_area for the matching rows
          new_pixel_area <- sum(matching_rows$intersection_area)
          
          # Update the pixel_area in the 'partitions' table
          partitions_houston$pixel_area[i] <- new_pixel_area
          
        } 
      }
      
      #write csv
      partitions_houston %>%
        st_drop_geometry() -> partitions_houston_list
      write.csv(partitions_houston_list, "output_data/BenMAP_files/1km/Houston/partitions_houston.csv")
      
    ### 6.5.3. Merge variables ###
      #Merge partitions with variables
      partitions_houston %>%
        left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_houston_complete
      
    ### 6.5.4. Calculate population totals per partition ###
      partitions_houston_complete %>%
        mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
               BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
               ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
               `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
               `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
               `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
               `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
               `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
        rename(Row = row, Column = col) %>%
        dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
               `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_houston
      
    ### 6.5.5. Calculate population totals per pixel ###
      population_2019_data_houston %>%
        group_by(pixel_ID, Row, Column) %>%
        summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
                  ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
                  `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
                  `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
        ungroup()-> population_2019_data_pixel_houston
      
    ### 6.5.6. Population by age ###
      population_2019_data_pixel_houston %>%
        st_drop_geometry() %>%
        dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
               `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
        gather(AgeRange, Population, -Row, -Column) %>%
        mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_houston
  
  #### 6.6. Write outputs ####   
    # Print the output table with today's date
    st_write(population_2019_data_pixel_houston, "output_data/shapefiles/1km/Houston/1_km_population_2019_houston.shp") 
    #Has total pixels 23,340 we use as grid in BenMAP. 
    #File with geometry, row, col, pixel ID, ages and races
    
    write.csv(population_2019_data_pixel_age_final_houston, row.names = FALSE, "output_data/BenMAP_files/1km/Houston/1_km_population_2019_houston.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
  
  #### 6.7. Figures ####
    #Figure
    population_2019_data_pixel_houston %>%
      mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_houston_log
    
    #Plot 1 km grid data
    p<- ggplot() +
      geom_sf(data = population_2019_data_pixel_houston_log, aes(fill = log_pixel_population), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("Houston 1km Rescaled Population")
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_houston_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_houston_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
  
  #### 6.8. Checks ####
    #Output (Partitions)
    check_houston_population_out <- round(sum(population_2019_data_pixel_houston$Population)) #6,884,138
    
    #Area ratio sum 
    partitions_houston_complete %>%
      group_by(census_block_ID) %>%
      summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_houston #should = 1 
  
#### 7. MIAMI ####
  #### 7.1. Prepare census block and grids ####
    census_block %>%
      filter(state_ID == "12")-> census_block_FL
    
    #Intersect census and MSA
    intersection_miami_NAD83_bg <- st_intersection(census_block_FL, cbsa_shapefile_miami)
    
    #The intersection includes some bordering block groups that should be excluded
    #For Miami the MSA has 3 counties: Palm Beach (099), Broward (011), Miami-Dade (086)
    intersection_miami_NAD83_bg %>%
      filter(grepl("12099", census_block_ID) | grepl("12011", census_block_ID) | grepl("12086", census_block_ID)) -> intersection_miami_NAD83_bg
    
    miami_blocks <- intersection_miami_NAD83_bg$census_block_ID #These are the census block groups in the MSA
    
    #Calculate pixel area
    grid_data_miami_NAD83_filtered %>% 
      mutate(pixel_area = round(st_area(grid_data_miami_NAD83_filtered), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_miami_NAD83_filtered_area
    
  #### 7.2. Join our shapefiles (census data + 1km grid) ####
    joined_data_original_miami <- st_join(grid_data_miami_NAD83_filtered_area, census_block_FL, join = st_intersects)
  
  #### 7.3. Create the function for calculating intersecting area ####
    calculate_intersection_area <- function(pixel_ID, census_block_ID) {
      pixel_geometry <- st_geometry(grid_data_miami_NAD83_filtered_area[grid_data_miami_NAD83_filtered_area$pixel_ID == pixel_ID, ])
      block_geometry <- st_geometry(census_block_FL[census_block_FL$census_block_ID == census_block_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
      return(intersection_area)
    }
  
  #### 7.4. Run the function ####
    partitions_miami <- joined_data_original_miami
    
    timing_result <- system.time({
      partitions_miami$intersection_area <- mapply(calculate_intersection_area, 
                                                     pixel_ID = partitions_miami$pixel_ID, 
                                                     census_block_ID = partitions_miami$census_block_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #372 = ~6.2 minutes
  
  #### 7.5. Outputs ####
    ### 7.5.1. Get area ratios ###
      partitions_miami %>%
        filter(census_block_ID %in% miami_blocks) -> partitions_miami #From 28,505 to 28,115
      
      #This is how much of the census block is in the pixel, group by census block should equal 1
      partitions_miami$area_ratio_block <- partitions_miami$intersection_area / partitions_miami$block_area
      
      #This is how much of the pixel is in each census block, group by pixel should equal 1
      partitions_miami$area_ratio_pixel <- partitions_miami$intersection_area / partitions_miami$pixel_area
      
    ### 7.5.2. Reset pixel area when needed###
      partitions_miami %>%
        group_by(pixel_ID) %>%
        summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
        ungroup() %>%
        st_drop_geometry()-> partitions_miami_area_pixel
      
      partitions_miami_area_pixel_border <- partitions_miami_area_pixel$pixel_ID[partitions_miami_area_pixel$sum_area_ratio_pixel < 0.99] #762
      
      for (i in seq_along(partitions_miami$pixel_ID)) {
        current_pixel_ID <- partitions_miami$pixel_ID[i]
        
        # Check if the current pixel_ID is in partitions_area_pixel_border
        if (current_pixel_ID %in% partitions_miami_area_pixel_border) {
          
          # Filter partitions to get the rows matching the current pixel_ID
          matching_rows <- partitions_miami[partitions_miami$pixel_ID == current_pixel_ID, ]
          
          # Recalculate pixel_area as the sum of intersection_area for the matching rows
          new_pixel_area <- sum(matching_rows$intersection_area)
          
          # Update the pixel_area in the 'partitions' table
          partitions_miami$pixel_area[i] <- new_pixel_area
          
        } 
      }
      
      #write csv
      partitions_miami %>%
        st_drop_geometry() -> partitions_miami_list
      write.csv(partitions_miami_list, "output_data/BenMAP_files/1km/Miami/partitions_miami.csv")
      
    ### 7.5.3. Merge variables ###
      #Merge partitions with variables
      partitions_miami %>%
        left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_miami_complete
      
    ### 7.5.4. Calculate population totals per partition ###
      partitions_miami_complete %>%
        mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
               BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
               ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
               `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
               `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
               `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
               `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
               `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
        rename(Row = row, Column = col) %>%
        dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
               `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_miami
      
    ### 7.5.5. Calculate population totals per pixel ###
      population_2019_data_miami %>%
        group_by(pixel_ID, Row, Column) %>%
        summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
                  ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
                  `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
                  `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
        ungroup()-> population_2019_data_pixel_miami
      
    ### 7.5.6. Population by age ###
      population_2019_data_pixel_miami %>%
        st_drop_geometry() %>%
        dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
               `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
        gather(AgeRange, Population, -Row, -Column) %>%
        mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_miami
  
  #### 7.6. Write outputs ####   
    # Print the output table with today's date
    st_write(population_2019_data_pixel_miami, "output_data/shapefiles/1km/Miami/1_km_population_2019_miami.shp") 
    #Has total pixels 15,126we use as grid in BenMAP. 
    #File with geometry, row, col, pixel ID, ages and races
    
    write.csv(population_2019_data_pixel_age_final_miami, row.names = FALSE, "output_data/BenMAP_files/1km/Miami/1_km_population_2019_miami.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
    
  #### 7.7. Figures ####
    #Figure
    population_2019_data_pixel_miami %>%
      mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_miami_log
    
    #Plot 1 km grid data
    p<- ggplot() +
      geom_sf(data = population_2019_data_pixel_miami_log, aes(fill = log_pixel_population), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("Miami 1km Rescaled Population")
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_miami_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_miami_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
  
  #### 7.8. Checks ####
    #Output (Partitions)
    check_miami_population_out <- round(sum(population_2019_data_pixel_miami$Population)) #6,090,660
    
    #Area ratio sum 
    partitions_miami_complete %>%
      group_by(census_block_ID) %>%
      summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_miami #should = 1 

#### 8. ATLANTA ####
  #### 8.1. Prepare census block and grids ####
    census_block %>%
      filter(state_ID == "13")-> census_block_GA
    
    #Intersect census and MSA
    intersection_atl_NAD83_bg <- st_intersection(census_block_GA, cbsa_shapefile_atl)
    
    #The intersection includes some bordering block groups that should be excluded
    #For Atlanta the MSA has 29 counties: Haralson (143), Carroll (045), Heard (149), Bartow (015), Paulding (223), Douglas (097),
    #Coweta (077), Meriwether (199), Pickens (227), Cherokee (057), Cobb (067), Fulton (121), Fayette (113), Spalding (255), 
    #Pike (231), Dawson (085), Forsyth (117), Gwinnett (135), DeKalb (089), Clayton (063),  Henry (151), Butts (035), Lamar (171), 
    #Rockdale (247), Barrow (013), Walton (297), Newton (217), Morgan (211), Jasper (159)
    intersection_atl_NAD83_bg %>%
      filter(grepl("13143", census_block_ID) | grepl("13045", census_block_ID) | grepl("13149", census_block_ID) |
             grepl("13015", census_block_ID) | grepl("13223", census_block_ID) | grepl("13097", census_block_ID) |
             grepl("13077", census_block_ID) | grepl("13199", census_block_ID) | grepl("13227", census_block_ID) |
             grepl("13057", census_block_ID) | grepl("13067", census_block_ID) | grepl("13121", census_block_ID) |
             grepl("13113", census_block_ID) | grepl("13255", census_block_ID) | grepl("13231", census_block_ID) |
             grepl("13085", census_block_ID) | grepl("13117", census_block_ID) | grepl("13135", census_block_ID) |
             grepl("13089", census_block_ID) | grepl("13063", census_block_ID) | grepl("13151", census_block_ID) |
             grepl("13035", census_block_ID) | grepl("13171", census_block_ID) | grepl("13247", census_block_ID) |
             grepl("13013", census_block_ID) | grepl("13297", census_block_ID) | grepl("13217", census_block_ID) |
             grepl("13211", census_block_ID) | grepl("13159", census_block_ID)) -> intersection_atl_NAD83_bg
    
    atl_blocks <- intersection_atl_NAD83_bg$census_block_ID #These are the census block groups in the MSA
    
    #Calculate pixel area
    grid_data_atl_NAD83_filtered %>% 
      mutate(pixel_area = round(st_area(grid_data_atl_NAD83_filtered), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_atl_NAD83_filtered_area
  
  #### 8.2. Join our shapefiles (census data + 1km grid) ####
    joined_data_original_atl <- st_join(grid_data_atl_NAD83_filtered_area, census_block_GA, join = st_intersects)
  
  #### 8.3. Create the function for calculating intersecting area ####
    calculate_intersection_area <- function(pixel_ID, census_block_ID) {
      pixel_geometry <- st_geometry(grid_data_atl_NAD83_filtered_area[grid_data_atl_NAD83_filtered_area$pixel_ID == pixel_ID, ])
      block_geometry <- st_geometry(census_block_GA[census_block_GA$census_block_ID == census_block_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
      return(intersection_area)
    }
  
  #### 8.4. Run the function ####
    partitions_atl <- joined_data_original_atl
    
    timing_result <- system.time({
      partitions_atl$intersection_area <- mapply(calculate_intersection_area, 
                                                   pixel_ID = partitions_atl$pixel_ID, 
                                                   census_block_ID = partitions_atl$census_block_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #610 = ~10 minutes
    
  #### 8.5. Outputs ####
    ### 8.5.1. Get area ratios ###
      partitions_atl %>%
        filter(census_block_ID %in% atl_blocks) -> partitions_atl 
      
      #This is how much of the census block is in the pixel, group by census block should equal 1
      partitions_atl$area_ratio_block <- partitions_atl$intersection_area / partitions_atl$block_area
      
      #This is how much of the pixel is in each census block, group by pixel should equal 1
      partitions_atl$area_ratio_pixel <- partitions_atl$intersection_area / partitions_atl$pixel_area
      
    ### 8.5.2. Reset pixel area when needed###
      partitions_atl %>%
        group_by(pixel_ID) %>%
        summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
        ungroup() %>%
        st_drop_geometry()-> partitions_atl_area_pixel
      
      partitions_atl_area_pixel_border <- partitions_atl_area_pixel$pixel_ID[partitions_atl_area_pixel$sum_area_ratio_pixel < 0.99] #971
      
      for (i in seq_along(partitions_atl$pixel_ID)) {
        current_pixel_ID <- partitions_atl$pixel_ID[i]
        
        # Check if the current pixel_ID is in partitions_area_pixel_border
        if (current_pixel_ID %in% partitions_atl_area_pixel_border) {
          
          # Filter partitions to get the rows matching the current pixel_ID
          matching_rows <- partitions_atl[partitions_atl$pixel_ID == current_pixel_ID, ]
          
          # Recalculate pixel_area as the sum of intersection_area for the matching rows
          new_pixel_area <- sum(matching_rows$intersection_area)
          
          # Update the pixel_area in the 'partitions' table
          partitions_atl$pixel_area[i] <- new_pixel_area
          
        } 
      }
      
      #write csv
      partitions_atl %>%
        st_drop_geometry() -> partitions_atl_list
      write.csv(partitions_atl_list, "output_data/BenMAP_files/1km/Atlanta/partitions_Atlanta.csv")
      
    ### 8.5.3. Merge variables ###
      #Merge partitions with variables
      partitions_atl %>%
        left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_atl_complete
      
    ### 8.5.4. Calculate population totals per partition ###
      partitions_atl_complete %>%
        mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
               BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
               ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
               `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
               `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
               `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
               `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
               `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
        rename(Row = row, Column = col) %>%
        dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
               `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_atl
      
    ### 8.5.5. Calculate population totals per pixel ###
      population_2019_data_atl %>%
        group_by(pixel_ID, Row, Column) %>%
        summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
                  ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
                  `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
                  `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
        ungroup()-> population_2019_data_pixel_atl
      
    ### 8.5.6. Population by age ###
      population_2019_data_pixel_atl %>%
        st_drop_geometry() %>%
        dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
               `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
        gather(AgeRange, Population, -Row, -Column) %>%
        mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_atl
  
  #### 8.6. Write outputs ####   
    # Print the output table with today's date
    st_write(population_2019_data_pixel_atl, "output_data/shapefiles/1km/Atlanta/1_km_population_2019_atlanta.shp") 
    #Has total pixels 23,341 we use as grid in BenMAP. 
    #File with geometry, row, col, pixel ID, ages and races
    
    write.csv(population_2019_data_pixel_age_final_atl, row.names = FALSE, "output_data/BenMAP_files/1km/Atlanta/1_km_population_2019_atlanta.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
  
  #### 8.7. Figures ####
    #Figure
    population_2019_data_pixel_atl %>%
      mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_atl_log
    
    #Plot 1 km grid data
    p<- ggplot() +
      geom_sf(data = population_2019_data_pixel_atl_log, aes(fill = log_pixel_population), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("Atlanta 1km Rescaled Population")
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_atlanta_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_atlanta_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
  
  #### 8.8. Checks ####
    #Output (Partitions)
    check_atl_population_out <- round(sum(population_2019_data_pixel_atl$Population)) #5,862,424
    
    #Area ratio sum 
    partitions_atl_complete %>%
      group_by(census_block_ID) %>%
      summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_atl #should = 1 
  
#### 9. CHICAGO ####
  #### 9.1. Prepare census block and grids ####
    census_block %>%
      filter(state_ID %in% c("17", "18", "55"))-> census_block_IL
    
    #Intersect census and MSA
    intersection_chicago_NAD83_bg <- st_intersection(census_block_IL, cbsa_shapefile_chicago)
    
    #The intersection includes some bordering block groups that should be excluded
    #For Chicago the MSA has 14 counties, 3 states: Lake (17097), Will (17197), Cook (17031), DuPage (17043), Kendall (17093), 
    #Kane (17089), DeKalb (17037), Grundy (17063), McHenry (17111); 
    #Jasper (18073), Porter (18127), Newton (18111), Lake (18089); 
    #Kenosha (55059), 
    intersection_chicago_NAD83_bg %>%
      filter(grepl("17097", census_block_ID) | grepl("17197", census_block_ID) | grepl("17031", census_block_ID) |
               grepl("17043", census_block_ID) | grepl("17093", census_block_ID) | grepl("17089", census_block_ID) |
               grepl("17037", census_block_ID) | grepl("17063", census_block_ID) | grepl("17111", census_block_ID) |
               grepl("18073", census_block_ID) | grepl("18127", census_block_ID) | grepl("18111", census_block_ID) |
               grepl("18089", census_block_ID) | grepl("55059", census_block_ID)) -> intersection_chicago_NAD83_bg
    
    chicago_blocks <- intersection_chicago_NAD83_bg$census_block_ID #These are the census block groups in the MSA
    
    #Calculate pixel area
    grid_data_chicago_NAD83_filtered %>% 
      mutate(pixel_area = round(st_area(grid_data_chicago_NAD83_filtered), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_chicago_NAD83_filtered_area
  
  #### 9.2. Join our shapefiles (census data + 1km grid) ####
    joined_data_original_chicago <- st_join(grid_data_chicago_NAD83_filtered_area, census_block_IL, join = st_intersects)
  
  #### 9.3. Create the function for calculating intersecting area ####
    calculate_intersection_area <- function(pixel_ID, census_block_ID) {
      pixel_geometry <- st_geometry(grid_data_chicago_NAD83_filtered_area[grid_data_chicago_NAD83_filtered_area$pixel_ID == pixel_ID, ])
      block_geometry <- st_geometry(census_block_IL[census_block_IL$census_block_ID == census_block_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
      return(intersection_area)
    }
  
  #### 9.4. Run the function ####
    partitions_chicago <- joined_data_original_chicago
    
    timing_result <- system.time({
      partitions_chicago$intersection_area <- mapply(calculate_intersection_area, 
                                                 pixel_ID = partitions_chicago$pixel_ID, 
                                                 census_block_ID = partitions_chicago$census_block_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #615 = ~10 minutes
  
  #### 9.5. Outputs ####
    ### 9.5.1. Get area ratios ###
      partitions_chicago %>%
        filter(census_block_ID %in% chicago_blocks) -> partitions_chicago #From 44,845 to 44,049
      
      #This is how much of the census block is in the pixel, group by census block should equal 1
      partitions_chicago$area_ratio_block <- partitions_chicago$intersection_area / partitions_chicago$block_area
      
      #This is how much of the pixel is in each census block, group by pixel should equal 1
      partitions_chicago$area_ratio_pixel <- partitions_chicago$intersection_area / partitions_chicago$pixel_area
      
    ### 9.5.2. Reset pixel area when needed###
      partitions_chicago %>%
        group_by(pixel_ID) %>%
        summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
        ungroup() %>%
        st_drop_geometry()-> partitions_chicago_area_pixel
      
      partitions_chicago_area_pixel_border <- partitions_chicago_area_pixel$pixel_ID[partitions_chicago_area_pixel$sum_area_ratio_pixel < 0.99] #872
      
      for (i in seq_along(partitions_chicago$pixel_ID)) {
        current_pixel_ID <- partitions_chicago$pixel_ID[i]
        
        # Check if the current pixel_ID is in partitions_area_pixel_border
        if (current_pixel_ID %in% partitions_chicago_area_pixel_border) {
          
          # Filter partitions to get the rows matching the current pixel_ID
          matching_rows <- partitions_chicago[partitions_chicago$pixel_ID == current_pixel_ID, ]
          
          # Recalculate pixel_area as the sum of intersection_area for the matching rows
          new_pixel_area <- sum(matching_rows$intersection_area)
          
          # Update the pixel_area in the 'partitions' table
          partitions_chicago$pixel_area[i] <- new_pixel_area
          
        } 
      }
      
      #write csv
      partitions_chicago %>%
        st_drop_geometry() -> partitions_chicago_list
      write.csv(partitions_chicago_list, "output_data/BenMAP_files/1km/Chicago/partitions_Chicago.csv")
      
    ### 9.5.3. Merge variables ###
      #Merge partitions with variables
      partitions_chicago %>%
        left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_chicago_complete
      
    ### 9.5.4. Calculate population totals per partition ###
      partitions_chicago_complete %>%
        mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
               BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
               ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
               `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
               `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
               `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
               `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
               `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
        rename(Row = row, Column = col) %>%
        dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
               `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_chicago
      
    ### 9.5.5. Calculate population totals per pixel ###
      population_2019_data_chicago %>%
        group_by(pixel_ID, Row, Column) %>%
        summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
                  ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
                  `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
                  `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
        ungroup()-> population_2019_data_pixel_chicago
      
    ### 9.5.6. Population by age ###
      population_2019_data_pixel_chicago %>%
        st_drop_geometry() %>%
        dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
               `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
        gather(AgeRange, Population, -Row, -Column) %>%
        mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_chicago
  
  #### 9.6. Write outputs ####   
    # Print the output table with today's date
    st_write(population_2019_data_pixel_chicago, "output_data/shapefiles/1km/Chicago/1_km_population_2019_chicago.shp") 
    #Has total pixels 19,209 we use as grid in BenMAP. 
    #File with geometry, row, col, pixel ID, ages and races
    
    write.csv(population_2019_data_pixel_age_final_chicago, row.names = FALSE, "output_data/BenMAP_files/1km/Chicago/1_km_population_2019_chicago.csv")#BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
    
  #### 9.7. Figures ####
    #Figure
    population_2019_data_pixel_chicago %>%
      mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_chicago_log
    
    #Plot 1 km grid data
    p<- ggplot() +
      geom_sf(data = population_2019_data_pixel_chicago_log, aes(fill = log_pixel_population), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("Chicago 1km Rescaled Population")
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_chicago_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_chicago_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
  
  #### 9.8. Checks ####
    #Output (Partitions)
    check_chicago_population_out <- round(sum(population_2019_data_pixel_chicago$Population)) #9,508,605
    
    #Area ratio sum 
    partitions_chicago_complete %>%
      group_by(census_block_ID) %>%
      summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_chicago #should = 1 

#### 10. WASHINGTON DC ####
  #### 10.1. Prepare census block and grids ####
    census_block %>%
      filter(state_ID %in% c("24", "51", "11", "54"))-> census_block_DC
    
    #Intersect census and MSA
    intersection_wash_NAD83_bg <- st_intersection(census_block_DC, cbsa_shapefile_wash)
    
    #The intersection includes some bordering block groups that should be excluded
    #For wash the MSA has 25: Frederick (24021), Charles (24017), Montgomery (24031),
    #Prince Georges (24033), Calvert (24009)
    #Fairfax (51600), Fauquier (51061), Prince William (51153), Stafford (51179), Fairfax (51059)
    #Loudon (51107), Culpeper (51047), Clarke (51043), Alexandria (51510), Falls Church (51610)
    #Arlington (51013), Manassas (51683), Manasas Park (51685), Warren (51187), Rappahannock (51157)
    #Madison (51113), Spotsylvania (51177), Fredericksburg (51630)
    #DC (11001), 
    #Jefferson (54037)
    intersection_wash_NAD83_bg %>%
      filter(grepl("24021", census_block_ID) | grepl("24017", census_block_ID) | grepl("24031", census_block_ID) |
               grepl("24033", census_block_ID) | grepl("24009", census_block_ID) | grepl("51600", census_block_ID) |
               grepl("51061", census_block_ID) | grepl("51153", census_block_ID) | grepl("51179", census_block_ID) |
               grepl("51059", census_block_ID) | grepl("51013", census_block_ID) | grepl("51683", census_block_ID) |
               grepl("51107", census_block_ID) | grepl("51047", census_block_ID) | grepl("51043", census_block_ID) |
               grepl("51510", census_block_ID) | grepl("51610", census_block_ID) |
               grepl("51685", census_block_ID) | grepl("51187", census_block_ID) | grepl("51157", census_block_ID) |
               grepl("51113", census_block_ID) | grepl("51177", census_block_ID) | grepl("51630", census_block_ID) |
               grepl("11001", census_block_ID) | grepl("54037", census_block_ID)) -> intersection_wash_NAD83_bg
    
    wash_blocks <- intersection_wash_NAD83_bg$census_block_ID #These are the census block groups in the MSA
    
    #Calculate pixel area
    grid_data_wash_NAD83_filtered %>% 
      mutate(pixel_area = round(st_area(grid_data_wash_NAD83_filtered), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_wash_NAD83_filtered_area
  
  #### 10.2. Join our shapefiles (census data + 1km grid) ####
    joined_data_original_wash <- st_join(grid_data_wash_NAD83_filtered_area, census_block_DC, join = st_intersects)
  
  #### 10.3. Create the function for calculating intersecting area ####
    calculate_intersection_area <- function(pixel_ID, census_block_ID) {
      pixel_geometry <- st_geometry(grid_data_wash_NAD83_filtered_area[grid_data_wash_NAD83_filtered_area$pixel_ID == pixel_ID, ])
      block_geometry <- st_geometry(census_block_DC[census_block_DC$census_block_ID == census_block_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
      return(intersection_area)
    }
  
  #### 10.4. Run the function ####
    partitions_wash <- joined_data_original_wash
    
    timing_result <- system.time({
      partitions_wash$intersection_area <- mapply(calculate_intersection_area, 
                                                     pixel_ID = partitions_wash$pixel_ID, 
                                                     census_block_ID = partitions_wash$census_block_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #522 = ~8.7 minutes
    
  #### 10.5. Outputs ####
    ### 10.5.1. Get area ratios ###
      partitions_wash %>%
        filter(census_block_ID %in% wash_blocks) -> partitions_wash #From 37,914 to 36,751
      
      #This is how much of the census block is in the pixel, group by census block should equal 1
      partitions_wash$area_ratio_block <- partitions_wash$intersection_area / partitions_wash$block_area
      
      #This is how much of the pixel is in each census block, group by pixel should equal 1
      partitions_wash$area_ratio_pixel <- partitions_wash$intersection_area / partitions_wash$pixel_area
      
    ### 10.5.2. Reset pixel area when needed###
      partitions_wash %>%
        group_by(pixel_ID) %>%
        summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
        ungroup() %>%
        st_drop_geometry()-> partitions_wash_area_pixel
      
      partitions_wash_area_pixel_border <- partitions_wash_area_pixel$pixel_ID[partitions_wash_area_pixel$sum_area_ratio_pixel < 0.99] #1180
      
      for (i in seq_along(partitions_wash$pixel_ID)) {
        current_pixel_ID <- partitions_wash$pixel_ID[i]
        
        # Check if the current pixel_ID is in partitions_area_pixel_border
        if (current_pixel_ID %in% partitions_wash_area_pixel_border) {
          
          # Filter partitions to get the rows matching the current pixel_ID
          matching_rows <- partitions_wash[partitions_wash$pixel_ID == current_pixel_ID, ]
          
          # Recalculate pixel_area as the sum of intersection_area for the matching rows
          new_pixel_area <- sum(matching_rows$intersection_area)
          
          # Update the pixel_area in the 'partitions' table
          partitions_wash$pixel_area[i] <- new_pixel_area
          
        } 
      }
      
      #write csv
      partitions_wash %>%
        st_drop_geometry() -> partitions_wash_list
      write.csv(partitions_wash_list, "output_data/BenMAP_files/1km/DC/partitions_DC.csv")
      
    ### 10.5.3. Merge variables ###
      #Merge partitions with variables
      partitions_wash %>%
        left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_wash_complete
      
    ### 10.5.4. Calculate population totals per partition ###
      partitions_wash_complete %>%
        mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
               BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
               ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
               `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
               `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
               `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
               `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
               `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
        rename(Row = row, Column = col) %>%
        dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
               `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_wash
      
    ### 10.5.5. Calculate population totals per pixel ###
      population_2019_data_wash %>%
        group_by(pixel_ID, Row, Column) %>%
        summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
                  ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
                  `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
                  `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
        ungroup()-> population_2019_data_pixel_wash
      
    ### 10.5.6. Population by age ###
      population_2019_data_pixel_wash %>%
        st_drop_geometry() %>%
        dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
               `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
        gather(AgeRange, Population, -Row, -Column) %>%
        mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_wash
  
  #### 10.6. Write outputs ####   
    # Print the output table with today's date
    st_write(population_2019_data_pixel_wash, "output_data/shapefiles/1km/DC/1_km_population_2019_wash.shp") 
    #Has total pixels 17,877 we use as grid in BenMAP. 
    #File with geometry, row, col, pixel ID, ages and races
    
    write.csv(population_2019_data_pixel_age_final_wash, row.names = FALSE, "output_data/BenMAP_files/1km/DC/1_km_population_2019_wash.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
  
  #### 10.7. Figures ####
    #Figure
    population_2019_data_pixel_wash %>%
      mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_wash_log
    
    #Plot 1 km grid data
    p<- ggplot() +
      geom_sf(data = population_2019_data_pixel_wash_log, aes(fill = log_pixel_population), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("DC Metro 1km Rescaled Population")
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_DC_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_DC_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
  
  #### 10.8. Checks ####
    #Output (Partitions)
    check_wash_population_out <- round(sum(population_2019_data_pixel_wash$Population)) #6,196,585
    
    #Area ratio sum 
    partitions_wash_complete %>%
      group_by(census_block_ID) %>%
      summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_wash #should = 1 

#### 11. PHILADELPHIA ####
  #### 11.1. Prepare census block and grids ####
    census_block %>%
      filter(state_ID %in% c("42", "10", "34", "24"))-> census_block_phil
  
    #Intersect census and MSA
    intersection_phil_NAD83_bg <- st_intersection(census_block_phil, cbsa_shapefile_phil)
    
    #The intersection includes some bordering block groups that should be excluded
    #For phil the MSA has 11: Philadelphia (42101), Chester (42029), Delaware (42045), 
    #Bucks (42017), Montgomery (42091);
    #New Castle (10003), 
    #Camden (34007), Gloucester (34015), Burlignton (34005), Salem (34033),
    #Cecil (24015)
    intersection_phil_NAD83_bg %>%
      filter(grepl("42101", census_block_ID) | grepl("42029", census_block_ID) | grepl("42045", census_block_ID) |
               grepl("42017", census_block_ID) | grepl("42091", census_block_ID) | grepl("10003", census_block_ID) |
               grepl("34007", census_block_ID) | grepl("34015", census_block_ID) | grepl("34005", census_block_ID) |
               grepl("34033", census_block_ID) | grepl("24015", census_block_ID)) -> intersection_phil_NAD83_bg
    
    phil_blocks <- intersection_phil_NAD83_bg$census_block_ID #These are the census block groups in the MSA
    
    #Calculate pixel area
    grid_data_phil_NAD83_filtered %>% 
      mutate(pixel_area = round(st_area(grid_data_phil_NAD83_filtered), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_phil_NAD83_filtered_area
  
  #### 11.2. Join our shapefiles (census data + 1km grid) ####
    joined_data_original_phil <- st_join(grid_data_phil_NAD83_filtered_area, census_block_phil, join = st_intersects)
  
  #### 11.3. Create the function for calculating intersecting area ####
    calculate_intersection_area <- function(pixel_ID, census_block_ID) {
      pixel_geometry <- st_geometry(grid_data_phil_NAD83_filtered_area[grid_data_phil_NAD83_filtered_area$pixel_ID == pixel_ID, ])
      block_geometry <- st_geometry(census_block_phil[census_block_phil$census_block_ID == census_block_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
      return(intersection_area)
    }
  
  #### 11.4. Run the function ####
    partitions_phil <- joined_data_original_phil
    
    timing_result <- system.time({
      partitions_phil$intersection_area <- mapply(calculate_intersection_area, 
                                                  pixel_ID = partitions_phil$pixel_ID, 
                                                  census_block_ID = partitions_phil$census_block_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #427 = ~7 minutes
  
  #### 11.5. Outputs ####
    ### 11.5.1. Get area ratios ###
      partitions_phil %>%
        filter(census_block_ID %in% phil_blocks) -> partitions_phil #From 32,739 to 31,778
      
      #This is how much of the census block is in the pixel, group by census block should equal 1
      partitions_phil$area_ratio_block <- partitions_phil$intersection_area / partitions_phil$block_area
      
      #This is how much of the pixel is in each census block, group by pixel should equal 1
      partitions_phil$area_ratio_pixel <- partitions_phil$intersection_area / partitions_phil$pixel_area
      
    ### 11.5.2. Reset pixel area when needed###
      partitions_phil %>%
        group_by(pixel_ID) %>%
        summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
        ungroup() %>%
        st_drop_geometry()-> partitions_phil_area_pixel
      
      partitions_phil_area_pixel_border <- partitions_phil_area_pixel$pixel_ID[partitions_phil_area_pixel$sum_area_ratio_pixel < 0.99] #897
      
      for (i in seq_along(partitions_phil$pixel_ID)) {
        current_pixel_ID <- partitions_phil$pixel_ID[i]
        
        # Check if the current pixel_ID is in partitions_area_pixel_border
        if (current_pixel_ID %in% partitions_phil_area_pixel_border) {
          
          # Filter partitions to get the rows matching the current pixel_ID
          matching_rows <- partitions_phil[partitions_phil$pixel_ID == current_pixel_ID, ]
          
          # Recalculate pixel_area as the sum of intersection_area for the matching rows
          new_pixel_area <- sum(matching_rows$intersection_area)
          
          # Update the pixel_area in the 'partitions' table
          partitions_phil$pixel_area[i] <- new_pixel_area
          
        } 
      }
      
      #write csv
      partitions_phil %>%
        st_drop_geometry() -> partitions_phil_list
      write.csv(partitions_phil_list, "output_data/BenMAP_files/1km/Philadelphia/partitions_phil.csv")
      
    ### 11.5.3. Merge variables ###
      #Merge partitions with variables
      partitions_phil %>%
        left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_phil_complete
      
    ### 11.5.4. Calculate population totals per partition ###
      partitions_phil_complete %>%
        mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
               BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
               ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
               `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
               `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
               `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
               `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
               `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
        rename(Row = row, Column = col) %>%
        dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
               `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_phil
      
    ### 11.5.5. Calculate population totals per pixel ###
      population_2019_data_phil %>%
        group_by(pixel_ID, Row, Column) %>%
        summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
                  ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
                  `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
                  `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
        ungroup()-> population_2019_data_pixel_phil
      
    ### 11.5.6. Population by age ###
      population_2019_data_pixel_phil %>%
        st_drop_geometry() %>%
        dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
               `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
        gather(AgeRange, Population, -Row, -Column) %>%
        mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_phil
  
  #### 11.6. Write outputs ####   
    # Print the output table with today's date
    st_write(population_2019_data_pixel_phil, "output_data/shapefiles/1km/Philadelphia/1_km_population_2019_philadelphia.shp") 
    #Has total pixels 12,652we use as grid in BenMAP. 
    #File with geometry, row, col, pixel ID, ages and races
    
    write.csv(population_2019_data_pixel_age_final_phil, row.names = FALSE, "output_data/BenMAP_files/1km/Philadelphia/1_km_population_2019_philadelphia.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
  
  #### 11.7. Figures ####
    #Figure
    population_2019_data_pixel_phil %>%
      mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_phil_log
    
    #Plot 1 km grid data
    p<- ggplot() +
      geom_sf(data = population_2019_data_pixel_phil_log, aes(fill = log_pixel_population), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("Philadelphia Metro 1km Rescaled Population")
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_philadelphia_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_philadelphia_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
  
  #### 11.8. Checks ####
    #Output (Partitions)
    check_phil_population_out <- round(sum(population_2019_data_pixel_phil$Population)) #6,079,130
    
    #Area ratio sum 
    partitions_phil_complete %>%
      group_by(census_block_ID) %>%
      summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_phil #should = 1 

#### 12. NEW YORK ####
  #### 12.1. Prepare census block and grids ####
    census_block %>%
      filter(state_ID %in% c("42", "34", "36"))-> census_block_NY
    
    #Intersect census and MSA
    intersection_NY_NAD83_bg <- st_intersection(census_block_NY, cbsa_shapefile_NY)
    
    #The intersection includes some bordering block groups that should be excluded
    #For NY the MSA has 23: Pike (42103), 
    #Sussex (34037), Essex (34013), Passaic (34031), Somerset (34035), Morris (34027),
    #Middlesex (34023), Bergen (34003), Ocean (34029), Hudson (34017), Union (34039),
    #Monmouth (34025), Hunterdon (34019)
    #Westchester (36119), Queens (36081), Kings (36047), New York (36061), Bronx (36005),
    #Richmond (36085), Rockland (36087), Nassau (36059), Suffolk (36103), Putnam (36079)
    intersection_NY_NAD83_bg %>%
      filter(grepl("42103", census_block_ID) | grepl("34037", census_block_ID) | grepl("34013", census_block_ID) |
               grepl("34031", census_block_ID) | grepl("34035", census_block_ID) | grepl("34027", census_block_ID) |
               grepl("34023", census_block_ID) | grepl("34003", census_block_ID) | grepl("34029", census_block_ID) |
               grepl("34017", census_block_ID) | grepl("34039", census_block_ID) | grepl("34025", census_block_ID) |
               grepl("34019", census_block_ID) | grepl("36119", census_block_ID) | grepl("36081", census_block_ID) |
               grepl("36047", census_block_ID) | grepl("36061", census_block_ID) | grepl("36005", census_block_ID) |
               grepl("36085", census_block_ID) | grepl("36087", census_block_ID) | grepl("36059", census_block_ID) |
               grepl("36103", census_block_ID) | grepl("36079", census_block_ID)) -> intersection_NY_NAD83_bg
    
    NY_blocks <- intersection_NY_NAD83_bg$census_block_ID #These are the census block groups in the MSA
    
    #Calculate pixel area
    grid_data_NY_NAD83_filtered %>% 
      mutate(pixel_area = round(st_area(grid_data_NY_NAD83_filtered), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_NY_NAD83_filtered_area
  
  #### 12.2. Join our shapefiles (census data + 1km grid) ####
    joined_data_original_NY <- st_join(grid_data_NY_NAD83_filtered_area, census_block_NY, join = st_intersects)
  
  #### 12.3. Create the function for calculating intersecting area ####
    calculate_intersection_area <- function(pixel_ID, census_block_ID) {
      pixel_geometry <- st_geometry(grid_data_NY_NAD83_filtered_area[grid_data_NY_NAD83_filtered_area$pixel_ID == pixel_ID, ])
      block_geometry <- st_geometry(census_block_NY[census_block_NY$census_block_ID == census_block_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
      return(intersection_area)
    }
  
  #### 12.4. Run the function ####
    partitions_NY <- joined_data_original_NY
    
    timing_result <- system.time({
      partitions_NY$intersection_area <- mapply(calculate_intersection_area, 
                                                  pixel_ID = partitions_NY$pixel_ID, 
                                                  census_block_ID = partitions_NY$census_block_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #895 = ~18 minutes
  
  #### 12.5. Outputs ####
    ### 12.5.1. Get area ratios ###
      partitions_NY %>%
        filter(census_block_ID %in% NY_blocks) -> partitions_NY #From 65,976 to 64,998
      
      #This is how much of the census block is in the pixel, group by census block should equal 1
      partitions_NY$area_ratio_block <- partitions_NY$intersection_area / partitions_NY$block_area
      
      #This is how much of the pixel is in each census block, group by pixel should equal 1
      partitions_NY$area_ratio_pixel <- partitions_NY$intersection_area / partitions_NY$pixel_area
      
    ### 12.5.2. Reset pixel area when needed###
      partitions_NY %>%
        group_by(pixel_ID) %>%
        summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
        ungroup() %>%
        st_drop_geometry()-> partitions_NY_area_pixel
      
      partitions_NY_area_pixel_border <- partitions_NY_area_pixel$pixel_ID[partitions_NY_area_pixel$sum_area_ratio_pixel < 0.99] #1818
      
      for (i in seq_along(partitions_NY$pixel_ID)) {
        current_pixel_ID <- partitions_NY$pixel_ID[i]
        
        # Check if the current pixel_ID is in partitions_area_pixel_border
        if (current_pixel_ID %in% partitions_NY_area_pixel_border) {
          
          # Filter partitions to get the rows matching the current pixel_ID
          matching_rows <- partitions_NY[partitions_NY$pixel_ID == current_pixel_ID, ]
          
          # Recalculate pixel_area as the sum of intersection_area for the matching rows
          new_pixel_area <- sum(matching_rows$intersection_area)
          
          # Update the pixel_area in the 'partitions' table
          partitions_NY$pixel_area[i] <- new_pixel_area
          
        } 
      }
      
      #write csv
      partitions_NY %>%
        st_drop_geometry() -> partitions_NY_list
      write.csv(partitions_NY_list, "output_data/BenMAP_files/1km/NY/partitions_NY.csv")
      
    ### 12.5.3. Merge variables ###
      #Merge partitions with variables
      partitions_NY %>%
        left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_NY_complete
      
    ### 12.5.4. Calculate population totals per partition ###
      partitions_NY_complete %>%
        mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
               BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
               ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
               `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
               `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
               `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
               `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
               `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
        rename(Row = row, Column = col) %>%
        dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
               `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_NY
      
    ### 12.5.5. Calculate population totals per pixel ###
      population_2019_data_NY %>%
        group_by(pixel_ID, Row, Column) %>%
        summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
                  ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
                  `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
                  `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
        ungroup()-> population_2019_data_pixel_NY
      
    ### 12.5.6. Population by age ###
      population_2019_data_pixel_NY %>%
        st_drop_geometry() %>%
        dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
               `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
        gather(AgeRange, Population, -Row, -Column) %>%
        mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_NY
  
  #### 12.6. Write outputs ####   
    # Print the output table with today's date
    st_write(population_2019_data_pixel_NY, "output_data/shapefiles/1km/NY/1_km_population_2019_NY.shp") 
    #Has total pixels 19,841we use as grid in BenMAP. 
    #File with geometry, row, col, pixel ID, ages and races
    
    write.csv(population_2019_data_pixel_age_final_NY, row.names = FALSE, "output_data/BenMAP_files/1km/NY/1_km_population_2019_NY.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender

  #### 12.7. Figures ####
    #Figure
    population_2019_data_pixel_NY %>%
      mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_NY_log
    
    #Plot 1 km grid data
    p<- ggplot() +
      geom_sf(data = population_2019_data_pixel_NY_log, aes(fill = log_pixel_population), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("NY Metro 1km Rescaled Population")
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_NY_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_NY_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
  
  #### 12.8. Checks ####
    #Output (Partitions)
    check_NY_population_out <- round(sum(population_2019_data_pixel_NY$Population)) #19,294,236
    
    #Area ratio sum 
    partitions_NY_complete %>%
      group_by(census_block_ID) %>%
      summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_NY #should = 1
  
#### 13. BOSTON ####
  #### 13.1. Prepare census block and grids ####
    census_block %>%
      filter(state_ID %in% c("33", "25"))-> census_block_boston
    
    #Intersect census and MSA
    intersection_boston_NAD83_bg <- st_intersection(census_block_boston, cbsa_shapefile_boston)
    
    #The intersection includes some bordering block groups that should be excluded
    #For Boston the MSA has 7 counties: Strafford (33017), Rockingham (33015), 
    #Suffolk (25025), Norfolk (25021), Essex (25009), Plymouth (25023), Middlesex (25017)
    intersection_boston_NAD83_bg %>%
      filter(grepl("33017", census_block_ID) | grepl("33015", census_block_ID) | grepl("25025", census_block_ID) |
               grepl("25021", census_block_ID) | grepl("25009", census_block_ID) | grepl("25023", census_block_ID) |
               grepl("25017", census_block_ID)) -> intersection_boston_NAD83_bg
    
    boston_blocks <- intersection_boston_NAD83_bg$census_block_ID #These are the census block groups in the MSA
    
    #Calculate pixel area
    grid_data_boston_NAD83_filtered %>% 
      mutate(pixel_area = round(st_area(grid_data_boston_NAD83_filtered), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_boston_NAD83_filtered_area
  
  #### 13.2. Join our shapefiles (census data + 1km grid) ####
    joined_data_original_boston <- st_join(grid_data_boston_NAD83_filtered_area, census_block_boston, join = st_intersects)
  
  #### 13.3. Create the function for calculating intersecting area ####
    calculate_intersection_area <- function(pixel_ID, census_block_ID) {
      pixel_geometry <- st_geometry(grid_data_boston_NAD83_filtered_area[grid_data_boston_NAD83_filtered_area$pixel_ID == pixel_ID, ])
      block_geometry <- st_geometry(census_block_boston[census_block_boston$census_block_ID == census_block_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
      return(intersection_area)
    }
    
  #### 13.4. Run the function ####
    partitions_boston <- joined_data_original_boston
    
    timing_result <- system.time({
      partitions_boston$intersection_area <- mapply(calculate_intersection_area, 
                                                pixel_ID = partitions_boston$pixel_ID, 
                                                census_block_ID = partitions_boston$census_block_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #355 = ~6 minutes
  
  #### 13.5. Outputs ####
    ### 13.5.1. Get area ratios ###
      partitions_boston %>%
        filter(census_block_ID %in% boston_blocks) -> partitions_boston #From 26,479 to 25,685
      
      #This is how much of the census block is in the pixel, group by census block should equal 1
      partitions_boston$area_ratio_block <- partitions_boston$intersection_area / partitions_boston$block_area
      
      #This is how much of the pixel is in each census block, group by pixel should equal 1
      partitions_boston$area_ratio_pixel <- partitions_boston$intersection_area / partitions_boston$pixel_area
      
    ### 13.5.2. Reset pixel area when needed###
      partitions_boston %>%
        group_by(pixel_ID) %>%
        summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
        ungroup() %>%
        st_drop_geometry()-> partitions_boston_area_pixel
      
      partitions_boston_area_pixel_border <- partitions_boston_area_pixel$pixel_ID[partitions_boston_area_pixel$sum_area_ratio_pixel < 0.99] #1137
      
      for (i in seq_along(partitions_boston$pixel_ID)) {
        current_pixel_ID <- partitions_boston$pixel_ID[i]
        
        # Check if the current pixel_ID is in partitions_area_pixel_border
        if (current_pixel_ID %in% partitions_boston_area_pixel_border) {
          
          # Filter partitions to get the rows matching the current pixel_ID
          matching_rows <- partitions_boston[partitions_boston$pixel_ID == current_pixel_ID, ]
          
          # Recalculate pixel_area as the sum of intersection_area for the matching rows
          new_pixel_area <- sum(matching_rows$intersection_area)
          
          # Update the pixel_area in the 'partitions' table
          partitions_boston$pixel_area[i] <- new_pixel_area
          
        } 
      }
      
      #write csv
      partitions_boston %>%
        st_drop_geometry() -> partitions_boston_list
      write.csv(partitions_boston_list, "output_data/BenMAP_files/1km/Boston/partitions_boston.csv")
      
    ### 13.5.3. Merge variables ###
      #Merge partitions with variables
      partitions_boston %>%
        left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_boston_complete
      
    ### 13.5.4. Calculate population totals per partition ###
      partitions_boston_complete %>%
        mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
               BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
               ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
               `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
               `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
               `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
               `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
               `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
        rename(Row = row, Column = col) %>%
        dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
               `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_boston
      
    ### 13.5.5. Calculate population totals per pixel ###
      population_2019_data_boston %>%
        group_by(pixel_ID, Row, Column) %>%
        summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
                  ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
                  `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
                  `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
        ungroup()-> population_2019_data_pixel_boston
      
    ### 13.5.6. Population by age ###
      population_2019_data_pixel_boston %>%
        st_drop_geometry() %>%
        dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
               `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
        gather(AgeRange, Population, -Row, -Column) %>%
        mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_boston
  
  #### 13.6. Write outputs ####   
    # Print the output table with today's date
    st_write(population_2019_data_pixel_boston, "output_data/shapefiles/1km/Boston/1_km_population_2019_boston.shp") 
    #Has total pixels 10,087 we use as grid in BenMAP. 
    #File with geometry, row, col, pixel ID, ages and races
    
    write.csv(population_2019_data_pixel_age_final_boston, row.names = FALSE, "output_data/BenMAP_files/1km/Boston/1_km_population_2019_boston.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
  
  #### 13.7. Figures ####
    #Figure
    population_2019_data_pixel_boston %>%
      mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_boston_log
    
    #Plot 1 km grid data
    p<- ggplot() +
      geom_sf(data = population_2019_data_pixel_boston_log, aes(fill = log_pixel_population), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("Boston Metro 1km Rescaled Population")
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_boston_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_boston_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
  
  #### 13.8. Checks ####
    #Output (Partitions)
    check_boston_population_out <- round(sum(population_2019_data_pixel_boston$Population)) #4,832,346
    
    #Area ratio sum 
    partitions_boston_complete %>%
      group_by(census_block_ID) %>%
      summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_boston #should = 1 
  
#### 14. DETROIT ####
  #### 14.1. Prepare census block and grids ####
    census_block %>%
      filter(state_ID %in% c("26"))-> census_block_detroit
    
    #Intersect census and MSA
    intersection_detroit_NAD83_bg <- st_intersection(census_block_detroit, cbsa_shapefile_detroit)
    
    #The intersection includes some bordering block groups that should be excluded
    #For Detroit the MSA has 6 counties: Wayne (26163), St. Clair (26147), Macomb (26099), 
    #Oakland (26125), Lapeer (26087), Livingston (26093)
    intersection_detroit_NAD83_bg %>%
      filter(grepl("26163", census_block_ID) | grepl("26147", census_block_ID) | grepl("26099", census_block_ID) |
               grepl("26125", census_block_ID) | grepl("26087", census_block_ID) | grepl("26093", census_block_ID)) -> intersection_detroit_NAD83_bg
    
    detroit_blocks <- intersection_detroit_NAD83_bg$census_block_ID #These are the census block groups in the MSA
    
    #Calculate pixel area
    grid_data_detroit_NAD83_filtered %>% 
      mutate(pixel_area = round(st_area(grid_data_detroit_NAD83_filtered), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_detroit_NAD83_filtered_area
  
  #### 14.2. Join our shapefiles (census data + 1km grid) ####
    joined_data_original_detroit <- st_join(grid_data_detroit_NAD83_filtered_area, census_block_detroit, join = st_intersects)
  
  #### 14.3. Create the function for calculating intersecting area ####
    calculate_intersection_area <- function(pixel_ID, census_block_ID) {
      pixel_geometry <- st_geometry(grid_data_detroit_NAD83_filtered_area[grid_data_detroit_NAD83_filtered_area$pixel_ID == pixel_ID, ])
      block_geometry <- st_geometry(census_block_detroit[census_block_detroit$census_block_ID == census_block_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
      return(intersection_area)
    }
    
  #### 14.4. Run the function ####
    partitions_detroit <- joined_data_original_detroit
    
    timing_result <- system.time({
      partitions_detroit$intersection_area <- mapply(calculate_intersection_area, 
                                                    pixel_ID = partitions_detroit$pixel_ID, 
                                                    census_block_ID = partitions_detroit$census_block_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #375 = ~6 minutes
  
  #### 14.5. Outputs ####
    ### 14.5.1. Get area ratios ###
      partitions_detroit %>%
        filter(census_block_ID %in% detroit_blocks) -> partitions_detroit #From 26,685 to 26,152
      
      #This is how much of the census block is in the pixel, group by census block should equal 1
      partitions_detroit$area_ratio_block <- partitions_detroit$intersection_area / partitions_detroit$block_area
      
      #This is how much of the pixel is in each census block, group by pixel should equal 1
      partitions_detroit$area_ratio_pixel <- partitions_detroit$intersection_area / partitions_detroit$pixel_area
      
    ### 14.5.2. Reset pixel area when needed###
      partitions_detroit %>%
        group_by(pixel_ID) %>%
        summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
        ungroup() %>%
        st_drop_geometry()-> partitions_detroit_area_pixel
      
      partitions_detroit_area_pixel_border <- partitions_detroit_area_pixel$pixel_ID[partitions_detroit_area_pixel$sum_area_ratio_pixel < 0.99] #682
      
      for (i in seq_along(partitions_detroit$pixel_ID)) {
        current_pixel_ID <- partitions_detroit$pixel_ID[i]
        
        # Check if the current pixel_ID is in partitions_area_pixel_border
        if (current_pixel_ID %in% partitions_detroit_area_pixel_border) {
          
          # Filter partitions to get the rows matching the current pixel_ID
          matching_rows <- partitions_detroit[partitions_detroit$pixel_ID == current_pixel_ID, ]
          
          # Recalculate pixel_area as the sum of intersection_area for the matching rows
          new_pixel_area <- sum(matching_rows$intersection_area)
          
          # Update the pixel_area in the 'partitions' table
          partitions_detroit$pixel_area[i] <- new_pixel_area
          
        } 
      }
      
      #write csv
      partitions_detroit %>%
        st_drop_geometry() -> partitions_detroit_list
      write.csv(partitions_detroit_list, "output_data/BenMAP_files/1km/Detroit/partitions_detroit.csv")
      
    ### 14.5.3. Merge variables ###
      #Merge partitions with variables
      partitions_detroit %>%
        left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_detroit_complete
      
    ### 14.5.4. Calculate population totals per partition ###
      partitions_detroit_complete %>%
        mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
               BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
               ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
               `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
               `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
               `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
               `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
               `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
        rename(Row = row, Column = col) %>%
        dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
               `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_detroit
      
    ### 14.5.5. Calculate population totals per pixel ###
      population_2019_data_detroit %>%
        group_by(pixel_ID, Row, Column) %>%
        summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
                  ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
                  `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
                  `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
        ungroup()-> population_2019_data_pixel_detroit
      
    ### 14.5.6. Population by age ###
      population_2019_data_pixel_detroit %>%
        st_drop_geometry() %>%
        dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
               `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
        gather(AgeRange, Population, -Row, -Column) %>%
        mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_detroit
  
  #### 14.6. Write outputs ####   
    # Print the output table with today's date
    st_write(population_2019_data_pixel_detroit, "output_data/shapefiles/1km/Detroit/1_km_population_2019_detroit.shp") 
    #Has total pixels 10,711 we use as grid in BenMAP. 
    #File with geometry, row, col, pixel ID, ages and races
    
    write.csv(population_2019_data_pixel_age_final_detroit, row.names = FALSE, "output_data/BenMAP_files/1km/Detroit/1_km_population_2019_detroit.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
  
  #### 14.7. Figures ####
    #Figure
    population_2019_data_pixel_detroit %>%
      mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_detroit_log
    
    #Plot 1 km grid data
    p<- ggplot() +
      geom_sf(data = population_2019_data_pixel_detroit_log, aes(fill = log_pixel_population), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("detroit Metro 1km Rescaled Population")
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_detroit_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_detroit_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
  
  #### 14.8. Checks ####
    #Output (Partitions)
    check_detroit_population_out <- round(sum(population_2019_data_pixel_detroit$Population)) #4,317,848
    
    #Area ratio sum 
    partitions_detroit_complete %>%
      group_by(census_block_ID) %>%
      summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_detroit #should = 1 

#### 15. SAN FRANCISCO ####
  #### 15.1. Prepare census block and grids ####
    census_block %>%
      filter(state_ID %in% c("06"))-> census_block_SF
    
    #Intersect census and MSA
    intersection_SF_NAD83_bg <- st_intersection(census_block_SF, cbsa_shapefile_SF)
    
    #The intersection includes some bordering block groups that should be excluded
    #For SF the MSA has 5 counties: Alameda (06001), Contra Costa (06013), Marin (06041),
    #San Mateo (06081), San Francisco (06075)
    intersection_SF_NAD83_bg %>%
      filter(grepl("06001", census_block_ID) | grepl("06013", census_block_ID) | grepl("06041", census_block_ID) |
               grepl("06081", census_block_ID) | grepl("06075", census_block_ID)) -> intersection_SF_NAD83_bg
    
    SF_blocks <- intersection_SF_NAD83_bg$census_block_ID #These are the census block groups in the MSA
    
    #Calculate pixel area
    grid_data_SF_NAD83_filtered %>% 
      mutate(pixel_area = round(st_area(grid_data_SF_NAD83_filtered), digits = 5), #Area in m^2
             pixel_area = as.numeric(gsub("\\[m²\\]", "", pixel_area))) -> grid_data_SF_NAD83_filtered_area
  
  #### 15.2. Join our shapefiles (census data + 1km grid) ####
    joined_data_original_SF <- st_join(grid_data_SF_NAD83_filtered_area, census_block_SF, join = st_intersects)
  
  #### 15.3. Create the function for calculating intersecting area ####
    calculate_intersection_area <- function(pixel_ID, census_block_ID) {
      pixel_geometry <- st_geometry(grid_data_SF_NAD83_filtered_area[grid_data_SF_NAD83_filtered_area$pixel_ID == pixel_ID, ])
      block_geometry <- st_geometry(census_block_SF[census_block_SF$census_block_ID == census_block_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, block_geometry)), digits = 5)
      return(intersection_area)
    }
  
  #### 15.4. Run the function ####
    partitions_SF <- joined_data_original_SF
    
    timing_result <- system.time({
      partitions_SF$intersection_area <- mapply(calculate_intersection_area, 
                                                     pixel_ID = partitions_SF$pixel_ID, 
                                                     census_block_ID = partitions_SF$census_block_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #243 = ~4 minutes
  
  #### 15.5. Outputs ####
    ### 15.5.1. Get area ratios ###
      partitions_SF %>%
        filter(census_block_ID %in% SF_blocks) -> partitions_SF #From 18,043 to 17,544
      
      #This is how much of the census block is in the pixel, group by census block should equal 1
      partitions_SF$area_ratio_block <- partitions_SF$intersection_area / partitions_SF$block_area
      
      #This is how much of the pixel is in each census block, group by pixel should equal 1
      partitions_SF$area_ratio_pixel <- partitions_SF$intersection_area / partitions_SF$pixel_area
      
    ### 15.5.2. Reset pixel area when needed###
      partitions_SF %>%
        group_by(pixel_ID) %>%
        summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
        ungroup() %>%
        st_drop_geometry()-> partitions_SF_area_pixel
      
      partitions_SF_area_pixel_border <- partitions_SF_area_pixel$pixel_ID[partitions_SF_area_pixel$sum_area_ratio_pixel < 0.99] #939
      
      for (i in seq_along(partitions_SF$pixel_ID)) {
        current_pixel_ID <- partitions_SF$pixel_ID[i]
        
        # Check if the current pixel_ID is in partitions_area_pixel_border
        if (current_pixel_ID %in% partitions_SF_area_pixel_border) {
          
          # Filter partitions to get the rows matching the current pixel_ID
          matching_rows <- partitions_SF[partitions_SF$pixel_ID == current_pixel_ID, ]
          
          # Recalculate pixel_area as the sum of intersection_area for the matching rows
          new_pixel_area <- sum(matching_rows$intersection_area)
          
          # Update the pixel_area in the 'partitions' table
          partitions_SF$pixel_area[i] <- new_pixel_area
          
        } 
      }
      
      #write csv
      partitions_SF %>%
        st_drop_geometry() -> partitions_SF_list
      write.csv(partitions_SF_list, "output_data/BenMAP_files/1km/SF/partitions_SF.csv")
      
    ### 15.5.3. Merge variables ###
      #Merge partitions with variables
      partitions_SF %>%
        left_join(census_block_variables, by = c("census_block_ID", "state_ID", "block_area")) -> partitions_SF_complete
      
    ### 15.5.4. Calculate population totals per partition ###
      partitions_SF_complete %>%
        mutate(Population = population_in * area_ratio_block, WHITE = WHITE_in * area_ratio_block,
               BLACK = BLACK_in * area_ratio_block, NATAMER = NATAMER_in * area_ratio_block,
               ASIAN = ASIAN_in * area_ratio_block, Other = Other_in * area_ratio_block,
               `0TO0` = `0_in` * area_ratio_block, `1TO17` = `1TO17_in` * area_ratio_block, 
               `18TO24` = `18TO24_in` * area_ratio_block,`25TO34` = `25TO34_in` * area_ratio_block,
               `35TO44` = `35TO44_in` * area_ratio_block,`45TO54` = `45TO54_in` * area_ratio_block,
               `55TO64` = `55TO64_in` * area_ratio_block,`65TO74` = `65TO74_in` * area_ratio_block,
               `75TO84` = `75TO84_in` * area_ratio_block,`85TO99` = `85TO99_in` * area_ratio_block) %>%
        rename(Row = row, Column = col) %>%
        dplyr::select(pixel_ID, Row, Column, Population, WHITE, BLACK, NATAMER, ASIAN, Other, `0TO0`, `1TO17`, 
               `18TO24`, `25TO34`,`35TO44`, `45TO54`, `55TO64`, `65TO74`, `75TO84`, `85TO99`)-> population_2019_data_SF
      
    ### 15.5.5. Calculate population totals per pixel ###
      population_2019_data_SF %>%
        group_by(pixel_ID, Row, Column) %>%
        summarise(Population = sum(Population), WHITE = sum(WHITE), BLACK = sum(BLACK), NATAMER = sum(NATAMER), 
                  ASIAN = sum(ASIAN), Other = sum(Other), `0TO0` = sum(`0TO0`), `1TO17` = sum(`1TO17`), 
                  `18TO24` = sum(`18TO24`),`25TO34` = sum(`25TO34`), `35TO44` = sum(`35TO44`), `45TO54` = sum(`45TO54`),
                  `55TO64` = sum(`55TO64`), `65TO74` = sum(`65TO74`),`75TO84` = sum(`75TO84`),`85TO99` = sum(`85TO99`)) %>%
        ungroup()-> population_2019_data_pixel_SF
      
    ### 15.5.6. Population by age ###
      population_2019_data_pixel_SF %>%
        st_drop_geometry() %>%
        dplyr::select(Row, Column, `0TO0`, `1TO17`, `18TO24`, `25TO34`, `35TO44`, `45TO54`,
               `55TO64`, `65TO74`, `75TO84`, `85TO99`) %>%
        gather(AgeRange, Population, -Row, -Column) %>%
        mutate(Year = 2020, Race = "ALL", Ethnicity = "ALL", Gender = "ALL")-> population_2019_data_pixel_age_final_SF
  
  #### 15.6. Write outputs ####   
    # Print the output table with today's date
    st_write(population_2019_data_pixel_SF, "output_data/shapefiles/1km/SF/1_km_population_2019_SF.shp") 
    #Has total pixels 12,652we use as grid in BenMAP. 
    #File with geometry, row, col, pixel ID, ages and races
    
    write.csv(population_2019_data_pixel_age_final_SF, row.names = FALSE, "output_data/BenMAP_files/1km/SF/1_km_population_2019_SF.csv") #BenMAP file wiht row, col, age range, population, all races, all ethnicity, all gender
  
  #### 15.7. Figures ####
    #Figure
    population_2019_data_pixel_SF %>%
      mutate(log_pixel_population = log(Population + 1)) -> population_2019_data_pixel_SF_log
    
    #Plot 1 km grid data
    p<- ggplot() +
      geom_sf(data = population_2019_data_pixel_SF_log, aes(fill = log_pixel_population), color = NA, size = 0) +
      scale_fill_viridis_c(option = "magma", direction = -1) +  # Using the magma color scale
      ggtitle("SF Metro 1km Rescaled Population")
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_SF_plot_2019_log.png", p, width = 16, height = 12, units = "in", dpi = 600)
    #ggsave("figures/BenMAP/1km/population/1km_rescaled_population_SF_plot_2019_log.svg", p, width = 16, height = 12, units = "in", dpi = 600)
    print(p)
  
  #### 15.8. Checks ####
    #Output (Partitions)
    check_SF_population_out <- round(sum(population_2019_data_pixel_SF$Population)) #4,701,332
    #Area ratio sum 
    partitions_SF_complete %>%
      group_by(census_block_ID) %>%
      summarise(sum_area_ratio = sum(area_ratio_block)) -> check_census_ratio_SF #should = 1 
    
#------------------------------------------------- SECTION A3: 1 KM INCIDENCE RATE -------------------------------------------------
#----GENERAL FILES----
#### 1. Incidence rates ####
    #### 1.1. Load files ####
    #BenMAP census file does not match the Census counties, which causes issues with our rows and columns designation. In order for our analysis to have the same map as a baseline, we use census bureau county shapefiles for the geometry, but BenMAP for the actual incidence
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
      rename(county_name = NAME.x,
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

#### 2. Calculate 2019 values ####
    incidence_2020 %>%
      rename(Value_2020 = Value) %>%
      left_join(incidence_2015, by = c("Endpoint.Group", "Endpoint", "Race", "Gender", "Ethnicity", "Start.Age", "End.Age",
                                       "Type", "Column", "Row")) %>%
      rename(Value_2015 = Value) %>%
      filter(Endpoint == "Mortality, All Cause") %>%
      mutate(age = as.character(End.Age))-> incidence_rates_2015_2020
    
    #Now linearly interpolate 2015 to 2020
    incidence_rates_2015_2020 %>% 
      mutate(Value_2019 = Value_2020 + (Value_2015 - Value_2020) / 5) %>%
      dplyr::select(-Value_2020, -Value_2015)-> incidence_2019
    
#----REGIONAL FILES----   
#### 1. SEATTLE ####
  #### 1.1. Join files ####
    grid_data_county %>%
      filter(county_ID %in% c(53033, 53053, 53061)) -> grid_data_county_WA

    system.time({joined_data_original_mortality_WA <- st_join(grid_data_Seattle_NAD83_filtered_area, grid_data_county_WA, join = st_intersects)}) #0.5 seconds
      
  #### 1.2. Intersection ####
    # Function to calculate intersection area between pixels and blocks
    calculate_intersection_area_WA <- function(pixel_ID, county_ID) {
      pixel_geometry <- st_geometry(grid_data_Seattle_NAD83_filtered_area[grid_data_Seattle_NAD83_filtered_area$pixel_ID == pixel_ID, ])
      county_geometry <- st_geometry(grid_data_county_WA[grid_data_county_WA$county_ID == county_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
      return(intersection_area)
    }
    
    #Run the function
    partitions_mortality_WA <- joined_data_original_mortality_WA
    
    timing_result <- system.time({
      partitions_mortality_WA$intersection_area <- mapply(calculate_intersection_area_WA, 
                                                       pixel_ID = partitions_mortality_WA$pixel_ID, 
                                                       county_ID = partitions_mortality_WA$county_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #235 = ~3.9 minutes
    
    #Calculate ratios
    #This is how much of the county is in the pixel, group by county should equal 1
    partitions_mortality_WA$area_ratio <- partitions_mortality_WA$intersection_area / partitions_mortality_WA$county_area 
    
    #This is how much of the pixel is in each county, group by pixel should equal 1
    partitions_mortality_WA$area_ratio_pixel <- partitions_mortality_WA$intersection_area / partitions_mortality_WA$pixel_area
    
  #### 1.3. Border pixels ####
    #NOTE: pixels that are on the continental coast are larger than continental border, which affects later weighted averages
    #To fix this we set the pixel area for these pixels = the sum of the intersection area with the census blocks contained in that pixel
    partitions_mortality_WA %>%
      group_by(pixel_ID) %>%
      summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
      ungroup() %>%
      st_drop_geometry()-> partitions_mortality_WA_area_pixel
    
    partitions_mortality_WA_area_pixel_border <- partitions_mortality_WA_area_pixel$pixel_ID[partitions_mortality_WA_area_pixel$sum_area_ratio_pixel < 0.99] #1167
    
    #Now we change the pixel_area for these 4043 pixels
    for (i in seq_along(partitions_mortality_WA$pixel_ID)) {
      current_pixel_ID <- partitions_mortality_WA$pixel_ID[i]
      
      # Check if the current pixel_ID is in partitions_mortality_area_pixel_border
      if (current_pixel_ID %in% partitions_mortality_WA_area_pixel_border) {
        
        # Filter partitions_mortality to get the rows matching the current pixel_ID
        matching_rows <- partitions_mortality_WA[partitions_mortality_WA$pixel_ID == current_pixel_ID, ]
        
        # Recalculate pixel_area as the sum of intersection_area for the matching rows
        new_pixel_area <- sum(matching_rows$intersection_area)
        
        # Update the pixel_area in the 'partitions_mortality' table
        partitions_mortality_WA$pixel_area[i] <- new_pixel_area
        
      } 
    }
  
    #Recalculate area pixel ratio
    #This is how much of the pixel is in each county, group by pixel should equal 1
    partitions_mortality_WA$area_ratio_pixel <- partitions_mortality_WA$intersection_area / partitions_mortality_WA$pixel_area
    
  #### 1.4. Incidence rates ####
    #Bring in the incidence rates for each row and column 
    partitions_mortality_WA %>%
      #benmap_ROW and benmap_COL are from BenMAP county,
      #Row and Column are from BenMAP incidence rates, 
      #row and col are from my grid.
      #Value is the incidence rate in the county
      left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
                relationship = "many-to-many") %>%#We expect a many-to-many relationship because there are 11 endpoints and 10 age buckets
      dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_WA
    
    #Weighted mortality rate
      partitions_rate_WA %>%
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
        unique()-> partitions_rate_final_WA
      
  #### 1.5. Print output table####
      #csv
    partitions_rate_final_WA %>%
        dplyr::select(-pixel_ID) %>%
        rename(Row = row, Column = col, Value = weighted_incidence) %>%
        st_drop_geometry()-> partitions_rate_final_clean_WA
      
      write.csv(partitions_rate_final_clean_WA, row.names = FALSE, "output_data/BenMAP_files/1km/Seattle/Incidence_1km_2019_seattle.csv")
      
      #shapefiles
      partitions_rate_final_WA %>%
        dplyr::select(-pixel_ID) %>%
        rename(Row = row, Column = col, Value = weighted_incidence) %>%
        filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_WA
      
      st_write(partitions_rate_final_clean_allcauses_geometry_WA, "output_data/shapefiles/1km/Seattle/1_km_mortality_2019_seattle.shp")
      
#### 2. LOS ANGELES ####
  #### 2.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c("06037", "06059")) -> grid_data_county_CA
      
  joined_data_original_mortality_LA <- st_join(grid_data_south_west_NAD83_LA_area, grid_data_county_CA, join = st_intersects)
  
  #### 2.2. Intersection ####
    #Function to calculate intersection area between pixels and blocks
    calculate_intersection_area_LA <- function(pixel_ID, county_ID) {
      pixel_geometry <- st_geometry(grid_data_south_west_NAD83_LA_area[grid_data_south_west_NAD83_LA_area$pixel_ID == pixel_ID, ])
      county_geometry <- st_geometry(grid_data_county_CA[grid_data_county_CA$county_ID == county_ID, ])
      
      intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
      return(intersection_area)
    }
    
    #Run the function
    partitions_mortality_LA <- joined_data_original_mortality_LA
    
    timing_result <- system.time({
      partitions_mortality_LA$intersection_area <- mapply(calculate_intersection_area_LA, 
                                                          pixel_ID = partitions_mortality_LA$pixel_ID, 
                                                          county_ID = partitions_mortality_LA$county_ID)
    })
    cat("Elapsed Time:", timing_result["elapsed"], "\n") #184 = ~3 minutes
    
    #Calculate ratios 
    partitions_mortality_LA$area_ratio <- partitions_mortality_LA$intersection_area / partitions_mortality_LA$county_area 
    partitions_mortality_LA$area_ratio_pixel <- partitions_mortality_LA$intersection_area / partitions_mortality_LA$pixel_area
  
  #### 2.3. Border pixels ####
    partitions_mortality_LA %>%
      group_by(pixel_ID) %>%
      summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
      ungroup() %>%
      st_drop_geometry()-> partitions_mortality_LA_area_pixel
    
    partitions_mortality_LA_area_pixel_border <- partitions_mortality_LA_area_pixel$pixel_ID[partitions_mortality_LA_area_pixel$sum_area_ratio_pixel < 0.99] #898
    
    for (i in seq_along(partitions_mortality_LA$pixel_ID)) {
      current_pixel_ID <- partitions_mortality_LA$pixel_ID[i]
      if (current_pixel_ID %in% partitions_mortality_LA_area_pixel_border) {
        matching_rows <- partitions_mortality_LA[partitions_mortality_LA$pixel_ID == current_pixel_ID, ]
        new_pixel_area <- sum(matching_rows$intersection_area)
        partitions_mortality_LA$pixel_area[i] <- new_pixel_area
      } 
    }
  
  #Recalculate area pixel ratio
  partitions_mortality_LA$area_ratio_pixel <- partitions_mortality_LA$intersection_area / partitions_mortality_LA$pixel_area
  
  #### 2.4. Incidence rates ####
    partitions_mortality_LA %>%
      left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
                relationship = "many-to-many") %>%
      dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_LA
    
    #Weighted mortality rate
    partitions_rate_LA %>%
      group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
      mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
      ungroup() %>%
      dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
             Start.Age, End.Age, Type, weighted_incidence) %>%
      unique()-> partitions_rate_final_LA
  
  #### 2.5. Print output table####
  #csv
  partitions_rate_final_LA %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_LA
  
  write.csv(partitions_rate_final_clean_LA, row.names = FALSE, "output_data/BenMAP_files/1km/LA/Incidence_1km_2019_LA.csv")
  
  #shapefiles
  partitions_rate_final_LA %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_LA
  
  st_write(partitions_rate_final_clean_allcauses_geometry_LA, "output_data/shapefiles/1km/LA/1_km_mortality_2019_LA.shp")
#### 3. RIVERSIDE ####
  #### 3.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c("06071", "06065")) -> grid_data_county_riverside

  joined_data_original_mortality_riverside <- st_join(grid_data_south_west_NAD83_riverside_area, grid_data_county_riverside, join = st_intersects)
  
  #### 3.2. Intersection ####
  #Function to calculate intersection area between pixels and blocks
  calculate_intersection_area_riverside <- function(pixel_ID, county_ID) {
    pixel_geometry <- st_geometry(grid_data_south_west_NAD83_riverside_area[grid_data_south_west_NAD83_riverside_area$pixel_ID == pixel_ID, ])
    county_geometry <- st_geometry(grid_data_county_riverside[grid_data_county_riverside$county_ID == county_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #Run the function
  partitions_mortality_riverside <- joined_data_original_mortality_riverside
  
  timing_result <- system.time({
    partitions_mortality_riverside$intersection_area <- mapply(calculate_intersection_area_riverside, 
                                                        pixel_ID = partitions_mortality_riverside$pixel_ID, 
                                                        county_ID = partitions_mortality_riverside$county_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #990 = ~16.5 minutes
  
  #Calculate ratios 
  partitions_mortality_riverside$area_ratio <- partitions_mortality_riverside$intersection_area / partitions_mortality_riverside$county_area 
  partitions_mortality_riverside$area_ratio_pixel <- partitions_mortality_riverside$intersection_area / partitions_mortality_riverside$pixel_area
  
  #### 3.3. Border pixels ####
  partitions_mortality_riverside %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_mortality_riverside_area_pixel
  
  partitions_mortality_riverside_area_pixel_border <- partitions_mortality_riverside_area_pixel$pixel_ID[partitions_mortality_riverside_area_pixel$sum_area_ratio_pixel < 0.99] #1306
  
  for (i in seq_along(partitions_mortality_riverside$pixel_ID)) {
    current_pixel_ID <- partitions_mortality_riverside$pixel_ID[i]
    if (current_pixel_ID %in% partitions_mortality_riverside_area_pixel_border) {
      matching_rows <- partitions_mortality_riverside[partitions_mortality_riverside$pixel_ID == current_pixel_ID, ]
      new_pixel_area <- sum(matching_rows$intersection_area)
      partitions_mortality_riverside$pixel_area[i] <- new_pixel_area
    } 
  }
  
  #Recalculate area pixel ratio
  partitions_mortality_riverside$area_ratio_pixel <- partitions_mortality_riverside$intersection_area / partitions_mortality_riverside$pixel_area
  
  #### 3.4. Incidence rates ####
  partitions_mortality_riverside %>%
    left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
              relationship = "many-to-many") %>%
    dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_riverside
  
  #Weighted mortality rate
  partitions_rate_riverside %>%
    group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
    mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
    ungroup() %>%
    dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
           Start.Age, End.Age, Type, weighted_incidence) %>%
    unique()-> partitions_rate_final_riverside
  
  #### 3.5. Print output table####
  #csv
  partitions_rate_final_riverside %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_riverside
  
  write.csv(partitions_rate_final_clean_riverside, row.names = FALSE, "output_data/BenMAP_files/1km/Riverside/Incidence_1km_2019_riverside.csv")
  
  #shapefiles
  partitions_rate_final_riverside %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_riverside
  
  st_write(partitions_rate_final_clean_allcauses_geometry_riverside, "output_data/shapefiles/1km/Riverside/1_km_mortality_2019_riverside.shp")

#### 4. PHOENIX ####
  #### 4.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c("04013", "04021")) -> grid_data_county_phoenix
  
  joined_data_original_mortality_phoenix <- st_join(grid_data_south_west_NAD83_phoenix_area, grid_data_county_phoenix, join = st_intersects)
  
  #### 4.2. Intersection ####
  #Function to calculate intersection area between pixels and blocks
  calculate_intersection_area_phoenix <- function(pixel_ID, county_ID) {
    pixel_geometry <- st_geometry(grid_data_south_west_NAD83_phoenix_area[grid_data_south_west_NAD83_phoenix_area$pixel_ID == pixel_ID, ])
    county_geometry <- st_geometry(grid_data_county_phoenix[grid_data_county_phoenix$county_ID == county_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #Run the function
  partitions_mortality_phoenix <- joined_data_original_mortality_phoenix
  
  timing_result <- system.time({
    partitions_mortality_phoenix$intersection_area <- mapply(calculate_intersection_area_phoenix, 
                                                               pixel_ID = partitions_mortality_phoenix$pixel_ID, 
                                                               county_ID = partitions_mortality_phoenix$county_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #539 = ~9 minutes
  
  #Calculate ratios 
  partitions_mortality_phoenix$area_ratio <- partitions_mortality_phoenix$intersection_area / partitions_mortality_phoenix$county_area 
  partitions_mortality_phoenix$area_ratio_pixel <- partitions_mortality_phoenix$intersection_area / partitions_mortality_phoenix$pixel_area
  
  #### 4.3. Border pixels ####
  partitions_mortality_phoenix %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_mortality_phoenix_area_pixel
  
  partitions_mortality_phoenix_area_pixel_border <- partitions_mortality_phoenix_area_pixel$pixel_ID[partitions_mortality_phoenix_area_pixel$sum_area_ratio_pixel < 0.99] #1041
  
  for (i in seq_along(partitions_mortality_phoenix$pixel_ID)) {
    current_pixel_ID <- partitions_mortality_phoenix$pixel_ID[i]
    if (current_pixel_ID %in% partitions_mortality_phoenix_area_pixel_border) {
      matching_rows <- partitions_mortality_phoenix[partitions_mortality_phoenix$pixel_ID == current_pixel_ID, ]
      new_pixel_area <- sum(matching_rows$intersection_area)
      partitions_mortality_phoenix$pixel_area[i] <- new_pixel_area
    } 
  }
  
  #Recalculate area pixel ratio
  partitions_mortality_phoenix$area_ratio_pixel <- partitions_mortality_phoenix$intersection_area / partitions_mortality_phoenix$pixel_area
  
  #### 4.4. Incidence rates ####
  partitions_mortality_phoenix %>%
    left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
              relationship = "many-to-many") %>%
    dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_phoenix
  
  #Weighted mortality rate
  partitions_rate_phoenix %>%
    group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
    mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
    ungroup() %>%
    dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
           Start.Age, End.Age, Type, weighted_incidence) %>%
    unique()-> partitions_rate_final_phoenix
  
  #### 4.5. Print output table####
  #csv
  partitions_rate_final_phoenix %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_phoenix
  
  write.csv(partitions_rate_final_clean_phoenix, row.names = FALSE, "output_data/BenMAP_files/1km/Phoenix/Incidence_1km_2019_phoenix.csv")
  
  #shapefiles
  partitions_rate_final_phoenix %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_phoenix
  
  st_write(partitions_rate_final_clean_allcauses_geometry_phoenix, "output_data/shapefiles/1km/Phoenix/1_km_mortality_2019_phoenix.shp")
  
#### 5. DALLAS ####
  #### 5.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c("48497", "48121", "48085", "48231", "48367", "48439", 
                            "48113", "48397", "48251", "48139", "48257")) -> grid_data_county_dallas
  
  joined_data_original_mortality_dallas <- st_join(grid_data_dallas_NAD83_filtered_area, grid_data_county_dallas, join = st_intersects)
  
  #### 5.2. Intersection ####
  #Function to calculate intersection area between pixels and blocks
  calculate_intersection_area_dallas <- function(pixel_ID, county_ID) {
    pixel_geometry <- st_geometry(grid_data_dallas_NAD83_filtered_area[grid_data_dallas_NAD83_filtered_area$pixel_ID == pixel_ID, ])
    county_geometry <- st_geometry(grid_data_county_dallas[grid_data_county_dallas$county_ID == county_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #Run the function
  partitions_mortality_dallas <- joined_data_original_mortality_dallas
  
  timing_result <- system.time({
    partitions_mortality_dallas$intersection_area <- mapply(calculate_intersection_area_dallas, 
                                                             pixel_ID = partitions_mortality_dallas$pixel_ID, 
                                                             county_ID = partitions_mortality_dallas$county_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #326 = ~5 minutes
  
  #Calculate ratios 
  partitions_mortality_dallas$area_ratio <- partitions_mortality_dallas$intersection_area / partitions_mortality_dallas$county_area 
  partitions_mortality_dallas$area_ratio_pixel <- partitions_mortality_dallas$intersection_area / partitions_mortality_dallas$pixel_area
  
  #### 5.3. Border pixels ####
  partitions_mortality_dallas %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_mortality_dallas_area_pixel
  
  partitions_mortality_dallas_area_pixel_border <- partitions_mortality_dallas_area_pixel$pixel_ID[partitions_mortality_dallas_area_pixel$sum_area_ratio_pixel < 0.99] #772
  
  for (i in seq_along(partitions_mortality_dallas$pixel_ID)) {
    current_pixel_ID <- partitions_mortality_dallas$pixel_ID[i]
    if (current_pixel_ID %in% partitions_mortality_dallas_area_pixel_border) {
      matching_rows <- partitions_mortality_dallas[partitions_mortality_dallas$pixel_ID == current_pixel_ID, ]
      new_pixel_area <- sum(matching_rows$intersection_area)
      partitions_mortality_dallas$pixel_area[i] <- new_pixel_area
    } 
  }
  
  #Recalculate area pixel ratio
  partitions_mortality_dallas$area_ratio_pixel <- partitions_mortality_dallas$intersection_area / partitions_mortality_dallas$pixel_area
  
  #### 5.4. Incidence rates ####
  partitions_mortality_dallas %>%
    left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
              relationship = "many-to-many") %>%
    dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_dallas
  
  #Weighted mortality rate
  partitions_rate_dallas %>%
    group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
    mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
    ungroup() %>%
    dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
           Start.Age, End.Age, Type, weighted_incidence) %>%
    unique()-> partitions_rate_final_dallas
  
  #### 5.5. Print output table####
  #csv
  partitions_rate_final_dallas %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_dallas
  
  write.csv(partitions_rate_final_clean_dallas, row.names = FALSE, "output_data/BenMAP_files/1km/Dallas/Incidence_1km_2019_dallas.csv")
  
  #shapefiles
  partitions_rate_final_dallas %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_dallas
  
  st_write(partitions_rate_final_clean_allcauses_geometry_dallas, "output_data/shapefiles/1km/Dallas/1_km_mortality_2019_dallas.shp")
  
#### 6. HOUSTON ####
  #### 6.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c("48039", "48157", "48015", "48167", "48201", "48473", 
                            "48071", "48291", "48339")) -> grid_data_county_houston
  
  joined_data_original_mortality_houston <- st_join(grid_data_houston_NAD83_filtered_area, grid_data_county_houston, join = st_intersects)
  
  #### 6.2. Intersection ####
  #Function to calculate intersection area between pixels and blocks
  calculate_intersection_area_houston <- function(pixel_ID, county_ID) {
    pixel_geometry <- st_geometry(grid_data_houston_NAD83_filtered_area[grid_data_houston_NAD83_filtered_area$pixel_ID == pixel_ID, ])
    county_geometry <- st_geometry(grid_data_county_houston[grid_data_county_houston$county_ID == county_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #Run the function
  partitions_mortality_houston <- joined_data_original_mortality_houston
  
  timing_result <- system.time({
    partitions_mortality_houston$intersection_area <- mapply(calculate_intersection_area_houston, 
                                                            pixel_ID = partitions_mortality_houston$pixel_ID, 
                                                            county_ID = partitions_mortality_houston$county_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #339 = ~5.6minutes
  
  #Calculate ratios 
  partitions_mortality_houston$area_ratio <- partitions_mortality_houston$intersection_area / partitions_mortality_houston$county_area 
  partitions_mortality_houston$area_ratio_pixel <- partitions_mortality_houston$intersection_area / partitions_mortality_houston$pixel_area
  
  #### 6.3. Border pixels ####
  partitions_mortality_houston %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_mortality_houston_area_pixel
  
  partitions_mortality_houston_area_pixel_border <- partitions_mortality_houston_area_pixel$pixel_ID[partitions_mortality_houston_area_pixel$sum_area_ratio_pixel < 0.99] #1405
  
  for (i in seq_along(partitions_mortality_houston$pixel_ID)) {
    current_pixel_ID <- partitions_mortality_houston$pixel_ID[i]
    if (current_pixel_ID %in% partitions_mortality_houston_area_pixel_border) {
      matching_rows <- partitions_mortality_houston[partitions_mortality_houston$pixel_ID == current_pixel_ID, ]
      new_pixel_area <- sum(matching_rows$intersection_area)
      partitions_mortality_houston$pixel_area[i] <- new_pixel_area
    } 
  }
  
  #Recalculate area pixel ratio
  partitions_mortality_houston$area_ratio_pixel <- partitions_mortality_houston$intersection_area / partitions_mortality_houston$pixel_area
  
  #### 6.4. Incidence rates ####
  partitions_mortality_houston %>%
    left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
              relationship = "many-to-many") %>%
    dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_houston
  
  #Weighted mortality rate
  partitions_rate_houston %>%
    group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
    mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
    ungroup() %>%
    dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
           Start.Age, End.Age, Type, weighted_incidence) %>%
    unique()-> partitions_rate_final_houston
  
  #### 6.5. Print output table####
  #csv
  partitions_rate_final_houston %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_houston
  
  write.csv(partitions_rate_final_clean_houston, row.names = FALSE, "output_data/BenMAP_files/1km/Houston/Incidence_1km_2019_houston.csv")
  
  #shapefiles
  partitions_rate_final_houston %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_houston
  
  st_write(partitions_rate_final_clean_allcauses_geometry_houston, "output_data/shapefiles/1km/Houston/1_km_mortality_2019_houston.shp")  

#### 7. MIAMI ####
  #### 7.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c("12099", "12011", "12086")) -> grid_data_county_miami
  
  joined_data_original_mortality_miami <- st_join(grid_data_miami_NAD83_filtered_area, grid_data_county_miami, join = st_intersects)
  
  #### 7.2. Intersection ####
  #Function to calculate intersection area between pixels and blocks
  calculate_intersection_area_miami <- function(pixel_ID, county_ID) {
    pixel_geometry <- st_geometry(grid_data_miami_NAD83_filtered_area[grid_data_miami_NAD83_filtered_area$pixel_ID == pixel_ID, ])
    county_geometry <- st_geometry(grid_data_county_miami[grid_data_county_miami$county_ID == county_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #Run the function
  partitions_mortality_miami <- joined_data_original_mortality_miami
  
  timing_result <- system.time({
    partitions_mortality_miami$intersection_area <- mapply(calculate_intersection_area_miami, 
                                                             pixel_ID = partitions_mortality_miami$pixel_ID, 
                                                             county_ID = partitions_mortality_miami$county_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #200 = ~3.4 minutes
  
  #Calculate ratios 
  partitions_mortality_miami$area_ratio <- partitions_mortality_miami$intersection_area / partitions_mortality_miami$county_area 
  partitions_mortality_miami$area_ratio_pixel <- partitions_mortality_miami$intersection_area / partitions_mortality_miami$pixel_area
  
  #### 7.3. Border pixels ####
  partitions_mortality_miami %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_mortality_miami_area_pixel
  
  partitions_mortality_miami_area_pixel_border <- partitions_mortality_miami_area_pixel$pixel_ID[partitions_mortality_miami_area_pixel$sum_area_ratio_pixel < 0.99] #762
  
  for (i in seq_along(partitions_mortality_miami$pixel_ID)) {
    current_pixel_ID <- partitions_mortality_miami$pixel_ID[i]
    if (current_pixel_ID %in% partitions_mortality_miami_area_pixel_border) {
      matching_rows <- partitions_mortality_miami[partitions_mortality_miami$pixel_ID == current_pixel_ID, ]
      new_pixel_area <- sum(matching_rows$intersection_area)
      partitions_mortality_miami$pixel_area[i] <- new_pixel_area
    } 
  }
  
  #Recalculate area pixel ratio
  partitions_mortality_miami$area_ratio_pixel <- partitions_mortality_miami$intersection_area / partitions_mortality_miami$pixel_area
  
  #### 7.4. Incidence rates ####
  partitions_mortality_miami %>%
    left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
              relationship = "many-to-many") %>%
    dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_miami
  
  #Weighted mortality rate
  partitions_rate_miami %>%
    group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
    mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
    ungroup() %>%
    dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
           Start.Age, End.Age, Type, weighted_incidence) %>%
    unique()-> partitions_rate_final_miami
  
  #### 7.5. Print output table####
  #csv
  partitions_rate_final_miami %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_miami
  
  write.csv(partitions_rate_final_clean_miami, row.names = FALSE, "output_data/BenMAP_files/1km/Miami/Incidence_1km_2019_miami.csv")
  
  #shapefiles
  partitions_rate_final_miami %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_miami
  
  st_write(partitions_rate_final_clean_allcauses_geometry_miami, "output_data/shapefiles/1km/Miami/1_km_mortality_2019_miami.shp")  
  
#### 8. ATLANTA ####
  #### 8.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c("13143", "13045", "13149", "13015", "13223", "13097","13077", "13199", "13227",
                            "13227", "13057", "13067", "13121", "13113","13255", "13231", "13085", "13117", 
                            "13135", "13089", "13063", "13151", "13035", "13171", "13247", "13013", "13297",
                            "13217","13211", "13159")) -> grid_data_county_atl
  
  joined_data_original_mortality_atl <- st_join(grid_data_atl_NAD83_filtered_area, grid_data_county_atl, join = st_intersects)
  
  #### 8.2. Intersection ####
  #Function to calculate intersection area between pixels and blocks
  calculate_intersection_area_atl <- function(pixel_ID, county_ID) {
    pixel_geometry <- st_geometry(grid_data_atl_NAD83_filtered_area[grid_data_atl_NAD83_filtered_area$pixel_ID == pixel_ID, ])
    county_geometry <- st_geometry(grid_data_county_atl[grid_data_county_atl$county_ID == county_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #Run the function
  partitions_mortality_atl <- joined_data_original_mortality_atl
  
  timing_result <- system.time({
    partitions_mortality_atl$intersection_area <- mapply(calculate_intersection_area_atl, 
                                                           pixel_ID = partitions_mortality_atl$pixel_ID, 
                                                           county_ID = partitions_mortality_atl$county_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #327 = ~5.5 minutes
  
  #Calculate ratios 
  partitions_mortality_atl$area_ratio <- partitions_mortality_atl$intersection_area / partitions_mortality_atl$county_area 
  partitions_mortality_atl$area_ratio_pixel <- partitions_mortality_atl$intersection_area / partitions_mortality_atl$pixel_area
  
  #### 8.3. Border pixels ####
  partitions_mortality_atl %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_mortality_atl_area_pixel
  
  partitions_mortality_atl_area_pixel_border <- partitions_mortality_atl_area_pixel$pixel_ID[partitions_mortality_atl_area_pixel$sum_area_ratio_pixel < 0.99] #971
  
  for (i in seq_along(partitions_mortality_atl$pixel_ID)) {
    current_pixel_ID <- partitions_mortality_atl$pixel_ID[i]
    if (current_pixel_ID %in% partitions_mortality_atl_area_pixel_border) {
      matching_rows <- partitions_mortality_atl[partitions_mortality_atl$pixel_ID == current_pixel_ID, ]
      new_pixel_area <- sum(matching_rows$intersection_area)
      partitions_mortality_atl$pixel_area[i] <- new_pixel_area
    } 
  }
  
  #Recalculate area pixel ratio
  partitions_mortality_atl$area_ratio_pixel <- partitions_mortality_atl$intersection_area / partitions_mortality_atl$pixel_area
  
  #### 8.4. Incidence rates ####
  partitions_mortality_atl %>%
    left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
              relationship = "many-to-many") %>%
    dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_atl
  
  #Weighted mortality rate
  partitions_rate_atl %>%
    group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
    mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
    ungroup() %>%
    dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
           Start.Age, End.Age, Type, weighted_incidence) %>%
    unique()-> partitions_rate_final_atl
  
  #### 8.5. Print output table####
  #csv
  partitions_rate_final_atl %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_atl
  
  write.csv(partitions_rate_final_clean_atl, row.names = FALSE, "output_data/BenMAP_files/1km/Atlanta/Incidence_1km_2019_atlanta.csv")
  
  #shapefiles
  partitions_rate_final_atl %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_atl
  
  st_write(partitions_rate_final_clean_allcauses_geometry_atl, "output_data/shapefiles/1km/Atlanta/1_km_mortality_2019_atlanta.shp")  
  
#### 9. CHICAGO ####
  #### 9.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c("17097", "17197", "17031", "17043", "17093", "17089", "17037", 
                            "17063","17111", "18073", "18127", "18111", "18089", "55059")) -> grid_data_county_chicago
  
  joined_data_original_mortality_chicago <- st_join(grid_data_chicago_NAD83_filtered_area, grid_data_county_chicago, join = st_intersects)
  
  #### 9.2. Intersection ####
  #Function to calculate intersection area between pixels and blocks
  calculate_intersection_area_chicago <- function(pixel_ID, county_ID) {
    pixel_geometry <- st_geometry(grid_data_chicago_NAD83_filtered_area[grid_data_chicago_NAD83_filtered_area$pixel_ID == pixel_ID, ])
    county_geometry <- st_geometry(grid_data_county_chicago[grid_data_county_chicago$county_ID == county_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #Run the function
  partitions_mortality_chicago <- joined_data_original_mortality_chicago
  
  timing_result <- system.time({
    partitions_mortality_chicago$intersection_area <- mapply(calculate_intersection_area_chicago, 
                                                         pixel_ID = partitions_mortality_chicago$pixel_ID, 
                                                         county_ID = partitions_mortality_chicago$county_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #257 = ~4 minutes
  
  #Calculate ratios 
  partitions_mortality_chicago$area_ratio <- partitions_mortality_chicago$intersection_area / partitions_mortality_chicago$county_area 
  partitions_mortality_chicago$area_ratio_pixel <- partitions_mortality_chicago$intersection_area / partitions_mortality_chicago$pixel_area
  
  #### 9.3. Border pixels ####
  partitions_mortality_chicago %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_mortality_chicago_area_pixel
  
  partitions_mortality_chicago_area_pixel_border <- partitions_mortality_chicago_area_pixel$pixel_ID[partitions_mortality_chicago_area_pixel$sum_area_ratio_pixel < 0.99] #872
  
  for (i in seq_along(partitions_mortality_chicago$pixel_ID)) {
    current_pixel_ID <- partitions_mortality_chicago$pixel_ID[i]
    if (current_pixel_ID %in% partitions_mortality_chicago_area_pixel_border) {
      matching_rows <- partitions_mortality_chicago[partitions_mortality_chicago$pixel_ID == current_pixel_ID, ]
      new_pixel_area <- sum(matching_rows$intersection_area)
      partitions_mortality_chicago$pixel_area[i] <- new_pixel_area
    } 
  }
  
  #Recalculate area pixel ratio
  partitions_mortality_chicago$area_ratio_pixel <- partitions_mortality_chicago$intersection_area / partitions_mortality_chicago$pixel_area
  
  #### 9.4. Incidence rates ####
  partitions_mortality_chicago %>%
    left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
              relationship = "many-to-many") %>%
    dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_chicago
  
  #Weighted mortality rate
  partitions_rate_chicago %>%
    group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
    mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
    ungroup() %>%
    dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
           Start.Age, End.Age, Type, weighted_incidence) %>%
    unique()-> partitions_rate_final_chicago
  
  #### 9.5. Print output table####
  #csv
  partitions_rate_final_chicago %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_chicago
  
  write.csv(partitions_rate_final_clean_chicago, row.names = FALSE, "output_data/BenMAP_files/1km/Chicago/Incidence_1km_2019_chicago.csv")
  
  #shapefiles
  partitions_rate_final_chicago %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_chicago
  
  st_write(partitions_rate_final_clean_allcauses_geometry_chicago, "output_data/shapefiles/1km/Chicago/1_km_mortality_2019_chicago.shp")  
  
#### 10. WASHINGTON DC ####
  #### 10.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c("24021","24017", "24031", "24033", "24009", "51600", "51061",
                            "51153", "51179", "51059", "51013","51683", "51107", "51047", 
                            "51043", "51510", "51610", "51685", "51187","51157", "51113", 
                            "51177", "51630", "11001", "54037")) -> grid_data_county_wash
  
  joined_data_original_mortality_wash <- st_join(grid_data_wash_NAD83_filtered_area, grid_data_county_wash, join = st_intersects)
  
  #### 10.2. Intersection ####
  #Function to calculate intersection area between pixels and blocks
  calculate_intersection_area_wash <- function(pixel_ID, county_ID) {
    pixel_geometry <- st_geometry(grid_data_wash_NAD83_filtered_area[grid_data_wash_NAD83_filtered_area$pixel_ID == pixel_ID, ])
    county_geometry <- st_geometry(grid_data_county_wash[grid_data_county_wash$county_ID == county_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #Run the function
  partitions_mortality_wash <- joined_data_original_mortality_wash
  
  timing_result <- system.time({
    partitions_mortality_wash$intersection_area <- mapply(calculate_intersection_area_wash, 
                                                             pixel_ID = partitions_mortality_wash$pixel_ID, 
                                                             county_ID = partitions_mortality_wash$county_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #250 = ~4 minutes
  
  #Calculate ratios 
  partitions_mortality_wash$area_ratio <- partitions_mortality_wash$intersection_area / partitions_mortality_wash$county_area 
  partitions_mortality_wash$area_ratio_pixel <- partitions_mortality_wash$intersection_area / partitions_mortality_wash$pixel_area
  
  #### 10.3. Border pixels ####
  partitions_mortality_wash %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_mortality_wash_area_pixel
  
  partitions_mortality_wash_area_pixel_border <- partitions_mortality_wash_area_pixel$pixel_ID[partitions_mortality_wash_area_pixel$sum_area_ratio_pixel < 0.99] #1180
  
  for (i in seq_along(partitions_mortality_wash$pixel_ID)) {
    current_pixel_ID <- partitions_mortality_wash$pixel_ID[i]
    if (current_pixel_ID %in% partitions_mortality_wash_area_pixel_border) {
      matching_rows <- partitions_mortality_wash[partitions_mortality_wash$pixel_ID == current_pixel_ID, ]
      new_pixel_area <- sum(matching_rows$intersection_area)
      partitions_mortality_wash$pixel_area[i] <- new_pixel_area
    } 
  }
  
  #Recalculate area pixel ratio
  partitions_mortality_wash$area_ratio_pixel <- partitions_mortality_wash$intersection_area / partitions_mortality_wash$pixel_area
  
  #### 10.4. Incidence rates ####
  partitions_mortality_wash %>%
    left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
              relationship = "many-to-many") %>%
    dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_wash
  
  #Weighted mortality rate
  partitions_rate_wash %>%
    group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
    mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
    ungroup() %>%
    dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
           Start.Age, End.Age, Type, weighted_incidence) %>%
    unique()-> partitions_rate_final_wash
  
  #### 10.5. Print output table####
  #csv
  partitions_rate_final_wash %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_wash
  
  write.csv(partitions_rate_final_clean_wash, row.names = FALSE, "output_data/BenMAP_files/1km/DC/Incidence_1km_2019_washington.csv")
  
  #shapefiles
  partitions_rate_final_wash %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_wash
  
  st_write(partitions_rate_final_clean_allcauses_geometry_wash, "output_data/shapefiles/1km/DC/1_km_mortality_2019_washington.shp")  
  
#### 11. PHILADELPHIA ####
  #### 11.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c(  "42101", "42029", "42045", "42017", "42091", "10003", "34007", 
                              "34015", "34005", "34033", "24015")) -> grid_data_county_phil
  
  joined_data_original_mortality_phil <- st_join(grid_data_phil_NAD83_filtered_area, grid_data_county_phil, join = st_intersects)
  
  #### 11.2. Intersection ####
  #Function to calculate intersection area between pixels and blocks
  calculate_intersection_area_phil <- function(pixel_ID, county_ID) {
    pixel_geometry <- st_geometry(grid_data_phil_NAD83_filtered_area[grid_data_phil_NAD83_filtered_area$pixel_ID == pixel_ID, ])
    county_geometry <- st_geometry(grid_data_county_phil[grid_data_county_phil$county_ID == county_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #Run the function
  partitions_mortality_phil <- joined_data_original_mortality_phil
  
  timing_result <- system.time({
    partitions_mortality_phil$intersection_area <- mapply(calculate_intersection_area_phil, 
                                                          pixel_ID = partitions_mortality_phil$pixel_ID, 
                                                          county_ID = partitions_mortality_phil$county_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #180 = ~3 minutes
  
  #Calculate ratios 
  partitions_mortality_phil$area_ratio <- partitions_mortality_phil$intersection_area / partitions_mortality_phil$county_area 
  partitions_mortality_phil$area_ratio_pixel <- partitions_mortality_phil$intersection_area / partitions_mortality_phil$pixel_area
  
  #### 11.3. Border pixels ####
  partitions_mortality_phil %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_mortality_phil_area_pixel
  
  partitions_mortality_phil_area_pixel_border <- partitions_mortality_phil_area_pixel$pixel_ID[partitions_mortality_phil_area_pixel$sum_area_ratio_pixel < 0.99] #897
  
  for (i in seq_along(partitions_mortality_phil$pixel_ID)) {
    current_pixel_ID <- partitions_mortality_phil$pixel_ID[i]
    if (current_pixel_ID %in% partitions_mortality_phil_area_pixel_border) {
      matching_rows <- partitions_mortality_phil[partitions_mortality_phil$pixel_ID == current_pixel_ID, ]
      new_pixel_area <- sum(matching_rows$intersection_area)
      partitions_mortality_phil$pixel_area[i] <- new_pixel_area
    } 
  }
  
  #Recalculate area pixel ratio
  partitions_mortality_phil$area_ratio_pixel <- partitions_mortality_phil$intersection_area / partitions_mortality_phil$pixel_area
  
  #### 11.4. Incidence rates ####
  partitions_mortality_phil %>%
    left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
              relationship = "many-to-many") %>%
    dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_phil
  
  #Weighted mortality rate
  partitions_rate_phil %>%
    group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
    mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
    ungroup() %>%
    dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
           Start.Age, End.Age, Type, weighted_incidence) %>%
    unique()-> partitions_rate_final_phil
  
  #### 11.5. Print output table####
  #csv
  partitions_rate_final_phil %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_phil
  
  write.csv(partitions_rate_final_clean_phil, row.names = FALSE, "output_data/BenMAP_files/1km/Philadelphia/Incidence_1km_2019_philadelphia.csv")
  
  #shapefiles
  partitions_rate_final_phil %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_phil
  
  st_write(partitions_rate_final_clean_allcauses_geometry_phil, "output_data/shapefiles/1km/Philadelphia/1_km_mortality_2019_philadelphia.shp")  
  
#### 12. NEW YORK ####
  #### 12.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c(    "42103", "34037", "34013", "34031","34035","34027", "34023", "34003", "34029", 
                                "34017", "34039", "34025", "34019", "36119", "36081", "36047", "36061", "36005", 
                                "36085", "36087", "36059", "36103","36079")) -> grid_data_county_NY
  
  joined_data_original_mortality_NY <- st_join(grid_data_NY_NAD83_filtered_area, grid_data_county_NY, join = st_intersects)
  
  #### 12.2. Intersection ####
  #Function to calculate intersection area between pixels and blocks
  calculate_intersection_area_NY <- function(pixel_ID, county_ID) {
    pixel_geometry <- st_geometry(grid_data_NY_NAD83_filtered_area[grid_data_NY_NAD83_filtered_area$pixel_ID == pixel_ID, ])
    county_geometry <- st_geometry(grid_data_county_NY[grid_data_county_NY$county_ID == county_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #Run the function
  partitions_mortality_NY <- joined_data_original_mortality_NY
  
  timing_result <- system.time({
    partitions_mortality_NY$intersection_area <- mapply(calculate_intersection_area_NY, 
                                                          pixel_ID = partitions_mortality_NY$pixel_ID, 
                                                          county_ID = partitions_mortality_NY$county_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #280 = ~5 minutes
  
  #Calculate ratios 
  partitions_mortality_NY$area_ratio <- partitions_mortality_NY$intersection_area / partitions_mortality_NY$county_area 
  partitions_mortality_NY$area_ratio_pixel <- partitions_mortality_NY$intersection_area / partitions_mortality_NY$pixel_area
  
  #### 12.3. Border pixels ####
  partitions_mortality_NY %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_mortality_NY_area_pixel
  
  partitions_mortality_NY_area_pixel_border <- partitions_mortality_NY_area_pixel$pixel_ID[partitions_mortality_NY_area_pixel$sum_area_ratio_pixel < 0.99] #1818
  
  for (i in seq_along(partitions_mortality_NY$pixel_ID)) {
    current_pixel_ID <- partitions_mortality_NY$pixel_ID[i]
    if (current_pixel_ID %in% partitions_mortality_NY_area_pixel_border) {
      matching_rows <- partitions_mortality_NY[partitions_mortality_NY$pixel_ID == current_pixel_ID, ]
      new_pixel_area <- sum(matching_rows$intersection_area)
      partitions_mortality_NY$pixel_area[i] <- new_pixel_area
    } 
  }
  
  #Recalculate area pixel ratio
  partitions_mortality_NY$area_ratio_pixel <- partitions_mortality_NY$intersection_area / partitions_mortality_NY$pixel_area
  
  #### 12.4. Incidence rates ####
  partitions_mortality_NY %>%
    left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
              relationship = "many-to-many") %>%
    dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_NY
  
  #Weighted mortality rate
  partitions_rate_NY %>%
    group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
    mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
    ungroup() %>%
    dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
           Start.Age, End.Age, Type, weighted_incidence) %>%
    unique()-> partitions_rate_final_NY
  
  #### 12.5. Print output table####
  #csv
  partitions_rate_final_NY %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_NY
  
  write.csv(partitions_rate_final_clean_NY, row.names = FALSE, "output_data/BenMAP_files/1km/NY/Incidence_1km_2019_NY.csv")
  
  #shapefiles
  partitions_rate_final_NY %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_NY
  
  st_write(partitions_rate_final_clean_allcauses_geometry_NY, "output_data/shapefiles/1km/NY/1_km_mortality_2019_NY.shp")  

#### 13. BOSTON ####
  #### 13.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c( "33017", "33015","25025", "25021","25009", "25023", "25017")) -> grid_data_county_boston
  
  joined_data_original_mortality_boston <- st_join(grid_data_boston_NAD83_filtered_area, grid_data_county_boston, join = st_intersects)
  
  #### 13.2. Intersection ####
  #Function to calculate intersection area between pixels and blocks
  calculate_intersection_area_boston <- function(pixel_ID, county_ID) {
    pixel_geometry <- st_geometry(grid_data_boston_NAD83_filtered_area[grid_data_boston_NAD83_filtered_area$pixel_ID == pixel_ID, ])
    county_geometry <- st_geometry(grid_data_county_boston[grid_data_county_boston$county_ID == county_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #Run the function
  partitions_mortality_boston <- joined_data_original_mortality_boston
  
  timing_result <- system.time({
    partitions_mortality_boston$intersection_area <- mapply(calculate_intersection_area_boston, 
                                                        pixel_ID = partitions_mortality_boston$pixel_ID, 
                                                        county_ID = partitions_mortality_boston$county_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #140 = ~2 minutes
  
  #Calculate ratios 
  partitions_mortality_boston$area_ratio <- partitions_mortality_boston$intersection_area / partitions_mortality_boston$county_area 
  partitions_mortality_boston$area_ratio_pixel <- partitions_mortality_boston$intersection_area / partitions_mortality_boston$pixel_area
  
  #### 13.3. Border pixels ####
  partitions_mortality_boston %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_mortality_boston_area_pixel
  
  partitions_mortality_boston_area_pixel_border <- partitions_mortality_boston_area_pixel$pixel_ID[partitions_mortality_boston_area_pixel$sum_area_ratio_pixel < 0.99] #1137
  
  for (i in seq_along(partitions_mortality_boston$pixel_ID)) {
    current_pixel_ID <- partitions_mortality_boston$pixel_ID[i]
    if (current_pixel_ID %in% partitions_mortality_boston_area_pixel_border) {
      matching_rows <- partitions_mortality_boston[partitions_mortality_boston$pixel_ID == current_pixel_ID, ]
      new_pixel_area <- sum(matching_rows$intersection_area)
      partitions_mortality_boston$pixel_area[i] <- new_pixel_area
    } 
  }
  
  #Recalculate area pixel ratio
  partitions_mortality_boston$area_ratio_pixel <- partitions_mortality_boston$intersection_area / partitions_mortality_boston$pixel_area
  
  #### 13.4. Incidence rates ####
  partitions_mortality_boston %>%
    left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
              relationship = "many-to-many") %>%
    dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_boston
  
  #Weighted mortality rate
  partitions_rate_boston %>%
    group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
    mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
    ungroup() %>%
    dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
           Start.Age, End.Age, Type, weighted_incidence) %>%
    unique()-> partitions_rate_final_boston
  
  #### 13.5. Print output table####
  #csv
  partitions_rate_final_boston %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_boston
  
  write.csv(partitions_rate_final_clean_boston, row.names = FALSE, "output_data/BenMAP_files/1km/Boston/Incidence_1km_2019_boston.csv")
  
  #shapefiles
  partitions_rate_final_boston %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_boston
  
  st_write(partitions_rate_final_clean_allcauses_geometry_boston, "output_data/shapefiles/1km/Boston/1_km_mortality_2019_boston.shp")  

#### 14. DETROIT ####
  #### 14.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c("26163", "26147","26099", "26125", "26087", "26093")) -> grid_data_county_detroit
  
  joined_data_original_mortality_detroit <- st_join(grid_data_detroit_NAD83_filtered_area, grid_data_county_detroit, join = st_intersects)
  
  #### 14.2. Intersection ####
  #Function to calculate intersection area between pixels and blocks
  calculate_intersection_area_detroit <- function(pixel_ID, county_ID) {
    pixel_geometry <- st_geometry(grid_data_detroit_NAD83_filtered_area[grid_data_detroit_NAD83_filtered_area$pixel_ID == pixel_ID, ])
    county_geometry <- st_geometry(grid_data_county_detroit[grid_data_county_detroit$county_ID == county_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #Run the function
  partitions_mortality_detroit <- joined_data_original_mortality_detroit
  
  timing_result <- system.time({
    partitions_mortality_detroit$intersection_area <- mapply(calculate_intersection_area_detroit, 
                                                            pixel_ID = partitions_mortality_detroit$pixel_ID, 
                                                            county_ID = partitions_mortality_detroit$county_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #141 = ~2 minutes
  
  #Calculate ratios 
  partitions_mortality_detroit$area_ratio <- partitions_mortality_detroit$intersection_area / partitions_mortality_detroit$county_area 
  partitions_mortality_detroit$area_ratio_pixel <- partitions_mortality_detroit$intersection_area / partitions_mortality_detroit$pixel_area
  
  #### 14.3. Border pixels ####
  partitions_mortality_detroit %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_mortality_detroit_area_pixel
  
  partitions_mortality_detroit_area_pixel_border <- partitions_mortality_detroit_area_pixel$pixel_ID[partitions_mortality_detroit_area_pixel$sum_area_ratio_pixel < 0.99] #682
  
  for (i in seq_along(partitions_mortality_detroit$pixel_ID)) {
    current_pixel_ID <- partitions_mortality_detroit$pixel_ID[i]
    if (current_pixel_ID %in% partitions_mortality_detroit_area_pixel_border) {
      matching_rows <- partitions_mortality_detroit[partitions_mortality_detroit$pixel_ID == current_pixel_ID, ]
      new_pixel_area <- sum(matching_rows$intersection_area)
      partitions_mortality_detroit$pixel_area[i] <- new_pixel_area
    } 
  }
  
  #Recalculate area pixel ratio
  partitions_mortality_detroit$area_ratio_pixel <- partitions_mortality_detroit$intersection_area / partitions_mortality_detroit$pixel_area
  
  #### 14.4. Incidence rates ####
  partitions_mortality_detroit %>%
    left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
              relationship = "many-to-many") %>%
    dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_detroit
  
  #Weighted mortality rate
  partitions_rate_detroit %>%
    group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
    mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
    ungroup() %>%
    dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
           Start.Age, End.Age, Type, weighted_incidence) %>%
    unique()-> partitions_rate_final_detroit
  
  #### 14.5. Print output table####
  #csv
  partitions_rate_final_detroit %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_detroit
  
  write.csv(partitions_rate_final_clean_detroit, row.names = FALSE, "output_data/BenMAP_files/1km/Detroit/Incidence_1km_2019_detroit.csv")
  
  #shapefiles
  partitions_rate_final_detroit %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_detroit
  
  st_write(partitions_rate_final_clean_allcauses_geometry_detroit, "output_data/shapefiles/1km/Detroit/1_km_mortality_2019_detroit.shp")  
  
#### 15. SAN FRANCISCO ####
  #### 15.1. Join files ####
  grid_data_county %>%
    filter(county_ID %in% c("06001", "06013", "06041", "06081", "06075")) -> grid_data_county_SF
  
  joined_data_original_mortality_SF <- st_join(grid_data_SF_NAD83_filtered_area, grid_data_county_SF, join = st_intersects)
  
  #### 15.2. Intersection ####
  #Function to calculate intersection area between pixels and blocks
  calculate_intersection_area_SF <- function(pixel_ID, county_ID) {
    pixel_geometry <- st_geometry(grid_data_SF_NAD83_filtered_area[grid_data_SF_NAD83_filtered_area$pixel_ID == pixel_ID, ])
    county_geometry <- st_geometry(grid_data_county_SF[grid_data_county_SF$county_ID == county_ID, ])
    
    intersection_area <- round(st_area(st_intersection(pixel_geometry, county_geometry)), digits = 5)
    return(intersection_area)
  }
  
  #Run the function
  partitions_mortality_SF <- joined_data_original_mortality_SF
  
  timing_result <- system.time({
    partitions_mortality_SF$intersection_area <- mapply(calculate_intersection_area_SF, 
                                                             pixel_ID = partitions_mortality_SF$pixel_ID, 
                                                             county_ID = partitions_mortality_SF$county_ID)
  })
  cat("Elapsed Time:", timing_result["elapsed"], "\n") #95 = ~1.5 minutes
  
  #Calculate ratios 
  partitions_mortality_SF$area_ratio <- partitions_mortality_SF$intersection_area / partitions_mortality_SF$county_area 
  partitions_mortality_SF$area_ratio_pixel <- partitions_mortality_SF$intersection_area / partitions_mortality_SF$pixel_area
  
  #### 15.3. Border pixels ####
  partitions_mortality_SF %>%
    group_by(pixel_ID) %>%
    summarise(sum_area_ratio_pixel = sum(area_ratio_pixel)) %>%
    ungroup() %>%
    st_drop_geometry()-> partitions_mortality_SF_area_pixel
  
  partitions_mortality_SF_area_pixel_border <- partitions_mortality_SF_area_pixel$pixel_ID[partitions_mortality_SF_area_pixel$sum_area_ratio_pixel < 0.99] #939
  
  for (i in seq_along(partitions_mortality_SF$pixel_ID)) {
    current_pixel_ID <- partitions_mortality_SF$pixel_ID[i]
    if (current_pixel_ID %in% partitions_mortality_SF_area_pixel_border) {
      matching_rows <- partitions_mortality_SF[partitions_mortality_SF$pixel_ID == current_pixel_ID, ]
      new_pixel_area <- sum(matching_rows$intersection_area)
      partitions_mortality_SF$pixel_area[i] <- new_pixel_area
    } 
  }
  
  #Recalculate area pixel ratio
  partitions_mortality_SF$area_ratio_pixel <- partitions_mortality_SF$intersection_area / partitions_mortality_SF$pixel_area
  
  #### 15.4. Incidence rates ####
  partitions_mortality_SF %>%
    left_join(incidence_2019, by = c("benmap_ROW" = "Row", "benmap_COL" = "Column"),
              relationship = "many-to-many") %>%
    dplyr::select(-benmap_ROW, -benmap_COL) -> partitions_rate_SF
  
  #Weighted mortality rate
  partitions_rate_SF %>%
    group_by(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, Start.Age, End.Age, Type) %>%
    mutate(weighted_incidence = sum(Value_2019 * area_ratio_pixel)) %>%
    ungroup() %>%
    dplyr::select(pixel_ID, row, col, Endpoint.Group, Endpoint, Race, Gender, Ethnicity, 
           Start.Age, End.Age, Type, weighted_incidence) %>%
    unique()-> partitions_rate_final_SF
  
  #### 15.5. Print output table####
  #csv
  partitions_rate_final_SF %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    st_drop_geometry()-> partitions_rate_final_clean_SF
  
  write.csv(partitions_rate_final_clean_SF, row.names = FALSE, "output_data/BenMAP_files/1km/SF/Incidence_1km_2019_SF.csv")
  
  #shapefiles
  partitions_rate_final_SF %>%
    dplyr::select(-pixel_ID) %>%
    rename(Row = row, Column = col, Value = weighted_incidence) %>%
    filter(Endpoint == "Mortality, All Cause") -> partitions_rate_final_clean_allcauses_geometry_SF
  
  st_write(partitions_rate_final_clean_allcauses_geometry_SF, "output_data/shapefiles/1km/SF/1_km_mortality_2019_SF.shp")  
  
  
#------------------------------------------------- SECTION A4: 1 KM POLLUTION -------------------------------------------------
#Here we prepare pollution files to csv for BenMAP. We need to get the mean of 365 days, and we need to assign exact row and columns to our data. We will have one file per period and per scenario (3 total: 2019, 2050 HIHG, 2050 LOW) 
#WRF-CMAQ files form Jing for daily average PM2.5
  #WE ARE USING THE ADJUSTED VALUES !!!
#### 1. SEATTLE ####
  #### 1.1. 2019 ####
    # 1.1.1 Get mean value 
        #Read in netcdf file with daily-average PM2.5 concentrations (ug/m3) for 2019
        #365 days, 399 rows, 612 columns, pixel size is 1000m2
        # Read the NetCDF file using stack
        seattle_nc_1km_stack_2019 <- stack("data/WRF-CMAQ/1km_V2/1_seattle/Dailymean.1km.BASE.2019.PM25_AVG.nc", varname = "PM25_AVG")
        # Set the CRS explicitly based on the information you obtained from gdalinfo
        crs(seattle_nc_1km_stack_2019)
        
        # Calculate the mean across all bands (365 days) for each cell
        seattle_mean_raster_2019 <- calc(seattle_nc_1km_stack_2019, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      
        # Plot the mean raster
        plot(seattle_mean_raster_2019, col = rev(magma(20)), main = " Seattle 2019 Yearly Mean PM2.5 Concentrations (ug/m3)")
        
    # 1.1.2. Get rows and columns matching
        #1. make sure geometries align
          bbox <- st_bbox(grid_data_Seattle$geometry)
          
          #Calculate the number of columns and rows 
          num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
          num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
          
          # 1. Check the CRS
          st_crs(grid_data_Seattle)
          crs(seattle_mean_raster_2019)
          
          #2. Print extent information
          print(extent(seattle_mean_raster_2019))
          print(st_bbox(grid_data_Seattle))
          
          #3. Check resolution
          res(seattle_mean_raster_2019)
          # Get the bounding box of the grid_data
          bbox <- st_bbox(grid_data_Seattle)
          # Calculate the resolution
          resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                          (bbox$ymax - bbox$ymin) / num_rows)
          # Print the resolution
          print(resolution)
        
        # 2. Set the CRS for mean_raster_2019
        crs(seattle_mean_raster_2019) <- st_crs(grid_data_Seattle)$proj4string
        
        # 3. Set the extent of mean_raster_aligned to match grid_data
        extent(seattle_mean_raster_2019) <- extent(grid_data_Seattle)
        
        #4. Filter pixels to keep
        grid_data_Seattle %>%
          filter(pixel_ID %in% seattle_pixels) -> grid_data_Seattle_filtered
        
    # 1.1.3. Get values from raster and assign them to my grid 
        # 1. Extract values using st_extract
        system.time({seattle_values <- raster::extract(seattle_mean_raster_2019, grid_data_Seattle_filtered)}) #110 s = ~1.8 min
        grid_data_PM25_Seattle <- grid_data_Seattle_filtered
        
        # 2. Assign the values to the PM25_AVG column
        grid_data_PM25_Seattle$PM25_AVG <- seattle_values
        grid_data_PM25_Seattle$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_Seattle$PM25_AVG))
    
    # 1.1.4. Prepare output table
        # 1. Convert to NAD83
        grid_data_PM25_Seattle_NAD <- st_transform(grid_data_PM25_Seattle, crs = st_crs("+proj=longlat +datum=NAD83"))
        
        #Get right columns 
        grid_data_PM25_Seattle_NAD %>%
          st_drop_geometry() %>%
          dplyr::select(-pixel_ID) %>%
          rename(Column = col,
                 Row = row,
                 Values = PM25_AVG) %>%
          mutate(Metric = "D24HourMean",
                 `Seasonal Metric` = "QuarterlyMean",
                 `Annual Metric` = "Mean")%>%
          filter(!is.na(Values))-> grid_data_PM25_Seattle_NAD_filtered_final
          
        # 2. Save file
        #Csv
        write.csv(grid_data_PM25_Seattle_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Seattle/PM25_2019_Seattle.csv"))
        #Shapefile
        st_write(grid_data_PM25_Seattle_NAD, "output_data/shapefiles/1km/Seattle/1_km_PM25_2019_seattle.shp")
        
    # 1.1.5. Prepare figure
        #Get national mean (population weighted)
        population_2019_data_pixel_age_final_seattle %>%
          group_by(Row, Column) %>%
          summarise(population = sum(Population)) %>%
          ungroup() -> seattle_pop_total
        
        grid_data_PM25_Seattle_NAD_filtered_final %>%
          left_join(seattle_pop_total, by = c("Row", "Column")) -> seattle_pop_PM25_total
        
        seattle_pop_PM25_total$product <- seattle_pop_PM25_total$Values * seattle_pop_PM25_total$population
        # Sum up products and total population
        sum_products <- sum(seattle_pop_PM25_total$product)
        total_population <- sum(seattle_pop_PM25_total$population)
        # Calculate population-weighted average pollution
        Seattle_mean_BASE_2019 <- sum_products / total_population 
        mean(seattle_pop_PM25_total$Values)
        
        #Set values > 9 to 9 for graphing purposes
        #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
        grid_data_PM25_Seattle_NAD %>%
          filter(!is.na(PM25_AVG)) %>%
          mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_Seattle_NAD_filtered_graph
        
        # Perform intersection to retain only pixels within the city boundary
        grid_data_PM25_Seattle_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_Seattle_NAD_filtered_graph, cbsa_shapefile_seattle)

        p <- ggplot() +
          geom_sf(data = grid_data_PM25_Seattle_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
          scale_fill_gradientn(
            colors = colors,
            values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
            limits = c(0, 10),  # Adjust limits to match your data range
            name = "PM2.5 (ug/m3)"
          ) +
          ggtitle("2019 Seattle Mean 1km Yearly PM2.5 (ug/m3)") +
          geom_text(aes(label = paste("Population weighted mean:", round(Seattle_mean_BASE_2019, 2), "ug/m3")),
                    x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
          geom_sf(data = cbsa_shapefile_seattle, color = "black", fill = NA)+
          theme_void() +
          annotation_scale() +
          geom_point(data = NULL, aes(x = -122.32987465320657, y = 47.60386428408079), shape = 1, color = "#95C623", size = 6, stroke = 2) #Seattle city hall+
        #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_Seattle.png", p, width = 16, height = 12, units = "in", dpi = 600)
        #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_Seattle.svg", p, width = 16, height = 12, units = "in", dpi = 600)
        print(p)

  #### 1.2. 2050 HIGH CDR ####
    # 1.2.1 Get mean value 
        seattle_nc_1km_stack_2050_high <- stack("data/WRF-CMAQ/1km_V2/1_seattle/Dailymean.1km.HighCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
        seattle_mean_raster_2050_high <- calc(seattle_nc_1km_stack_2050_high, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
        plot(seattle_mean_raster_2050_high, col = rev(magma(20)), main = " Seattle 2050_high Yearly Mean PM2.5 Concentrations (ug/m3)")
        
    # 1.2.2. Get rows and columns matching
        bbox <- st_bbox(grid_data_Seattle$geometry)
        num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
        num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
        st_crs(grid_data_Seattle)
        crs(seattle_mean_raster_2050_high)
        print(extent(seattle_mean_raster_2050_high))
        print(st_bbox(grid_data_Seattle))
        res(seattle_mean_raster_2050_high)
        bbox <- st_bbox(grid_data_Seattle)
        resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                        (bbox$ymax - bbox$ymin) / num_rows)
        print(resolution)
        
        crs(seattle_mean_raster_2050_high) <- st_crs(grid_data_Seattle)$proj4string
        extent(seattle_mean_raster_2050_high) <- extent(grid_data_Seattle)
        
    # 1.2.3. Get values from raster and assign them to my grid 
        # 1. Extract values using st_extract
        system.time({seattle_values_2050_high <- raster::extract(seattle_mean_raster_2050_high, grid_data_Seattle_filtered)}) #110 s = ~1.8 min
        grid_data_PM25_Seattle_2050_high <- grid_data_Seattle_filtered
        
        # 2. Assign the values to the PM25_AVG column
        grid_data_PM25_Seattle_2050_high$PM25_AVG <- seattle_values_2050_high
        grid_data_PM25_Seattle_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_Seattle_2050_high$PM25_AVG))
        
    # 1.2.4. Prepare output table
        # 1. Convert to NAD83
        grid_data_PM25_Seattle_NAD_2050_high <- st_transform(grid_data_PM25_Seattle_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
        
        grid_data_PM25_Seattle_NAD_2050_high %>%
          st_drop_geometry() %>%
          dplyr::select(-pixel_ID) %>%
          rename(Column = col,
                 Row = row,
                 Values = PM25_AVG) %>%
          mutate(Metric = "D24HourMean",
                 `Seasonal Metric` = "QuarterlyMean",
                 `Annual Metric` = "Mean") -> grid_data_PM25_Seattle_NAD_filtered_final_2050_high
        
        # 2. Save file
        write.csv(grid_data_PM25_Seattle_NAD_filtered_final_2050_high, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Seattle/PM25_2050_high_Seattle.csv"))
        #Shapefile
        st_write(grid_data_PM25_Seattle_NAD_2050_high, "output_data/shapefiles/1km/Seattle/1_km_PM25_2050_HIGH_seattle.shp")
        
    # 1.2.5. Prepare figure
        grid_data_PM25_Seattle_NAD_filtered_final_2050_high %>%
          left_join(seattle_pop_total, by = c("Row", "Column")) %>%
          filter(!is.na(Values))-> seattle_pop_PM25_total_2050_high
        
        seattle_pop_PM25_total_2050_high$product <- seattle_pop_PM25_total_2050_high$Values * seattle_pop_PM25_total_2050_high$population
        sum_products <- sum(seattle_pop_PM25_total_2050_high$product)
        total_population <- sum(seattle_pop_PM25_total_2050_high$population)
        Seattle_mean_2050_high <- sum_products / total_population
        mean(seattle_pop_PM25_total_2050_high$Values)
        
        grid_data_PM25_Seattle_NAD_2050_high %>%
          mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_Seattle_NAD_filtered_graph_2050_high
        
        # Perform intersection to retain only pixels within the city boundary
        grid_data_PM25_Seattle_NAD_filtered_graph_2050_high_crop <- st_intersection(grid_data_PM25_Seattle_NAD_filtered_graph_2050_high, cbsa_shapefile_seattle)
        
        p <- ggplot() +
          geom_sf(data = grid_data_PM25_Seattle_NAD_filtered_graph_2050_high_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
          scale_fill_gradientn(colors = colors,
                               values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5         
                               limits = c(0, 10),  # Adjust limits to match your data range         
                              name = "PM2.5 (ug/m3)"       ) +
          ggtitle("2050 HIGH Seattle Mean 1km Yearly PM2.5 (ug/m3)") +
          geom_text(aes(label = paste("Population weighted mean:", round(Seattle_mean_2050_high, 2), "ug/m3")),
                    x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
          geom_sf(data = cbsa_shapefile_seattle, color = "black", fill = NA)+
          theme_void() +
          annotation_scale() +
          geom_point(data = NULL, aes(x = -122.32987465320657, y = 47.60386428408079), shape = 1, color = "#95C623", size = 6, stroke = 2) #Seattle city hall+
        #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_Seattle.png", p, width = 16, height = 12, units = "in", dpi = 600)
        #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_Seattle.svg", p, width = 16, height = 12, units = "in", dpi = 600)
        print(p)
        
  #### 1.3. 2050 LOW CDR ####
    # 1.3.1 Get mean value 
        seattle_nc_1km_stack_2050_low <- stack("data/WRF-CMAQ/1km_V2/1_seattle/Dailymean.1km.LowCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
        seattle_mean_raster_2050_low <- calc(seattle_nc_1km_stack_2050_low, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
        plot(seattle_mean_raster_2050_low, col = rev(magma(20)), main = " Seattle 2050_low Yearly Mean PM2.5 Concentrations (ug/m3)")
        
    # 1.3.2. Get rows and columns matching
        bbox <- st_bbox(grid_data_Seattle$geometry)
        num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
        num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
        st_crs(grid_data_Seattle)
        crs(seattle_mean_raster_2050_low)
        print(extent(seattle_mean_raster_2050_low))
        print(st_bbox(grid_data_Seattle))
        res(seattle_mean_raster_2050_low)
        bbox <- st_bbox(grid_data_Seattle)
        resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                        (bbox$ymax - bbox$ymin) / num_rows)
        print(resolution)
        
        crs(seattle_mean_raster_2050_low) <- st_crs(grid_data_Seattle)$proj4string
        extent(seattle_mean_raster_2050_low) <- extent(grid_data_Seattle)
        
    # 1.3.3. Get values from raster and assign them to my grid 
        # 1. Extract values using st_extract
        system.time({seattle_values_2050_low <- raster::extract(seattle_mean_raster_2050_low, grid_data_Seattle_filtered)}) #112s 
        grid_data_PM25_Seattle_2050_low <- grid_data_Seattle_filtered
        
        # 2. Assign the values to the PM25_AVG column
        grid_data_PM25_Seattle_2050_low$PM25_AVG <- seattle_values_2050_low
        grid_data_PM25_Seattle_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_Seattle_2050_low$PM25_AVG))
        
    # 1.3.4. Prepare output table
        # 1. Convert to NAD83
        grid_data_PM25_Seattle_NAD_2050_low <- st_transform(grid_data_PM25_Seattle_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
        
        grid_data_PM25_Seattle_NAD_2050_low %>%
          st_drop_geometry() %>%
          dplyr::select(-pixel_ID) %>%
          rename(Column = col,
                 Row = row,
                 Values = PM25_AVG) %>%
          mutate(Metric = "D24HourMean",
                 `Seasonal Metric` = "QuarterlyMean",
                 `Annual Metric` = "Mean") -> grid_data_PM25_Seattle_NAD_filtered_final_2050_low
        
        # 2. Save file
        write.csv(grid_data_PM25_Seattle_NAD_filtered_final_2050_low, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Seattle/PM25_2050_low_Seattle.csv"))
        #Shapefile
        st_write(grid_data_PM25_Seattle_NAD_2050_low, "output_data/shapefiles/1km/Seattle/1_km_PM25_2050_LOW_seattle.shp")
        
    # 1.3.5. Prepare figure
        grid_data_PM25_Seattle_NAD_filtered_final_2050_low %>%
          left_join(seattle_pop_total, by = c("Row", "Column")) %>%
          filter(!is.na(Values))-> seattle_pop_PM25_total_2050_low
        
        seattle_pop_PM25_total_2050_low$product <- seattle_pop_PM25_total_2050_low$Values * seattle_pop_PM25_total_2050_low$population
        sum_products <- sum(seattle_pop_PM25_total_2050_low$product)
        total_population <- sum(seattle_pop_PM25_total_2050_low$population)
        Seattle_mean_2050_low <- sum_products / total_population
        mean(seattle_pop_PM25_total_2050_low$Values)
        
        grid_data_PM25_Seattle_NAD_2050_low %>%
          mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_Seattle_NAD_filtered_graph_2050_low
        
        # Perform intersection to retain only pixels within the city boundary
        grid_data_PM25_Seattle_NAD_filtered_graph_2050_low_crop <- st_intersection(grid_data_PM25_Seattle_NAD_filtered_graph_2050_low, cbsa_shapefile_seattle)
        
        p <- ggplot() +
          geom_sf(data = grid_data_PM25_Seattle_NAD_filtered_graph_2050_low_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
          scale_fill_gradientn(         colors = colors,         
                                        values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5         
                                        limits = c(0, 10),  # Adjust limits to match your data range         
                                        name = "PM2.5 (ug/m3)"       ) +
          ggtitle("2050 low Seattle Mean 1km Yearly PM2.5 (ug/m3)") +
          geom_text(aes(label = paste("Population weighted mean:", round(Seattle_mean_2050_low, 2), "ug/m3")),
                    x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
          geom_sf(data = cbsa_shapefile_seattle, color = "black", fill = NA)+
          theme_void() +
          annotation_scale() +
          geom_point(data = NULL, aes(x = -122.32987465320657, y = 47.60386428408079), shape = 1, color = "#95C623", size = 6, stroke = 2) #Seattle city hall+
        #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_Seattle.png", p, width = 16, height = 12, units = "in", dpi = 600)
        #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_Seattle.svg", p, width = 16, height = 12, units = "in", dpi = 600)
        print(p)
        
#### 2. LOS ANGELES ####
  #### 2.1. 2019 ####
    # 2.1.1 Get mean value 
      southwest_nc_1km_stack_2019 <- stack("data/WRF-CMAQ/1km_V2/2_southwest/Dailymean.1km.BASE.2019.PM25_AVG.nc", varname = "PM25_AVG")
      southwest_mean_raster_2019 <- calc(southwest_nc_1km_stack_2019, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(southwest_mean_raster_2019, col = rev(magma(20)), main = " Southwest 2019 Yearly Mean PM2.5 Concentrations (ug/m3)")
    
    # 2.1.2. Get rows and columns matching
      bbox <- st_bbox(grid_data_south_west$geometry)
      
      #Calculate the number of columns and rows 
      num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
      num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
      # 1. Check the CRS
      st_crs(grid_data_south_west)
      crs(southwest_mean_raster_2019)
      #2. Print extent information
      print(extent(southwest_mean_raster_2019))
      print(st_bbox(grid_data_south_west))
      #3. Check resolution
      res(southwest_mean_raster_2019)
      bbox <- st_bbox(grid_data_south_west)
      resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                      (bbox$ymax - bbox$ymin) / num_rows) 
      print(resolution)
      # 4. Set the CRS for mean_raster_2019
      crs(southwest_mean_raster_2019) <- st_crs(grid_data_south_west)$proj4string
      # 5. Set the extent of mean_raster_aligned to match grid_data
      extent(southwest_mean_raster_2019) <- extent(grid_data_south_west)
      #6. Filter pixels 
      grid_data_south_west %>%
        filter(pixel_ID %in% LA_pixels) -> grid_data_south_west_LA
      
    # 2.1.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({southwest_values <- raster::extract(southwest_mean_raster_2019, grid_data_south_west_LA)}) #91 s = ~1.5 min
      grid_data_PM25_southwest_LA <- grid_data_south_west_LA
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_southwest_LA$PM25_AVG <- southwest_values
      grid_data_PM25_southwest_LA$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_southwest_LA$PM25_AVG))
    
    # 2.1.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_southwest_LA_NAD <- st_transform(grid_data_PM25_southwest_LA, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      #Fix one pixel with NA (this may be because the intersection fraction is very small). We set pollution form neighboring pixel
      grid_data_PM25_southwest_LA_NAD %>%
        filter(!is.na(PM25_AVG)) -> grid_data_PM25_southwest_LA_NAD
      
      #Get right columns 
      grid_data_PM25_southwest_LA_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_southwest_LA_NAD_final
      
        # 2. Save file
        write.csv(grid_data_PM25_southwest_LA_NAD_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/LA/PM25_2019_LA.csv"))
        
        st_write(grid_data_PM25_southwest_LA_NAD, "output_data/shapefiles/1km/LA/1_km_PM25_2019_LA.shp")
      
    # 2.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_LA %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> LA_pop_total

      grid_data_PM25_southwest_LA_NAD_final %>%
        left_join(LA_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> LA_pop_PM25_total

      LA_pop_PM25_total$product <- LA_pop_PM25_total$Values * LA_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(LA_pop_PM25_total$product)
      total_population <- sum(LA_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      LA_mean_BASE_2019 <- sum_products / total_population
      mean(LA_pop_PM25_total$Values)
    
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_southwest_LA_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_southwest_LA_NAD_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_southwest_LA_NAD_graph_crop <- st_intersection(grid_data_PM25_southwest_LA_NAD_graph, cbsa_shapefile_LA)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_southwest_LA_NAD_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range 
                             name = "PM2.5 (ug/m3)") +
        ggtitle("2019 LA Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(LA_mean_BASE_2019, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_LA, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -118.24271554610264,  y =  34.05368499865087), shape = 1, color = "#95C623", size = 6, stroke = 2) #LA city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_LA.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_LA.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
    
  #### 2.2. 2050 HIGH ####
    # 2.2.1 Get mean value 
      southwest_nc_1km_stack_2050_high <- stack("data/WRF-CMAQ/1km_V2/2_southwest/Dailymean.1km.HighCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      southwest_mean_raster_2050_high <- calc(southwest_nc_1km_stack_2050_high, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(southwest_mean_raster_2050_high, col = rev(magma(20)), main = " Southwest 2050_high Yearly Mean PM2.5 Concentrations (ug/m3)")
      
    # 2.2.2. Get rows and columns matching
      crs(southwest_mean_raster_2050_high) <- st_crs(grid_data_south_west)$proj4string
      extent(southwest_mean_raster_2050_high) <- extent(grid_data_south_west)
      
    # 2.2.3. Get values from raster and assign them to my grid 
      system.time({southwest_values_2050_high <- raster::extract(southwest_mean_raster_2050_high, grid_data_south_west_LA)}) #94 s 
      grid_data_PM25_southwest_LA_2050_high <- grid_data_south_west_LA
      grid_data_PM25_southwest_LA_2050_high$PM25_AVG <- southwest_values_2050_high
      grid_data_PM25_southwest_LA_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_southwest_LA_2050_high$PM25_AVG))
      
    # 2.2.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_southwest_LA_NAD_2050_high <- st_transform(grid_data_PM25_southwest_LA_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      #Get right columns 
      grid_data_PM25_southwest_LA_NAD_2050_high %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_southwest_LA_NAD_final_2050_high
      
      # 2. Save file
      write.csv(grid_data_PM25_southwest_LA_NAD_final_2050_high, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/LA/PM25_2050_high_LA.csv"))
      st_write(grid_data_PM25_southwest_LA_NAD_2050_high, "output_data/shapefiles/1km/LA/1_km_PM25_2050_high_LA.shp")
      
    # 2.2.5. Prepare figure
      grid_data_PM25_southwest_LA_NAD_final_2050_high %>%
        left_join(LA_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> LA_pop_PM25_total_2050_high
      
      LA_pop_PM25_total_2050_high$product <- LA_pop_PM25_total_2050_high$Values * LA_pop_PM25_total_2050_high$population
      sum_products <- sum(LA_pop_PM25_total_2050_high$product)
      total_population <- sum(LA_pop_PM25_total_2050_high$population)
      LA_mean_BASE_2050_high <- sum_products / total_population
      mean(LA_pop_PM25_total_2050_high$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_southwest_LA_NAD_2050_high %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_southwest_LA_NAD_2050_high_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_southwest_LA_NAD_2050_high_graph_crop <- st_intersection(grid_data_PM25_southwest_LA_NAD_2050_high_graph, cbsa_shapefile_LA)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_southwest_LA_NAD_2050_high_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)") +
        ggtitle("2050_high LA Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(LA_mean_BASE_2050_high, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_LA, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -118.24271554610264,  y =  34.05368499865087), shape = 1, color = "#95C623", size = 6, stroke = 2) #LA city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_LA.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_LA.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 2.3. 2050 LOW ####
    # 2.3.1 Get mean value 
        southwest_nc_1km_stack_2050_low <- stack("data/WRF-CMAQ/1km_V2/2_southwest/Dailymean.1km.LowCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
        southwest_mean_raster_2050_low <- calc(southwest_nc_1km_stack_2050_low, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
        plot(southwest_mean_raster_2050_low, col = rev(magma(20)), main = " Southwest 2050_low Yearly Mean PM2.5 Concentrations (ug/m3)")
      
    # 2.3.2. Get rows and columns matching
        crs(southwest_mean_raster_2050_low) <- st_crs(grid_data_south_west)$proj4string
        extent(southwest_mean_raster_2050_low) <- extent(grid_data_south_west)
      
    # 2.3.3. Get values from raster and assign them to my grid 
        system.time({southwest_values_2050_low <- raster::extract(southwest_mean_raster_2050_low, grid_data_south_west_LA)}) #92 s 
        grid_data_PM25_southwest_LA_2050_low <- grid_data_south_west_LA
        grid_data_PM25_southwest_LA_2050_low$PM25_AVG <- southwest_values_2050_low
        grid_data_PM25_southwest_LA_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_southwest_LA_2050_low$PM25_AVG))
      
    # 2.34. Prepare output table
        # 1. Convert to NAD83
        grid_data_PM25_southwest_LA_NAD_2050_low <- st_transform(grid_data_PM25_southwest_LA_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
        
        #Get right columns 
        grid_data_PM25_southwest_LA_NAD_2050_low %>%
          st_drop_geometry() %>%
          dplyr::select(-pixel_ID) %>%
          rename(Column = col,
                 Row = row,
                 Values = PM25_AVG) %>%
          mutate(Metric = "D24HourMean",
                 `Seasonal Metric` = "QuarterlyMean",
                 `Annual Metric` = "Mean") -> grid_data_PM25_southwest_LA_NAD_final_2050_low
        
        # 2. Save file
        write.csv(grid_data_PM25_southwest_LA_NAD_final_2050_low, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/LA/PM25_2050_low_LA.csv"))
        #grid_data_PM25_southwest_LA_NAD_final_2050_low<- read.csv("output_data/BenMAP_files/1km/LA/Date_ 2024-05-17 PM25_2050_low_LA.csv")
        st_write(grid_data_PM25_southwest_LA_NAD_2050_low, "output_data/shapefiles/1km/LA/1_km_PM25_2050_low_LA.shp")
        
    # 2.3.5. Prepare figure
        grid_data_PM25_southwest_LA_NAD_final_2050_low %>%
          left_join(LA_pop_total, by = c("Row", "Column")) %>%
          filter(!is.na(Values)) -> LA_pop_PM25_total_2050_low
        
        LA_pop_PM25_total_2050_low$product <- LA_pop_PM25_total_2050_low$Values * LA_pop_PM25_total_2050_low$population
        sum_products <- sum(LA_pop_PM25_total_2050_low$product)
        total_population <- sum(LA_pop_PM25_total_2050_low$population)
        LA_mean_BASE_2050_low <- sum_products / total_population
        mean(LA_pop_PM25_total_2050_low$Values)
        
        #Set values > 9 to 9 for graphing purposes
        grid_data_PM25_southwest_LA_NAD_2050_low %>%
          mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_southwest_LA_NAD_2050_low_graph
        
        # Perform intersection to retain only pixels within the city boundary
        grid_data_PM25_southwest_LA_NAD_2050_low_graph_crop <- st_intersection(grid_data_PM25_southwest_LA_NAD_2050_low_graph, cbsa_shapefile_LA)
        
        p <- ggplot() +
          geom_sf(data = grid_data_PM25_southwest_LA_NAD_2050_low_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
          scale_fill_gradientn(colors = colors,         
                               values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                               limits = c(0, 10),  # Adjust limits to match your data range
                               name = "PM2.5 (ug/m3)"       ) +
          ggtitle("2050_low LA Mean 1km Yearly PM2.5 (ug/m3)") +
          geom_text(aes(label = paste("Population weighted mean:", round(LA_mean_BASE_2050_low, 2), "ug/m3")),
                    x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
          annotation_scale() +
          geom_sf(data = cbsa_shapefile_LA, color = "black", fill = NA)+
          theme_void() +
          geom_point(data = NULL, aes(x = -118.24271554610264,  y =  34.05368499865087), shape = 1, color = "#95C623", size = 6, stroke = 2) #LA city hall
        #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_LA.png", p, width = 16, height = 12, units = "in", dpi = 600)
        #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_LA.svg", p, width = 16, height = 12, units = "in", dpi = 600)
        print(p)

#### 3. RIVERSIDE ####
  #### 3.1. 2019 ####
    #6. Filter pixels 
      grid_data_south_west %>%
        filter(pixel_ID %in% riverside_pixels) -> grid_data_south_west_riverside
    
    # 3.1.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({riverside_values <- raster::extract(southwest_mean_raster_2019, grid_data_south_west_riverside)}) #499 s = ~8 min
      grid_data_PM25_southwest_riverside <- grid_data_south_west_riverside
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_southwest_riverside$PM25_AVG <- riverside_values
      grid_data_PM25_southwest_riverside$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_southwest_riverside$PM25_AVG))
    
    # 3.1.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_southwest_riverside_NAD <- st_transform(grid_data_PM25_southwest_riverside, crs = st_crs("+proj=longlat +datum=NAD83"))
      #Get right columns 
      grid_data_PM25_southwest_riverside_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_southwest_riverside_NAD_final
      # 2. Save file
      write.csv(grid_data_PM25_southwest_riverside_NAD_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Riverside/PM25_2019_Riverside.csv"))
      
      st_write(grid_data_PM25_southwest_riverside_NAD, "output_data/shapefiles/1km/Riverside/1_km_PM25_2019_riverside.shp")
      
    # 3.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_riverside %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> riverside_pop_total
      
      grid_data_PM25_southwest_riverside_NAD_final %>%
        left_join(riverside_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values)) -> riverside_pop_PM25_total
      
      riverside_pop_PM25_total$product <- riverside_pop_PM25_total$Values * riverside_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(riverside_pop_PM25_total$product)
      total_population <- sum(riverside_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      riverside_mean_BASE_2019 <- sum_products / total_population
      mean(riverside_pop_PM25_total$Values)
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_southwest_riverside_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_southwest_riverside_NAD_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_southwest_riverside_NAD_graph_crop <- st_intersection(grid_data_PM25_southwest_riverside_NAD_graph, cbsa_shapefile_riverside)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_southwest_riverside_NAD_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range 
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2019 Riverside Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(riverside_mean_BASE_2019, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +  
        geom_sf(data = cbsa_shapefile_riverside, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -117.37559005667082,  y =  33.980696653550375), shape = 1, color = "#95C623", size = 6, stroke = 2) #Riverside city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_riverside.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_riverside.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)  
  
  #### 3.2. 2050 HIGH ####
    # 3.2.3. Get values from raster and assign them to my grid 
      system.time({riverside_values_2050_high <- raster::extract(southwest_mean_raster_2050_high, grid_data_south_west_riverside)}) #4556s
      grid_data_PM25_southwest_riverside_2050_high <- grid_data_south_west_riverside
      grid_data_PM25_southwest_riverside_2050_high$PM25_AVG <- riverside_values_2050_high
      grid_data_PM25_southwest_riverside_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_southwest_riverside_2050_high$PM25_AVG))
      
    # 3.2.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_southwest_riverside_2050_high_NAD <- st_transform(grid_data_PM25_southwest_riverside_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_southwest_riverside_2050_high_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_southwest_riverside_2050_high_NAD_final
      # 2. Save file
      write.csv(grid_data_PM25_southwest_riverside_2050_high_NAD_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Riverside/PM25_2050_high_Riverside.csv"))
      
      st_write(grid_data_PM25_southwest_riverside_2050_high_NAD, "output_data/shapefiles/1km/Riverside/1_km_PM25_2050_high_riverside.shp")
      
    # 3.2.5. Prepare figure
      grid_data_PM25_southwest_riverside_2050_high_NAD_final %>%
        left_join(riverside_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> riverside_pop_PM25_total_2050_high
      
      riverside_pop_PM25_total_2050_high$product <- riverside_pop_PM25_total_2050_high$Values * riverside_pop_PM25_total_2050_high$population
      sum_products <- sum(riverside_pop_PM25_total_2050_high$product)
      total_population <- sum(riverside_pop_PM25_total_2050_high$population)
      riverside_mean_2050_high <- sum_products / total_population
      mean(riverside_pop_PM25_total_2050_high$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_southwest_riverside_2050_high_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_southwest_riverside_2050_high_NAD_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_southwest_riverside_2050_high_NAD_graph_crop <- st_intersection(grid_data_PM25_southwest_riverside_2050_high_NAD_graph, cbsa_shapefile_riverside)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_southwest_riverside_2050_high_NAD_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5 
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050 High Riverside Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(riverside_mean_2050_high, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +  
        geom_sf(data = cbsa_shapefile_riverside, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -117.37559005667082,  y =  33.980696653550375), shape = 1, color = "#95C623", size = 6, stroke = 2) #Riverside city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_riverside.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_riverside.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)  
      
  #### 3.3. 2050 LOW ####
    # 3.3.3. Get values from raster and assign them to my grid 
        system.time({riverside_values_2050_low <- raster::extract(southwest_mean_raster_2050_low, grid_data_south_west_riverside)}) #538s
        grid_data_PM25_southwest_riverside_2050_low <- grid_data_south_west_riverside
        grid_data_PM25_southwest_riverside_2050_low$PM25_AVG <- riverside_values_2050_low
        grid_data_PM25_southwest_riverside_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_southwest_riverside_2050_low$PM25_AVG))
      
    # 3.3.4. Prepare output table
        # 1. Convert to NAD83
        grid_data_PM25_southwest_riverside_2050_low_NAD <- st_transform(grid_data_PM25_southwest_riverside_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
        
        grid_data_PM25_southwest_riverside_2050_low_NAD %>%
          st_drop_geometry() %>%
          dplyr::select(-pixel_ID) %>%
          rename(Column = col,
                 Row = row,
                 Values = PM25_AVG) %>%
          mutate(Metric = "D24HourMean",
                 `Seasonal Metric` = "QuarterlyMean",
                 `Annual Metric` = "Mean") -> grid_data_PM25_southwest_riverside_2050_low_NAD_final
        # 2. Save file
        write.csv(grid_data_PM25_southwest_riverside_2050_low_NAD_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Riverside/PM25_2050_low_Riverside.csv"))
        
        st_write(grid_data_PM25_southwest_riverside_2050_low_NAD, "output_data/shapefiles/1km/Riverside/1_km_PM25_2050_low_riverside.shp")
        
    # 3.3.5. Prepare figure
        grid_data_PM25_southwest_riverside_2050_low_NAD_final %>%
          left_join(riverside_pop_total, by = c("Row", "Column")) %>%
          filter(!is.na(Values))-> riverside_pop_PM25_total_2050_low
        
        riverside_pop_PM25_total_2050_low$product <- riverside_pop_PM25_total_2050_low$Values * riverside_pop_PM25_total_2050_low$population
        sum_products <- sum(riverside_pop_PM25_total_2050_low$product)
        total_population <- sum(riverside_pop_PM25_total_2050_low$population)
        riverside_mean_2050_low <- sum_products / total_population
        mean(riverside_pop_PM25_total_2050_low$Values)
        
        #Set values > 9 to 9 for graphing purposes
        grid_data_PM25_southwest_riverside_2050_low_NAD %>%
          mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_southwest_riverside_2050_low_NAD_graph
        
        # Perform intersection to retain only pixels within the city boundary
        grid_data_PM25_southwest_riverside_2050_low_NAD_graph_crop <- st_intersection(grid_data_PM25_southwest_riverside_2050_low_NAD_graph, cbsa_shapefile_riverside)
        
        p <- ggplot() +
          geom_sf(data = grid_data_PM25_southwest_riverside_2050_low_NAD_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
          scale_fill_gradientn(colors = colors,
                               values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                               limits = c(0, 10),  # Adjust limits to match your data range
                               name = "PM2.5 (ug/m3)"       ) +
          ggtitle("2050 low Riverside Mean 1km Yearly PM2.5 (ug/m3)") +
          geom_text(aes(label = paste("Population weighted mean:", round(riverside_mean_2050_low, 2), "ug/m3")),
                    x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
          annotation_scale() +  
          geom_sf(data = cbsa_shapefile_riverside, color = "black", fill = NA)+
          theme_void() +
          geom_point(data = NULL, aes(x = -117.37559005667082,  y =  33.980696653550375), shape = 1, color = "#95C623", size = 6, stroke = 2) #Riverside city hall
        #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_riverside.png", p, width = 16, height = 12, units = "in", dpi = 600)
        #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_riverside.svg", p, width = 16, height = 12, units = "in", dpi = 600)
        print(p)
        
#### 4. PHOENIX ####
  #### 4.1. 2019 ####
    #6. Filter pixels 
      grid_data_south_west %>%
        filter(pixel_ID %in% phoenix_pixels) -> grid_data_south_west_phoenix
    
    # 4.1.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({phoenix_values <- raster::extract(southwest_mean_raster_2019, grid_data_south_west_phoenix)}) #263 s = ~4.3 min
      grid_data_PM25_southwest_phoenix <- grid_data_south_west_phoenix
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_southwest_phoenix$PM25_AVG <- phoenix_values
      grid_data_PM25_southwest_phoenix$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_southwest_phoenix$PM25_AVG))
    
    # 4.1.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_southwest_phoenix_NAD <- st_transform(grid_data_PM25_southwest_phoenix, crs = st_crs("+proj=longlat +datum=NAD83"))
      #Get right columns 
      grid_data_PM25_southwest_phoenix_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_southwest_phoenix_NAD_final
      # 2. Save file
      write.csv(grid_data_PM25_southwest_phoenix_NAD_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Phoenix/PM25_2019_Phoenix.csv"))
      st_write(grid_data_PM25_southwest_phoenix_NAD, "output_data/shapefiles/1km/Phoenix/1_km_PM25_2019_phoenix.shp")
    
    # 4.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_phoenix %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> phoenix_pop_total
      
      grid_data_PM25_southwest_phoenix_NAD_final %>%
        left_join(phoenix_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> phoenix_pop_PM25_total
      
      phoenix_pop_PM25_total$product <- phoenix_pop_PM25_total$Values * phoenix_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(phoenix_pop_PM25_total$product)
      total_population <- sum(phoenix_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      phoenix_mean_BASE_2019 <- sum_products / total_population
      mean(phoenix_pop_PM25_total$Values)
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_southwest_phoenix_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_southwest_phoenix_NAD_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_southwest_phoenix_NAD_graph_crop <- st_intersection(grid_data_PM25_southwest_phoenix_NAD_graph, cbsa_shapefile_phoenix)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_southwest_phoenix_NAD_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn( colors = colors,         
                              values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                              limits = c(0, 10),  # Adjust limits to match your data range
                              name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2019 Phoenix Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(phoenix_mean_BASE_2019, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_phoenix, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -112.07721684407342,  y =  33.448615125541835), shape = 1, color = "#95C623", size = 6, stroke = 2) #Phoenix city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_phoenix.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_phoenix.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)   
      
  #### 4.2. 2050 HIGH ####
    # 4.2.3. Get values from raster and assign them to my grid 
        system.time({phoenix_values_2050_high <- raster::extract(southwest_mean_raster_2050_high, grid_data_south_west_phoenix)}) #275s
        grid_data_PM25_southwest_phoenix_2050_high <- grid_data_south_west_phoenix
        grid_data_PM25_southwest_phoenix_2050_high$PM25_AVG <- phoenix_values_2050_high
        grid_data_PM25_southwest_phoenix_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_southwest_phoenix_2050_high$PM25_AVG))
      
    # 4.2.4. Prepare output table
        grid_data_PM25_southwest_phoenix_2050_high_NAD <- st_transform(grid_data_PM25_southwest_phoenix_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
        
        grid_data_PM25_southwest_phoenix_2050_high_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_southwest_phoenix_2050_high_NAD_final

      write.csv(grid_data_PM25_southwest_phoenix_2050_high_NAD_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Phoenix/PM25_2050_high_Phoenix.csv"))
      st_write(grid_data_PM25_southwest_phoenix_2050_high_NAD, "output_data/shapefiles/1km/Phoenix/1_km_PM25_2050_high_phoenix.shp")
      
    # 4.2.5. Prepare figure
      grid_data_PM25_southwest_phoenix_2050_high_NAD_final %>%
        left_join(phoenix_pop_total, by = c("Row", "Column")) %>%
      filter(!is.na(Values))-> phoenix_pop_PM25_total_2050_high
      
      phoenix_pop_PM25_total_2050_high$product <- phoenix_pop_PM25_total_2050_high$Values * phoenix_pop_PM25_total_2050_high$population
      sum_products <- sum(phoenix_pop_PM25_total_2050_high$product)
      total_population <- sum(phoenix_pop_PM25_total_2050_high$population)
      phoenix_mean_2050_high <- sum_products / total_population
      mean(phoenix_pop_PM25_total_2050_high$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_southwest_phoenix_2050_high_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_southwest_phoenix_2050_high_NAD_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_southwest_phoenix_2050_high_NAD_graph_crop <- st_intersection(grid_data_PM25_southwest_phoenix_2050_high_NAD_graph, cbsa_shapefile_phoenix)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_southwest_phoenix_2050_high_NAD_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range         
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050 High Phoenix Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(phoenix_mean_2050_high, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_phoenix, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -112.07721684407342,  y =  33.448615125541835), shape = 1, color = "#95C623", size = 6, stroke = 2) #Phoenix city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_phoenix.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_phoenix.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)  
      
  #### 4.3. 2050 LOW ####
    # 4.3.3. Get values from raster and assign them to my grid 
      system.time({phoenix_values_2050_low <- raster::extract(southwest_mean_raster_2050_low, grid_data_south_west_phoenix)}) #274s
      grid_data_PM25_southwest_phoenix_2050_low <- grid_data_south_west_phoenix
      grid_data_PM25_southwest_phoenix_2050_low$PM25_AVG <- phoenix_values_2050_low
      grid_data_PM25_southwest_phoenix_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_southwest_phoenix_2050_low$PM25_AVG))
      
    # 4.3.4. Prepare output table
      grid_data_PM25_southwest_phoenix_2050_low_NAD <- st_transform(grid_data_PM25_southwest_phoenix_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_southwest_phoenix_2050_low_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_southwest_phoenix_2050_low_NAD_final
      
      write.csv(grid_data_PM25_southwest_phoenix_2050_low_NAD_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Phoenix/PM25_2050_low_Phoenix.csv"))
    
      st_write(grid_data_PM25_southwest_phoenix_2050_low_NAD, "output_data/shapefiles/1km/Phoenix/1_km_PM25_2050_low_phoenix.shp")
      
    # 4.3.5. Prepare figure
      grid_data_PM25_southwest_phoenix_2050_low_NAD_final %>%
        left_join(phoenix_pop_total, by = c("Row", "Column")) -> phoenix_pop_PM25_total_2050_low
      
      phoenix_pop_PM25_total_2050_low$product <- phoenix_pop_PM25_total_2050_low$Values * phoenix_pop_PM25_total_2050_low$population
      sum_products <- sum(phoenix_pop_PM25_total_2050_low$product)
      total_population <- sum(phoenix_pop_PM25_total_2050_low$population)
      phoenix_mean_2050_low <- sum_products / total_population
      mean(phoenix_pop_PM25_total_2050_low$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_southwest_phoenix_2050_low_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_southwest_phoenix_2050_low_NAD_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_southwest_phoenix_2050_low_NAD_graph_crop <- st_intersection(grid_data_PM25_southwest_phoenix_2050_low_NAD_graph, cbsa_shapefile_phoenix)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_southwest_phoenix_2050_low_NAD_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5    
                             limits = c(0, 10),  # Adjust limits to match your data range        
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050 low Phoenix Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(phoenix_mean_2050_low, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_phoenix, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -112.07721684407342,  y =  33.448615125541835), shape = 1, color = "#95C623", size = 6, stroke = 2) #Phoenix city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_phoenix.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_phoenix.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p) 
      
#### 5. DALLAS ####
  #### 5.1. 2019 ####
    # 5.1.1 Get mean value 
      #Read in netcdf file with daily-average PM2.5 concentrations (ug/m3) for 2019
      #365 days, 399 rows, 612 columns, pixel size is 1000m2
      # Read the NetCDF file using stack
      dallas_nc_1km_stack_2019 <- stack("data/WRF-CMAQ/1km_V2/3_dallas/Dailymean.1km.BASE.2019.PM25_AVG.nc", varname = "PM25_AVG")
      # Set the CRS explicitly based on the information you obtained from gdalinfo
      crs(dallas_nc_1km_stack_2019)
      
      # Calculate the mean across all bands (365 days) for each cell
      dallas_mean_raster_2019 <- calc(dallas_nc_1km_stack_2019, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      
      # Plot the mean raster
      plot(dallas_mean_raster_2019, col = rev(magma(20)), main = " dallas 2019 Yearly Mean PM2.5 Concentrations (ug/m3)")
      
    # 5.1.2. Get rows and columns matching
        #1. make sure geometries align
        bbox <- st_bbox(grid_data_dallas$geometry)
        
        #Calculate the number of columns and rows 
        num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
        num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
        # 1. Check the CRS
        st_crs(grid_data_dallas)
        crs(dallas_mean_raster_2019)
        
        #2. Print extent information
        print(extent(dallas_mean_raster_2019))
        print(st_bbox(grid_data_dallas))
        
        #3. Check resolution
        res(dallas_mean_raster_2019)
        # Get the bounding box of the grid_data
        bbox <- st_bbox(grid_data_dallas)
        # Calculate the resolution
        resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                        (bbox$ymax - bbox$ymin) / num_rows)
        # Print the resolution
        print(resolution)
        
        # 2. Set the CRS for mean_raster_2019
        crs(dallas_mean_raster_2019) <- st_crs(grid_data_dallas)$proj4string
        
        # 3. Set the extent of mean_raster_aligned to match grid_data
        extent(dallas_mean_raster_2019) <- extent(grid_data_dallas)
        
        #4. Filter pixels to keep
        grid_data_dallas %>%
          filter(pixel_ID %in% dallas_pixels) -> grid_data_dallas_filtered
      
    # 5.1.3. Get values from raster and assign them to my grid 
        # 1. Extract values using st_extract
        system.time({dallas_values <- raster::extract(dallas_mean_raster_2019, grid_data_dallas_filtered)}) #160 s = ~2.6 min
        grid_data_PM25_dallas <- grid_data_dallas_filtered
        
        # 2. Assign the values to the PM25_AVG column
        grid_data_PM25_dallas$PM25_AVG <- dallas_values
        grid_data_PM25_dallas$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_dallas$PM25_AVG))
      
    # 5.1.4. Prepare output table
        # 1. Convert to NAD83
        grid_data_PM25_dallas_NAD <- st_transform(grid_data_PM25_dallas, crs = st_crs("+proj=longlat +datum=NAD83"))
        
        #Get right columns 
        grid_data_PM25_dallas_NAD %>%
          st_drop_geometry() %>%
          dplyr::select(-pixel_ID) %>%
          rename(Column = col,
                 Row = row,
                 Values = PM25_AVG) %>%
          mutate(Metric = "D24HourMean",
                 `Seasonal Metric` = "QuarterlyMean",
                 `Annual Metric` = "Mean") -> grid_data_PM25_dallas_NAD_filtered_final
        
        # 2. Save file
        #Csv
        write.csv(grid_data_PM25_dallas_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Dallas/PM25_2019_Dallas.csv"))
        
        st_write(grid_data_PM25_dallas_NAD, "output_data/shapefiles/1km/Dallas/1_km_PM25_2019_dallas.shp")
      
    # 5.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_dallas %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> dallas_pop_total
      
      grid_data_PM25_dallas_NAD_filtered_final %>%
        left_join(dallas_pop_total, by = c("Row", "Column")) -> dallas_pop_PM25_total
      
      dallas_pop_PM25_total$product <- dallas_pop_PM25_total$Values * dallas_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(dallas_pop_PM25_total$product)
      total_population <- sum(dallas_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      dallas_mean_BASE_2019 <- sum_products / total_population
      mean(dallas_pop_PM25_total$Values)
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_dallas_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_dallas_NAD_filtered_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_dallas_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_dallas_NAD_filtered_graph, cbsa_shapefile_dallas)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_dallas_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5  
                             limits = c(0, 10),  # Adjust limits to match your data range     
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2019 Dallas Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(dallas_mean_BASE_2019, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_dallas, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -96.7968013977644,  y =  32.776295519527835), shape = 1, color = "#95C623", size = 6, stroke = 2) #Dallas city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_dallas.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_dallas.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
    
  #### 5.2. 2050 HIGH ####
    # 5.2.1 Get mean value 
      dallas_nc_1km_stack_2050_high <- stack("data/WRF-CMAQ/1km_V2/3_dallas/Dailymean.1km.HighCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      dallas_mean_raster_2050_high <- calc(dallas_nc_1km_stack_2050_high, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(dallas_mean_raster_2050_high, col = rev(magma(20)), main = "Dallas 2050 High Yearly Mean PM2.5 Concentrations (ug/m3)")
      
    # 5.2.2. Get rows and columns matching
      crs(dallas_mean_raster_2050_high) <- st_crs(grid_data_dallas)$proj4string
      extent(dallas_mean_raster_2050_high) <- extent(grid_data_dallas)
      
    # 5.2.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({dallas_values_2050_high <- raster::extract(dallas_mean_raster_2050_high, grid_data_dallas_filtered)}) #178s
      grid_data_PM25_dallas_2050_high <- grid_data_dallas_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_dallas_2050_high$PM25_AVG <- dallas_values_2050_high
      grid_data_PM25_dallas_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_dallas_2050_high$PM25_AVG))
      
    # 5.2.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_dallas_2050_high_NAD <- st_transform(grid_data_PM25_dallas_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_dallas_2050_high_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_dallas_2050_high_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_dallas_2050_high_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Dallas/PM25_2050_high_Dallas.csv"))
      
      st_write(grid_data_PM25_dallas_2050_high_NAD, "output_data/shapefiles/1km/Dallas/1_km_PM25_2050_high_dallas.shp")
      
    # 5.2.5. Prepare figure
      grid_data_PM25_dallas_2050_high_NAD_filtered_final %>%
        left_join(dallas_pop_total, by = c("Row", "Column")) -> dallas_pop_PM25_total_2050_high
      
      dallas_pop_PM25_total_2050_high$product <- dallas_pop_PM25_total_2050_high$Values * dallas_pop_PM25_total_2050_high$population
      sum_products <- sum(dallas_pop_PM25_total_2050_high$product)
      total_population <- sum(dallas_pop_PM25_total_2050_high$population)
      dallas_mean_2050_high <- sum_products / total_population
      mean(dallas_pop_PM25_total_2050_high$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_dallas_2050_high_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_dallas_2050_high_NAD_filtered_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_dallas_2050_high_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_dallas_2050_high_NAD_filtered_graph, cbsa_shapefile_dallas)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_dallas_2050_high_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050 High Dallas Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(dallas_mean_2050_high, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_dallas, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -96.7968013977644,  y =  32.776295519527835), shape = 1, color = "#95C623", size = 6, stroke = 2) #Dallas city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_dallas.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_dallas.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 5.3. 2050 LOW ####
    # 5.3.1 Get mean value 
      dallas_nc_1km_stack_2050_low <- stack("data/WRF-CMAQ/1km_V2/3_dallas/Dailymean.1km.LowCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      dallas_mean_raster_2050_low <- calc(dallas_nc_1km_stack_2050_low, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(dallas_mean_raster_2050_low, col = rev(magma(20)), main = "Dallas 2050 low Yearly Mean PM2.5 Concentrations (ug/m3)")
      
    # 5.3.2. Get rows and columns matching
      crs(dallas_mean_raster_2050_low) <- st_crs(grid_data_dallas)$proj4string
      extent(dallas_mean_raster_2050_low) <- extent(grid_data_dallas)
      
    # 5.3.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({dallas_values_2050_low <- raster::extract(dallas_mean_raster_2050_low, grid_data_dallas_filtered)}) #170s
      grid_data_PM25_dallas_2050_low <- grid_data_dallas_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_dallas_2050_low$PM25_AVG <- dallas_values_2050_low
      grid_data_PM25_dallas_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_dallas_2050_low$PM25_AVG))
      
    # 5.3.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_dallas_2050_low_NAD <- st_transform(grid_data_PM25_dallas_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_dallas_2050_low_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_dallas_2050_low_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_dallas_2050_low_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Dallas/PM25_2050_low_Dallas.csv"))
      
      st_write(grid_data_PM25_dallas_2050_low_NAD, "output_data/shapefiles/1km/Dallas/1_km_PM25_2050_low_dallas.shp")
      
    # 5.3.5. Prepare figure
      grid_data_PM25_dallas_2050_low_NAD_filtered_final %>%
        left_join(dallas_pop_total, by = c("Row", "Column")) -> dallas_pop_PM25_total_2050_low
      
      dallas_pop_PM25_total_2050_low$product <- dallas_pop_PM25_total_2050_low$Values * dallas_pop_PM25_total_2050_low$population
      sum_products <- sum(dallas_pop_PM25_total_2050_low$product)
      total_population <- sum(dallas_pop_PM25_total_2050_low$population)
      dallas_mean_2050_low <- sum_products / total_population
      mean(dallas_pop_PM25_total_2050_low$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_dallas_2050_low_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_dallas_2050_low_NAD_filtered_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_dallas_2050_low_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_dallas_2050_low_NAD_filtered_graph, cbsa_shapefile_dallas)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_dallas_2050_low_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050 low Dallas Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(dallas_mean_2050_low, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) + 
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_dallas, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -96.7968013977644,  y =  32.776295519527835), shape = 1, color = "#95C623", size = 6, stroke = 2) #Dallas city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_dallas.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_dallas.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
    
#### 6. HOUSTON ####
  #### 6.1. 2019 ####
    # 6.1.1 Get mean value 
      #Read in netcdf file with daily-average PM2.5 concentrations (ug/m3) for 2019
      #365 days, 399 rows, 612 columns, pixel size is 1000m2
      # Read the NetCDF file using stack
      houston_nc_1km_stack_2019 <- stack("data/WRF-CMAQ/1km_V2/4_houston/Dailymean.1km.BASE.2019.PM25_AVG.nc", varname = "PM25_AVG")
      # Set the CRS explicitly based on the information you obtained from gdalinfo
      crs(houston_nc_1km_stack_2019)
      
      # Calculate the mean across all bands (365 days) for each cell
      houston_mean_raster_2019 <- calc(houston_nc_1km_stack_2019, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      
      # Plot the mean raster
      plot(houston_mean_raster_2019, col = rev(magma(20)), main = "Houston 2019 Yearly Mean PM2.5 Conc. (ug/m3)")
        
    # 6.1.2. Get rows and columns matching
      #1. make sure geometries align
      bbox <- st_bbox(grid_data_houston$geometry)
      
      #Calculate the number of columns and rows 
      num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
      num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
      # 1. Check the CRS
      st_crs(grid_data_houston)
      crs(houston_mean_raster_2019)
      
      #2. Print extent information
      print(extent(houston_mean_raster_2019))
      print(st_bbox(grid_data_houston))
      
      #3. Check resolution
      res(houston_mean_raster_2019)
      # Get the bounding box of the grid_data
      bbox <- st_bbox(grid_data_houston)
      # Calculate the resolution
      resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                      (bbox$ymax - bbox$ymin) / num_rows)
      # Print the resolution
      print(resolution)
      
      # 2. Set the CRS for mean_raster_2019
      crs(houston_mean_raster_2019) <- st_crs(grid_data_houston)$proj4string
      
      # 3. Set the extent of mean_raster_aligned to match grid_data
      extent(houston_mean_raster_2019) <- extent(grid_data_houston)
      
      #4. Filter pixels to keep
      grid_data_houston %>%
        filter(pixel_ID %in% houston_pixels) -> grid_data_houston_filtered
      
    # 6.1.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({houston_values <- raster::extract(houston_mean_raster_2019, grid_data_houston_filtered)}) #160 s = ~2.6 min
      grid_data_PM25_houston <- grid_data_houston_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_houston$PM25_AVG <- houston_values
      grid_data_PM25_houston$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_houston$PM25_AVG))
      
    # 6.1.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_houston_NAD <- st_transform(grid_data_PM25_houston, crs = st_crs("+proj=longlat +datum=NAD83"))
  
      #Get right columns 
      grid_data_PM25_houston_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_houston_NAD_filtered_final
      
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_houston_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Houston/PM25_2019_Houston.csv"))
      
      #Shapefile
      st_write(grid_data_PM25_houston_NAD, "output_data/shapefiles/1km/Houston/1_km_PM25_2019_houston.shp")
      
    # 6.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_houston %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> houston_pop_total
      
      grid_data_PM25_houston_NAD_filtered_final %>%
        left_join(houston_pop_total, by = c("Row", "Column"))  %>%
        filter(!is.na(Values))-> houston_pop_PM25_total
      
      houston_pop_PM25_total$product <- houston_pop_PM25_total$Values * houston_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(houston_pop_PM25_total$product)
      total_population <- sum(houston_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      houston_mean_BASE_2019 <- sum_products / total_population
      mean(houston_pop_PM25_total$Values)
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_houston_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_houston_NAD_filtered_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_houston_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_houston_NAD_filtered_graph, cbsa_shapefile_houston)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_houston_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2019 Houston Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(houston_mean_BASE_2019, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_houston, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -95.36946640065364,  y =  29.760080695254256), shape = 1, color = "#95C623", size = 6, stroke = 2) #Houston city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_houston.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_houston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p) 
    
  #### 6.2. 2050 HIGH ####
    # 6.2.1 Get mean value 
      houston_nc_1km_stack_2050_high <- stack("data/WRF-CMAQ/1km_V2/4_houston/Dailymean.1km.HighCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      houston_mean_raster_2050_high <- calc(houston_nc_1km_stack_2050_high, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(houston_mean_raster_2050_high, col = rev(magma(20)), main = "Houston 2050 High Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 6.2.2. Get rows and columns matching
      crs(houston_mean_raster_2050_high) <- st_crs(grid_data_houston)$proj4string
      extent(houston_mean_raster_2050_high) <- extent(grid_data_houston)
  
    # 6.2.3. Get values from raster and assign them to my grid 
      system.time({houston_values_2050_high <- raster::extract(houston_mean_raster_2050_high, grid_data_houston_filtered)}) #172s
      grid_data_PM25_houston_2050_high <- grid_data_houston_filtered
    
      grid_data_PM25_houston_2050_high$PM25_AVG <- houston_values_2050_high
      grid_data_PM25_houston_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_houston_2050_high$PM25_AVG))
      
    # 6.2.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_houston_2050_high_NAD <- st_transform(grid_data_PM25_houston_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))

      grid_data_PM25_houston_2050_high_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_houston_2050_high_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_houston_2050_high_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Houston/PM25_2050_high_Houston.csv"))
      
      st_write(grid_data_PM25_houston_2050_high_NAD, "output_data/shapefiles/1km/Houston/1_km_PM25_2050_high_houston.shp")
      
    # 6.2.5. Prepare figure
      grid_data_PM25_houston_2050_high_NAD_filtered_final %>%
        left_join(houston_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> houston_pop_PM25_total_2050_high
      
      houston_pop_PM25_total_2050_high$product <- houston_pop_PM25_total_2050_high$Values * houston_pop_PM25_total_2050_high$population
      sum_products <- sum(houston_pop_PM25_total_2050_high$product)
      total_population <- sum(houston_pop_PM25_total_2050_high$population)
      houston_mean_2050_high <- sum_products / total_population
      mean(houston_pop_PM25_total_2050_high$Values)
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_houston_2050_high_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_houston_2050_high_NAD_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_houston_2050_high_NAD_graph_crop <- st_intersection(grid_data_PM25_houston_2050_high_NAD_graph, cbsa_shapefile_houston)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_houston_2050_high_NAD_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050 High Houston Mean 1km Yearly PM2.5 (ug/m3)") +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_houston, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -95.36946640065364,  y =  29.760080695254256), shape = 1, color = "#95C623", size = 6, stroke = 2) + #Houston city hall
        geom_text(aes(label = paste("Population weighted mean:", round(houston_mean_2050_high, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_houston.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_houston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)   
  
  #### 6.3. 2050 LOW ####
    # 6.3.1 Get mean value 
      houston_nc_1km_stack_2050_low <- stack("data/WRF-CMAQ/1km_V2/4_houston/Dailymean.1km.LowCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      houston_mean_raster_2050_low <- calc(houston_nc_1km_stack_2050_low, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(houston_mean_raster_2050_low, col = rev(magma(20)), main = "Houston 2050 low Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 6.3.2. Get rows and columns matching
      crs(houston_mean_raster_2050_low) <- st_crs(grid_data_houston)$proj4string
      extent(houston_mean_raster_2050_low) <- extent(grid_data_houston)
      
    # 6.3.3. Get values from raster and assign them to my grid 
      system.time({houston_values_2050_low <- raster::extract(houston_mean_raster_2050_low, grid_data_houston_filtered)}) #159s
      grid_data_PM25_houston_2050_low <- grid_data_houston_filtered
      
      grid_data_PM25_houston_2050_low$PM25_AVG <- houston_values_2050_low
      grid_data_PM25_houston_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_houston_2050_low$PM25_AVG))
      
    # 6.3.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_houston_2050_low_NAD <- st_transform(grid_data_PM25_houston_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_houston_2050_low_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_houston_2050_low_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_houston_2050_low_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Houston/PM25_2050_low_Houston.csv"))
      
      st_write(grid_data_PM25_houston_2050_low_NAD, "output_data/shapefiles/1km/Houston/1_km_PM25_2050_low_houston.shp")
      
    # 6.3.5. Prepare figure
      grid_data_PM25_houston_2050_low_NAD_filtered_final %>%
        left_join(houston_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> houston_pop_PM25_total_2050_low
      
      houston_pop_PM25_total_2050_low$product <- houston_pop_PM25_total_2050_low$Values * houston_pop_PM25_total_2050_low$population
      sum_products <- sum(houston_pop_PM25_total_2050_low$product)
      total_population <- sum(houston_pop_PM25_total_2050_low$population)
      houston_mean_2050_low <- sum_products / total_population
      mean(houston_pop_PM25_total_2050_low$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_houston_2050_low_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_houston_2050_low_NAD_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_houston_2050_low_NAD_graph_crop <- st_intersection(grid_data_PM25_houston_2050_low_NAD_graph, cbsa_shapefile_houston)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_houston_2050_low_NAD_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050 low Houston Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(houston_mean_2050_low, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_houston, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -95.36946640065364,  y =  29.760080695254256), shape = 1, color = "#95C623", size = 6, stroke = 2) #Houston city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_houston.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_houston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)   
      
#### 7. MIAMI ####
  #### 7.1. 2019 ####
    # 7.1.1 Get mean value 
      # Read the NetCDF file using stack
      miami_nc_1km_stack_2019 <- stack("data/WRF-CMAQ/1km_V2/5_miami/Dailymean.1km.BASE.2019.PM25_AVG.nc", varname = "PM25_AVG")
      # Calculate the mean across all bands (365 days) for each cell
      miami_mean_raster_2019 <- calc(miami_nc_1km_stack_2019, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      # Plot the mean raster
      plot(miami_mean_raster_2019, col = rev(magma(20)), main = "Miami 2019 Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 7.1.2. Get rows and columns matching
      #1. make sure geometries align
        bbox <- st_bbox(grid_data_miami$geometry)
        #Calculate the number of columns and rows 
        num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
        num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
        # 1. Check the CRS
        st_crs(grid_data_miami)
        crs(miami_mean_raster_2019)
        
        #2. Print extent information
        print(extent(miami_mean_raster_2019))
        print(st_bbox(grid_data_miami))
        
        #3. Check resolution
        res(miami_mean_raster_2019)
        # Get the bounding box of the grid_data
        bbox <- st_bbox(grid_data_miami)
        # Calculate the resolution
        resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                        (bbox$ymax - bbox$ymin) / num_rows)
        # Print the resolution
        print(resolution)
      
      # 2. Set the CRS for mean_raster_2019
      crs(miami_mean_raster_2019) <- st_crs(grid_data_miami)$proj4string
      
      # 3. Set the extent of mean_raster_aligned to match grid_data
      extent(miami_mean_raster_2019) <- extent(grid_data_miami)
      
      #4. Filter pixels to keep
      grid_data_miami %>%
        filter(pixel_ID %in% miami_pixels) -> grid_data_miami_filtered
      
    # 7.1.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({miami_values <- raster::extract(miami_mean_raster_2019, grid_data_miami_filtered)}) #160 s = ~2.6 min
      grid_data_PM25_miami <- grid_data_miami_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_miami$PM25_AVG <- miami_values
      grid_data_PM25_miami$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_miami$PM25_AVG))
      
    # 7.1.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_miami_NAD <- st_transform(grid_data_PM25_miami, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      #Get right columns 
      grid_data_PM25_miami_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_miami_NAD_filtered_final
      
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_miami_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Miami/PM25_2019_Miami.csv"))
      
      #Shapefile
      st_write(grid_data_PM25_miami_NAD, "output_data/shapefiles/1km/Miami/1_km_PM25_2019_miami.shp")
      
    # 7.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_miami %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> miami_pop_total
      
      grid_data_PM25_miami_NAD_filtered_final %>%
        left_join(miami_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> miami_pop_PM25_total
      
      miami_pop_PM25_total$product <- miami_pop_PM25_total$Values * miami_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(miami_pop_PM25_total$product)
      total_population <- sum(miami_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      miami_mean_BASE_2019 <- sum_products / total_population
      mean(miami_pop_PM25_total$Values)
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_miami_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_miami_NAD_filtered_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_miami_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_miami_NAD_filtered_graph, cbsa_shapefile_miami)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_miami_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2019 Miami Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(miami_mean_BASE_2019, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_miami, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -80.23398527668328,  y =  25.72761157460114), shape = 1, color = "#95C623", size = 6, stroke = 2) #Miami city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_miami.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_miami.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
    
  #### 7.2. 2050 HIGH ####
    # 7.2.1 Get mean value 
      miami_nc_1km_stack_2050_high <- stack("data/WRF-CMAQ/1km_V2/5_miami/Dailymean.1km.HighCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      miami_mean_raster_2050_high <- calc(miami_nc_1km_stack_2050_high, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(miami_mean_raster_2050_high, col = rev(magma(20)), main = "Miami 2050_high Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 7.2.2. Get rows and columns matching
      crs(miami_mean_raster_2050_high) <- st_crs(grid_data_miami)$proj4string
      extent(miami_mean_raster_2050_high) <- extent(grid_data_miami)
      
    # 7.2.3. Get values from raster and assign them to my grid 
      system.time({miami_values_2050_high <- raster::extract(miami_mean_raster_2050_high, grid_data_miami_filtered)}) #104s
      grid_data_PM25_miami_2050_high <- grid_data_miami_filtered
      
      grid_data_PM25_miami_2050_high$PM25_AVG <- miami_values_2050_high
      grid_data_PM25_miami_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_miami_2050_high$PM25_AVG))
      
    # 7.2.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_miami_2050_high_NAD <- st_transform(grid_data_PM25_miami_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_miami_2050_high_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_miami_2050_high_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_miami_2050_high_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Miami/PM25_2050_high_Miami.csv"))
      
      st_write(grid_data_PM25_miami_2050_high_NAD, "output_data/shapefiles/1km/Miami/1_km_PM25_2050_high_miami.shp")
      
    # 7.2.5. Prepare figure
      grid_data_PM25_miami_2050_high_NAD_filtered_final %>%
        left_join(miami_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> miami_pop_PM25_total_2050_high
      
      miami_pop_PM25_total_2050_high$product <- miami_pop_PM25_total_2050_high$Values * miami_pop_PM25_total_2050_high$population
      sum_products <- sum(miami_pop_PM25_total_2050_high$product)
      total_population <- sum(miami_pop_PM25_total_2050_high$population)
      miami_mean_2050_high <- sum_products / total_population
      mean(miami_pop_PM25_total_2050_high$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_miami_2050_high_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> ggrid_data_PM25_miami_2050_high_NAD_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_miami_2050_high_NAD_graph_crop <- st_intersection(ggrid_data_PM25_miami_2050_high_NAD_graph, cbsa_shapefile_miami)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_miami_2050_high_NAD_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050 High Miami Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(miami_mean_2050_high, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_miami, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -80.23398527668328,  y =  25.72761157460114), shape = 1, color = "#95C623", size = 6, stroke = 2) #Miami city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_miami.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_miami.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 7.3. 2050 LOW ####
    # 7.3.1 Get mean value 
      miami_nc_1km_stack_2050_low <- stack("data/WRF-CMAQ/1km_V2/5_miami/Dailymean.1km.LowCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      miami_mean_raster_2050_low <- calc(miami_nc_1km_stack_2050_low, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(miami_mean_raster_2050_low, col = rev(magma(20)), main = "Miami 2050_low Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 7.3.2. Get rows and columns matching
      crs(miami_mean_raster_2050_low) <- st_crs(grid_data_miami)$proj4string
      extent(miami_mean_raster_2050_low) <- extent(grid_data_miami)
      
    # 7.3.3. Get values from raster and assign them to my grid 
      system.time({miami_values_2050_low <- raster::extract(miami_mean_raster_2050_low, grid_data_miami_filtered)}) #102s
      grid_data_PM25_miami_2050_low <- grid_data_miami_filtered
      
      grid_data_PM25_miami_2050_low$PM25_AVG <- miami_values_2050_low
      grid_data_PM25_miami_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_miami_2050_low$PM25_AVG))
      
    # 7.3.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_miami_2050_low_NAD <- st_transform(grid_data_PM25_miami_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_miami_2050_low_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_miami_2050_low_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_miami_2050_low_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Miami/PM25_2050_low_Miami.csv"))
      
      st_write(grid_data_PM25_miami_2050_low_NAD, "output_data/shapefiles/1km/Miami/1_km_PM25_2050_low_miami.shp")
      
    # 7.3.5. Prepare figure
      grid_data_PM25_miami_2050_low_NAD_filtered_final %>%
        left_join(miami_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> miami_pop_PM25_total_2050_low
      
      miami_pop_PM25_total_2050_low$product <- miami_pop_PM25_total_2050_low$Values * miami_pop_PM25_total_2050_low$population
      sum_products <- sum(miami_pop_PM25_total_2050_low$product)
      total_population <- sum(miami_pop_PM25_total_2050_low$population)
      miami_mean_2050_low <- sum_products / total_population
      mean(miami_pop_PM25_total_2050_low$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_miami_2050_low_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> ggrid_data_PM25_miami_2050_low_NAD_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_miami_2050_low_NAD_graph_crop <- st_intersection(ggrid_data_PM25_miami_2050_low_NAD_graph, cbsa_shapefile_miami)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_miami_2050_low_NAD_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050 low Miami Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(miami_mean_2050_low, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_miami, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -80.23398527668328,  y =  25.72761157460114), shape = 1, color = "#95C623", size = 6, stroke = 2) #Miami city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_miami.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_miami.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
    
#### 8. ATLANTA ####
  #### 8.1. 2019 ####
    # 8.1.1 Get mean value 
      # Read the NetCDF file using stack
      atlanta_nc_1km_stack_2019 <- stack("data/WRF-CMAQ/1km_V2/6_atlanta/Dailymean.1km.BASE.2019.PM25_AVG.nc", varname = "PM25_AVG")
      # Calculate the mean across all bands (365 days) for each cell
      atlanta_mean_raster_2019 <- calc(atlanta_nc_1km_stack_2019, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      # Plot the mean raster
      plot(atlanta_mean_raster_2019, col = rev(magma(20)), main = "Atlanta 2019 Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 8.1.2. Get rows and columns matching
      #1. make sure geometries align
      bbox <- st_bbox(grid_data_atl$geometry)
      #Calculate the number of columns and rows 
      num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
      num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
      # 1. Check the CRS
      st_crs(grid_data_atl)
      crs(atlanta_mean_raster_2019)
      
      #2. Print extent information
      print(extent(atlanta_mean_raster_2019))
      print(st_bbox(grid_data_atl))
      
      #3. Check resolution
      res(atlanta_mean_raster_2019)
      # Get the bounding box of the grid_data
      bbox <- st_bbox(grid_data_atl)
      # Calculate the resolution
      resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                      (bbox$ymax - bbox$ymin) / num_rows)
      # Print the resolution
      print(resolution)
      
      # 2. Set the CRS for mean_raster_2019
      crs(atlanta_mean_raster_2019) <- st_crs(grid_data_atl)$proj4string
      
      # 3. Set the extent of mean_raster_aligned to match grid_data
      extent(atlanta_mean_raster_2019) <- extent(grid_data_atl)
      
      #4. Filter pixels to keep
      grid_data_atl %>%
        filter(pixel_ID %in% atl_pixels) -> grid_data_atlanta_filtered
      
    # 8.1.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({atlanta_values <- raster::extract(atlanta_mean_raster_2019, grid_data_atlanta_filtered)}) #161 s = ~2.6 min
      grid_data_PM25_atlanta <- grid_data_atlanta_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_atlanta$PM25_AVG <- atlanta_values
      grid_data_PM25_atlanta$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_atlanta$PM25_AVG))
      
    # 8.1.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_atlanta_NAD <- st_transform(grid_data_PM25_atlanta, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      #Get right columns 
      grid_data_PM25_atlanta_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_atlanta_NAD_filtered_final
      
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_atlanta_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Atlanta/PM25_2019_Atlanta.csv"))
      
      #Shapefile
      st_write(grid_data_PM25_atlanta_NAD, "output_data/shapefiles/1km/Atlanta/1_km_PM25_2019_atlanta.shp")
      
    # 8.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_atl %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> atlanta_pop_total
      
      grid_data_PM25_atlanta_NAD_filtered_final %>%
        left_join(atlanta_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values)) -> atlanta_pop_PM25_total
      
      atlanta_pop_PM25_total$product <- atlanta_pop_PM25_total$Values * atlanta_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(atlanta_pop_PM25_total$product)
      total_population <- sum(atlanta_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      atlanta_mean_BASE_2019 <- sum_products / total_population
      mean(atlanta_pop_PM25_total$Values)
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_atlanta_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_atlanta_NAD_filtered_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_atlanta_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_atlanta_NAD_filtered_graph, cbsa_shapefile_atl)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_atlanta_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2019 Atlanta Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(atlanta_mean_BASE_2019, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_atl, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -84.39062010249631,  y =  33.74857740874673), shape = 1, color = "#95C623", size = 6, stroke = 2) #Atlanta city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_atlanta_V2.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_atlanta.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)   
    
  #### 8.2. 2050 HIGH ####
    # 8.2.1 Get mean value 
      atlanta_nc_1km_stack_2050_high <- stack("data/WRF-CMAQ/1km_V2/6_atlanta/Dailymean.1km.HighCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      atlanta_mean_raster_2050_high <- calc(atlanta_nc_1km_stack_2050_high, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(atlanta_mean_raster_2050_high, col = rev(magma(20)), main = "Atlanta 2050_high Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 8.2.2. Get rows and columns matching
      crs(atlanta_mean_raster_2050_high) <- st_crs(grid_data_atl)$proj4string
      extent(atlanta_mean_raster_2050_high) <- extent(grid_data_atl)
      
    # 8.2.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({atlanta_values_2050_high <- raster::extract(atlanta_mean_raster_2050_high, grid_data_atlanta_filtered)}) #166s
      grid_data_PM25_atlanta_2050_high <- grid_data_atlanta_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_atlanta_2050_high$PM25_AVG <- atlanta_values_2050_high
      grid_data_PM25_atlanta_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_atlanta_2050_high$PM25_AVG))
      
    # 8.2.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_atlanta_2050_high_NAD <- st_transform(grid_data_PM25_atlanta_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_atlanta_2050_high_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_atlanta_2050_high_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_atlanta_2050_high_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Atlanta/PM25_2050_high_Atlanta.csv"))
      
      st_write(grid_data_PM25_atlanta_2050_high_NAD, "output_data/shapefiles/1km/Atlanta/1_km_PM25_2050_high_atlanta.shp")
      
    # 8.2.5. Prepare figure
      grid_data_PM25_atlanta_2050_high_NAD_filtered_final %>%
        left_join(atlanta_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values)) -> atlanta_pop_PM25_total_2050_high
      
      atlanta_pop_PM25_total_2050_high$product <- atlanta_pop_PM25_total_2050_high$Values * atlanta_pop_PM25_total_2050_high$population
      sum_products <- sum(atlanta_pop_PM25_total_2050_high$product)
      total_population <- sum(atlanta_pop_PM25_total_2050_high$population)
      atlanta_mean_2050_high <- sum_products / total_population
      mean(atlanta_pop_PM25_total_2050_high$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_atlanta_2050_high_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_atlanta_2050_high_NAD_filtered_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_atlanta_2050_high_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_atlanta_2050_high_NAD_filtered_graph, cbsa_shapefile_atl)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_atlanta_2050_high_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_high Atlanta Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(atlanta_mean_2050_high, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_atl, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -84.39062010249631,  y =  33.74857740874673), shape = 1, color = "#95C623", size = 6, stroke = 2) #Atlanta city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_atlanta.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_atlanta.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)  
        
  #### 8.3. 2050 LOW ####
    # 8.3.1 Get mean value 
      atlanta_nc_1km_stack_2050_low <- stack("data/WRF-CMAQ/1km_V2/6_atlanta/Dailymean.1km.LowCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      atlanta_mean_raster_2050_low <- calc(atlanta_nc_1km_stack_2050_low, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(atlanta_mean_raster_2050_low, col = rev(magma(20)), main = "Atlanta 2050_low Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 8.3.2. Get rows and columns matching
      crs(atlanta_mean_raster_2050_low) <- st_crs(grid_data_atl)$proj4string
      extent(atlanta_mean_raster_2050_low) <- extent(grid_data_atl)
      
    # 8.3.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({atlanta_values_2050_low <- raster::extract(atlanta_mean_raster_2050_low, grid_data_atlanta_filtered)}) #157
      grid_data_PM25_atlanta_2050_low <- grid_data_atlanta_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_atlanta_2050_low$PM25_AVG <- atlanta_values_2050_low
      grid_data_PM25_atlanta_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_atlanta_2050_low$PM25_AVG))
      
    # 8.3.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_atlanta_2050_low_NAD <- st_transform(grid_data_PM25_atlanta_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_atlanta_2050_low_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_atlanta_2050_low_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_atlanta_2050_low_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Atlanta/PM25_2050_low_Atlanta.csv"))
      
      st_write(grid_data_PM25_atlanta_2050_low_NAD, "output_data/shapefiles/1km/Atlanta/1_km_PM25_2050_low_atlanta.shp")
      
    # 8.3.5. Prepare figure
      grid_data_PM25_atlanta_2050_low_NAD_filtered_final %>%
        left_join(atlanta_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values)) -> atlanta_pop_PM25_total_2050_low
      
      atlanta_pop_PM25_total_2050_low$product <- atlanta_pop_PM25_total_2050_low$Values * atlanta_pop_PM25_total_2050_low$population
      sum_products <- sum(atlanta_pop_PM25_total_2050_low$product)
      total_population <- sum(atlanta_pop_PM25_total_2050_low$population)
      atlanta_mean_2050_low <- sum_products / total_population
      mean(atlanta_pop_PM25_total_2050_low$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_atlanta_2050_low_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_atlanta_2050_low_NAD_filtered_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_atlanta_2050_low_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_atlanta_2050_low_NAD_filtered_graph, cbsa_shapefile_atl)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_atlanta_2050_low_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_low Atlanta Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(atlanta_mean_2050_low, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_atl, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -84.39062010249631,  y =  33.74857740874673), shape = 1, color = "#95C623", size = 6, stroke = 2) #Atlanta city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_atlanta.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_atlanta.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)  
 
#### 9. CHICAGO ####
  #### 9.1. 2019 ####
    # 9.1.1 Get mean value 
      # Read the NetCDF file using stack
      chicago_nc_1km_stack_2019 <- stack("data/WRF-CMAQ/1km_V2/7_chicago/Dailymean.1km.BASE.2019.PM25_AVG.nc", varname = "PM25_AVG")
      # Calculate the mean across all bands (365 days) for each cell
      chicago_mean_raster_2019 <- calc(chicago_nc_1km_stack_2019, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      # Plot the mean raster
      plot(chicago_mean_raster_2019, col = rev(magma(20)), main = "Chicago 2019 Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 9.1.2. Get rows and columns matching
      #1. make sure geometries align
        bbox <- st_bbox(grid_data_chicago$geometry)
        #Calculate the number of columns and rows 
        num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
        num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
        # 1. Check the CRS
        st_crs(grid_data_chicago)
        crs(chicago_mean_raster_2019)
        #2. Print extent information
        print(extent(chicago_mean_raster_2019))
        print(st_bbox(grid_data_chicago))
        #3. Check resolution
        res(chicago_mean_raster_2019)
        # Get the bounding box of the grid_data
        bbox <- st_bbox(grid_data_chicago)
        # Calculate the resolution
        resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                        (bbox$ymax - bbox$ymin) / num_rows)
        # Print the resolution
        print(resolution)
      
      # 2. Set the CRS for mean_raster_2019
      crs(chicago_mean_raster_2019) <- st_crs(grid_data_chicago)$proj4string
      
      # 3. Set the extent of mean_raster_aligned to match grid_data
      extent(chicago_mean_raster_2019) <- extent(grid_data_chicago)
      
      #4. Filter pixels to keep
      grid_data_chicago %>%
        filter(pixel_ID %in% chicago_pixels) -> grid_data_chicago_filtered
      
    # 9.1.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({chicago_values <- raster::extract(chicago_mean_raster_2019, grid_data_chicago_filtered)}) #132s
      grid_data_PM25_chicago <- grid_data_chicago_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_chicago$PM25_AVG <- chicago_values
      grid_data_PM25_chicago$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_chicago$PM25_AVG))
      
    # 9.1.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_chicago_NAD <- st_transform(grid_data_PM25_chicago, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      #Get right columns 
      grid_data_PM25_chicago_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_chicago_NAD_filtered_final
      
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_chicago_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Chicago/PM25_2019_Chicago.csv"))
      
      #Shapefile
      st_write(grid_data_PM25_chicago_NAD, "output_data/shapefiles/1km/Chicago/1_km_PM25_2019_chicago.shp")
      
    # 9.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_chicago %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> chicago_pop_total
      
      grid_data_PM25_chicago_NAD_filtered_final %>%
        left_join(chicago_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> chicago_pop_PM25_total
      
      chicago_pop_PM25_total$product <- chicago_pop_PM25_total$Values * chicago_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(chicago_pop_PM25_total$product)
      total_population <- sum(chicago_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      chicago_mean_BASE_2019 <- sum_products / total_population
      mean(chicago_pop_PM25_total$Values)
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_chicago_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_chicago_NAD_filtered_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_chicago_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_chicago_NAD_filtered_graph, cbsa_shapefile_chicago)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_chicago_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2019 Chicago Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(chicago_mean_BASE_2019, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_chicago, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -87.63207808921189,  y =  41.8837839226344), shape = 1, color = "#95C623", size = 6, stroke = 2) #Chicago city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_chicago.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_chicago.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)   
      
  #### 9.2. 2050 HIGH ####
    # 9.2.1 Get mean value 
      chicago_nc_1km_stack_2050_high <- stack("data/WRF-CMAQ/1km_V2/7_chicago/Dailymean.1km.HighCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      chicago_mean_raster_2050_high <- calc(chicago_nc_1km_stack_2050_high, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(chicago_mean_raster_2050_high, col = rev(magma(20)), main = "Chicago 2050_high Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 9.2.2. Get rows and columns matching
      crs(chicago_mean_raster_2050_high) <- st_crs(grid_data_chicago)$proj4string
      extent(chicago_mean_raster_2050_high) <- extent(grid_data_chicago)
      
    # 9.2.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({chicago_values_2050_high <- raster::extract(chicago_mean_raster_2050_high, grid_data_chicago_filtered)}) #134s
      grid_data_PM25_chicago_2050_high <- grid_data_chicago_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_chicago_2050_high$PM25_AVG <- chicago_values_2050_high
      grid_data_PM25_chicago_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_chicago_2050_high$PM25_AVG))
      
    # 9.2.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_chicago_2050_high_NAD <- st_transform(grid_data_PM25_chicago_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
      grid_data_PM25_chicago_2050_high_NAD %>%
        filter(!is.na(PM25_AVG)) -> grid_data_PM25_chicago_2050_high_NAD
      
      grid_data_PM25_chicago_2050_high_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_chicago_2050_high_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_chicago_2050_high_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Chicago/PM25_2050_high_Chicago.csv"))
      
      st_write(grid_data_PM25_chicago_2050_high_NAD, "output_data/shapefiles/1km/Chicago/1_km_PM25_2050_high_chicago.shp")
      
    # 9.2.5. Prepare figure
      grid_data_PM25_chicago_2050_high_NAD_filtered_final %>%
        left_join(chicago_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> chicago_pop_PM25_total_2050_high
      
      chicago_pop_PM25_total_2050_high$product <- chicago_pop_PM25_total_2050_high$Values * chicago_pop_PM25_total_2050_high$population
      sum_products <- sum(chicago_pop_PM25_total_2050_high$product)
      total_population <- sum(chicago_pop_PM25_total_2050_high$population)
      chicago_mean_2050_high <- sum_products / total_population
      mean(chicago_pop_PM25_total_2050_high$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_chicago_2050_high_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_chicago_2050_high_NAD_filtered_graph
      
      # Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_chicago_2050_high_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_chicago_2050_high_NAD_filtered_graph, cbsa_shapefile_chicago)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_chicago_2050_high_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_high Chicago Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(chicago_mean_2050_high, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_chicago, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -87.63207808921189,  y =  41.8837839226344), shape = 1, color = "#95C623", size = 6, stroke = 2) #Chicago city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_chicago.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_chicago.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)  
      
  #### 9.3. 2050 LOW ####
    # 9.3.1 Get mean value 
      chicago_nc_1km_stack_2050_low <- stack("data/WRF-CMAQ/1km_V2/7_chicago/Dailymean.1km.LowCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      chicago_mean_raster_2050_low <- calc(chicago_nc_1km_stack_2050_low, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(chicago_mean_raster_2050_low, col = rev(magma(20)), main = "Chicago 2050_low Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 9.3.2. Get rows and columns matching
      crs(chicago_mean_raster_2050_low) <- st_crs(grid_data_chicago)$proj4string
      extent(chicago_mean_raster_2050_low) <- extent(grid_data_chicago)
      
    # 9.3.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({chicago_values_2050_low <- raster::extract(chicago_mean_raster_2050_low, grid_data_chicago_filtered)}) #134
      grid_data_PM25_chicago_2050_low <- grid_data_chicago_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_chicago_2050_low$PM25_AVG <- chicago_values_2050_low
      grid_data_PM25_chicago_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_chicago_2050_low$PM25_AVG))
      
    # 9.3.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_chicago_2050_low_NAD <- st_transform(grid_data_PM25_chicago_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
    
      grid_data_PM25_chicago_2050_low_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_chicago_2050_low_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_chicago_2050_low_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Chicago/PM25_2050_low_Chicago.csv"))
      
      st_write(grid_data_PM25_chicago_2050_low_NAD, "output_data/shapefiles/1km/Chicago/1_km_PM25_2050_low_chicago.shp")
      
    # 9.3.5. Prepare figure
      grid_data_PM25_chicago_2050_low_NAD_filtered_final %>%
        left_join(chicago_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> chicago_pop_PM25_total_2050_low
      
      chicago_pop_PM25_total_2050_low$product <- chicago_pop_PM25_total_2050_low$Values * chicago_pop_PM25_total_2050_low$population
      sum_products <- sum(chicago_pop_PM25_total_2050_low$product)
      total_population <- sum(chicago_pop_PM25_total_2050_low$population)
      chicago_mean_2050_low <- sum_products / total_population
      mean(chicago_pop_PM25_total_2050_low$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_chicago_2050_low_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_chicago_2050_low_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_chicago_2050_low_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_chicago_2050_low_NAD_filtered_graph, cbsa_shapefile_chicago)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_chicago_2050_low_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_low Chicago Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(chicago_mean_2050_low, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_chicago, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -87.63207808921189,  y =  41.8837839226344), shape = 1, color = "#95C623", size = 6, stroke = 2) #Chicago city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_chicago.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_chicago.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)     
      
#### 10. WASHINGTON DC ####
  #### 10.1. 2019 ####
    # 10.1.1 Get mean value 
      # Read the NetCDF file using stack
      wash_nc_1km_stack_2019 <- stack("data/WRF-CMAQ/1km_V2/8_washington/Dailymean.1km.BASE.2019.PM25_AVG.nc", varname = "PM25_AVG")
      # Calculate the mean across all bands (365 days) for each cell
      wash_mean_raster_2019 <- calc(wash_nc_1km_stack_2019, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      # Plot the mean raster
      plot(wash_mean_raster_2019, col = rev(magma(20)), main = "DC 2019 Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 10.1.2. Get rows and columns matching
      #1. make sure geometries align
      bbox <- st_bbox(grid_data_wash$geometry)
      #Calculate the number of columns and rows 
      num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
      num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
      # 1. Check the CRS
      st_crs(grid_data_wash)
      crs(wash_mean_raster_2019)
      #2. Print extent information
      print(extent(wash_mean_raster_2019))
      print(st_bbox(grid_data_wash))
      #3. Check resolution
      res(wash_mean_raster_2019)
      # Get the bounding box of the grid_data
      bbox <- st_bbox(grid_data_wash)
      # Calculate the resolution
      resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                      (bbox$ymax - bbox$ymin) / num_rows)
      # Print the resolution
      print(resolution)
      
      # 2. Set the CRS for mean_raster_2019
      crs(wash_mean_raster_2019) <- st_crs(grid_data_wash)$proj4string
      
      # 3. Set the extent of mean_raster_aligned to match grid_data
      extent(wash_mean_raster_2019) <- extent(grid_data_wash)
      
      #4. Filter pixels to keep
      grid_data_wash %>%
        filter(pixel_ID %in% wash_pixels) -> grid_data_wash_filtered
      
    # 10.1.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({wash_values <- raster::extract(wash_mean_raster_2019, grid_data_wash_filtered)}) #126s
      grid_data_PM25_wash <- grid_data_wash_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_wash$PM25_AVG <- wash_values
      grid_data_PM25_wash$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_wash$PM25_AVG))
      
    # 10.1.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_wash_NAD <- st_transform(grid_data_PM25_wash, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      #Get right columns 
      grid_data_PM25_wash_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_wash_NAD_filtered_final
      
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_wash_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/DC/PM25_2019_DC.csv"))
      
      #Shapefile
      st_write(grid_data_PM25_wash_NAD, "output_data/shapefiles/1km/DC/1_km_PM25_2019_wash.shp")
      
    # 10.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_wash %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> wash_pop_total
      
      grid_data_PM25_wash_NAD_filtered_final %>%
        left_join(wash_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> wash_pop_PM25_total
      
      wash_pop_PM25_total$product <- wash_pop_PM25_total$Values * wash_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(wash_pop_PM25_total$product)
      total_population <- sum(wash_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      wash_mean_BASE_2019 <- sum_products / total_population
      mean(wash_pop_PM25_total$Values)
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_wash_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_wash_NAD_filtered_graph
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_wash_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_wash_NAD_filtered_graph, cbsa_shapefile_wash)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_wash_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2019 DC Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(wash_mean_BASE_2019, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_wash, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -77.03653714296149,  y =  38.89771345976116), shape = 1, color = "#95C623", size = 6, stroke = 2) #The White House
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_wash.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_wash.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)   
      
  #### 10.2. 2050 HIGH ####
    # 10.2.1 Get mean value 
      wash_nc_1km_stack_2050_high <- stack("data/WRF-CMAQ/1km_V2/8_washington/Dailymean.1km.HighCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      wash_mean_raster_2050_high <- calc(wash_nc_1km_stack_2050_high, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(wash_mean_raster_2050_high, col = rev(magma(20)), main = "DC 2050_high Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 10.2.2. Get rows and columns matching
      crs(wash_mean_raster_2050_high) <- st_crs(grid_data_wash)$proj4string
      extent(wash_mean_raster_2050_high) <- extent(grid_data_wash)
      
    # 10.2.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({wash_values_2050_high <- raster::extract(wash_mean_raster_2050_high, grid_data_wash_filtered)}) #131s
      grid_data_PM25_wash_2050_high <- grid_data_wash_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_wash_2050_high$PM25_AVG <- wash_values_2050_high
      grid_data_PM25_wash_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_wash_2050_high$PM25_AVG))
      
    # 10.2.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_wash_2050_high_NAD <- st_transform(grid_data_PM25_wash_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_wash_2050_high_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_wash_2050_high_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_wash_2050_high_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/DC/PM25_2050_high_DC.csv"))
      
      st_write(grid_data_PM25_wash_2050_high_NAD, "output_data/shapefiles/1km/DC/1_km_PM25_2050_high_wash.shp")
      
    # 10.2.5. Prepare figure
      grid_data_PM25_wash_2050_high_NAD_filtered_final %>%
        left_join(wash_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> wash_pop_PM25_total_2050_high
      
      wash_pop_PM25_total_2050_high$product <- wash_pop_PM25_total_2050_high$Values * wash_pop_PM25_total_2050_high$population
      sum_products <- sum(wash_pop_PM25_total_2050_high$product)
      total_population <- sum(wash_pop_PM25_total_2050_high$population)
      wash_mean_2050_high <- sum_products / total_population
      mean(wash_pop_PM25_total_2050_high$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_wash_2050_high_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_wash_2050_high_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_wash_2050_high_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_wash_2050_high_NAD_filtered_graph, cbsa_shapefile_wash)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_wash_2050_high_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_high DC Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(wash_mean_2050_high, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_wash, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -77.03653714296149,  y =  38.89771345976116), shape = 1, color = "#95C623", size = 6, stroke = 2) #The White House
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_wash.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_wash.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)  
      
  #### 10.3. 2050 LOW ####
    # 10.3.1 Get mean value 
      wash_nc_1km_stack_2050_low <- stack("data/WRF-CMAQ/1km_V2/8_washington/Dailymean.1km.LowCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      wash_mean_raster_2050_low <- calc(wash_nc_1km_stack_2050_low, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(wash_mean_raster_2050_low, col = rev(magma(20)), main = "DC 2050_low Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 10.3.2. Get rows and columns matching
      crs(wash_mean_raster_2050_low) <- st_crs(grid_data_wash)$proj4string
      extent(wash_mean_raster_2050_low) <- extent(grid_data_wash)
      
    # 10.3.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({wash_values_2050_low <- raster::extract(wash_mean_raster_2050_low, grid_data_wash_filtered)}) #120
      grid_data_PM25_wash_2050_low <- grid_data_wash_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_wash_2050_low$PM25_AVG <- wash_values_2050_low
      grid_data_PM25_wash_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_wash_2050_low$PM25_AVG))
      
    # 10.3.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_wash_2050_low_NAD <- st_transform(grid_data_PM25_wash_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_wash_2050_low_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_wash_2050_low_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_wash_2050_low_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/DC/PM25_2050_low_DC.csv"))
      
      st_write(grid_data_PM25_wash_2050_low_NAD, "output_data/shapefiles/1km/DC/1_km_PM25_2050_low_wash.shp")
      
    # 10.3.5. Prepare figure
      grid_data_PM25_wash_2050_low_NAD_filtered_final %>%
        left_join(wash_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values)) -> wash_pop_PM25_total_2050_low
      
      wash_pop_PM25_total_2050_low$product <- wash_pop_PM25_total_2050_low$Values * wash_pop_PM25_total_2050_low$population
      sum_products <- sum(wash_pop_PM25_total_2050_low$product)
      total_population <- sum(wash_pop_PM25_total_2050_low$population)
      wash_mean_2050_low <- sum_products / total_population
      mean(wash_pop_PM25_total_2050_low$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_wash_2050_low_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_wash_2050_low_NAD_filtered_graph
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_wash_2050_low_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_wash_2050_low_NAD_filtered_graph, cbsa_shapefile_wash)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_wash_2050_low_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_low DC Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(wash_mean_2050_low, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_wash, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -77.03653714296149,  y =  38.89771345976116), shape = 1, color = "#95C623", size = 6, stroke = 2) #The White House
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_wash.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_wash.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)   
      
#### 11. PHILADELPHIA  ####
  #### 11.1. 2019 ####
    # 11.1.1 Get mean value 
      # Read the NetCDF file using stack
      phil_ny_nc_1km_stack_2019 <- stack("data/WRF-CMAQ/1km_V2/9_phil_ny/Dailymean.1km.BASE.2019.PM25_AVG.nc", varname = "PM25_AVG")
      # Calculate the mean across all bands (365 days) for each cell
      phil_ny_mean_raster_2019 <- calc(phil_ny_nc_1km_stack_2019, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      # Plot the mean raster
      plot(phil_ny_mean_raster_2019, col = rev(magma(20)), main = "Northeast 2019 Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 11.1.2. Get rows and columns matching
      #1. make sure geometries align
      bbox <- st_bbox(grid_data_northeast$geometry)
      #Calculate the number of columns and rows 
      num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
      num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
      # 1. Check the CRS
      st_crs(grid_data_northeast)
      crs(phil_ny_mean_raster_2019)
      #2. Print extent information
      print(extent(phil_ny_mean_raster_2019))
      print(st_bbox(grid_data_northeast))
      #3. Check resolution
      res(phil_ny_mean_raster_2019)
      # Get the bounding box of the grid_data
      bbox <- st_bbox(grid_data_northeast)
      # Calculate the resolution
      resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                      (bbox$ymax - bbox$ymin) / num_rows)
      # Print the resolution
      print(resolution)
      
      # 2. Set the CRS for mean_raster_2019
      crs(phil_ny_mean_raster_2019) <- st_crs(grid_data_northeast)$proj4string
      
      # 3. Set the extent of mean_raster_aligned to match grid_data
      extent(phil_ny_mean_raster_2019) <- extent(grid_data_northeast)
      
      #4. Filter pixels to keep
      grid_data_northeast %>%
        filter(pixel_ID %in% phil_pixels) -> grid_data_northeast_filtered_phil
      
    # 11.1.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({phil_values <- raster::extract(phil_ny_mean_raster_2019, grid_data_northeast_filtered_phil)}) #89s
      grid_data_PM25_phil <- grid_data_northeast_filtered_phil
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_phil$PM25_AVG <- phil_values
      grid_data_PM25_phil$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_phil$PM25_AVG))
      
    # 11.1.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_phil_NAD <- st_transform(grid_data_PM25_phil, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      #Get right columns 
      grid_data_PM25_phil_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_phil_NAD_filtered_final
      
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_phil_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Philadelphia/PM25_2019_Philadelphia.csv"))
      
      #Shapefile
      st_write(grid_data_PM25_phil_NAD, "output_data/shapefiles/1km/Philadelphia/1_km_PM25_2019_phil.shp")
      
    # 11.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_phil %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> phil_pop_total
      
      grid_data_PM25_phil_NAD_filtered_final %>%
        left_join(phil_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> phil_pop_PM25_total
      
      phil_pop_PM25_total$product <- phil_pop_PM25_total$Values * phil_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(phil_pop_PM25_total$product)
      total_population <- sum(phil_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      phil_mean_BASE_2019 <- sum_products / total_population
      mean(phil_pop_PM25_total$Values)
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_phil_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_phil_NAD_filtered_graph
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_phil_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_phil_NAD_filtered_graph, cbsa_shapefile_phil)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_phil_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2019 Philadelphia Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(phil_mean_BASE_2019, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_phil, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -75.16326872597556,  y =  39.95279997140329), shape = 1, color = "#95C623", size = 6, stroke = 2) #Philadelphia city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_phil.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_phil.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)   
      
  #### 11.2. 2050 HIGH ####
    # 11.2.1 Get mean value 
      phil_nc_1km_stack_2050_high <- stack("data/WRF-CMAQ/1km_V2/9_phil_ny/Dailymean.1km.HighCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      phil_mean_raster_2050_high <- calc(phil_nc_1km_stack_2050_high, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(phil_mean_raster_2050_high, col = rev(magma(20)), main = "Philadelphia 2050_high Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 11.2.2. Get rows and columns matching
      crs(phil_mean_raster_2050_high) <- st_crs(grid_data_northeast)$proj4string
      extent(phil_mean_raster_2050_high) <- extent(grid_data_northeast)
      
    # 11.2.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({phil_values_2050_high <- raster::extract(phil_mean_raster_2050_high, grid_data_northeast_filtered_phil)}) #86s
      grid_data_PM25_phil_2050_high <- grid_data_northeast_filtered_phil
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_phil_2050_high$PM25_AVG <- phil_values_2050_high
      grid_data_PM25_phil_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_phil_2050_high$PM25_AVG))
      
    # 11.2.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_phil_2050_high_NAD <- st_transform(grid_data_PM25_phil_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_phil_2050_high_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_phil_2050_high_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_phil_2050_high_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Philadelphia/PM25_2050_high_Philadelphia.csv"))
      
      st_write(grid_data_PM25_phil_2050_high_NAD, "output_data/shapefiles/1km/Philadelphia/1_km_PM25_2050_high_phil.shp")
      
    # 11.2.5. Prepare figure
      grid_data_PM25_phil_2050_high_NAD_filtered_final %>%
        left_join(phil_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> phil_pop_PM25_total_2050_high
      
      phil_pop_PM25_total_2050_high$product <- phil_pop_PM25_total_2050_high$Values * phil_pop_PM25_total_2050_high$population
      sum_products <- sum(phil_pop_PM25_total_2050_high$product)
      total_population <- sum(phil_pop_PM25_total_2050_high$population)
      phil_mean_2050_high <- sum_products / total_population
      mean(phil_pop_PM25_total_2050_high$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_phil_2050_high_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_phil_2050_high_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_phil_2050_high_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_phil_2050_high_NAD_filtered_graph, cbsa_shapefile_phil)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_phil_2050_high_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_high Philadelphia Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(phil_mean_2050_high, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_phil, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -75.16326872597556,  y =  39.95279997140329), shape = 1, color = "#95C623", size = 6, stroke = 2) #Philadelphia city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_phil.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_phil.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)  
      
  #### 11.3. 2050 LOW ####
    # 11.3.1 Get mean value 
      phil_nc_1km_stack_2050_low <- stack("data/WRF-CMAQ/1km_V2/9_phil_ny/Dailymean.1km.LowCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      phil_mean_raster_2050_low <- calc(phil_nc_1km_stack_2050_low, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(phil_mean_raster_2050_low, col = rev(magma(20)), main = "Philadelphia 2050_low Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 11.3.2. Get rows and columns matching
      crs(phil_mean_raster_2050_low) <- st_crs(grid_data_northeast)$proj4string
      extent(phil_mean_raster_2050_low) <- extent(grid_data_northeast)
      
    # 11.3.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({phil_values_2050_low <- raster::extract(phil_mean_raster_2050_low, grid_data_northeast_filtered_phil)}) #116s
      grid_data_PM25_phil_2050_low <- grid_data_northeast_filtered_phil
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_phil_2050_low$PM25_AVG <- phil_values_2050_low
      grid_data_PM25_phil_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_phil_2050_low$PM25_AVG))
      
    # 11.3.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_phil_2050_low_NAD <- st_transform(grid_data_PM25_phil_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_phil_2050_low_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_phil_2050_low_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_phil_2050_low_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Philadelphia/PM25_2050_low_Philadelphia.csv"))
      
      st_write(grid_data_PM25_phil_2050_low_NAD, "output_data/shapefiles/1km/Philadelphia/1_km_PM25_2050_low_phil.shp")
      
    # 11.3.5. Prepare figure
      grid_data_PM25_phil_2050_low_NAD_filtered_final %>%
        left_join(phil_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> phil_pop_PM25_total_2050_low
      
      phil_pop_PM25_total_2050_low$product <- phil_pop_PM25_total_2050_low$Values * phil_pop_PM25_total_2050_low$population
      sum_products <- sum(phil_pop_PM25_total_2050_low$product)
      total_population <- sum(phil_pop_PM25_total_2050_low$population)
      phil_mean_2050_low <- sum_products / total_population
      mean(phil_pop_PM25_total_2050_low$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_phil_2050_low_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_phil_2050_low_NAD_filtered_graph
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_phil_2050_low_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_phil_2050_low_NAD_filtered_graph, cbsa_shapefile_phil)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_phil_2050_low_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_low Philadelphia Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(phil_mean_2050_low, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_phil, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -75.16326872597556,  y =  39.95279997140329), shape = 1, color = "#95C623", size = 6, stroke = 2) #Philadelphia city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_phil.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_phil.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
     
#### 12. NEW YORK  ####
  #### 12.1. 2019 ####
    # 12.1.1 Get mean value 
      grid_data_northeast %>%
        filter(pixel_ID %in% NY_pixels) -> grid_data_northeast_filtered_NY
      
    # 12.1.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({NY_values <- raster::extract(phil_ny_mean_raster_2019, grid_data_northeast_filtered_NY)}) #143s
      grid_data_PM25_NY <- grid_data_northeast_filtered_NY
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_NY$PM25_AVG <- NY_values
      grid_data_PM25_NY$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_NY$PM25_AVG))
      
    # 12.1.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_NY_NAD <- st_transform(grid_data_PM25_NY, crs = st_crs("+proj=longlat +datum=NAD83"))

      #Get right columns 
      grid_data_PM25_NY_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_NY_NAD_filtered_final
      
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_NY_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/NY/PM25_2019_NY.csv"))
      
      #Shapefile
      st_write(grid_data_PM25_NY_NAD, "output_data/shapefiles/1km/NY/1_km_PM25_2019_NY.shp")
      
    # 12.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_NY %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> NY_pop_total
      
      grid_data_PM25_NY_NAD_filtered_final %>%
        left_join(NY_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> NY_pop_PM25_total
      
      NY_pop_PM25_total$product <- NY_pop_PM25_total$Values * NY_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(NY_pop_PM25_total$product)
      total_population <- sum(NY_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      NY_mean_BASE_2019 <- sum_products / total_population
      mean(NY_pop_PM25_total$Values)
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_NY_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_NY_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
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
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_NY, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -74.00583906157757,  y =  40.71266458461421), shape = 1, color = "#95C623", size = 6, stroke = 2) #New York city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_NY.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_NY.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)   
      
  #### 12.2. 2050 HIGH ####
    # 12.2.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({NY_values_2050_high <- raster::extract(phil_mean_raster_2050_high, grid_data_northeast_filtered_NY)}) #137s
      grid_data_PM25_NY_2050_high <- grid_data_northeast_filtered_NY
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_NY_2050_high$PM25_AVG <- NY_values_2050_high
      grid_data_PM25_NY_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_NY_2050_high$PM25_AVG))
      
    # 12.2.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_NY_2050_high_NAD <- st_transform(grid_data_PM25_NY_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_NY_2050_high_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_NY_2050_high_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_NY_2050_high_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/NY/PM25_2050_high_NY.csv"))
      
      st_write(grid_data_PM25_NY_2050_high_NAD, "output_data/shapefiles/1km/NY/1_km_PM25_2050_high_NY.shp")
      
    # 12.2.5. Prepare figure
      grid_data_PM25_NY_2050_high_NAD_filtered_final %>%
        left_join(NY_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> NY_pop_PM25_total_2050_high
      
      NY_pop_PM25_total_2050_high$product <- NY_pop_PM25_total_2050_high$Values * NY_pop_PM25_total_2050_high$population
      sum_products <- sum(NY_pop_PM25_total_2050_high$product)
      total_population <- sum(NY_pop_PM25_total_2050_high$population)
      NY_mean_2050_high <- sum_products / total_population
      mean(NY_pop_PM25_total_2050_high$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_NY_2050_high_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_NY_2050_high_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_NY_2050_high_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_NY_2050_high_NAD_filtered_graph, cbsa_shapefile_NY)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_NY_2050_high_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_high NY Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(NY_mean_2050_high, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_NY, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -74.00583906157757,  y =  40.71266458461421), shape = 1, color = "#95C623", size = 6, stroke = 2) #New York city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_NY.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_NY.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)  
      
  #### 12.3. 2050 LOW ####
    # 12.3.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({NY_values_2050_low <- raster::extract(phil_mean_raster_2050_low, grid_data_northeast_filtered_NY)}) #140s
      grid_data_PM25_NY_2050_low <- grid_data_northeast_filtered_NY
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_NY_2050_low$PM25_AVG <- NY_values_2050_low
      grid_data_PM25_NY_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_NY_2050_low$PM25_AVG))
      
    # 12.3.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_NY_2050_low_NAD <- st_transform(grid_data_PM25_NY_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_NY_2050_low_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_NY_2050_low_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_NY_2050_low_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/NY/PM25_2050_low_NY.csv"))
      
      st_write(grid_data_PM25_NY_2050_low_NAD, "output_data/shapefiles/1km/NY/1_km_PM25_2050_low_NY.shp")
      
    # 12.3.5. Prepare figure
      grid_data_PM25_NY_2050_low_NAD_filtered_final %>%
        left_join(NY_pop_total, by = c("Row", "Column"))  %>%
        filter(!is.na(Values))-> NY_pop_PM25_total_2050_low
      
      NY_pop_PM25_total_2050_low$product <- NY_pop_PM25_total_2050_low$Values * NY_pop_PM25_total_2050_low$population
      sum_products <- sum(NY_pop_PM25_total_2050_low$product)
      total_population <- sum(NY_pop_PM25_total_2050_low$population)
      NY_mean_2050_low <- sum_products / total_population
      mean(NY_pop_PM25_total_2050_low$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_NY_2050_low_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_NY_2050_low_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_NY_2050_low_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_NY_2050_low_NAD_filtered_graph, cbsa_shapefile_NY)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_NY_2050_low_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_low NY Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(NY_mean_2050_low, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_NY, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -74.00583906157757,  y =  40.71266458461421), shape = 1, color = "#95C623", size = 6, stroke = 2) #New York city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_NY.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_NY.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)   
      
#### 13. BOSTON  ####
  #### 13.1. 2019 ####
    # 13.1.1 Get mean value 
      # Read the NetCDF file using stack
      boston_nc_1km_stack_2019 <- stack("data/WRF-CMAQ/1km_V2/10_boston/Dailymean.1km.BASE.2019.PM25_AVG.nc", varname = "PM25_AVG")
      # Calculate the mean across all bands (365 days) for each cell
      boston_mean_raster_2019 <- calc(boston_nc_1km_stack_2019, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      # Plot the mean raster
      plot(boston_mean_raster_2019, col = rev(magma(20)), main = "Boston 2019 Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 13.1.2. Get rows and columns matching
      #1. make sure geometries align
      bbox <- st_bbox(grid_data_boston$geometry)
      #Calculate the number of columns and rows 
      num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
      num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
      # 1. Check the CRS
      st_crs(grid_data_boston)
      crs(boston_mean_raster_2019)
      #2. Print extent information
      print(extent(boston_mean_raster_2019))
      print(st_bbox(grid_data_boston))
      #3. Check resolution
      res(boston_mean_raster_2019)
      # Get the bounding box of the grid_data
      bbox <- st_bbox(grid_data_boston)
      # Calculate the resolution
      resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                      (bbox$ymax - bbox$ymin) / num_rows)
      # Print the resolution
      print(resolution)
      
      # 2. Set the CRS for mean_raster_2019
      crs(boston_mean_raster_2019) <- st_crs(grid_data_boston)$proj4string
      
      # 3. Set the extent of mean_raster_aligned to match grid_data
      extent(boston_mean_raster_2019) <- extent(grid_data_boston)
      
      #4. Filter pixels to keep
      grid_data_boston %>%
        filter(pixel_ID %in% boston_pixels) -> grid_data_boston_filtered
      
    # 13.1.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({boston_values <- raster::extract(boston_mean_raster_2019, grid_data_boston_filtered)}) #72s
      grid_data_PM25_boston <- grid_data_boston_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_boston$PM25_AVG <- boston_values
      grid_data_PM25_boston$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_boston$PM25_AVG))
      
    # 13.1.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_boston_NAD <- st_transform(grid_data_PM25_boston, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      #Get right columns 
      grid_data_PM25_boston_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_boston_NAD_filtered_final
      
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_boston_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Boston/PM25_2019_Boston.csv"))
      
      #Shapefile
      st_write(grid_data_PM25_boston_NAD, "output_data/shapefiles/1km/Boston/1_km_PM25_2019_boston.shp")
      
    # 13.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_boston %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> boston_pop_total
      
      grid_data_PM25_boston_NAD_filtered_final %>%
        left_join(boston_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> boston_pop_PM25_total
      
      boston_pop_PM25_total$product <- boston_pop_PM25_total$Values * boston_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(boston_pop_PM25_total$product)
      total_population <- sum(boston_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      boston_mean_BASE_2019 <- sum_products / total_population
      mean(boston_pop_PM25_total$Values)
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_boston_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_boston_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_boston_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_boston_NAD_filtered_graph, cbsa_shapefile_boston)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_boston_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2019 Boston Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(boston_mean_BASE_2019, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_boston, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -71.05789998431737,  y =  42.360362270351416), shape = 1, color = "#95C623", size = 6, stroke = 2) #Boston city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_boston.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_boston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)   
      
  #### 13.2. 2050 HIGH ####
    # 13.2.1 Get mean value 
      boston_nc_1km_stack_2050_high <- stack("data/WRF-CMAQ/1km_V2/10_boston/Dailymean.1km.HighCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      boston_mean_raster_2050_high <- calc(boston_nc_1km_stack_2050_high, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(boston_mean_raster_2050_high, col = rev(magma(20)), main = "Boston 2050_high Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 13.2.2. Get rows and columns matching
      crs(boston_mean_raster_2050_high) <- st_crs(grid_data_boston)$proj4string
      extent(boston_mean_raster_2050_high) <- extent(grid_data_boston)
      
    # 13.2.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({boston_values_2050_high <- raster::extract(boston_mean_raster_2050_high, grid_data_boston_filtered)}) #72s
      grid_data_PM25_boston_2050_high <- grid_data_boston_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_boston_2050_high$PM25_AVG <- boston_values_2050_high
      grid_data_PM25_boston_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_boston_2050_high$PM25_AVG))
      
    # 13.2.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_boston_2050_high_NAD <- st_transform(grid_data_PM25_boston_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
    
      grid_data_PM25_boston_2050_high_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_boston_2050_high_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_boston_2050_high_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Boston/PM25_2050_high_Boston.csv"))
      
      st_write(grid_data_PM25_boston_2050_high_NAD, "output_data/shapefiles/1km/Boston/1_km_PM25_2050_high_boston.shp")
      
    # 13.2.5. Prepare figure
      grid_data_PM25_boston_2050_high_NAD_filtered_final %>%
        left_join(boston_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> boston_pop_PM25_total_2050_high
      
      boston_pop_PM25_total_2050_high$product <- boston_pop_PM25_total_2050_high$Values * boston_pop_PM25_total_2050_high$population
      sum_products <- sum(boston_pop_PM25_total_2050_high$product)
      total_population <- sum(boston_pop_PM25_total_2050_high$population)
      boston_mean_2050_high <- sum_products / total_population
      mean(boston_pop_PM25_total_2050_high$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_boston_2050_high_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_boston_2050_high_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_boston_2050_high_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_boston_2050_high_NAD_filtered_graph, cbsa_shapefile_boston)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_boston_2050_high_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_high Boston Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(boston_mean_2050_high, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_boston, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -71.05789998431737,  y =  42.360362270351416), shape = 1, color = "#95C623", size = 6, stroke = 2) #Boston city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_boston.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_boston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)  
      
  #### 13.3. 2050 LOW ####
    # 13.3.1 Get mean value 
      boston_nc_1km_stack_2050_low <- stack("data/WRF-CMAQ/1km_V2/10_boston/Dailymean.1km.LowCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      boston_mean_raster_2050_low <- calc(boston_nc_1km_stack_2050_low, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(boston_mean_raster_2050_low, col = rev(magma(20)), main = "Boston 2050_low Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 13.3.2. Get rows and columns matching
      crs(boston_mean_raster_2050_low) <- st_crs(grid_data_boston)$proj4string
      extent(boston_mean_raster_2050_low) <- extent(grid_data_boston)
      
    # 13.3.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({boston_values_2050_low <- raster::extract(boston_mean_raster_2050_low, grid_data_boston_filtered)}) #71s
      grid_data_PM25_boston_2050_low <- grid_data_boston_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_boston_2050_low$PM25_AVG <- boston_values_2050_low
      grid_data_PM25_boston_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_boston_2050_low$PM25_AVG))
      
    # 13.3.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_boston_2050_low_NAD <- st_transform(grid_data_PM25_boston_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_boston_2050_low_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_boston_2050_low_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_boston_2050_low_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Boston/PM25_2050_low_Boston.csv"))
      
      st_write(grid_data_PM25_boston_2050_low_NAD, "output_data/shapefiles/1km/Boston/1_km_PM25_2050_low_boston.shp")
      
    # 13.3.5. Prepare figure
      grid_data_PM25_boston_2050_low_NAD_filtered_final %>%
        left_join(boston_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> boston_pop_PM25_total_2050_low
      
      boston_pop_PM25_total_2050_low$product <- boston_pop_PM25_total_2050_low$Values * boston_pop_PM25_total_2050_low$population
      sum_products <- sum(boston_pop_PM25_total_2050_low$product)
      total_population <- sum(boston_pop_PM25_total_2050_low$population)
      boston_mean_2050_low <- sum_products / total_population
      mean(boston_pop_PM25_total_2050_low$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_boston_2050_low_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_boston_2050_low_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_boston_2050_low_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_boston_2050_low_NAD_filtered_graph, cbsa_shapefile_boston)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_boston_2050_low_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_low Boston Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(boston_mean_2050_low, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_boston, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -71.05789998431737,  y =  42.360362270351416), shape = 1, color = "#95C623", size = 6, stroke = 2) #Boston city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_boston.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_boston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
#### 14. DETROIT  ####
  #### 14.1. 2019 ####
    # 14.1.1 Get mean value 
      # Read the NetCDF file using stack
      detroit_nc_1km_stack_2019 <- stack("data/WRF-CMAQ/1km_V2/11_detroit/Dailymean.1km.BASE.2019.PM25_AVG.nc", varname = "PM25_AVG")
      # Calculate the mean across all bands (365 days) for each cell
      detroit_mean_raster_2019 <- calc(detroit_nc_1km_stack_2019, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      # Plot the mean raster
      plot(detroit_mean_raster_2019, col = rev(magma(20)), main = "detroit 2019 Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 14.1.2. Get rows and columns matching
      #1. make sure geometries align
      bbox <- st_bbox(grid_data_detroit$geometry)
      #Calculate the number of columns and rows 
      num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
      num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
      # 1. Check the CRS
      st_crs(grid_data_detroit)
      crs(detroit_mean_raster_2019)
      #2. Print extent information
      print(extent(detroit_mean_raster_2019))
      print(st_bbox(grid_data_detroit))
      #3. Check resolution
      res(detroit_mean_raster_2019)
      # Get the bounding box of the grid_data
      bbox <- st_bbox(grid_data_detroit)
      # Calculate the resolution
      resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                      (bbox$ymax - bbox$ymin) / num_rows)
      # Print the resolution
      print(resolution)
      
      # 2. Set the CRS for mean_raster_2019
      crs(detroit_mean_raster_2019) <- st_crs(grid_data_detroit)$proj4string
      
      # 3. Set the extent of mean_raster_aligned to match grid_data
      extent(detroit_mean_raster_2019) <- extent(grid_data_detroit)
      
      #4. Filter pixels to keep
      grid_data_detroit %>%
        filter(pixel_ID %in% detroit_pixels) -> grid_data_detroit_filtered
      
    # 14.1.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({detroit_values <- raster::extract(detroit_mean_raster_2019, grid_data_detroit_filtered)}) #74s
      grid_data_PM25_detroit <- grid_data_detroit_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_detroit$PM25_AVG <- detroit_values
      grid_data_PM25_detroit$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_detroit$PM25_AVG))
      
    # 14.1.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_detroit_NAD <- st_transform(grid_data_PM25_detroit, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      #Get right columns 
      grid_data_PM25_detroit_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_detroit_NAD_filtered_final
      
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_detroit_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Detroit/PM25_2019_Detroit.csv"))
      
      #Shapefile
      st_write(grid_data_PM25_detroit_NAD, "output_data/shapefiles/1km/Detroit/1_km_PM25_2019_detroit.shp")
      
    # 14.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_detroit %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> detroit_pop_total
      
      grid_data_PM25_detroit_NAD_filtered_final %>%
        left_join(detroit_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> detroit_pop_PM25_total
      
      detroit_pop_PM25_total$product <- detroit_pop_PM25_total$Values * detroit_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(detroit_pop_PM25_total$product)
      total_population <- sum(detroit_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      detroit_mean_BASE_2019 <- sum_products / total_population
      mean(detroit_pop_PM25_total$Values)
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_detroit_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_detroit_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_detroit_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_detroit_NAD_filtered_graph, cbsa_shapefile_detroit)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_detroit_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2019 Detroit Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(detroit_mean_BASE_2019, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_detroit, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -83.04362102880836,  y =  42.32974049721709), shape = 1, color = "#95C623", size = 6, stroke = 2) #Detroit city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_detroit.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_detroit.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)   
      
  #### 14.2. 2050 HIGH ####
    # 14.2.1 Get mean value 
      detroit_nc_1km_stack_2050_high <- stack("data/WRF-CMAQ/1km_V2/11_detroit/Dailymean.1km.HighCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      detroit_mean_raster_2050_high <- calc(detroit_nc_1km_stack_2050_high, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(detroit_mean_raster_2050_high, col = rev(magma(20)), main = "Detroit 2050_high Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 14.2.2. Get rows and columns matching
      crs(detroit_mean_raster_2050_high) <- st_crs(grid_data_detroit)$proj4string
      extent(detroit_mean_raster_2050_high) <- extent(grid_data_detroit)
      
    # 14.2.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({detroit_values_2050_high <- raster::extract(detroit_mean_raster_2050_high, grid_data_detroit_filtered)}) #73s
      grid_data_PM25_detroit_2050_high <- grid_data_detroit_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_detroit_2050_high$PM25_AVG <- detroit_values_2050_high
      grid_data_PM25_detroit_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_detroit_2050_high$PM25_AVG))
      
    # 14.2.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_detroit_2050_high_NAD <- st_transform(grid_data_PM25_detroit_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_detroit_2050_high_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_detroit_2050_high_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_detroit_2050_high_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Detroit/PM25_2050_high_Detroit.csv"))
      
      st_write(grid_data_PM25_detroit_2050_high_NAD, "output_data/shapefiles/1km/Detroit/1_km_PM25_2050_high_detroit.shp")
      
    # 14.2.5. Prepare figure
      grid_data_PM25_detroit_2050_high_NAD_filtered_final %>%
        left_join(detroit_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> detroit_pop_PM25_total_2050_high
      
      detroit_pop_PM25_total_2050_high$product <- detroit_pop_PM25_total_2050_high$Values * detroit_pop_PM25_total_2050_high$population
      sum_products <- sum(detroit_pop_PM25_total_2050_high$product)
      total_population <- sum(detroit_pop_PM25_total_2050_high$population)
      detroit_mean_2050_high <- sum_products / total_population
      mean(detroit_pop_PM25_total_2050_high$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_detroit_2050_high_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_detroit_2050_high_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_detroit_2050_high_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_detroit_2050_high_NAD_filtered_graph, cbsa_shapefile_detroit)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_detroit_2050_high_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_high Detroit Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(detroit_mean_2050_high, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_detroit, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -83.04362102880836,  y =  42.32974049721709), shape = 1, color = "#95C623", size = 6, stroke = 2) #Detroit city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_detroit.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_detroit.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)  
      
  #### 14.3. 2050 LOW ####
    # 14.3.1 Get mean value 
      detroit_nc_1km_stack_2050_low <- stack("data/WRF-CMAQ/1km_V2/11_detroit/Dailymean.1km.LowCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      detroit_mean_raster_2050_low <- calc(detroit_nc_1km_stack_2050_low, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(detroit_mean_raster_2050_low, col = rev(magma(20)), main = "Detroit 2050_low Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 14.3.2. Get rows and columns matching
      crs(detroit_mean_raster_2050_low) <- st_crs(grid_data_detroit)$proj4string
      extent(detroit_mean_raster_2050_low) <- extent(grid_data_detroit)
      
    # 14.3.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({detroit_values_2050_low <- raster::extract(detroit_mean_raster_2050_low, grid_data_detroit_filtered)}) #78s
      grid_data_PM25_detroit_2050_low <- grid_data_detroit_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_detroit_2050_low$PM25_AVG <- detroit_values_2050_low
      grid_data_PM25_detroit_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_detroit_2050_low$PM25_AVG))
      
    # 14.3.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_detroit_2050_low_NAD <- st_transform(grid_data_PM25_detroit_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_detroit_2050_low_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_detroit_2050_low_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_detroit_2050_low_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Detroit/PM25_2050_low_Detroit.csv"))
      
      st_write(grid_data_PM25_detroit_2050_low_NAD, "output_data/shapefiles/1km/Detroit/1_km_PM25_2050_low_detroit.shp")
      
    # 14.3.5. Prepare figure
      grid_data_PM25_detroit_2050_low_NAD_filtered_final %>%
        left_join(detroit_pop_total, by = c("Row", "Column")) %>%
        filter(!is.na(Values))-> detroit_pop_PM25_total_2050_low
      
      detroit_pop_PM25_total_2050_low$product <- detroit_pop_PM25_total_2050_low$Values * detroit_pop_PM25_total_2050_low$population
      sum_products <- sum(detroit_pop_PM25_total_2050_low$product)
      total_population <- sum(detroit_pop_PM25_total_2050_low$population)
      detroit_mean_2050_low <- sum_products / total_population
      mean(detroit_pop_PM25_total_2050_low$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_detroit_2050_low_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_detroit_2050_low_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_detroit_2050_low_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_detroit_2050_low_NAD_filtered_graph, cbsa_shapefile_detroit)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_detroit_2050_low_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_low Detroit Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(detroit_mean_2050_low, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_detroit, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -83.04362102880836,  y =  42.32974049721709), shape = 1, color = "#95C623", size = 6, stroke = 2) #Detroit city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_detroit.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_detroit.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)     
  
#### 15. SAN FRANCISCO  ####
  #### 15.1. 2019 ####
    # 15.1.1 Get mean value 
      # Read the NetCDF file using stack
      SF_nc_1km_stack_2019 <- stack("data/WRF-CMAQ/1km_V2/12_sanfrancisco/Dailymean.1km.BASE.2019.PM25_AVG.nc", varname = "PM25_AVG")
      # Calculate the mean across all bands (365 days) for each cell
      SF_mean_raster_2019 <- calc(SF_nc_1km_stack_2019, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      # Plot the mean raster
      plot(SF_mean_raster_2019, col = rev(magma(20)), main = "SF 2019 Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 15.1.2. Get rows and columns matching
      #1. make sure geometries align
      bbox <- st_bbox(grid_data_SF$geometry)
      #Calculate the number of columns and rows 
      num_cols <- ceiling((bbox["xmax"] - bbox["xmin"]) / 1000)
      num_rows <- ceiling((bbox["ymax"] - bbox["ymin"]) / 1000)
      # 1. Check the CRS
      st_crs(grid_data_SF)
      crs(SF_mean_raster_2019)
      #2. Print extent information
      print(extent(SF_mean_raster_2019))
      print(st_bbox(grid_data_SF))
      #3. Check resolution
      res(SF_mean_raster_2019)
      # Get the bounding box of the grid_data
      bbox <- st_bbox(grid_data_SF)
      # Calculate the resolution
      resolution <- c((bbox$xmax - bbox$xmin) / num_cols,
                      (bbox$ymax - bbox$ymin) / num_rows)
      # Print the resolution
      print(resolution)
      
      # 2. Set the CRS for mean_raster_2019
      crs(SF_mean_raster_2019) <- st_crs(grid_data_SF)$proj4string
      
      # 3. Set the extent of mean_raster_aligned to match grid_data
      extent(SF_mean_raster_2019) <- extent(grid_data_SF)
      
      #4. Filter pixels to keep
      grid_data_SF %>%
        filter(pixel_ID %in% SF_pixels) -> grid_data_SF_filtered
      
    # 15.1.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({SF_values <- raster::extract(SF_mean_raster_2019, grid_data_SF_filtered)}) #48s
      grid_data_PM25_SF <- grid_data_SF_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_SF$PM25_AVG <- SF_values
      grid_data_PM25_SF$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_SF$PM25_AVG))
      
    # 15.1.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_SF_NAD <- st_transform(grid_data_PM25_SF, crs = st_crs("+proj=longlat +datum=NAD83"))
      grid_data_PM25_SF_NAD %>%
        filter(!is.na(PM25_AVG)) -> grid_data_PM25_SF_NAD
      
      #Get right columns 
      grid_data_PM25_SF_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_SF_NAD_filtered_final
      
      # 2. Save file
      #Csv
      write.csv(grid_data_PM25_SF_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/SF/PM25_2019_SF.csv"))
      st_write(grid_data_PM25_SF_NAD, "output_data/shapefiles/1km/SF/1_km_PM25_2019_SF.shp")
      
    # 15.1.5. Prepare figure
      #Get national mean (population weighted)
      population_2019_data_pixel_age_final_SF %>%
        group_by(Row, Column) %>%
        summarise(population = sum(Population)) %>%
        ungroup() -> SF_pop_total
      
      grid_data_PM25_SF_NAD_filtered_final %>%
        left_join(SF_pop_total, by = c("Row", "Column")) %>%
        mutate(population = if_else(is.na(population), 0, population))-> SF_pop_PM25_total
      
      SF_pop_PM25_total$product <- SF_pop_PM25_total$Values * SF_pop_PM25_total$population
      # Sum up products and total population
      sum_products <- sum(SF_pop_PM25_total$product)
      total_population <- sum(SF_pop_PM25_total$population)
      # Calculate population-weighted average pollution
      SF_mean_BASE_2019 <- sum_products / total_population
      mean(SF_pop_PM25_total$Values)
      
      #Set values > 9 to 9 for graphing purposes
      #Based on the EPA -> https://www.epa.gov/pm-pollution/national-ambient-air-quality-standards-naaqs-pm
      grid_data_PM25_SF_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_SF_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
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
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_SF, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -122.41894505158999,  y =  37.779200252025916), shape = 1, color = "#95C623", size = 6, stroke = 2) #San Francisco city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_SF.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2019_plot_SF.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)   
      
  #### 15.2. 2050 HIGH ####
    # 15.2.1 Get mean value 
      SF_nc_1km_stack_2050_high <- stack("data/WRF-CMAQ/1km_V2/12_sanfrancisco/Dailymean.1km.HighCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      SF_mean_raster_2050_high <- calc(SF_nc_1km_stack_2050_high, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(SF_mean_raster_2050_high, col = rev(magma(20)), main = "SF 2050_high Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 15.2.2. Get rows and columns matching
      crs(SF_mean_raster_2050_high) <- st_crs(grid_data_SF)$proj4string
      extent(SF_mean_raster_2050_high) <- extent(grid_data_SF)
      
    # 15.2.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({SF_values_2050_high <- raster::extract(SF_mean_raster_2050_high, grid_data_SF_filtered)}) #56s
      grid_data_PM25_SF_2050_high <- grid_data_SF_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_SF_2050_high$PM25_AVG <- SF_values_2050_high
      grid_data_PM25_SF_2050_high$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_SF_2050_high$PM25_AVG))
      
    # 15.2.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_SF_2050_high_NAD <- st_transform(grid_data_PM25_SF_2050_high, crs = st_crs("+proj=longlat +datum=NAD83"))
      
      grid_data_PM25_SF_2050_high_NAD %>%
        filter(!is.na(PM25_AVG)) -> grid_data_PM25_SF_2050_high_NAD
      
      grid_data_PM25_SF_2050_high_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_SF_2050_high_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_SF_2050_high_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/SF/PM25_2050_high_SF.csv"))
      
      st_write(grid_data_PM25_SF_2050_high_NAD, "output_data/shapefiles/1km/SF/1_km_PM25_2050_high_SF.shp")
      
     # 15.2.5. Prepare figure
      grid_data_PM25_SF_2050_high_NAD_filtered_final %>%
        left_join(SF_pop_total, by = c("Row", "Column"))%>%
        mutate(population = if_else(is.na(population), 0, population))-> SF_pop_PM25_total_2050_high
      
      SF_pop_PM25_total_2050_high$product <- SF_pop_PM25_total_2050_high$Values * SF_pop_PM25_total_2050_high$population
      sum_products <- sum(SF_pop_PM25_total_2050_high$product)
      total_population <- sum(SF_pop_PM25_total_2050_high$population)
      SF_mean_2050_high <- sum_products / total_population
      mean(SF_pop_PM25_total_2050_high$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_SF_2050_high_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_SF_2050_high_NAD_filtered_graph
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_SF_2050_high_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_SF_2050_high_NAD_filtered_graph, cbsa_shapefile_SF)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_SF_2050_high_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_high SF Mean 1km Yearly PM2.5 (ug/m3)") +
        # geom_text(aes(label = paste("Population weighted mean:", round(SF_mean_2050_high, 2), "ug/m3")),
        #           x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5)+
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_SF, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -122.41894505158999,  y =  37.779200252025916), shape = 1, color = "#95C623", size = 6, stroke = 2) #San Francisco city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_SF.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_high_plot_SF.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)  
      
  #### 15.3. 2050 LOW ####
    # 15.3.1 Get mean value 
      SF_nc_1km_stack_2050_low <- stack("data/WRF-CMAQ/1km_V2/12_sanfrancisco/Dailymean.1km.LowCCS.2050.PM25_AVG.nc", varname = "PM25_AVG")
      SF_mean_raster_2050_low <- calc(SF_nc_1km_stack_2050_low, fun = mean, na.rm = TRUE, na.rm.args = list(na.rm = TRUE))
      plot(SF_mean_raster_2050_low, col = rev(magma(20)), main = "SF 2050_low Yearly Mean PM2.5 Conc. (ug/m3)")
      
    # 15.3.2. Get rows and columns matching
      crs(SF_mean_raster_2050_low) <- st_crs(grid_data_SF)$proj4string
      extent(SF_mean_raster_2050_low) <- extent(grid_data_SF)
      
    # 15.3.3. Get values from raster and assign them to my grid 
      # 1. Extract values using st_extract
      system.time({SF_values_2050_low <- raster::extract(SF_mean_raster_2050_low, grid_data_SF_filtered)}) #51s
      grid_data_PM25_SF_2050_low <- grid_data_SF_filtered
      
      # 2. Assign the values to the PM25_AVG column
      grid_data_PM25_SF_2050_low$PM25_AVG <- SF_values_2050_low
      grid_data_PM25_SF_2050_low$PM25_AVG <- as.numeric(gsub("[^0-9.-]", "", grid_data_PM25_SF_2050_low$PM25_AVG))
      
    # 15.3.4. Prepare output table
      # 1. Convert to NAD83
      grid_data_PM25_SF_2050_low_NAD <- st_transform(grid_data_PM25_SF_2050_low, crs = st_crs("+proj=longlat +datum=NAD83"))
      grid_data_PM25_SF_2050_low_NAD %>%
        filter(!is.na(PM25_AVG)) -> grid_data_PM25_SF_2050_low_NAD
      
      grid_data_PM25_SF_2050_low_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(-pixel_ID) %>%
        rename(Column = col,
               Row = row,
               Values = PM25_AVG) %>%
        mutate(Metric = "D24HourMean",
               `Seasonal Metric` = "QuarterlyMean",
               `Annual Metric` = "Mean") -> grid_data_PM25_SF_2050_low_NAD_filtered_final
      
      # 2. Save file
      write.csv(grid_data_PM25_SF_2050_low_NAD_filtered_final, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/SF/PM25_2050_low_SF.csv"))
      
      st_write(grid_data_PM25_SF_2050_low_NAD, "output_data/shapefiles/1km/SF/1_km_PM25_2050_low_SF.shp")
      
    # 15.3.5. Prepare figure
      grid_data_PM25_SF_2050_low_NAD_filtered_final %>%
        left_join(SF_pop_total, by = c("Row", "Column")) %>%
        mutate(population = if_else(is.na(population), 0, population))-> SF_pop_PM25_total_2050_low
      
      SF_pop_PM25_total_2050_low$product <- SF_pop_PM25_total_2050_low$Values * SF_pop_PM25_total_2050_low$population
      sum_products <- sum(SF_pop_PM25_total_2050_low$product)
      total_population <- sum(SF_pop_PM25_total_2050_low$population)
      SF_mean_2050_low <- sum_products / total_population
      mean(SF_pop_PM25_total_2050_low$Values)
      
      #Set values > 9 to 9 for graphing purposes
      grid_data_PM25_SF_2050_low_NAD %>%
        mutate(PM25_AVG_graph = if_else(PM25_AVG >= 9, 9, PM25_AVG)) -> grid_data_PM25_SF_2050_low_NAD_filtered_graph
      
      #Perform intersection to retain only pixels within the city boundary
      grid_data_PM25_SF_2050_low_NAD_filtered_graph_crop <- st_intersection(grid_data_PM25_SF_2050_low_NAD_filtered_graph, cbsa_shapefile_SF)
      
      p <- ggplot() +
        geom_sf(data = grid_data_PM25_SF_2050_low_NAD_filtered_graph_crop, aes(fill = PM25_AVG_graph), color = NA, size = 0) +
        scale_fill_gradientn(colors = colors,         
                             values = scales::rescale(c(0, 5, 10)),  # Set the midpoint to 5
                             limits = c(0, 10),  # Adjust limits to match your data range
                             name = "PM2.5 (ug/m3)"       ) +
        ggtitle("2050_low SF Mean 1km Yearly PM2.5 (ug/m3)") +
        geom_text(aes(label = paste("Population weighted mean:", round(SF_mean_2050_low, 2), "ug/m3")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -0.6, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_SF, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -122.41894505158999,  y =  37.779200252025916), shape = 1, color = "#95C623", size = 6, stroke = 2) #San Francisco city hall
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_SF.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #gsave("figures/BenMAP/1km/pollution/1km_PM25_2050_low_plot_SF.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)     
      
#------------------------------------------------- SECTION A5: FIX FILES ---------------------------------------------------------------------------
#There are some pixels with NA in air pollution. This happens in pixels that are on the coast and have very little overlap with the land. This is because CMAQ results were cropped for post-processing, and some were lost in the process.
#The cities affected are:
      #LA (1 pixel, 0 people), Houston (41 pixels, 23 people), Miami (32 pixels, 27 people),
      #Chicago (1 pixel, 0 people), DC (1 pixel, 0 people), Philadelphia (3 pixels, 88 people)
      #New York (7 pixels, 56 people), Boston (65 pixels, 1866 people), Detroit (4 pixels, 72 people),
      #San Francisco (11 pixels, 102 people)
  #NOTE: this is never more than 0.5% of the pixels, and 0.04% of the people
  #Because of this we filter these pixels out

##### 1. LOS ANGELES ####
      final_pixels_LA <- grid_data_PM25_southwest_LA_NAD$pixel_ID
      grid_data_PM25_southwest_LA_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(row, col) %>%
        unite(row_col, c(row, col), sep = "_") -> row_col_LA
      #Has total pixels 13,120 we use as grid in BenMAP.
      
    #GRID
      grid_data_south_west_NAD83_LA %>%
        filter(pixel_ID %in% final_pixels_LA) -> grid_data_south_west_NAD83_LA_fix
      st_write(grid_data_south_west_NAD83_LA_fix, "output_data/shapefiles/1km/LA/1km_shapefile_2019_03252024_LA_fix.shp") 
      
    #POPULATION 
      population_2019_data_pixel_age_final_LA %>%
        unite(row_col, c(Row, Column), sep = "_", remove = FALSE)%>%
        right_join(row_col_LA, by = "row_col") %>%
        dplyr::select(-row_col)-> population_2019_data_pixel_age_final_LA_fix
      write.csv(population_2019_data_pixel_age_final_LA_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/LA/1_km_population_2019_LA_fixed.csv")) 
      
    #MORTALITY
      partitions_rate_final_LA %>%
        filter(pixel_ID %in% final_pixels_LA) %>%
        dplyr::select(-pixel_ID) %>%
        rename(Row = row, Column = col, Value = weighted_incidence) %>%
        st_drop_geometry()-> partitions_rate_final_clean_LA_fix
      write.csv(partitions_rate_final_clean_LA_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/LA/Incidence_1km_2019_LA_fixed.csv"))
      
##### 6. HOUSTON ####
      final_pixels_houston <- grid_data_PM25_houston_NAD$pixel_ID
      grid_data_PM25_houston_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(row, col) %>%
        unite(row_col, c(row, col), sep = "_") -> row_col_houston
      #Has total pixels 23,299 we use as grid in BenMAP.
      
    #GRID
      grid_data_houston_NAD83 %>%
        filter(pixel_ID %in% final_pixels_houston) -> grid_data_houston_NAD83_fix
      st_write(grid_data_houston_NAD83_fix, "output_data/shapefiles/1km/Houston/1km_shapefile_2019_04082024_houston_fix.shp") 
    #POPULATION 
      population_2019_data_pixel_age_final_houston %>%
        unite(row_col, c(Row, Column), sep = "_", remove = FALSE)%>%
        right_join(row_col_houston, by = "row_col") %>%
        dplyr::select(-row_col)-> population_2019_data_pixel_age_final_houston_fix
      write.csv(population_2019_data_pixel_age_final_houston_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Houston/1_km_population_2019_houston_fixed.csv")) 
      
    #MORTALITY
      partitions_rate_final_houston %>%
        filter(pixel_ID %in% final_pixels_houston) %>%
        dplyr::select(-pixel_ID) %>%
        rename(Row = row, Column = col, Value = weighted_incidence) %>%
        st_drop_geometry()-> partitions_rate_final_clean_houston_fix
      write.csv(partitions_rate_final_clean_houston_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Houston/Incidence_1km_2019_houston_fixed.csv"))
    
##### 7. MIAMI #####
      final_pixels_miami <- grid_data_PM25_miami_NAD$pixel_ID
      grid_data_PM25_miami_NAD %>%
        st_drop_geometry() %>%
        dplyr::select(row, col) %>%
        unite(row_col, c(row, col), sep = "_") -> row_col_miami
      #Has total pixels 15,094 we use as grid in BenMAP.
      
    #GRID
      grid_data_miami_NAD83 %>%
        filter(pixel_ID %in% final_pixels_miami) -> grid_data_miami_NAD83_fix
      st_write(grid_data_miami_NAD83_fix, "output_data/shapefiles/1km/Miami/1km_shapefile_2019_04082024_miami_fix.shp") 
    
    #POPULATION 
      population_2019_data_pixel_age_final_miami %>%
        unite(row_col, c(Row, Column), sep = "_", remove = FALSE)%>%
        right_join(row_col_miami, by = "row_col") %>%
        dplyr::select(-row_col)-> population_2019_data_pixel_age_final_miami_fix
      write.csv(population_2019_data_pixel_age_final_miami_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Miami/1_km_population_2019_miami_fixed.csv")) 
      
    #MORTALITY
      partitions_rate_final_miami %>%
        filter(pixel_ID %in% final_pixels_miami) %>%
        dplyr::select(-pixel_ID) %>%
        rename(Row = row, Column = col, Value = weighted_incidence) %>%
        st_drop_geometry()-> partitions_rate_final_clean_miami_fix
      write.csv(partitions_rate_final_clean_miami_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Miami/Incidence_1km_2019_miami_fixed.csv"))
      
##### 9. CHICAGO #####
      final_pixels_chicago <- grid_data_PM25_chicago_NAD$pixel_ID
      grid_data_PM25_chicago_NAD %>% 
        st_drop_geometry() %>%
        dplyr::select(row, col) %>%
        unite(row_col, c(row, col), sep = "_") -> row_col_chicago
      #Has total pixels 19,208 we use as grid in BenMAP.
      
    #GRID
      grid_data_chicago_NAD83 %>%
        filter(pixel_ID %in% final_pixels_chicago) -> grid_data_chicago_NAD83_fix
      st_write(grid_data_chicago_NAD83_fix, "output_data/shapefiles/1km/Chicago/1km_shapefile_2019_04082024_chicago_fix.shp") 
      
    #POPULATION 
      population_2019_data_pixel_age_final_chicago %>%
        unite(row_col, c(Row, Column), sep = "_", remove = FALSE)%>%
        right_join(row_col_chicago, by = "row_col")%>%
        dplyr::select(-row_col)-> population_2019_data_pixel_age_final_chicago_fix
      write.csv(population_2019_data_pixel_age_final_chicago_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Chicago/1_km_population_2019_chicago_fixed.csv")) 
      
    #MORTALITY
      partitions_rate_final_chicago %>%
        filter(pixel_ID %in% final_pixels_chicago) %>%
        dplyr::select(-pixel_ID) %>%
        rename(Row = row, Column = col, Value = weighted_incidence) %>%
        st_drop_geometry()-> partitions_rate_final_clean_chicago_fix
      write.csv(partitions_rate_final_clean_chicago_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Chicago/Incidence_1km_2019_chicago_fixed.csv"))
  
##### 10. WASHINGTON DC #####
      final_pixels_wash <- grid_data_PM25_wash_NAD$pixel_ID
      grid_data_PM25_wash_NAD %>% 
        st_drop_geometry() %>%
        dplyr::select(row, col) %>%
        unite(row_col, c(row, col), sep = "_") -> row_col_wash
      #Has total pixels 17,876 we use as grid in BenMAP.
      
    #GRID
      grid_data_wash_NAD83 %>%
        filter(pixel_ID %in% final_pixels_wash) -> grid_data_wash_NAD83_fix
      st_write(grid_data_wash_NAD83_fix, "output_data/shapefiles/1km/DC/1km_shapefile_2019_04082024_wash_fix.shp") 
      
    #POPULATION 
      population_2019_data_pixel_age_final_wash %>%
        unite(row_col, c(Row, Column), sep = "_", remove = FALSE)%>%
        right_join(row_col_wash, by = "row_col")%>%
        dplyr::select(-row_col)-> population_2019_data_pixel_age_final_wash_fix
      write.csv(population_2019_data_pixel_age_final_wash_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/DC/1_km_population_2019_wash_fixed.csv")) 
      
    #MORTALITY
      partitions_rate_final_wash %>%
        filter(pixel_ID %in% final_pixels_wash) %>%
        dplyr::select(-pixel_ID) %>%
        rename(Row = row, Column = col, Value = weighted_incidence) %>%
        st_drop_geometry()-> partitions_rate_final_clean_wash_fix
      write.csv(partitions_rate_final_clean_wash_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/DC/Incidence_1km_2019_wash_fixed.csv"))
    
##### 11. PHILADELPHIA #####
      final_pixels_phil <- grid_data_PM25_phil_NAD$pixel_ID
      grid_data_PM25_phil_NAD %>% 
        st_drop_geometry() %>%
        dplyr::select(row, col) %>%
        unite(row_col, c(row, col), sep = "_") -> row_col_phil
      #Has total pixels 12,649 we use as grid in BenMAP.
      
    #GRID
      grid_data_northeast_NAD83 %>%
        filter(pixel_ID %in% final_pixels_phil) -> grid_data_phil_NAD83_fix
      st_write(grid_data_phil_NAD83_fix, "output_data/shapefiles/1km/Philadelphia/1km_shapefile_2019_04082024_phil_fix.shp") 
      
    #POPULATION 
      population_2019_data_pixel_age_final_phil %>%
        unite(row_col, c(Row, Column), sep = "_", remove = FALSE)%>%
        right_join(row_col_phil, by = "row_col")%>%
        dplyr::select(-row_col)-> population_2019_data_pixel_age_final_phil_fix
      write.csv(population_2019_data_pixel_age_final_phil_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Philadelphia/1_km_population_2019_phil_fixed.csv")) 
      
    #MORTALITY
      partitions_rate_final_phil %>%
        filter(pixel_ID %in% final_pixels_phil) %>%
        dplyr::select(-pixel_ID) %>%
        rename(Row = row, Column = col, Value = weighted_incidence) %>%
        st_drop_geometry()-> partitions_rate_final_clean_phil_fix
      write.csv(partitions_rate_final_clean_phil_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Philadelphia/Incidence_1km_2019_phil_fixed.csv"))
    
##### 12. NEW YORK #####
      final_pixels_NY <- grid_data_PM25_NY_NAD$pixel_ID
      grid_data_PM25_NY_NAD %>% 
        st_drop_geometry() %>%
        dplyr::select(row, col) %>%
        unite(row_col, c(row, col), sep = "_") -> row_col_NY
      #Has total pixels 12,649 we use as grid in BenMAP.
      
    #GRID
      grid_data_northeast_NAD83 %>%
        filter(pixel_ID %in% final_pixels_NY) -> grid_data_NY_NAD83_fix
      st_write(grid_data_NY_NAD83_fix, "output_data/shapefiles/1km/NY/1km_shapefile_2019_04082024_NY_fix.shp") 
      
    #POPULATION 
      population_2019_data_pixel_age_final_NY %>%
        unite(row_col, c(Row, Column), sep = "_", remove = FALSE)%>%
        right_join(row_col_NY, by = "row_col")%>%
        dplyr::select(-row_col)-> population_2019_data_pixel_age_final_NY_fix
      write.csv(population_2019_data_pixel_age_final_NY_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/NY/1_km_population_2019_NY_fixed.csv")) 
      
    #MORTALITY
      partitions_rate_final_NY %>%
        filter(pixel_ID %in% final_pixels_NY) %>%
        dplyr::select(-pixel_ID) %>%
        rename(Row = row, Column = col, Value = weighted_incidence) %>%
        st_drop_geometry()-> partitions_rate_final_clean_NY_fix
      write.csv(partitions_rate_final_clean_NY_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/NY/Incidence_1km_2019_NY_fixed.csv"))
    
##### 13. BOSTON #####
      final_pixels_boston <- grid_data_PM25_boston_NAD$pixel_ID
      grid_data_PM25_boston_NAD %>% 
        st_drop_geometry() %>%
        dplyr::select(row, col) %>%
        unite(row_col, c(row, col), sep = "_") -> row_col_boston
      #Has total pixels 10,022 we use as grid in BenMAP.
      
    #GRID
      grid_data_boston_NAD83 %>%
        filter(pixel_ID %in% final_pixels_boston) -> grid_data_boston_NAD83_fix
      st_write(grid_data_boston_NAD83_fix, "output_data/shapefiles/1km/Boston/1km_shapefile_2019_04082024_boston_fix.shp") 
    #POPULATION 
      population_2019_data_pixel_age_final_boston %>%
        unite(row_col, c(Row, Column), sep = "_", remove = FALSE)%>%
        right_join(row_col_boston, by = "row_col") %>%
        dplyr::select(-row_col)-> population_2019_data_pixel_age_final_boston_fix
      write.csv(population_2019_data_pixel_age_final_boston_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Boston/1_km_population_2019_boston_fixed.csv")) 
      
    #MORTALITY
      partitions_rate_final_boston %>%
        filter(pixel_ID %in% final_pixels_boston) %>%
        dplyr::select(-pixel_ID) %>%
        rename(Row = row, Column = col, Value = weighted_incidence) %>%
        st_drop_geometry()-> partitions_rate_final_clean_boston_fix
      write.csv(partitions_rate_final_clean_boston_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Boston/Incidence_1km_2019_boston_fixed.csv"))
      
    
##### 14. DETROIT #####
      final_pixels_detroit <- grid_data_PM25_detroit_NAD$pixel_ID
      grid_data_PM25_detroit_NAD %>% 
        st_drop_geometry() %>%
        dplyr::select(row, col) %>%
        unite(row_col, c(row, col), sep = "_") -> row_col_detroit
      #Has total pixels 10,707 we use as grid in BenMAP.
      
    #GRID
      grid_data_detroit_NAD83 %>%
        filter(pixel_ID %in% final_pixels_detroit) -> grid_data_detroit_NAD83_fix
      st_write(grid_data_detroit_NAD83_fix, "output_data/shapefiles/1km/Detroit/1km_shapefile_2019_04082024_detroit_fix.shp") 
      
    #POPULATION 
      population_2019_data_pixel_age_final_detroit %>%
        unite(row_col, c(Row, Column), sep = "_", remove = FALSE)%>%
        right_join(row_col_detroit, by = "row_col") %>%
        dplyr::select(-row_col)-> population_2019_data_pixel_age_final_detroit_fix
      write.csv(population_2019_data_pixel_age_final_detroit_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Detroit/1_km_population_2019_detroit_fixed.csv")) 
      
    #MORTALITY
      partitions_rate_final_detroit %>%
        filter(pixel_ID %in% final_pixels_detroit) %>%
        dplyr::select(-pixel_ID) %>%
        rename(Row = row, Column = col, Value = weighted_incidence) %>%
        st_drop_geometry()-> partitions_rate_final_clean_detroit_fix
      write.csv(partitions_rate_final_clean_detroit_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/Detroit/Incidence_1km_2019_detroit_fixed.csv"))
  
##### 15. SAN FRANCISCO #####
      final_pixels_SF <- grid_data_PM25_SF_NAD$pixel_ID
      grid_data_PM25_SF_NAD %>% 
        st_drop_geometry() %>%
        dplyr::select(row, col) %>%
        unite(row_col, c(row, col), sep = "_") -> row_col_SF
      #Has total pixels 7,061 we use as grid in BenMAP.
      
    #GRID
      grid_data_SF_NAD83 %>%
        filter(pixel_ID %in% final_pixels_SF) -> grid_data_SF_NAD83_fix
      st_write(grid_data_SF_NAD83_fix, "output_data/shapefiles/1km/SF/1km_shapefile_2019_04082024_SF_fix.shp") 
    
    #POPULATION 
      population_2019_data_pixel_age_final_SF %>%
        unite(row_col, c(Row, Column), sep = "_", remove = FALSE)%>%
        right_join(row_col_SF, by = "row_col") %>%
        dplyr::select(-row_col)-> population_2019_data_pixel_age_final_SF_fix
      write.csv(population_2019_data_pixel_age_final_SF_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/SF/1_km_population_2019_SF_fixed.csv")) 
      
    #MORTALITY
      partitions_rate_final_SF %>%
        filter(pixel_ID %in% final_pixels_SF) %>%
        dplyr::select(-pixel_ID) %>%
        rename(Row = row, Column = col, Value = weighted_incidence) %>%
        st_drop_geometry()-> partitions_rate_final_clean_SF_fix
      write.csv(partitions_rate_final_clean_SF_fix, row.names = FALSE, file = paste("output_data/BenMAP_files/1km/SF/Incidence_1km_2019_detroit_fixed.csv"))

      
      
#------------------------------------------------- B. BENMAP OUTPUTS ---------------------------------------------------------------------------------
#------------------------------------------------- SECTION B1: 1 KM MORTALITY BENMAP RESULTS -------------------------------------------------
#### 1. SEATTLE ####
  #### 1.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_Seattle <- st_read("BenMAP_results/1km_V2/Seattle/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_Seattle, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 Seattle Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Seattle.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Seattle.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 1.2. Mortality results ####
    #Here we load BenMAP shapefiles with mortality results to create plots 
      benmap_2050_Seattle <- st_read("BenMAP_results/1km_V2/Seattle/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") 
      #Set 811 pixels with no population to 0
      PM_difference_2050_Seattle %>%
        dplyr::select(ROW, COL, geometry) %>%
        left_join(benmap_2050_Seattle %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
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
      total_population_Seattle <- sum(population_2019_data_pixel_age_final_seattle$Population)

      ### Deaths per 1000
      p <- ggplot() +
        geom_sf(data = benmap_2050_Seattle_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Seattle Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_Seattle),
                                    "(",round((total_deaths_Seattle*10^6)/total_population_Seattle), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale()+
        geom_sf(data = cbsa_shapefile_seattle, color = "black", fill = NA)+
        theme_void() +
        #theme(plot.background = element_rect(fill = "white")) +
        geom_point(data = NULL, aes(x = -122.32987465320657, y = 47.60386428408079), shape = 1, color = "purple", size = 6, stroke = 2) #Seattle city hall+
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Seattle.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Seattle.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
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
        #theme(plot.background = element_rect(fill = "white")) +
        geom_point(data = NULL, aes(x = -122.32987465320657, y = 47.60386428408079), shape = 1, color = "purple", size = 6, stroke = 2) #Seattle city hall+
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Seattle_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Seattle_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_Seattle <- summary(benmap_2050_Seattle_pop_weighted_cropped$Point.Estim)
      mean_value <- mean(benmap_2050_Seattle_pop_weighted_cropped$Point.Estim)
      median_value <- median(benmap_2050_Seattle_pop_weighted_cropped$Point.Estim)
      sd_value <- sd(benmap_2050_Seattle_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_Seattle_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value, label = paste("Mean:", round(mean_value, 2))), y = 4500, x = 1, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value, label = paste("Median:", round(median_value, 2))), y = 4240, x = 1, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value, label = paste("Std. Dev.:", round(sd_value, 2))), y = 4000, x = 1, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in Seattle") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Seattle.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Seattle.svg", h, width = 8, height = 6, units = "in", dpi = 300)
  
     
#### 2. LOS ANGELES ####
  #### 2.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_LA <- st_read("BenMAP_results/1km_V2/LA/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_LA, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 LA Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_LA.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_LA.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 2.2. Mortality results ####
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
      total_population_LA <- sum(population_2019_data_pixel_age_final_LA_fix$Population)
      
      ###MAP
      p <- ggplot() +
        geom_sf(data = benmap_2050_LA_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 LA Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_LA),
                                    "(",round((total_deaths_LA*10^6)/total_population_LA), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_LA, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -118.24271554610264,  y =  34.05368499865087), shape = 1, color = "purple", size = 6, stroke = 2) #LA city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_LA.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_LA.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Total deaths
      benmap_2050_LA_pop_weighted_cropped %>%
        mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_LA_pop_weighted_cropped_graph
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_LA_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 LA Mortality due to Mean Yearly PM2.5 [High - Low]")+
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_LA, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -118.24271554610264,  y =  34.05368499865087), shape = 1, color = "purple", size = 6, stroke = 2) #LA city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_LA_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_LA_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_LA <- summary(benmap_2050_LA_pop_weighted_cropped$Point.Estim)
      mean_value_LA <- mean(benmap_2050_LA_pop_weighted_cropped$Point.Estim)
      median_value_LA <- median(benmap_2050_LA_pop_weighted_cropped$Point.Estim)
      sd_value_LA <- sd(benmap_2050_LA_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_LA_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_LA, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_LA, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_LA, label = paste("Mean:", round(mean_value_LA, 2))), y = 3500, x = 1, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_LA, label = paste("Median:", round(median_value_LA, 2))), y = 3240, x = 1, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_LA, label = paste("Std. Dev.:", round(sd_value_LA, 2))), y = 3000, x = 1, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in LA") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_LA.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_LA.svg", h, width = 8, height = 6, units = "in", dpi = 300)
      
#### 3. RIVERSIDE ####
  #### 3.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_Riverside <- st_read("BenMAP_results/1km_V2/Riverside/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_Riverside, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 Riverside Difference Pollution PM2.5 (ug/m3) [High - Low]") 
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Riverside.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Riverside.png.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 3.2. Mortality results ####
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
      total_population_Riverside <- sum(population_2019_data_pixel_age_final_riverside$Population)
      
      ###MAP
      p <- ggplot() +
        geom_sf(data = benmap_2050_Riverside_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Riverside Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_Riverside),
                                    "(",round((total_deaths_Riverside*10^6)/total_population_Riverside), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() +  
        geom_sf(data = cbsa_shapefile_riverside, color = "black", fill = NA)+
        theme_void() +
      geom_point(data = NULL, aes(x = -117.37559005667082,  y =  33.980696653550375), shape = 1, color = "purple", size = 6, stroke = 2) #Riverside city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Riverside.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Riverside.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Total deaths
      benmap_2050_Riverside_pop_weighted_cropped %>%
        mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Riverside_pop_weighted_cropped_graph
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_Riverside_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Riverside Mortality due to Mean Yearly PM2.5 [High - Low]")+
        annotation_scale() +  
        geom_sf(data = cbsa_shapefile_riverside, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -117.37559005667082,  y =  33.980696653550375), shape = 1, color = "purple", size = 6, stroke = 2) #Riverside city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Riverside_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Riverside_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_Riverside <- summary(benmap_2050_Riverside_pop_weighted_cropped$Point.Estim)
      mean_value_Riverside <- mean(benmap_2050_Riverside_pop_weighted_cropped$Point.Estim)
      median_value_Riverside <- median(benmap_2050_Riverside_pop_weighted_cropped$Point.Estim)
      sd_value_Riverside <- sd(benmap_2050_Riverside_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_Riverside_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_Riverside, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_Riverside, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_Riverside, label = paste("Mean:", round(mean_value_Riverside, 2))), y = 27000, x = 1, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_Riverside, label = paste("Median:", round(median_value_Riverside, 2))), y = 23500, x = 1, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_Riverside, label = paste("Std. Dev.:", round(sd_value_Riverside, 2))), y = 20000, x = 1, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in Riverside") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Riverside.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Riverside.svg", h, width = 8, height = 6, units = "in", dpi = 300)
      
#### 4. PHOENIX ####
  #### 4.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_Phoenix <- st_read("BenMAP_results/1km_V2/Phoenix/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_Phoenix, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 Phoenix Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Phoenix.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Phoenix.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 4.2. Mortality results ####
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
      total_population_Phoenix <- sum(population_2019_data_pixel_age_final_phoenix$Population)
      
      ### MAP
      p <- ggplot() +
        geom_sf(data = benmap_2050_Phoenix_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Phoenix Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_Phoenix),
                                    "(",round((total_deaths_Phoenix*10^6)/total_population_Phoenix), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_phoenix, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -112.07721684407342,  y =  33.448615125541835), shape = 1, color = "purple", size = 6, stroke = 2) #Phoenix city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Phoenix.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Phoenix.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Total deaths
      benmap_2050_Phoenix_pop_weighted_cropped %>%
        mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Phoenix_pop_weighted_cropped_graph
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_Phoenix_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Phoenix Mortality due to Mean Yearly PM2.5 [High - Low]")+
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_phoenix, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -112.07721684407342,  y =  33.448615125541835), shape = 1, color = "purple", size = 6, stroke = 2) #Phoenix city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Phoenix_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Phoenix_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)

      ### HISTOGRAM
      # Calculate summary statistics
      statistics_Phoenix <- summary(benmap_2050_Phoenix_pop_weighted_cropped$Point.Estim)
      mean_value_Phoenix <- mean(benmap_2050_Phoenix_pop_weighted_cropped$Point.Estim)
      median_value_Phoenix <- median(benmap_2050_Phoenix_pop_weighted_cropped$Point.Estim)
      sd_value_Phoenix <- sd(benmap_2050_Phoenix_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_Phoenix_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_Phoenix, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_Phoenix, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_Phoenix, label = paste("Mean:", round(mean_value_Phoenix, 2))), y = 11000, x = 1, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_Phoenix, label = paste("Median:", round(median_value_Phoenix, 2))), y = 10500, x = 1, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_Phoenix, label = paste("Std. Dev.:", round(sd_value_Phoenix, 2))), y = 10000, x = 1, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in Phoenix") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Phoenix.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Phoenix.svg", h, width = 8, height = 6, units = "in", dpi = 300)
      
#### 5. DALLAS ####
  #### 5.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_Dallas <- st_read("BenMAP_results/1km_V2/Dallas/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240517.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_Dallas, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 Dallas Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Dallas.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Dallas.png.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 5.2. Mortality results ####
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
      total_population_Dallas <- sum(population_2019_data_pixel_age_final_dallas$Population)
      
      ### MAP
      p <- ggplot() +
        geom_sf(data = benmap_2050_Dallas_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Dallas Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_Dallas),
                                    "(",round((total_deaths_Dallas*10^6)/total_population_Dallas), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_dallas, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -96.7968013977644,  y =  32.776295519527835), shape = 1, color = "purple", size = 6, stroke = 2) #Dallas city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Dallas.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Dallas.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Total deaths
      benmap_2050_Dallas_pop_weighted_cropped %>%
        mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Dallas_pop_weighted_cropped_graph
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_Dallas_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Dallas Mortality due to Mean Yearly PM2.5 [High - Low]")+
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_dallas, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -96.7968013977644,  y =  32.776295519527835), shape = 1, color = "purple", size = 6, stroke = 2) #Dallas city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Dallas_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Dallas_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_Dallas <- summary(benmap_2050_Dallas_pop_weighted_cropped$Point.Estim)
      mean_value_Dallas <- mean(benmap_2050_Dallas_pop_weighted_cropped$Point.Estim)
      median_value_Dallas <- median(benmap_2050_Dallas_pop_weighted_cropped$Point.Estim)
      sd_value_Dallas <- sd(benmap_2050_Dallas_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_Dallas_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_Dallas, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_Dallas, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_Dallas, label = paste("Mean:", round(mean_value_Dallas, 2))), y = 4000, x = 0.6, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_Dallas, label = paste("Median:", round(median_value_Dallas, 2))), y = 3750, x = 0.6, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_Dallas, label = paste("Std. Dev.:", round(sd_value_Dallas, 2))), y = 3500, x = 0.6, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in Dallas") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Dallas.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Dallas.svg", h, width = 8, height = 6, units = "in", dpi = 300)
      
#### 6. HOUSTON ####
  #### 6.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_Houston <- st_read("BenMAP_results/1km_V2/Houston/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240517.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_Houston, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 Houston Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Houston.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Houston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 6.2. Mortality results ####
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
      total_population_Houston <- sum(population_2019_data_pixel_age_final_houston_fix$Population)
      
      ### MAPS
      p <- ggplot() +
        geom_sf(data = benmap_2050_Houston_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Houston Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_Houston),
                                    "(",round((total_deaths_Houston*10^6)/total_population_Houston), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_houston, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -95.36946640065364,  y =  29.760080695254256), shape = 1, color = "purple", size = 6, stroke = 2) #Houston city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Houston.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Houston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Total deaths
      benmap_2050_Houston_pop_weighted_cropped %>%
        mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Houston_pop_weighted_cropped_graph
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_Houston_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Houston Mortality due to Mean Yearly PM2.5 [High - Low]")+
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_houston, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -95.36946640065364,  y =  29.760080695254256), shape = 1, color = "purple", size = 6, stroke = 2) #Houston city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Houston_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Houston_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_Houston <- summary(benmap_2050_Houston_pop_weighted_cropped$Point.Estim)
      mean_value_Houston <- mean(benmap_2050_Houston_pop_weighted_cropped$Point.Estim)
      median_value_Houston <- median(benmap_2050_Houston_pop_weighted_cropped$Point.Estim)
      sd_value_Houston <- sd(benmap_2050_Houston_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_Houston_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_Houston, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_Houston, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_Houston, label = paste("Mean:", round(mean_value_Houston, 2))), y = 4000, x = 0.6, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_Houston, label = paste("Median:", round(median_value_Houston, 2))), y = 3750, x = 0.6, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_Houston, label = paste("Std. Dev.:", round(sd_value_Houston, 2))), y = 3500, x = 0.6, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in Houston") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Houston.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Houston.svg", h, width = 8, height = 6, units = "in", dpi = 300)
      
#### 7. MIAMI ####
  #### 7.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_Miami <- st_read("BenMAP_results/1km_V2/Miami/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_Miami, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 Miami Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Miami.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Miami.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 7.2. Mortality results ####
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
      total_population_Miami <- sum(population_2019_data_pixel_age_final_miami_fix$Population)
      
      ### MAP
      p <- ggplot() +
        geom_sf(data = benmap_2050_Miami_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Miami Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_Miami),
                                    "(",round((total_deaths_Miami*10^6)/total_population_Miami), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_miami, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -80.23398527668328,  y =  25.72761157460114), shape = 1, color = "purple", size = 6, stroke = 2) #Miami city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Miami.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Miami.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Total deaths
      benmap_2050_Miami_pop_weighted_cropped %>%
        mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Miami_pop_weighted_cropped_graph
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_Miami_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Miami Mortality due to Mean Yearly PM2.5 [High - Low]")+
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_miami, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -80.23398527668328,  y =  25.72761157460114), shape = 1, color = "purple", size = 6, stroke = 2) #Miami city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Miami_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Miami_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_Miami <- summary(benmap_2050_Miami_pop_weighted_cropped$Point.Estim)
      mean_value_Miami <- mean(benmap_2050_Miami_pop_weighted_cropped$Point.Estim)
      median_value_Miami <- median(benmap_2050_Miami_pop_weighted_cropped$Point.Estim)
      sd_value_Miami <- sd(benmap_2050_Miami_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_Miami_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_Miami, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_Miami, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_Miami, label = paste("Mean:", round(mean_value_Miami, 2))), y = 4000, x = 0.6, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_Miami, label = paste("Median:", round(median_value_Miami, 2))), y = 3750, x = 0.6, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_Miami, label = paste("Std. Dev.:", round(sd_value_Miami, 2))), y = 3500, x = 0.6, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in Miami") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Miami.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Miami.svg", h, width = 8, height = 6, units = "in", dpi = 300)
      
#### 8. ATLANTA ####
  #### 8.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_Atlanta <- st_read("BenMAP_results/1km_V2/Atlanta/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240517.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_Atlanta, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 Atlanta Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Atlanta.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Atlanta.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 8.2. Mortality results ####
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
      total_population_Atlanta <- sum(population_2019_data_pixel_age_final_atl$Population)
      
      ### MAP
      p <- ggplot() +
        geom_sf(data = benmap_2050_Atlanta_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Atlanta Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_Atlanta),
                                    "(",round((total_deaths_Atlanta*10^6)/total_population_Atlanta), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_atl, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -84.39062010249631,  y =  33.74857740874673), shape = 1, color = "purple", size = 6, stroke = 2) #Atlanta city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Atlanta.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Atlanta.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Total deaths
      benmap_2050_Atlanta_pop_weighted_cropped %>%
        mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Atlanta_pop_weighted_cropped_graph
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_Atlanta_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Atlanta Mortality due to Mean Yearly PM2.5 [High - Low]")+
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_atl, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -84.39062010249631,  y =  33.74857740874673), shape = 1, color = "purple", size = 6, stroke = 2) #Atlanta city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Atlanta_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Atlanta_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_Atlanta <- summary(benmap_2050_Atlanta_pop_weighted_cropped$Point.Estim)
      mean_value_Atlanta <- mean(benmap_2050_Atlanta_pop_weighted_cropped$Point.Estim)
      median_value_Atlanta <- median(benmap_2050_Atlanta_pop_weighted_cropped$Point.Estim)
      sd_value_Atlanta <- sd(benmap_2050_Atlanta_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_Atlanta_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_Atlanta, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_Atlanta, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_Atlanta, label = paste("Mean:", round(mean_value_Atlanta, 2))), y = 4000, x = 0.6, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_Atlanta, label = paste("Median:", round(median_value_Atlanta, 2))), y = 3750, x = 0.6, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_Atlanta, label = paste("Std. Dev.:", round(sd_value_Atlanta, 2))), y = 3500, x = 0.6, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in Atlanta") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Atlanta.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Atlanta.svg", h, width = 8, height = 6, units = "in", dpi = 300)

#### 9. CHICAGO ####
  #### 9.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_Chicago <- st_read("BenMAP_results/1km_V2/Chicago/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240521.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_Chicago, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 Chicago Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Chicago.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Chicago.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 9.2. Mortality results ####
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
      total_population_Chicago <- sum(population_2019_data_pixel_age_final_chicago_fix$Population)
      
      ### MAP
      p <- ggplot() +
        geom_sf(data = benmap_2050_Chicago_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Chicago Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_Chicago),
                                    "(",round((total_deaths_Chicago*10^6)/total_population_Chicago), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_chicago, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -87.63207808921189,  y =  41.8837839226344), shape = 1, color = "purple", size = 6, stroke = 2) #Chicago city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Chicago.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Chicago.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Total deaths
      benmap_2050_Chicago_pop_weighted_cropped %>%
        mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Chicago_pop_weighted_cropped_graph
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_Chicago_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Chicago Mortality due to Mean Yearly PM2.5 [High - Low]")+
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_chicago, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -87.63207808921189,  y =  41.8837839226344), shape = 1, color = "purple", size = 6, stroke = 2) #Chicago city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Chicago_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Chicago_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_Chicago <- summary(benmap_2050_Chicago_pop_weighted_cropped$Point.Estim)
      mean_value_Chicago <- mean(benmap_2050_Chicago_pop_weighted_cropped$Point.Estim)
      median_value_Chicago <- median(benmap_2050_Chicago_pop_weighted_cropped$Point.Estim)
      sd_value_Chicago <- sd(benmap_2050_Chicago_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_Chicago_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_Chicago, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_Chicago, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_Chicago, label = paste("Mean:", round(mean_value_Chicago, 2))), y = 4000, x = 0.6, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_Chicago, label = paste("Median:", round(median_value_Chicago, 2))), y = 3750, x = 0.6, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_Chicago, label = paste("Std. Dev.:", round(sd_value_Chicago, 2))), y = 3500, x = 0.6, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in Chicago") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Chicago.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Chicago.svg", h, width = 8, height = 6, units = "in", dpi = 300)
      
#### 10. WASHINGTON DC ####
  #### 10.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_DC <- st_read("BenMAP_results/1km_V2/DC/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240517.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_DC, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 DC Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_DC.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_DC.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 10.2. Mortality results ####
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
      total_population_DC <- sum(population_2019_data_pixel_age_final_wash_fix$Population)
      
      ### MAP
      p <- ggplot() +
        geom_sf(data = benmap_2050_DC_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 DC Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_DC),
                                    "(",round((total_deaths_DC*10^6)/total_population_DC), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_wash, color = "black", fill = NA)+
        theme_void() +
      geom_point(data = NULL, aes(x = -77.03653714296149,  y =  38.89771345976116), shape = 1, color = "purple", size = 6, stroke = 2) #The White House
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_DC.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_DC.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Total deaths
      benmap_2050_DC_pop_weighted_cropped %>%
        mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_DC_pop_weighted_cropped_graph
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_DC_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 DC Mortality due to Mean Yearly PM2.5 [High - Low]")+
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_wash, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -77.03653714296149,  y =  38.89771345976116), shape = 1, color = "purple", size = 6, stroke = 2) #The White House
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_DC_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_DC_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_DC <- summary(benmap_2050_DC_pop_weighted_cropped$Point.Estim)
      mean_value_DC <- mean(benmap_2050_DC_pop_weighted_cropped$Point.Estim)
      median_value_DC <- median(benmap_2050_DC_pop_weighted_cropped$Point.Estim)
      sd_value_DC <- sd(benmap_2050_DC_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_DC_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_DC, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_DC, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_DC, label = paste("Mean:", round(mean_value_DC, 2))), y = 4000, x = 0.6, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_DC, label = paste("Median:", round(median_value_DC, 2))), y = 3750, x = 0.6, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_DC, label = paste("Std. Dev.:", round(sd_value_DC, 2))), y = 3500, x = 0.6, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in DC") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_DC.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_DC.svg", h, width = 8, height = 6, units = "in", dpi = 300)
      
#### 11. PHILADELPHIA ####
  #### 11.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_Philadelphia <- st_read("BenMAP_results/1km_V2/Philadelphia/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_Philadelphia, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 Philadelphia Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Philadelphia.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Philadelphia.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 11.2. Mortality results ####
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
      total_population_Philadelphia <- sum(population_2019_data_pixel_age_final_phil_fix$Population)
      
      ### MAPS
      p <- ggplot() +
        geom_sf(data = benmap_2050_Philadelphia_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Philadelphia Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_Philadelphia),
                                    "(",round((total_deaths_Philadelphia*10^6)/total_population_Philadelphia), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_phil, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -75.16326872597556,  y =  39.95279997140329), shape = 1, color = "purple", size = 6, stroke = 2) #Philadelphia city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Philadelphia.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Philadelphia.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Total deaths
      benmap_2050_Philadelphia_pop_weighted_cropped %>%
        mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Philadelphia_pop_weighted_cropped_graph
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_Philadelphia_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Philadelphia Mortality due to Mean Yearly PM2.5 [High - Low]")+
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_phil, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -75.16326872597556,  y =  39.95279997140329), shape = 1, color = "purple", size = 6, stroke = 2) #Philadelphia city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Philadelphia_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Philadelphia_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_Philadelphia <- summary(benmap_2050_Philadelphia_pop_weighted_cropped$Point.Estim)
      mean_value_Philadelphia <- mean(benmap_2050_Philadelphia_pop_weighted_cropped$Point.Estim)
      median_value_Philadelphia <- median(benmap_2050_Philadelphia_pop_weighted_cropped$Point.Estim)
      sd_value_Philadelphia <- sd(benmap_2050_Philadelphia_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_Philadelphia_pop_weighted_cropped, aes(Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_Philadelphia, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_Philadelphia, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_Philadelphia, label = paste("Mean:", round(mean_value_Philadelphia, 2))), y = 4000, x = 0.6, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_Philadelphia, label = paste("Median:", round(median_value_Philadelphia, 2))), y = 3750, x = 0.6, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_Philadelphia, label = paste("Std. Dev.:", round(sd_value_Philadelphia, 2))), y = 3500, x = 0.6, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in Philadelphia") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Philadelphia.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Philadelphia.svg", h, width = 8, height = 6, units = "in", dpi = 300)
      
#### 12. NEW YORK ####
  #### 12.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_NY <- st_read("BenMAP_results/1km_V2/NY/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_NY, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 NY Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_NY.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_NY.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 12.2. Mortality results ####
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
      total_population_NY <- sum(population_2019_data_pixel_age_final_NY_fix$Population)
      
      ### MAP
      p <- ggplot() +
        geom_sf(data = benmap_2050_NY_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 NY Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_NY),
                                    "(",round((total_deaths_NY*10^6)/total_population_NY), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_NY, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -74.00583906157757,  y =  40.71266458461421), shape = 1, color = "purple", size = 6, stroke = 2) #New York city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_NY.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_NY.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Total deaths
      benmap_2050_NY_pop_weighted_cropped %>%
        mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_NY_pop_weighted_cropped_graph
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_NY_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 NY Mortality due to Mean Yearly PM2.5 [High - Low]")+
        annotation_scale() +
        geom_sf(data = cbsa_shapefile_NY, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -74.00583906157757,  y =  40.71266458461421), shape = 1, color = "purple", size = 6, stroke = 2) #New York city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_NY_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_NY_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_NY <- summary(benmap_2050_NY_pop_weighted_cropped$Point.Estim)
      mean_value_NY <- mean(benmap_2050_NY_pop_weighted_cropped$Point.Estim)
      median_value_NY <- median(benmap_2050_NY_pop_weighted_cropped$Point.Estim)
      sd_value_NY <- sd(benmap_2050_NY_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_NY_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_NY, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_NY, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_NY, label = paste("Mean:", round(mean_value_NY, 2))), y = 4000, x = 0.6, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_NY, label = paste("Median:", round(median_value_NY, 2))), y = 3750, x = 0.6, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_NY, label = paste("Std. Dev.:", round(sd_value_NY, 2))), y = 3500, x = 0.6, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in NY") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_NY.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_NY.svg", h, width = 8, height = 6, units = "in", dpi = 300)
      
#### 13. BOSTON ####
  #### 13.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_Boston <- st_read("BenMAP_results/1km_V2/Boston/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240517.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_Boston, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 Boston Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Boston.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Boston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 13.2. Mortality results ####
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
      total_population_Boston <- sum(population_2019_data_pixel_age_final_boston_fix$Population)
      
      ### MAPS
      p <- ggplot() +
        geom_sf(data = benmap_2050_Boston_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Boston Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_Boston),
                                    "(",round((total_deaths_Boston*10^6)/total_population_Boston), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_boston, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -71.05789998431737,  y =  42.360362270351416), shape = 1, color = "purple", size = 6, stroke = 2) #Boston city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Boston.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Boston.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Total deaths
      benmap_2050_Boston_pop_weighted_cropped %>%
        mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_Boston_pop_weighted_cropped_graph
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_Boston_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Boston Mortality due to Mean Yearly PM2.5 [High - Low]")+
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_boston, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -71.05789998431737,  y =  42.360362270351416), shape = 1, color = "purple", size = 6, stroke = 2) #Boston city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Boston_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Boston_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_Boston <- summary(benmap_2050_Boston_pop_weighted_cropped$Point.Estim)
      mean_value_Boston <- mean(benmap_2050_Boston_pop_weighted_cropped$Point.Estim)
      median_value_Boston <- median(benmap_2050_Boston_pop_weighted_cropped$Point.Estim)
      sd_value_Boston <- sd(benmap_2050_Boston_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_Boston_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_Boston, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_Boston, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_Boston, label = paste("Mean:", round(mean_value_Boston, 2))), y = 4000, x = 0.6, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_Boston, label = paste("Median:", round(median_value_Boston, 2))), y = 3750, x = 0.6, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_Boston, label = paste("Std. Dev.:", round(sd_value_Boston, 2))), y = 3500, x = 0.6, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in Boston") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Boston.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Boston.svg", h, width = 8, height = 6, units = "in", dpi = 300)
      
#### 14. DETROIT ####
  #### 14.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_Detroit <- st_read("BenMAP_results/1km_V2/Detroit/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240517.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_Detroit, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 Detroit Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Detroit.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_Detroit.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 14.2. Mortality results ####
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
      total_population_Detroit <- sum(population_2019_data_pixel_age_final_detroit_fix$Population)
      
      ### MAPS
      p <- ggplot() +
        geom_sf(data = benmap_2050_Detroit_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 Detroit Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_Detroit),
                                    "(",round((total_deaths_Detroit*10^6)/total_population_Detroit), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_detroit, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -83.04362102880836,  y =  42.32974049721709), shape = 1, color = "purple", size = 6, stroke = 2) #Detroit city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Detroit.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Detroit.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
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
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Detroit_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_Detroit_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_Detroit <- summary(benmap_2050_Detroit_pop_weighted_cropped$Point.Estim)
      mean_value_Detroit <- mean(benmap_2050_Detroit_pop_weighted_cropped$Point.Estim)
      median_value_Detroit <- median(benmap_2050_Detroit_pop_weighted_cropped$Point.Estim)
      sd_value_Detroit <- sd(benmap_2050_Detroit_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_Detroit_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_Detroit, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_Detroit, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_Detroit, label = paste("Mean:", round(mean_value_Detroit, 2))), y = 3000, x = 0.6, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_Detroit, label = paste("Median:", round(median_value_Detroit, 2))), y = 2750, x = 0.6, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_Detroit, label = paste("Std. Dev.:", round(sd_value_Detroit, 2))), y = 2500, x = 0.6, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in Detroit") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Detroit.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_Detroit.svg", h, width = 8, height = 6, units = "in", dpi = 300)

#### 15. SAN FRANCISCO ####
  #### 15.1. Air quality difference ####
      #PM2.5 difference: High CDR - Low CDR
      PM_difference_2050_SF <- st_read("BenMAP_results/1km_V2/SF/air_quality_2050_1km/Air Quality-PM2.5-Delta_20240520.shp")
      
      p <- ggplot() +
        geom_sf(data = PM_difference_2050_SF, aes(fill = D24HourMean), color = NA, size = 0) +
        scale_fill_viridis_c(option = "magma", direction = -1) + 
        ggtitle("2050 SF Difference Pollution PM2.5 (ug/m3) [High - Low]")
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_SF.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/pollution/1km_PM25_2050_DIFF_plot_SF.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
  #### 15.2. Mortality results ####
      #Here we load BenMAP shapefiles with mortality results to create plots 
      benmap_2050_SF <- st_read("BenMAP_results/1km_V2/SF/health_impacts_2050_1km/Health Impacts-Pope 2019_20240520.shp") 
      #Set 25 pixels with no population to 0
      PM_difference_2050_SF %>%
        dplyr::select(ROW, COL, geometry) %>%
        left_join(benmap_2050_SF %>% st_drop_geometry(), by = c("ROW", "COL")) %>%
        mutate(Point.Estim = if_else(is.na(Point.Estim), 0, Point.Estim)) %>%
        mutate(Population = if_else(is.na(Population), 0, Population)) %>%
        mutate(deaths_per_1000 = if_else(Population == 0, 0, (Point.Estim / Population)*1000), #This is the population in each pixel from 18-99 years, which is where the health impacts are applied
               deaths_per_1000_graph = if_else(deaths_per_1000 >= 1, 1, deaths_per_1000)) -> benmap_2050_SF_pop_weighted #This has number of people dying per pixel for every 1000 inhabitants age 18-99
      
      # Ensure both datasets have the same CRS
      benmap_2050_SF_pop_weighted_cropped <- st_transform(benmap_2050_SF_pop_weighted, crs = st_crs(cbsa_shapefile_SF))
      # Perform intersection to retain only pixels within the city boundary
      benmap_2050_SF_pop_weighted_cropped <- st_intersection(benmap_2050_SF_pop_weighted_cropped, cbsa_shapefile_SF)
      
      #Calculate total
      total_deaths_SF <- sum(benmap_2050_SF$Point.Estim)
      total_deaths_SF_2p5 <- round(sum(benmap_2050_SF$Percentile)) #This is percentile 2.5
      total_deaths_SF_97p5 <- round(sum(benmap_2050_SF$Percentile.19)) #This is percentile 97.5
      total_population_SF <- sum(population_2019_data_pixel_age_final_SF_fix$Population)
      
      ### MAP
      p <- ggplot() +
        geom_sf(data = benmap_2050_SF_pop_weighted_cropped, aes(fill = deaths_per_1000_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 SF Mortality due to Mean Yearly PM2.5 (per 1000 people) [High - Low]")+
        geom_text(aes(label = paste("Total Deaths:", round(total_deaths_SF),
                                    "(",round((total_deaths_SF*10^6)/total_population_SF), "per million people)")),
                  x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_SF, color = "black", fill = NA)+
        theme_void() +
      geom_point(data = NULL, aes(x = -122.41894505158999,  y =  37.779200252025916), shape = 1, color = "purple", size = 6, stroke = 2) #San Francisco city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_SF.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_SF.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      #Total deaths
      benmap_2050_SF_pop_weighted_cropped %>%
        mutate(deaths_graph = if_else(Point.Estim >= 1, 1, Point.Estim)) -> benmap_2050_SF_pop_weighted_cropped_graph
      
      p <- ggplot() +
        geom_sf(data = benmap_2050_SF_pop_weighted_cropped_graph, aes(fill = deaths_graph), color = NA, size = 0) +
        scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1)) + 
        ggtitle("2050 SF Mortality due to Mean Yearly PM2.5  [High - Low]")+
        annotation_scale() + 
        geom_sf(data = cbsa_shapefile_SF, color = "black", fill = NA)+
        theme_void() +
        geom_point(data = NULL, aes(x = -122.41894505158999,  y =  37.779200252025916), shape = 1, color = "purple", size = 6, stroke = 2) #San Francisco city hall
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_SF_total.png", p, width = 16, height = 12, units = "in", dpi = 600)
      #ggsave("figures/BenMAP/1km/mortality/1km_PM25_2050_Benmap_SF_total.svg", p, width = 16, height = 12, units = "in", dpi = 600)
      print(p)
      
      ### HISTOGRAM
      # Calculate summary statistics
      statistics_SF <- summary(benmap_2050_SF_pop_weighted_cropped$Point.Estim)
      mean_value_SF <- mean(benmap_2050_SF_pop_weighted_cropped$Point.Estim)
      median_value_SF <- median(benmap_2050_SF_pop_weighted_cropped$Point.Estim)
      sd_value_SF <- sd(benmap_2050_SF_pop_weighted_cropped$Point.Estim)
      
      # Create the histogram
      h <- ggplot(benmap_2050_SF_pop_weighted_cropped, aes(x = Point.Estim)) +
        geom_histogram(fill = "white", color = "black", bins = 20) +  # Histogram plot with 20 bins
        geom_vline(xintercept = mean_value_SF, linetype = "dashed", color = "red", size = 0.5) +  
        geom_vline(xintercept = median_value_SF, linetype = "dashed", color = "blue", size = 0.5) +
        geom_text(aes(x = mean_value_SF, label = paste("Mean:", round(mean_value_SF, 2))), y = 3000, x = 0.6, hjust = 0, color = "red") +  
        geom_text(aes(x = median_value_SF, label = paste("Median:", round(median_value_SF, 2))), y = 2750, x = 0.6, hjust = 0,  color = "blue") + 
        geom_text(aes(x = sd_value_SF, label = paste("Std. Dev.:", round(sd_value_SF, 2))), y = 2500, x = 0.6, hjust = 0,  color = "black") + 
        ggtitle("Deaths per Thousand People in SF") +
        xlab("Deaths per Thousand People") +
        ylab("Frequency")
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_SF.png", h, width = 8, height = 6, units = "in", dpi = 300)
      #ggsave("figures/BenMAP/1km/mortality/distribution_plot_SF.svg", h, width = 8, height = 6, units = "in", dpi = 300)

      