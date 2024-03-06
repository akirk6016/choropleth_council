
###################### Loading all necessary packages ########################
rm(list = ls())
library(tidyverse)
library(here)
library(sf) # Vector spatial data
library(terra) # Raster spatial data
library(tidyterra) # For rasters in GGplot
library(gstat)
library(stars)
library(broom)
library(tmap)
library(spatstat)
library(patchwork)
library(stringr)
library(tidytext)
library(pdftools)
library(ggwordcloud)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, # data wrangling
  tidyr, # reshape
  tidyUSDA, # download USDA NASS data
  keyring, # API key management
  raster,
  FedData, # download Daymet data
  daymetr, # download Daymet data
  future.apply, # parallel processing
  CropScapeR, # download CDL data
  prism, # download PRISM data
  exactextractr # extract raster values to sf --> i.e. zonal statistics
)

############################## MAX WIDGET WORK ###############################

#### Define color palettes ####

carbon_pal = c("0" = "peachpuff2", "1" = "darkorange2")
food_pal = c("0" = "darkseagreen1", "1" = "darkgreen")

#### Loading ahupuaa.shp as an sf ####
max_ahupuaa_raw_sf <- read_sf(here("data/ahupuaa/ahupuaa.shp"))


#### Loading carbon & food model outputs as sf's ####
max_carbon_sf <- read_sf(here("data/carbon_polygon/carbon_polygon.shp"))

max_carbon_sf <- max_carbon_sf %>%
  mutate(gridcode = as.factor(gridcode))


max_food_sf <- read_sf(here("data/food_polygon/food_polygon.shp"))

max_food_sf <- max_food_sf %>%
  mutate(gridcode = as.factor(gridcode))


#### Load the intersected ahupuaa/model data as sf's ####
max_ahu_carbon_sf <- read_sf(here("data/ahu_model_intersect/ahupuaa_carbon_intersect.shp"))

max_ahu_food_sf <- read_sf(here("data/ahu_model_intersect/ahupuaa_food_intersect.shp"))

# Turn the intersected sf's into df's #
max_ahu_carbon_df <- max_ahu_carbon_sf %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  mutate(gridcode = as.factor(gridcode))

max_ahu_food_df <- max_ahu_food_sf %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  mutate(gridcode = as.factor(gridcode))

# Join the two datasets to prep for shiny reactivity
max_ahu_model_join_df <- max_ahu_carbon_df %>%
  full_join(max_ahu_food_df) %>%
  mutate(model_type = as.character(FID_carbon))

max_ahu_model_join_df$model_type <- ifelse(!is.na(max_ahu_model_join_df$FID_food), "Food",
                                       ifelse(!is.na(max_ahu_model_join_df$FID_carbon),
                                              "Carbon", NA))

# DF FOR SHINY REACTIVITY
max_ahu_model_join_select_df <- max_ahu_model_join_df %>%
  dplyr::select(mokupuni, gridcode, area_hecta, model_type)


############################## DUSTIN WIDGET WORK ###############################
#### AHUPUAA .SHP FILE ####
## Loading ahupuaa .shp file as 'data'
data_dustin <- st_read(here::here("data", "ahupuaa","ahupuaa.shp")) %>%
  janitor::clean_names()

## Obtaining the layers from the ahupuaa .shp file
st_layers(dsn = here::here("data", "ahupuaa","ahupuaa.shp"))

## Converting .shp file to a simple features collection

data_sf_dustin <- st_as_sf(data_dustin) # --> EPSG 3750

## Reprojecting data to WGS84
(
  data_new_sf_dustin <- st_transform(data_dustin, 3750)
)
## Checking coordinate reference system
st_crs(data_sf_dustin)

## Converting the wack apostrophies and other characters to ones R can recognize
data_sf_clean_dustin <- data_sf_dustin %>%
  mutate(moku = str_replace_all(moku, pattern = "ʻ", replacement = "'"),
         mokupuni = str_replace_all(mokupuni, pattern = "ʻ", replacement = "'"),
         ahupuaa = str_replace_all(ahupuaa, pattern = "ʻ", replacement = "'"),
         ahupuaa = str_replace_all(ahupuaa, pattern = "ā", replacement = "a"),
         ahupuaa = str_replace_all(ahupuaa, pattern = "ī", replacement = "i"),
         ahupuaa = str_replace_all(ahupuaa, pattern = "ï", replacement = "i"),
         ahupuaa = str_replace_all(ahupuaa, pattern = "ō", replacement = "o"),
         ahupuaa = str_replace_all(ahupuaa, pattern = "ē", replacement = "e"),
         ahupuaa = str_replace_all(ahupuaa, pattern = "ū", replacement = "u"),
         ahupuaa = str_replace_all(ahupuaa, pattern = "Ï", replacement = "I"),
         ahupuaa = str_replace_all(ahupuaa, pattern = "Ō", replacement = "O"),
         ahupuaa = str_replace_all(ahupuaa, pattern = "Ā", replacement = "A")) %>%
  mutate(moku = ifelse(
    ahupuaa == "Kiauea", "Ko'olau", moku),
    mokupuni = ifelse(ahupuaa == "Kiauea" & moku == "Ko'olau", "Kaua'i", mokupuni

    )) %>%
  mutate(county = case_when(
    mokupuni == "Kaua'i" ~ "Kaua'i and Ni'ihau",
    mokupuni == "Ni'ihau" ~ "Kaua'i and Ni'ihau",
    mokupuni == "Molokini" ~ "Maui County",
    mokupuni == "Maui" ~ "Maui County",
    mokupuni == "Moloka'i" ~ "Maui County",
    mokupuni == "Lāna'i" ~ "Maui County",
    mokupuni == "Kaho'olawe" ~ "Maui County",
    mokupuni == "Hawai'i" ~ "Hawai'i",
    mokupuni == "O'ahu" ~ "O'ahu"
  )) %>%
  janitor::clean_names()

## Rasterizing data_sf_clean (just in case )
(
  data_rast_dustin <- rast(data_sf_clean_dustin)
)

## Vectorizing data_sf_clean
data_vec_dustin <- vect(data_sf_clean_dustin)

# Filtering data to just hawaii (checking)
data_vec_hawaii_dustin <- data_sf_clean_dustin %>% filter( mokupuni == "Hawai'i")
# Vectorizing it
vect(data_vec_hawaii_dustin)


#### FOOD .TIF FILE #####

## Loads it as a value and then rasterizing it makes it a SpatRaster
food_file_dustin <- here::here("data", "food_priority1_100m.tif")

## Loads it directly as a RasterStack object
# (
# food_rs <- stack(here::here("data", "food_priority1_100m.tif"))
# )

## Checking CRS
# terra::crs(food_rs)

## Rasterizing food_file --> Turns to SpatRaster
(
  food_rast_dustin <- rast(food_file_dustin)
)

## Tells us that the raster is projected in WGS 84
terra::crs(food_rast_dustin)

## Turning food raster into a dataframe for plotting purposes
food_df_dustin <- as.data.frame(food_rast_dustin, xy = TRUE) %>%
  na.omit() %>%
  mutate(Food = as.character(Food))

#### CARBON .TIF FILE ####

## Loads it as a value and then rasterizing it makes it a SpatRaster
carbon_file_dustin <- here("data", "carbon_priority2_100m.tif")

## Rasterizing carbon_file --> Turns to SpatRaster
(
  carbon_rs_dustin <- rast(carbon_file_dustin)
)

## Turning carbon raster into a dataframe for plotting purposes
carbon_df_dustin <- as.data.frame(carbon_rs_dustin, xy = TRUE) %>%
  na.omit() %>%
  mutate(Carbon = as.character(Carbon))

############################## MULTI-LAYER .TIF FILE ##########################

## Loading both carbon and food files as a list
files_list_dustin <- here::here(c("data/food_priority1_100m.tif", "data/carbon_priority2_100m.tif"))

## Rasterizing them --> Same rows and columns, but two layers
(
  multi_layer_rs_dustin <- rast(files_list_dustin)
)

## Converting the raster to polygons
(
  multi_layer_polygons_dustin <- as.polygons(multi_layer_rs_dustin)
)
## Converting polygons to sf
multi_layer_sf <- st_as_sf(multi_layer_polygons_dustin)


(
  data_resample_dustin <- terra::resample(data_rast_dustin, multi_layer_rs_dustin, method='bilinear')
)

multi_layer_df_dustin <- as.data.frame(multi_layer_rs_dustin, xy = TRUE)

multi_layer_df_longer_dustin <- multi_layer_df_dustin %>%
  mutate(Food = as.factor(Food),
         Carbon = as.factor(Carbon)) %>%
  pivot_longer(cols = c("Food", "Carbon"),
               names_to = "priority",
               values_to = "foodcarbon")

multi_layer_df_food_dustin <- as.data.frame(multi_layer_rs_dustin, xy = TRUE) %>%
  filter(Food == 1 & Carbon == 0)

multi_layer_df_carbon_dustin <- as.data.frame(multi_layer_rs_dustin, xy = TRUE) %>%
  filter(Food == 0 & Carbon == 1)

## Send this down after loading the multi_layers_sr
values <- raster::extract(multi_layer_rs_dustin, data_vec_dustin, fun = max, na.rm = TRUE)

########################### LAND USE COVER DATA ###############################

# Success he thinks it will be useful!
(
  landuse_dustin <- st_read(here::here("data/Land_Use_Land_Cover_(LULC)/Land_Use_Land_Cover_(LULC).shp"))
)
(
  landuse_dustin <- st_transform(landuse_dustin, 3750)
)
landuse_sf_dustin <- st_as_sf(landuse_dustin) %>%
  mutate(landcover = as.factor(landcover)) %>%
  mutate(resample = case_when(
    landcover == "0" ~ "No classification",
    landcover == "11" ~ "Urban or Built-Up",
    landcover == "12" ~ "Urban or Built-Up",
    landcover == "13" ~ "Urban or Built-Up",
    landcover == "14" ~ "Urban or Built-Up",
    landcover == "15" ~ "Urban or Built-Up",
    landcover == "16" ~ "Urban or Built-Up",
    landcover == "17" ~ "Urban or Built-Up",
    landcover == "21" ~ "Agriculture",
    landcover == "22" ~ "Agriculture",
    landcover == "23" ~ "Agriculture",
    landcover == "24" ~ "Agriculture",
    landcover == "31" ~ "Rangeland",
    landcover == "32" ~ "Rangeland",
    landcover == "33" ~ "Rangeland",
    landcover == "42" ~ "Forest Land",
    landcover == "51" ~ "Water",
    landcover == "52" ~ "Water",
    landcover == "53" ~ "Water",
    landcover == "54" ~ "Water",
    landcover == "61" ~ "Wetlands",
    landcover == "62" ~ "Wetlands",
    landcover == "72" ~ "Barren",
    landcover == "73" ~ "Barren",
    landcover == "74" ~ "Barren",
    landcover == "75" ~ "Barren",
    landcover == "76" ~ "Urban or Built-Up",
    landcover == "77" ~ "Barren",
  )) %>%
  mutate(resample = as.factor(resample)) %>%
  dplyr::select(!c(createdate, createdby, modifiedda, modifiedby, agency, deliveryda, sourceid, severity, publishdat, comments, featureuid))

landuse_vec_dustin <- vect(landuse_sf_dustin)

## Intersecting the ahupuaa and landuse dataframe to obtain total area (ha)

intersected_polygons_dustin <- sf::st_intersection(data_sf_clean_dustin, landuse_sf_dustin)

## Summing landuse coverage per ahupua'a
area_landuse_sum_dustin <- intersected_polygons_dustin %>%
  group_by( moku, mokupuni, county, resample) %>%
  summarize(landuse_ha = sum(st_areasha, na.rm = TRUE)*(1^-4)) %>%
  ungroup()


############################## ABIGAIL WIDGET WORK ###############################
