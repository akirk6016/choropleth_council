
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
library(MetBrewer)
library(textdata)
library(ggExtra)
library(ggnewscale)
library(ggh4x)


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

carbon_pal = c("0" = "bisque1", "1" = "darkorange2")
food_pal = c("0" = "darkseagreen1", "1" = "darkgreen")

#### Loading ahupuaa.shp as an sf ####
max_ahupuaa_raw_sf <- read_sf(here("data/ahupuaa/ahupuaa.shp")) %>%
  dplyr::select(Ahupuaa = ahupuaa, Moku = moku, Mokupuni = mokupuni,
                Acres = gisacres)


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
levels <- c("Hawai'i", 'Maui', 'Kaho\'olawe', 'Lāna\'i', 'Moloka\'i', 'O\'ahu', 'Kaua\'i', "Ni\'ihau")
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
    mokupuni == "O'ahu" ~ "Honolulu County"
  )) %>%
  filter(!mokupuni == "Molokini") %>%
  mutate(mokupuni = factor(mokupuni, levels = levels)) %>%
  arrange(mokupuni) %>%
  janitor::clean_names()

#### FOOD .TIF FILE #####

## Loads it as a value and then rasterizing it makes it a SpatRaster
food_file_dustin <- here::here("data", "food_priority1_100m.tif")

## Checking CRS
# terra::crs(food_rs)

## Rasterizing food_file --> Turns to SpatRaster

food_rs_dustin_init <- rast(food_file_dustin)

food_rs_dustin <- extend(food_rs_dustin_init, data_sf_clean_dustin)


## Tells us that the raster is projected in WGS 84
terra::crs(food_rs_dustin)

#### CARBON .TIF FILE ####

## Loads it as a value and then rasterizing it makes it a SpatRaster
carbon_file_dustin <- here("data", "carbon_priority2_100m.tif")

## Rasterizing carbon_file --> Turns to SpatRaster

carbon_rs_dustin_init <- rast(carbon_file_dustin)

carbon_rs_dustin <- extend(carbon_rs_dustin_init, data_sf_clean_dustin)


## Turning carbon raster into a dataframe for plotting purposes
# carbon_df_dustin <- as.data.frame(carbon_rs_dustin, xy = TRUE) %>%
#   na.omit() %>%
#   mutate(Carbon = as.character(Carbon))

############################## MULTI-LAYER .TIF FILE ##########################

## Loading both carbon and food files as a list
# files_list_dustin <- here::here(c("data/food_priority1_100m.tif", "data/carbon_priority2_100m.tif"))
#
# ## Rasterizing them --> Same rows and columns, but two layers
#
# (
#   multi_layer_rs_dustin <- rast(files_list_dustin)
# )
# # Take the multi_layer_rs_dustin file and increase its bounding box to that of data_sf_clean_dustin.
# multi_layer_rs_dustin <- extend(multi_layer_rs_dustin, data_sf_dustin)
# ## Converting the raster to polygons
# (
#   multi_layer_polygons_dustin <- as.polygons(multi_layer_rs_dustin)
# )
# ## Converting polygons to sf
# multi_layer_sf <- st_as_sf(multi_layer_polygons_dustin)
#
#
# (
#   data_resample_dustin <- terra::resample(data_rast_dustin, multi_layer_rs_dustin, method='bilinear')
# )
#
# multi_layer_df_dustin <- as.data.frame(multi_layer_rs_dustin, xy = TRUE)
#
#
# multi_layer_df_longer_dustin <- multi_layer_df_dustin %>%
#   mutate(Food = as.factor(Food),
#          Carbon = as.factor(Carbon)) %>%
#   pivot_longer(cols = c("Food", "Carbon"),
#                names_to = "priority",
#                values_to = "foodcarbon")
#
# multi_layer_df_food_dustin <- as.data.frame(multi_layer_rs_dustin, xy = TRUE) %>%
#   filter(Food == 1 & Carbon == 0)
#
# multi_layer_df_carbon_dustin <- as.data.frame(multi_layer_rs_dustin, xy = TRUE) %>%
#   filter(Food == 0 & Carbon == 1)

## Send this down after loading the multi_layers_sr
# values <- raster::extract(multi_layer_rs_dustin, data_vec_dustin, fun = max, na.rm = TRUE)

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
  filter(!mokupuni == "Molokini") %>%
  group_by( moku, mokupuni, county, resample) %>%
  summarize(landuse_ha = sum(st_areasha, na.rm = TRUE)*(1^-4)) %>%
  ungroup()

strips <- strip_themed(
  text_x = elem_list_text(face = "bold", size = 10)
)



############################## ABIGAIL WIDGET WORK ###############################
intrvw_1 <- pdftools::pdf_text(here::here('data','Pioneer Mill Interviews', 'pioneermill_03.pdf'))
intrvw_2 <- pdftools::pdf_text(here::here('data', 'Pioneer Mill Interviews','pioneermill_04.pdf'))
intrvw_3 <- pdftools::pdf_text(here::here('data', 'Pioneer Mill Interviews', 'pioneermill_05.pdf'))
intrvw_4 <- pdftools::pdf_text(here::here('data','Pioneer Mill Interviews', 'pioneermill_06.pdf'))
intrvw_5 <- pdftools::pdf_text(here::here('data', 'Pioneer Mill Interviews','pioneermill_07.pdf'))
intrvw_6 <- pdftools::pdf_text(here::here('data', 'Pioneer Mill Interviews','pioneermill_08.pdf'))
intrvw_7 <- pdftools::pdf_text(here::here('data','Pioneer Mill Interviews', 'pioneermill_09.pdf'))
intrvw_8 <- pdftools::pdf_text(here::here('data', 'Pioneer Mill Interviews','pioneermill_10.pdf'))
intrvw_9 <- pdftools::pdf_text(here::here('data', 'Pioneer Mill Interviews','pioneermill_11.pdf'))
intrvw_10 <- pdftools::pdf_text(here::here('data', 'Pioneer Mill Interviews','pioneermill_12.pdf'))
intrvw_11 <- pdftools::pdf_text(here::here('data', 'Pioneer Mill Interviews','pioneermill_13.pdf'))
intrvw_12 <- pdftools::pdf_text(here::here('data', 'Pioneer Mill Interviews','pioneermill_14.pdf'))
intrvw_13 <- pdftools::pdf_text(here::here('data', 'Pioneer Mill Interviews','pioneermill_15.pdf'))
intrvw_14 <- pdftools::pdf_text(here::here('data', 'Pioneer Mill Interviews','pioneermill_16.pdf'))
intrvw_15 <- pdftools::pdf_text(here::here('data', 'Pioneer Mill Interviews','pioneermill_17.pdf'))
intrvw_16 <- pdftools::pdf_text(here::here('data', 'Pioneer Mill Interviews','pioneermill_18.pdf'))


lines_1 <- data.frame(intrvw_1) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_1, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_2 <- data.frame(intrvw_2) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_2, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space


lines_3 <- data.frame(intrvw_3) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_3, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_4 <- data.frame(intrvw_4) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_4, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_5 <- data.frame(intrvw_5) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_5, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_6 <- data.frame(intrvw_6) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_6, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_7 <- data.frame(intrvw_7) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_7, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_8 <- data.frame(intrvw_8) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_8, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_9 <- data.frame(intrvw_9) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_9, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_10 <- data.frame(intrvw_10) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_10, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_11 <- data.frame(intrvw_11) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_11, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_12 <- data.frame(intrvw_12) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_12, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_13 <- data.frame(intrvw_13) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_13, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_14 <- data.frame(intrvw_14) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_14, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_15 <- data.frame(intrvw_15) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_15, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space

lines_16 <- data.frame(intrvw_16) %>%
  mutate(page = 1:n()) %>%
  mutate(text_full = str_split(intrvw_16, pattern = '\\n')) %>%
  unnest(text_full) %>% ## turns lines on the page and "pivot longer"
  mutate(text_full = str_trim(text_full)) ## gets ride of white space


words_1 <- lines_1 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_1)

words_2 <- lines_2 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_2)

words_3 <- lines_3 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_3)

words_4 <- lines_4 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_4)

words_5 <- lines_5 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_5)

words_6 <- lines_6 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_6)

words_7 <- lines_7 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_7)

words_8 <- lines_8 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_8)

words_9 <- lines_9 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_9)

words_10 <- lines_10 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_10)

words_11 <- lines_11 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_11)

words_12 <- lines_12 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_12)

words_13 <- lines_13 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_13)

words_14 <- lines_14 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_14)

words_15 <- lines_15 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_15)

words_16 <- lines_16 %>%
  unnest_tokens(word, text_full) %>%
  dplyr::select(-intrvw_16)

wordcount_1 <- words_1 %>%
  count(word)

wordcount_2 <- words_2 %>%
  count(word)

wordcount_3 <- words_3 %>%
  count(word)

wordcount_4 <- words_4 %>%
  count(word)

wordcount_5 <- words_5 %>%
  count(word)

wordcount_6 <- words_6 %>%
  count(word)

wordcount_7 <- words_7 %>%
  count(word)

wordcount_8 <- words_8 %>%
  count(word)

wordcount_9 <- words_9 %>%
  count(word)

wordcount_10 <- words_10 %>%
  count(word)

wordcount_11 <- words_11 %>%
  count(word)

wordcount_12 <- words_12 %>%
  count(word)

wordcount_13 <- words_13 %>%
  count(word)

wordcount_14 <- words_14 %>%
  count(word)

wordcount_15 <- words_15 %>%
  count(word)

wordcount_16 <- words_16 %>%
  count(word)


wc_clean_1 <- wordcount_1 %>%
  anti_join(stop_words, by = 'word') %>%
  filter(word != 'hy') %>%
  filter(word != 'jk')%>%
  filter(word != 'yeah')


wc_clean_2 <- wordcount_2 %>%
  anti_join(stop_words, by = 'word')  %>%
  filter(word != 'wn') %>%
  filter(word != 'ss') %>%
  filter(word != 'yeah')

wc_clean_3 <- wordcount_3 %>%
  anti_join(stop_words, by = 'word')  %>%
  filter(word != 'wn') %>%
  filter(word != 'mh') %>%
  filter(word != 'yeah')

wc_clean_4 <- wordcount_4 %>%
  anti_join(stop_words, by = 'word')  %>%
  filter(word != 'wn') %>%
  filter(word != 'dr') %>%
  filter(word != 'yeah')


wc_clean_5 <- wordcount_5 %>%
  anti_join(stop_words, by = 'word')  %>%
  filter(word != 'wn') %>%
  filter(word != 'sh') %>%
  filter(word != 'yeah') %>%
  filter(word != 'em') %>%
  filter(word != 'mh')



wc_clean_6 <- wordcount_6 %>%
  anti_join(stop_words, by = 'word')  %>%
  filter(word != 'wn') %>%
  filter(word != 'av') %>%
  filter(word != 'yeah')

wc_clean_7 <- wordcount_7 %>%
  anti_join(stop_words, by = 'word')  %>%
  filter(word != 'em') %>%
  filter(word != 'wn') %>%
  filter(word != 'yeah')  %>%
  filter(word != 'bb') %>%
  filter(word != 'eh')


wc_clean_8 <- wordcount_8 %>%
  anti_join(stop_words, by = 'word')  %>%
  filter(word != 'mo') %>%
  filter(word != 'jh') %>%
  filter(word != 'yeah') %>%
  filter(word != 'em')


wc_clean_9 <- wordcount_9 %>%
  anti_join(stop_words, by = 'word')  %>%
  filter(word != 'em') %>%
  filter(word != 'mo') %>%
  filter(word != 'yeah')  %>%
  filter(word != 'fh') %>%
  filter(word != 'pu') %>%
  filter(word != 'ukoli')


wc_clean_10 <- wordcount_10 %>%
  anti_join(stop_words, by = 'word') %>%
  filter(word != 'mo') %>%
  filter(word != 'sk') %>%
  filter(word != 'yeah') %>%
  filter(word != '')

wc_clean_11 <- wordcount_11 %>%
  anti_join(stop_words, by = 'word')  %>%
  filter(word != 'wn') %>%
  filter(word != 'hf') %>%
  filter(word != 'yeah') %>%
  filter(word != 'em')

wc_clean_12 <- wordcount_12 %>%
  anti_join(stop_words, by = 'word') %>%
  filter(word != 'wn') %>%
  filter(word != 'df') %>%
  filter(word != 'yeah')


wc_clean_13 <- wordcount_13 %>%
  anti_join(stop_words, by = 'word')  %>%
  filter(word != 'ak') %>%
  filter(word != 'alk') %>%
  filter(word != 'yeah')  %>%
  filter(word != 'tk') %>%
  filter(word != 'wn')



wc_clean_14 <- wordcount_14 %>%
  anti_join(stop_words, by = 'word')  %>%
  filter(word != 'mh') %>%
  filter(word != 'hy') %>%
  filter(word != 'yeah')

wc_clean_15 <- wordcount_15 %>%
  anti_join(stop_words, by = 'word')

wc_clean_16 <- wordcount_16 %>%
  anti_join(stop_words, by = 'word') %>%
  filter(word != 'mo') %>%
  filter(word != 'td') %>%
  filter(word != 'em') %>%
  filter(word != 'yeah')

top_1 <- wc_clean_1 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 1)

top_2 <- wc_clean_2 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 2)

top_3 <- wc_clean_3 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 3)

top_4 <- wc_clean_4 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 4)

top_5 <- wc_clean_5 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 5)


top_6 <- wc_clean_6 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 6)

top_7 <- wc_clean_7 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 7)

top_8 <- wc_clean_8 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 8)

top_9 <- wc_clean_9 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 9)

top_10 <- wc_clean_10 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 10)

top_11 <- wc_clean_11 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 11)

top_12 <- wc_clean_12 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 12)

top_13 <- wc_clean_13 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 13)

top_14 <- wc_clean_14 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 14)

top_15 <- wc_clean_15 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 15)

top_16 <- wc_clean_16 %>%
  arrange(-n) %>%
  slice(1:10) %>%
  mutate(interview = 16)


unions <- rbind(top_1,top_2,top_3,top_4,top_5,top_6,top_7,top_8,top_9,top_10,top_11,top_12,top_13,top_14,top_15,top_16)

# Make some graphs:
ggplot(data = unions, aes(x = n, y = word)) +
  geom_col(fill = "blue") +
  facet_wrap(~interview, scales = "free")

unions_nrc <- unions %>%
  inner_join(get_sentiments("nrc"), by = 'word')



unions_nrc_counts <- unions_nrc %>%
  group_by(interview, sentiment) %>%
  summarize(n = n())




unions_plot <- ggplot(data = unions_nrc_counts, aes(x = n, y = sentiment, fill = sentiment)) +
  geom_col() +
  facet_wrap(~interview) +
  labs(x = "Wordcount", y = "Sentiment") +
  theme_bw() +
  scale_fill_manual(values =met.brewer("Benedictus", 10)) +
  guides(fill="none")


