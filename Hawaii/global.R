
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

############################## DUSTIN WIDGET WORK ###############################

############################## ABIGAIL WIDGET WORK ###############################
