## code to prepare `Pak_Indicators_Data` dataset goes here

################################################################################
# Data Preparation for Pakistan Indicator App
################################################################################
rm(list=ls())
################################################################################
#Loading required libraries
library(tidyverse)
library(janitor)
library(sf)
library(utf8)
################################################################################

#Shapefiles preparation (To update shapefile in data/, uncomment last code line in the Pak-Shapefile script)
source(file.path("data-raw/Pak_Shapfiles.R"), local = TRUE)

################################################################################
#Join tables for All
#Provinces - District Map
prov_dist <-
  Pak_Shapfiles %>%
  as_tibble() %>%
  distinct(province, district)
################################################################################


#Now no FR Distircts in New Shapefiles (Excluding from datasets)
fr_districts <- c("FR Bannu", "FR Dera Ismail Khan", "FR Kohat",
                  "FR Lakki Marwat", "FR Peshawar", "FR Tank")


################################################################################

# Data Prep Pakistan Indicators legacy
source(file.path("data-raw/Pak_Indicator_Legacy.R"), local = TRUE)

# ################################################################################
# Have followed these district names for all other surveys (Changes made in Pak ind and shapefiles)
dist_to_follow <-
  pak_ind %>%
  distinct(district) %>%
  arrange(district)
# ################################################################################

# Data Prep: PLSM2019 | Poverty Maps 2018 | NNS2018 | MICS Balochistan 2020 | MICS KP and Sindh : 2019
source(file.path("data-raw/Pak_PSLM_2019.R"), local = TRUE)
source(file.path("data-raw/Pak_Poverty_2018.R"), local = TRUE)
source(file.path("data-raw/Pak_NNS_2018.R"), local = TRUE)
source(file.path("data-raw/Pak_Balochistan_MICS_2020.R"), local = TRUE)
source(file.path("data-raw/Pak_KP_Sindh_MICS_2019.R"), local = TRUE)

################################################################################
################################################################################
#Caution rerunning this file (Otherwise will have to redo contextual  color  mapping)

#For revised color mapping (For making amends in units)

# Pak_Indicators_Data %>%
#   distinct(indicator, context) %>% # , indicator_1, units, source, definition, context
#   write.csv("data-raw/rawdata/raw_indicators/color_mapping_revised.csv")

################################################################################
#Final Dataset
################################################################################
# color_mapping <- read.csv("data-raw/rawdata/raw_indicators/color_mapping.csv")    #Old
color_mapping <- read.csv("data-raw/rawdata/raw_indicators/color_mapping_revised.csv")  #New

Pak_Indicators_Data <-
  pak_ind %>%
  bind_rows(pslm_2019,
            pov_2018,
            nns_2018,
            MICS_Bal_2020,
            MICS_2019_KP_Sindh_combined) %>%
  mutate(units = replace_na(units,"")) %>%
  mutate(value = round(value, 2)) %>%
  arrange(year, district) %>%
  select(-context) %>%
  left_join(color_mapping, by=c("indicator")) %>%   #Revised color mapping - loading from CSV to be changed instantly from there
  mutate(domain =
           case_when(
             domain == "Health and Well-being" ~ "Health and Wellbeing",
             TRUE ~ domain
           ),
         context = ifelse(is.na(context), "negative", context))

################################################################################
#Final Data to be used in the Application
################################################################################

 # usethis::use_data(Pak_Indicators_Data, overwrite = TRUE)

################################################################################
################################################################################

