## code to prepare `Pak_Shapfiles` dataset goes here

# rm(list=ls())
################################################################################
# library(sf)
# library(tidyverse)
# library(janitor)
################################################################################
#Pakistan Shape file for app input

#OLder Shape files 2018
# pak_shp <-
# st_read("data-raw/rawdata/PAK_SHP_2018/pakistan_indicators.shp")

#Latest OCHA Shape files - Sep 2022
pak_shp <-
  st_read("data-raw/rawdata/PAK_SHP_2022/pak_adm_wfp_20220909_shp/pak_admbnda_adm2_wfp_20220909.shp")

################################################################################
#Preparing Shapefiles for the app (136 Districts) (6 new polygons for Karachi City)
Pak_Shapfiles <-
  pak_shp %>%
  st_as_sf() %>%
  janitor::clean_names() %>%
  select(district = adm2_en,
         # district_code = adm2_pcode,
         province = adm1_en,
         # province_code = adm1_pcode,
         geometry) %>%
  filter(province != "Azad Kashmir" & province != "Gilgit Baltistan") %>%  #AJK AND GB Excluded
  arrange(district) %>%
  sf::st_transform(crs=4326) %>%
  mutate(district =
           case_when(
             district == "D. I. Khan" ~ "Dera Ismail Khan" ,
             district == "Leiah" ~ "Layyah",
             district == "Kambar Shahdad Kot" ~ "Qambar Shahdadkot",
             district == "Shaheed Benazir Abad" ~ "Shaheed Benazirabad",
             district == "Sherani" ~ "Sheerani",
             # district == "" ~ "",
             # district == "" ~ "",
             # district == "" ~ "",


             TRUE ~ district
           ))
################################################################################

#Shapefile Documented in ./R/data.R (roxygen - ~.R/)

################################################################################
##run this script to replace the old shapefiles in the .~/data/

# usethis::use_data(Pak_Shapfiles, overwrite = TRUE)

################################################################################

