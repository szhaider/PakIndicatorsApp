## code to prepare `Pak_Indicators_Data` dataset goes here

################################################################################
################################################################################
# Data Preparation for Pakistan Indicator App
################################################################################
# rm(list=ls())
################################################################################
#Loading required libraries
library(tidyverse)
library(vroom)
library(janitor)
library(sf)
library(utf8)
################################################################################
################################################################################

#Join tables for All
#Provinces - District Map
prov_dist <-
  Pak_Shapfiles %>%
  as_tibble() %>%
  distinct(province, district)


#Now no FR Distircts in New Shapefiles (Excluding from datasets)
fr_districts <- c("FR Bannu", "FR Dera Ismail Khan", "FR Kohat",
                  "FR Lakki Marwat", "FR Peshawar", "FR Tank")

################################################################################
#Pakistan Indicators legacy Dataset (2004-2018)
pak_ind <- read_delim("data-raw/rawdata/raw_indicators/pakistan_indicators.csv",
                      delim=",",
                      na = c("", "NA"),
                      locale = locale(encoding = "windows-1252"))

# GB, AJK and Kashmir arn't given in the .shp file , so dropping these from indicators dataset for consistency in the join later
#4308 rows aren't matching up due to these 3 locations.


#Pivoting long which is easier to work with
pak_ind <-
  pak_ind %>%
  filter(!District %in% c("Gilgit Baltistan", "Jammu & Kashmir", "AJ&K")) %>%
  pivot_longer(-(Province:Year), names_to = "indicator", values_to = "value") %>%
  clean_names() %>%
  filter(
    #Since FR districts aren't included in New shape files, Morover, they are part of bigger districts after 2018
    !district %in% fr_districts)

#i.e further data cleaning on the file and adding domain and source in it

pak_ind %>% group_by(year) %>% count(district, sort=T)
pak_ind %>% group_by(year, indicator) %>% count(district) %>% filter(n!=1)
#Chitral entry  repeats twice in 2019, data entry error (146*2)

#data %>% group_by(year) %>%  count(district) %>% filter(n != 146) # only chitral is an error for 2019.

chitral <-
  pak_ind %>%
  filter(district == "Chitral" & year == 2019) %>%
  slice(row_number(1:146))

#Since Kachhi (Balochistan) District is missing in 2019. I'm puttin it in with NA value for all indicators so coorect values popup on resprective districts in 2019
kachhi <-
  chitral %>%    #using chitral obs to create the same with NAs for Kachhi
  mutate(province = "Balochistan",
         district = "Kachhi",
         value = NA)

#Pakistan Indicators file
#Mutating the domain class of each indicator so user can select first domain than respective indicator

pak_ind <- pak_ind %>%
  filter(!(district == "Chitral" & year == 2019)) %>%
  rbind(chitral) %>%
  rbind(kachhi) %>%
  mutate_if(is.character, utf8_encode) %>%
  arrange(province, district) %>%
  mutate(year= as.character(year))

#Adding 6 New Karachi Districts (with same values on all) and removing Karachi City
pak_ind <-
  pak_ind %>%
  pivot_wider(id_cols = -c(province) ,
              names_from = district,
              values_from = value) %>%
  mutate(#6 new karachi districts
    `East Karachi` = `Karachi City`,
    `Malir Karachi` = `Karachi City`,
    `Korangi Karachi`  = `Karachi City`,
    `Central Karachi` = `Karachi City`,
    `South Karachi` = `Karachi City`,
    `West Karachi`  =  `Karachi City`) %>%
  mutate(#New district in Shapefile, Which are either not available in latest shapefile or bifurcations of old districts took place such as below
    `Chitral Lower` = Chitral,
    `Chitral Upper` = Chitral,
    `Kohistan Lower` = Kohistan,
    `Kohistan Upper` = Kohistan) %>%
  select(-`Karachi City`,
         -Chitral,
         -Kohistan) %>%
  mutate(#New Additions (Introducing as NAs)
    Chaman = NA,
    Duki = NA,
    Kohlu = NA,
    `Kolai Palas Kohistan` = NA,
    `Shaheed Sikandarabad` = NA) %>%

  pivot_longer( Awaran:`Shaheed Sikandarabad`,
                names_to = "district" ,
                values_to = "value",
                values_drop_na = FALSE)

pak_ind <-
  pak_ind  %>%
  mutate(district =
           case_when(
             district == "South Waziristan Agency" ~ "South Waziristan",
             district == "North Waziristan Agency" ~ "North Waziristan",
             district == "Mohmand Agency" ~ "Mohmand",
             district == "Orakzai Agency" ~ "Orakzai",
             district == "Kurram Agency" ~ "Kurram",
             district == "Khyber Agency" ~ "Khyber",
             district == "Bajaur Agency" ~ "Bajaur",
             district == "Malakand PA" ~ "Malakand",
             district == "Tando Allah Yar" ~ "Tando Allahyar",
             district == "Umerkot" ~ "Umer Kot",
             district == "Las Bela" ~ "Lasbela",
             TRUE ~ district))
#Checking if names of districts match in both datasets (Pak Ind - Shapefiles)
dist_shp <-
  Pak_Shapfiles %>%
  distinct(district) %>%
  arrange(district)

dist_ind <-
  pak_ind %>%
  # filter(!district %in% c("Gilgit Baltistan", "Jammu & Kashmir", "AJ&K")) %>%
  distinct(district)%>%
  arrange(district)

is.element(dist_ind$district, dist_shp$district)  #All Matched
is.element(dist_shp$district, dist_ind$district)  #All Matched

pak_ind <-
  pak_ind %>%
  left_join(prov_dist, by=c("district"))

#Reading in csv for domain names and source and definitions for Pakistan Indicators legacy data
domain <-
  read_csv("data-raw/rawdata/raw_indicators/domain_source_pak_indicators.csv", na = c("", ""))

domain <- domain %>%
  mutate(indicator_1 = str_to_title(indicator_1),
         units = replace_na(units,""))

#Joining domain + source + definition with the data
pak_ind <-
  left_join(pak_ind, domain, by="indicator")

pak_ind <-
  pak_ind %>%
  select(province:year, district, indicator_1, units,
         domain:definition, context, indicator,
         value)

#Creating a new variable for year + source filtering
pak_ind <-
  pak_ind %>%
  mutate(year_1 = year, source_1 = source) %>%
  unite(year_1, c("year_1", "source_1"), sep="-") %>%
  mutate(year_1 =
           case_when(year_1 == "2018-Census" ~  "2017-Census",
                     year_1 == "2017-Census" ~ "2018-Census",
                     TRUE ~ year_1)) %>%
  filter(source != "N/A") %>%
  mutate(positive = context=="positive") %>%
  mutate(negative = context== "negative") %>%
  mutate(district1 = paste(province,",", district))  %>%
  filter(year_1   != "2019-PSLM" &            #Previous 2019 PSLM (NAs) excluded so we can add new dataset
           year_1 != "2019-HIES/PSLM" &
           year_1 != "2018-HIES/PSLM") %>%

  filter(
    year_1 != "2019-MICS"   # Exclusing 2019-MICS so we can add 2019-MICS for KP and Sindh (5371 obs)
  ) %>%
  mutate(year = as.numeric(year))

################################################################################
#Will follow these distirct names for all other surveys (Changes made in Pak ind and shapefiles)
dist_to_follow <-
  pak_ind %>%
  distinct(district) %>%
  arrange(district)
################################################################################
# 2019 PSLM dataset

pslm_2019 <-
  readxl::read_excel("data-raw/rawdata/raw_indicators/2019PSLM_indicators_long2.xlsx", na= "") %>% #PSLM-2019.xlsx << Previously this version>>UPdated new version with employment label fixes
  mutate(year_1 = year, source_1 = source) %>%
  unite(year_1, c("year_1", "source_1"), sep="-") %>%
  mutate(district1 = paste(province, ",", district)) %>%
  mutate(context=
           case_when(
             indicator == "Open defecation (% of population)" ~ "negative",
             indicator == "Household size" ~ "negative",
             indicator == "Household dependency ratio" ~ "negative",
             indicator == "Household child dependency ratio" ~ "negative",
             indicator == "Household senior dependency ratio" ~ "negative",
             indicator == "Household members per room" ~ "negative",
             indicator == "Overcrowding (% of population)" ~ "negative",
             indicator == "Unpaid employment (% of total employment)" ~ "negative",
             indicator == "Child labor (% of children aged 10-17)" ~ "negative",
             indicator == "Unpaid employment, male (% of male employment)" ~ "negative",
             indicator == "Unpaid employment, female (% of female employment)" ~ "negative",
             indicator == "Diarrhea incidence (% of children under 5)" ~ "negative",
             TRUE ~ "positive"
           )) %>%
  mutate(positive = context == "positive") %>%
  mutate(negative = context == "negative") %>%
  arrange(year, district) %>%
  mutate(district =
           case_when(
             district == "South Waziristan Agency" ~ "South Waziristan",
             district == "North Waziristan Agency" ~ "North Waziristan",
             district == "Mohmand Agency" ~ "Mohmand",
             district == "Orakzai Agency" ~ "Orakzai",
             district == "Kurram Agency" ~ "Kurram",
             district == "Khyber Agency" ~ "Khyber",
             district == "Bajaur Agency" ~ "Bajaur",
             district == 'Kachhi/ Bolan' ~ 'Kachhi',
             district ==  'Kech/Turbat' ~ 'Kech',
             district ==  'Qilla Abdullah' ~ 'Killa Abdullah',
             district ==  'Qilla Saifullah' ~ 'Killa Saifullah',
             district ==  'Karachi Central' ~ 'Central Karachi',
             district ==  'Karachi East' ~ 'East Karachi',
             district ==  'Korangi' ~ 'Korangi Karachi',
             district ==  'Karachi Malir' ~ 'Malir Karachi',
             district ==  'Karachi South' ~ 'South Karachi',
             district == 'Karachi West' ~ 'West Karachi',
             district == 'Umerkot' ~ 'Umer Kot',
             district ==  'Tando Allah Yar' ~ 'Tando Allahyar',
             district ==  'Shaheed Sikandar Abad' ~ 'Shaheed Sikandarabad',
             district ==  'Malakand PA' ~ 'Malakand',
             district == 'Las Bela' ~ 'Lasbela',
             TRUE ~ district  ))

district <- pak_ind %>%
  distinct(district, province)

join_info <- pslm_2019 %>%
  distinct(indicator, units, source, domain, definition, year_1, context, positive, negative)

pslm_2019 <- pslm_2019 %>%
  pivot_wider(indicator, names_from = district, values_from = value) %>%
  mutate(Chagai = NA,
         'Jhal Magsi' = NA,
         Lehri = NA,
         Musakhel = NA,
         Panjgur = NA,
         Zhob = NA,
         Chaman = NA,
         `Kolai Palas Kohistan` = NA
  ) %>%   #Adding the districts as NAs in the PSLM, which are available in shapefile but  missing here, to match one to one on maps
  mutate(
    `Chitral Lower` = Chitral,
    `Chitral Upper` = Chitral,
    `Kohistan Lower` = Kohistan,
    `Kohistan Upper` = Kohistan
  ) %>%
  select(-Chitral , -Kohistan)

pslm_2019 <- pslm_2019 %>%
  pivot_longer(cols= Abbottabad:`Kohistan Upper`, names_to = "district", values_to = "value", values_drop_na = FALSE) %>%
  inner_join(join_info, by="indicator") %>%
  mutate(year = 2019,
         indicator_1 = indicator)  %>%

  inner_join(district, by = "district") %>%
  mutate(district1 = paste(province, ",", district)) %>%
  arrange(year, district)

# View(pslm_2019 %>% distinct(district))
is.element(dist_to_follow$district, unique(pslm_2019$district)) #All Matched
is.element(unique(pslm_2019$district), dist_to_follow$district) #All Matched

################################################################################
#Reading in 2018 Provisional Poverty Numbers

pov_2018 <- readxl::read_excel("data-raw/rawdata/raw_indicators/Povertyheadcount.xlsx", na="") %>%
  select(-Province, -Division, -msepovrate, -nIndividuals) %>%
  clean_names() %>%
  filter(!district %in% fr_districts)

pov_2018 <-
  pov_2018 %>%
  pivot_longer(-district,names_to = "indicator", values_to = "value") %>%
  arrange(indicator) %>%
  pivot_wider(names_from = district, values_from = value) %>%
  mutate( #6 new karachi districts
    `East Karachi` = `Karachi City`,
    `Malir Karachi` = `Karachi City`,
    `Korangi Karachi`  = `Karachi City`,
    `Central Karachi` = `Karachi City`,
    `South Karachi` = `Karachi City`,
    `West Karachi`  =  `Karachi City`) %>%
  mutate(#New district in Shapefile, Which are either not available in latest shapefile or bifurcations of old districts took place such as below
    `Chitral Lower` = Chitral,
    `Chitral Upper` = Chitral,
    `Kohistan Lower` = Kohistan,
    `Kohistan Upper` = Kohistan) %>%
  select(-`Karachi City`,
         -Chitral,
         -Kohistan) %>%
  mutate(
    Chaman = NA,
    Duki= NA,
    `Kolai Palas Kohistan` = NA,
    `Shaheed Sikandarabad` = NA
  ) %>%
  pivot_longer(
    cols = -indicator,
    names_to = "district", values_to = "value",
    values_drop_na = FALSE
  )

pov_2018 <-
  pov_2018 %>%
  mutate(district=
           case_when(
             district == "Las Bela" ~ "Lasbela",
             district == "Malakand PA" ~ "Malakand",
             district == "Tando Allah Yar" ~ "Tando Allahyar",
             district == "Umerkot" ~ "Umer Kot",
             TRUE ~ district
           ))

# pov_2018 %>% distinct(district) %>% View()
is.element(dist_to_follow$district ,pov_2018$district) #All Matched
is.element(unique(pov_2018$district), dist_to_follow$district) #All Matched

pov_2018 <-
  pov_2018 %>%
  left_join(prov_dist, by="district") %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  mutate('National poverty rank (N)' =
           rank(povrate, ties.method = "first", na.last = "keep")) %>%
  group_by(province) %>%
  mutate(`Provincial poverty rank (N)` =
           rank(povrate, ties.method = "first", na.last = "keep")) %>%
  mutate(year = 2018,
         district1 = paste(province, ",",district),
         povrate = povrate * 100) %>%
  rename('Poverty Rate (%)'=povrate, "Number of poor (1,000s)" = povertyheadcount) %>%
  select(district, province, year, everything()) %>%
  pivot_longer(
    "Number of poor (1,000s)":
    # "Poverty Rate (%)":
      "Provincial poverty rank (N)",
    names_to = "indicator" ,
    values_to = "value" ) %>%
  mutate(value = ifelse(indicator == "Number of poor (1,000s)", value/1000, value)) %>%
  left_join(domain, by="indicator") %>%
  mutate(year = 2018) %>%
  mutate(year_1 = year, source_1 = source) %>%
  unite(year_1, c("year_1", "source_1"), sep="-") %>%
  mutate(indicator_1 = indicator) %>%
  mutate(positive = context == "positive") %>%
  mutate(negative = context == "negative") %>%
  arrange(year, district) %>%
  mutate(indicator_1 = str_to_title(indicator))

################################################################################
#Reading in Nutrition Survey (NNS- 2018)
nns_2018 <- readxl::read_excel("data-raw/rawdata/raw_indicators/NNS-2018.xlsx")

nns_info <- nns_2018 %>%
  distinct(indicator, units, source, domain, definition)

nns_2018 <- nns_2018 %>%
  filter(district != "Gilgit",
         district != "Ghizer",
         district != "Hunza",
         district != "Nagar",
         district != "Shigar",
         district != "Ghanche",
         district != "Baltistan",
         district != "Kharmang",
         district != "Astore",
         district != "Muzaffarabad",
         district != "Neelum",
         district != "Hattian Bala",
         district != "Bagh",
         district != "Sudhnoti",
         district != "Poonch",
         district != "Haveli",
         district != "Bhimber",
         district != "Mirpur",
         district != "Kotli") %>%
  filter(!district %in% fr_districts) #Excluding FR Districts since not included in Shapefiles

nns_2018 <-
  nns_2018 %>%
  pivot_wider(indicator, names_from = district, values_from = value) %>%

  rename( #6 new karachi districts
    `East Karachi` = `Karachi East`,
    `Malir Karachi` = `Malir`,
    `Korangi Karachi`  = `Korangi`,
    `Central Karachi` = `Karachi Central`,
    `South Karachi` = `Karachi South`,
    `West Karachi`  =  `Karachi West`) %>%
  mutate(#New district in Shapefile, Which are either not available in latest shapefile or bifurcations of old districts took place such as below
    `Chitral Lower` = Chitral,
    `Chitral Upper` = Chitral,
    `Kohistan Lower` = Kohistan,
    `Kohistan Upper` = Kohistan) %>%
  select(
    -Chitral,
    -Kohistan) %>%
  mutate(Abbottabad = NA,
         Chaman = NA,
         Duki = NA,
         Haripur = NA,
         `Kolai Palas Kohistan` = NA,
         Mansehra = NA,
         'North Waziristan' = NA,
         `Shaheed Sikandarabad` = NA,
         'South Waziristan' = NA
  )

nns_2018 <-
  nns_2018 %>%
  pivot_longer(`Upper Dir`:`South Waziristan`, names_to = "district", values_to = "value") %>%
  mutate(district =
           case_when(
             district == "Las Bela" ~ "Lasbela",
             district == "Malakand PA" ~ "Malakand",
             district == "Tando Allah Yar" ~ "Tando Allahyar",
             district == "Umerkot" ~ "Umer Kot",
             TRUE ~ district))

is.element(unique(dist_to_follow$district), unique(nns_2018$district)) #All matched
is.element(unique(nns_2018$district), unique(dist_to_follow$district)) #All matched

nns_2018 <- nns_2018 %>%
  inner_join(nns_info, by="indicator") %>%
  mutate(year = 2018,
         indicator_1 = indicator) %>%
  inner_join(prov_dist, by = "district") %>%
  mutate(district1 = paste(province, ",", district)) %>%
  mutate(year_1 = year, source_1 = source) %>%
  unite(year_1, c("year_1", "source_1"), sep="-") %>%
  mutate(context = "negative") %>%
  mutate(positive = context != "negative") %>%
  mutate(negative = context == "positive") %>%
  arrange(year, district)

################################################################################
#MICS Balochistan 2020

dis_except_balochistan <-
  pak_ind %>%
  filter(province != "Balochistan") %>%
  distinct(province, district)

MICS_Bal_2020 <-
  readxl::read_excel("data-raw/rawdata/raw_indicators/MICS_Latest/MICS_Balochistan/MICS_Balochistan_long.xls")

domain_balochistan <- MICS_Bal_2020 %>%
  distinct(domain, indicator)


#Adding 3 additional districts as NAs
MICS_Bal_2020 <-
MICS_Bal_2020 %>%
  pivot_wider(names_from = district , values_from = value) %>%
  mutate(Duki = NA,
         Chaman = NA,
         `Shaheed Sikandarabad` = NA) %>%
  pivot_longer(Awaran:`Shaheed Sikandarabad`
               ,names_to = "district",
               values_to = "value")

MICS_Bal_2020 <-
  MICS_Bal_2020 %>%
  pivot_wider(values_from =value , names_from = indicator, -domain) %>%
  bind_rows(dis_except_balochistan) %>%
  mutate(year =
           case_when(
             is.na(year) ~ 2020,
             TRUE ~ year
           )) %>%
  mutate(source =
           case_when(
             is.na(source) ~ 'MICS',
             TRUE ~ source
           )) %>%
  pivot_longer(cols = -c(province, district, year, source) ,
               names_to = "indicator", values_to = "value") %>%
  mutate(district1 = paste0(province," , ", district),
         year_1 = paste0("Balochistan", " : ", year,"-", source),  #
         definition = indicator,
         indicator_1 = indicator
  ) %>%
  mutate(context =
           case_when(
             indicator == "Open defecation (% of population)" ~ "negative",
             indicator == "Diarrhea incidence (% of children under 5 who had diarrhea during a 2-week recal" ~ "negative",
             TRUE ~ "positive"
           )) %>%
  mutate(positive = context != "positive") %>%
  mutate(negative = context == "negative")  %>%
  mutate(district =
           case_when(
             district == 'Chaghi' ~ 'Chagai',
             district == 'Kachhi (Bolan)' ~ 'Kachhi',
             district == 'Kech (Turbat)' ~ 'Kech',
             district == 'Las Bela' ~ 'Lasbela',
             district == 'Sibbi' ~ 'Sibi',
             district == 'SohbatPur' ~ 'Sohbatpur',
             TRUE ~ district
           ),
         units=
           case_when(
         indicator == 'Average age at first birth of women' ~ "",
         TRUE ~ "%"
           )) %>%
  left_join(domain_balochistan, by= "indicator")

is.element(unique(dist_to_follow$district), unique(MICS_Bal_2020$district)) #All Matched
is.element(unique(MICS_Bal_2020$district), unique(dist_to_follow$district)) #All Matched

###############################################################################
#MICS Sindh 2019 and MICS KP 2019 combined

dis_other_than_kpandsindh <-
  pak_ind %>%
  filter(province != "Sindh" & province != "Khyber Pakhtunkhwa") %>%
  distinct(province, district)

MICS_Sindh_2019_raw <-
  readxl::read_excel("data-raw/rawdata/raw_indicators/MICS_Latest/MICS_Sindh/MICS_Sindh_long.xls")

domain_sindh <-
  MICS_Sindh_2019_raw %>%
  distinct(domain, indicator)  ##Domain mapping prepared in excel for Sindh, using the same for KP indicator -domain mapping

MICS_KP_2019_raw <-
  readxl::read_excel("data-raw/rawdata/raw_indicators/MICS_Latest/MICS_KP/MICS_KP_long.xls") %>%
  left_join(domain_sindh, by="indicator")  #Using the same domain-indicator mapping in KP

MICS_Sindh_2019 <-
  MICS_Sindh_2019_raw %>%
  pivot_wider(values_from =value , names_from = indicator, -domain)

MICS_KP_2019 <-
  MICS_KP_2019_raw %>%
  pivot_wider(values_from =value , names_from = indicator, -domain)

#Binding KP and Sindh MICS since same year (2019)

MICS_2019_KP_Sindh_combined <-
  MICS_Sindh_2019 %>%
  mutate(year = as.numeric(year)) %>%
  bind_rows(MICS_KP_2019) %>%
  bind_rows(dis_other_than_kpandsindh %>% select(district)) %>%
  mutate(year =
           case_when(
             is.na(year) ~ 2019,
             TRUE ~ year
           )) %>%
  mutate(source =
           case_when(
             is.na(source) ~ 'MICS',
             TRUE ~ source
           )) %>%
  mutate(district =
           case_when(
             district == 'Umerkot' ~ 'Umer Kot',
             district == 'Shahdad Kot' ~ 'Qambar Shahdadkot',
             district == 'Tando Allah Yar' ~'Tando Allahyar',
             district == 'Tando Muhmmad Khan' ~ 'Tando Muhammad Khan',
             district == 'Abbotabad' ~ 'Abbottabad',
             district == 'Bajor' ~ 'Bajaur',
             district == 'Hari Pur' ~ 'Haripur',
             district == 'Kuram' ~ 'Kurram',
             district == 'Laki Marwat' ~ 'Lakki Marwat',
             district == 'Malakand PA' ~ 'Malakand',
             district == 'Mohmind' ~ 'Mohmand',
             district == 'Nowshehra' ~ 'Nowshera',
             district == 'Torghar' ~ 'Tor Ghar',
             TRUE ~ district
           ),
         units = "") %>%
  pivot_longer(cols = -c(district, year, source, units) ,
               names_to = "indicator",
               values_to = "value") %>%
  pivot_wider(id_cols = c(year, source, units, indicator),
              names_from = district,
              values_from = value) %>%
  mutate(Chaman = NA,
         Duki = NA,
         `Shaheed Sikandarabad` = NA,
         `Kolai Palas Kohistan` = NA) %>%
  mutate(
    `Chitral Lower` = Chitral,
    `Chitral Upper` = Chitral,
    `Kohistan Lower` = Kohistan,
    `Kohistan Upper` = Kohistan) %>%
  select(
    -Chitral,
    -Kohistan) %>%
  rename( #6 new karachi districts
    `East Karachi` = `Karachi East`,
    `Malir Karachi` = `Karachi Malir`,
    `Korangi Karachi`  = `Karachi Korangi`,
    `Central Karachi` = `Karachi Central`,
    `South Karachi` = `Karachi South`,
    `West Karachi`  =  `Karachi West`) %>%

  pivot_longer(cols= Badin:`Kohistan Upper`,
               names_to = "district",
               values_to = "value",
               values_drop_na = FALSE) %>%
  left_join(domain_sindh ,by="indicator") %>%
  left_join(prov_dist, by="district") %>%
  mutate(
    district1 = paste0(province," , ", district),
    year_1 = paste0("KP & Sindh : ",year,"-", source),
    definition = indicator,
    indicator_1 = indicator
  ) %>%
  mutate(context =
           case_when(
             indicator == "Open defecation (% of population)" ~ "negative",
             indicator == "Diarrhea incidence (% of children under 5 who had
             diarrhea during a 2-week recal" ~ "negative",
             TRUE ~ "positive"
           )) %>%
  mutate(positive = context == "positive") %>%
  mutate(negative = context == "negative") %>%
  mutate(units=
  case_when(
    indicator == 'Average age at first birth of women' ~ "",
    TRUE ~ "%"
  ))

is.element(unique(dist_to_follow$district), unique(MICS_2019_KP_Sindh_combined$district)) #All matched
is.element(unique(MICS_2019_KP_Sindh_combined$district), unique(dist_to_follow$district)) #All matched

################################################################################
################################################################################
#Caution rerunning this file (Otherwise will have to redo contextual  color  mapping)

#For revised color mapping (For making amends in units)

# Pak_Indicators_Data %>%
#   distinct(indicator, context) %>% # , indicator_1, units, source, definition, context
#   write.csv("data-raw/rawdata/raw_indicators/color_mapping_revised.csv")

################################################################################
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

