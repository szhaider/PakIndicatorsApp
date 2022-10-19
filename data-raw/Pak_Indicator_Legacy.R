
################################################################################
################################################################################

#Pakistan Indicators legacy Dataset (2004-2018)

################################################################################
################################################################################

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
