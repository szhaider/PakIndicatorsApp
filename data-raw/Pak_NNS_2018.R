

################################################################################
#Reading in Nutrition Survey (NNS- 2018)
################################################################################

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
