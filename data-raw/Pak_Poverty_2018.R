
################################################################################
#Reading in 2018 Provisional Poverty Numbers
################################################################################

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

