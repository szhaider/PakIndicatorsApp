
############################################################################

# 2019 PSLM dataset

################################################################################

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

