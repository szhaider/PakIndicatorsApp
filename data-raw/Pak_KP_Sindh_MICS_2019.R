#################################################
#MICS Sindh 2019 and MICS KP 2019 combined
################################################################################

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
    year_1 = paste0(year,"-", source, " : ", "KP & Sindh"),
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

