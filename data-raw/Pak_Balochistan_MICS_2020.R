################################################################################

#MICS Balochistan 2020

################################################################################

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
         year_1 = paste0(year,"-", source, " : ", "Balochistan"),
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
