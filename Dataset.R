##### Dataset #####

# Health outcome
ts_dm <- read_sas("/userdata06/room051/working/temp/ts_dm.sas7bdat", catalog_file=NULL) 
ts_ad <- read_sas("/userdata06/room051/working/temp/ts_admit.sas7bdat", catalog_file=NULL)
ts_dt <- read_sas("/userdata06/room051/working/temp/ts_death.sas7bdat", catalog_file=NULL)

# Exposure
temp <- read_excel("/userdata06/room051/data_source/user_data/Exposure_dataset.xlsx",sheet = "exp") %>%
  rename(sido_kr=sido, SIDO=sido_bjd) %>% 
  mutate(date = ymd(paste(year,month,day,sep = "/")),
         temp_mean_F = celsius.to.fahrenheit(temp_mean),
         wind_mph = 2.23694*wind,
         WC_F = ifelse(temp_mean_F <=50 & wind_mph >3, 
                       35.74 + 0.6215*temp_mean_F - 35.75*(wind_mph^0.16) + 0.4275*temp_mean_F*(wind_mph^0.16), NA),
         WC_C = fahrenheit.to.celsius(WC_F),
         HI_F = heat.index(t = temp_mean_F, rh = humidity, temperature.metric = "fahrenheit", output.metric = 'fahrenheit', round=3),
         HI_C = heat.index(t = temp_mean, rh = humidity, temperature.metric = "celsius", output.metric = 'celsius', round=3),
         AT_hw = ifelse(is.na(WC_C),HI_C,WC_C)) %>%
  select(-c(year,month,day))

df_dt <- merge(caco_dt, temp, by=c('SIDO','date'))
df_dm <- merge(caco_dm, temp, by=c('SIDO','date')) 
df_ad <- merge(caco_ad, temp, by=c('SIDO','date'))


# Merge
df_dm <- merge(temp, ts_dm, by=c('SIDO','date'), all.x=T) %>%
  mutate(city = as.factor(SIDO),
         month = as.factor(month(date)),
         year = as.factor(format(date, format="%Y")),
         dow = as.factor(weekdays(date)),
         stratum = as.factor(city:year:month:dow),
         across(DM_def_1_all:hosp_long_1, ~if_else(is.na(.), 0, .))) %>% 
  arrange(SIDO, date)

df_dt <- merge(temp, ts_dt, by=c('SIDO','date'), all.x=T) %>%
  mutate(city = as.factor(SIDO),
         month = as.factor(month(date)),
         year = as.factor(format(date, format="%Y")),
         dow = as.factor(weekdays(date)),
         stratum = as.factor(city:year:month:dow),
         across(p1_DM_def_1_all:p0_CCI_2, ~if_else(is.na(.), 0, .))) %>% 
  arrange(SIDO, date)

df_ad <- merge(temp, ts_ad, by=c('SIDO','date'), all.x=T) %>%
  mutate(city = as.factor(SIDO),
         month = as.factor(month(date)),
         year = as.factor(format(date, format="%Y")),
         dow = as.factor(weekdays(date)),
         stratum = as.factor(city:year:month:dow),
         across(p1_DM_def_1_all:p0_hosp_long_1, ~if_else(is.na(.), 0, .))) %>% 
  arrange(SIDO, date)