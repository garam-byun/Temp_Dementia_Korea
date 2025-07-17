# Prepare analytic dataset for dementia admissions

# Required packages
library(haven)
library(readxl)
library(lubridate)
library(dplyr)
library(weathermetrics)  # for heat.index, temp conversion

# Load health outcomes 
ts_dm <- read_sas("/userdata06/room051/working/temp/ts_dm.sas7bdat", catalog_file=NULL)

# Load exposure dataset
# Province (SIDO)–level daily time-series data for temperature, humidity, PM10, heatwave, and coldspell, etc.
temp <- read_excel("data/raw/Exposure_dataset.xlsx", sheet = "exp") %>%
  mutate(
    date = ymd(paste(year, month, day, sep = "/")),
    temp_mean_F = celsius.to.fahrenheit(temp_mean),
    wind_mph = 2.23694 * wind,
    WC_F = ifelse(temp_mean_F <= 50 & wind_mph > 3,
                  35.74 + 0.6215 * temp_mean_F - 35.75 * (wind_mph^0.16) + 
                    0.4275 * temp_mean_F * (wind_mph^0.16), NA),
    WC_C = fahrenheit.to.celsius(WC_F),
    HI_F = heat.index(t = temp_mean_F, rh = humidity, temperature.metric = "fahrenheit", output.metric = "fahrenheit"),
    HI_C = heat.index(t = temp_mean, rh = humidity, temperature.metric = "celsius", output.metric = "celsius"),
    AT_hw = ifelse(is.na(WC_C), HI_C, WC_C)
  ) %>%
  select(-year, -month, -day)

# Merge exposure with health outcomes
df_dm <- merge(temp, ts_dm, by=c('SIDO','date'), all.x=T) %>%
  mutate(city = as.factor(SIDO),
         month = as.factor(month(date)),
         year = as.factor(format(date, format="%Y")),
         dow = as.factor(weekdays(date)),
         stratum = as.factor(city:year:month:dow),
         across(DM_def_1_all:climate_2, ~if_else(is.na(.), 0, .))) %>% 
  arrange(SIDO, date)

# Save for analysis
save(df_dm, temp, file = "data/input_data.RData")
