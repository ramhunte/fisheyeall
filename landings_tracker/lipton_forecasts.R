# Load required packages
library(dplyr)
library(reshape2)
library(ggplot2)

# Deflator
# This package includes Q1 and Q2 2020
# devtools::install_github("sboysel/fredr")
library(fredr)
fredr_set_key('a5d92deb507a05eb5a7f84355a02f602')
fred_gdpdefl <- fredr(
  series_id = "GDPDEF",
  observation_start = as.Date("2015-01-01")
) 

defl_adj <- mutate(fred_gdpdefl, LANDING_YEAR = format(date,"%Y")) %>%
  group_by(LANDING_YEAR) %>%
  summarise(defl = mean(value)) %>%
  mutate(DEFL = defl/defl[LANDING_YEAR == 2019])

write.table(fredr_series(series_id = "GDPDEF"), file = 'gdp_defl_details.csv', sep = ',')

# set current month to determine which months should be used "as-is" and which should be forecast
currentmonth <- 8

# pull in data from datapull.R
comp_dat_raw_load <- readRDS('comp_dat_raw.RDS') %>%
  merge(fred_gdpdefl) %>%
  mutate(EXVESSEL_REVENUE = EXVESSEL_REVENUE/DEFL) %>%
  select(-DEFL, -defl)

# Calculate baseline for each fishery/month - mean, min, max
monthly_rev <- group_by(comp_dat_raw_load, LANDING_MONTH, SPECIES_GROUP, LANDING_YEAR) %>%
  summarise(REV = sum(EXVESSEL_REVENUE)) %>%
  mutate(period = ifelse(LANDING_YEAR < 2020, 'baseline', '2020')) %>%
  group_by(LANDING_MONTH, SPECIES_GROUP, period) %>%
  summarise(mean = mean(REV),
            min = min(REV),
            max = max(REV))

# just pull the baseline numbers
baseline_monthly_rev <- subset(monthly_rev, period == 'baseline') %>%
  rename(baseline_avg = mean,
         baseline_min = min,
         baseline_max = max) %>%
  select(LANDING_MONTH, SPECIES_GROUP, baseline_avg, baseline_min, baseline_max)

# just pull the current numbers
current_monthly_rev <- subset(monthly_rev, period == '2020' & LANDING_MONTH < currentmonth) %>%
  rename(revenue = mean) %>%
  select(LANDING_MONTH, SPECIES_GROUP, revenue) %>%
  mutate(source = 'actual') %>%
  mutate(propchange = 1)

# calculate the change compared to baseline mean, using march:current month -1
change_by_species <- subset(comp_dat_raw_load, LANDING_MONTH >= 3 & LANDING_MONTH < currentmonth) %>%
  group_by(SPECIES_GROUP, LANDING_YEAR) %>%
  summarise(REV = sum(EXVESSEL_REVENUE)) %>%
  mutate(period = ifelse(LANDING_YEAR < 2020, 'baseline', '2020')) %>%
  group_by(SPECIES_GROUP, period) %>%
  summarise(mean = mean(REV)) %>%
  dcast(SPECIES_GROUP ~ period, value.var = 'mean', fill = 0) %>%
  mutate(propchange = (`2020`/baseline - 1)) %>%
  select(SPECIES_GROUP, propchange)

# do the forecast - 2020
forecasts_2020 <- subset(baseline_monthly_rev, LANDING_MONTH >= currentmonth) %>%
  left_join(change_by_species) %>%
  mutate(revenue = (propchange + 1) * baseline_avg,
         min_revenue = (propchange + 1) * baseline_min,
         max_revenue = (propchange + 1) * baseline_max) %>%
  mutate(source = 'forecast') %>%
  select(LANDING_MONTH, SPECIES_GROUP, propchange, revenue, min_revenue, max_revenue, source)

# combine the 2020 data
full_data_set_2020 <- bind_rows(current_monthly_rev, forecasts_2020) %>%
  left_join(baseline_monthly_rev) %>%
  mutate(LANDING_YEAR = 2020) %>%
  select(LANDING_YEAR, LANDING_MONTH, SPECIES_GROUP, revenue, min_revenue, max_revenue, source, propchange, baseline_avg, baseline_min, baseline_max)

# do the forecasts - 2021
forecasts_2021 <- baseline_monthly_rev %>%
  left_join(change_by_species) %>%
  mutate(revenue = (propchange + 1) * baseline_avg,
         min_revenue = (propchange + 1) * baseline_min,
         max_revenue = (propchange + 1) * baseline_max) %>%
  mutate(source = 'forecast') %>%
  select(LANDING_MONTH, SPECIES_GROUP, propchange, revenue, min_revenue, max_revenue, source)

# combine the 2021 data
full_data_set_2021 <- forecasts_2021 %>%
  left_join(baseline_monthly_rev) %>%
  mutate(LANDING_YEAR = 2021) %>%
  select(LANDING_YEAR, LANDING_MONTH, SPECIES_GROUP, revenue, min_revenue, max_revenue, source, propchange, baseline_avg, baseline_min, baseline_max)

# combine the data
full_data_set <- bind_rows(current_monthly_rev, forecasts) %>%
  left_join(baseline_monthly_rev)

subset(full_data_set, SPECIES_GROUP != 'DUNGENESS CRAB' & LANDING_YEAR == 2020) %>%
ggplot(aes(x = LANDING_MONTH, y = revenue)) +
  geom_line(aes(color = SPECIES_GROUP, linetype = source))

write.table(full_data_set, file = 'R:/Confidential/Research/COVID-19/Lipton forecasts/forecast_first_draft_with_mean.csv', sep = ',', row.names = F)



