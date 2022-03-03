library(dplyr)
library(data.table)
source("helperfns.R")
library(odbc)
library(getPass)
library(fredr)

# pacfin <- DBI::dbConnect(odbc::odbc(),
#                          host   = "pacfindb.psmfc.org",
#                          UID    = getPass('pacfin username'),
#                          PWD    = getPass('pacfin password'),
#                          dsn    = 'pacfin',
#                          port   = 2045)

# Deflator #####
fredr_set_key('a5d92deb507a05eb5a7f84355a02f602')
fred_gdpdefl <- fredr(
  series_id = "GDPDEF",
  observation_start = as.Date("2015-01-01")
) 

defl_adj <- mutate(fred_gdpdefl, LANDING_YEAR = format(date,"%Y")) %>%
  group_by(LANDING_YEAR) %>%
  summarise(defl = mean(value)) %>%
  mutate(DEFL = defl/defl[LANDING_YEAR == 2020]) %>%
  select(-defl)

## ------------------------------------- Region-wide statistics ---------------------------------------------- ##

# Some notes - 
# (1) No action needed; noting in case question about numbers diff than tracker: In the landings tracker we lump 
# 'OTHER CRAB', 'ANCHOVY', 'SARDINE', 'OTHER COASTAL PELAGIC' into other species. But I think for these 
# purposes it is better to leave them as is. Also there are 21 CA whiting tickets. 
# We removed these from the landings tracker, but think they are included here. 
# Landings tracker removes a few outliers, but I don't think it is worth it here.


# Region-wide change in ex-vessel revenue & landed weight
comp_dat_raw_load <- readRDS('comp_dat_raw.RDS') %>%
  # Decided not to implement the disaster years because it doesn't work same way here
  # I think we would need to find 5 yr median for each fishery then add together to implement
  # Remove diaster years from the baseline for crab. For sardine I think we need
  # to remove completely because if 0 sardine in baseline then comparing to 2020 w sardine wouldn't make sense (I think)
  # mutate(excl = case_when(grepl('CRAB', SPECIES_GROUP) & LANDING_YEAR %in% c(2015,2016) ~ 1,
  #                         SPECIES_GROUP == 'SARDINE' ~ 1,
  #                         T ~ 0)) %>%
  # I think I would remove shellfish like we discussed (ie. too nuanced)
  filter(SPECIES_GROUP != 'SHELLFISH', LANDING_YEAR > 2014, AGENCY_CODE != 'F') %>% #, excl == 0) %>%
  merge(defl_adj) %>%
  mutate(EXVESSEL_REVENUE = EXVESSEL_REVENUE/DEFL) %>%
  select(-DEFL) #, -excl)

region_state_stats <- comp_dat_raw_load %>%
  mutate(AGENCY_CODE = 'All') %>% # add agency_code = Region for region-wide stats
  rbind(comp_dat_raw_load) %>%
  filter(LANDING_MONTH < 8) %>%
  mutate(month = month.abb[LANDING_MONTH]) %>%
  group_by(LANDING_YEAR, month, AGENCY_CODE) %>% 
  summarise(rev = sum(EXVESSEL_REVENUE)) %>% # sum to year-month-state level
  reshape2::dcast(LANDING_YEAR + AGENCY_CODE ~ month,
                  value.var = "rev") %>%
  mutate(JanJuly = Jan + Feb + Mar + Apr + May + Jun + Jul,
         JanMay = Jan + Feb + Mar + Apr + May,
         MarMay = Mar + Apr + May, 
         MarJuly = Mar + Apr + May + Jun + Jul) %>% # sum across months ranges for each individual year, then take median
  select(LANDING_YEAR, AGENCY_CODE, JanMay, JanJuly, MarMay, MarJuly, Jan, Feb, Mar, Apr, May, Jun, Jul) %>%
  reshape2::melt(c('LANDING_YEAR', 'AGENCY_CODE')) %>%
  mutate(period = ifelse(LANDING_YEAR < 2020, 'baseline', 'current')) %>%
  rename(month_range = variable) %>%
  group_by(period, AGENCY_CODE, month_range) %>%
  summarise(median = median(value)) %>%
  reshape2::dcast(AGENCY_CODE + month_range ~ period,
                  value.var = "median") %>%
  mutate(percchange = round((current/baseline - 1)*100,0)) %>%
  reshape2::dcast(AGENCY_CODE ~ month_range,
                  value.var = 'percchange') %>%
  rename(State = AGENCY_CODE) %>%
  mutate(Fishery = 'All',
         Description = '% change from 5-year median',
         Metric = 'Exvessel revenue',
         State = ifelse(State == 'C', 'California', ifelse(State == 'O', 'Oregon', ifelse(State == 'W', 'Washington', State)))) %>%
  select(State, Fishery, Description, Metric, JanMay, JanJuly, MarMay, MarJuly, Jan, Feb, Mar, Apr, May, Jun, Jul) %>%
  mutate(state_prop1 = NA,
         N = NA)

## ------------------------------------- Fishery-level statistics ---------------------------------------------- ##
# Formatting data frame for Marie #####
comp_dat_covid_app <- readRDS("comp_dat_covidapp.RDS") %>%
  data.table()
setkey(comp_dat_covid_app, State, Species)
# split up the month and other filters befor joining to reduce size of df 
addlfilters <- readRDS("addlfilters.RDS")
othr_filter <- select(addlfilters, -c(month_prop, select_month)) %>% distinct() %>%
  data.table()
setkey(othr_filter, State, Species)

data <- othr_filter[comp_dat_covid_app, on = c('State','Species')] %>%
  data.table()
# If is.na(Value) then confidential

fishery_state_stats <- data %>%
  filter(Cumulative == 'Y', Year %in% c('2020', 'Baseline'), Dates_as_char %in% c('01','02','03','04','05', '06', '07'), Interval == 'Monthly', 
         Statistic == 'Total', State != 'At-sea') %>%
  mutate(month = case_when(
    Dates_as_char == '01' ~ 'Jan_cum',
    Dates_as_char == '02' ~ 'Feb_cum',
    Dates_as_char == '03' ~ 'Mar_cum',
    Dates_as_char == '04' ~ 'Apr_cum',
    Dates_as_char == '05' ~ 'May_cum',
    Dates_as_char == '06' ~ 'Jun_cum',
    Dates_as_char == '07' ~ 'Jul_cum',
    T ~ 'error')) %>%
  reshape2::dcast(State + Species + Year + state_prop1 + Metric ~ month,
                  value.var = "Value") %>%
  mutate(JanMay = May_cum,
         JanJul = Jul_cum,
         MarMay = May_cum - Feb_cum,
         MarJul = Jul_cum - Feb_cum,
         Jul = Jul_cum - Jun_cum) %>%
  mutate(period = ifelse(Year != 2020, 'baseline', 'current')) %>%
  rename(Fishery = Species) %>%
  select(period, State, Fishery, Metric, state_prop1, JanMay, JanJul, MarMay, MarJul, Jul) %>%
  reshape2::melt(c('period', 'State', 'Fishery', 'Metric', 'state_prop1')) %>%
  rename(month_range = variable) %>%
  reshape2::dcast(State + Fishery + Metric + state_prop1 + month_range ~ period,
                  value.var = "value") %>%
  mutate(percchange = round((current/baseline - 1)*100,0)) %>%
  reshape2::dcast(State + Fishery + Metric + state_prop1 ~ month_range,
                  value.var = 'percchange') %>%
  mutate(Description = '% change from 5-year median') %>%
  select(State, Fishery, Description, Metric, JanMay, JanJul, MarMay, MarJul, Jul, state_prop1)


# ---------"fishery importance" variables -------- #

# number of unique vessels by year (based on 5-year median)
comp_dat_all <- readRDS("comp_dat_all.RDS")

participants <- comp_dat_all %>%
  filter(!is.na(VESSEL_NUM), YEAR != 2020) %>%
  group_by(YEAR, SPECIES_GROUP, AGENCY_CODE) %>%
  summarise(num_vss = length(unique(VESSEL_NUM))) %>%
  group_by(SPECIES_GROUP, AGENCY_CODE) %>%
  summarise(N = median(num_vss)) %>%
  ungroup() %>%
  rename(Fishery = SPECIES_GROUP,
         State = AGENCY_CODE) %>%
  mutate(Fishery = convert_sp(Fishery),
         Fishery = as.factor(Fishery),
         State = convert_state(State))

final_fishery <- merge(fishery_state_stats, participants, by = c('State', 'Fishery'))

final_dataset <- rbind(region_state_stats, final_fishery) %>%
  filter(Fishery %in% c('All', 'All non-whiting groundfish', 'Dungeness crab', 'Non-whiting groundfish: Fixed gear-non-nearshore',
                        'Non-whiting groundfish: IFQ-bottom trawl', 'Salmon' ,'Shrimp', 'Market squid'))
write.table(final_dataset, file = 'S:/Research/COVID-19/Rita request 2020_07_21/2020toBaselineComparisons_final.csv', sep = ',', row.names = F)

perchange_whiting <- data %>%
  filter(Cumulative == 'Y', Year %in% c('2020', 'Baseline'), grepl('Whiting', Species), grepl('07', Dates_as_char), Interval == 'Monthly', Statistic == 'Total') %>%
  mutate(Year = ifelse(Year == '2020', 'current', 'Baseline')) %>%
  reshape2::dcast(State + Species + Metric  ~ Year,
                  value.var = "Value") %>%
  mutate(percchange = round((current - Baseline)/Baseline*100,0)) 

perchange_whiting_check <- data %>%
  filter(Cumulative == 'N', Year %in% c('2020', 'Baseline'), State == 'All', grepl('Whiting', Species), Dates < 8, Metric %in% c('Exvessel revenue', 'Landed weight (mt)'), Interval == 'Monthly', Statistic == 'Total') %>%
  group_by(Year, Species, Metric) %>%
  summarise(val = sum(Value, na.rm = T)) %>%
  reshape2::dcast(Species + Metric  ~ Year,
                  value.var = "val") %>%
  mutate(percchange = round((`2020` - Baseline)/Baseline*100,0)) 

######################################################################################################################################

## -------------------------------- Exploratory tables & analysis -------------------------------------------------------- ##

# Percent change for each month at fishery-state level. 
fishery_state_month <- data %>%
  filter(Cumulative == 'N', Year %in% c('2020', 'Baseline'), LANDING_MONTH2 %in% c('01','02','03','04','05'), Interval == 'Monthly',
         Statistic == 'Total', State != 'At-sea') %>%
  mutate(month = case_when(
    LANDING_MONTH2 == '01' ~ 'Jan',
    LANDING_MONTH2 == '02' ~ 'Feb',
    LANDING_MONTH2 == '03' ~ 'Mar',
    LANDING_MONTH2 == '04' ~ 'Apr',
    LANDING_MONTH2 == '05' ~ 'May',
    T ~ 'error')) %>%
  mutate(period = ifelse(Year != 2020, 'baseline', 'current')) %>%
  rename(Fishery = Species) %>%
  select(period, State, Fishery, Metric, state_prop1, month, Value) %>%
  distinct() %>%
  reshape2::dcast(State + Fishery + Metric + state_prop1 + month ~ period,
                  value.var = "Value") %>%
  mutate(percchange = round((current/baseline - 1)*100,0)) %>%
  reshape2::dcast(State + Fishery + Metric + state_prop1 ~ month,
                  value.var = 'percchange') %>%
  mutate(Description = '% change from 5-year median') %>%
  select(State, Fishery, Description, Metric, Jan, Feb, Mar, Apr, May)

# Examine contribution of each fishery to region & state
region_tot <- comp_dat_raw_load %>%
  mutate(AGENCY_CODE = 'All') %>% # add agency_code = Region for region-wide stats
  rbind(comp_dat_raw_load) %>%
  filter(LANDING_MONTH < 6, LANDING_YEAR == 2020) %>%
  mutate(month = month.abb[LANDING_MONTH]) %>%
  group_by(LANDING_YEAR, AGENCY_CODE, month) %>% 
  summarise(rev = sum(EXVESSEL_REVENUE)) %>% # sum to year-month-state level
  reshape2::dcast(LANDING_YEAR + AGENCY_CODE ~ month,
                  value.var = "rev") %>%
  mutate(JanMay = Jan + Feb + Mar + Apr + May,
         FebMay = Feb + Mar + Apr + May,
         MarMay = Mar + Apr + May,
         AprMay = Apr + May) %>% # sum across months ranges for each individual year, then take median
  select(LANDING_YEAR, AGENCY_CODE, JanMay, FebMay, MarMay, AprMay, Jan, Feb, Mar, Apr, May) %>%
  reshape2::melt(c('LANDING_YEAR', "AGENCY_CODE")) %>%
  rename(total = value)

region_species <- comp_dat_raw_load %>%
  mutate(AGENCY_CODE = 'All') %>% # add agency_code = Region for region-wide stats
  rbind(comp_dat_raw_load) %>%
  filter(LANDING_MONTH < 6, LANDING_YEAR == 2020) %>%
  mutate(month = month.abb[LANDING_MONTH]) %>%
  group_by(LANDING_YEAR, AGENCY_CODE, month, SPECIES_GROUP) %>% 
  summarise(rev = sum(EXVESSEL_REVENUE)) %>% # sum to year-month-state level
  reshape2::dcast(LANDING_YEAR + SPECIES_GROUP + AGENCY_CODE ~ month,
                  value.var = "rev", fill = 0) %>%
  mutate(JanMay = Jan + Feb + Mar + Apr + May,
         FebMay = Feb + Mar + Apr + May,
         MarMay = Mar + Apr + May,
         AprMay = Apr + May) %>% # sum across months ranges for each individual year, then take median
  select(LANDING_YEAR, SPECIES_GROUP, AGENCY_CODE, JanMay, FebMay, MarMay, AprMay, Jan, Feb, Mar, Apr, May) %>%
  reshape2::melt(c('LANDING_YEAR', 'SPECIES_GROUP', 'AGENCY_CODE'))

region_species_imp <- merge(region_tot, region_species, by = c('LANDING_YEAR', 'variable', 'AGENCY_CODE')) %>%
  mutate(contribution = round((value/total)*100, 0)) %>%
  reshape2::dcast(LANDING_YEAR + SPECIES_GROUP + AGENCY_CODE ~ variable,
                  value.var = "contribution")

state_by_month <- comp_dat_raw_load %>%
  filter(LANDING_MONTH < 6, AGENCY_CODE != 'F', SPECIES_GROUP == 'DUNGENESS CRAB') %>%
  group_by(LANDING_YEAR, AGENCY_CODE, LANDING_MONTH) %>%
  summarise(REV = sum(EXVESSEL_REVENUE),
            WEIGHT = sum(ROUND_WEIGHT_MTONS)) %>%
  reshape2::melt(c('LANDING_YEAR', 'AGENCY_CODE', 'LANDING_MONTH')) %>%
  mutate(period = ifelse(LANDING_YEAR < 2020, 'baseline', 'current')) %>%
  group_by(period, AGENCY_CODE, LANDING_MONTH, variable) %>%
  summarise(median = median(value)) %>%
  #mutate(description = 'Total state-wide') %>%
  reshape2::dcast(AGENCY_CODE + LANDING_MONTH + variable ~ period,
                  value.var = "median") %>%
  mutate(percchange = round((current - baseline)/baseline*100,0)) %>%
  filter(!is.na(current))%>%  
  group_by(AGENCY_CODE, variable) %>%
  mutate(tot = sum(current)) %>%
  mutate(contribution = (current/tot)*100) 


# Check which species are driving shrimp numbers
shrimp_check <- merge(dbGetQuery(pacfin, "select sum(landed_weight_lbs)/1e3 lbs, sum(exvessel_revenue)/1e3 rev, pacfin_species_code, agency_code
                           from pacfin_marts.comprehensive_ft
                           where landing_year = 2020 and management_group_code = 'SRMP' and landing_month < 6
                           group by pacfin_species_code, agency_code
                           order by agency_code, rev desc"),
                      dbGetQuery(pacfin, "select sum(landed_weight_lbs)/1e3 tot_lbs, sum(exvessel_revenue)/1e3 tot_rev, agency_code
                           from pacfin_marts.comprehensive_ft
                           where landing_year = 2020 and management_group_code = 'SRMP' and landing_month < 6
                           group by agency_code
                           order by agency_code, tot_rev desc"), by = 'AGENCY_CODE') %>%
  mutate(share_lbs = round((LBS/TOT_LBS)*100, 0),
         share_rev = round((REV/TOT_REV)*100, 0))

shrimp_month <- dbGetQuery(pacfin, "select sum(landed_weight_lbs)/1e3 lbs, sum(exvessel_revenue)/1e3 rev, pacfin_species_code, landing_month, agency_code
                           from pacfin_marts.comprehensive_ft
                           where landing_year = 2020 and management_group_code = 'SRMP' and landing_month < 6
                           group by pacfin_species_code, landing_month, agency_code
                           order by agency_code, landing_month, rev desc")
