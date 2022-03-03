# Data compare checksheet ####
library(dplyr)
library(data.table)
library(edcdataprep)
# PART 1. Comparing fish tickets ####

# Load last fish ticket pull. ADJUST DATE
old_raw <- readRDS("R:/Confidential/FISHEyE/data/landings_tracker/comp_dat_raw2020-09-23.RDS")

# Load new fish ticket pull. 
new_raw <- readRDS('comp_dat_raw.RDS')

# 1) Basic understanding of what changed
# Compare the number of tickets by month, year, state, species
old_raw_n <- old_raw %>%
  group_by(LANDING_YEAR, LANDING_MONTH, AGENCY_CODE, SPECIES_GROUP) %>%
  summarize(N = length(EXVESSEL_REVENUE))
new_raw_n <- new_raw %>%
  group_by(LANDING_YEAR, LANDING_MONTH, AGENCY_CODE, SPECIES_GROUP) %>%
  summarize(N = length(EXVESSEL_REVENUE))
compare_n <- comparefun(old_raw_n, new_raw_n, c('N'), 'wide')
# Find tickets where N changed
n_change <- filter(compare_n, abs(percDiff) > 0)
# Find tickets in categories that didn't exist or no longer exist
n_new <- filter(compare_n, combomiss == 'Missing combo')

# 2) Identifying changes and summarizing rev/wt added or lost
compare <- comparefun(old_raw, new_raw, c('EXVESSEL_REVENUE', 'ROUND_WEIGHT_MTONS'),'wide')
compare_diff <- filter(compare, abs(EXVESSEL_REVENUE_percDiff) > 0 | combomiss == 'Missing combo')
compare_diff_smry <- compare_diff %>%
  group_by(LANDING_YEAR, LANDING_MONTH, AGENCY_CODE, SPECIES_GROUP) %>%
  summarize(REV_change = sum(EXVESSEL_REVENUE_rawDiff),
            WT_change = sum(ROUND_WEIGHT_MTONS_rawDiff))

compare_new <- filter(compare, combomiss == 'Missing combo' & new_flag == 'new' & is.na(old_flag))
compare_new_smry <- compare_new %>%
  group_by(LANDING_YEAR, LANDING_MONTH, AGENCY_CODE, SPECIES_GROUP) %>%
  summarize(REV_plus = sum(EXVESSEL_REVENUE_new),
            WT_plus = sum(ROUND_WEIGHT_MTONS_new))
compare_old <- filter(compare, combomiss == 'Missing combo' & old_flag == 'old' & is.na(new_flag))
compare_old_smry <- compare_old %>%
  group_by(LANDING_YEAR, LANDING_MONTH, AGENCY_CODE, SPECIES_GROUP) %>%
  summarize(REV_minus = sum(EXVESSEL_REVENUE_old),
            WT_minus = sum(ROUND_WEIGHT_MTONS_old))

compare_change_smry <- merge(compare_diff_smry, compare_new_smry, all = T) %>%
  merge(compare_old_smry, all = T) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(REV_smry = REV_change + REV_plus - REV_minus,
         WT_smry = WT_change + WT_plus - WT_minus) %>%
  select(-c(REV_change, WT_change, REV_plus, WT_plus, REV_minus, WT_minus)) %>%
  filter(abs(REV_smry) > 0)

saveRDS(compare_change_smry, paste0("R:/Confidential/FISHEyE/data/landings_tracker/datacompare_raw", Sys.Date(), ".RDS"))

# PART 2. Comparing app data ####
# Load last app data ####
old_app <- readRDS("R:/Confidential/FISHEyE/data/landings_tracker/comp_dat_covidapp2020-08-28.RDS")
old_app <- readRDS("comp_dat_covidapp0716.RDS")
# Load new app data ####
new_app <- new
#1) Basic understanding of how total revenue changed
old_app_n <- old_app %>%
  filter(Metric == 'Exvessel revenue', Statistic == 'Total', Cumulative == 'N', Interval == 'Monthly') %>%
  select(-c(conf_state, conf_species, Type, ylab, upper, lower, Dates_as_char, complete, no_pts, 
            q25, q75, Variance, Cumulative, Interval)) %>%
  distinct(Value, .keep_all = T)
new_app_n <- new_app %>%
  filter(Metric == 'Exvessel revenue', Statistic == 'Total', Cumulative == 'N', Interval == 'Monthly') %>%
  select(-c(conf_state, conf_species, Type, ylab, upper, lower, Dates_as_char, complete, no_pts, 
            q25, q75, Variance, Cumulative, Interval))
compare_app <- comparefun(old_app_n, new_app_n, c('Value','N_vss','N_buy'), 'wide')
# Find combos where revenue changed
rev_change_app <- filter(compare_app, abs(Value_percDiff) > 0)
# Revenue changed by greater than 10%
rev_change_10 <- filter(compare_app, abs(Value_percDiff) > 0.02)

# Confidentiality changes
rev_conf <- filter(compare_app, combomiss == 'Missing combo') %>%
  mutate(rm = case_when(N_vss_old == 0 & (N_vss_new == 0 | is.na(N_vss_new)) ~ 1, 
                        N_vss_new == 0 & (N_vss_old == 0 | is.na(N_vss_old)) ~ 1,
                        T ~ 0)) %>%
  filter(rm == 0) %>%
  select(-rm)

rev_conf_2020 <- filter(rev_conf, Year == 2020)

### ------------------ Data compare with pacfin raw data summary and app data -------------- ####
# 07/20/2020 #####
raw <- readRDS('comp_dat_raw.RDS')

fredr_set_key('a5d92deb507a05eb5a7f84355a02f602')
fred_gdpdefl <- fredr(
  series_id = "GDPDEF",
  observation_start = as.Date("2015-01-01")
) 

defl_adj <- mutate(fred_gdpdefl, YEAR = format(date,"%Y")) %>%
  group_by(YEAR) %>%
  summarise(defl = mean(value)) %>%
  mutate(DEFL = defl/defl[YEAR == 2020]) %>%
  select(-defl)

# species groups changes #####
wa_or_othr <- c('OTHER CRAB', 'ANCHOVY', 'SARDINE', 'OTHER COASTAL PELAGIC')

# Load data from data_pull.R ####
comp_dat_raw1 <- raw %>%
  rename(YEAR = LANDING_YEAR) %>%
  select(-TICKET_SOURCE_CODE) %>%
  subset(!(AGENCY_CODE == 'C' & SPECIES_GROUP == 'WHITING')) %>%
  # move wa/or cps into other species
  mutate(SPECIES_GROUP = case_when(
    AGENCY_CODE %in% c('W', 'O') & SPECIES_GROUP %in% wa_or_othr ~ 'OTHER',
    T ~ SPECIES_GROUP)) %>%
  filter(YEAR > 2014)

comp_dat_outadj <- comp_dat_raw1 %>%
  mutate(price = EXVESSEL_REVENUE/(ROUND_WEIGHT_MTONS*2204.62)) %>%
  filter(price < 150) %>%
  select(-price)

comp_dat_sub <- comp_dat_outadj %>%
  select( -LANDING_DATE) %>%
  reshape2::melt(c('VESSEL_NUM','DEALER_NUM','SPECIES_GROUP','YEAR', 'LANDING_MONTH', 'AGENCY_CODE')) %>%
  rename(Metric = variable,
         Value = value) %>%
  # Deflator is being used here #
  merge(defl_adj) %>%
  mutate(Value = case_when(Metric == 'EXVESSEL_REVENUE' ~ Value/DEFL,
                           T ~ Value)) %>%
  select(-DEFL)

comp_dat_tot <- comp_dat_sub %>%
  group_by(SPECIES_GROUP, AGENCY_CODE, YEAR, LANDING_MONTH, Metric) %>%
  summarize(Value = sum(Value)) %>%
  ungroup() %>%
  mutate(Species = convert_sp(SPECIES_GROUP),
         Species = as.factor(Species),
         State = convert_state(AGENCY_CODE),
         State = as.factor(State),
         Metric = case_when(Metric == 'EXVESSEL_REVENUE' ~ 'Exvessel revenue',
                            T ~ 'Landed weight (mt)'),
         Year = as.factor(YEAR),
         LANDING_MONTH = case_when(LANDING_MONTH < 10 ~ paste0(0,LANDING_MONTH),
                                   T ~ as.character(LANDING_MONTH))) %>%
  select(-SPECIES_GROUP, -AGENCY_CODE, -YEAR) %>%
  data.frame()

new_dat <- filter(app_data, Interval == 'Monthly' & Cumulative == 'N' & Statistic == 'Total' &
                    Metric %in% c('Exvessel revenue','Landed weight (mt)') &
                    Year != 'Baseline') %>%
  select(Year, Metric, State, Species, Value, LANDING_MONTH2) %>%
  rename(LANDING_MONTH = LANDING_MONTH2) %>%
  mutate(Year = as.factor(Year))

data_check <- comparefun(comp_dat_tot, new_dat, 'Value', 'wide')
check_5 <- filter(data_check, abs(percDiff) > 0.05)
