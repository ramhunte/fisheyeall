library(dplyr)
library(data.table)
source("helperfns.R")
source("confTreat.R")
library(odbc)
library(getPass)
library(fredr)
library(ggplot2)
library(ggthemes)

currentmonth <- 8

## ------------------------------------- Region-wide & State-wide statistics ---------------------------------------------- ####

# Some notes - 
# No disaster years are excluded from region-wide & state-wide statistics because that leads to apples to oranges comparison 
# FROM ERIN: changing the starting file (from raw data) - it's vessel-dealer-day level data but has the following things already done:
# 1. inflation adjustment
# 2. removal of shellfish
# 3. removal of 2014
# 4. removal of outlier prices (prices over $150/lb, 7 obs from other, crab, tuna, and shrimp)
# 5. removal of landings with NULL vessel_num
# 6. includes state = "All" and fishery = "all non-whiting groundfish"¬

# pull in raw data from dataprep_fn.R
region_state_fishery_raw <- readRDS('day_raw_full_keep_all_fisheries.RDS') %>%
  select(-AGENCY_CODE, -SPECIES_GROUP, -Year) %>%
  mutate(Value = case_when( # set at-sea revenue to zero
    Species %in% c('Whiting: Mothership', 'Whiting: Catcher processor') & Metric == 'EXVESSEL_REVENUE' ~ 0, 
    T ~ Value)) %>%
  filter(State != 'At-sea') # use state = 'all' for at-sea stats for simplicity

# regional totals by month for follow-up request on 9-28-20
region_by_month_treated <- region_state_fishery_raw %>%
  filter(!Species %in% c('All non-whiting groundfish', 'Whiting: Mothership', 'Whiting: Catcher processor'), 
         Metric == 'EXVESSEL_REVENUE', State == 'All') %>% # remove all groundfish to avoid double-counting, no at-sea revenue
  confTreat(c('LANDING_YEAR', 'LANDING_MONTH'),
            valvar = 'Value', confunit = c('VESSEL_NUM','DEALER_NUM')) %>% 
  mutate(Species = 'All fisheries') %>%
  group_by(LANDING_YEAR, LANDING_MONTH) %>%
  summarise(rev = sum(Value)/1e3)

# generate baseline mean & median
region_by_month_baseline <- region_by_month_treated %>% 
  filter(LANDING_YEAR != 2020) %>%
  mutate(Year = "baseline") %>%
  group_by(Year, LANDING_MONTH) %>%
  summarise(med = median(rev),
            mean = mean(rev)) %>%
  reshape2::melt(c('Year', 'LANDING_MONTH'))

# bind together
region_by_month <- rbind(region_by_month_treated %>% rename(Year = LANDING_YEAR) %>% mutate(variable = "value") %>%
                           reshape2::dcast(Year + variable ~ LANDING_MONTH, value.var = 'rev'),
                         region_by_month_baseline %>% reshape2::dcast(Year + variable ~ LANDING_MONTH, value.var = 'value'))

write.csv(region_by_month, file = paste("S:/Research/COVID-19/Round 3 - data & figures (Sept 2020)/region_by_month", Sys.Date(),".csv "), sep = ',', row.names = F)


# Create a species group category for all coastal pelagics
cpel_list <- c('Sardine', 'Anchovy', 'Market squid', 'Other coastal pelagic')

region_state_fishery_upd <- region_state_fishery_raw %>%
  rbind(filter(region_state_fishery_raw, Species %in% cpel_list) %>%
          mutate(Species = 'All coastal pelagic'))

region_state_fishery <- rbind(
  # Dataframe for Jan-current aggregations
  (region_state_fishery_upd %>%
    filter(LANDING_MONTH < currentmonth) %>%
    mutate(month_range = 'YTD')),
  # Dataframe for Mar-current aggregations
  (region_state_fishery_upd %>%
     filter(LANDING_MONTH < currentmonth & LANDING_MONTH >= 3) %>%
     mutate(month_range = 'MarchTD')),
  # Dataframe for individual months aggregations
  (region_state_fishery_upd %>%
     filter(LANDING_MONTH < currentmonth) %>%
     mutate(month_range = month.abb[LANDING_MONTH]))) %>%
  mutate(Year = case_when(
           LANDING_YEAR < 2020 ~ "Baseline",
           T ~ "2020")) 

# Confidentiality check using Year rather than LANDING_YEAR because we don't want to suppress individual years in baseline.  
# by baseline/2020 and state (ca, wa, or, all), month range for all species combined
# Need to remove pre-aggregated fisheries (all grnd, all cpel to avoid double counting)
region_state_conf_treated <-
  filter(region_state_fishery, !Species %in% c('All coastal pelagic', 'All non-whiting groundfish')) %>%
  confTreat(c('Year','State', 'Metric', 'month_range'),
            valvar = 'Value', confunit = c('VESSEL_NUM','DEALER_NUM')) %>%
  mutate(Species = 'All fisheries')

# by species and baseline/2020 and state (ca, wa, or, all), month range
region_state_fishery_conf_treated <- 
  # remove the disaster years so they aren't included in the baseline calculation for crab, but are included in the baseline calculation for all species
  subset(region_state_fishery, !(Species == 'Dungeness crab' & LANDING_YEAR %in% 2015:2016)) %>%
   confTreat(c('Year','State', 'Species', 'Metric', 'month_range'),
             valvar = 'Value', confunit = c('VESSEL_NUM','DEALER_NUM'))

# calculate the median of the month (median of totals)
full_treated <- bind_rows(region_state_conf_treated, region_state_fishery_conf_treated) %>%
  group_by(Metric, State, Species, Year, month_range, LANDING_YEAR) %>%
  # total landings by month range
  summarise(Value = sum(Value)) %>%
  group_by(Metric, State, Species, Year, month_range) %>%
  # median total landings
  summarize(median = median(Value))


# total of medians (landings tracker method)
# full_treated <- bind_rows(region_state_conf_treated, region_state_fishery_conf_treated) %>%
#   group_by(LANDING_MONTH, Metric, State, Species, Year, month_range, LANDING_YEAR) %>%
#   # total landings by month-year (one total for each state, species, year, month)
#   summarise(Value = sum(Value))%>%
#   group_by(Metric, State, Species, Year, month_range, LANDING_MONTH) %>%
#   # median total landings by month (one total for each state, species, month)
#   summarize(median = median(Value)) %>%
#   # cumulative total landings
#   summarize(median = sum(median))

# generate # of participants by fishery
num_participants <- region_state_fishery_upd %>%
  filter(LANDING_YEAR != 2020) %>%
  group_by(LANDING_YEAR, Species, State) %>%
  summarise(num_vss = length(unique(VESSEL_NUM)),
            num_buyers = length(unique(DEALER_NUM))) %>%
  group_by(Species, State) %>%
  summarise(N_vss = median(num_vss),
            N_buyer = median(num_buyers))

# Pull in fishery revenue as proportion of state/all
proportion <- readRDS("addlfilters.RDS") %>%
  rename(prop_rev = state_prop1) %>%
  select(State, Species, prop_rev) %>%
  distinct()

## --------------- All % Change Statistics ----------------------------- ####
state_stats <- full_treated %>%
  reshape2::dcast(State + month_range + Metric + Species ~ Year,
                  value.var = "median") %>%
  mutate(percchange = round((`2020`/Baseline - 1)*100,0)) %>%
  reshape2::dcast(State + Metric + Species ~ month_range,
                  value.var = 'percchange') %>%
  mutate(
    Description = '% change from 5-year median',
    Metric = case_when(
      Metric == 'EXVESSEL_REVENUE' ~ 'Ex-vessel revenue',
      T ~ 'Landed weight (mtons)')) %>%
  merge(num_participants, by = c('State', 'Species'), all.x = T) %>%
  merge(proportion, by = c('State', 'Species'), all.x = T) %>%
  select(State, Species, Description, Metric, YTD, MarchTD, Jan, Feb, Mar, Apr, May, Jun, Jul, N_vss, N_buyer, prop_rev)


## --------------- Region-wide Figures ----------------------------- ####

# Redo confidentiality on individual years & combine some species groups for figure data
figure_treated <- region_state_fishery %>%
  filter(!Species %in% c('All coastal pelagic', 'All non-whiting groundfish')) %>% #don't want to double count grnd or cpel
  mutate(Species = case_when(
    grepl("crab", Species) ~ 'Crab',
    grepl("groundfish: IFQ", Species) ~ 'Non-whiting groundfish IFQ',
    grepl("groundfish: Fixed", Species) ~ 'Non-whiting groundfish Fixed gear',
    Species %in% c('Anchovy', 'Sardine', 'Other coastal pelagic') ~ 'Other coastal pelagic',
    Species %in% c('Whiting: Mothership', 'Whiting: Catcher processor') ~ 'Whiting: At-sea',
    T ~ Species)) %>%
  confTreat(c('LANDING_YEAR','State', 'Species', 'Metric', 'month_range'),
            valvar = 'Value', confunit = c('VESSEL_NUM','DEALER_NUM'))

figure_data <- figure_treated %>%
  filter(State == 'All') %>%
  group_by(LANDING_YEAR, month_range, Species, Metric) %>% 
  summarise(val = sum(Value))

# figure data for follow-up request 9-28-20
figure_data_for_sharing <- figure_data %>%
  filter(month_range == 'MarchTD', Metric == 'EXVESSEL_REVENUE', Species != 'Whiting: At-sea') %>%
  mutate(Value = val/1e6) %>%
  reshape2::dcast(Species + Metric ~ LANDING_YEAR, value.var = 'Value')
  
write.csv(figure_data_for_sharing, file = paste("S:/Research/COVID-19/Round 3 - data & figures (Sept 2020)/fig1_data", Sys.Date(),".csv "), sep = ',', row.names = F)

ggplot(figure_data %>% 
         filter(month_range == 'MarchTD', Metric == 'EXVESSEL_REVENUE', Species != 'Whiting: At-sea'), 
       aes(fill=Species, y=val/1e6, x=LANDING_YEAR)) + 
  geom_bar(position="stack", stat="identity") + ggtitle("March-July Shoreside ex-vessel revenue (2020 $)") + xlab(" ") + ylab("Revenue (millions)") +
  scale_fill_stata('s2color', name = '')

## -------------- Other exploratory graphs  ------- ##

# ggplot(figure_data %>% 
#          filter(month_range == 'JanJuly', Metric == 'EXVESSEL_REVENUE', Species != 'Whiting: At-sea'), 
#        aes(fill=Species, y=val/1e6, x=LANDING_YEAR)) + 
#   geom_bar(position="stack", stat="identity") + ggtitle("Year-to-date Shoreside ex-vessel revenue (2020 $)") + xlab(" ") + ylab("Revenue (millions)") +
#   scale_fill_stata('s2color', name = '')
# 
# ggplot(figure_data %>% 
#          filter(month_range == 'MarJuly', Metric == 'ROUND_WEIGHT_MTONS'), 
#        aes(fill=Species, y=val/1e3, x=LANDING_YEAR)) + 
#   geom_bar(position="stack", stat="identity") + ggtitle("March-July Shoreside Volume Landed") + xlab(" ") + ylab("Metric tons (thousands)") +
#   scale_fill_stata('s2color', name = '')
# 
# ggplot(figure_data %>% 
#          filter(month_range == 'JanJuly', Metric == 'ROUND_WEIGHT_MTONS'), 
#        aes(fill=Species, y=val/1e3, x=LANDING_YEAR)) + 
#   geom_bar(position="stack", stat="identity") + ggtitle("Year-to-date Shoreside Volume Landed") + xlab(" ") + ylab("Metric tons (thousands)") +
#   scale_fill_stata('s2color', name = '')

## -------------- Other exploratory graphs at the moht level ------- ##

#Redo confidentiality on individual months and years & combine some species groups for figure data
# month_figure_treated <- region_state_fishery %>%
#   filter(!Species %in% c('All coastal pelagic', 'All non-whiting groundfish')) %>% #don't want to double count groundfish
#   mutate(Species = case_when(
#     grepl("crab", Species) ~ 'Crab',
#     grepl("groundfish: IFQ", Species) ~ 'Non-whiting groundfish IFQ',
#     grepl("groundfish: Fixed", Species) ~ 'Non-whiting groundfish Fixed gear',
#     Species %in% c('Anchovy', 'Sardine', 'Other coastal pelagic') ~ 'Other coastal pelagic',
#     Species %in% c('Whiting: Mothership', 'Whiting: Catcher processor') ~ 'Whiting: At-sea',
#     T ~ Species)) %>%
#   confTreat(c('LANDING_YEAR','State', 'Species', 'Metric', 'LANDING_MONTH'),
#             valvar = 'Value', confunit = c('VESSEL_NUM','DEALER_NUM'))
# 
# month_figure_data <- month_figure_treated %>%
#   filter(State == 'All', !month_range %in% c('YTD','MarchTD')) %>%
#   group_by(LANDING_YEAR, LANDING_MONTH, Species, Metric) %>%
#   summarise(val = sum(Value)) %>%
#   mutate(month = month.abb[LANDING_MONTH])
# # 
# ggplot(month_figure_data %>%
#          filter(LANDING_MONTH %in% c( 3, 4, 5, 6, 7), Metric == 'EXVESSEL_REVENUE', Species != 'Whiting: At-sea'),
#        aes(fill=Species, y=val/1e6, x=LANDING_YEAR)) +
#   facet_wrap(~ LANDING_MONTH) +
#   geom_bar(position="stack", stat="identity") + ggtitle("March-July by Month Ex-vessel revenue (2020 $)") + xlab(" ") + ylab("Revenue (millions)") +
#   scale_fill_stata('s2color', name = '')
# # 
# ggplot(month_figure_data %>%
#          filter(LANDING_MONTH %in% c(1, 2, 3, 4, 5, 6, 7), Metric == 'EXVESSEL_REVENUE', Species != 'Whiting: At-sea'),
#        aes(fill=Species, y=val/1e6, x=LANDING_YEAR)) +
#   facet_wrap(~ LANDING_MONTH) +
#   geom_bar(position="stack", stat="identity") + ggtitle("January-July by Month Ex-vessel revenue (2020 $)") + xlab(" ") + ylab("Revenue (millions)") +
#   scale_fill_stata('s2color', name = '')
# 
# ggplot(month_figure_data %>% 
#          filter(LANDING_MONTH %in% c(1, 2, 3, 4, 5, 6, 7), LANDING_YEAR == 2020, Metric == 'EXVESSEL_REVENUE', Species != 'Whiting: At-sea'), 
#        aes(fill=Species, y=val/1e6, x=LANDING_MONTH)) + 
#   geom_bar(position="stack", stat="identity") + ggtitle("2020 January-July by Month Ex-vessel revenue (2020 $)") + xlab(" ") + ylab("Revenue (millions)") +
#   scale_fill_stata('s2color', name = '')
  

## --------------- Other stats for write-up ----------------------------- ####


pacfin <- DBI::dbConnect(odbc::odbc(),
                         host   = "pacfindb.psmfc.org",
                         UID    = getPass('pacfin username'),
                         PWD    = getPass('pacfin password'),
                         dsn    = 'pacfin',
                         port   = 2045)

fredr_set_key('a5d92deb507a05eb5a7f84355a02f602')
fred_gdpdefl <- fredr(
  series_id = "GDPDEF",
  observation_start = as.Date("2015-01-01")
) 

# pull and save the deflators
defl_adj <- mutate(fred_gdpdefl, LANDING_YEAR = format(date,"%Y")) %>%
  group_by(LANDING_YEAR) %>%
  summarize(defl = mean(value), .groups = 'drop') %>%
  mutate(DEFL = defl/defl[LANDING_YEAR == 2020]) %>%
  select(-defl) %>%
  mutate(LANDING_YEAR = as.numeric(LANDING_YEAR))


# Check which species are driving shrimp numbers
shrimp_check <- merge(dbGetQuery(pacfin, "select sum(landed_weight_lbs)/1e3 lbs, sum(exvessel_revenue)/1e3 rev, pacfin_species_code, agency_code
                           from pacfin_marts.comprehensive_ft
                           where landing_year = 2020 and management_group_code = 'SRMP' and landing_month < 8
                           group by pacfin_species_code, agency_code
                           order by agency_code, rev desc"),
                      dbGetQuery(pacfin, "select sum(landed_weight_lbs)/1e3 tot_lbs, sum(exvessel_revenue)/1e3 tot_rev, agency_code
                           from pacfin_marts.comprehensive_ft
                           where landing_year = 2020 and management_group_code = 'SRMP' and landing_month < 8
                           group by agency_code
                           order by agency_code, tot_rev desc"), by = 'AGENCY_CODE') %>%
  mutate(share_lbs = round((LBS/TOT_LBS)*100, 0),
         share_rev = round((REV/TOT_REV)*100, 0))

shrimp_tot_check <- merge(dbGetQuery(pacfin, "select sum(landed_weight_lbs)/1e3 lbs, sum(exvessel_revenue)/1e3 rev, pacfin_species_code
                           from pacfin_marts.comprehensive_ft
                           where landing_year = 2016 and management_group_code = 'SRMP'
                           group by pacfin_species_code
                           order by rev desc"),
                      dbGetQuery(pacfin, "select sum(landed_weight_lbs)/1e3 tot_lbs, sum(exvessel_revenue)/1e3 tot_rev
                           from pacfin_marts.comprehensive_ft
                           where landing_year = 2016 and management_group_code = 'SRMP'
                           order by  tot_rev desc")) %>%
  mutate(share_lbs = round((LBS/TOT_LBS)*100, 0),
         share_rev = round((REV/TOT_REV)*100, 0))

shrimp_month <- dbGetQuery(pacfin, "select sum(landed_weight_lbs)/1e3 lbs, sum(exvessel_revenue)/1e3 rev, count(distinct(dealer_num)) num_proc, count(distinct(vessel_id)) num_vss,
landing_month, agency_code
                           from pacfin_marts.comprehensive_ft
                           where landing_year = 2020 and management_group_code = 'SRMP' and landing_month < 8
                           group by landing_month, agency_code
                           order by agency_code, landing_month, rev desc")

shrimp_price <- dbGetQuery(pacfin, "
    select sum(exvessel_revenue) rev, sum(round_weight_lbs) lbs, landing_year, pacfin_species_code,
count(distinct(dealer_num)) num_proc, count(distinct(vessel_id)) num_vss
                         from pacfin_marts.comprehensive_ft
                         where council_code IN ('P','*')
                         AND participation_group_code IN ('C')
                         AND LANDING_YEAR in (2019, 2020) and landing_month < 8
                         AND inpfc_area_type_code NOT IN ('CT','VC','GS')
                         AND exvessel_revenue > 0
                         -- Excluding Roe as in original script --
                         AND condition_code != 'E'
                         AND management_group_code = 'SRMP'
                         group by pacfin_species_code, landing_year
                         ") %>%
  merge(defl_adj) %>%
  mutate(REV_DEFL = REV/DEFL,
         price = round(REV_DEFL/LBS, 2))

# Check which species are driving non-whiting numbers
grnd_check <- merge(dbGetQuery(pacfin, "select sum(landed_weight_lbs)/1e3 lbs, sum(exvessel_revenue)/1e3 rev, pacfin_species_code
                           from pacfin_marts.comprehensive_ft
                           where landing_year = 2020 and management_group_code = 'GRND' and pacfin_species_code != 'PWHT' and landing_month < 8
                           group by pacfin_species_code
                           order by rev desc"),
                      dbGetQuery(pacfin, "select sum(landed_weight_lbs)/1e3 tot_lbs, sum(exvessel_revenue)/1e3 tot_rev
                           from pacfin_marts.comprehensive_ft
                           where landing_year = 2020 and management_group_code = 'GRND' and pacfin_species_code != 'PWHT' and landing_month < 8
                           order by tot_rev desc")) %>%
  mutate(share_lbs = round((LBS/TOT_LBS)*100, 0),
         share_rev = round((REV/TOT_REV)*100, 0))

grnd_price <- dbGetQuery(pacfin, "
    select sum(exvessel_revenue) rev, sum(round_weight_lbs) lbs, landing_year, pacfin_species_code,
count(distinct(dealer_num)) num_proc, count(distinct(vessel_id)) num_vss
                         from pacfin_marts.comprehensive_ft
                         where council_code IN ('P','*')
                         AND participation_group_code IN ('C')
                         AND LANDING_YEAR in (2019, 2020) and landing_month in (3,4,5,6,7)
                         AND inpfc_area_type_code NOT IN ('CT','VC','GS')
                         AND exvessel_revenue > 0
                         -- Excluding Roe as in original script --
                         AND condition_code != 'E'
                         AND management_group_code = 'GRND' and pacfin_species_code in ('SABL', 'PTRL', 'DOVR')
                         group by pacfin_species_code, landing_year
                         ") %>%
  merge(defl_adj) %>%
  mutate(REV_DEFL = REV/DEFL,
    price = round(REV_DEFL/LBS, 2))

sabl_price <- dbGetQuery(pacfin, "
    select sum(exvessel_revenue) rev, sum(round_weight_lbs) lbs, landing_month, landing_year, gmt_sablefish_code,
count(distinct(dealer_num)) num_proc, count(distinct(vessel_id)) num_vss
                         from pacfin_marts.comprehensive_ft
                         where council_code IN ('P','*')
                         AND participation_group_code IN ('C')
                         AND LANDING_YEAR in (2019, 2020) and landing_month = 7 
                         AND inpfc_area_type_code NOT IN ('CT','VC','GS')
                         AND exvessel_revenue > 0
                         -- Excluding Roe as in original script --
                         AND condition_code != 'E'
                         AND management_group_code = 'GRND' and pacfin_species_code = 'SABL'
                         group by landing_month, landing_year, gmt_sablefish_code
                         ") %>%
  merge(defl_adj) %>%
  mutate(REV_DEFL = REV/DEFL,
         price = round(REV_DEFL/LBS, 2))

group_by(AGENCY_CODE, LANDING_YEAR) %>%
  mutate(prop = LBS/sum(LBS))

da_yang_closure <- dbGetQuery(pacfin, "select distinct landing_date
                              from pacfin_marts.comprehensive_ft
                              where dealer_num = '0891' and landing_year = 2020 and landing_month in (6,7,8)
                              order by landing_date")
