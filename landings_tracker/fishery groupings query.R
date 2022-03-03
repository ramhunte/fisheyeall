library(dplyr)
library(ggplot2)
library(lubridate)

# Insert database connection information here #
source("C:/Program Files/R/connectioninfoROracle.r")
# looking into sardine and urchins for decisions about how to handle disaster data ####
sardine_query <- dbGetQuery(pacfin, "select sum(exvessel_revenue) rev, agency_code, landing_year year, landing_month month, vessel_id 
from pacfin_marts.comprehensive_ft
where pacfin_species_code = 'NANC' and participation_group_code in ('C') and landing_year > 2013 and exvessel_revenue > 0
group by agency_code, landing_year, vessel_id, landing_month")

sardine_adj <- sardine_query %>%
  group_by(AGENCY_CODE, YEAR, MONTH) %>%
  mutate(N = length(unique(VESSEL_ID)),
         REV = case_when(N < 3 ~ NA_real_,
                         T ~ REV)) %>%
  summarize(REV = sum(REV)) %>%
  ungroup() %>%
  mutate(MONTH = month(MONTH, label = T),
         YEAR = as.factor(YEAR),
         Group = 'Sardine')

sardine_plot <- ggplot(sardine_adj, aes(x = MONTH, y = REV, color = YEAR, group =YEAR)) +
  geom_point(aes(color = YEAR)) +
  geom_line(aes(color = YEAR)) +
  facet_wrap(~AGENCY_CODE)
sardine_plot

cpel_query <- dbGetQuery(pacfin, "select sum(exvessel_revenue) rev, agency_code, landing_year year, landing_month month, vessel_id 
from pacfin_marts.comprehensive_ft
where management_group_code = 'CPEL' and participation_group_code in ('C') and landing_year > 2013 and exvessel_revenue > 0
group by agency_code, landing_year, vessel_id, landing_month")

cpel_adj <- cpel_query %>%
  group_by(AGENCY_CODE, YEAR, MONTH) %>%
  mutate(N = length(unique(VESSEL_ID)),
         REV = case_when(N < 3 ~ NA_real_,
                         T ~ REV)) %>%
  summarize(REV = sum(REV)) %>%
  ungroup() %>%
  mutate(MONTH = month(MONTH, label = T),
         YEAR = as.factor(YEAR),
         Group = 'CPEL')

compare_cpel <- rbind(cpel_adj, sardine_adj)

filter(compare_cpel, AGENCY_CODE == 'C' & Group == 'Sardine') %>%
  ggplot(aes(x = MONTH, y = REV, fill = Group)) +
  geom_bar(stat ='identity', position = 'dodge') +
  facet_wrap(~YEAR)
