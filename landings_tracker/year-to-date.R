library(ggplot2)
library(viridis)
library(dplyr)
library(lubridate)

comp_dat_covid_app <- readRDS("comp_dat_covidapp.RDS")

today <- Sys.Date()
completeness_cutoff <- today - 14
month_cutoff <- month(completeness_cutoff) - 1

# WE NEED TO FIX THE UNITS IF WE WANT TO USE THIS

dat <- subset(comp_dat_covid_app,
  Metric == 'Exvessel revenue' &
    Statistic == 'Total' &
    Cumulative == 'Y' &
    Interval == 'Monthly') %>%
  group_by(Year, State, Species, Type, unit) %>%
  subset(month(LANDING_MONTH) == month_cutoff) %>%
  mutate(Species2 = factor(Species, levels = rev(c(
    'Dungeness crab'                                    ,
    'Shrimp'                                            ,
    'Whiting: Mothership'                               ,
    'Whiting: Catcher processor'                        ,
    'Whiting: Shorebased'                               ,
    'All non-whiting groundfish'                        ,
    'Non-whiting groundfish: IFQ-midwater trawl'        ,
    'Non-whiting groundfish: IFQ-bottom trawl'          ,
    'Non-whiting groundfish: Fixed gear-nearshore'      ,
    'Non-whiting groundfish: Fixed gear-non-nearshore'  ,
    'Non-whiting groundfish: Fixed gear-other'          ,
    'Anchovy'                                           ,
    'Market squid'                                      ,
    'Puget Sound fisheries'                             ,
    'Salmon'                                            ,
    'Sardine'                                           ,
    'Tuna'                                              ,
    'Other coastal pelagic'                             ,
    'Other crab'                                        ,
    'Other species'))))

# Facet plot,
year_to_date <- filter(dat, State != 'At-sea' & !grepl('Median', Type) & !grepl('crab', Species)) %>%
  ggplot(aes(x = Value, y = Species2, group = Type)) +
  geom_point(aes(color = Type, shape = Type, alpha = Type)) +
  scale_alpha_manual(values = c(.5, 1)) +
  scale_color_manual(values = c('grey', 'blue')) +
  scale_shape_manual(values = c(16,3)) + 
  facet_wrap(~State)
ggsave('www/year-to-date.png', year_to_date, width = 13, height = 9, units = 'in')
