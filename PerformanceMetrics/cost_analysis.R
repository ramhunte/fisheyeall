library(dplyr)
library(tidyr)
library(ggplot2)

dat <- readRDS("data/CVperfmetrics.rds")
cost_cats <- subset(dat, grepl('dollar revenue', STAT)) |>
    distinct(METRIC) |> pull(METRIC)

tt <- subset(dat, METRIC %in% cost_cats & whitingv == 'All vessels' & STAT %in% c('Fleet-wide total', 'Fleet-wide average/dollar revenue') & CATEGORY == 'Fisheries' & VARIABLE %in% c('Pacific whiting', 'Groundfish with trawl gear')) 

subset(tt, STAT == 'Fleet-wide total' &
    !METRIC %in% c('All variable costs', 'On-board equipment', 'Other fixed costs', 'Fishing gear')) |>
ggplot(aes(x = as.numeric(YEAR), y = VALUE)) +
    geom_point(aes(color = METRIC)) +
    geom_line(aes(color = METRIC)) +
    facet_wrap(~VARIABLE) 

tot_agency <- subset(tt, STAT == 'Fleet-wide total' & METRIC %in% c('Observers/EM', 'Cost recovery fees', 'Buyback fees')) |>
    group_by(VARIABLE, YEAR) |>
    summarize(VALUE = sum(VALUE))

subset(tt, STAT == 'Fleet-wide total' &
    !METRIC %in% c('All variable costs', 'On-board equipment', 'Other fixed costs', 'Fishing gear')) |>
    mutate(METRIC = factor(METRIC, levels = c('All fixed costs', 'Labor', 'Fuel', 'Other variable costs', 'Observers/EM', 'Cost recovery fees', 'Buyback fees'))) |>
ggplot(aes(x = as.numeric(YEAR), y = VALUE/1e6)) +
    geom_area(aes(fill = METRIC)) +
    facet_wrap(~VARIABLE) +
    geom_line(data = tot_agency) +
    ylab('Total costs (milllions, nominal)') +
    xlab('Year') +
    scale_fill_viridis(discrete=TRUE) +
    theme(panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.margin = margin(20, 40, 10, 10))

subset(tt, STAT == 'Fleet-wide average/dollar revenue' &
    !METRIC %in% c('All variable costs', 'On-board equipment', 'Other fixed costs', 'Fishing gear')) |>
    mutate(METRIC = factor(METRIC, levels = c('All fixed costs', 'Labor', 'Fuel', 'Other variable costs', 'Observers/EM', 'Cost recovery fees', 'Buyback fees'))) |>
ggplot(aes(x = as.numeric(YEAR), y = VALUE)) +
    geom_point(aes(color = METRIC, shape = METRIC)) +
    geom_line(aes(color = METRIC)) +
    facet_wrap(~VARIABLE) +
    scale_color_viridis(discrete=TRUE) +
    geom_hline(yintercept = .03, linetype = 2, color = 'dark grey') +
    geom_hline(yintercept = 0) +
    ylab('Costs as % of revenue') +
    xlab('Year') +
    theme(panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.margin = margin(20, 40, 10, 10)) +
    scale_y_continuous(labels = scales::label_percent()) +
    annotate('text', x = 2024, y = .03, label = '3%') +
    coord_cartesian(xlim = c(2009,2023), expand = F, clip = "off") 
