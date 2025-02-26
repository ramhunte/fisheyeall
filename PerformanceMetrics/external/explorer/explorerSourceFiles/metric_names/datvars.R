# Variables needed for selectors (Excludes metrics which are listed in metric_names file to allow creation of the html links)

currentyear <- 2023
currentyearFR <- 2023
nrcomponents <- c('Revenue', 'Variable costs', 'Fixed costs', 'Variable cost net revenue', 'Total cost net revenue')

# CV
datVars_cv <- list(
    YEAR = 2004:currentyear,
    NRlist = nrcomponents,
    CATEGORY = c(
        "Fisheries",
        "Homeport",
        "State of homeport" = "State",
        "Vessel length class"
    ),
    whitingv = c("All vessels", "Non-whiting vessels", "Whiting vessels"),
    STAT =  c(
        "Mean per vessel",
        "Mean per vessel/day",
        "Mean per vessel/metric ton caught",
        "Mean per vessel/dollar revenue",
        "Median per vessel",
        "Median per vessel/day",
        "Median per vessel/metric ton caught",
        "Median per vessel/dollar revenue",
        "Fleet-wide total",
        "Fleet-wide average/day",
        "Fleet-wide average/metric ton caught",
        "Fleet-wide average/dollar revenue"
    )
)

# save(datVars_cv, file = "PerformanceMetrics/data/datvars_cv.RData")
save(datVars_cv, file = "data/datvars_cv.RData")
# FR
datVars_fr <- list(
    YEAR = 2004:currentyearFR,
    NRlist = c('Revenue', 'Seafood sales revenue', 'Offload revenue', 'Custom processing and other revenue',
        'Variable costs', 'Fixed costs', 'Variable cost net revenue', 'Total cost net revenue'),
    CATEGORY = c("Production activities" = "Fisheries", "Region", "Processor size"),
    whitingv = c(
        "All processors",
        "Whiting processors",
        "Non-whiting processors"
    ),
    STAT =  c(
        "Mean per processor",
        "Mean per processor/metric ton produced",
        "Mean per processor/dollar of revenue",
        "Median per processor",
        "Median per processor/metric ton produced",
        "Median per processor/dollar of revenue",
        "Industry-wide total",
        "Industry-wide average/metric ton produced",
        "Industry-wide average/dollar of revenue"
    )
)

# save(datVars_fr, file = "PerformanceMetrics/data/datvars_fr.RData")
save(datVars_fr, file = "data/datvars_fr.RData")
# MS
datVars_ms <- list(
    YEAR = 2004:currentyear,
    NRlist = nrcomponents,
    CATEGORY = "Fisheries",
    whitingv = "Whiting vessels",
    STAT =  c(
        "Mean per vessel",
        "Mean per vessel/day",
        "Mean per vessel/metric ton purchased",
        "Mean per vessel/metric ton produced",
        "Mean per vessel/dollar of revenue",
        "Median per vessel",
        "Median per vessel/day",
        "Median per vessel/metric ton purchased",
        "Median per vessel/metric ton produced",
        "Median per vessel/dollar of revenue",
        "Fleet-wide total",
        'Fleet-wide average/day',
        'Fleet-wide average/metric ton purchased',
        'Fleet-wide average/metric ton produced',
        'Fleet-wide average/dollar of revenue'
    )
)

# save(datVars_ms, file = "PerformanceMetrics/data/datvars_ms.RData")
save(datVars_ms, file = "data/datvars_ms.RData")

# CP

datVars_cp <- list(
    YEAR = 2004:currentyear,
    NRlist = nrcomponents,
    CATEGORY = "Fisheries",
    whitingv = "Whiting vessels",
    STAT =  c(
        "Mean per vessel",
        "Mean per vessel/day",
        "Mean per vessel/metric ton produced",
        "Mean per vessel/metric ton caught",
        "Mean per vessel/dollar of revenue",
        "Median per vessel",
        "Median per vessel/day",
        "Median per vessel/metric ton produced",
        "Median per vessel/metric ton caught",
        "Median per vessel/dollar of revenue",
        "Fleet-wide total",
        'Fleet-wide average/day',
        'Fleet-wide average/metric ton produced',
        'Fleet-wide average/metric ton caught',
        'Fleet-wide average/dollar of revenue'
    )
)

# save(datVars_cp, file = "PerformanceMetrics/data/datvars_cp.RData")
save(datVars_cp, file = "data/datvars_cp.RData")
