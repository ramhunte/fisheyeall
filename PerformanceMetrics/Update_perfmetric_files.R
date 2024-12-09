# Display list of available date stamp folders for performance metrics
dir("R:/Confidential/FISHEyE/data/PerformanceMetrics")

# Copy files over for Performance Metrics
# Note from 6/25/2024 - I only copied over the CP data file
perfmetricsdir <- "R:/Confidential/FISHEyE/data/PerformanceMetrics/2024-12-09"
perfmetricsfiles <- list.files(perfmetricsdir)[grepl('RData', list.files(perfmetricsdir))]

perfmetricsfiles <- list.files(perfmetricsdir)[grepl('perfmetrics.rds', list.files(perfmetricsdir))]

try(if(length(perfmetricsfiles) == 0) stop("There aren't any files to move in that folder"))

destination <- ifelse(grepl('PerformanceMetrics', getwd()), "data", "Performancemetrics/data")

for(pfiles in perfmetricsfiles) {

    file.copy(
        from = file.path(perfmetricsdir, pfiles),
        to = destination,
        overwrite = T)

}

