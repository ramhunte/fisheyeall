# Display list of available date stamp folders for performance metrics
dir("R:/Confidential/FISHEyE/data/PerformanceMetrics")

# Copy files over for Performance Metrics
perfmetricsdir <- "R:/Confidential/FISHEyE/data/PerformanceMetrics/2024-06-03"
perfmetricsfiles <- list.files(perfmetricsdir)[grepl('RData', list.files(perfmetricsdir))]

perfmetricsfiles <- list.files(perfmetricsdir)[grepl('perfmetrics.rds', list.files(perfmetricsdir))]

destination <- ifelse(grepl('PerformanceMetrics', getwd()), "data", "Performancemetrics/data")

for(pfiles in perfmetricsfiles) {

    file.copy(
        from = file.path(perfmetricsdir, pfiles), 
        to = destination, 
        overwrite = T)

}

# Display list of available date stamp folders for whiting
dir("R:/Confidential/FISHEyE/data/Whiting")
whitingdir <- "R:/Confidential/FISHEyE/data/Whiting/2024-06-03"
whitingfiles <- list.files(whitingdir)[grepl('RDS', list.files(whitingdir))]

for(wfiles in whitingfiles) {

    file.copy(
        from = file.path(whitingdir, whitingfiles), 
        to = "Whiting", 
        overwrite = T)

}
