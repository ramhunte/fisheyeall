# Copy files over for Performance Metrics
perfmetricsdir <- "R:/Confidential/FISHEyE/data/PerformanceMetrics/2024-05-13"
perfmetricsfiles <- list.files(perfmetricsdir)[grepl('RData', list.files(perfmetricsdir))]

for(pfiles in perfmetricsfiles) {

    file.copy(
        from = file.path(perfmetricsdir, pfiles), 
        to = "PerformanceMetrics/data", 
        overwrite = T)

}

# Copy files over for Whiting
whitingdir <- "R:/Confidential/FISHEyE/data/Whiting/2024-05-31"
whitingfiles <- list.files(whitingdir)[grepl('RDS', list.files(whitingdir))]

for(wfiles in whitingfiles) {

    file.copy(
        from = file.path(whitingdir, whitingfiles), 
        to = "Whiting/data", 
        overwrite = T)

}
