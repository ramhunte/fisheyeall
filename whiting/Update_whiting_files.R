

# Display list of available date stamp folders for whiting
dir("R:/Confidential/FISHEyE/data/Whiting")
whitingdir <- "R:/Confidential/FISHEyE/data/Whiting/2024-12-09"
whitingfiles <- list.files(whitingdir)[grepl('RDS', list.files(whitingdir))]

try(if(length(whitingfiles) == 0) stop("There aren't any files to move in that folder"))

# copy all of the whiting files from fisheyedataprep output folder to app folder
for(wfiles in whitingfiles) {

    file.copy(
        from = file.path(whitingdir, whitingfiles),
        to  = here::here("whiting"),
        overwrite = T)

}

# copy gdp_defl from the performance metrics app into whiting app
file.copy(
        from = file.path("PerformanceMetrics/data/gdp_defl.RData"),
        to = "Whiting",
        overwrite = T)
