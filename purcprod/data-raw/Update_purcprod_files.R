data_files <- c('mini_purcprod_targ.rds', 'coverage.rds')

for(pfiles in data_files) {

    file.copy(
        from = file.path("../../fisheyedataprep/dataprep_Purcprod", pfiles),
        to = file.path(here::here(), 'data-raw', pfiles),
        overwrite = T)

}



