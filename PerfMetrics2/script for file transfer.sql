-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

-- ! All files to dev
pscp -r C:/Users/Ashley.Vizek/Documents/fisheye-mini-purcprod/  avizek@nwcrweb.nwfsc.noaa.gov:/srv/shiny-server/fisheye/PerfMetrics2

-- ! All files to prod
-- !pscp -r C:/Users/Ashley.Vizek/Documents/fisheye-mini-purcprod/  avizek@nwcshiny.nwfsc.noaa.gov:/srv/shiny-server/fisheye/