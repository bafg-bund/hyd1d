
# get credentials
if (Sys.info()["nodename"] == "lvps46-163-72-150.dedicated.hosteurope.de") {
    gd_credentials <- credentials("/home/arnd/BfG/hyd1d/DB_credentials_gauging_data")
} else {
    gd_credentials <- credentials("/home/WeberA/hyd1d/DB_credentials_gauging_data")
}

# read the data
# access the gauging_data DB
gd_con <- DBI::dbConnect(drv      = DBI::dbDriver("PostgreSQL"),
                         host     = gd_credentials["host"], 
                         dbname   = gd_credentials["dbname"], 
                         user     = gd_credentials["user"], 
                         password = gd_credentials["password"], 
                         port     = gd_credentials["port"])

# retrieve the data
df.gauging_station_data <- DBI::dbGetQuery(gd_con, 
    paste0("SELECT id, gauging_station, uuid, agency, km, water_shortname,",
           " longitude, latitude, mw, mw_timespan, pnp, data_present, km_q",
           "ps FROM gauging_station_data ORDER BY water_shortname, km_qps"))
df.gauging_station_data$river <- df.gauging_station_data$water_shortname
df.gauging_station_data$water_shortname <- NULL

# replace non-ASCII characters
for (a in c("gauging_station", "river")){
    df.gauging_station_data[, a] <- iconv(df.gauging_station_data[, a],
                                          from = "UTF-8", to = "ASCII",
                                          sub = "byte")
}

# store df.gauging_station_data as external dataset
if (!(file.exists("data/df.gauging_station_data.rda"))) {
    devtools::use_data(df.gauging_station_data, pkg = ".", overwrite = TRUE,
                       compress = "bzip2")
} else {
    print("data/df.gauging_station_data.rda exists already")
}

# variables for RDO
RDO_NROW_DF.GAUGING_STATION_DATA <- 
    as.character(nrow(df.gauging_station_data))

# clean up
rm(gd_con, gd_credentials, a)

