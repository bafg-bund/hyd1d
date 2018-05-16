require(devtools)
require(DBI)
require(RPostgreSQL)

# source hyd1d-internal to obtain the credentials function
source("R/hyd1d-internal.R")

if (Sys.info()["nodename"] == "lvps46-163-72-150.dedicated.hosteurope.de") {
    credentials <- credentials("/home/arnd/BfG/hyd1d/DB_credentials_gauging_data")
} else {
    credentials <- credentials("/home/WeberA/hyd1d/DB_credentials_gauging_data")
}

# read the data
# access the gauging_data DB
gd_con <- DBI::dbConnect(drv      = DBI::dbDriver("PostgreSQL"),
                         host     = credentials["host"], 
                         dbname   = credentials["dbname"], 
                         user     = credentials["user"], 
                         password = credentials["password"], 
                         port     = credentials["port"])

# retrieve the data
df.gauging_station_data <- DBI::dbGetQuery(gd_con, 
                                           paste0("SELECT id, gauging_station,",
                                                  "uuid, agency, km, water_sh",
                                                  "ortname, longitude, latitu",
                                                  "de, mw, mw_timespan, pnp, ",
                                                  "data_present, km_qps FROM ",
                                                  "gauging_station_data ",
                                                  "ORDER BY water_shortname, ",
                                                  "km_qps"))
df.gauging_station_data$river <- df.gauging_station_data$water_shortname
df.gauging_station_data$water_shortname <- NULL

# replace non-ASCII characters
for (a in c("gauging_station", "river")){
    df.gauging_station_data[, a] <- iconv(df.gauging_station_data[, a],
                                          from = "UTF-8", to = "ASCII",
                                          sub = "byte")
}

# store df.gauging_station_data as external dataset
devtools::use_data(df.gauging_station_data, pkg = ".", overwrite = TRUE,
                   compress = "bzip2")

# variables for RDO
RDO_NROW_DF.GAUGING_STATION_DATA <- as.character(nrow(df.gauging_station_data))

# clean up
rm(gd_con, credentials, a)
