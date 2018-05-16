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
#query_string <- paste0("SELECT * FROM gauging_data WHERE date >= '1990-01-",
#                       "01' ORDER BY gauging_station, date")
query_string <- paste0("SELECT gauging_station, date, w FROM gauging_data ",
                       "WHERE date >= '1990-01-01' ORDER BY ",
                       "gauging_station, date")
df.gauging_data <- DBI::dbGetQuery(gd_con, query_string)

# replace non-ASCII characters
df.gauging_data$gauging_station <- iconv(df.gauging_data$gauging_station,
                                         from = "UTF-8", to = "ASCII", 
                                         sub = "byte")

# store df.gauging_data as external dataset
devtools::use_data(df.gauging_data, pkg = ".", overwrite = TRUE,
                   compress = "bzip2")

# variables for RDO
RDO_NROW_DF.GAUGING_DATA <- as.character(nrow(df.gauging_data))

# clean up
rm(gd_con, query_string, df.gauging_data)

# Test 1:
# - remove redundant columns
#df.gauging_data_test1 <- df.gauging_data[, c(2,3,7)]
#devtools::use_data(df.gauging_data_test1, pkg = ".",
#                   overwrite = TRUE, compress = "bzip2")
#
# Test2:
# - convert character columns to factor
#df.gauging_data_test2 <- df.gauging_data_test1
#df.gauging_data_test2$gauging_station <- 
#    as.factor(df.gauging_data_test2$gauging_station)
#devtools::use_data(df.gauging_data_test2, pkg = ".",
#                   overwrite = TRUE, compress = "bzip2")
#

