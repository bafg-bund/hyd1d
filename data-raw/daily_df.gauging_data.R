##################################################
# daily_df.gauging_data.R
#
# author: arnd.weber@bafg.de
# date:   15.05.2018
#
# purpose: 
#   - export gauging data stored in the gauging_data DB into an R data file
#
##################################################

# make this script executable only on aqualogy-cloud.de
if (Sys.info()["nodename"] != "lvps46-163-72-150.dedicated.hosteurope.de") {
    print("This script has to be executed on aqualogy-cloud.de!")
    q("no")
}

# configure output
verbose <- TRUE

# load required packages
require(devtools)
require(DBI)
require(RPostgreSQL)

# source hyd1d-internal to obtain the credentials function
source("R/hyd1d-internal.R")

# read the data
# get DB credentials
credentials <- credentials("/home/arnd/BfG/hyd1d/DB_credentials_gauging_data")

# access the gauging_data DB
con <- DBI::dbConnect(drv      = DBI::dbDriver("PostgreSQL"),
                      host     = credentials["host"], 
                      dbname   = credentials["dbname"], 
                      user     = credentials["user"], 
                      password = credentials["password"], 
                      port     = credentials["port"])

# retrieve the data
query_string <- paste0("SELECT gauging_station, date, w FROM gauging_data ",
                       "WHERE date >= '1990-01-01' ORDER BY ",
                       "gauging_station, date")
df.gauging_data <- DBI::dbGetQuery(con, query_string)

# replace non-ASCII characters
df.gauging_data$gauging_station <- iconv(df.gauging_data$gauging_station,
                                         from = "UTF-8", to = "ASCII", 
                                         sub = "byte")

# rename yesterdays version to the day before yesterday (latest date in the dataset)
dir.create("public/downloads", showWarnings = FALSE, recursive = TRUE, 
           mode = "0755")
from <- "public/downloads/df.gauging_data_latest.rda"
to <- paste0("public/downloads/df.gauging_data_", as.character(Sys.Date() - 2), 
             ".rda")
if (file.exists(from) & !(file.exists(to))){
    file.rename(from = from, to = to)
}

# store df.gauging_data
save(df.gauging_data, file = from, compress = "bzip2")

# exit R
q("no")
