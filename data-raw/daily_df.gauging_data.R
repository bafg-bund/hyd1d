##################################################
# daily_df.gauging_data.R
#
# author: arnd.weber@bafg.de
# date:   21.06.2018
#
# purpose: 
#   - export gauging data stored in the gauging_data DB into an R data file
#
##################################################

# output paths
downloads <- "docs/downloads"
dir.create(downloads, FALSE, TRUE)

write(paste0(downloads, "/df.gauging_data_latest.RDS will be produced"), 
      stdout())

# check the existence of resulting datasets and time
from <- paste0(downloads, "/df.gauging_data_latest.RDS")
if (file.exists(from)) {
    from_mtime <- strftime(file.mtime(from), format = "%Y-%m-%d")
    to <- paste0(downloads, "/df.gauging_data_", from_mtime, ".RDS")
    if (file.exists(to)) {
        write(paste0(to, " exists already and will be replaced!"), 
              stderr())
        file.remove(to)
    }
    file.rename(from = from, to = to)
}
from2 <- paste0(downloads, "/df.gauging_data_latest_v2.RDS")
if (file.exists(from2)) {
    from2_mtime <- strftime(file.mtime(from2), format = "%Y-%m-%d")
    to2 <- paste0(downloads, "/df.gauging_data_", from2_mtime, "_v2.RDS")
    if (file.exists(to2)) {
        write(paste0(to2, " exists already and will be replaced!"), 
              stderr())
        file.remove(to2)
    }
    file.rename(from = from2, to = to2)
}

# load required packages
require(devtools)
require(DBI)
require(RPostgreSQL)

# source hyd1d-internal to obtain the credentials function
source("R/hyd1d-internal.R")

# read the data
# get DB credentials
credentials <- credentials("DB_credentials_gauging_data")

# access the gauging_data DB
con <- DBI::dbConnect(drv      = DBI::dbDriver("PostgreSQL"),
                      host     = credentials["host"], 
                      dbname   = credentials["dbname"], 
                      user     = credentials["user"], 
                      password = credentials["password"], 
                      port     = credentials["port"])

# retrieve the data
query_string <- paste0("SELECT gauging_station, date, w FROM gauging_data WHER",
                       "E date >= '1960-01-01' ORDER BY gauging_station, date")
df.gauging_data <- DBI::dbGetQuery(con, query_string)

# store df.gauging_data
saveRDS(df.gauging_data, file = from, version = 3, compress = "bzip2")
saveRDS(df.gauging_data, file = from2, version = 2, compress = "bzip2")

# exit R
q("no")
