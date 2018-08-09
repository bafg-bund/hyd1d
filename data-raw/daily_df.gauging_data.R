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

# configure output
verbose <- TRUE
quiet <- !verbose

# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")

# output paths
downloads <- paste0("public/", R_version, "/downloads")
dir.create(downloads, verbose, TRUE)

# check the existence of resulting datasets and time
from <- paste0(downloads, "/df.gauging_data_latest.rda")
to <- paste0(downloads, "/df.gauging_data_", as.character(Sys.Date() - 2), 
             ".rda")

hour <- as.numeric(strftime(Sys.time(), "%H"))

if (file.exists(from) & !(file.exists(to)) & hour >= 6 & hour < 7){
    
    write(paste0(downloads, "/df.gauging_data_latest.rda will be produced"), 
          stderr())
    
    # load required packages
    require(devtools, lib.loc = lib)
    require(DBI, lib.loc = lib)
    require(RPostgreSQL, lib.loc = lib)
    
    # source hyd1d-internal to obtain the credentials function
    source("R/hyd1d-internal.R")
    
    # read the data
    # get DB credentials
    credentials <- credentials("/home/WeberA/hyd1d/DB_credentials_gauging_data")
    
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
    
    # rename yesterdays version to the day before yesterday (latest date in the
    # dataset)
    file.rename(from = from, to = to)
    
    # store df.gauging_data
    save(df.gauging_data, file = from, compress = "bzip2")
    
} else {
    write(paste0("It is not the time to produce ", downloads, "/df.gauging_dat",
                 "a_latest.rda or it has been produced already."), 
          stderr())
}

# exit R
q("no")
