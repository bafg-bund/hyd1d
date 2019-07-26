# load the necessary packages
library(shiny)
library(DBI)
library(RPostgreSQL)
library(sp)
library(rgdal)
library(plotrix)

# access and query the gauging_data DB
# extract DB credentials from unversioned credential files
# 
credentials <- function(file) {
    if (file.exists(file)) {
        credentials_temp <- utils::read.table(file, header = FALSE, sep = ";", 
                                              stringsAsFactors = FALSE)
    } else {
        if (file.exists(basename(file))) {
            credentials_temp <- utils::read.table(file = basename(file), 
                                                  header = FALSE, sep = ";", 
                                                  stringsAsFactors = FALSE)
        } else {
            stop("'file' could not be found")
        }
    }
    
    credentials <- credentials_temp$V2
    names(credentials) <- credentials_temp$V1
    return(credentials)
}
credentials <- credentials("DB_credentials_gauging_data")
p_con <- dbConnect(drv      = RPostgreSQL::PostgreSQL(),
                   dbname   = credentials["dbname"],
                   host     = credentials["host"],
                   user     = credentials["user"],
                   password = credentials["password"])

# yesterday
yesterday <- Sys.Date() - 1

# rivers
rivers <- dbGetQuery(p_con, "SELECT DISTINCT water_longname FROM public.gauging_station_data")[,1]

# for some basic information
df.gauging_stations <- dbGetQuery(p_con, "SELECT * FROM public.gauging_station_data WHERE data_present IS TRUE")
spdf.gauging_stations <- SpatialPointsDataFrame(coords      = df.gauging_stations[,c("longitude", "latitude")],
                                                data        = df.gauging_stations, 
                                                proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# the gauging_data after 1989
df.gauging_data    <- dbGetQuery(p_con, "SELECT * FROM public.gauging_data WHERE year >= 1990 ORDER BY date ASC")
