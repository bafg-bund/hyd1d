################################################################################
# import_pegelonline2gauging_station_data.R
#
# author: arnd.weber@bafg.de
# date:   24.07.2019
#
# purpose:
#   - query pegelonline.wsv.de for the gauging_station_data along the Elbe
#
################################################################################
# load required packages
require(httr)
require(jsonlite)
require(RPostgreSQL)

# source hyd1d-internal to obtain the credentials function
source("R/hyd1d-internal.R")

# get DB credentials
credentials <- credentials("DB_credentials_gauging_data")

# access the gauging_data DB
con <- DBI::dbConnect(drv      = DBI::dbDriver("PostgreSQL"),
                      host     = credentials["host"], 
                      dbname   = credentials["dbname"], 
                      user     = credentials["user"], 
                      password = credentials["password"], 
                      port     = credentials["port"])

# function to treat special letters in url's
stringReplace <- function(x) {
    x <- gsub(" ", "%20", x)
    x <- gsub("Ä", "%C3%84", x)
    x <- gsub("ä", "%C3%A4", x)
    x <- gsub("Ö", "%C3%96", x)
    x <- gsub("ö", "%C3%B6", x)
    x <- gsub("Ü", "%C3%9C", x)
    x <- gsub("ü", "%C3%BC", x)
    return(x)
}

# specify the kilometers
fr <- 0
to <- 363

center <- (to + fr) / 2
radius <- (to - fr) / 2

# construct the urls
station_url <- paste0("http://www.pegelonline.wsv.de/webservices/rest-api/v2/s",
                      "tations.json?waters=WESER&km=", as.character(center),
                      "&radius=", as.character(radius))
waters_url <- "http://www.pegelonline.wsv.de/webservices/rest-api/v2/stations/"

# query pegelonline.wsv.de
get_stations <- GET(station_url)
get_stations_text <- content(get_stations, "text")

# load json data
get_stations_json <- fromJSON(get_stations_text, flatten = TRUE)
df.stations <- as.data.frame(get_stations_json)

# remove stations downstream of the Weserwehr (km 362.15)
df.stations <- df.stations[which(df.stations$km < 362.15), ]
df.stations <- df.stations[which(df.stations$km > 115.1 | 
                                 (! df.stations$agency %in% 
                                      c("WSA BREMEN", "WSA BREMERHAVEN"))), ]
df.stations <- df.stations[order(df.stations$km), ]

print(paste0("A total of ", as.character(nrow(df.stations)), " gauging station",
             "s were found for the specified river section."))

# add confluence and Weserwehr to df.stations
df.stations <- rbind(data.frame(uuid = NA_character_, number = NA_character_, 
                                shortname = "GRENZE_HM", longname = "GRENZE_HM",
                                km = 0, agency = NA_character_, 
                                longitude = NA_real_, latitude = NA_real_,
                                water.shortname = "WESER", 
                                water.longname = "WESER",
                                stringsAsFactors = FALSE),
                     df.stations, stringsAsFactors = FALSE)
df.stations <- rbind(df.stations, 
                     data.frame(uuid = NA_character_, number = NA_character_, 
                                shortname = "WESERWEHR", longname = "WESERWEHR",
                                km = 362.15, agency = NA_character_, 
                                longitude = NA_real_, latitude = NA_real_,
                                water.shortname = "WESER", 
                                water.longname = "WESER",
                                stringsAsFactors = FALSE),
                     stringsAsFactors = FALSE)

for (i in 1:nrow(df.stations)) {
    
    write(df.stations$longname[i], stdout())
    
    # check existence of a record
    gs <- dbGetQuery(con, paste0("SELECT * FROM gauging_station_data WHERE gau",
                                 "ging_station = \'", df.stations$shortname[i],
                                 "\'"))
    
    if (nrow(gs) == 0) {
        # INSERT
        if (is.na(df.stations$uuid[i])) {
            write("  INSERT NULL", stdout())
            
            dbSendQuery(con,
                paste0("INSERT INTO public.gauging_station_data (id, gauging_s",
                       "tation, uuid, agency, number, km, water_shortname, wat",
                       "er_longname, gauging_station_shortname, gauging_statio",
                       "n_longname, longitude, latitude, mw, mw_timespan, pnp,",
                       " data_present, data_present_timespan, km_qpf, km_qps) ",
                       "VALUES (DEFAULT, \'",
                       toupper(df.stations$longname[i]), "\', NULL, NULL, NULL",
                       ", ", df.stations$km[i], ", \'WESER\', \'WESER\', \'",
                       toupper(df.stations$shortname[i]), "\', \'",
                       toupper(df.stations$longname[i]), "\', NULL, NULL, NULL",
                       ", NULL, NULL, FALSE, NULL, ", df.stations$km[i], ", ",
                       df.stations$km[i], ")"))
        } else {
            write("  INSERT DATA", stdout())
            
            # get the characteristic W values
            # query pegelonline.wsv.de
            get_station <- GET(paste0(waters_url, df.stations$uuid[i], "/W.jso",
                                      "n?includeCharacteristicValues=true"))
            get_station_text <- content(get_station, "text")
            
            # load json data
            get_station_json <- fromJSON(get_station_text, flatten = TRUE)
            df.station <- as.data.frame(get_station_json)
            
            # PNP
            pnp <- df.station$gaugeZero.value[1]
            if (is.null(pnp)) {
                if (df.stations$longname[i] == "STOLZENAU") {
                    pnp <- 23.52
                }
                if (df.stations$longname[i] == "HOYA") {
                    pnp <- 11.17
                }
            }
            
            # MW
            id_mw <- which(df.station$characteristicValues.shortname == "MW")
            mw <- df.station$characteristicValues.value[id_mw] / 100
            mw_start <- df.station$characteristicValues.timespanStart[id_mw]
            mw_end <- df.station$characteristicValues.timespanEnd[id_mw]
            mw_timespan <- paste(sep = " - ", mw_start, mw_end)
            
            dbSendQuery(con,
                paste0("INSERT INTO public.gauging_station_data (id, gauging_s",
                       "tation, uuid, agency, number, km, water_shortname, wat",
                       "er_longname, gauging_station_shortname, gauging_statio",
                       "n_longname, longitude, latitude, mw, mw_timespan, pnp,",
                       " data_present, data_present_timespan, km_qpf, km_qps) ",
                       "VALUES (DEFAULT, \'",
                       toupper(df.stations$longname[i]), "\', \'",
                       df.stations$uuid[i], "\', \'",
                       toupper(df.stations$agency[i]), "\', \'",
                       df.stations$number[i], "\', ",
                       df.stations$km[i], ", \'",
                       toupper(df.stations$water.shortname[i]), "\', \'",
                       toupper(df.stations$water.longname[i]), "\', \'",
                       toupper(df.stations$shortname[i]), "\', \'",
                       toupper(df.stations$longname[i]), "\', ",
                       df.stations$longitude[i], ", ",
                       df.stations$latitude[i], ", ",
                       mw, ", \'",
                       mw_timespan, "\', ",
                       pnp, ", ",
                       "TRUE, ",
                       "\'", paste(sep = " - ", Sys.Date() - 31,
                                   Sys.Date() - 1), "\', ",
                       df.stations$km[i], ", ",
                       df.stations$km[i], ")"))
        }
    } else {
        # UPDATE
        if (is.na(df.stations$uuid[i])) {
            write("  UPDATE NULL", stdout())
            
            dbSendQuery(con,
                paste0("UPDATE public.gauging_station_data SET ", 
                       "uuid = NULL, ",
                       "agency = NULL, ",
                       "number = NULL, ",
                       "km = ", df.stations$km[i], ", ",
                       "water_shortname = \'WESER\', ",
                       "water_longname = \'WESER\', ",
                       "gauging_station_shortname = \'",
                           toupper(df.stations$shortname[i]), "\', ",
                       "gauging_station_longname = \'",
                           toupper(df.stations$longname[i]), "\', ",
                       "longitude = NULL, ",
                       "latitude = NULL, ",
                       "mw = NULL, ",
                       "mw_timespan = NULL, ",
                       "pnp = NULL, ",
                       "km_qpf = ", df.stations$km[i], ", ",
                       "km_qps = ", df.stations$km[i],
                       " WHERE gauging_station = \'",
                           toupper(df.stations$longname[i]), "\'"))
        } else {
            write("  UPDATE DATA", stdout())
            
            # get the characteristic W values
            # query pegelonline.wsv.de
            get_station <- GET(paste0(waters_url, df.stations$uuid[i], "/W.jso",
                                      "n?includeCharacteristicValues=true"))
            get_station_text <- content(get_station, "text")
            
            # load json data
            get_station_json <- fromJSON(get_station_text, flatten = TRUE)
            df.station <- as.data.frame(get_station_json)
            
            # PNP
            pnp <- df.station$gaugeZero.value[1]
            if (is.null(pnp)) {
                if (df.stations$longname[i] == "STOLZENAU") {
                    pnp <- 23.52
                }
                if (df.stations$longname[i] == "HOYA") {
                    pnp <- 11.169
                }
            }
            
            # MW
            id_mw <- which(df.station$characteristicValues.shortname == "MW")
            mw <- df.station$characteristicValues.value[id_mw] / 100
            mw_start <- df.station$characteristicValues.timespanStart[id_mw]
            mw_end <- df.station$characteristicValues.timespanEnd[id_mw]
            mw_timespan <- paste(sep = " - ", mw_start, mw_end)
            
            dbSendQuery(con,
                paste0("UPDATE public.gauging_station_data SET ", 
                       "uuid = \'", df.stations$uuid[i], "\', ",
                       "agency = \'", toupper(df.stations$agency[i]), "\', ",
                       "number = \'", df.stations$number[i], "\', ",
                       "km = ", df.stations$km[i], ", ",
                       "water_shortname = \'WESER\', ",
                       "water_longname = \'WESER\', ",
                       "gauging_station_shortname = \'",
                           toupper(df.stations$shortname[i]), "\', ",
                       "gauging_station_longname = \'",
                           toupper(df.stations$longname[i]), "\', ",
                       "longitude = ", df.stations$longitude[i], ", ",
                       "latitude = ", df.stations$latitude[i], ", ",
                       "mw = ", mw, ", ",
                       "mw_timespan = \'", mw_timespan, "\', ",
                       "pnp = ", pnp, ", ",
                       "km_qpf = ", df.stations$km[i], ", ",
                       "km_qps = ", df.stations$km[i],
                       " WHERE gauging_station = \'",
                           toupper(df.stations$longname[i]), "\'"))
        }
    }
}

# dbWriteTable(con, "gauging_station_data", df.stations, append = TRUE,
#              row.names = TRUE)

dbDisconnect(con)

q("no")
