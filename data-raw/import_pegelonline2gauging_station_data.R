################################################################################
# import_pegelonline2gauging_station_data.R
#
# author: arnd.weber@bafg.de
# date:   24.07.2019
#
# purpose:
#   - query pegelonline.wsv.de for the gauging_station_data along the Elbe 
#     estuary
#
################################################################################
# load required packages
library(httr2)
library(jsonlite)
library(RPostgreSQL)

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
    x <- gsub("Ä", "AE", x)
    x <- gsub("ä", "ae", x)
    x <- gsub("Ö", "OE", x)
    x <- gsub("ö", "oe", x)
    x <- gsub("Ü", "UE", x)
    x <- gsub("ü", "ue", x)
    return(x)
}

river <- "ELBE"

# specify the kilometers
if (river == "WESER") {
    fr <- 0
    to <- 363
}

if (river == "ELBE") {
    fr <- 585.990
    to <- 724.000
}

center <- (to + fr) / 2
radius <- (to - fr) / 2

# construct the urls
station_url <- paste0("http://www.pegelonline.wsv.de/webservices/rest-api/v2/s",
                      "tations.json?waters=", river, "&km=",
                      as.character(center), "&radius=", as.character(radius))
waters_url <- "http://www.pegelonline.wsv.de/webservices/rest-api/v2/stations/"

# query pegelonline.wsv.de
get_stations <- GET(station_url)
get_stations_text <- content(get_stations, "text")
get_waters_text <- content(GET(waters_url), "text")
get_waters_json <- fromJSON(get_waters_text, flatten = TRUE)
df.waters <- as.data.frame(get_waters_json)

# load json data
get_stations_json <- fromJSON(get_stations_text, flatten = TRUE)
df.stations <- as.data.frame(get_stations_json)

if (river == "WESER") {
    # remove stations downstream of the Weserwehr (km 362.15)
    df.stations <- df.stations[which(df.stations$km < 362.15), ]
    df.stations <- df.stations[which(df.stations$km > 115.1 | 
                                         (! df.stations$agency %in% 
                                              c("WSA BREMEN",
                                                "WSA BREMERHAVEN"))), ]
    df.stations <- df.stations[order(df.stations$km), ]
    
    print(paste0("A total of ", as.character(nrow(df.stations)), " gauging sta",
                 "tions were found for the specified river section."))
    
    # add confluence and Weserwehr to df.stations
    df.stations <- rbind(data.frame(uuid = NA_character_,
                                    number = NA_character_, 
                                    shortname = "GRENZE_HM",
                                    longname = "GRENZE_HM",
                                    km = 0, agency = NA_character_, 
                                    longitude = NA_real_, latitude = NA_real_,
                                    water.shortname = river, 
                                    water.longname = river,
                                    stringsAsFactors = FALSE),
                         df.stations, stringsAsFactors = FALSE)
    df.stations <- rbind(df.stations, 
                         data.frame(uuid = NA_character_,
                                    number = NA_character_,
                                    shortname = "WESERWEHR",
                                    longname = "WESERWEHR",
                                    km = 362.15, agency = NA_character_, 
                                    longitude = NA_real_, latitude = NA_real_,
                                    water.shortname = river, 
                                    water.longname = river,
                                    stringsAsFactors = FALSE),
                         stringsAsFactors = FALSE)
}

if (river == "ELBE") {
    # remove stations downstream of the Weserwehr (km 362.15)
    df.stations <- df.stations[which(df.stations$km < 725), ]
    df.stations <- df.stations[which(df.stations$km > 585.7), ]
    df.stations <- df.stations[order(df.stations$km), ]
    df.stations <- df.stations[which(!df.stations$shortname %in%
        c("HAMBURG-HARBURG", "CRANZ", "SCHÖPFSTELLE", "HAHNÖFER SAND WEST SIEL",
          "D1 HANSKALBSAND OBERFLÄCHE", "HETLINGEN", "TWIELENFLETH SIEL",
          "D2 JUELSSAND OBERFLÄCHE", "D2 Juelssand OBERFLÄCHE",
          "PINNAU-SPERRWERK AP", "GRAUERORT REEDE",
          "KRÜCKAU-SPERRWERK AP", "KRAUTSAND REEDE",
          "D4 RHINPLATE-NORD OBERFLÄCHE", "STÖR-SPERRWERK AP",
          "SCHÖNEWORTH SIEL")), ]
    
    # add Nordseepegel (MITTELGRUND, SCHARHÖRN, BAKE Z)
    station_url <- paste0("http://www.pegelonline.wsv.de/webservices/rest-api/",
                          "v2/stations.json?ids=MITTELGRUND,SCHARH%C3%96RN,BAK",
                          "E%20Z")
    get_stations <- GET(station_url)
    get_stations_text <- content(get_stations, "text")
    get_stations_json <- fromJSON(get_stations_text, flatten = TRUE)
    df.stations_add <- as.data.frame(get_stations_json)
    df.stations_add$water.shortname <- rep(river, nrow(df.stations_add))
    df.stations_add$water.longname <- rep(river, nrow(df.stations_add))
    
    # add Wehr Geesthacht and North Sea to df.stations
    df.stations <- rbind(data.frame(uuid = NA_character_,
                                    number = NA_character_, 
                                    shortname = "GEESTHACHT_WEHR",
                                    longname = "GEESTHACHT_WEHR",
                                    km = 585.7, agency = NA_character_, 
                                    longitude = NA_real_, latitude = NA_real_,
                                    water.shortname = river, 
                                    water.longname = river,
                                    stringsAsFactors = FALSE),
                         df.stations, stringsAsFactors = FALSE)
    df.stations <- rbind(df.stations, df.stations_add, stringsAsFactors = FALSE)
    df.stations <- rbind(df.stations, 
                         data.frame(uuid = NA_character_,
                                    number = NA_character_,
                                    shortname = "NORTH_SEA",
                                    longname = "NORTH_SEA",
                                    km = 760, agency = NA_character_, 
                                    longitude = NA_real_, latitude = NA_real_,
                                    water.shortname = river, 
                                    water.longname = river,
                                    stringsAsFactors = FALSE),
                         stringsAsFactors = FALSE)
    
    print(paste0("A total of ", as.character(nrow(df.stations)), " gauging sta",
                 "tions were found for the specified river section."))
}

for (i in 1:nrow(df.stations)) {
    
    write(df.stations$longname[i], stdout())
    
    # check existence of a record
    gs <- dbGetQuery(con, paste0("SELECT * FROM gauging_station_data WHERE gau",
                                 "ging_station = \'", 
                                 stringReplace(df.stations$shortname[i]), "\'"))
    
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
                       stringReplace(toupper(df.stations$longname[i])),
                       "\', NULL, NULL, NULL",
                       ", ", df.stations$km[i], ", \'", river, "\', \'", river,
                       "\', \'", toupper(df.stations$shortname[i]), "\', \'",
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
            if (length(get_station_json$characteristicValues) == 0) {
                get_station_json$characteristicValues <- NULL
                charval <- FALSE
            } else {
                charval <- TRUE
            }
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
            if (charval) {
                id_mw <- which(df.station$characteristicValues.shortname == "MW")
            } else {
                id_mw <- integer(0)
            }
            if (length(id_mw) > 0){
                mw <- df.station$characteristicValues.value[id_mw] / 100
                mw_start <- df.station$characteristicValues.timespanStart[id_mw]
                mw_end <- df.station$characteristicValues.timespanEnd[id_mw]
                mw_timespan <- paste0("\'",
                                      paste(sep = " - ", mw_start, mw_end),
                                      "\'")
            } else {
                mw <- "NULL"
                mw_timespan <- "NULL"
            }
            
            dbSendQuery(con,
                paste0("INSERT INTO public.gauging_station_data (id, gauging_s",
                       "tation, uuid, agency, number, km, water_shortname, wat",
                       "er_longname, gauging_station_shortname, gauging_statio",
                       "n_longname, longitude, latitude, mw, mw_timespan, pnp,",
                       " data_present, data_present_timespan, km_qpf, km_qps) ",
                       "VALUES (DEFAULT, \'",
                       stringReplace(toupper(df.stations$longname[i])),
                       "\', \'", df.stations$uuid[i], "\', \'",
                       stringReplace(toupper(df.stations$agency[i])), "\', \'",
                       df.stations$number[i], "\', ",
                       df.stations$km[i], ", \'",
                       toupper(df.stations$water.shortname[i]), "\', \'",
                       toupper(df.stations$water.longname[i]), "\', \'",
                       toupper(df.stations$shortname[i]), "\', \'",
                       toupper(df.stations$longname[i]), "\', ",
                       df.stations$longitude[i], ", ",
                       df.stations$latitude[i], ", ",
                       mw, ", ",
                       mw_timespan, ", ",
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
                       stringReplace(toupper(df.stations$longname[i])), "\'"))
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
            if (length(id_mw) > 0){
                mw <- df.station$characteristicValues.value[id_mw] / 100
                mw_start <- df.station$characteristicValues.timespanStart[id_mw]
                mw_end <- df.station$characteristicValues.timespanEnd[id_mw]
                mw_timespan <- paste0("\'",
                                      paste(sep = " - ", mw_start, mw_end),
                                      "\'")
            } else {
                mw <- "NULL"
                mw_timespan <- "NULL"
            }
            
            dbSendQuery(con,
                paste0("UPDATE public.gauging_station_data SET ", 
                       "uuid = \'", df.stations$uuid[i], "\', ",
                       "agency = \'",
                       stringReplace(toupper(df.stations$agency[i])), "\', ",
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
                       "mw_timespan = ", mw_timespan, ", ",
                       "pnp = ", pnp, ", ",
                       "km_qpf = ", df.stations$km[i], ", ",
                       "km_qps = ", df.stations$km[i],
                       " WHERE gauging_station = \'",
                       stringReplace(toupper(df.stations$longname[i])), "\'"))
        }
    }
}

# dbWriteTable(con, "gauging_station_data", df.stations, append = TRUE,
#              row.names = TRUE)

dbDisconnect(con)

q("no")
