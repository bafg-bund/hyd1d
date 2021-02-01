##################################################
# remove_Umlaute.R
#
# author: arnd.weber@bafg.de
# date:   01.02.2021
#
# purpose: 
#   - replace all Umlaute
#
##################################################
# load required packages
require("DBI")
require("RPostgreSQL")
require("RCurl")

# source hyd1d-internal to obtain the credentials function
source("R/hyd1d-internal.R")

### open the connection using user, password, etc., as
credentials <- credentials("DB_credentials_gauging_data")
con <- dbConnect("PostgreSQL",
                 host = credentials["host"],
                 dbname = credentials["dbname"],
                 user = credentials["user"],
                 password = credentials["password"],
                 port = credentials["port"])
postgresqlpqExec(con, "SET client_encoding = 'UTF-8'")

df.gauging_station_data <- DBI::dbGetQuery(con, 
                                           paste0("SELECT id, gauging_station, uuid, agency, km, water_shortname,",
                                                  " longitude, latitude, mw, mw_timespan, pnp, data_present, km_q",
                                                  "ps FROM gauging_station_data ORDER BY water_shortname, km_qps"))

orig <- c("Ä", "Ö", "Ü")
repl <- c("AE", "OE", "UE")

for (umlaut in 1:3) {
    gs <- df.gauging_station_data$gauging_station[
        grepl(orig[umlaut], df.gauging_station_data$gauging_station)]
     
    for (a_gs in gs) {
        b_gs <- gsub(orig[umlaut], repl[umlaut], a_gs)
        print(a_gs)
        DBI::dbExecute(con,
                       paste0("UPDATE gauging_station_data SET gauging_statio",
                              "n = \'", b_gs, "\' WHERE gauging_station = \'",
                              a_gs, "\'"))
        DBI::dbExecute(con,
                       paste0("UPDATE gauging_data SET gauging_station = \'",
                              b_gs, "\' WHERE gauging_station = \'", a_gs,
                              "\'"))
        DBI::dbExecute(con,
                       paste0("UPDATE gauging_data_missing SET gauging_statio",
                              "n = \'", b_gs, "\' WHERE gauging_station = \'",
                              a_gs, "\'"))
    }
}


q("no")
