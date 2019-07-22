##################################################
# import_rds2gauging_data.R
#
# author: arnd.weber@bafg.de
# date:   17.07.2019
#
# purpose: 
#   - import *.rds data into the gauging_data database
#
##################################################
# load required packages
require("DBI")
require("RPostgreSQL")
source("R/hyd1d-internal.R")

# open the connection using user, password, etc., as
credentials <- credentials("DB_credentials_gauging_data")

# access the gauging_data DB
con <- DBI::dbConnect(drv      = DBI::dbDriver("PostgreSQL"),
                      host     = credentials["host"], 
                      dbname   = credentials["dbname"], 
                      user     = credentials["user"], 
                      password = credentials["password"], 
                      port     = credentials["port"])

# read *.RDS-file into a vector
t <- tempfile()
download.file(paste0("https://www.aqualogy.de/wp-content/uploads/bfg/df.gaugin",
                     "g_data_latest.RDS"), t)
df.gauging_data <- readRDS(t)
unlink(t)
df.gauging_data <- df.gauging_data[which(df.gauging_data$date >= as.Date("2017-01-01")), ]
df.gauging_data$gauging_station <- asc2utf8(df.gauging_data$gauging_station)

# gauging_stations present in the DB
gauging_stations <- dbGetQuery(con, paste0("SELECT gauging_station FROM public",
                                           ".gauging_station_data WHERE data_p",
                                           "resent"))$gauging_station

# loop through the *.zrx-files
for(a_gs in gauging_stations){
    
    print(a_gs)
    
    df.db <- dbGetQuery(con, paste0("SELECT * FROM gauging_data WHERE gauging_",
                                    "station = \'", a_gs, "\' AND date >= '201",
                                    "7-01-01' ORDER BY date ASC"))
    df.gd <- df.gauging_data[which(df.gauging_data$gauging_station == a_gs), ]
    
    for (i in 1:nrow(df.gd)) {
        write(as.character(df.gd$date[i]), stdout())
        if (df.gd$date[i] %in% df.db$date) {
            j <- which(df.db$date == df.gd$date[i])
            if (df.gd$w[i] != df.gd$w[j]) {
                dbSendQuery(con, paste0("UPDATE public.gauging_data SET w = ",
                                        df.gd$w[i], " WHERE gauging_station = ",
                                        "\'", a_gs, "\' AND date = \'",
                                        df.gd$date[i], "\'"))
                write("  Updated", stdout())
            } else {
                write("  Skipped", stdout())
            }
        } else {
            dbSendQuery(con, paste0("INSERT INTO public.gauging_data (id, ",
                                    "gauging_station, date, year, month, d",
                                    "ay, w) VALUES (DEFAULT, \'",
                                    a_gs, "\', \'",
                                    df.gd$date[i], "\', ",
                                    as.character(strftime(df.gd$date[i], "%Y")),
                                    ", ",
                                    as.integer(strftime(df.gd$date[i], "%m")),
                                    ", ",
                                    as.integer(strftime(df.gd$date[i], "%d")),
                                    ", ",
                                    round(df.gd$w[i], 0), ")"))
            write("  Inserted", stdout())
        }
    }
}

q("no")
