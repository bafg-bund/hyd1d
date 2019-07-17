##################################################
# gauging_data.R
#
# author: arnd.weber@bafg.de
# date:   17.07.2019
#
# purpose: 
#   - import *.zrx data into the gauging_data database
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

# read *.zrx-files into a vector
files_e <- list.files(path = paste0("/home/WeberA/freigaben/U/U3/Auengruppe_IN",
                                    "FORM/EL_000_586_UFD/data/w/prep/2017"),
                      pattern = "*.zrx", full.names = TRUE, recursive = TRUE)
files_r <- list.files(path = paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFO",
                                  "RM/RH_336_867_UFD/data/w/prep/2017"),
                      pattern = "*.zrx", full.names = TRUE, recursive = TRUE)
files <- c(files_e, files_r)

# gauging_stations present in the DB
gauging_stations <- dbGetQuery(con, paste0("SELECT gauging_station FROM public",
                                           ".gauging_station_data WHERE data_p",
                                           "resent"))$gauging_station

# loop through the *.zrx-files
for(a_file in files){
    
    # get gauging_station name from file name
    path_components <- unlist(strsplit(a_file, "/"))
    gs <- path_components[length(path_components)]
    gauging_station <- toupper(substr(gs, 1, nchar(gs) - 6))
    if (gauging_station %in% gauging_stations) {
        write(gauging_station, stdout())
        
        # read zrx into data.frame
        df <- read.table(file = a_file, col.names = c("date", "W", "rem"),
                         colClasses=c("character", "numeric", "character"),
                         skip = 5, comment.char = "#", sep=" ", 
                         blank.lines.skip = TRUE, header = FALSE)
        df$rem <- NULL
        
        # remove all rows with NA values (-777)
        id_na <- which(df$W == -777)
        if(length(id_na) > 0){
            df <- df[-id_na,]
        }
        
        # restructure df
        df_db <- data.frame(
            date = as.Date(strptime(df$date, format = "%Y%m%d%H%M%S")),
            w = df$W, 
            year = format(strptime(df$date, format = "%Y%m%d%H%M%S"), 
                          format = "%Y"),
            month = format(strptime(df$date, format = "%Y%m%d%H%M%S"),
                           format = "%m"),
            day = format(strptime(df$date, format = "%Y%m%d%H%M%S"),
                         format = "%d"))
        
        # insert or update the DB
        for(a_row in 1:nrow(df_db)){
            # check if the data exist in the DB already
            entry <- dbGetQuery(con, paste0("SELECT w FROM public.gauging_data",
                                            " WHERE gauging_station = \'", 
                                            gauging_station, "\' AND date = \'",
                                            df_db$date[a_row], "\'"))$w
            
            if(length(entry) == 0){
                dbSendQuery(con, paste0("INSERT INTO public.gauging_data (id, ",
                                        "gauging_station, date, year, month, d",
                                        "ay, w) VALUES (DEFAULT, \'",
                                        gauging_station, "\', \'",
                                        df_db$date[a_row], "\', ",
                                        as.character(df_db$year[a_row]), ", ",
                                        as.integer(df_db$month[a_row]), ", ",
                                        as.integer(df_db$day[a_row]), ", ",
                                        round(df_db$w[a_row], 0), ")"))
                write(paste0("   Inserted ", gauging_station, ", row: ", a_row,
                             ", date: ", df_db$date[a_row]), stdout())
            } else {
                if(entry != df_db$w[a_row]){
                    dbSendQuery(con, paste0("UPDATE public.gauging_data SET w ",
                                            "= ", round(df_db$w[a_row], 0),
                                            " WHERE gauging_station = \'",
                                            gauging_station, "\' AND date = \'",
                                            df_db$date[a_row], "\'"))
                    write(paste0("   Updated ", gauging_station, ", row: ",
                                 a_row, ", date: ", df_db$date[a_row]),
                          stdout())
                } else {
                    write(paste0("   Skipped ", gauging_station, ", row: ",
                                 a_row, ", date: ", df_db$date[a_row]),
                          stdout())
                }
            }
        }
    } else {
        write(paste0(gauging_station, " does not exist in the DB!"), stderr())
    }
}

q("no")
