###################################################
# import_M22gauging_data.R
#
# author: arnd.weber@bafg.de
# date:   17.07.2019
#
# purpose: 
#   - manually import missing data from M2 into the gauging_data database
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

# base path
base <- paste0("/home/WeberA/freigaben/M/M2/daten/orig/hyd/pegelonline/downloa",
               "d_wget/Wasserstand+Rohdaten/")

# choose dates to be imported
from <- as.Date("2019-05-01")
to <- as.Date("2019-05-31")
dates <- as.character(seq.Date(from, to , by = "1 day"))

for (river in c("ELBE", "RHEIN")) {
    gs <- dbGetQuery(con, paste0("SELECT gauging_station FROM gauging_station_",
                                 "data WHERE water_longname = \'", river, "\' ",
                                 "AND data_present"))$gauging_station
    # if (river == "ELBE") {
    #     gs <- c("MAGDEBURG-ROTHENSEE", "NIEGRIPP AP", "NEU DARCHAU")
    # } else {
    #     gs <- c("SANKT GOAR", "RUHRORT")
    # }
    for (a_gs in gs) {
        write(a_gs, stdout())
        
        # MAGDEBURG-ROTHENSEE
        # NIEGRIPP+AP
        # NEU+DARCHAU
        # DUISBURG-RUHRORT
        if (a_gs == "MAGDEBURG-ROTHENSEE") {
            b_gs <- "ROTHENSEE"
        } else if (a_gs == "NIEGRIPP AP") {
            b_gs <- "NIEGRIPP+AP"
        } else if (a_gs == "NEU DARCHAU") {
            b_gs <- "NEU+DARCHAU"
        } else if (a_gs == "RUHRORT") {
            b_gs <- "DUISBURG-RUHRORT"
        } else if (a_gs == "SANKT GOAR") {
            b_gs <- "SANKT+GOAR"
        } else {
            b_gs <- a_gs
        }
        write(b_gs, stdout())
        
        for (a_date in dates) {
            b_date <- as.Date(a_date)
            write(paste(" ", a_date), stdout())
            w <- dbGetQuery(con, paste0("SELECT w FROM gauging_data WHERE gaug",
                                        "ing_station = \'", a_gs, "\' and date",
                                        " = \'", a_date, "\'"))$w
            if (length(w) == 0) {
                write(paste("   w is missing"), stdout())
                destfile <- paste0(base, river, "/", b_gs, "/",
                                   strftime(b_date, "%d.%m.%Y"), "/down.csv")
                if (file.exists(destfile)) {
                    write(paste("    file found"), stdout())
                    # data
                    df.data <- read.table(destfile, header = FALSE, sep = ";",
                                          skip = 1, na.strings = "XXX,XXX")
                    
                    # calculate daily mean
                    w <- round(mean(as.numeric(df.data$V2), na.rm = TRUE), 0)
                    
                    # insert data into the gauging_data table
                    dbSendQuery(con, paste0("INSERT INTO public.gauging_data (",
                                            "id, gauging_station, date, year, ",
                                            "month, day, w) VALUES (DEFAULT, ",
                                            "\'", a_gs, "\', \'",
                                            strftime(b_date, "%Y-%m-%d"),
                                            "\', ",
                                            strftime(b_date, "%Y"), ", ",
                                            strftime(b_date, "%m"), ", ",
                                            strftime(b_date, "%d"), ", ", w,
                                            ")"))
                    
                    # delete row(s) from gauging_data_missing table
                    query_str4 <- paste0("SELECT * FROM public.gauging_data_mi",
                                         "ssing WHERE gauging_station = \'",
                                         a_gs, "\' AND date = \'",
                                         strftime(b_date, "%Y-%m-%d"), "\'")
                    if (nrow(dbGetQuery(con, query_str4))  > 0) {
                        dbSendQuery(con, paste0("DELETE FROM public.gauging_da",
                                                "ta_missing WHERE gauging_stat",
                                                "ion = \'", a_gs, "\' AND",
                                                " date = \'",
                                                strftime(b_date, "%Y-%m-%d"),
                                                "\'"))
                    }
                } else {
                    write("    file not found", stdout())
                }
            }
        }
    }
}

#dbGetQuery(con, "SELECT * FROM gauging_data WHERE gauging_station = 'SANKT GOAR' AND (date >= '2019-05-01' AND date <= '2019-05-31') ORDER BY date ASC")
#unique(dbGetQuery(con, "SELECT gauging_station FROM gauging_data")$gauging_station)
