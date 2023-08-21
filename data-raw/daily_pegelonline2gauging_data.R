##################################################
# daily_pegelonline2gauging_data.R
#
# author: arnd.weber@bafg.de
# date:   21.06.2018
#
# purpose: 
#   - download gauging data from pegelonline.wsv.de to r.bafg.de
#   - write them into the postgresql database
#
##################################################
write("gauging_data are queried from pegelonline.wsv.de", stderr())

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

###
# get the rivers and gauging_stations to download data from
df.gs <- dbGetQuery(con, paste0("SELECT gauging_station, gauging_station_short",
                                "name, water_longname, pnp, data_present_times",
                                "pan FROM public.gauging_station_data WHERE da",
                                "ta_present IS TRUE ORDER BY id ASC"))

###
# produce a vector of dates to be downloaded
# set constant variables
days_back <- 8
req_dates <- as.character(seq(Sys.Date() - days_back, Sys.Date() - 1,
                              length.out = days_back))

###
# process the data
i <- 1
for(a_gs in df.gs$gauging_station) {
    
    b_gs <- df.gs$gauging_station_shortname[i]
    c_gs <- gsub(" ", "+", b_gs)
    # d_gs <- simpleCap(b_gs)
    # e_gs <- gsub(" ", "+",
    #              gsub("Ä", "%C4",
    #                   gsub("Ö", "%D6",
    #                        gsub("Ü", "%DC",
    #                             gsub("ä", "%E4",
    #                                  gsub("ö", "%F6",
    #                                       gsub("ü", "%FC",
    #                                            d_gs)))))))
    
    # obtain the present range of available data for a_gs
    date_range_present <- as.Date(unlist(strsplit(
        df.gs$data_present_timespan[i], " - ")))
    
    for(a_date in req_dates) {
        
        #####
        # check for existing entries
        query_str1 <- paste0("SELECT w FROM public.gauging_data WHERE gauging_",
                             "station = \'", a_gs, "\' AND date = \'",
                             strftime(a_date, "%Y-%m-%d"), "\'")
        if(nrow(dbGetQuery(con, query_str1)) == 1) {
            write(paste(sep=" ", a_gs, a_date, "existiert bereits"),
                  stderr())
            
            # delete accidentally present entries in gauging_data_missing
            query_str2 <- paste0("SELECT * FROM public.gauging_data_missing WH",
                                 "ERE gauging_station = \'", a_gs, "\' AND dat",
                                 "e = \'", strftime(a_date, "%Y-%m-%d"), "\'")
            query_str3 <- paste0("DELETE FROM public.gauging_data_missing WHER",
                                 "E gauging_station = \'", a_gs, "\' AND date ",
                                 "= \'", strftime(a_date, "%Y-%m-%d"), "\'")
            if (nrow(dbGetQuery(con, query_str2)) > 0) {
                dbSendQuery(con, query_str3)
            }
            
            next
            
        }
        
        write(paste(sep=" ", a_gs, a_date, "wird eingefügt"), stdout())
        
        #####
        # assemble the regular url
        url <- paste0("http://www.pegelonline.wsv.de/webservices/files/Wassers",
                      "tand+Rohdaten/", df.gs$water_longname[i], "/", c_gs, "/",
                      strftime(a_date, format="%d.%m.%Y"), "/down.csv")
        
        # first check of the regular url
        if (!url.exists(url)) {
            
        #####
        # assemble the url with umlaut replacements
            url <- paste0("http://www.pegelonline.wsv.de/webservices/files/Was",
                          "serstand+Rohdaten/", df.gs$water_longname[i], "/",
                          c_gs, "/", strftime(a_date, format="%d.%m.%Y"),
                          "/down.csv")
            
        # second check of the url
            if(!url.exists(url)) {
        
        #####
        # assemble special url's for MAGDEBURG-ROTHENSEE, DUISBURG-RUHRORT
                if (a_gs == "MAGDEBURG-ROTHENSEE") {
                          url <- paste0("http://www.pegelonline.wsv.de/webserv",
                                        "ices/files/Wasserstand+Rohdaten/",
                                        df.gs$water_longname[i], "/ROTHENSEE/",
                                        strftime(a_date, format="%d.%m.%Y"),
                                        "/down.csv")
                }
                if (a_gs == "RUHRORT") {
                    url <- paste0("http://www.pegelonline.wsv.de/webservices/f",
                                  "iles/Wasserstand+Rohdaten/",
                                  df.gs$water_longname[i], "/DUISBURG-RUHRORT/",
                                  strftime(a_date, format="%d.%m.%Y"),
                                  "/down.csv")
                }
                
        # third check of the url
                if(!url.exists(url)) {
                    
        #####
        # record missing values and jump to next step in the for loop
                    write(paste(sep=" ", a_gs, a_date, "URL problems"),
                          stderr())
                    write(a_gs, stderr())
                    write(str(a_gs), stderr())
                    write(paste0("INSERT INTO public.gauging_data_missing (id,",
                                 " gauging_station, date) VALUES (DEFAULT, \'",
                                 a_gs, "\', \'",
                                 as.Date(a_date, origin="1970-01-01"), "\')"),
                          stderr())
                    
                    # update gauging_station_data
                    dbSendQuery(con, paste0("UPDATE public.gauging_station_dat",
                                            "a SET data_missing = TRUE WHERE g",
                                            "auging_station = \'", a_gs, "\'"))
                    
                    # insert missing values
                    dbSendQuery(con, paste0("INSERT INTO public.gauging_data_m",
                                            "issing (id, gauging_station, date",
                                            ") VALUES (DEFAULT, \'", a_gs,
                                            "\', \'",
                                            as.Date(a_date,
                                                    origin="1970-01-01"),
                                            "\')"))
                    
                    next
                    
                }
            }
        }
        
        # create a temporary file name for the download of data
        if (Sys.info()["nodename"] == "pvil-r" & 
            Sys.info()["user"] == "WeberA") {
            # assemble a file name
            destfile <- paste0("/home/WeberA/flut3_",
                               simpleCap(df.gs$water_longname[i]),
                               "/data/w/pegelonline.wsv.de/",
                               df.gs$water_longname[i], "_", a_gs, "_",
                               strftime(a_date, format="%Y%m%d"), ".csv")
            delete <- FALSE
        } else {
            destfile <- tempfile()
            delete <- TRUE
        }
        
        # download the file
        download.file(url, destfile, "wget", quiet = TRUE)
        
        # read the downloaded file
        # header (CHECK PNP!!!)
        header <- scan(destfile, "list", sep = ";", nlines = 1)
        
        # data
        df.data <- read.table(destfile, header = FALSE, sep = ";", skip = 1,
                              na.strings = "XXX,XXX")
        
        # delete destfile
        if (delete) {unlink(destfile)}
        
        # calculate daily mean
        w <- round(mean(as.numeric(df.data$V2), na.rm = TRUE), 0)
        
        if (is.nan(w)) {
            write("w ist NaN", stderr())
            next
        }
        
        # insert data into the gauging_data table
        dbSendQuery(con, paste0("INSERT INTO public.gauging_data (id, gauging_",
                                "station, date, year, month, day, w) VALUES (D",
                                "EFAULT, \'", a_gs, "\', \'",
                                strftime(a_date, "%Y-%m-%d"), "\', ",
                                strftime(a_date, "%Y"), ", ",
                                strftime(a_date, "%m"), ", ",
                                strftime(a_date, "%d"), ", ", w, ")"))
        
        # delete row(s) from gauging_data_missing table
        query_str4 <- paste0("SELECT * FROM public.gauging_data_missing WHERE ",
                             "gauging_station = \'", a_gs, "\' AND date = \'",
                             strftime(a_date, "%Y-%m-%d"), "\'")
        if (nrow(dbGetQuery(con, query_str4))  > 0) {
            dbSendQuery(con, paste0("DELETE FROM public.gauging_data_missing W",
                                    "HERE gauging_station = \'", a_gs, "\' AND",
                                    " date = \'", strftime(a_date, "%Y-%m-%d"),
                                    "\'"))
        }
        
        # update the tables gauging_station_data and gauging_data_missing
        if(as.Date(a_date) > date_range_present[2]) {
            missing_dates <- as.character(seq(date_range_present[2],
                                              as.Date(a_date), by = "days"))
            for (a_missing_date in missing_dates) {
                query_str5 <- paste0("SELECT * FROM public.gauging_data_missin",
                                     "g WHERE gauging_station = \'",
                                     a_gs, "\' AND date = \'",
                                     a_missing_date, "\'")
                query_str6 <- paste0("SELECT * FROM public.gauging_data WHERE ",
                                     "gauging_station = \'", a_gs, "\' AND dat",
                                     "e = \'", a_missing_date, "\'")
                if (nrow(dbGetQuery(con, query_str5)) == 0 &
                    nrow(dbGetQuery(con, query_str6)) == 0) {
                    
                    # print
                    write(paste(sep=" ", a_gs, a_missing_date, "missing"),
                          stdout())
                    
                    dbSendQuery(con, paste0("INSERT INTO public.gauging_data_m",
                                            "issing (id, gauging_station, date",
                                            ") VALUES (DEFAULT, \'", a_gs, "\'",
                                            ", \'", strftime(a_missing_date,
                                                             "%Y-%m-%d"),
                                            "\')"))
                    dbSendQuery(con, paste0("UPDATE public.gauging_station_dat",
                                            "a SET data_missing = TRUE WHERE g",
                                            "auging_station = \'", a_gs, "\'"))
                }
            }
            
            # extend the data_present_timespan
            date_range_present <- c(date_range_present[1], as.Date(a_date))
            date_range_str <- paste(date_range_present, collapse = " - ")
            dbSendQuery(con, paste0("UPDATE public.gauging_station_data SET da",
                                    "ta_present_timespan = \'", date_range_str,
                                    "\' WHERE gauging_station = \'", a_gs,
                                    "\'"))
            
        }
    }
    
    i <- i + 1
    
}

# exit R
q("no")
