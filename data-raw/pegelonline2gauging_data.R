##################################################
# pegelonline2gauging_data.R
#
# author: arnd.weber@bafg.de
# date:   09.05.2018
#
# purpose: 
#   - download gauging data from pegelonline.wsv.de
#   - write them into the postgresql database
#
##################################################
verbose <- TRUE

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

###
# get the rivers and gauging_stations to download data from
df.gs <- dbGetQuery(con, paste0("SELECT gauging_station, water_longname, pnp, ",
                                "data_present_timespan FROM public.gauging_sta",
                                "tion_data WHERE data_present IS TRUE"))

###
# produce a vector of dates to be downloaded
# set constant variables
days_back <- 7
req_dates <- as.character(seq(Sys.Date() - days_back, Sys.Date() - 1, 
                              length.out = days_back))

###
# process the data
i <- 1
for(a_gs in df.gs$gauging_station) {
    
    # obtain the present range of available data for a_gs
    date_range_present <- as.Date(unlist(strsplit(df.gs$data_present_timespan[i],
                                                  " - ")))
    
    for(a_date in req_dates) {
        
        #####
        # check for existing entries
        query_str1 <- paste0("SELECT w FROM public.gauging_data WHERE gauging_",
                             "station = \'", a_gs, "\' AND date = \'", 
                             strftime(a_date, "%Y-%m-%d"), "\'")
        if(nrow(dbGetQuery(con, query_str1)) == 1) {
            if (verbose) {
                write(paste(sep=" ", a_gs, a_date, "existiert bereits"), 
                      stderr())
                write(paste(sep=" ", a_gs, a_date, "existiert bereits"), 
                      stdout())
            }
            
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
        
        if (verbose) {
            write(paste(sep=" ", a_gs, a_date, "wird eingefügt"), stderr())
            write(paste(sep=" ", a_gs, a_date, "wird eingefügt"), stdout())
        }
        
        #####
        # assemble the regular url
        b_gs <- gsub(" ", "+", 
                     gsub("Ä", "%C4", 
                          gsub("Ö", "%D6", 
                               gsub("Ü", "%DC", a_gs))))
        url <- paste0("http://www.pegelonline.wsv.de/webservices/files/Wassers",
                      "tand+Rohdaten/", df.gs$water_longname[i], "/", b_gs, "/",
                      strftime(a_date, format="%d.%m.%Y"), "/down.csv")
        
        # first check of the regular url
        if (!url.exists(url)) {
            
        #####
        # assemble the url with umlaut replacements
            c_gs <- simpleCap(a_gs)
            d_gs <- gsub(" ", "+", 
                         gsub("Ä", "%C4", 
                              gsub("Ö", "%D6", 
                                   gsub("Ü", "%DC", 
                                        gsub("ä", "%E4", 
                                             gsub("ö", "%F6", 
                                                  gsub("ü", "%FC", c_gs)))))))
            
            url <- paste0("http://www.pegelonline.wsv.de/webservices/files",
                          "/Wasserstand+Rohdaten/", 
                          df.gs$water_longname[i], "/", d_gs, "/", 
                          strftime(a_date, format="%d.%m.%Y"), "/down.csv")
            
        # second check of the url
            if(!url.exists(url)) {
        
        #####
        # assemble a special url for MAGDEBURG-ROTHENSEE
                if (a_gs == "MAGDEBURG-ROTHENSEE"){
                          url <- paste0("http://www.pegelonline.wsv.de/web",
                                        "services/files/Wasserstand+Rohdaten/",
                                        df.gs$water_longname[i], "/ROTHENSEE/", 
                                        strftime(a_date, format="%d.%m.%Y"),
                                        "/down.csv")
                }
                
        # third check of the url
                if(!url.exists(url)){
                    
        #####
        # record missing values and jump to next step in the for loop
                    if (verbose) {
                        write(paste(sep=" ", a_gs, a_date, "URL-Probleme"),
                              stderr())
                        write(paste(sep=" ", a_gs, a_date, "URL-Probleme"),
                              stdout())
                    }
                    
                    # update gauging_station_data
                    dbSendQuery(con, paste0("UPDATE public.gauging_station_dat",
                                            "a SET (data_missing) = (TRUE) WHE",
                                            "RE gauging_station = \'", a_gs,
                                            "\'"))
                    
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
        destfile <- tempfile()
        
        # download the file
        download.file(url, destfile, "wget", quiet = TRUE)
        
        # read the downloaded file
        # header (CHECK PNP!!!)
        header <- scan(destfile, "list", sep = ";", nlines = 1, quiet = TRUE)
        
        # data
        df.data <- read.table(destfile, header = FALSE, sep = ";", skip = 1, 
                              na.strings = "XXX,XXX")
        
        # calculate daily mean
        w <- round(mean(as.numeric(df.data$V2), na.rm = TRUE), 0)
        
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
        if (nrow(dbGetQuery(con, query_str4))  > 0){
            dbSendQuery(con, paste0("DELETE FROM public.gauging_data_missing W",
                                    "HERE gauging_station = \'", a_gs, "\' AND",
                                    " date = \'", strftime(a_date, "%Y-%m-%d"), 
                                    "\'"))
        }
        
        # update the tables gauging_station_data and gauging_data_missing
        if(as.Date(a_date) > date_range_present[2]){
            missing_dates <- as.character(seq(date_range_present[2], 
                                              as.Date(a_date), by = "days"))
            for (a_missing_date in missing_dates){
                query_str5 <- paste0("SELECT * FROM public.gauging_data_missin",
                                     "g WHERE gauging_station = \'", a_gs, 
                                     "\' AND date = \'", a_missing_date, "\'")
                query_str6 <- paste0("SELECT * FROM public.gauging_data WHERE ",
                                     "gauging_station = \'", a_gs, "\' AND dat",
                                     "e = \'", a_missing_date, "\'")
                if (nrow(dbGetQuery(con, query_str5)) == 0 &
                    nrow(dbGetQuery(con, query_str6)) == 0){
                    
                    # print
                    if (verbose) {
                        write(paste(sep=" ", a_gs, a_missing_date, "missing"), 
                              stderr())
                        write(paste(sep=" ", a_gs, a_missing_date, "missing"), 
                              stdout())
                    }
                    
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
                                    "\' WHERE gauging_station = \'", a_gs, "\'"))
            
        }
    }
    
    i <- i + 1
    
}

q("no")
