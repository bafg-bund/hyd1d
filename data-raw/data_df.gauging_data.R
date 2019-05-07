
if (!(file.exists("data/df.gauging_data.rda"))) {
    
    # get credentials
    gd_credentials <- credentials("DB_credentials_gauging_data")
    
    # read the data
    # access the gauging_data DB
    gd_con <- DBI::dbConnect(drv      = DBI::dbDriver("PostgreSQL"),
                             host     = gd_credentials["host"], 
                             dbname   = gd_credentials["dbname"], 
                             user     = gd_credentials["user"], 
                             password = gd_credentials["password"], 
                             port     = gd_credentials["port"])
    
    # retrieve the data
    query_string <- paste0("SELECT gauging_station, date, w FROM gauging_data ",
                           "WHERE (date >= '2016-12-01' AND date <= '2016-12-3",
                           "1') OR date = '1991-12-16' ORDER BY gauging_statio",
                           "n, date")
    df.gauging_data <- DBI::dbGetQuery(gd_con, query_string)
    
    # replace non-ASCII characters
    df.gauging_data$gauging_station <- iconv(df.gauging_data$gauging_station,
                                             from = "UTF-8", to = "ASCII", 
                                             sub = "byte")
    
    # store df.gauging_data as external dataset
    usethis::use_data(df.gauging_data, overwrite = TRUE, compress = "bzip2")
    
    # clean up
    rm(gd_con, gd_credentials, query_string, df.gauging_data)
    
} else {
    write("data/df.gauging_data.rda exists already", stderr())
}



