
if (!(file.exists("data/df.gauging_station_data.rda"))) {
    
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
    df.gauging_station_data <- DBI::dbGetQuery(gd_con, 
        paste0("SELECT id, gauging_station, uuid, agency, km, water_shortname,",
               " longitude, latitude, mw, mw_timespan, pnp, data_present, km_q",
               "ps FROM gauging_station_data ORDER BY water_shortname, km_qps"))
    df.gauging_station_data$river <- df.gauging_station_data$water_shortname
    df.gauging_station_data$water_shortname <- NULL
    df.gauging_station_data$river[df.gauging_station_data$river == "RHEIN"] <- 
        "RHINE"
    
    # store df.gauging_station_data as external dataset
    usethis::use_data(df.gauging_station_data, overwrite = TRUE,
                      compress = "bzip2")
    
    # clean up
    rm(gd_con, gd_credentials)
    
} else {
    write("data/df.gauging_station_data.rda exists already", stderr())
}

