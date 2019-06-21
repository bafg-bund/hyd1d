
.onLoad <- function(libname, pkgname) {
    # load package data
    utils::data("df.flys", "df.flys_sections", "df.gauging_station_data", 
                "df.sections", "df.gauging_data", package = pkgname, 
                envir = parent.env(environment()))
    df.gauging_data$gauging_station <- asc2utf8(df.gauging_data$gauging_station)
    
    # set relevant DB variables
    if (utils::compareVersion(as.character(getRversion()), "3.5.0") < 0) {
        file_date <- paste0(path.expand('~'), "/.hyd1d/date_gauging_data_v2.RD",
                            "S")
        file_data <- paste0(path.expand('~'), "/.hyd1d/df.gauging_data_latest_",
                            "v2.RDS")
    } else {
        file_date <- paste0(path.expand('~'), "/.hyd1d/date_gauging_data.RDS")
        file_data <- paste0(path.expand('~'), "/.hyd1d/df.gauging_data_latest.",
                            "RDS")
    }
    
    # update date_gauging_data 
    .db_updated <<- FALSE
    if (file.exists(file_date)) {
        # check, when it was updated the last time
        date_gauging_data <- readRDS(file_date)
        if (date_gauging_data < Sys.Date()) {
            # update
            if (updateGaugingData(x = date_gauging_data)) {
                date_gauging_data <- Sys.Date()
                saveRDS(date_gauging_data, file = file_date)
            }
        }
    } else {
        # update
        date_gauging_data <- Sys.Date()
        if (updateGaugingData(x = date_gauging_data)) {
            saveRDS(date_gauging_data, file = file_date)
        }
    }
    
    # load df.gauging_data into .GlobalEnv
    df.gd <- readRDS(file_data)
    df.gd$gauging_station <- asc2utf8(df.gd$gauging_station)
    .GlobalEnv$.df.gauging_data <- df.gd
    if (exists("df.gauging_data", envir = .GlobalEnv)) {
        rm(df.gauging_data, envir = .GlobalEnv)
    }
}

.onAttach <- function(libname, pkgname) {
    if (.db_updated) {
        packageStartupMessage(paste0("\nThe internal dataset 'df.gaugi",
                                     "ng_data' has been updated."))
    }
}

.onUnload  <- function(libpath) {
    if (exists(".df.gauging_data", envir = .GlobalEnv)) {
        rm(.df.gauging_data, envir = .GlobalEnv)
    }
}

