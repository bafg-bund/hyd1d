
.onLoad <- function(libname, pkgname) {
    # load package data
    utils::data("df.flys", "df.flys_sections", "df.gauging_station_data", 
                "df.sections", "df.gauging_data", package = pkgname, 
                envir = parent.env(environment()))
    
    # set relevant DB variables
    file_date <- paste0(DBpath(), "date_gauging_data.rda")
    file_data <- paste0(DBpath(), "df.gauging_data_latest.rda")
    
    # update date_gauging_data 
    .db_updated <<- FALSE
    if (file.exists(file_date)) {
        # check, when it was updated the last time
        load(file_date)
        if (date_gauging_data < Sys.Date()) {
            # update
            if (updateGaugingData(x = date_gauging_data)) {
                date_gauging_data <- Sys.Date()
                save(date_gauging_data, file = file_date)
            }
        }
    } else {
        # update
        date_gauging_data <- Sys.Date()
        if (updateGaugingData(x = date_gauging_data)) {
            save(date_gauging_data, file = file_date)
        }
    }
    
    # load df.gauging_data into .GlobalEnv
    load(file_data, envir = .GlobalEnv)
    .GlobalEnv$.df.gauging_data <- .GlobalEnv$df.gauging_data
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

