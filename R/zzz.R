
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
    .db_updated <<- list(FALSE)
    if (file.exists(file_date)) {
        # check, when it was updated the last time
        date_gauging_data <- readRDS(file_date)
        t <- paste0("'df.gauging_data' was last updated on ",
                     as.character(date_gauging_data), ".")
        
        if (date_gauging_data < Sys.Date() - 29) {
            t <- paste0(t, "\nIt will be updated now ...")
            
            # update
            if (updateGaugingData(x = date_gauging_data)) {
                t <- paste0(t, "\n'df.gauging_data' was updated successfully.")
                date_gauging_data <- Sys.Date()
                saveRDS(date_gauging_data, file = file_date)
            } else {
                t <- paste0(t, "\n'df.gauging_data' was not updated successful",
                            "ly.")
            }
        }
        .db_updated <<- list(TRUE, t)
    } else {
        t <- "'df.gauging_data' will be downloaded initially."
        # update
        date_gauging_data <- Sys.Date()
        if (updateGaugingData(x = date_gauging_data)) {
            t <- paste0(t, "\n'df.gauging_data' was downloaded successfully.")
            saveRDS(date_gauging_data, file = file_date)
        } else {
            t <- paste0(t, "\n'df.gauging_data' was not downloaded.")
        }
        
        .db_updated <<- list(TRUE, t)
    }
    
    # load df.gauging_data into .GlobalEnv
    .GlobalEnv$.df.gauging_data <- readRDS(file_data)
    .GlobalEnv$.df.gauging_data$gauging_station <- asc2utf8(
        .GlobalEnv$.df.gauging_data$gauging_station
    )
    if (exists("df.gauging_data", envir = .GlobalEnv)) {
        rm(df.gauging_data, envir = .GlobalEnv)
    }
}

.onAttach <- function(libname, pkgname) {
    if (.db_updated[[1]]) {
        packageStartupMessage(.db_updated[[2]])
    }
}

.onUnload  <- function(libpath) {
    if (exists(".df.gauging_data", envir = .GlobalEnv)) {
        rm(.df.gauging_data, envir = .GlobalEnv)
    }
}

