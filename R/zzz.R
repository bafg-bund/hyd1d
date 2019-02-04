
.onLoad <- function(libname, pkgname) {
    # load package data
    utils::data("df.flys", "df.flys_sections", "df.gauging_station_data", 
                "df.sections", package = pkgname, 
                envir = parent.env(environment()))
    
    # set relevant variables
    p_source <- find.package(pkgname)
    file_date <- paste0(p_source, "/data/date_gauging_data.rda")
    file_data <- paste0(p_source, "/data/df.gauging_data_latest.rda")
    
    # update date_gauging_data
    if (file.exists(file_date)){
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
    
    # load df.gauging_data
    if (file.exists(file_data)){
        #browser()
        load(file_data, envir = parent.env(environment()))
    } else (
        utils::data("df.gauging_data", package = pkgname, 
                    envir = parent.env(environment()))
    )
}


.onAttach <- function(libname, pkgname) {
    
    # set relevant variables
    p_source <- find.package(pkgname)
    file_date <- paste0(p_source, "/data/date_gauging_data.rda")
    
    # send message
    if (file.exists(file_date)){
        # check, when it was updated the last time
        load(file_date)
        if (date_gauging_data < Sys.Date()) {
            # update
            if (updateGaugingData(x = date_gauging_data)) {
                date_gauging_data <- Sys.Date()
                save(date_gauging_data, file = file_date)
                packageStartupMessage(paste0("#####\n Package '", pkgname,
                                             "':\n The internal dataset 'df.ga",
                                             "uging_data' has been updated."))
            }
        }
    }
}


.onUnload  <- function(libpath) {
    for (a_dataset in c("df.flys", "df.flys_sections", "df.gauging_data", 
                        "df.gauging_station_data", "df.sections",
                        "date_gauging_data")){
        if (exists(a_dataset, envir = globalenv())){
            rm(list = a_dataset, envir = globalenv())
        }
    }
}


#if(getRversion() >= "2.15.1"){
#    utils::globalVariables(c("df.flys_data", "df.flys_sections", "df.gauging_data", 
#                             "df.gauging_station_data", "df.sections_data"))
#}

