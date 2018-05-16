
.onLoad <- function(libname, pkgname) {
    # load package data
    utils::data("df.flys_data", "df.flys_sections", "df.gauging_station_data", 
                "df.sections_data", package = pkgname, 
                envir = parent.env(environment()))
    
    # set relevant variables
    p_source <- find.package(pkgname)
    file_date <- paste0(p_source, "/data/date_gauging_data.rda")
    file_data <- paste0(p_source, "/data/df.gauging_data.rda")
    file_upd <- paste0(p_source, "/data/upd.txt")
        
    # update date_gauging_data
    if (file.exists(file_date)){
        load(file_date, envir = parent.env(environment()))
    } else {
        utils::data("date_gauging_data", package = pkgname, 
                    envir = parent.env(environment()))
    }
    
    # update df.gauging_data.rda, if required
    if (updateGaugingData(x = date_gauging_data)){
        write("1", file = file_upd)
    } else {
        if (file.exists(file_upd)){
            file.remove(file_upd)
        }
    }
    
    # load df.gauging_data
    if (file.exists(file_data)){
        load(file_data, envir = parent.env(environment()))
    } else (
        utils::data("df.gauging_data", package = pkgname, 
                    envir = parent.env(environment()))
    )
}


.onAttach <- function(libname, pkgname) {
    
    # set relevant variables
    p_source <- find.package(pkgname)
    file_upd <- paste0(p_source, "/data/upd.txt")
    
    # send message
    if (file.exists(file_upd)){
        packageStartupMessage(paste0("#####\n Package ',", pkgname,"':\n The ",
                                     "internal dataset 'df.gauging_data' has ", 
                                     "been updated."))
        file.remove(file_upd)
    }
}


.onUnload  <- function(libpath) {
    for (a_dataset in c("df.flys_data", "df.flys_sections", "df.gauging_data", 
                        "df.gauging_station_data", "df.sections_data",
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

