#' @name waterLevelFlys3Seq
#' @rdname waterLevelFlys3
#' @aliases waterLevelFlys3Seq
#' 
#' @export
#' 
waterLevelFlys3Seq <- function(river = c("Elbe", "Rhine"), name, from, to) {
    
    ##########
    # check arguments
    ##
    # vectors to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    #####
    # river
    error_river <- FALSE
    
    ##
    # presence
    if (missing(river)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'river' ",
                                   "argument has to be supplied."))
        error_river <- TRUE
    } else {
        
        ##
        # character
        if (!inherits(river, "character")) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'river' must be type 'character'."))
            error_river <- TRUE
        }
        
        ##
        # length
        if (length(river) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'river' must have a length ",
                                       "equal 1."))
            error_river <- TRUE
        }
        
        ##
        # set 'river'-specific limits of station_int
        if (!(error_river)) {
            
            ##
            # %in% c("Elbe", "Rhine")
            if (!(river %in% c("Elbe", "Rhine"))) {
                errors <- c(errors, paste0("Error ", l(errors),
                                           ": 'river' must be an element ",
                                           "of c('Elbe', 'Rhine')."))
                error_river <- TRUE
            } else {
                
                if (river == "Elbe") {
                    station_int_min <- 0
                    station_int_max <- 585700
                }
                if (river == "Rhine") {
                    station_int_min <- 336200
                    station_int_max <- 865700
                }
                
                wldf_river <- river
                
            }
        } else {
            station_int_min <- 0
            station_int_max <- 865700
        }
    }
    
    #####
    # name
    ##
    # presence
    if (missing(name)) {
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": The 'name' argument has to be supplied."))
    } else {
        ##
        # character
        if (!inherits(name, "character")) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'name' must be type 'character'."))
        }
        
        ##
        # length
        if (length(name) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'name' must have a length ",
                                       "equal 1."))
        }
        
        ##
        # %in% flys3_water_levels
        if (!(error_river)) {
            if (river == "Elbe") {
                flys3_water_levels <- c("0.5MNQ", "MNQ", "0.5MQ", "a", "0.75MQ",
                                        "b", "MQ", "c", "2MQ", "3MQ", "d", "e",
                                        "MHQ", "HQ2", "f", "HQ5", "g", "h",
                                        "HQ10", "HQ15", "HQ20", "HQ25", "HQ50",
                                        "HQ75", "HQ100", "i", "HQ150", "HQ200",
                                        "HQ300", "HQ500")
            }
            if (river == "Rhine") {
                flys3_water_levels <- c("Ud=1", "Ud=5", "GlQ2012", "Ud=50",
                                        "Ud=80", "Ud=100", "Ud=120", "Ud=183",
                                        "MQ", "Ud=240","Ud=270", "Ud=310",
                                        "Ud=340", "Ud=356", "Ud=360", "MHQ",
                                        "HQ2", "HQ5", "HQ5-10", "HQ10",
                                        "HQ10-20", "~HQ20","HQ20-50", "HQ50",
                                        "HQ50-100", "HQ100", "HQ100-200",
                                        "HQ200", "HQ200-ex", "HQextr.")
            }
            
            if (!(name %in% flys3_water_levels)) {
                errors <- c(errors, paste0("Error ", l(errors),
                                           ": 'name' must be an element ",
                                           "of c('",
                                           paste0(flys3_water_levels,
                                                  collapse="', '"),
                                           "')."))
            }
        }
    }
    
    #####
    # from
    error_from <- FALSE
    
    ##
    # presence
    if (missing(from)) {
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": The 'from' argument has to be supplied."))
        error_from <- TRUE
    } else {
        ##
        # integer | numeric
        if (!inherits(from, "integer") & !inherits(from, "numeric")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'from' must be ",
                                       "type 'integer' or 'numeric'."))
            error_from <- TRUE
        }
        
        ##
        # length
        if (length(from) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'from' must have a length equal 1."))
            error_from <- TRUE
        }
        
        ##
        # range
        # Elbe 0 - 585700
        # Rhine 336200 - 865700
        if (!(error_river)) {
            if (inherits(from, "integer")) {
                if (from < station_int_min) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'from' ",
                                               "must be above ",
                                               as.character(station_int_min),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min/1000)),
                                               ") for river '", river, "'."))
                    error_from <- TRUE
                }
                if (from > station_int_max) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'from' ",
                                               "must be below ",
                                               as.character(station_int_max),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max/1000)),
                                               ") for river '", river, "'."))
                    error_from <- TRUE
                }
                
                wldf_from <- as.numeric(from / 1000)
            }
            if (inherits(from, "numeric")) {
                if (from < station_int_min / 1000) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'from' ",
                                               "must be above km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min/1000)),
                                               " for river '", river, "'."))
                    error_from <- TRUE
                }
                if (from > station_int_max / 1000) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'from' ",
                                               "must be below km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max/1000)),
                                               " for river '", river, "'."))
                    error_from <- TRUE
                }
                
                wldf_from <- from
            }
        } else {
            if (inherits(from, "integer")) {
                if (from < station_int_min) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'from' ",
                                               "must be above ",
                                               as.character(station_int_min),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min/1000)),
                                               ")."))
                    error_from <- TRUE
                }
                if (from > station_int_max) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'from' ",
                                               "must be below ",
                                               as.character(station_int_max),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max/1000)),
                                               ")."))
                    error_from <- TRUE
                }
            }
            if (inherits(from, "numeric")) {
                if (from < station_int_min / 1000) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'from' ",
                                               "must be above km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min/1000)),
                                               "."))
                    error_from <- TRUE
                }
                if (from > station_int_max / 1000) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'from' ",
                                               "must be below km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max/1000)),
                                               "."))
                    error_from <- TRUE
                }
            }
        }
    }
    
    #####
    # to
    ##
    # presence
    if (missing(to)) {
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": The 'to' argument has to be supplied."))
    } else {
        ##
        # integer | numeric
        if (!inherits(from, "integer") & !inherits(from, "numeric")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'to' must be ",
                                       "type 'integer' or 'numeric'."))
        }
        
        ##
        # length
        if (length(to) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'to' must have a length equal 1."))
        }
        
        ##
        # range
        # Elbe 0 - 585700
        # Rhine 336200 - 865700
        if (!(error_river)) {
            if (inherits(to, "integer")) {
                if (to < station_int_min) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'to' ",
                                               "must be above ",
                                               as.character(station_int_min),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min/1000)),
                                               ") for river '", river, "'."))
                }
                if (to > station_int_max) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'to' ",
                                               "must be below ",
                                               as.character(station_int_max),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max/1000)),
                                               ") for river '", river, "'."))
                }
                
                wldf_to <- as.numeric(to / 1000)
            }
            if (inherits(to, "numeric")) {
                if (to < station_int_min / 1000) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'to' ",
                                               "must be above km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min/1000)),
                                               " for river '", river, "'."))
                }
                if (to > station_int_max / 1000) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'to' ",
                                               "must be below km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max/1000)),
                                               " for river '", river, "'."))
                }
                
                wldf_to <- to
            }
        } else {
            if (inherits(to, "integer")) {
                if (to < station_int_min) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'to' ",
                                               "must be above ",
                                               as.character(station_int_min),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min/1000)),
                                               ")."))
                }
                if (to > station_int_max) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'to' ",
                                               "must be below ",
                                               as.character(station_int_max),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max/1000)),
                                               ")."))
                }
            }
            if (inherits(to, "numeric")) {
                if (to < station_int_min / 1000) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'to' ",
                                               "must be above km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min/1000)),
                                               "."))
                }
                if (to > station_int_max / 1000) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'to' ",
                                               "must be below km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max/1000)),
                                               "."))
                }
            }
        }
        
        if (!(missing(from)) & !(error_from)) {
            ##
            # class(from) != class(to)
            if (class(from) != class(to)) {
                errors <- c(errors, paste0("Error ", l(errors), ": class(fro",
                                           "m) must be equal to class(to)."))
            }
            
            ##
            # from < to
            if (from >= to) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'to' must ",
                                           "be above 'from', since stationing ",
                                           "increases downstream and these ",
                                           "two parameters must be set in ",
                                           "this order."))
            }
        }
    }
    
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    }
    
    ##########
    # processing
    #####
    # access the FLYS3 data
    get("df.flys", pos = -1)
    
    # select the water level for a specified river and name
    id <- which(df.flys$river == wldf_river & df.flys$name == name)
    df.flys_sel <- df.flys[id,]
    
    # identify the relevant river stretch
    id <- which(df.flys_sel$station >= wldf_from &
                df.flys_sel$station <= wldf_to)
    df.wl_left <- df.flys_sel[min(id), ]
    df.wl_right <- df.flys_sel[max(id), ]
    id <- c(min(id) - 1, id, max(id) + 1)
    df.wl <- stats::na.omit(df.flys_sel[id, ])
    
    #####
    # interpolate
    station <- seq(from = wldf_from, to = wldf_to, by = 0.1)
    
    data <- stats::approx(x = df.wl$station, y = df.wl$w,
                          xout = station, method = "linear",
                          yleft = df.wl_left$w, yright = df.wl_right$w,
                          rule = c(2, 2), ties = "ordered")
    data$x <- as.integer(round(data$x * 1000, 0))
    
    ##########
    # initialize the resulting WaterLevelDataFrame and return it
    wldf <- WaterLevelDataFrame(river = river,
                                time = as.POSIXct(NA),
                                gauging_stations_missing = as.character(NA),
                                comment = paste0("Computed by ", 
                                                 "waterLevelFlys3Seq(): ",
                                                 name),
                                station_int = data$x,
                                w = data$y)
    
    return(wldf)
}


