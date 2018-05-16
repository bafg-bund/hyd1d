#' @name waterLevelFlys3InterpolateX
#' @rdname waterLevelFlys3InterpolateX
#' 
#' @title Interpolate FLYS3 water levels for given stations
#' 
#' @description Function to interpolate FLYS3 water levels for selected 
#'   stations and return it with the structure of \code{\link{df.flys_data}}.
#' 
#' @details \code{\link{df.flys_data}} contains 1D water level data computed
#'   with SOBEK for every second hectometer (every 200 m). This function
#'   provides a way to interpolate the 30 stationary water levels for selected
#'   stations inbetween these hectometers and returns them with the 
#'   \code{data.frame}-structure of the original dataset.
#' 
#' @param river a required argument to fill the \code{WaterLevelDataFrame}-slot
#'   \code{river}. It has to be type \code{character}, has to have a length of
#'   one and can be either \strong{Elbe} or \strong{Rhein}.
#' @param station a possible argument to hand over the stationing along the
#'   specified \code{river}. If specified, it has to be type \code{"numeric"}
#'   and has to have the same length as other optional arguments (\code{id},
#'   \code{station_int} and \code{w}) forming the \code{"data.frame"}-component
#'   of a \code{WaterLevelDataFrame}. If both stationing arguments
#'   (\code{station} and \code{station_int}) are specified, all elements of
#'   \code{station} have to be equal to \code{as.numeric(station_int / 1000)}.
#'   Minimal and maximal allowed values of \code{station} are
#'   \code{river}-specific: Elbe (km 0 - 585.7), Rhein (km 336.2 - 865.7).
#' @param station_int a possible argument to hand over the stationing along the
#'   specified \code{river}. If specified, it has to be type \code{"integer"}
#'   and has to have the same length as other optional arguments (\code{id},
#'   \code{station} and \code{w}) forming the \code{"data.frame"}-component of a
#'   \code{WaterLevelDataFrame}. If both stationing arguments (\code{station}
#'   and \code{station_int}) are specified, all elements of \code{station_int}
#'   have to be equal to \code{as.integer(station * 1000)}. Minimal and maximal
#'   allowed values of \code{station_int} are \code{river}-specific: Elbe (m 0 -
#'   585700), Rhein (m 336200 - 865700).
#'
#' @return An object of class \code{data.frame} with the structure of
#'   \code{\link{df.flys_data}}.
#'
#' @references 
#'   \insertRef{busch_einheitliche_2009}{hyd1d}
#'   
#'   \insertRef{hkv_hydrokontor_erstellung_2014}{hyd1d}
#'   
#'   \insertRef{deltares_sobek_2018}{hyd1d}
#' 
#' @examples
#' df.flys  <- waterLevelFlys3InterpolateX("Elbe", 257.1)
#'
#' @export
#' 
waterLevelFlys3InterpolateX <- function(river = c("Elbe", "Rhein"), 
                                        station = NULL,
                                        station_int = NULL) {
    
    #####
    # assemble internal variables and check the existence of required data
    ##
    # vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    ##### 
    # check arguments
    ##
    # river
    error_river <- FALSE
    # presence
    if (missing(river)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'river' ",
                                   "argument has to be supplied."))
        error_river <- TRUE
    } else {
        # character
        if (class(river) != "character") {
            errors <- c(errors, paste0("Error ", l(errors), ": 'river' must ",
                                       "be type 'character'."))
            error_river <- TRUE
        }
        # length
        if (length(river) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'river' must ",
                                       "have length 1."))
            error_river <- TRUE
        }
        # %in% c('Elbe', 'Rhein')
        if (!(river %in% c("Elbe", "Rhein"))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'river' must ",
                                       "be an element of c('Elbe', 'Rhein')."))
            error_river <- TRUE
        }
        # set 'river'-specific limits of station_int
        if (!(error_river)) {
            if (river == "Elbe") {
                station_int_min <- 0
                station_int_max <- 585700
                w_min <- 0
                w_max <- 130
            }
            if (river == "Rhein") {
                station_int_min <- 336200
                station_int_max <- 865700
                w_min <- 5
                w_max <- 120
            }
            wldf_river <- river
        } else {
            station_int_min <- 0
            station_int_max <- 865700
            w_min <- 0
            w_max <- 130
        }
    }
    
    ##
    # station_int & station
    if (missing(station_int) & missing(station)) {
        errors <- c(errors, paste0("Error ", l(errors), ": At least one ",
                                   "station argument ('station_int' or ",
                                   "'station') must be supplied."))
    } else {
        if (!(missing(station_int)) & !(missing(station))) {
            if (length(station_int) != length(station)) {
                errors <- c(errors, paste0("Error ", l(errors), ": The length ",
                                           "of 'station_int' and 'station' ",
                                           "must be equal."))
            } else {
                if (!(all(station_int == as.integer(station * 1000)))) {
                    errors <- c(errors, paste0("Error ", l(errors), ": If both",
                                               " station arguments ('station_i",
                                               "nt', 'station') are supplied,",
                                               "\n  all elements of 'station_i",
                                               "nt' must be equal to as.intege",
                                               "r(station * 1000)."))
                }
            }
        }
        
        if (!(missing(station))){
            # station: numeric
            if (class(station) != "numeric") {
                errors <- c(errors, paste0("Error ", l(errors), ": 'station' ",
                                           "must be type 'numeric'."))
            }
            # is.na
            if (any(is.na(station))) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'station' ",
                                           "must not contain NA's."))
            }
            # station: range
            if (!(error_river)) {
                if (min(station, na.rm = TRUE) * 1000 < station_int_min) {
                    errors <- c(errors, paste0("Error ", l(errors), ": min",
                                               "(station) must be above km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min / 1000)),
                                               " for river '", river, "'."))
                }
                if (max(station, na.rm = TRUE) * 1000 > station_int_max) {
                    errors <- c(errors, paste0("Error ", l(errors), ": max",
                                               "(station) must be below km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max / 1000)),
                                               " for river '", river, "'."))
                }
            } else {
                if (min(station, na.rm = TRUE) * 1000 < station_int_min) {
                    errors <- c(errors, paste0("Error ", l(errors), ": min",
                                               "(station) must be above km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min / 1000)),
                                               "."))
                }
                if (max(station, na.rm = TRUE) * 1000 > station_int_max) {
                    errors <- c(errors, paste0("Error ", l(errors), ": max",
                                               "(station) must be below km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max / 1000)),
                                               "."))
                }
            }
            
            stations <- as.numeric(station)
            len <- length(stations)
            
        }
        
        if (!(missing(station_int))) {
            # station_int: integer
            if (class(station_int) != "integer") {
                errors <- c(errors, paste0("Error ", l(errors), ": 'station_",
                                           "int' must be type 'integer'."))
            }
            # station_int: length
            if (any(is.na(station_int))) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'station_",
                                           "int' must not contain NA's."))
            }
            # station_int: range
            if (!(error_river)) {
                if (min(station_int, na.rm = TRUE) < station_int_min) {
                    errors <- c(errors, paste0("Error ", l(errors), ": min(sta",
                                               "tion_int) must be above ",
                                               as.character(station_int_min),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min / 1000)),
                                               ") for river '", river, "'."))
                }
                if (max(station_int, na.rm = TRUE) > station_int_max) {
                    errors <- c(errors, paste0("Error ", l(errors), ": max(sta",
                                               "tion_int) must be below ",
                                               as.character(station_int_max),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max / 1000)),
                                               ") for river '", river, "'."))
                }
            } else {
                if (min(station_int, na.rm = TRUE) < station_int_min) {
                    errors <- c(errors, paste0("Error ", l(errors), ": min(sta",
                                               "tion_int) must be above ",
                                               as.character(station_int_min),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min / 1000)),
                                               ")."))
                }
                if (max(station_int, na.rm = TRUE) > station_int_max) {
                    errors <- c(errors, paste0("Error ", l(errors), ": max(sta",
                                               "tion_int) must be below ",
                                               as.character(station_int_max),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max / 1000)),
                                               ")."))
                }
            }
            
            stations <- as.numeric(station_int / 1000)
            len <- length(stations)
            
        }
    }
    
    if (l(errors) != "1") {
        stop(paste0(errors, collapse = "\n  "))
    }
    
    #####
    # load internally used data
    # make parent environment accessible through the local environment
    e <- environment()
    p_env <- parent.env(e)
    
    # access the FLYS3 data
    if (exists("df.flys_data", where = p_env)){
        get("df.flys_data", envir = p_env)
    } else {
        utils::data("df.flys_data")
    }
    
    # prepare flys variables
    df.flys_data <- df.flys_data[df.flys_data$river == river, ]
    flys_stations <- unique(df.flys_data$station)
    flys_wls_ordered <- df.flys_data[df.flys_data$station == flys_stations[1], 
                                     "name"]
    
    #####
    # processing
    df.flys_data_app <- data.frame(river = character(),
                                   name = character(),
                                   station = numeric(),
                                   w = numeric(),
                                   stringsAsFactors = FALSE)
    for (a_wl in flys_wls_ordered) {
        id <- which(df.flys_data$name == a_wl)
        res <- stats::approx(x = df.flys_data$station[id],
                             y = df.flys_data$w[id],
                             xout = stations, rule = c(2, 2))
        df.temp <- data.frame(river = rep(river, len),
                              name = rep(a_wl, len),
                              station = stations,
                              w = round(res$y, 2),
                              stringsAsFactors = FALSE)
        df.flys_data_app <- rbind(df.flys_data_app, df.temp, 
                                  stringsAsFactors = FALSE)
    }
    
    #####
    # reorder by station and w and return
    df.flys_data_app <- df.flys_data_app[order(df.flys_data_app$station, 
                                               df.flys_data_app$w), ]
    
    return(df.flys_data_app)
    
}