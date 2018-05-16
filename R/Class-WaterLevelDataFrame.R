#' @name WaterLevelDataFrame-class
#' @rdname WaterLevelDataFrame-class
#' @title S4 class for 1D water level data
#'
#' @description The S4 class \linkS4class{WaterLevelDataFrame} is inherited from
#'   the S3 class \code{data.frame} and stores 1D water level information 
#'   together with the official stationing along German federal waterways Elbe 
#'   and Rhein.
#' 
#' @details In addition to the 1D water level data stored in the 
#'   \code{data.frame} further slots contain necessary information used for or
#'   computed during the computation of water levels:
#'
#' @slot .Data contains the \code{data.frame} with at least three columns:
#'   \code{station}, \code{station_int} and \code{w}. The columns
#'   \code{station} and \code{station_int} represent the official stationing
#'   along the waterways in two different formats. They are totally exchangeable
#'   since \code{station <- as.numeric(station_int / 1000)} and
#'   \code{station_int <- as.integer(station * 1000)}. The column \code{w}
#'   represents the height of the waterlevel relative to normal height null
#'   (DHHN92). These first three columns are required, but further columns can
#'   be added.
#' @slot river is a required slot clearly determining the location of a station.
#'   Possible values of \code{river} have to be type \code{character}, have to
#'   have a length of one and are either \strong{Elbe} or \strong{Rhein}.
#' @slot time is a slot determining the time for which the water level has been
#'   computed. \code{time} has to be type \code{c("POSIXct", "POSIXt")}, has to
#'   have a length of one and be in the range between \code{1990-01-01 00:00:00
#'   CET} and now (\code{Sys.time()}) or \code{NA}.
#' @slot gauging_stations possibly contains a \code{data.frame} with relevant
#'   information about gauging stations within the relevant \code{river} stretch
#'   and the closer surrounding up- and downstream of the relevant \code{river}
#'   stretch. It is usually filled by the functions \code{\link{waterLevel}} or
#'   \code{\link{waterLevelPegelonline}}.
#' @slot gauging_stations_missing possibly contains a vector of type
#'   \code{character} with names of gauging stations for which no gauging data
#'   existed for the requested \code{time}. It is automatically filled by the
#'   functions \code{\link{waterLevel}}, \code{\link{waterLevelPegelonline}},
#'   \code{\link{waterLevelFlys3}} and \code{\link{waterLevelFlys3Seq}}.
#' @slot comment contains information which function has been used to create
#'   (\code{\link{WaterLevelDataFrame}}, \code{\link{readWaterLevelFileDB}},
#'   \code{\link{readWaterLevelJson}}, \code{\link{readWaterLevelStationInt}})
#'   or compute (\code{\link{waterLevel}}, \code{\link{waterLevelPegelonline}},
#'   \code{\link{waterLevelFlys3}} and \code{\link{waterLevelFlys3Seq}}) an
#'   object of class \linkS4class{WaterLevelDataFrame}.
#' 
#' @exportClass WaterLevelDataFrame
#' 
setClass(Class     = "WaterLevelDataFrame",
         slots     = c(river                    = "character",
                       time                     = "POSIXct",
                       gauging_stations         = "data.frame",
                       gauging_stations_missing = "character",
                       comment                  = "character"),
         contains  = "data.frame",
         prototype = prototype(
             
         data.frame(station     = numeric(),
                    station_int = integer(),
                    w           = numeric()),
         
         river                    = as.character(NA),
         time                     = as.POSIXct(NA),
         gauging_stations         = data.frame(id                 = integer(),
                                               gauging_station    = character(),
                                               uuid               = character(),
                                               km                 = numeric(),
                                               km_qps             = numeric(),
                                               river              = character(),
                                               longitude          = numeric(),
                                               latitude           = numeric(),
                                               mw                 = numeric(),
                                               mw_timespan        = character(),
                                               pnp                = numeric(),
                                               w                  = numeric(),
                                               wl                 = numeric(),
                                               n_wls_below_w_do   = integer(),
                                               n_wls_above_w_do   = integer(),
                                               n_wls_below_w_up   = integer(),
                                               n_wls_above_w_up   = integer(),
                                               name_wl_below_w_do = character(),
                                               name_wl_above_w_do = character(),
                                               name_wl_below_w_up = character(),
                                               name_wl_above_w_up = character(),
                                               w_wl_below_w_do    = numeric(),
                                               w_wl_above_w_do    = numeric(),
                                               w_wl_below_w_up    = numeric(),
                                               w_wl_above_w_up    = numeric(),
                                               weight_up          = numeric(),
                                               weight_do          = numeric()),
         
         gauging_stations_missing = as.character(NA),
         comment                  = as.character(NA)),
         
         validity  = function(object) {
            
             ## vector and function to catch error messages
            errors <- character()
            l <- function(errors) {as.character(length(errors) + 1)}
            
            ## slot: river
            error_river <- FALSE
            # character
            if (class(object@river) != "character") {
                errors <- c(errors, paste0("Error ", l(errors), ": 'river' ",
                                           "must be type 'character'."))
                error_river <- TRUE
            }
            # length
            if (length(object@river) != 1L) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'river' ",
                                           "must have length 1."))
                error_river <- TRUE
            }
            # %in% c('Elbe', 'Rhein')
            if (!(object@river %in% c("Elbe", "Rhein"))) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'river' ",
                                           "must be an element of ",
                                           "c('Elbe', 'Rhein')."))
                error_river <- TRUE
            }
            # set 'river'-specific limits of station_int and w
            if (!(error_river)) {
                if (object@river == "Elbe") {
                    station_int_min <- 0
                    station_int_max <- 585700
                    w_min <- 0
                    w_max <- 130
                }
                if (object@river == "Rhein") {
                    station_int_min <- 336200
                    station_int_max <- 865700
                    w_min <- 5
                    w_max <- 120
                }
            } else {
                station_int_min <- 0
                station_int_max <- 865700
                w_min <- 0
                w_max <- 130
            }
            
            ## slot: time
            # POSIXct
            if (!(all(class(object@time) == c("POSIXct", "POSIXt")))) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'time' ",
                                           "must be type c('POSIXct', ",
                                           "'POSIXt')."))
            }
            # length
            if (length(object@time) != 1L) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'time' ",
                                           "must have length 1."))
            }
            # 1990-01-01 and now
            if (!(is.na(object@time))) {
                if (object@time < as.POSIXct("1990-01-01 00:00:00 CET") |
                    object@time > Sys.time()) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'time' ",
                                               "must be 1990-01-01 00:00:00 ",
                                               "and now or NA."))
                }
            }
            
            ## slot: gauging_stations
            # data.frame
            if (class(object@gauging_stations) != "data.frame" & 
                class(object@gauging_stations) != "list"){
                errors <- c(errors, paste0("Error ", l(errors), ": 'gauging_",
                                           "stations' must be type ",
                                           "'data.frame'."))
            }
            # names
            gs_colnames <- c("id", "gauging_station", "uuid", "km", "km_qps",
                             "river", "longitude", "latitude", "mw",
                             "mw_timespan", "pnp", "w", "wl", 
                             "n_wls_below_w_do", "n_wls_above_w_do", 
                             "n_wls_below_w_up","n_wls_above_w_up", 
                             "name_wl_below_w_do", "name_wl_above_w_do", 
                             "name_wl_below_w_up", "name_wl_above_w_up", 
                             "w_wl_below_w_do", "w_wl_above_w_do", 
                             "w_wl_below_w_up", "w_wl_above_w_up", "weight_up",
                             "weight_do")
            if (!(all(names(object@gauging_stations) == gs_colnames))){
                errors <- c(errors, paste0("Error ", l(errors), ": names(",
                                           "gauging_stations) must be c('",
                                           paste0(gs_colnames,
                                                  collapse = "', '")
                                           , "')."))
            }
            # column classes
            gs_column_types <- c("integer", "character", "character", "numeric",
                                 "numeric", "character", "numeric", "numeric",
                                 "numeric", "character", "numeric", "numeric", 
                                 "numeric", "integer", "integer", "integer", 
                                 "integer","character", "character", 
                                 "character", "character", "numeric", "numeric",
                                 "numeric", "numeric", "numeric", "numeric")
            i <- 1L
            for (a_column in gs_colnames){
                if (class(object@gauging_stations[, a_column]) !=
                    gs_column_types[i]){
                    errors <- c(errors, paste0("Error ", l(errors),
                                               ": 'gauging_stations' column '",
                                               a_column, "' must be type '",
                                               gs_column_types[i], "'."))
                }
                i <- i + 1L
            }
            
            ## slot: gauging_stations_missing 
            # character
            if (class(object@gauging_stations_missing) != "character") {
                errors <- c(errors, paste0("Error ", l(errors), ": ",
                                           "'gauging_stations_missing' must ",
                                           "be type 'character'."))
            }
            
            ## slot: comment
            # character
            if (class(object@comment) != "character") {
                errors <- c(errors, paste0("Error ", l(errors), ": 'comment' ",
                                           "must be type 'character'."))
            }
            # length 
            # if (length(object@comment) != 1L) {
            #     errors <- c(errors, paste0("Error ", l(errors), ": 'comment'",
            #                                " must have length 1."))
            # }
            
            ## slot: .Data
            # data.frame
            if (class(object@.Data) != "data.frame" & 
                class(object@.Data) != "list") {
                errors <- c(errors, paste0("Error ", l(errors), ": '.Data' ",
                                           "must be type 'data.frame'."))
            }
            # names
            if (!(all(names(object)[1:3] ==
                      c("station", "station_int", "w")))) {
                errors <- c(errors, paste0("Error ", l(errors), ": names(wldf)",
                                           "[1:3] must be c('station', ",
                                           "'station_int', 'w')."))
            }
            
            ## .Data$station
            # numeric
            if (class(object$station) != "numeric") {
                errors <- c(errors, paste0("Error ", l(errors), ": ",
                                           "'station' must be type ",
                                           "'numeric'."))
            }
            # is.na
            if (any(is.na(object$station))) {
                errors <- c(errors, paste0("Error ", l(errors), ": ",
                                           "'station' must not contain ",
                                           "NA's."))
            }
            # range (Elbe: 0 - 585700; Rhein: 336200 - 865700)
            if (!(error_river)) {
                if (min(object$station) < station_int_min / 1000) {
                    errors <- c(errors, paste0("Error ", l(errors), ": ",
                                               "min(station) must be ",
                                               "above km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min / 1000)),
                                               " for river '",
                                               object@river, "'."))
                }
                if (max(object$station) > station_int_max / 1000) {
                    errors <- c(errors, paste0("Error ", l(errors), ": ",
                                               "max(station) must be ",
                                               "below km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max / 1000)),
                                               " for river '",
                                               object@river, "'."))
                }
            } else {
                if (min(object$station) < station_int_min / 1000) {
                    errors <- c(errors, paste0("Error ", l(errors), ": ",
                                               "min(station) must be ",
                                               "above km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min / 1000)),
                                               "."))
                }
                if (max(object$station) > station_int_max / 1000) {
                    errors <- c(errors, paste0("Error ", l(errors), ": ",
                                               "max(station) must be ",
                                               "below km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max / 1000)),
                                               "."))
                }
            }
            
            ## .Data$station_int
            # integer
            if (class(object$station_int) != "integer") {
                errors <- c(errors, paste0("Error ", l(errors), ": ",
                                           "'station_int' must be type ",
                                           "'integer'."))
            }
            # is.na
            if (any(is.na(object$station_int))) {
                errors <- c(errors, paste0("Error ", l(errors), ": ",
                                           "'station_int' must not contain ",
                                           "NA's."))
            }
            # range (Elbe: 0 - 585700; Rhein: 336200 - 865700)
            if (!(error_river)) {
                if (min(object$station_int) < station_int_min) {
                    errors <- c(errors, paste0("Error ", l(errors), ": ",
                                               "min(station_int) must be ",
                                               "above ",
                                               as.character(station_int_min),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min / 1000)),
                                               ") for river '",
                                               object@river, "'."))
                }
                if (max(object$station_int) > station_int_max) {
                    errors <- c(errors, paste0("Error ", l(errors), ": ",
                                               "max(station_int) must be ",
                                               "below ",
                                               as.character(station_int_max),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max / 1000)),
                                               ") for river '",
                                               object@river, "'."))
                }
            } else {
                if (min(object$station_int) < station_int_min) {
                    errors <- c(errors, paste0("Error ", l(errors), ": ",
                                               "min(station_int) must be ",
                                               "above ",
                                               as.character(station_int_min),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_min / 1000)),
                                               ")."))
                }
                if (max(object$station_int) > station_int_max) {
                    errors <- c(errors, paste0("Error ", l(errors), ": ",
                                               "max(station_int) must be ",
                                               "below ",
                                               as.character(station_int_max),
                                               " (km ",
                                               as.character(
                                                   as.numeric(
                                                       station_int_max / 1000)),
                                               ")."))
                }
            }
            
            ## .Data$station == .Data$station_int / 1000
            if (!(all(object$station == object$station_int / 1000))){
                errors <- c(errors, paste0("Error ", l(errors), ": ",
                                           "'station_int' must be equal ",
                                           "to as.integer(station * 1000)."))
            }
            
            ## .Data$w
            # numeric
            if (class(object$w) != "numeric") {
                errors <- c(errors, paste0("Error ", l(errors), ": 'w' must ",
                                           "be type 'numeric'."))
            }
            # range (Elbe: 130 - 0; Rhein: 120 - 5)
            if (!(error_river)) {
                if (!(all(is.na(object$w)))) {
                    if (min(object$w, na.rm = TRUE) < w_min) {
                        errors <- c(errors, paste0("Error ", l(errors), ": ",
                                                   "min(w) must be above ",
                                                    as.character(w_min), " m ",
                                                    "a.s.l. (DHHN92) for river",
                                                    " '", object@river, "'."))
                    }
                    if (max(object$w, na.rm = TRUE) > w_max) {
                        errors <- c(errors, paste0("Error ", l(errors), ": ",
                                                   "max(w) must be below ",
                                                   as.character(w_max), " m ",
                                                   "a.s.l. (DHHN92) for river",
                                                   " '", object@river, "'."))
                    }
                }
            } else {
                if (!(all(is.na(object$w)))) {
                    if (min(object$w, na.rm = TRUE) < w_min) {
                        errors <- c(errors, paste0("Error ", l(errors), ": ",
                                                   "min(w) must be above ",
                                                   as.character(w_min),
                                                   " m a.s.l. (DHHN92)."))
                    }
                    if (max(object$w, na.rm = TRUE) > w_max) {
                        errors <- c(errors, paste0("Error ", l(errors), ": ",
                                                   "max(w) must be below ",
                                                   as.character(w_max),
                                                   " m a.s.l. (DHHN92)."))
                    }
                }
            }
            # length(station_int) == length(w)
            if (length(object$station_int) != length(object$w)) {
                    errors <- c(errors, paste0("Error ", l(errors), ": ",
                                               "'station_int' and 'w' must ",
                                               "have equal length."))
            } 
# order
# else {
#     if (!(all(order(object$station_int) == order(-object$w)))) { 
#         len_in_order <- sum((order(object$station_int) == order(-object$w)))
#         len_total <- length(object$station_int) 
#         if (len_in_order/len_total < 0.8) {
#             errors <- c(errors, paste0("Error ", l(errors), ": ",
#                                        "'station_int', and 'w' ",
#                                        "should be in inversed ",
#                                        "order since 'w' decreases ",
#                                        "in flow direction with ",
#                                        "increasing 'station_int'."))
#         }
#     }
# }
            
            ## return
            if (l(errors) != "1") {
                stop(paste0(errors, collapse = "\n  "))
            } else {
                TRUE
            }
        })

#' @name WaterLevelDataFrame
#' @rdname WaterLevelDataFrame
#' @title Initialize a WaterLevelDataFrame
#'
#' @description To initialize an object of class \code{WaterLevelDataFrame} this
#'   function should be used. It checks all the required input data and 
#'   validates the final object.
#'
#' @param river a required argument to fill the \code{WaterLevelDataFrame}-slot
#'   \code{river}. It has to be type \code{"character"}, has to have a length of
#'   one and can be either "Elbe" or "Rhein".
#' @param time a required argument to fill the \code{WaterLevelDataFrame}-slot
#'   \code{time}. It has to be type \code{c("POSIXct", "POSIXt")}, has to have a
#'   length of one and must be in the temporal range between \code{1990-01-01
#'   00:00:00 CET} and now (\code{Sys.time()}) or be \code{NA}.
#' @param gauging_stations a slot of class \code{"data.frame"}.
#'   \code{gauging_stations} has to be a \code{"data.frame"} with the following
#'   columns and column types: "id" (\code{"integer"}), "gauging_station"
#'   (\code{"character"}), "uuid" (\code{"character"}), "km" (\code{"numeric"}),
#'   "km_qps" (\code{"numeric"}), "river" (\code{"character"}),
#'   "longitude" (\code{"numeric"}), "latitude" (\code{"numeric"}), "mw"
#'   (\code{"numeric"}), "pnp" (\code{"numeric"}), "w" (\code{"numeric"}), "wl"
#'   (\code{"numeric"}), "n_wls_below_w_do" (\code{"integer"}),
#'   "n_wls_above_w_do" (\code{"integer"}), "n_wls_below_w_up"
#'   (\code{"integer"}), "n_wls_above_w_up" (\code{"integer"}),
#'   "name_wl_below_w_do" (\code{"character"}), "name_wl_above_w_do"
#'   (\code{"character"}), "name_wl_below_w_up" (\code{"character"}),
#'   "name_wl_above_w_up" (\code{"character"}), "w_wl_below_w_do"
#'   (\code{"numeric"}), "w_wl_above_w_do" (\code{"numeric"}), "w_wl_below_w_up"
#'   (\code{"numeric"}), "w_wl_above_w_up" (\code{"numeric"}), "weight_up"
#'   (\code{"numeric"}), "weight_do" (\code{"numeric"}).
#' @param gauging_stations_missing a possible argument to fill the
#'   \code{WaterLevelDataFrame}-slot \code{gauging_stations_missing}. It has to
#'   be type \code{"character"} and usually contains a vector with names of
#'   gauging stations for which no water level information was available for the
#'   specified \code{time}. This argument is used by the functions
#'   \code{\link{waterLevel}}, \code{\link{waterLevelPegelonline}},
#'   \code{\link{waterLevelFlys3}} and \code{\link{waterLevelFlys3Seq}}.
#' @param comment a possible argument to fill the
#'   \code{WaterLevelDataFrame}-slot \code{comment}. It has to be type
#'   \code{"character"} and is used by the functions
#'   \code{\link{WaterLevelDataFrame}}, \code{\link{readWaterLevelFileDB}},
#'   \code{\link{readWaterLevelJson}}, \code{\link{readWaterLevelStationInt}},
#'   \code{\link{waterLevel}}, \code{\link{waterLevelPegelonline}},
#'   \code{\link{waterLevelFlys3}} and \code{\link{waterLevelFlys3Seq}}.
#' @param id a possible argument to hand over the \code{row.names(wldf)}.
#'   \code{id} has to be type \code{"integer"} and has to have the same length
#'   as other optional arguments (\code{station}, \code{station_int} and
#'   \code{w}) forming the \code{"data.frame"}-component of a
#'   \code{WaterLevelDataFrame}.
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
#' @param w a possible argument to hand over the water level information along
#'   the stationing of the specified \code{river} for a given \code{time}. If
#'   specified, it has to be type \code{"numeric"} and has to have the same
#'   length as other optional arguments (\code{id}, \code{station} and
#'   \code{station_int}) forming the \code{"data.frame"}-component of a
#'   \code{WaterLevelDataFrame}. If not specified, the respective
#'   \code{WaterLevelDataFrame}-column \code{w} can be computed by the functions
#'   \code{\link{waterLevel}}, \code{\link{waterLevelPegelonline}},
#'   \code{\link{waterLevelFlys3}} and \code{\link{waterLevelFlys3Seq}}. Minimal
#'   and maximal allowed values of \code{w} are river-specific: Elbe (m a.s.l. 0
#'   - 130), Rhein (m a.s.l. 5 - 120).
#'
#' @return The function above produces an object of class
#'   \code{"WaterLevelDataFrame"} which might contain 1D water level data and
#'   information to recompute it.
#'
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf <- waterLevel(wldf)
#' 
#' @export
#' 
WaterLevelDataFrame <- function(river = c("Elbe", "Rhein"),
                                time,
                                gauging_stations         = NULL,
                                gauging_stations_missing = NULL,
                                comment                  = NULL,
                                id                       = NULL,
                                station                  = NULL,
                                station_int              = NULL,
                                w                        = NULL){
    
    ## vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    ##### check arguments
    ## river
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
    
    ## time
    # presence
    if (missing(time)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'time' ",
                                   "argument has to be supplied."))
    } else {
        # POSIXct
        if (!(all(class(time) == c("POSIXct", "POSIXt")))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'time' must ",
                                       "be type c('POSIXct', 'POSIXt')."))
        }
        # length
        if (length(time) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'time' must ",
                                       "have length 1."))
        }
        # 1990-01-01
        if (!(is.na(time))) {
            if (time < as.POSIXct("1990-01-01 00:00:00 CET") |
                time > Sys.time()) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'time' ",
                                           "must be 1990-01-01 00:00:00 and ",
                                           "now or NA."))
            }
        }
        wldf_time <- time
    }
    
    ## gauging_stations
    # presence
    if (missing(gauging_stations)) {
        wldf_gs <- data.frame(id                 = integer(),
                              gauging_station    = character(),
                              uuid               = character(),
                              km                 = numeric(),
                              km_qps             = numeric(),
                              river    = character(),
                              longitude          = numeric(),
                              latitude           = numeric(),
                              mw                 = numeric(),
                              mw_timespan        = character(),
                              pnp                = numeric(),
                              w                  = numeric(),
                              wl                 = numeric(),
                              n_wls_below_w_do   = integer(),
                              n_wls_above_w_do   = integer(),
                              n_wls_below_w_up   = integer(),
                              n_wls_above_w_up   = integer(),
                              name_wl_below_w_do = character(),
                              name_wl_above_w_do = character(),
                              name_wl_below_w_up = character(),
                              name_wl_above_w_up = character(),
                              w_wl_below_w_do    = numeric(),
                              w_wl_above_w_do    = numeric(),
                              w_wl_below_w_up    = numeric(),
                              w_wl_above_w_up    = numeric(),
                              weight_up          = numeric(),
                              weight_do          = numeric(),
                              stringsAsFactors = FALSE)
    } else {
        # data.frame
        if (class(gauging_stations) != "data.frame"){
            errors <- c(errors, paste0("Error ", l(errors), ": 'gauging_stat",
                                       "ions' must be type 'data.frame'."))
        }
        # names(gauging_stations)
        gauging_stations_colnames <- c("id", "gauging_station", "uuid", "km",
                                     "km_qps", "river", "longitude",
                                     "latitude", "mw", "mw_timespan", "pnp", 
                                     "w", "wl", "n_wls_below_w_do", 
                                     "n_wls_above_w_do", "n_wls_below_w_up", 
                                     "n_wls_above_w_up", "name_wl_below_w_do", 
                                     "name_wl_above_w_do", "name_wl_below_w_up",
                                     "name_wl_above_w_up", "w_wl_below_w_do", 
                                     "w_wl_above_w_do", "w_wl_below_w_up", 
                                     "w_wl_above_w_up", "weight_up", 
                                     "weight_do")
        if (!(all(names(gauging_stations) == gauging_stations_colnames))){
            errors <- c(errors, paste0("Error ", l(errors), ": The columns of ",
                                       "'gauging_stations' must be '",
                                       paste(sep = "', '",
                                             gauging_stations_colnames,
                                             collapse = TRUE),
                                       "' in this order."))
        }
        wldf_gs <- gauging_stations
    }
    
    ## gauging_stations_missing
    # presence
    if (missing(gauging_stations_missing)) {
        wldf_gsm <- as.character(NA)
    } else {
        # character
        if (class(gauging_stations_missing) != "character") {
            errors <- c(errors, paste0("Error ", l(errors), ": ",
                                       "'gauging_stations_missing' must be ",
                                       "type 'character'."))
        }
        wldf_gsm <- gauging_stations_missing
    }
    
    ## comment
    # presence
    if (missing(comment)) {
        wldf_comment <- paste0("Initialised by WaterLevelDataFrame().")
    } else {
        # character
        if (class(comment) != "character") {
            errors <- c(errors, paste0("Error ", l(errors), ": 'comment' must ",
                                       "be type 'character'."))
        }
        # length 
        # if (length(comment) != 1L){
        #     errors <- c(errors, paste0("Error ", l(errors), ": 'comment' ",
        #                                "must have length 1."))
        # }
        wldf_comment <- comment
    }
    
    ## id
    # presence
    if (!missing(id)) {
        # integer
        if (class(id) != "integer") {
            errors <- c(errors, paste0("Error: ", l(errors), ": 'id' must be ",
                                       "type 'integer'."))
        }
        wldf_id <- id
    }
    
    ## station_int & station & w
    # station_int & station
    if (missing(station_int) & missing(station)) {
        errors <- c(errors, paste0("Error: ", l(errors), ": At least one ",
                                   "station argument ('station_int' or ",
                                   "'station') must be supplied."))
    }
    if (!(missing(station_int)) & !(missing(station))) {
        if (length(station_int) != length(station)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The length of ",
                                       "'station_int' and 'station' must be ",
                                       "equal."))
        } else {
            if (!(all(station_int == as.integer(station * 1000)))) {
                errors <- c(errors, paste0("Error ", l(errors), ": If both ",
                                           "station arguments ('station_int',",
                                           " 'station') are supplied, all ",
                                           "elements of 'station_int' must ",
                                           "be equal to as.integer(station ",
                                           "* 1000)."))
            }
        }
    }
    # station_int & w | station & w
    if (!(missing(station_int))) {
        # station_int: integer
        if (class(station_int) != "integer") {
            errors <- c(errors, paste0("Error: ", l(errors), ": 'station_int' ",
                                       "must be type 'integer'."))
        }
        # station_int: is.na
        if (any(is.na(station_int))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'station_int' ",
                                       "must not contain NA's."))
        }
        # station_int: range
        if (!(error_river)) {
            if (min(station_int, na.rm = TRUE) < station_int_min) {
                errors <- c(errors, paste0("Error ", l(errors), ": min(station",
                                           "_int) must be above ",
                                           as.character(station_int_min),
                                           " (km ",
                                           as.character(
                                               as.numeric(
                                                   station_int_min / 1000)),
                                           ") for river '", river, "'."))
            }
            if (max(station_int, na.rm = TRUE) > station_int_max) {
                errors <- c(errors, paste0("Error ", l(errors), ": max(statio",
                                           "n_int) must be below ",
                                           as.character(station_int_max),
                                           " (km ",
                                           as.character(
                                               as.numeric(
                                                   station_int_max / 1000)),
                                           ") for river '", river, "'."))
            }
        } else {
            if (min(station_int, na.rm = TRUE) < station_int_min) {
                errors <- c(errors, paste0("Error ", l(errors), ": min(station",
                                           "_int) must be above ",
                                           as.character(station_int_min),
                                           " (km ",
                                           as.character(
                                               as.numeric(
                                                   station_int_min / 1000)),
                                           ")."))
            }
            if (max(station_int, na.rm = TRUE) > station_int_max) {
                errors <- c(errors, paste0("Error ", l(errors), ": max(station",
                                           "_int) must be below ",
                                           as.character(station_int_max),
                                           " (km ",
                                           as.character(
                                               as.numeric(
                                                   station_int_max / 1000)),
                                           ")."))
            }
        }
        # station_int: length
        len_int <- length(station_int)
        # length(id)
        if (!(missing(id))) {
            if (length(wldf_id) != len_int) {
                errors <- c(errors, paste0("Error: ", l(errors), ": The ",
                                           "lengths of 'id' and 'station_",
                                           "int' have to be equal."))
            }
        } else {
            wldf_id <- 1:len_int
        }
        wldf_si <- station_int
        wldf_s <- wldf_si / 1000
        if (!(missing(w))) {
            # w: numeric
            if (class(w) != "numeric") {
                errors <- c(errors, paste0("Error: ", l(errors),
                                           ": 'w' ", "must be type 'numeric'."))
            }
            
            # w: length
            if (len_int != length(w)) {
                errors <- c(errors, paste0("Error: ", l(errors), ": The ",
                                           "lengths of 'station_int' and 'w' ",
                                           "must be equal."))
            }
            # w: range
            if (!(error_river)) {
                if (min(w, na.rm = TRUE) < w_min) {
                  errors <- c(errors, paste0("Error ", l(errors), ": min(w) ",
                                             "must be above ",
                                             as.character(w_min),
                                             " m a.s.l. (DHHN92) for river '",
                                             river, "'."))
                }
                if (max(w, na.rm = TRUE) > w_max) {
                  errors <- c(errors, paste0("Error ", l(errors), ": max(w) ",
                                             "must be below ",
                                             as.character(w_max),
                                             " m a.s.l. (DHHN92) for river '",
                                             river, "'."))
                }
            } else {
                if (min(w, na.rm = TRUE) < w_min) {
                  errors <- c(errors, paste0("Error ", l(errors), ": min(w) ",
                                             "must be above ", 
                                             as.character(w_min),
                                             " m a.s.l.", " (DHHN92)."))
                }
                if (max(w, na.rm = TRUE) > w_max) {
                  errors <- c(errors, paste0("Error ", l(errors), ": max(w) ",
                                             "must be below ", 
                                             as.character(w_max),
                                             " m a.s.l.", " (DHHN92)."))
                }
            }
            wldf_w <- w
        } else {
            wldf_w <- as.numeric(rep(NA, len_int))
        }
    } else {
        # station: numeric
        if (class(station) != "numeric") {
            errors <- c(errors, paste0("Error: ", l(errors), ": 'station' ",
                                       "must be type 'numeric'."))
        }
        # is.na
        if (any(is.na(station))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'station' must ",
                                       "not contain NA's."))
        }
        # station: range
        if (!(error_river)) {
            if (min(station, na.rm = TRUE) * 1000 < station_int_min) {
                errors <- c(errors, paste0("Error ", l(errors),
                                           ": min(station) must be above km ",
                                           as.character(
                                               as.numeric(
                                                   station_int_min / 1000)),
                                           " for river '", river, "'."))
            }
            if (max(station, na.rm = TRUE) * 1000 > station_int_max) {
                errors <- c(errors, paste0("Error ", l(errors),
                                           ": max(station) must be below km ",
                                           as.character(
                                               as.numeric(
                                                   station_int_max / 1000)),
                                           " for river '", river, "'."))
            }
        } else {
            if (min(station, na.rm = TRUE) * 1000 < station_int_min) {
                errors <- c(errors, paste0("Error ", l(errors),
                                           ": min(station) must be above km ",
                                           as.character(
                                               as.numeric(
                                                   station_int_min / 1000)),
                                           "."))
            }
            if (max(station, na.rm = TRUE) * 1000 > station_int_max) {
                errors <- c(errors, paste0("Error ", l(errors),
                                           ": max(station) must be below km ",
                                           as.character(
                                               as.numeric(
                                                   station_int_max / 1000)),
                                           "."))
            }
        }
        # station: length
        len <- length(station)
        # length(id)
        if (!(missing(id))) {
            if (length(wldf_id) != len) {
                errors <- c(errors, paste0("Error: ", l(errors), ": The ",
                                           "length of 'id' and 'station' ",
                                           "must be equal."))
            }
        } else {
            wldf_id <- 1:len
        }
        wldf_s <- as.numeric(station)
        wldf_si <- as.integer(round(wldf_s * 1000, 0))
        if (!(missing(w))) {
            # w: numeric
            if (class(w) != "numeric") {
                errors <- c(errors, paste0("Error: ", l(errors), ": 'w' ",
                                           "must be type 'numeric'."))
            }
            # w: length
            if (len != length(w)) {
                errors <- c(errors, paste0("Error: ", l(errors), ": The ",
                                           "lengths of 'station' and 'w' have ",
                                           "to be equal."))
            }
            ## w: range
            if (!(error_river)) {
                if (min(w, na.rm = TRUE) < w_min) {
                  errors <- c(errors, paste0("Error ", l(errors), ": min(w) ",
                                             "must be above ",
                                             as.character(w_min),
                                             " m a.s.l. (DHHN92) for river '",
                                             river, "'."))
                }
                if (max(w, na.rm = TRUE) > w_max) {
                  errors <- c(errors, paste0("Error ", l(errors), ": max(w) ",
                                             "must be below ",
                                             as.character(w_max),
                                             " m a.s.l. (DHHN92) for river '",
                                             river, "'."))
                }
            } else {
                if (min(w, na.rm = TRUE) < w_min) {
                  errors <- c(errors, paste0("Error ", l(errors), ": min(w) ",
                                             "must be above ",
                                             as.character(w_min),
                                             " m a.s.l. (DHHN92)."))
                }
                if (max(w, na.rm = TRUE) > w_max) {
                  errors <- c(errors, paste0("Error ", l(errors), ": max(w) ",
                                             "must be below ",
                                             as.character(w_max),
                                             " m a.s.l. (DHHN92)."))
                }
            }
            wldf_w <- w
        } else {
            wldf_w <- as.numeric(rep(NA, len))
        }
    }
    
    #####
    # return
    if (l(errors) == "1") {
        wldf_data <- data.frame(station     = as.numeric(wldf_s),
                                station_int = as.integer(round(wldf_si, 0)),
                                w           = round(as.numeric(wldf_w), 2))
        row.names(wldf_data) <- wldf_id
        wldf <- methods::new("WaterLevelDataFrame",
                        wldf_data,
                        river                    = as.character(wldf_river),
                        time                     = as.POSIXct(wldf_time),
                        gauging_stations         = as.data.frame(wldf_gs),
                        gauging_stations_missing = as.character(wldf_gsm),
                        comment                  = as.character(wldf_comment))
        return(wldf)
    } else {
        stop(paste0(errors, collapse = "\n  "))
    }
}


