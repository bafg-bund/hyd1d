#' @include Class-WaterLevelDataFrame.R
NULL


#' @name as.data.frame.WaterLevelDataFrame
#' @rdname as.data.frame.WaterLevelDataFrame
#' @aliases as.data.frame.WaterLevelDataFrame
#' @title Coerce a WaterLevelDataFrame to a data.frame
#' 
#' @description  A function to coerce an object of class 
#'   \linkS4class{WaterLevelDataFrame} to a \code{\link[base]{data.frame}}.
#' 
#' @param x an object of class \linkS4class{WaterLevelDataFrame}.
#' @param \dots additional arguments to be passed to the internally used
#'   \code{\link[base]{as.data.frame}}-function.
#' 
#' @return \code{as.data.frame} returns a \code{\link[base]{data.frame}}.
#' 
#' @seealso \code{\linkS4class{WaterLevelDataFrame}}, 
#'   \code{\link[base]{data.frame}}, \code{\link[base]{as.data.frame}}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' df <- as.data.frame(wldf)
#' 
#' @export
#' 
#####
# S3 method
as.data.frame.WaterLevelDataFrame <- function(x, ...) {
    if (!inherits(x, "WaterLevelDataFrame")) {
        stop("'x' must be type 'WaterLevelDataFrame'.")
    }
    df <- as.data.frame(x@.Data, ...)
    names(df) <- names(x)
    return(df)
}


#####
# S4 method
#methods::setGeneric("as.data.frame", function(x, ...) {
#    standardGeneric("as.data.frame")
#})
#
#
# @rdname as.data.frame-WaterLevelDataFrame-method
#methods::setMethod("as.data.frame", 
#                   methods::signature(x = "WaterLevelDataFrame"),
#                   function(x, ...) {
#    if (!inherits(x, "WaterLevelDataFrame")) {
#        stop("'x' must be type 'WaterLevelDataFrame'.")
#    }
#    df <- as.data.frame(x@.Data, ...)
#    names(df) <- names(x)
#    return(df)
#})


#' @name getGaugingStations
#' @rdname getGaugingStations
#' @aliases getGaugingStations,WaterLevelDataFrame-method
#' @title Extract a WaterLevelDataFrame's slot gauging_stations
#' 
#' @description A function to extract the slot \code{gauging_stations} from an 
#'   object of class \linkS4class{WaterLevelDataFrame}.
#' 
#' @param x an object of class \linkS4class{WaterLevelDataFrame}.
#' 
#' @return The function above extracts the slot \code{gauging_stations} and
#'   returns an object of class \code{\link[base]{data.frame}}, which might
#'   contain gauging station data that have been used for the interpolation of a
#'   water level for the specified date.
#' 
#' @seealso \code{\link[=setGaugingStations<-]{setGaugingStations<--method}}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf <- waterLevel(wldf)
#' getGaugingStations(wldf)
#' 
#' @exportMethod getGaugingStations
#' 
methods::setGeneric("getGaugingStations", function(x) {
    standardGeneric("getGaugingStations")
})


#' @name getGaugingStations-method
#' @rdname getGaugingStations
#' @aliases getGaugingStations,WaterLevelDataFrame-method
methods::setMethod("getGaugingStations", 
                   methods::signature("WaterLevelDataFrame"),
                   function(x) {
    if (!inherits(x, "WaterLevelDataFrame")) {
        stop("'x' must be type 'WaterLevelDataFrame'.")
    }
    return(x@gauging_stations)
})


#' @name getGaugingStationsMissing
#' @rdname getGaugingStationsMissing
#' @aliases getGaugingStationsMissing,WaterLevelDataFrame-method
#' @title Extract a WaterLevelDataFrame's slot gauging_stations_missing
#'
#' @description A function to extract the slot \code{gauging_stations_missing}
#'   from an object of class \linkS4class{WaterLevelDataFrame}.
#'
#' @param x an object of class \linkS4class{WaterLevelDataFrame}.
#'
#' @return The function above extracts the slot \code{gauging_stations_missing}
#'   and returns an object of class \code{character}, which might contain a
#'   vector with gauging stations without gauging data for the specified date.
#'
#' @seealso \code{\link[=setGaugingStationsMissing<-]{setGaugingStationsMissing<--method}}
#'
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("1991-12-16"),
#'                             station = seq(500, 501, 0.1))
#' wldf <- waterLevel(wldf)
#' getGaugingStationsMissing(wldf)
#'
#' @exportMethod getGaugingStationsMissing
#' 
methods::setGeneric("getGaugingStationsMissing", function(x) {
    standardGeneric("getGaugingStationsMissing")
})


#' @name getGaugingStationsMissing-method
#' @rdname getGaugingStationsMissing
#' @aliases getGaugingStationsMissing,WaterLevelDataFrame-method
methods::setMethod("getGaugingStationsMissing", 
                   methods::signature("WaterLevelDataFrame"),
                   function(x) {
    if (!inherits(x, "WaterLevelDataFrame")) {
        stop("'x' must be type 'WaterLevelDataFrame'.")
    }
    return(x@gauging_stations_missing)
})


#' @name getRiver
#' @rdname getRiver
#' @aliases getRiver,WaterLevelDataFrame-method
#' @title Extract a WaterLevelDataFrame's slot river
#' 
#' @description A function to extract the slot \code{river} from an object
#'   of class \linkS4class{WaterLevelDataFrame}.
#' 
#' @param x an object of class \linkS4class{WaterLevelDataFrame}.
#' 
#' @return The function above extracts the slot \code{river}
#'   and returns an object of class \code{character}.
#' 
#' @seealso \code{\link[=setRiver<-]{setRiver<--method}}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' getRiver(wldf)
#'
#' @exportMethod getRiver
#' 
methods::setGeneric("getRiver", function(x) {
    standardGeneric("getRiver")
})


#' @name getRiver-method
#' @rdname getRiver
#' @aliases getRiver,WaterLevelDataFrame-method
methods::setMethod("getRiver", 
                   methods::signature("WaterLevelDataFrame"), 
                   function(x) {
    if (!inherits(x, "WaterLevelDataFrame")) {
        stop("'x' must be type 'WaterLevelDataFrame'.")
    }
    return(x@river)
})

#' @name getTime
#' @rdname getTime
#' @aliases getTime,WaterLevelDataFrame-method
#' @title Extract a WaterLevelDataFrame's slot time
#'
#' @description A function to extract the slot \code{time} from an object of
#'   class \linkS4class{WaterLevelDataFrame}.
#' 
#' @param x an object of class \linkS4class{WaterLevelDataFrame}.
#' 
#' @return The function above extracts the slot \code{time} and returns an
#'   object of type \code{\link[base:POSIXct]{c("POSIXct", "POSIXt")}}.
#' 
#' @seealso \code{\link[=setTime<-]{setTime<--method}}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' getTime(wldf)
#' 
#' @exportMethod getTime
#' 
methods::setGeneric("getTime", function(x) {
    standardGeneric("getTime")
})


#' @name getTime-method
#' @rdname getTime
#' @aliases getTime,WaterLevelDataFrame-method
methods::setMethod("getTime", 
                   methods::signature("WaterLevelDataFrame"), 
                   function(x) {
    if (!inherits(x, "WaterLevelDataFrame")) {
        stop("'x' must be type 'WaterLevelDataFrame'.")
    }
    return(x@time)
})


#' @name names<-,WaterLevelDataFrame,character-method
#' @rdname names
#' @aliases names<-,WaterLevelDataFrame,character-method
#' @title Set names of a WaterLevelDataFrame
#' 
#' @description Function to get or set the column names of an object of class
#'   \linkS4class{WaterLevelDataFrame}.
#' 
#' @param x an object of class \linkS4class{WaterLevelDataFrame}.
#' @param value a character vector of up to the same length as \code{ncol(x)}.
#'   Since the names of the first three columns of an object of class
#'   \linkS4class{WaterLevelDataFrame} are predetermined (\code{"station",
#'   "station_int", "w"}) only the later names of additional columns can be
#'   modified.
#' 
#' @return For \code{names}, a character vector of the same length as
#'   \code{ncol(x)}.
#'
#'   For \code{names<-}, the updated object. (Note that the value of
#'   \code{names(x) <- value} is that of the assignment, \code{value}, not the
#'   return value from the left-hand side.)
#'
#' @note To access the slot names of an object of class
#'   \linkS4class{WaterLevelDataFrame} the function
#'   \code{\link[methods:slot]{slotNames}} has to be used.
#' 
#' @seealso \code{\link[base]{names}}, \code{\link[methods:slot]{slotNames}}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf <- waterLevel(wldf, TRUE)
#' names(wldf) <- c(names(wldf)[1:5], "WEIGHT_Y")
#' 
#' @export
#' 
setReplaceMethod("names",
                 methods::signature(x="WaterLevelDataFrame", value="character"),
                 function(x, value) {
    if (!inherits(x, "WaterLevelDataFrame")) {
        stop("'x' must be type 'WaterLevelDataFrame'.")
    }
    if (!inherits(value, "character")) {
        stop("'value' must be type 'character'.")
    }
    x@names <- value
    if(methods::validObject(object=x)) {
        return(x)
    }
})


#' @name rbind.WaterLevelDataFrame
#' @rdname rbind.WaterLevelDataFrame
#' @aliases rbind.WaterLevelDataFrame
#' @title Combine WaterLevelDataFrames by Rows
#' 
#' @description Take \linkS4class{WaterLevelDataFrame}s that were produced for 
#'   the same \code{river} and \code{time} and combine them by \code{r}ows.
#' 
#' @param \dots objects of class \linkS4class{WaterLevelDataFrame}.
#' 
#' @return All supplied objects of class \linkS4class{WaterLevelDataFrame} will
#'   be combined to one object of class \linkS4class{WaterLevelDataFrame} which
#'   is returned.
#' 
#' @seealso \code{\link[base:cbind]{rbind}}
#' 
#' @examples
#' wldf1 <- WaterLevelDataFrame(river   = "Elbe",
#'                              time    = as.POSIXct("2016-12-21"),
#'                              station = seq(257, 262, 0.1))
#' wldf2 <- WaterLevelDataFrame(river = "Elbe",
#'                              time = as.POSIXct("2016-12-21"),
#'                              station = seq(262, 270, 0.1))
#' wldf <- rbind(wldf1, wldf2)
#' 
#' @export
#' 
#####
# S3 method
rbind.WaterLevelDataFrame <- function(...) {
    
    dots <- list(...)
    names(dots) <- NULL
    
    # .Data
    wldf_data <- do.call(rbind.data.frame,
                         lapply(dots,
                                function(x) {
                                    as.data.frame(x)}))
    names(wldf_data) <- names(dots[[1]])
    
    # river
    wldf_river <- do.call(c, lapply(dots, function(x) {getRiver(x)}))
    if (!(all(wldf_river[1] == wldf_river))) {
        stop("getRiver(x) has to be equal for all elements.")
    }
    
    # time
    wldf_time <- do.call(c, lapply(dots, function(x) {getTime(x)}))
    if (!(all(wldf_time[1] == wldf_time))) {
        stop("getTime(x) has to be equal for all elements.")
    }
    
    # gauging_stations
    wldf_gs <- do.call(rbind.data.frame, lapply(dots, 
                           function(x) {getGaugingStations(x)}))
    id_duplicated <- duplicated(wldf_gs$gauging_station)
    duplicats <- wldf_gs$gauging_station[id_duplicated]
    columns <- names(wldf_gs)[13:ncol(wldf_gs)]
    for (a_duplicat in duplicats) {
        for (a_col in columns) {
            rows <- which(wldf_gs$gauging_station == a_duplicat)
            na <- is.na(wldf_gs[rows, a_col])
            if (any(na)) {
                value <- wldf_gs[rows, a_col][!na]
                if (length(value) == 0) {
                    wldf_gs[rows, a_col] <- NA
                } else {
                    wldf_gs[rows, a_col][na] <- value
                }
            }
        }
    }
    
    wldf_gs <- unique(wldf_gs)
    if (nrow(wldf_gs) >= 1) {
        row.names(wldf_gs) <- as.character(1:nrow(wldf_gs))
    }
    
    # keep specific columns as type character
    columns <- c("gauging_station", "uuid", "river", "mw_timespan",
                 "name_wl_below_w_do", "name_wl_above_w_do", 
                 "name_wl_below_w_up", "name_wl_above_w_up")
    for (a_column in columns) {
        wldf_gs[, a_column] <- as.character(wldf_gs[, a_column])
    }
    
    # recompute section information depending on wldf_gs
    if ("section" %in% colnames(wldf_data)) {
        for (a_section in 1:(nrow(wldf_gs) - 1)) {
            upper_limit <- wldf_gs$km_qps[a_section]
            lower_limit <- wldf_gs$km_qps[a_section + 1]
            id <- which(wldf_data$station >= upper_limit & 
                            wldf_data$station < lower_limit)
            wldf_data$section[id] <- a_section
        }
    }
    wldf_data <- unique(wldf_data)
    
    # gauging_stations_missing
    wldf_gsm <- do.call(c, lapply(dots, function(x) {
        getGaugingStationsMissing(x)}))
    wldf_gsm <- wldf_gsm[!is.na(wldf_gsm)]
    if (length(wldf_gsm) == 0) {
        wldf_gsm <- as.character(NA)
    }
    
    # comment
    wldf_comment <- do.call(c, lapply(dots, function(x) {comment(x)}))
    wldf_comment <- c("rbind(wldf's)", wldf_comment)
    
    # construct the new wldf
    wldf <- methods::new("WaterLevelDataFrame",
                         wldf_data,
                         river                    = wldf_river[1],
                         time                     = wldf_time[1],
                         gauging_stations         = wldf_gs,
                         gauging_stations_missing = wldf_gsm,
                         comment                  = wldf_comment)
    
    return(wldf)
    
}


#####
# S4 method
#methods::setGeneric("rbind", function(...) {
#    standardGeneric("rbind")
#})
#
#
# @rdname rbind-WaterLevelDataFrame-method
#methods::setMethod("rbind", 
#                   methods::signature("data.frame"),
#                   function(...) {
#    
#    dots <- list(...)
#    names(dots) <- NULL
#    
#    df <- do.call(rbind.data.frame,
#                  lapply(dots, function(x) {x}))
#    names(df) <- names(dots[[1]])
#    return(df)
#})
#
#
# @rdname rbind-WaterLevelDataFrame-method
#methods::setMethod("rbind", 
#                   methods::signature("WaterLevelDataFrame"),
#                   function(...) {
#    
#    dots <- list(...)
#    names(dots) <- NULL
#    
#    # .Data
#    wldf_data <- do.call(rbind.data.frame,
#                         lapply(dots,
#                                function(x) {
#                                    as.data.frame(x)}))
#    names(wldf_data) <- names(dots[[1]])
#    
#    # river
#    wldf_river <- do.call(c, lapply(dots, function(x) {getRiver(x)}))
#    if (!(all(wldf_river[1] == wldf_river))) {
#        stop("getRiver(x) has to be equal for all elements.")
#    }
#    
#    # time
#    wldf_time <- do.call(c, lapply(dots, function(x) {getTime(x)}))
#    if (!(all(wldf_time[1] == wldf_time))) {
#        stop("getTime(x) has to be equal for all elements.")
#    }
#    
#    # gauging_stations
#    wldf_gs <- do.call(rbind.data.frame, lapply(dots,
#                                         function(x) {getGaugingStations(x)}))
#    id_duplicated <- duplicated(wldf_gs$gauging_station)
#    duplicats <- wldf_gs$gauging_station[id_duplicated]
#    columns <- names(wldf_gs)[13:ncol(wldf_gs)]
#    for (a_duplicat in duplicats) {
#        for (a_col in columns) {
#            rows <- which(wldf_gs$gauging_station == a_duplicat)
#            na <- is.na(wldf_gs[rows, a_col])
#            if (any(na)) {
#                value <- wldf_gs[rows, a_col][!na]
#                if (length(value) == 0) {
#                    wldf_gs[rows, a_col] <- NA
#                } else {
#                    wldf_gs[rows, a_col][na] <- value
#                }
#            }
#        }
#    }
#    
#    wldf_gs <- unique(wldf_gs)
#    if (nrow(wldf_gs) >= 1) {
#        row.names(wldf_gs) <- as.character(1:nrow(wldf_gs))
#    }
#    
#    # keep specific columns as type character
#    columns <- c("gauging_station", "uuid", "water_shortname",
#                 "name_wl_below_w_do", "name_wl_above_w_do", 
#                 "name_wl_below_w_up", "name_wl_above_w_up")
#    for (a_column in columns) {
#        wldf_gs[, a_column] <- as.character(wldf_gs[, a_column])
#    }
#    
#    # recompute section information depending on wldf_gs
#    if ("section" %in% colnames(wldf_data)) {
#        for (a_section in 1:(nrow(wldf_gs) - 1)) {
#            upper_limit <- wldf_gs$km_qps[a_section]
#            lower_limit <- wldf_gs$km_qps[a_section + 1]
#            id <- which(wldf_data$station >= upper_limit & 
#                        wldf_data$station < lower_limit)
#            wldf_data$section[id] <- a_section
#        }
#    }
#    wldf_data <- unique(wldf_data)
#    
#    # gauging_stations_missing
#    wldf_gsm <- do.call(c, lapply(dots, function(x) {
#                                                getGaugingStationsMissing(x)}))
#    wldf_gsm <- wldf_gsm[!is.na(wldf_gsm)]
#    if (length(wldf_gsm) == 0) {
#        wldf_gsm <- as.character(NA)
#    }
#    
#    # comment
#    wldf_comment <- do.call(c, lapply(dots, function(x) {comment(x)}))
#    wldf_comment <- c("rbind(wldf's)", wldf_comment)
#    
#    # construct the new wldf
#    wldf <- methods::new("WaterLevelDataFrame",
#                         wldf_data,
#                         river                    = wldf_river[1],
#                         time                     = wldf_time[1],
#                         gauging_stations         = wldf_gs,
#                         gauging_stations_missing = wldf_gsm,
#                         comment                  = wldf_comment)
#    
#    return(wldf)
#    
#})


#' @name setGaugingStations<-
#' @rdname setGaugingStations
#' @aliases setGaugingStations<-,WaterLevelDataFrame,data.frame-method
#' @title Set a WaterLevelDataFrame's slot gauging_stations
#' 
#' @description A function to set the slot \code{gauging_stations} of an object
#'   of class \linkS4class{WaterLevelDataFrame}.
#' 
#' @param x an object of class \linkS4class{WaterLevelDataFrame}.
#' @param value a new value of class \code{\link[base]{data.frame}} for the
#'   \code{gauging_stations} slot. \code{value} has to be a 
#'   \code{\link[base]{data.frame}} with the following columns and column types:
#'   id (\code{integer}), gauging_station (\code{character}),
#'   uuid (\code{character}), km (\code{numeric}), km_qps (\code{numeric}),
#'   water_shortname (\code{character}), longitude (\code{numeric}), latitude
#'   (\code{numeric}), mw (\code{numeric}), pnp (\code{numeric}), w
#'   (\code{numeric}), wl (\code{numeric}), n_wls_below_w_do (\code{integer}),
#'   n_wls_above_w_do (\code{integer}), n_wls_below_w_up (\code{integer}),
#'   n_wls_above_w_up (\code{integer}), name_wl_below_w_do (\code{character}),
#'   name_wl_above_w_do (\code{character}), name_wl_below_w_up
#'   (\code{character}), name_wl_above_w_up (\code{character}),
#'   w_wl_below_w_do (\code{numeric}), w_wl_above_w_do (\code{numeric}),
#'   w_wl_below_w_up (\code{numeric}), w_wl_above_w_up (\code{numeric}),
#'   weight_up (\code{numeric}), weight_do (\code{numeric}).
#' 
#' @return The function sets a new \code{value} for the slot
#'   \code{gauging_stations} and returns an object of class
#'   \linkS4class{WaterLevelDataFrame}. Since \code{value} is normally generated
#'   inside the functions \code{\link{waterLevel}} or
#'   \code{\link{waterLevelPegelonline}} this function is of very little use
#'   outside these functions.
#' 
#' @seealso \code{\link{getGaugingStations-method}}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf <- waterLevel(wldf)
#' 
#' df <- data.frame(id = integer(),
#'                  gauging_station    = character(),
#'                  uuid               = character(),
#'                  km                 = numeric(),
#'                  km_qps             = numeric(),
#'                  river              = character(),
#'                  longitude          = numeric(),
#'                  latitude           = numeric(),
#'                  mw                 = numeric(),
#'                  mw_timespan        = character(),
#'                  pnp                = numeric(),
#'                  w                  = numeric(),
#'                  wl                 = numeric(),
#'                  n_wls_below_w_do   = integer(),
#'                  n_wls_above_w_do   = integer(),
#'                  n_wls_below_w_up   = integer(),
#'                  n_wls_above_w_up   = integer(),
#'                  name_wl_below_w_do = character(),
#'                  name_wl_above_w_do = character(),
#'                  name_wl_below_w_up = character(),
#'                  name_wl_above_w_up = character(),
#'                  w_wl_below_w_do    = numeric(),
#'                  w_wl_above_w_do    = numeric(),
#'                  w_wl_below_w_up    = numeric(),
#'                  w_wl_above_w_up    = numeric(),
#'                  weight_up          = numeric(),
#'                  weight_do          = numeric(),
#'                  stringsAsFactors   = FALSE)
#' setGaugingStations(wldf) <- df
#' 
#' @exportMethod setGaugingStations<-
#' 
methods::setGeneric("setGaugingStations<-", function(x, value) {
    standardGeneric("setGaugingStations<-")
})


#' @name setGaugingStations<--method
#' @rdname setGaugingStations
#' @aliases setGaugingStations<-,WaterLevelDataFrame,data.frame-method
methods::setMethod("setGaugingStations<-",
                   methods::signature(x = "WaterLevelDataFrame", 
                                      value = "data.frame"),
                   function(x, value) {
    
    # check basic requirements
    if (!inherits(x, "WaterLevelDataFrame")) {
        stop("'x' must be type 'WaterLevelDataFrame'.")
    }
    if (!inherits(value, "data.frame")) {
        stop("'value' must be type 'data.frame'")
    }
    gs_colnames <- c("id", "gauging_station", "uuid", "km",
                     "km_qps", "river", "longitude",
                     "latitude", "mw", "mw_timespan", "pnp", "w", "wl",
                     "n_wls_below_w_do", "n_wls_above_w_do",
                     "n_wls_below_w_up", "n_wls_above_w_up",
                     "name_wl_below_w_do", "name_wl_above_w_do",
                     "name_wl_below_w_up", "name_wl_above_w_up",
                     "w_wl_below_w_do", "w_wl_above_w_do",
                     "w_wl_below_w_up", "w_wl_above_w_up",
                     "weight_up", "weight_do")
    gs_column_types <- c("integer", "character", "character",
                         "numeric", "numeric", "character", "numeric",
                         "numeric", "numeric", "character", "numeric", 
                         "numeric", "numeric", "integer", "integer", "integer",
                         "integer", "character", "character",
                         "character", "character", "numeric",
                         "numeric", "numeric", "numeric", "numeric",
                         "numeric")
    if (!(all(names(value) == gs_colnames))) {
        stop(paste0("names(value) must be c('",
                    paste0(gs_colnames, collapse = "', '"),
                    "')."))
    }
    i <- 1L
    errors <- character()
    for (a_column in gs_colnames) {
        if (!inherits(value[,a_column], gs_column_types[i])) {
            errors <- c(errors, paste0("'value$", a_column, "' must ",
                                       "be type '",
                                       gs_column_types[i], "'."))
        }
        i <- i + 1L
    }
    if (length(errors) > 0) {
        stop(paste0(errors, collapse="\n  "))
    }
    
    # set the value
    x@gauging_stations <- value
    
    # return the validated WaterLevelShinyDataFrame
    if(methods::validObject(x)) {
        return(x)
    }
    
})


#' @name setGaugingStationsMissing<-
#' @rdname setGaugingStationsMissing
#' @aliases setGaugingStationsMissing<-,WaterLevelDataFrame,character-method
#' @title Set a WaterLevelDataFrame's slot gauging_stations_missing
#' 
#' @description A function to set the slot \code{gauging_stations_missing} of 
#'   an object of class \linkS4class{WaterLevelDataFrame}.
#' 
#' @param x an object of class \linkS4class{WaterLevelDataFrame}.
#' @param value a new value of class \code{character} for the
#'   \code{gauging_stations_missing} slot.
#' 
#' @return The function above sets a new \code{value} for the slot
#'   \code{gauging_stations_missing} and returns an object of class
#'   \linkS4class{WaterLevelDataFrame}.
#' 
#' @seealso \code{\link{getGaugingStationsMissing-method}}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' setGaugingStationsMissing(wldf) <- as.character("VOCKERODE")
#' 
#' @exportMethod setGaugingStationsMissing<-
#' 
methods::setGeneric("setGaugingStationsMissing<-", function(x, value) {
    standardGeneric("setGaugingStationsMissing<-")
})


#' @name setGaugingStationsMissing<--method
#' @rdname setGaugingStationsMissing
#' @aliases setGaugingStationsMissing<-,WaterLevelDataFrame,character-method
methods::setMethod("setGaugingStationsMissing<-",
                   methods::signature(x = "WaterLevelDataFrame", 
                                      value = "character"),
                   function(x, value) {
    
    # check basic requirements
    if (!inherits(x, "WaterLevelDataFrame")) {
        stop("'x' must be type 'WaterLevelDataFrame'.")
    }
    if (!inherits(value, "character")) {
        stop("'value' must be type 'character'.")
    }
    
    # set the value
    x@gauging_stations_missing <- value
    
    # return the validated WaterLevelDataFrame
    if(methods::validObject(x)) {
        return(x)
    }
    
})


#' @name setRiver<-
#' @rdname setRiver
#' @aliases setRiver<-,WaterLevelDataFrame,character-method
#' @title Set a WaterLevelDataFrame's slot river
#' 
#' @description A function to set the slot \code{river} of an object of class
#'   \linkS4class{WaterLevelDataFrame}.
#' 
#' @param x an object of class \linkS4class{WaterLevelDataFrame}.
#' @param value a new value of class \code{character} for the \code{river}
#'   slot. \code{value} has to have a length of one and has to be \strong{Elbe}
#'   or \strong{Rhine}.
#' 
#' @return The function above sets a new \code{value} for the slot \code{river}
#'   and returns an object of class \linkS4class{WaterLevelDataFrame}. Since
#'   \code{river} is a slot relevant for the computation of the
#'   \code{\link[base]{data.frame}} column \code{w}, \code{w} is set to
#'   \code{NA} and needs to be recomputed by functions like
#'   \code{\link{waterLevel}} or \code{\link{waterLevelPegelonline}}.
#' 
#' @seealso \code{\link{getRiver-method}}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(500, 501, 0.1))
#' setRiver(wldf) <- as.character("Rhine")
#' 
#' @exportMethod setRiver<-
#' 
methods::setGeneric("setRiver<-", function(x, value) {
    standardGeneric("setRiver<-")
})


#' @name setRiver<--method
#' @rdname setRiver
#' @aliases setRiver<-,WaterLevelDataFrame,character-method
methods::setMethod("setRiver<-",
                   methods::signature(x = "WaterLevelDataFrame", 
                                      value = "character"),
                   function(x, value) {
  
    # check basic requirements
    if (!inherits(x, "WaterLevelDataFrame")) {
        stop("'x' must be type 'WaterLevelDataFrame'.")
    }
    if (!(value %in% c("Elbe", "Rhine"))) {
        stop("'value' has to be either 'Elbe' or 'Rhine'.")
    }
    
    # set the value
    x@river <- value
    
    # reset w
    x$w <- as.numeric(rep(NA, nrow(x)))
    
    # return the validated WaterLevelDataFrame
    if(methods::validObject(x)) {
        return(x)
    }
    
})


#' @name setTime<-
#' @rdname setTime
#' @aliases setTime<-,WaterLevelDataFrame,ANY-method
#' @title Set a WaterLevelDataFrame's slot time
#' 
#' @description A function to set the slot \code{time} of an object of class
#'   \linkS4class{WaterLevelDataFrame}.
#' 
#' @param x an object of class \linkS4class{WaterLevelDataFrame}.
#' @param value a new value of class \code{\link[base:POSIXct]{c("POSIXct", "POSIXt")}}
#'   for the \code{time} slot. \code{value} has to have a length of one and has
#'   to be in the temporal range between \code{1960-01-01 00:00:00 CET} and now
#'   (\code{Sys.time()} or \code{NA}.
#' 
#' @return The function above sets a new \code{value} for the slot \code{time} 
#'   and returns an object of class \linkS4class{WaterLevelDataFrame}. Since
#'   \code{time} is a slot relevant for the computation of the
#'   \code{\link[base]{data.frame}} column \code{w}, \code{w} is set to
#'   \code{NA} and needs to be recomputed by functions like
#'   \code{\link{waterLevel}} or \code{\link{waterLevelPegelonline}}.
#' 
#' @seealso \code{\link{getTime-method}}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' setTime(wldf) <- as.POSIXct("2016-12-22")
#' 
#' @exportMethod setTime<-
#' 
methods::setGeneric("setTime<-", function(x, value) {
    standardGeneric("setTime<-")
})

.setTime <- function(x, value) {
    # check basic requirements
    if (!inherits(x, "WaterLevelDataFrame")) {
        stop("'x' must be type 'WaterLevelDataFrame'.")
    }
    if (!any(c(inherits(value, "POSIXct"),
               inherits(value, "POSIXt"),
               inherits(value, "Date")))) {
        stop("'value' must be type c('POSIXct', 'POSIXlt') or 'Date'.")
    }
    if (length(value) != 1L) {
        stop("'value' must have a length equal 1.")
    }
    if (!(is.na(value))) {
        if (all(c(inherits(value, "POSIXct"),
                  inherits(value, "POSIXt")))) {
            if (value < as.POSIXct("1960-01-01 00:00:00 CET") |
                value > Sys.time()) {
                stop(paste0("'time' must be between 1960-01-01 00:00:00 and no",
                            "w or NA."))
            }
        } else {
            if (value < as.Date("1960-01-01") |
                value > Sys.Date()) {
                stop("'time' must be between 1960-01-01 and now or NA.")
            }
        }
    }
    
    # set the value
    x@time <- as.POSIXct(value)
    
    # reset w
    x$w <- as.numeric(rep(NA, nrow(x)))
    
    # return the validated WaterLevelDataFrame
    if(methods::validObject(x)) {
        return(x)
    }
}

#' @name setTime<--method
#' @rdname setTime
#' @aliases setTime<-,WaterLevelDataFrame,POSIXct-method
methods::setMethod("setTime<-",
                   methods::signature(x = "WaterLevelDataFrame", 
                                      value = "POSIXct"),
                   function(x, value) {.setTime(x, value)})

#' @name setTime<--method
#' @rdname setTime
#' @aliases setTime<-,WaterLevelDataFrame,POSIXlt-method
methods::setMethod("setTime<-",
                   methods::signature(x = "WaterLevelDataFrame", 
                                      value = "POSIXlt"),
                   function(x, value) {.setTime(x, value)})

#' @name setTime<--method
#' @rdname setTime
#' @aliases setTime<-,WaterLevelDataFrame,Date-method
methods::setMethod("setTime<-",
                   methods::signature(x = "WaterLevelDataFrame", 
                                      value = "Date"),
                   function(x, value) {.setTime(x, value)})

#' @name subset.WaterLevelDataFrame
#' @rdname subset.WaterLevelDataFrame
#' @aliases subset.WaterLevelDataFrame
#'
#' @title Subsetting WaterLevelDataFrames
#'
#' @description Returns subsets of \linkS4class{WaterLevelDataFrame}s which meet
#'   conditions.
#'
#' @param x object of class \linkS4class{WaterLevelDataFrame}.
#' @param subset logical expression indicating elements or rows to keep: missing
#'   values are taken as false.
#' @param select expression, indicating columns to select from a data frame.
#' @param drop passed on to [ indexing operator.
#' @param \dots further arguments to be passed to or from other methods.
#'
#' @return An object similar to x, containing just the selected rows and
#'   columns. All other slots of the \linkS4class{WaterLevelDataFrame} remain 
#'   unchanged.
#' 
#' @seealso \code{\link[base]{subset}}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf <- subset(wldf, station >= 258 & station <= 261)
#' 
#' @export
#' 
subset.WaterLevelDataFrame <- function(x, subset, select, drop = FALSE, ...) {
    
    wldf_river <- getRiver(x)
    wldf_time <- getTime(x)
    wldf_gs <- getGaugingStations(x)
    wldf_gsm <- getGaugingStationsMissing(x)
    wldf_comment <- comment(x)
    
    r <- if (missing(subset)) {
        rep_len(TRUE, nrow(x))
    } else {
        e <- substitute(subset)
        r <- eval(e, x, parent.frame())
        if (!is.logical(r)) 
            stop("'subset' must be logical")
        r & !is.na(r)
    }
    vars <- if (missing(select)) {
        TRUE
    } else {
        nl <- as.list(seq_along(x))
        names(nl) <- names(x)
        eval(substitute(select), nl, parent.frame())
    }
    
    # construct the "new" WaterLevelDataFrame
    wldf_data <- as.data.frame(x[r, vars, drop = drop])
    row.names(wldf_data) <- as.character(1:nrow(wldf_data))
    
    wldf <- methods::new("WaterLevelDataFrame",
                         wldf_data,
                         river                    = wldf_river,
                         time                     = wldf_time,
                         gauging_stations         = wldf_gs,
                         gauging_stations_missing = wldf_gsm,
                         comment                  = wldf_comment)
    
    return(wldf)
}


#' @name summary.WaterLevelDataFrame
#' @rdname summary.WaterLevelDataFrame
#' @aliases summary.WaterLevelDataFrame
#' 
#' @title WaterLevelDataFrame summary
#' 
#' @description Returns a list of descriptive statistics for an object of class 
#'   \linkS4class{WaterLevelDataFrame}.
#' 
#' @param object an object of class \linkS4class{WaterLevelDataFrame} for which
#'   a summary is desired.
#' @param \dots additional arguments to be passed to internally used functions.
#' 
#' @return A list of summary statistics of the \linkS4class{WaterLevelDataFrame}
#'   and its slots.
#' 
#' @seealso \code{\link[base]{summary}}
#' 
#' @examples 
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf <- waterLevel(wldf)
#' summary(wldf)
#' 
#' @export
#' 
#####
# S3 method
summary.WaterLevelDataFrame <- function(object, ...) {
    
    if (!inherits(object, "WaterLevelDataFrame")) {
        stop("'object' must be type 'WaterLevelDataFrame'.")
    }
    
    s_data <- summary(as.data.frame.WaterLevelDataFrame(object, ...))
    s_river <- getRiver(object)
    s_time <- as.character(getTime(object))
    s_gauging_stations <- getGaugingStations(object)$gauging_station
    if (length(s_gauging_stations) == 0) {
        s_gauging_stations <- "None"
    } else {
        s_gauging_stations <- paste(s_gauging_stations, collapse = ", ")
    }
    s_gauging_stations_missing <- getGaugingStationsMissing(object)
    if (length(s_gauging_stations_missing) == 0) {
        s_gauging_stations_missing <- "None"
    } else if (length(s_gauging_stations_missing) == 1) {
        if (is.na(s_gauging_stations_missing)) {
            s_gauging_stations_missing <- "None"
        }
    } else {
        s_gauging_stations_missing <- paste(s_gauging_stations_missing, 
                                            collapse = ", ")
    }
    s_comment <- comment(object)
    s_slots <- c(s_river, s_time, s_gauging_stations, 
                 s_gauging_stations_missing, s_comment)
    names(s_slots) <- c("river", "time", "gauging_stations", 
                        "gauging_stations_missing", "comment")
    s_slots <- as.data.frame(s_slots)
    names(s_slots) <- NULL
    res <- list(slots = s_slots,
                data = s_data)
    return(res)
}


#methods::setGeneric("summary", function(object, ...) {
#    standardGeneric("summary")
#})
#
#
# @name summary-method
# @rdname summary
# @aliases summary,WaterLevelDataFrame-method
#methods::setMethod(f = "summary", 
#                   methods::signature(object = "WaterLevelDataFrame"),
#                   function(object) {
#    s_data <- summary(as.data.frame(object))
#    s_river <- getRiver(object)
#    s_time <- as.character(getTime(object))
#    s_gauging_stations <- getGaugingStations(object)$gauging_station
#    if (length(s_gauging_stations) == 0) {
#        s_gauging_stations <- "None"
#    } else {
#        s_gauging_stations <- paste(s_gauging_stations, collapse = ", ")
#    }
#    s_gauging_stations_missing <- getGaugingStationsMissing(object)
#    if (length(s_gauging_stations_missing) == 0) {
#        s_gauging_stations_missing <- "None"
#    } else if (length(s_gauging_stations_missing) == 1 & 
#               is.na(s_gauging_stations_missing)) {
#        s_gauging_stations_missing <- "None"
#    } else {
#        s_gauging_stations_missing <- paste(s_gauging_stations_missing, 
#                                            collapse = ", ")
#    }
#    s_comment <- comment(object)
#    s_slots <- c(s_river, s_time, s_gauging_stations, 
#                 s_gauging_stations_missing, s_comment)
#    names(s_slots) <- c("river", "time", "gauging_stations", 
#                        "gauging_stations_missing", "comment")
#    s_slots <- as.data.frame(s_slots)
#    names(s_slots) <- NULL
#    res <- list(slots = s_slots,
#                data = s_data)
#    return(res)
#})


#' @name [.WaterLevelDataFrame
#' @rdname extract.WaterLevelDataFrame
#' @aliases [,WaterLevelDataFrame-method [,WaterLevelDataFrame,ANY,ANY-method
#' 
#' @title Extract or replace parts of a WaterLevelDataFrame
#' 
#' @description Extract or replace subsets of the \code{.Data} slot of an object
#'   of class \linkS4class{WaterLevelDataFrame}.
#' @method [ WaterLevelDataFrame
#' @param x object of class \linkS4class{WaterLevelDataFrame}.
#' @param i,j elements to extract or replace. For \code{[}, these are 
#'   \code{numeric} or \code{character} or empty. Numeric values are coerced to 
#'   integer as if by \code{\link[base:integer]{as.integer}}. For replacement 
#'   by \code{[}, a logical matrix is allowed.
#' @param value A suitable replacement value: it will be repeated a whole number
#'   of times if necessary and it may be coerced: see the Coercion section. If 
#'   \code{NULL}, deletes the column if a single column is selected.
#' 
#' @return A new object of class \linkS4class{WaterLevelDataFrame} is returned. 
#'   Since the extraction or replacement acts only on the \code{.Data}-slot of 
#'   the object, all other slots remain unchanged.
#' 
#' @details For details see \code{\link[base:[.data.frame]{[.data.frame}}.
#' 
#' @seealso \code{\link[base:[.data.frame]{[.data.frame}}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf <- wldf[which(wldf$station >= 259 & wldf$station <= 261), ]
#' 
#' @exportMethod [
#' 
methods::setMethod("[", 
                   methods::signature(x = "WaterLevelDataFrame",
                                      i = "ANY", j = "ANY"),
                   function(x, i, j) {
                   # methods::initialize(x, 
                   #                     .Data = as.data.frame(x)[i, j],
                   #                     river = getRiver(x),
                   #                     time = getTime(x),
                   #                     gauging_stations = getGaugingStations(x),
                   #                     gauging_stations_missing = 
                   #                         getGaugingStationsMissing(x),
                   #                     comment = comment(x))
   wldf_data <- as.data.frame(x)[i, j, drop = TRUE]
   wldf <- WaterLevelDataFrame(river = getRiver(x),
                               time = getTime(x),
                               gauging_stations_missing =
                                   getGaugingStationsMissing(x),
                               comment = comment(x),
                               station_int = 
                                   wldf_data$station_int,
                               w = wldf_data$w)
   return(wldf)
})


#' @name [<-.WaterLevelDataFrame
#' @rdname extract.WaterLevelDataFrame
#' @aliases [<-,WaterLevelDataFrame-method [<-,WaterLevelDataFrame,ANY,ANY,data.frame-method
#' @method [<- WaterLevelDataFrame
#' @exportMethod [<-
methods::setReplaceMethod("[",
                 methods::signature(x = "WaterLevelDataFrame",
                                    i = "ANY", j = "ANY",
                                    value = "data.frame"),
                 function(x, i, j, value) {
                     wldf_data <- as.data.frame(x)
                     wldf_data[i, j] <- value
                     # methods::initialize(x, 
                     #                     .Data = wldf_data,
                     #                     river = getRiver(x),
                     #                     time = getTime(x),
                     #                     gauging_stations = 
                     #                         getGaugingStations(x),
                     #                     gauging_stations_missing = 
                     #                         getGaugingStationsMissing(x),
                     #                     comment = comment(x))
                     wldf <- WaterLevelDataFrame(river = getRiver(x),
                                                 time = getTime(x),
                                                 gauging_stations_missing =
                                                   getGaugingStationsMissing(x),
                                                 comment = comment(x),
                                                 station_int = 
                                                   wldf_data$station_int,
                                                 w = wldf_data$w)
                     return(wldf)
})

