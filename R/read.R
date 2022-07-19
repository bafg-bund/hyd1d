#' @name readWaterLevelFileDB
#' @rdname readWaterLevelFileDB
#' @title Read precomputed WaterLevelDataFrames from a file database on Z:
#'
#' @description Take water level data stored in file system and import them as
#'   \linkS4class{WaterLevelDataFrame}.
#'
#' @param river has to be type \code{character} with a length of one and can be
#'   either \strong{Elbe} or \strong{Rhine}.
#' @param time has to be type \code{\link[base:POSIXct]{c("POSIXct", "POSIXt")}},
#'   has to have a length of one and must be in the temporal range between
#'   \code{1960-01-01 00:00:00 CET} and now (\code{Sys.time()}).
#' @param from specifies the minimum station value and has to be either type
#'   \code{numeric} or \code{integer} with a length of one.
#' @param to specifies the maximum station value and has to have the same type
#'   like \code{from} with a length of one.
#'
#' @return a precomputed object of class \linkS4class{WaterLevelDataFrame}.
#'
#' @details The allowed minimum and maximum values of the parameters \code{from}
#'   and \code{to} are \code{river}- and type-specific. If the \code{river} is
#'   the \strong{Elbe} the allowed range is 0 - 585.7 km for type
#'   \code{numeric}, respectively 0 - 585700 m for type \code{integer}. For the
#'   river \strong{Rhine} the allowed range is 336.2 - 865.7 km for type
#'   \code{numeric}, respectively 336200 - 865700 m for type \code{integer}.
#'
#'   Internally \code{readWaterLevelFileDB} uses the dataset
#'   \code{\link{df.sections}} to locate the individual sections,
#'   \code{\link{readWaterLevelJson}} to import the individual sections'
#'   water level data, \code{\link{rbind.WaterLevelDataFrame}} to combine them
#'   to one \linkS4class{WaterLevelDataFrame} and
#'   \code{\link{subset.WaterLevelDataFrame}} to subset the resulting
#'   \linkS4class{WaterLevelDataFrame} and limit it with \code{from} and
#'   \code{to}.
#'
#' @seealso \code{\link{df.sections}}, \code{\link{readWaterLevelJson}},
#'   \code{\link{rbind.WaterLevelDataFrame}},
#'   \code{\link{subset.WaterLevelDataFrame}}
#'
#' @examples
#' \dontrun{
#' wldf <- readWaterLevelFileDB(river = "Elbe",
#'                              time = as.POSIXct("2016-12-21"),
#'                              from = 257, to = 262)
#' }
#' 
#' @export
#' 
readWaterLevelFileDB <- function(river = c("Elbe", "Rhine"), time, from, to) {
    
    ## vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    ##########
    # check basic requirements
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
        # %in% c("Elbe", "Rhine")
        if (!(river %in% c("Elbe", "Rhine"))) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'river' must be an element ",
                                       "of c('Elbe', 'Rhine')."))
            error_river <- TRUE
        }
        
        ##
        # set 'river'-specific limits of station_int
        if (!(error_river)) {
            if (river == "Elbe") {
                station_int_min <- 0
                station_int_max <- 585700
            }
            if (river == "Rhine") {
                station_int_min <- 336200
                station_int_max <- 865700
            }
            
            wldf_river <- river
            
        } else {
            station_int_min <- 0
            station_int_max <- 865700
        }
    }
    
    #####
    # time
    ##
    # presence
    if (missing(time)) {
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": The 'time' argument has to be supplied."))
    } else {
        ##
        # POISXct
        if (!(all(c(inherits(time ,"POSIXct"),
                    inherits(time ,"POSIXt"))))) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'time' must be type c('POSIXct', ",
                                       "'POSIXt')."))
        }
        
        ##
        # length
        if (length(time) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'time' must have a length ",
                                       "equal 1."))
        }
        
        ##
        # 1960-01-01 - now
        if (!(is.na(time))) {
            if (time < as.POSIXct("1960-01-01 00:00:00 CET") |
                time > Sys.time()) {
                errors <- c(errors, paste0("Error ", l(errors),
                                           ": 'time' must be between ",
                                           "1960-01-01 00:00:00 and now."))
            }
        } else {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'time' must not be NA."))
        }
        
        wldf_time <- time
        
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
        if (!(inherits(from, "integer") | inherits(from, "numeric"))) {
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
                
                wldf_from <- as.numeric(from) / 1000
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
        if (!(inherits(to, "integer") | inherits(to, "numeric"))) {
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
                
                wldf_to <- as.numeric(to) / 1000
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
                errors <- c(errors, paste0("Error ", l(errors), ": class(from",
                                           ") must be equal to class(to)."))
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
    
    #####
    # return
    if (l(errors) == "1") {
        #####
        # sections
        ##
        #  get the names of all available gauging_stations
        get("df.sections", pos = -1)
        
        # replace byte encoded letters
        columns <- c("name", "gs_upper", "gs_lower")
        
        #####
        # import the data
        ##
        # identify the relevant sections
        id_sections <- which(df.sections$river == toupper(wldf_river) &
                             df.sections$to_km >= wldf_from &
                             df.sections$from_km <= wldf_to)
        
        dir <- c(Elbe = "EL_000_586_UFD", Rhine = "RH_336_867_UFD")
        
        i <- 1
        for (s in id_sections) {
            file <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/",
                           dir[wldf_river], "/data/wl/",
                           df.sections$name[s], "/",
                           strftime(wldf_time, "%Y"), "/",
                           strftime(wldf_time, "%Y%m%d"), ".txt")
            wldf_temp <- readWaterLevelJson(file)
            if (i == 1) {
                wldf <- wldf_temp
            } else {
                wldf <- rbind.WaterLevelDataFrame(wldf, wldf_temp)
            }
            i <- i + 1L
        }
        
        # subset the data with from and to
        if (inherits(from, "numeric")) {
            wldf <- subset.WaterLevelDataFrame(wldf, 
                                               wldf$station >= from & 
                                                   wldf$station <= to)
        } else {
            wldf <- subset.WaterLevelDataFrame(wldf, 
                                               wldf$station_int >= from & 
                                                   wldf$station_int <= to)
        }
        
        # modify comments
        comment(wldf)[1] <- "Imported by readWaterLevelDB()."
        comment(wldf) <- comment(wldf)[which(comment(wldf) != "rbind(wldf's)")]
        comment(wldf) <- comment(wldf)[!(startsWith(comment(wldf), "'river'"))]
        comment(wldf) <- comment(wldf)[!(startsWith(comment(wldf), "'time'"))]
        
        return(wldf)
    } else {
        #####
        # error messages
        stop(paste0(errors, collapse="\n  "))
    }
}


#' @name readWaterLevelJson
#' @rdname readWaterLevelJson
#' @title Import a WaterLevelDataFrame from JSON
#'
#' @description Import water level data stored as JSON object in
#'   file system as \linkS4class{WaterLevelDataFrame}.
#'
#' @param file name of the file the JSON-formatted data are to be read
#'   from. If it does not contain an \emph{absolute} path, the file name is
#'   \emph{relative} to the current working directory,
#'   \code{\link[base]{getwd}()}. Tilde-expansion is performed where supported.
#'
#'   Since precomputed water level data held in file system are stored in a
#'   certain directory and file structure
#'   (Z:/../\code{river}/section/\code{year}/\code{date}.txt) parameters
#'   \code{river} and \code{time} are derived from \code{file}. For \code{file}s
#'   stored elsewhere \code{river} and \code{time} have to supplied
#'   additionally.
#' @param river has to be supplied, if the imported \code{file} is stored
#'   outside the standard directory structure so that \code{river} cannot be
#'   extracted from \code{file}. If supplied, it has to be type
#'   \code{character}, has to have a length of one and can be either
#'   \strong{Elbe} or \strong{Rhine}.
#' @param time has to be supplied, if the imported \code{file} is stored outside
#'   the standard directory structure so that \code{time} cannot be extracted
#'   from \code{file}. If supplied, it has to be type \code{\link[base:POSIXct]{c("POSIXct",
#'   "POSIXt")}}, has to have a length of one and has to be in the temporal range
#'   between \code{1960-01-01 00:00:00 CET} and now (\code{Sys.time()})
#'
#' @return an object of class \linkS4class{WaterLevelDataFrame}.
#'
#' @seealso \code{\link{writeWaterLevelJson}}
#'
#' @examples
#' \dontrun{
#' wldf <- readWaterLevelJson("Elbe/section/2016/20161221.txt")
#' }
#'
#' @export
#' 
readWaterLevelJson <- function(file, river = NULL, time = NULL) {
    
    ## vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    # wldf_comment
    wldf_comment <- c("", "", "")
    
    #####
    # file
    ##
    # character
    if (!inherits(file, "character")) {
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": 'file' must be type 'character'."))
    }
    
    ##
    # length
    if (length(file) != 1L) {
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": 'file' must have a length equal 1."))
    }
    
    ##
    # check, if file exists
    if (!(file.exists(file))) {
        if (!(file.exists(paste(sep = .Platform$file.sep, getwd(), file)))) {
            errors <- c(errors, paste0("Error ", l(errors), ": The file does ",
                                       "not exist. Please supply an ",
                                       "existing file."))
        }
    }
    wldf_comment[1] <- paste0("Imported by readWaterLevelJson() from file '",
                              file, "'.")
    
    #####
    # river
    ##
    # river(file) ...
    if(grepl("ELBE", file) | grepl("Elbe", file) |
       grepl("elbe", file) | grepl(paste0(.Platform$file.sep, "EL_"), file)) {
        wldf_river <- "Elbe"
        wldf_comment[2] <- "'river' (Elbe) was determined from 'file'."
    } else if (grepl("RHEIN", file) | grepl("Rhein", file) |
               grepl("rhein", file) | grepl("RHINE", file) |
               grepl("Rhine", file) | grepl("rhine", file) |
               grepl(paste0(.Platform$file.sep, "RH_"),
                                            file)) {
        wldf_river <- "Rhine"
        wldf_comment[2] <- "'river' (Rhine) was determined from 'file'."
    } else {
        if (missing(river)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The 'river' ",
                                       "can not be extracted from 'file'.\n   ",
                                       "        Please supply it by the ",
                                       "'river' argument."))
        }
        
        wldf_river <- ""
        
    }
    
    if (!(missing(river))) {
        
        ##
        # length
        if (length(river) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'river' must have a length equal 1."))
        }
        
        ##
        # character
        if (!inherits(river, "character")) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'river' must be type 'character'."))
        }
        
        ##
        # %in% c("Elbe", "Rhine")
        if (!(river %in% c("Elbe", "Rhine"))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'river' ",
                                       "must be an element of c('Elbe', ",
                                       "'Rhine')."))
        }
        
        ##
        # river != river(file)
        if (wldf_river != "") {
            if (river != wldf_river) {
                errors <- c(errors, paste0("Error ", l(errors), ": The river ",
                                           "name extracted from 'file' (",
                                           as.character(wldf_river), ") and ",
                                           "the 'river' argument (",
                                           as.character(river), ") conflict.\n",
                                           "Please supply a 'river' ",
                                           "argument that fits to 'file' ('",
                                           as.character(wldf_river), "')."))
            }
        }
        
        wldf_river <- river
        wldf_comment[2] <- paste0("'river' (", river, ") was supplied as ",
                                  "argument.")
        
    }
    
    #####
    # time
    wldf_time <- tryCatch(as.POSIXct(unlist(strsplit(basename(file),
                                                     ".", TRUE))[1],
                                     format="%Y%m%d"), finally = as.POSIXct(NA))
    wldf_comment[3] <- paste0("'time' (", wldf_time,
                              ") was determined from 'file'.")
    
    if (!(missing(time))) {
        
        ##
        # length
        if (length(time) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'time' must have a length equal 1."))
        }
        
        ##
        # POSIXct
        if (any(c(!inherits(time, "POSIXct"),
                  !inherits(time, "POSIXt")))) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'time' must be type c('POSIXct', ",
                                       "'POSIXt')."))
        } else {
            ##
            # 1960-01-01 - now
            if (!(is.na(time))) {
                if (time < as.POSIXct("1960-01-01 00:00:00 CET") |
                    time > Sys.time()) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'time' must ",
                                               "be between 1960-01-01 and now ",
                                               "or NA."))
                }
            }
            
            ##
            # time != time(file)
            if (time != wldf_time & (!(is.na(wldf_time)))) {
                errors <- c(errors, paste0("Error ", l(errors),
                                           ": The time extracted from 'file' (",
                                           as.character(wldf_time), ") and ",
                                           "the 'time' argument (",
                                           as.character(time), ") conflict.\n     ",
                                           "      Please remove the 'time' ",
                                           "argument or correct it that it fits",
                                           " to '", as.character(wldf_time), "'."))
            }
        }
        
        wldf_time <- time
        wldf_comment[3] <- paste0("'time' (", wldf_time, ") was supplied as ",
                                  "argument.")
        
    }
    
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    }
    
    #####
    # import
    list_wl <- RJSONIO::fromJSON(content = file, nullValue = NA)
    
    # conversion to "wl"
    len <- length(list_wl)
    df <- data.frame(id          = as.integer(rep(NA, len)),
                     station     = as.numeric(rep(NA, len)),
                     station_int = as.integer(rep(NA, len)),
                     w           = round(as.numeric(rep(NA, len)), 2))
    for(i in 1:len) {
        df$id[i] <- as.integer(list_wl[[i]][1])
        df$station[i] <- as.numeric(list_wl[[i]][2] / 1000)
        df$station_int[i] <- as.integer(list_wl[[i]][2])
        df$w[i] <- list_wl[[i]][3]
    }
    df <- unique(df)
    
    #####
    # construct the "new" WaterLevelDataFrame and return it
    wldf_data <- data.frame(station     = as.numeric(df$station),
                            station_int = as.integer(round(df$station_int,
                                                           0)),
                            w           = round(as.numeric(df$w), 2))
    row.names(wldf_data) <- df$id
    
    wldf_gs <- data.frame(id                 = integer(),
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
                          weight_do          = numeric(),
                          stringsAsFactors = FALSE)
    
    wldf <- methods::new("WaterLevelDataFrame",
                         wldf_data,
                         river                    = wldf_river,
                         time                     = wldf_time,
                         gauging_stations         = wldf_gs,
                         gauging_stations_missing = as.character(NA),
                         comment                  = wldf_comment)
    
    return(wldf)
}


#' @name readWaterLevelStationInt
#' @rdname readWaterLevelStationInt
#' @title Import integer station values to construct a WaterLevelDataFrame
#'
#' @description Import station values stored as ascii file in file system and
#'   construct and empty \linkS4class{WaterLevelDataFrame} from them.
#'
#' @param file name of the file the integer-formatted station values
#'   are to be read from. If it does not contain an \emph{absolute} path, the
#'   file name is \emph{relative} to the current working directory,
#'   \code{\link[base]{getwd}()}. Tilde-expansion is performed where supported.
#'
#'   Since \code{integer} station values in file system are stored in certain
#'   directory and file structure
#'   (Z:/../\code{river}/Abschnitt/km_values.txt) parameter \code{river} can be
#'   derived from \code{file}. For \code{file}s stored elsewhere
#'   \code{river} has to supplied additionally.
#' @param river has to be supplied, if the imported \code{file} is stored
#'   outside the standard directory structure so that \code{river} cannot be
#'   extracted from \code{file}. If supplied, it has to be type
#'   \code{character} with a length of one and can be either
#'   \strong{Elbe} or \strong{Rhine}.
#' @param time can be supplied to set the \code{time} slot prior to water level
#'   computations and save one line of code. If supplied, it has to be type
#'   \code{\link[base:POSIXct]{c("POSIXct", "POSIXt")}} with a length of
#'   one and has to be in the temporal range between
#'   \code{1960-01-01 00:00:00 CET} and now (\code{Sys.time()}).
#'
#' @return an object of class \linkS4class{WaterLevelDataFrame}.
#'
#' @seealso \code{\link{writeWaterLevelStationInt}}
#'
#' @examples
#' \dontrun{
#' wldf <- readWaterLevelStationInt("Elbe/Abschnitt1/km_values.txt")
#' }
#'
#' @export
#' 
readWaterLevelStationInt <- function(file, river = NULL, time = NULL) {
    
    ## vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    # wldf_comment
    wldf_comment <- c("", "", "")
    
    #####
    # file
    ##
    # presence
    if (missing(file)) {
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": The 'file' argument has to be supplied."))
    } else {
        ##
        # character
        if (!inherits(file, "character")) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'file' must be type 'character'."))
        }
        
        ##
        # length
        if (length(file) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'file' must have a length equal 1."))
        }
        
        ##
        # check, if file exists
        if (!(file.exists(file))) {
            if (!(file.exists(paste(sep = .Platform$file.sep, getwd(), file)))) {
                errors <- c(errors, paste0("Error ", l(errors), ": The file ",
                                           "does not exist. Please supply an ",
                                           "existing file."))
            }
        }
    }
    
    wldf_comment[1] <- paste0("Imported by readWaterLevelStationInt() ",
                              "from file '", file, "'.")
    
    #####
    # river
    ##
    # river(file) ...
    if(grepl("ELBE", file) | grepl("Elbe", file) |
       grepl("elbe", file) | grepl(paste0(.Platform$file.sep, "EL_"), file)) {
        wldf_river <- "Elbe"
        wldf_comment[2] <- "'river' (Elbe) was determined from 'file'."
    } else if (grepl("RHINE", file) | grepl("Rhine", file) |
               grepl("rhine", file) | grepl(paste0(.Platform$file.sep, "RH_"),
                                            file)) {
        wldf_river <- "Rhine"
        wldf_comment[2] <- "'river' (Rhine) was determined from 'file'."
    } else {
        if (missing(river)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The 'river' can",
                                       " not be extracted from 'file'. Please ",
                                       "supply it by the 'river' argument."))
        }
        
        wldf_river <- ""
        
    }
    
    ##
    # river(argument)
    if (!(missing(river))) {
        
        ##
        # character
        if (!inherits(river, "character")) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'river' must be type 'character'."))
        }
        
        ##
        # length
        if (length(river) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'river' must ",
                                       "have a length equal 1."))
        }
        
        ##
        # %in% c("Elbe", "Rhine")
        if (!(river %in% c("Elbe", "Rhine"))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'river' must ",
                                       "be an element of c('Elbe', 'Rhine')."))
        }
        
        ##
        # river != river(file)
        if (wldf_river != "") {
            if (river != wldf_river) {
                errors <- c(errors, paste0("Error ", l(errors), ": The river ",
                                           "name extracted from 'file' (",
                                           as.character(wldf_river), ") and ",
                                           "the 'river' argument (",
                                           as.character(river), ") conflict.\n",
                                           "Please supply a 'river' ",
                                           "argument that fits to 'file' ('",
                                           as.character(wldf_river), "')."))
            }
        }
        
        wldf_river <- river
        wldf_comment[2] <- paste0("'river' (", river, ") was supplied as ",
                                  "argument.")
    }
    
    #####
    # time
    if (!(missing(time))) {
        
        ##
        # length
        if (length(time) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'time' must have a length equal 1."))
        }
        
        ##
        # POSIXct
        if (!(all(c(inherits(time,"POSIXct"),
                    inherits(time, "POSIXt"))))) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'time' must be type c('POSIXct', ",
                                       "'POSIXt')."))
        } else {
            ##
            # 1960-01-01 - now
            if (!(is.na(time))) {
                if (time < as.POSIXct("1960-01-01 00:00:00 CET") |
                    time > Sys.time()) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'time' ",
                                               "must be between 1960-01-01 and",
                                               " now or NA."))
                }
            }
        }
        
        wldf_time <- time
        wldf_comment[3] <- paste0("'time' (", wldf_time, ") was supplied as ",
                                  "argument.")
        
    } else {
        wldf_time <- as.POSIXct(NA)
    }
    
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    }
    
    #####
    # import
    station_int <- as.integer(scan(file, what = "integer", quiet = TRUE))
    
    #####
    # construct the "new" WaterLevelDataFrame and return it
    wldf_data <- data.frame(station     = as.numeric(station_int / 1000),
                            station_int = as.integer(round(station_int, 0)),
                            w = as.numeric(rep(NA, length(station_int))))
    row.names(wldf_data) <- 1:nrow(wldf_data)
    
    wldf_gs <- data.frame(id                 = integer(),
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
                          weight_do          = numeric(),
                          stringsAsFactors = FALSE)
    
    wldf <- methods::new("WaterLevelDataFrame",
                         wldf_data,
                         river                    = wldf_river,
                         time                     = wldf_time,
                         gauging_stations         = wldf_gs,
                         gauging_stations_missing = as.character(NA),
                         comment              = as.character(wldf_comment))
    return(wldf)
    
}
