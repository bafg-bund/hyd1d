#' @name getPegelonlineW
#' @rdname getPegelonlineW
#' @title Get W from pegelonline.wsv.de for the specified gauging station and
#'   time
#' 
#' @description Download and temporarily interpolate or average water level data
#'   from \url{https://pegelonline.wsv.de/gast/start}.
#' 
#' @eval param_gauging_station()
#' @param time must be type \code{c("POSIXct", "POSIXt")} or \code{Date} and
#'   be in the  temporal range between 31 days ago
#'   \code{\link[base:Sys.time]{Sys.time()} - 2678400} or
#'   \code{\link[base:Sys.time]{Sys.Date()} - 31} and now
#'   (\code{\link[base:Sys.time]{Sys.time()}}) or yesterday
#'   (\code{\link[base:Sys.time]{Sys.Date()} - 1}).
#' @eval param_uuid()
#' 
#' @details This functions queries online water level data through the
#'   \href{https://en.wikipedia.org/wiki/Representational_state_transfer}{REST}
#'   service of \href{https://pegelonline.wsv.de/gast/start}{PEGELONLINE}. The
#'   gauging data from \href{https://pegelonline.wsv.de/gast/start}{PEGELONLINE}
#'   have a high temporal resolution of 15 minutes, enabling meaningful linear
#'   temporal interpolation if \code{time} is supplied with type
#'   \code{\link[base:POSIXct]{c("POSIXct", "POSIXt")}}. If \code{time} is
#'   supplied with type \code{Date} water level data are aggregated to daily
#'   averages.
#'   
#'   Since data from \href{https://pegelonline.wsv.de/gast/start}{PEGELONLINE}
#'   expire after 31 days, this function is only applicable to query unvalidated
#'   water level values for the last 31 days before function call. If you need
#'   older and validated data, feel free to contact the data service at the
#'   Federal Institute of Hydrology by email (\email{Datenstelle-M1@@bafg.de}).
#' 
#' @note Internally \code{\link[utils:download.file]{download.file}} is used to
#'   obtain the gauging data from \url{https://pegelonline.wsv.de/gast/start}.
#'   The download method can be set through the option "\code{download.file.method}":
#'   see \code{\link[base:options]{options()}}.
#' 
#' @return The returned output depends on the type of the input parameter
#'   \code{time}. If \code{time} is type
#'   \code{\link[base:POSIXct]{c("POSIXct", "POSIXt")}} the
#'   returned object contains queried and interpolated water levels. If
#'   \code{time} is type \code{Date} the returned object contains daily averaged
#'   water levels.
#' 
#' @seealso \code{\link[utils:download.file]{download.file}}, \code{\link{waterLevelPegelonline}}
#' 
#' @references \insertRef{wsv_pegelonline_2018}{hyd1d}
#' 
#' @examples
#' getPegelonlineW(gauging_station = "DESSAU", time = Sys.time() - 3600)
#' getPegelonlineW(gauging_station = "DESSAU", time = Sys.Date() - 1)
#' 
#' @export
#' 
getPegelonlineW <- function(gauging_station, time, uuid) {
    
    #####
    # assemble internal variables and check the existence of required data
    ##
    # determine download method
    method <- getOption("download.file.method")
    if (is.null(method)) {
        if (Sys.info()["sysname"] == "Windows") {
            if (compareVersion("4.2.0", as.character(getRversion())) < 0) {
                method <- "auto"
            } else {
                method <- "wininet"
            }
        } else {
            method <- "auto"
        }
    }
    
    #  get the names of all available gauging_stations
    get("df.gauging_station_data", pos = -1)
    id <- which(df.gauging_station_data$data_present)
    gs <- df.gauging_station_data$gauging_station[id]
    uuids <- df.gauging_station_data$uuid[id]
        
    # gauging_station &| uuid
    if (missing(gauging_station) & missing(uuid)) {
        stop(paste0("The 'gauging_station' or 'uuid' argument has to ",
                    "be supplied."))
    } else {
        if (!(missing(gauging_station))) {
            if (!inherits(gauging_station, "character")) {
                stop("'gauging_station' must be type 'character'.")
            }
            if (length(gauging_station) != 1) {
                stop("'gauging_station' must have length 1.")
            }
            
            if (!(gauging_station %in% gs)) {
                stop(paste0("'gauging_station' must be an element of ",
                            "c('", paste0(gs, collapse = "', '"), "')."))
            }
            id_gs <- which(gs == gauging_station)
            uuid_internal <- uuids[id_gs]
        }
        
        if (!(missing(uuid))) {
            if (!inherits(uuid, "character")) {
                stop("'uuid' must be type 'character'.")
            }
            if (length(uuid) != 1) {
                stop("'uuid' must have length 1.")
            }
            
            if (!(uuid %in% uuids)) {
                stop(paste0("'uuid' must be an element of ",
                            "c('", paste0(uuids, collapse = "', '"), "')."))
            }
            id_uu <- which(uuids == uuid)
            uuid_internal <- uuids[id_uu]
        }
        
        if (!(missing(gauging_station)) & !(missing(uuid))) {
            if (id_gs != id_uu) {
                stop("'gauging_station' and 'uuid' must fit to each ",
                     "other.\nThe uuid for the supplied 'gauging_station' ",
                     "is ", uuids[id_gs], ".\nThe gauging station for the ",
                     "supplied 'uuid' is ", gs[id_uu], ".")
            }
        }
    }
    
    ##
    # time
    if (missing(time)) {
        stop("The 'time' argument has to be supplied.")
    }
    if (!any(c(inherits(time, "POSIXct"), 
               inherits(time, "POSIXt"),
               inherits(time, "Date")))) {
        stop("'time' must be type c('POSIXct', 'POSIXt') or 'Date'.")
    }
    
    if (all(c(inherits(time, "POSIXct"), 
              inherits(time, "POSIXt")))) {
        time_min <- trunc(Sys.time() - as.difftime(31, units = "days"),
                          units = "days")
        if (any(time_min > time)) {
            stop(paste0("http://pegelonline.wsv.de provides data only for ",
                        "the time period between ", 
                        strftime(time_min, format = "%Y-%m-%d"), " 00:00 and ",
                        strftime(Sys.time(), format = "%Y-%m-%d %H:%M"),
                        ". You requested data for ",
                        strftime(min(time), format = "%Y-%m-%d"), 
                        " which is \n",
                        as.integer(difftime(Sys.time(), time, units = "days")),
                        " days in the past and out of the allowed range. ",
                        "Please adjust the submitted 'time' and retry."))
        }
        if (any(time > Sys.time())) {
            stop(paste0("http://pegelonline.wsv.de provides data only for ",
                        "the time period between ", 
                        strftime(time_min, format = "%Y-%m-%d"), " 00:00 and ",
                        strftime(Sys.time(), format = "%Y-%m-%d %H:%M"),
                        ". You requested data for ",
                        strftime(max(time), format = "%Y-%m-%d %H:%M"), 
                        " which is in the future and out of the allowed ",
                        "range. Please adjust the submitted 'time' and retry."))
        }
        
        ###
        # gauging data w as cm relative to PNP
        url <- paste0("https://www.pegelonline.wsv.de/webservices/rest-api/v2/",
                      "stations/", uuid_internal, "/W/measurements.json?start=",
                      strftime(min(time)
                               - as.difftime(60,
                                             units = "mins"),
                               format = "%Y-%m-%d"),
                      "T00:00%2B01:00&end=",
                      strftime(max(time)
                               + as.difftime(60,
                                             units = "mins"),
                               format = "%Y-%m-%dT%H:%M"),
                      "%2B01:00")
        w_string <- tryCatch({
            tf <- tempfile()
            utils::download.file(url, tf, method = method, quiet = TRUE,
                                 extra = getOption("download.file.extra"))
            tf
        }, 
        error = function(e){
            msg <- paste0("It was not possible to access gauging data from\n",
                          "https://pegelonline.wsv.de\n",
                          "Please try again later, if the server was not available.\n",
                          "Please read the notes if you recieve an SSL error.\n",
                          e)
            message(msg)
            return(NA)
        })
        w_list <- RJSONIO::fromJSON(w_string)
        if (length(w_list) == 0 | is.na(w_string)) {
            message(paste0("It was not possible to obtain gauging data from\n",
                           "https://pegelonline.wsv.de\n",
                           "Please try again later."))
            return(NA)
        }
        df.w <- data.frame(time = as.POSIXct(rep(NA, length(w_list))),
                           w    = as.numeric(rep(NA, length(w_list))))
        for(i in 1:length(w_list)) {
            df.w$time[i] <- strptime(w_list[[i]]$timestamp, 
                                     format = "%Y-%m-%dT%H:%M:%S")
            df.w$w[i] <- as.numeric(w_list[[i]]$value)
        }
        
        w <- stats::approx(x = df.w$time, y = df.w$w, xout = 
                               as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S"),
                           rule = 2, method = "linear", ties = mean)$y
        w <- round(w, 0)
        
        return(w)
        
    } else {
        time_min <- Sys.Date() - as.difftime(31, units = "days")
        if (any(time_min > time)) {
            stop(paste0("http://pegelonline.wsv.de provides data only for ",
                        "the time period between ", 
                        strftime(time_min, format = "%Y-%m-%d"), " 00:00 and ",
                        strftime(Sys.time(), format = "%Y-%m-%d %H:%M"),
                        ". You requested data for ",
                        strftime(min(time), format = "%Y-%m-%d"), 
                        " which is \n",
                        as.integer(difftime(Sys.time(), time, units = "days")),
                        " days in the past and out of the allowed range. ",
                        "Please adjust the submitted 'time' and retry."))
        }
        if (any(time >= Sys.Date())) {
            stop(paste0("http://pegelonline.wsv.de provides daily averaged ",
                        "data only for the time period between ", 
                        strftime(time_min, format = "%Y-%m-%d"), " 00:00 and ",
                        strftime(Sys.Date(), format = "%Y-%m-%d"), " 00:00",
                        ". You requested data for ",
                        strftime(max(time), format = "%Y-%m-%d"), " which is ",
                        "today or in the future and thereby out of the ",
                        "allowed range. Please adjust the submitted 'time' ",
                        "and retry."))
        }
        
        ###
        # gauging data w as cm relative to PNP
        url <- paste0("https://www.pegelonline.wsv.de/webservices/rest-api/v2/",
                      "stations/", uuid_internal, "/W/measurements.json?start=",
                      strftime(min(time)
                               - as.difftime(60,
                                             units = "mins"),
                               format = "%Y-%m-%d"),
                      "T00:00%2B01:00&end=",
                      strftime(max(time) + 1
                               + as.difftime(60,
                                             units = "mins"),
                               format = "%Y-%m-%dT%H:%M"),
                      "%2B01:00")
        w_string <- tryCatch({
            tf <- tempfile()
            utils::download.file(url, tf, method = method, quiet = TRUE,
                                 extra = getOption("download.file.extra"))
            tf
        }, 
        error = function(e){
            msg <- paste0("It was not possible to access gauging data from\n",
                          "https://pegelonline.wsv.de\n",
                          "Please try again later, if the server was not available.\n",
                          "Please read the notes if you recieve an SSL error.\n",
                          e)
            message(msg)
            return(NA)
        })
        w_list <- RJSONIO::fromJSON(w_string)
        if (length(w_list) == 0 | is.na(w_string)) {
            message(paste0("It was not possible to obtain gauging data from\n",
                           "https://pegelonline.wsv.de\n",
                           "Please try again later."))
            return(NA)
        }
        df.w <- data.frame(time = as.POSIXct(rep(NA, length(w_list))),
                           w    = as.numeric(rep(NA, length(w_list))))
        for(i in 1:length(w_list)) {
            df.w$time[i] <- strptime(w_list[[i]]$timestamp, 
                                     format = "%Y-%m-%dT%H:%M:%S")
            df.w$w[i] <- as.numeric(w_list[[i]]$value)
        }
        w <- numeric()
        for (a_day in time) {
            a_time <- as.POSIXct(time) - as.difftime(60, units = "mins")
            b_time <- as.POSIXct(time) + as.difftime(23 * 60, units = "mins")
            id <- which(df.w$time >= a_time & df.w$time < b_time)
            w <- append(w, round(mean(df.w$w[id], na.rm = TRUE), 0))
        }
        
        return(w)
        
    }
    
}
