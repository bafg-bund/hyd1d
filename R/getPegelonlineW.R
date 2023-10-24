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
#' @return The returned output depends on the type of the input parameter
#'   \code{time}. If \code{time} is type
#'   \code{\link[base:POSIXct]{c("POSIXct", "POSIXt")}} the
#'   returned object contains queried and interpolated water levels. If
#'   \code{time} is type \code{Date} the returned object contains daily averaged
#'   water levels.
#' 
#' @seealso \code{\link{waterLevelPegelonline}}
#' 
#' @references \insertRef{wsv_pegelonline_2018}{hyd1d}
#' 
#' @examplesIf hyd1d:::.pegelonline_status()
#'    getPegelonlineW(gauging_station = "DESSAU", time = Sys.time() - 3600)
#'    getPegelonlineW(gauging_station = "DESSAU", time = Sys.Date() - 1)
#' 
#' @export
#' 
getPegelonlineW <- function(gauging_station, time, uuid) {
    
    #####
    # assemble internal variables and check the existence of required data
    ##
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
        # query pegelonline data
        if (!curl::has_internet()) {
            stop ("The PEGELONLINE rest-api is unavailable without internet.",
                  call. = FALSE
            )
        }
        
        # assemble the request
        req <- httr2::request(.pegelonline_rest_url)
        req <- httr2::req_url_path_append(req, "stations")
        req <- httr2::req_url_path_append(req, uuid_internal)
        req <- httr2::req_url_path_append(req, "W")
        req <- httr2::req_url_path_append(req, "measurements.json")
        req <- httr2::req_url_query(req,
            start = I(paste0(strftime(min(time) 
                                      - as.difftime(60, units = "mins"),
                                      format = "%Y-%m-%d"),
                             "T00:00%2B01:00")),
            end = I(paste0(strftime(max(time) + as.difftime(60, units = "mins"),
                                    format = "%Y-%m-%dT%H:%M"),
                           "%2B01:00")))
        req <- httr2::req_method(req, "GET")
        req <- httr2::req_retry(req, max_tries = 10L)
        
        # perform the request
        resp <- httr2::req_perform(req)
        df.w <- httr2::resp_body_json(resp, simplifyVector = TRUE)
        df.w$timestamp <- strptime(df.w$timestamp, format = "%Y-%m-%dT%H:%M:%S")
        
        # interpolate w
        w <- stats::approx(x = df.w$timestamp, y = df.w$value, xout = 
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
        # query pegelonline data
        if (!curl::has_internet()) {
            stop("The PEGELONLINE rest-api is unavailable without internet.",
                 call. = FALSE)
        }
        
        # assemble the request
        req <- httr2::request(.pegelonline_rest_url)
        req <- httr2::req_url_path_append(req, "stations")
        req <- httr2::req_url_path_append(req, uuid_internal)
        req <- httr2::req_url_path_append(req, "W")
        req <- httr2::req_url_path_append(req, "measurements.json")
        req <- httr2::req_url_query(req,
            start = I(paste0(strftime(min(time)
                                      - as.difftime(60, units = "mins"),
                                      format = "%Y-%m-%d"),
                             "T00:00%2B01:00")),
            end = I(paste0(strftime(max(time) + 1
                                    + as.difftime(60, units = "mins"),
                                    format = "%Y-%m-%dT%H:%M"),
                           "%2B01:00")))
        req <- httr2::req_method(req, "GET")
        req <- httr2::req_retry(req, max_tries = 10L)
        
        # perform the request
        resp <- httr2::req_perform(req)
        df.w <- httr2::resp_body_json(resp, simplifyVector = TRUE)
        df.w$timestamp <- strptime(df.w$timestamp, format = "%Y-%m-%dT%H:%M:%S")
        
        # interpolate daily means
        w <- numeric()
        for (a_day in time) {
            a_time <- as.POSIXct(time) - as.difftime(60, units = "mins")
            b_time <- as.POSIXct(time) + as.difftime(23 * 60, units = "mins")
            id <- which(df.w$timestamp >= a_time & df.w$timestamp < b_time)
            w <- append(w, round(mean(df.w$value[id], na.rm = TRUE), 0))
        }
        
        return(w)
        
    }
    
}

.pegelonline_status <- function() {
    req <- httr2::req_url_path_append(httr2::request(.pegelonline_rest_url),
                                      "stations.json")
    if (httr2::req_perform(req)$status_code == 200) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


