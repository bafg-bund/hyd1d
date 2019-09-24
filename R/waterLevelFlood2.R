#' @name waterLevelFlood2
#' @rdname waterLevelFlood2
#' @aliases waterLevelFlood2
#'
#' @title Compute 1D water level data through linear interpolation with 
#'   neighboring gauging stations according to the INFORM 3-method Flood2
#'   (Flut2)
#'
#' @description This function computes a 1D water level according to the
#'   \href{https://www.bafg.de/DE/08_Ref/U2/02_analyse/01_INFORM/inform.html}{INFORM}
#'   flood duration method Flood2 (Flut2) and stores it as column \code{w} of an
#'   S4 object of type \linkS4class{WaterLevelDataFrame}. Flood2 is designed to
#'   enable water level computation between gauging stations along waterways 
#'   without reference water levels, provided for example by FLYS3. The function
#'   uses neighboring gauging stations for linear interpolation of gauging 
#'   station water levels along the selected river stretch. Here it is provided
#'   mainly for historical reasons and more advanced functions like 
#'   \code{\link{waterLevel}} or \code{\link{waterLevelPegelonline}} be used.
#'   
#' @param wldf an object of class \linkS4class{WaterLevelDataFrame}.
#'
#' @return An object of class \linkS4class{WaterLevelDataFrame}.
#'
#' @details This function computes a water level through simple linear
#'   interpolation of water levels at neighboring gauging stations. Historically
#'   it has been designed for rivers without 1D reference water levels provided
#'   by FLYS3 for \code{\link{df.flys}}.
#' 
#' @references 
#'   \insertRef{rosenzweig_inform_2011}{hyd1d}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf1 <- waterLevelFlood2(wldf)
#' wldf1
#'
#' @export
#' 
waterLevelFlood2 <- function(wldf) {
    
    # make parent environment accessible through the local environment
    e <- environment()
    p_env <- parent.env(e)
    
    #####
    # assemble internal variables and check the existence of required data
    ##
    # vector and function to catch error messages
    errors <- character()
    l <- function(x) {as.character(length(x) + 1)}
    
    ## wldf
    # WaterLevelDataFrame
    if (class(wldf) != "WaterLevelDataFrame"){
        errors <- c(errors, paste0("Error ", l(errors), ": 'wldf' ",
                                   "must be type 'WaterLevelDataFrame'."))
    } else {
        
        # wldf variables
        df.data <- data.frame(id = row.names(wldf), station = wldf$station, 
                              station_int = wldf$station_int, w = wldf$w)
        river   <- getRiver(wldf)
        RIVER   <- toupper(river)
        
        # start
        start_f <- min(wldf$station)
        
        # end
        end_f = max(wldf$station)
        
        # time
        time    <- as.Date(trunc(getTime(wldf), units = "days"))
        if (is.na(time)){
            errors <- c(errors, paste0("Error ", l(errors), ": The time slot ",
                                       "of 'wldf' must not be NA."))
        }
    }
    
    if (l(errors) != "1"){
        stop(paste0(errors, collapse="\n  "))
    }
    
    # access the gauging_station_data
    get("df.gauging_station_data", pos = -1)
    for (a in c("gauging_station", "agency", "river")) {
        df.gauging_station_data[, a] <- asc2utf8(df.gauging_station_data[, a])
    }
    id <- which(df.gauging_station_data$river == "RHEIN" & 
                    df.gauging_station_data$km_qps < 336.2)
    df.gauging_station_data <- df.gauging_station_data[-id,]
    
    #####
    # gauging_stations
    # get a data.frame of the relevant gauging stations between start and end
    id_gs <- which(df.gauging_station_data$river == RIVER &
                       df.gauging_station_data$km_qps >= start_f &
                       df.gauging_station_data$km_qps <= end_f)
    df.gs_inarea <- df.gauging_station_data[id_gs, ]
    
    # get a data.frame of the next gauging station upstream
    id <- which(df.gauging_station_data$river == RIVER &
                    df.gauging_station_data$km_qps < start_f)
    
    # catch exception for the areas upstream of SCH\u00d6NA or IFFEZHEIM
    if (length(id > 0)) {
        id_gs <- max(id)
        df.gs_up <- df.gauging_station_data[id_gs, ]
    } else {
        if(nrow(df.gs_inarea) > 1) {
            df.gs_up <- df.gs_inarea[1, ]
        } else {
            df.gs_up <- df.gs_inarea
        }
    }
    
    # get a data.frame of the next gauging station downstream
    id <- which(df.gauging_station_data$river == RIVER &
                    df.gauging_station_data$km_qps > end_f)
    
    # catch exception for the areas downstream of GEESTHACHT or EMMERICH
    if (length(id) > 0) {
        id_gs <- min(id)
        df.gs_do <- df.gauging_station_data[id_gs,]
    } else {
        if(nrow(df.gs_inarea) > 1) {
            df.gs_do <- df.gs_inarea[nrow(df.gs_inarea), ]
        } else {
            df.gs_do <- df.gs_inarea
        }
    }
    
    #####
    # assemble a data.frame of the relevant gauging stations and resulting
    # sections to loop over ...
    # prepare an empty vector to data for the slot gauging_stations_missing
    gauging_stations_missing <- character()
    
    ###
    # add the df.gs_up to this data.frame, if w is available for the df.gs_up
    # on the specified date
    if (df.gs_up$gauging_station %in% c("GRENZE_CZ", "KEHL-KRONENHOF")) {
        df.gs_up$w <- NA_real_
        gs_up_missing <- character()
    } else {
        gs_up_missing <- character()
        w <- getGaugingDataW(df.gs_up$gauging_station, time)
        if (is.na(w)) {
            gs_up_missing <- df.gs_up$gauging_station
        }
        df.gs_up$w <- w
    }
    
    # replace df.gs_up with the next gs further upstream, if w is
    # available for the df.gs_up further upstream on the specified date
    while (length(gs_up_missing) > 0) {
        gauging_stations_missing <- append(gauging_stations_missing,
                                           paste0('up: ', gs_up_missing))
        id <- which(df.gauging_station_data$river == RIVER &
                        df.gauging_station_data$km_qps < df.gs_up$km_qps &
                        df.gauging_station_data$data_present)
        if (length(id) > 0) {
            id_gs <- max(id)
            df.gs_up <- df.gauging_station_data[id_gs, ]
        } else {
            break
        }
        gs_up_missing <- character()
        w <- getGaugingDataW(df.gs_up$gauging_station, time)
        if (is.na(w)) {
            gs_up_missing <- df.gs_up$gauging_station
        }
        df.gs_up$w <- w
    }
    
    ###
    # append the df.gs_inarea to this data.frame, if w is available for a_gs on
    # the specified date
    df.gs_inarea$w <- rep(NA_real_, nrow(df.gs_inarea))
    i <- 1
    for (a_gs in df.gs_inarea$gauging_station) {
        if (a_gs %in% 
            df.gauging_station_data$gauging_station[!
                df.gauging_station_data$data_present]) {
            no_limit <- FALSE
            w <- NA_real_
        } else {
            no_limit <- TRUE
            w <- getGaugingDataW(a_gs, time)
        }
        if (is.na(w) & no_limit) {
            gauging_stations_missing <- append(gauging_stations_missing,
                                               paste0('in: ', a_gs))
        }
        df.gs_inarea$w[i] <- w
        rm(no_limit)
        i <- i + 1
    }
    
    ###
    # append the df.gs_do to this list, if w is available for the df.gs_do on
    # the specified date
    if (df.gs_do$gauging_station %in% c("GEESTHACHT_WEHR", "GRENZE_NL")) {
        gs_do_missing <- character()
        df.gs_do$w <- NA_real_
    } else {
        gs_do_missing <- character()
        w <- getGaugingDataW(df.gs_do$gauging_station, time)
        if (is.na(w)) {
            gs_do_missing <- df.gs_do$gauging_station
        }
        df.gs_do$w <- w
    }
    
    # replace df.gs_do with the next gs further downstream, if w is
    # available for the df.gs_do further downstream on the specified date
    while (length(gs_do_missing) > 0) {
        gauging_stations_missing <- append(gauging_stations_missing,
                                           paste0('do: ', gs_do_missing))
        id <- which(df.gauging_station_data$river == RIVER &
                        df.gauging_station_data$km_qps > df.gs_do$km_qps &
                        df.gauging_station_data$data_present)
        if (length(id) > 0) {
            id_gs <- min(id)
            df.gs_do <- df.gauging_station_data[id_gs, ]
        } else {
            break
        }
        gs_do_missing <- character()
        w <- getGaugingDataW(df.gs_do$gauging_station, time)
        if (is.na(w)) {
            gs_do_missing <- df.gs_do$gauging_station
        }
        df.gs_do$w <- w
    }
    
    # bind the df.gs_. datasets and remove gauging stations which should 
    # have data, but don't have them
    df.gs <- rbind(df.gs_up, df.gs_inarea, df.gs_do, stringsAsFactors = FALSE)
    df.gs <- unique(df.gs)
    df.gs <- df.gs[order(df.gs$km_qps),]
    df.gs <- df.gs[!(df.gs$data_present & is.na(df.gs$w)),]
    
    if (nrow(df.gs) == 2) {
        if (is.na(df.gs$w[1])) {
            id_do <- which(df.gauging_station_data$river == RIVER &
                           df.gauging_station_data$km_qps > df.gs$km_qps[2] &
                           df.gauging_station_data$data_present)
            for (id_d in id_do) {
                gs <- df.gauging_station_data$gauging_station[id_d]
                w <- getGaugingDataW(gs, time)
                if (! is.na(w)) {
                    break
                } else {
                    gauging_stations_missing <- append(
                        gauging_stations_missing, paste0('do: ', gs))
                }
            }
            df.gs_do <- df.gauging_station_data[id_d,]
            df.gs_do$w <- w
            df.gs <- rbind(df.gs, df.gs_do, stringsAsFactors = FALSE)
        } else if (is.na(df.gs$w[nrow(df.gs)])) {
            id_up <- which(df.gauging_station_data$river == RIVER &
                           df.gauging_station_data$km_qps < df.gs$km_qps[1] &
                           df.gauging_station_data$data_present)
            for (id_u in rev(id_up)) {
                gs <- df.gauging_station_data$gauging_station[id_u]
                w <- getGaugingDataW(gs, time)
                if (! is.na(w)) {
                    break
                } else {
                    gauging_stations_missing <- append(
                        gauging_stations_missing, paste0('up: ', gs))
                }
            }
            df.gs_up <- df.gauging_station_data[id_u,]
            df.gs_up$w <- w
            df.gs <- rbind(df.gs_up, df.gs, stringsAsFactors = FALSE)
        }
    }
    
    # clean up temporary objects
    remove(df.gs_inarea, df.gs_do, df.gs_up)
    
    # add additional result columns to df.gs
    df.gs$wl <- round(df.gs$pnp + df.gs$w/100, 3)
    
    df.gs$n_wls_below_w_do <- as.integer(rep(NA, nrow(df.gs)))
    df.gs$n_wls_above_w_do <- as.integer(rep(NA, nrow(df.gs)))
    df.gs$n_wls_below_w_up <- as.integer(rep(NA, nrow(df.gs)))
    df.gs$n_wls_above_w_up <- as.integer(rep(NA, nrow(df.gs)))
    
    df.gs$name_wl_below_w_do <- as.character(rep(NA, nrow(df.gs)))
    df.gs$name_wl_above_w_do <- as.character(rep(NA, nrow(df.gs)))
    df.gs$name_wl_below_w_up <- as.character(rep(NA, nrow(df.gs)))
    df.gs$name_wl_above_w_up <- as.character(rep(NA, nrow(df.gs)))
    
    df.gs$w_wl_below_w_do <- as.numeric(rep(NA, nrow(df.gs)))
    df.gs$w_wl_above_w_do <- as.numeric(rep(NA, nrow(df.gs)))
    df.gs$w_wl_below_w_up <- as.numeric(rep(NA, nrow(df.gs)))
    df.gs$w_wl_above_w_up <- as.numeric(rep(NA, nrow(df.gs)))
    
    df.gs$weight_up <- as.numeric(rep(NA, nrow(df.gs)))
    df.gs$weight_do <- as.numeric(rep(NA, nrow(df.gs)))
    
    #####
    # loop over the sections
    for (s in 1:(nrow(df.gs)-1)) {
        
        ###
        # identify the stations within this section
        id <- which(wldf$station >= df.gs$km_qps[s] & 
                    wldf$station <= df.gs$km_qps[s + 1])
        
        #####
        # catch the exceptions for areas
        # upstream of:
        #   - SCHÖNA
        #   - IFFEZHEIM
        # downstream of:
        #   - GEESTHACHT
        #   - EMMERICH
        if (any(is.na(df.gs$w[c(s, s + 1)]))) {
            if (s == 1) {
                # compute df.gs$wl[1]
                x <- c(df.gs$km_qps[2], df.gs$km_qps[3])
                y <- c(df.gs$wl[2], df.gs$wl[3])
                df.gs$wl[1] <- stats::predict.lm(stats::lm(y ~ x),
                                            data.frame(x = df.gs$km_qps[1]))
                
            } else if (s == (nrow(df.gs) - 1)) {
                # compute df.gs$wl[s + 1]
                x <- c(df.gs$km_qps[s - 1], df.gs$km_qps[s])
                y <- c(df.gs$wl[s - 1], df.gs$wl[s])
                df.gs$wl[s + 1] <- stats::predict.lm(stats::lm(y ~ x),
                                            data.frame(x = df.gs$km_qps[s + 1]))
            } else {
                stop(paste0("Error: There are obviously no gauging data availa",
                            "ble\n       where they should exist!"))
            }
        }
        
        #####
        # interpolate water levels
        df.data$w[id] <- round(stats::approx(
            x = c(df.gs$km_qps[s],
                  df.gs$km_qps[s + 1]),
            y = c(df.gs$wl[s],
                  df.gs$wl[s + 1]),
            xout = df.data$station[id])$y, 2)
        
    }
    
    #####
    # assemble and return the final products
    wldf_data <- df.data[ ,c("station", "station_int", "w")]
    row.names(wldf_data) <- df.data$id
    
    df.gs <- df.gs[, c('id', 'gauging_station', 'uuid', 'km', 'km_qps',
                       'river', 'longitude', 'latitude', 'mw', 'mw_timespan', 
                       'pnp', 'w', 'wl', 'n_wls_below_w_do', 'n_wls_above_w_do',
                       'n_wls_below_w_up', 'n_wls_above_w_up',
                       'name_wl_below_w_do', 'name_wl_above_w_do',
                       'name_wl_below_w_up', 'name_wl_above_w_up',
                       'w_wl_below_w_do', 'w_wl_above_w_do', 'w_wl_below_w_up',
                       'w_wl_above_w_up', 'weight_up', 'weight_do')]
    c_columns <- c("gauging_station", "uuid", "river", "mw_timespan",
                   "name_wl_below_w_do", "name_wl_above_w_do", 
                   "name_wl_below_w_up", "name_wl_above_w_up")
    for (a_column in c_columns) {
        df.gs[ , a_column] <- as.character(df.gs[ , a_column])
    }
    
    wldf <- methods::new("WaterLevelDataFrame",
                         wldf_data,
                         river                    = river,
                         time                     = getTime(wldf),
                         gauging_stations         = df.gs,
                         gauging_stations_missing = gauging_stations_missing,
                         comment = paste0("Computed by waterLevelFlood2()"))
    return(wldf)
}

