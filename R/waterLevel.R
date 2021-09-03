#' @name waterLevel
#' @rdname waterLevel
#'
#' @title Compute a 1D water level dataset
#'
#' @description Functions to compute 1D water level information and store it as
#'   column \code{w} of an S4 object of type \linkS4class{WaterLevelDataFrame}.
#'
#' @details \code{waterLevel} interpolates 1D water level along the river axis
#'   of Elbe and Rhein based on daily averaged, mostly validated gauging data
#'   stored in the internal dataset \code{\link{df.gauging_data}}. Internally
#'   stored gauging data are available from 1960-01-01 until 2018-03-14.
#'
#'   \code{waterLevelPegelonline} carries out the interpolation with gauging
#'   data obtained through a
#'   \href{https://en.wikipedia.org/wiki/Representational_state_transfer}{REST}
#'   service from \url{https://pegelonline.wsv.de} . The gauging data from
#'   PEGELONLINE have a high temporal resolution of 15 minutes, enabling
#'   meaningful linear temporal interpolation. Since data from PEGELONLINE
#'   expire after 31 days, this function is only applicable for
#'   \linkS4class{WaterLevelDataFrame}s with a \code{time}-slot set to
#'   appropriate values within the last 31 days before function call.
#'
#' @param wldf an object of class \linkS4class{WaterLevelDataFrame}.
#' @param shiny \code{logical}, determing wether columns (\code{section},
#'   \code{weight_x}, \code{weight_y}) relevant for the
#'   \code{\link{plotShiny}()}-function are appended to the resulting
#'   \linkS4class{WaterLevelDataFrame}.
#'
#' @return An object of class \code{WaterLevelDataFrame}.
#'
#' @seealso \code{\link{plotShiny}}
#'
#' @references 
#'   \insertRef{busch_einheitliche_2009}{hyd1d}
#'   
#'   \insertRef{hkv_hydrokontor_erstellung_2014}{hyd1d}
#'   
#'   \insertRef{bundesanstalt_fur_gewasserkunde_flys_2016}{hyd1d}
#'   
#'   \insertRef{wsv_pegelonline_2018}{hyd1d}
#'
#' @examples
#' # waterLevel
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf <- waterLevel(wldf)
#'
#' # waterLevelPegelonline
#' wldf1 <- wldf
#' setTime(wldf1) <- Sys.time() - as.difftime(60, units = "mins")
#' wldf1 <- waterLevelPegelonline(wldf1)
#'
#' @export
#' 
waterLevel <- function(wldf, shiny = FALSE) {
    
    #####
    # assemble internal variables and check the existence of required data
    ##
    # wldf
    if (class(wldf) != "WaterLevelDataFrame") {
        stop("'wldf' has to be of type 'WaterLevelDataFrame'.")
    }
    df.data <- data.frame(id = row.names(wldf), station = wldf$station, 
                          station_int = wldf$station_int, w = wldf$w)
    river   <- getRiver(wldf)
    RIVER   <- toupper(river)
    time    <- as.Date(trunc(getTime(wldf), units = "days"))
    
    # start
    start_f <- min(df.data$station)
    
    # end
    end_f <- max(df.data$station)
    
    ##
    # shiny
    if (class(shiny) != "logical") {
        stop("'shiny' has to be of type 'logical'.")
    }
    if (length(shiny) != 1L) {
        stop("'shiny' must have length 1.")
    }
    
    #####
    # connect the relevant datasets
    # access the FLYS3 data
    get("df.flys", pos = -1)
    
    # prepare flys variables
    flys_stations <- unique(df.flys[df.flys$river == river, "station"])
    flys_wls <- df.flys[df.flys$station == flys_stations[1] &
                        df.flys$river == river, "name"]
    
    # access the gauging_station_data
    get("df.gauging_station_data", pos = -1)
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
    
    # collect additional results to construct a WaterLevelShinyDataFrame
    df.data_shiny <- data.frame(section  = as.integer(rep(NA, nrow(df.data))),
                                weight_x = as.numeric(rep(NA, nrow(df.data))),
                                weight_y = as.numeric(rep(NA, nrow(df.data))))
    
    #####
    # loop over the sections
    for (s in 1:(nrow(df.gs)-1)) {
        
        if (length(which(df.data$station >= df.gs$km_qps[s] &
                         df.data$station <= df.gs$km_qps[s + 1])) == 0) {
            next
        }
        
        #####
        # catch the exceptions for areas
        # upstream of:
        #   - SCHÖNA
        #   - IFFEZHEIM
        # downstream of:
        #   - GEESTHACHT
        #   - EMMERICH
        if (any(is.na(df.gs$w[c(s, s + 1)]))) {
            
            id_s <- c(s, s + 1)[!is.na(df.gs$w[c(s, s + 1)])]
            
            ###
            # query FLYS3 to get the 30 stationary water levels for the gauging
            # stations stationing
            df.flys <- waterLevelFlys3InterpolateX(river, df.gs$km_qps[id_s])
            
            ###
            # determine the computation relevant water levels
            df.gs$n_wls_below_w_up[s] <- sum(df.flys$w <= df.gs$wl[id_s])
            df.gs$n_wls_above_w_up[s] <- sum(df.flys$w > df.gs$wl[id_s])
            df.gs$n_wls_below_w_do[s + 1] <- sum(df.flys$w <= df.gs$wl[id_s])
            df.gs$n_wls_above_w_do[s + 1] <- sum(df.flys$w > df.gs$wl[id_s])
            
            wls_below <- sum(df.flys$w <= df.gs$wl[id_s])
            wls_above <- 31 - sum(df.flys$w > df.gs$wl[id_s])
            
            # special situation, that the w is
            # - below the lowest stationary water level of FLYS
            # - above the highest stationary water level of FLYS
            if (wls_below == 0) {
                wls_below <- 1
                if (wls_above == wls_below) {
                    wls_above <- wls_above + 1L
                }
            }
            if (wls_above == 31) {
                wls_above <- 30
                if (wls_below == wls_above) {
                    wls_below <- wls_below - 1L
                }
            }
            
            df.gs$name_wl_below_w_up[s] <- df.flys$name[wls_below]
            df.gs$name_wl_above_w_up[s] <- df.flys$name[wls_above]
            df.gs$name_wl_below_w_do[s + 1] <- df.flys$name[wls_below]
            df.gs$name_wl_above_w_do[s + 1] <- df.flys$name[wls_above]
            
            wl_le <- df.flys$w[wls_below]
            wl_gr <- df.flys$w[wls_above]
            df.gs$w_wl_below_w_up[s] <- wl_le
            df.gs$w_wl_above_w_up[s] <- wl_gr
            df.gs$w_wl_below_w_do[s + 1] <- wl_le
            df.gs$w_wl_above_w_do[s + 1] <- wl_gr
            
            ###
            # calculate the relative positions
            y <- (df.gs$wl[id_s] - wl_le) / (wl_gr - wl_le)
            df.gs$weight_up[s] <- y
            df.gs$weight_do[s + 1] <- y
            
            ###
            # get the stationary water levels between the gauging_stations
            # - identify the stations within this section
            id <- which(wldf$station >= df.gs$km_qps[s] & 
                            wldf$station <= df.gs$km_qps[s + 1])
            
            # get the lower stationary water level for a section
            wldf.wl_le <- waterLevelFlys3(wldf[id, ], flys_wls[wls_below])
            
            # get the upper stationary water level for a section
            wldf.wl_gr <- waterLevelFlys3(wldf[id, ], flys_wls[wls_above])
            
            ###
            # record df.data_shiny
            df.data_shiny$section[id] <- s
            df.data_shiny$weight_x[id] <- 1
            df.data_shiny$weight_y[id] <- y
            
            #####
            # interpolate water levels
            df.data$w[id] <- round(wldf.wl_le$w + 
                                       y*(wldf.wl_gr$w - wldf.wl_le$w), 2)
            
        } else {
            
            #####
            # query FLYS3 to get the relevant stationary water levels
            ###
            # upstream
            df.flys_up <- waterLevelFlys3InterpolateX(river, df.gs$km_qps[s])
            
            df.gs$n_wls_below_w_up[s] <- sum(df.flys_up$w <= df.gs$wl[s])
            df.gs$n_wls_above_w_up[s] <- sum(df.flys_up$w > df.gs$wl[s])
            
            ###
            # downstream
            df.flys_do <- waterLevelFlys3InterpolateX(river, df.gs$km_qps[s + 1])
            
            df.gs$n_wls_below_w_do[s + 1] <- sum(df.flys_do$w <= 
                                                     df.gs$wl[s + 1])
            df.gs$n_wls_above_w_do[s + 1] <- sum(df.flys_do$w > df.gs$wl[s + 1])
            
            ###
            # determine the computation relevant water levels
            wls_below <- min(df.gs$n_wls_below_w_up[s],
                             df.gs$n_wls_below_w_do[s + 1])
            wls_above <- 31 - min(df.gs$n_wls_above_w_up[s],
                                  df.gs$n_wls_above_w_do[s + 1])
            
            # special situation, that at least one w is
            # - below the lowest stationary water level of FLYS
            # - above the highest stationary water level of FLYS
            if (wls_below == 0) {
                wls_below <- 1
                if (wls_above == wls_below) {
                    wls_above <- wls_above + 1L
                }
            }
            if (wls_above == 31) {
                wls_above <- 30
                if (wls_below == wls_above) {
                    wls_below <- wls_below - 1L
                }
            }
            
            df.gs$name_wl_below_w_up[s] <-
                as.character(df.flys_up$name[wls_below])
            df.gs$name_wl_above_w_up[s] <-
                as.character(df.flys_up$name[wls_above])
            df.gs$name_wl_below_w_do[s + 1] <-
                as.character(df.flys_do$name[wls_below])
            df.gs$name_wl_above_w_do[s + 1] <-
                as.character(df.flys_do$name[wls_above])
            
            wl_up_le <- df.flys_up$w[wls_below]
            wl_up_gr <- df.flys_up$w[wls_above]
            wl_do_le <- df.flys_do$w[wls_below]
            wl_do_gr <- df.flys_do$w[wls_above]
            
            df.gs$w_wl_below_w_up[s] <- wl_up_le
            df.gs$w_wl_above_w_up[s] <- wl_up_gr
            df.gs$w_wl_below_w_do[s + 1] <- wl_do_le
            df.gs$w_wl_above_w_do[s + 1] <- wl_do_gr
            
            #####
            # calculate the relative positions
            y_up = (df.gs$wl[s] - wl_up_le) / (wl_up_gr - wl_up_le)
            df.gs$weight_up[s] <- y_up
            y_do = (df.gs$wl[s + 1] - wl_do_le) / (wl_do_gr - wl_do_le)
            df.gs$weight_do[s + 1] <- y_do
            
            ###
            # identify the stations within this section
            id <- which(wldf$station >= df.gs$km_qps[s] & 
                        wldf$station <= df.gs$km_qps[s + 1])
            #wldf_sub <- subset(wldf, station >= df.gs$km_qps[s] & 
            #                         station <= df.gs$km_qps[s + 1])
            
            ###
            # get the lower stationary water level for a section
            wldf.wl_le <- waterLevelFlys3(wldf[id, ], flys_wls[wls_below])
            
            # get the upper stationary water level for a section
            wldf.wl_gr <- waterLevelFlys3(wldf[id, ], flys_wls[wls_above])
            
            #####
            # record df.data_shiny
            df.data_shiny$section[id] <- s
            df.data_shiny$weight_x[id] <- stats::approx(
                x = c(df.gs$km_qps[s],
                      df.gs$km_qps[s + 1]),
                y = c(1, 0),
                xout = df.data$station[id])$y
            df.data_shiny$weight_y[id] <- df.data_shiny$weight_x[id] * y_up + 
                (1 - df.data_shiny$weight_x[id]) * y_do
            
            #####
            # interpolate water levels
            df.data$w[id] <- round(wldf.wl_le$w + 
                                       df.data_shiny$weight_y[id] * 
                                       (wldf.wl_gr$w - wldf.wl_le$w), 2)
            
        }
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
    
    if (shiny) {
        wldf_data <- cbind(wldf_data, df.data_shiny)
        wldf <- methods::new("WaterLevelDataFrame",
                         wldf_data,
                         river                    = getRiver(wldf),
                         time                     = getTime(wldf),
                         gauging_stations         = df.gs,
                         gauging_stations_missing = gauging_stations_missing,
                         comment                  = "Computed by waterLevel().")
        return(wldf)
    } else {
        wldf <- methods::new("WaterLevelDataFrame",
                         wldf_data,
                         river                    = getRiver(wldf),
                         time                     = getTime(wldf),
                         gauging_stations         = df.gs,
                         gauging_stations_missing = gauging_stations_missing,
                         comment                  = "Computed by waterLevel().")
        return(wldf)
    }
}


