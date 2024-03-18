#' @name waterLevelFlood2
#' @rdname waterLevelFlood2
#' @aliases waterLevelFlood2
#'
#' @title Compute 1d water level data through linear interpolation with 
#'   neighboring gauging stations according to the INFORM 3-method Flood2
#'   (Flut2)
#'
#' @description This function computes a 1d water level according to the
#'   \href{https://www.bafg.de/DE/08_Ref/U3/02_analyse/01_INFORM/inform.html}{INFORM}
#'   flood duration method Flood2 (Flut2) and stores it as column \code{w} of an
#'   S4 object of type \linkS4class{WaterLevelDataFrame}. Flood2 is designed to
#'   enable water level computation between gauging stations along waterways 
#'   without reference water levels, provided for example by
#'   \href{https://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html}{FLYS3}.
#'   The function uses neighboring gauging stations for linear interpolation of
#'   gauging station water levels along the selected river stretch. Here it is
#'   provided mainly for historical reasons and more advanced functions like 
#'   \code{\link{waterLevel}} or \code{\link{waterLevelPegelonline}} should be
#'   used.
#' 
#' @param wldf an object of class \linkS4class{WaterLevelDataFrame}.
#' @param value an optional value of type \code{character}. Commonly available
#'   values are \code{c("MThw", "MTnw", "HThw", "NTnw", "HHW", "NNW", "MNW",
#'   "MW", "MHW")} or a column supplied in \code{df}.
#' @param df an optional object of type \code{data.frame}, which must contain
#'   the columns \code{gauging_station}, \code{river}, \code{longitude},
#'   \code{latitude}, \code{km_csa}, \code{pnp} and finally a water level column
#'   named in \code{value}.
#'
#' @return An object of class \linkS4class{WaterLevelDataFrame}.
#'
#' @details This function computes a water level through simple linear
#'   interpolation of water levels at neighboring gauging stations. Historically
#'   it has been designed for rivers without 1d reference water levels provided
#'   by FLYS3 for \code{\link{df.flys}}. In the meantime it has been extended to
#'   linearly interpolate characteristic water levels queried from 
#'   \href{https://pegelonline.wsv.de/gast/start}{PEGELONLINE} through
#'   \code{\link{getPegelonlineCharacteristicValues}} or supplied through the
#'   external data provided with \code{df}.
#' 
#' @seealso \code{\link{getPegelonlineCharacteristicValues}}
#' 
#' @references 
#'   \insertRef{rosenzweig_inform_2011}{hyd1d}
#' 
#' @examples
#' # using internal gauging data
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf1 <- waterLevelFlood2(wldf)
#' 
#' # using characteristic water levels queried from PEGELONLINE
#' wldf2 <- WaterLevelDataFrame(river   = "Elbe_tidal",
#'                             time    = as.POSIXct(NA),
#'                             station_int = as.integer(seq(500, 170400, 100)))
#' wldf2 <- waterLevelFlood2(wldf2, "MThw")
#' 
#' # supply external water levels through a data.frame
#' df <- data.frame(gauging_station = c("test1", "test2"),
#'                  river = c("Elbe_tidal", "Elbe_tidal"),
#'                  longitude = c(4, 5),
#'                  latitude = c(4, 5),
#'                  km_csa = c(60, 40),
#'                  pnp = c(0, 0),
#'                  MThw = c(400, 450))
#' wldf3 <- WaterLevelDataFrame(river   = "Elbe_tidal",
#'                             time    = as.POSIXct(NA),
#'                             station_int = as.integer(seq(30000, 70000,
#'                                                          1000)))
#' wldf3 <- waterLevelFlood2(wldf3, "MThw", df)
#'
#' @export
#' 
waterLevelFlood2 <- function(wldf, value = NULL, df = NULL) {
    
    #####
    # assemble internal variables and check the existence of required data
    ##
    # vector and function to catch error messages
    errors <- character()
    l <- function(x) {as.character(length(x) + 1)}
    
    ## wldf
    # WaterLevelDataFrame
    if (!inherits(wldf, "WaterLevelDataFrame")) {
        errors <- c(errors, paste0("Error ", l(errors), ": 'wldf' ",
                                   "must be type 'WaterLevelDataFrame'."))
    } else {
        
        # wldf variables
        df.data <- data.frame(id = row.names(wldf), station = wldf$station, 
                              station_int = wldf$station_int, w = wldf$w)
        river <- getRiver(wldf)
        RIVER <- toupper(river)
        River <- ifelse(length(unlist(strsplit(river, "_"))) > 1,
                        paste0(toupper(unlist(strsplit(river, "_"))[1]), "_",
                               unlist(strsplit(river, "_"))[2]),
                        toupper(river))
        
        # start
        start_f <- min(wldf$station)
        
        # end
        end_f = max(wldf$station)
        
        # time
        time    <- as.Date(trunc(getTime(wldf), units = "days"))
        if (is.na(getTime(wldf)) & is.null(value)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The time slot ",
                                       "of 'wldf' must not be NA, if value is",
                                       " NULL."))
        }
    }
    
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    }
    
    # access the gauging_station_data
    get("df.gauging_station_data", pos = -1)
    df.gsd <- df.gauging_station_data[
        which(df.gauging_station_data$river == River), ]
    
    ##
    # value
    if (is.null(df)) {
        if (!is.null(value)) {
            if (!inherits(value, "character")) {
                stop("'value' must be type 'character'.")
            }
            
            charact_values <- c("PNP", "MThw", "MTnw", "HThw", "NTnw", "HHW",
                                "NNW", "MNW", "MW", "MHW")
            
            if (length(value) == 1) {
                stopifnot(value %in% charact_values)
            } else {
                if (! any(value %in% charact_values)) {
                    stop(paste0("None of your supplied values is among the com",
                                "monly queried values:\n  '",
                                paste0(charact_values, collapse = "', '"),
                                "'"))
                } else if (! all(value %in% charact_values)) {
                    warning(paste0("Not all of your supplied values are among ",
                                   "the commonly queried values:\n  '",
                                   paste0(charact_values, collapse = "', '"),
                                   "'\n  The function will return data for the",
                                   " available values"))
                    warn <- TRUE
                }
            }
        }
        
        #####
        # gauging_stations
        # get a data.frame of the relevant gauging stations between start and
        # end
        id_gs <- which(df.gsd$km_qps >= start_f & df.gsd$km_qps <= end_f)
        df.gs_inarea <- df.gsd[id_gs, ]
        
        # get a data.frame of the next gauging station upstream
        id <- which(df.gsd$km_qps < start_f)
        
        # catch exception for the areas upstream of SCHOENA, IFFEZHEIM, ...
        if (length(id) > 0) {
            id_gs <- max(id)
            df.gs_up <- df.gsd[id_gs, ]
        } else {
            if(nrow(df.gs_inarea) > 1) {
                df.gs_up <- df.gs_inarea[1, ]
            } else {
                df.gs_up <- df.gs_inarea
            }
        }
        
        # get a data.frame of the next gauging station downstream
        id <- which(df.gsd$km_qps > end_f)
        
        # catch exception for the areas downstream of GEESTHACHT or EMMERICH
        if (length(id) > 0) {
            id_gs <- min(id)
            df.gs_do <- df.gsd[id_gs,]
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
        gs_missing <- character()
        
        ###
        # add the df.gs_up to this data.frame, if w is available for the
        # df.gs_up on the specified date
        if (df.gs_up$gauging_station == df.gsd$gauging_station[1] & 
            !df.gsd$data_present[1]) {
            df.gs_up$w <- NA_real_
            gs_up_missing <- character()
        } else {
            gs_up_missing <- character()
            if (is.null(value)) {
                w <- getGaugingDataW(df.gs_up$gauging_station, time)
                if (is.na(w)) {
                    gs_up_missing <- df.gs_up$gauging_station
                }
                df.gs_up$w <- w
            } else {
                w <- getPegelonlineCharacteristicValues(
                    df.gs_up$gauging_station, value = value,
                    as_list = TRUE, abs_height = FALSE, verbose = FALSE)
                if (is.na(w)) {
                    gs_up_missing <- df.gs_up$gauging_station
                } else {
                    df.gs_up$w <- w[[value]]$value
                    df.gs_up$timespan <- paste(c(w[[value]]$timespanStart,
                                                 w[[value]]$timespanEnd),
                                               collapse = " - ")
                }
            }
        }
        
        # replace df.gs_up with the next gs further upstream, if w is
        # available for the df.gs_up further upstream on the specified date
        while (length(gs_up_missing) > 0) {
            gs_missing <- append(gs_missing,
                                 paste0('up: ', gs_up_missing))
            id <- which(df.gsd$km_qps < df.gs_up$km_qps & df.gsd$data_present)
            if (length(id) > 0) {
                id_gs <- max(id)
                df.gs_up <- df.gsd[id_gs, ]
            } else {
                break
            }
            gs_up_missing <- character()
            if (is.null(value)) {
                w <- getGaugingDataW(df.gs_up$gauging_station, time)
                if (is.na(w)) {
                    gs_up_missing <- df.gs_up$gauging_station
                }
                df.gs_up$w <- w
            } else {
                w <- getPegelonlineCharacteristicValues(
                    df.gs_up$gauging_station, value = value, as_list = TRUE,
                    abs_height = FALSE, verbose = FALSE)
                if (is.na(w)) {
                    gs_up_missing <- df.gs_up$gauging_station
                } else {
                    df.gs_up$w <- w[[value]]$value
                    df.gs_up$timespan <- paste(c(w[[value]]$timespanStart,
                                                 w[[value]]$timespanEnd),
                                               collapse = " - ")
                }
            }
        }
        if (!"timespan" %in% colnames(df.gs_up)) {
            df.gs_up$timespan <- rep(NA_character_, nrow(df.gs_up))
        }
        
        ###
        # append the df.gs_inarea to this data.frame, if w is available for a_gs
        # on the specified date
        df.gs_inarea$w <- rep(NA_real_, nrow(df.gs_inarea))
        df.gs_inarea$timespan <- rep(NA_character_, nrow(df.gs_inarea))
        i <- 1
        for (a_gs in df.gs_inarea$gauging_station) {
            if (a_gs %in% df.gsd$gauging_station[!df.gsd$data_present]) {
                no_limit <- FALSE
                w <- NA_real_
            } else {
                no_limit <- TRUE
                if (is.null(value)) {
                    w <- getGaugingDataW(a_gs, time)
                    if (is.na(w) & no_limit) {
                        gs_missing <- append(gs_missing, paste0('in: ', a_gs))
                    }
                    df.gs_inarea$w[i] <- w
                } else {
                    w <- getPegelonlineCharacteristicValues(a_gs, value = value,
                        as_list = TRUE, abs_height = FALSE, verbose = FALSE)
                    if (is.na(w) & no_limit) {
                        gs_missing <- append(gs_missing, paste0('in: ', a_gs))
                    } else {
                        df.gs_inarea$w[i] <- w[[value]]$value
                        df.gs_inarea$timespan[i] <- 
                            paste(c(w[[value]]$timespanStart,
                                    w[[value]]$timespanEnd),
                                  collapse = " - ")
                    }
                }
            }
            rm(no_limit)
            i <- i + 1
        }
        
        ###
        # append the df.gs_do to this list, if w is available for the df.gs_do
        # on the specified date
        if (df.gs_do$gauging_station == df.gsd$gauging_station[nrow(df.gsd)] & 
            !df.gsd$data_present[nrow(df.gsd)]) {
            gs_do_missing <- character()
            df.gs_do$w <- NA_real_
        } else {
            gs_do_missing <- character()
            if (is.null(value)) {
                w <- getGaugingDataW(df.gs_do$gauging_station, time)
                if (is.na(w)) {
                    gs_do_missing <- df.gs_do$gauging_station
                }
                df.gs_do$w <- w
            } else {
                w <- getPegelonlineCharacteristicValues(
                    df.gs_do$gauging_station, value = value, as_list = TRUE,
                    abs_height = FALSE, verbose = FALSE)
                if (is.na(w)) {
                    gs_do_missing <- df.gs_do$gauging_station
                } else {
                    df.gs_do$w <- w[[value]]$value
                    df.gs_do$timespan <- paste(c(w[[value]]$timespanStart,
                                                 w[[value]]$timespanEnd),
                                               collapse = " - ")
                }
            }
        }
        
        # replace df.gs_do with the next gs further downstream, if w is
        # available for the df.gs_do further downstream on the specified date
        while (length(gs_do_missing) > 0) {
            gs_missing <- append(gs_missing, paste0('do: ', gs_do_missing))
            id <- which(df.gsd$km_qps > df.gs_do$km_qps & df.gsd$data_present)
            if (length(id) > 0) {
                id_gs <- min(id)
                df.gs_do <- df.gsd[id_gs, ]
            } else {
                break
            }
            gs_do_missing <- character()
            if (is.null(value)) {
                w <- getGaugingDataW(df.gs_do$gauging_station, time)
                if (is.na(w)) {
                    gs_do_missing <- df.gs_do$gauging_station
                }
                df.gs_do$w <- w
            } else {
                w <- getPegelonlineCharacteristicValues(
                    df.gs_do$gauging_station, value = value, as_list = TRUE,
                    abs_height = FALSE, verbose = FALSE)
                if (is.na(w)) {
                    gs_do_missing <- df.gs_do$gauging_station
                } else {
                    df.gs_do$w <- w[[value]]$value
                    df.gs_do$timespan <- paste(c(w[[value]]$timespanStart,
                                                 w[[value]]$timespanEnd),
                                               collapse = " - ")
                }
            }
        }
        if (!"timespan" %in% colnames(df.gs_do)) {
            df.gs_do$timespan <- rep(NA_character_, nrow(df.gs_do))
        }
        
        # bind the df.gs_. datasets and remove gauging stations which should 
        # have data, but don't have them
        df.gs <- rbind(df.gs_up, df.gs_inarea, df.gs_do,
                       stringsAsFactors = FALSE)
        df.gs <- unique(df.gs)
        df.gs <- df.gs[order(df.gs$km_qps),]
        df.gs <- df.gs[!(df.gs$data_present & is.na(df.gs$w)),]
        
        if (nrow(df.gs) == 2) {
            if (is.na(df.gs$w[1])) {
                id_do <- which(df.gsd$km_qps > df.gs$km_qps[2] &
                                   df.gsd$data_present)
                for (id_d in id_do) {
                    gs <- df.gsd$gauging_station[id_d]
                    if (is.null(value)) {
                        w <- getGaugingDataW(gs, time)
                    } else {
                        w <- getPegelonlineCharacteristicValues(
                            df.gs_do$gauging_station, value = value,
                            as_list = TRUE, abs_height = FALSE, verbose = FALSE)
                    }
                    if (! is.na(w)) {
                        break
                    } else {
                        gs_missing <- append(
                            gs_missing, paste0('do: ', gs))
                    }
                }
                df.gs_do <- df.gsd[id_d,]
                df.gs_do$w <- w
                if (!"timespan" %in% colnames(df.gs_do)) {
                    df.gs_do$timespan <- rep(NA_character_, nrow(df.gs_do))
                }
                df.gs <- rbind(df.gs, df.gs_do, stringsAsFactors = FALSE)
            } else if (is.na(df.gs$w[nrow(df.gs)])) {
                id_up <- which(df.gsd$km_qps < df.gs$km_qps[1] &
                                   df.gsd$data_present)
                for (id_u in rev(id_up)) {
                    gs <- df.gsd$gauging_station[id_u]
                    if (is.null(value)) {
                        w <- getGaugingDataW(gs, time)
                    } else {
                        w <- getPegelonlineCharacteristicValues(
                                df.gs_do$gauging_station, value = value,
                                as_list = TRUE, abs_height = FALSE,
                                verbose = FALSE)
                    }
                    if (! is.na(w)) {
                        break
                    } else {
                        gs_missing <- append(
                            gs_missing, paste0('up: ', gs))
                    }
                }
                df.gs_up <- df.gsd[id_u,]
                df.gs_up$w <- w
                if (!"timespan" %in% colnames(df.gs_up)) {
                    df.gs_up$timespan <- rep(NA_character_, nrow(df.gs_up))
                }
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
        
        if (length(unique(stats::na.omit(df.gs$timespan))) > 1 &
            !is.null(value)) {
            warning(paste0("The internally queried characteristic values have ",
                           "different reference periods:\n",
                           paste0("      ",
                                  df.gs$timespan[!is.na(df.gs$timespan)],
                                  ": ", 
                                  df.gs$gauging_station[!is.na(df.gs$timespan)],
                                  collapse = "\n")))
        }
        
    } else {
        if (!inherits(df, "data.frame")) {
            stop("'df' must be type 'data.frame'.")
        }
        if (!inherits(value, "character")) {
            stop("'value' must be type 'character'.")
        }
        
        stopifnot("gauging_station" %in% names(df))
        stopifnot("river" %in% names(df))
        stopifnot(all(unique(tolower(df$river)) %in% 
                          unique(tolower(df.gsd$river))))
        stopifnot("longitude" %in% names(df))
        stopifnot("latitude" %in% names(df))
        stopifnot("km_csa" %in% names(df))
        stopifnot("pnp" %in% names(df))
        stopifnot(value %in% names(df))
        
        # construct df.gs
        df <- df[order(df$km_csa), ]
        df.gs_template <- data.frame(id                 = NA_integer_,
                                     gauging_station    = NA_character_,
                                     uuid               = NA_character_,
                                     km                 = NA_real_,
                                     km_qps             = NA_real_,
                                     river              = NA_real_,
                                     longitude          = NA_real_,
                                     latitude           = NA_real_,
                                     mw                 = NA_real_,
                                     mw_timespan        = NA_character_,
                                     pnp                = NA_real_,
                                     w                  = NA_real_,
                                     wl                 = NA_real_,
                                     n_wls_below_w_do   = NA_integer_,
                                     n_wls_above_w_do   = NA_integer_,
                                     n_wls_below_w_up   = NA_integer_,
                                     n_wls_above_w_up   = NA_integer_,
                                     name_wl_below_w_do = NA_character_,
                                     name_wl_above_w_do = NA_character_,
                                     name_wl_below_w_up = NA_character_,
                                     name_wl_above_w_up = NA_character_,
                                     w_wl_below_w_do    = NA_real_,
                                     w_wl_above_w_do    = NA_real_,
                                     w_wl_below_w_up    = NA_real_,
                                     w_wl_above_w_up    = NA_real_,
                                     weight_up          = NA_real_,
                                     weight_do          = NA_real_)
        if (min(wldf$station) < min(df$km_csa)) {
            df.gs_up <- df.gs_template
            df.gs_up$gauging_station <- "UPSTREAM"
            df.gs_up$km_qps <- min(wldf$station)
        } else {
            df.gs_up <- df.gs_template[-1,]
        }
        df.gs <- data.frame(id                 = integer(nrow(df)),
                            gauging_station    = df$gauging_station,
                            uuid               = character(nrow(df)),
                            km                 = numeric(nrow(df)),
                            km_qps             = df$km_csa,
                            river              = df$river,
                            longitude          = df$longitude,
                            latitude           = df$latitude,
                            mw                 = numeric(nrow(df)),
                            mw_timespan        = character(nrow(df)),
                            pnp                = df$pnp,
                            w                  = df[, value],
                            wl                 = df$pnp + df[, value] / 100,
                            n_wls_below_w_do   = integer(nrow(df)),
                            n_wls_above_w_do   = integer(nrow(df)),
                            n_wls_below_w_up   = integer(nrow(df)),
                            n_wls_above_w_up   = integer(nrow(df)),
                            name_wl_below_w_do = character(nrow(df)),
                            name_wl_above_w_do = character(nrow(df)),
                            name_wl_below_w_up = character(nrow(df)),
                            name_wl_above_w_up = character(nrow(df)),
                            w_wl_below_w_do    = numeric(nrow(df)),
                            w_wl_above_w_do    = numeric(nrow(df)),
                            w_wl_below_w_up    = numeric(nrow(df)),
                            w_wl_above_w_up    = numeric(nrow(df)),
                            weight_up          = numeric(nrow(df)),
                            weight_do          = numeric(nrow(df)))
        if (max(wldf$station) > max(df$km_csa)) {
            df.gs_do <- df.gs_template
            df.gs_do$gauging_station <- "DOWNSTREAM"
            df.gs_do$km_qps <- max(wldf$station)
        } else {
            df.gs_do <- df.gs_template[-1,]
        }
        df.gs <- rbind(df.gs_up, df.gs, df.gs_do, stringsAsFactors = FALSE)
        df.gs$id <- 1:nrow(df.gs)
        
        # gs_missing
        gs_missing <- "None"
    }
    
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
        # - ...
        # downstream of:
        #   - GEESTHACHT
        #   - EMMERICH
        #   - ...
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
        df <- unique(data.frame(x = c(df.gs$km_qps[s], df.gs$km_qps[s + 1]),
                                y = round(c(df.gs$wl[s], df.gs$wl[s + 1]), 3)))
        
        if (nrow(df) == 1) {
            df.data$w[id] <- round(df$y, 2)
        } else {
            df.data$w[id] <- round(
                stats::approx(x = df$x, y = df$y,
                              xout = df.data$station[id])$y, 2)
        }
    }
    
    #####
    # assemble and return the final products
    wldf_data <- df.data[ ,c("station", "station_int", "w")]
    row.names(wldf_data) <- df.data$id
    
    columns <- c('id', 'gauging_station', 'uuid', 'km', 'km_qps', 'river',
                 'longitude', 'latitude', 'mw', 'mw_timespan', 
                 'pnp', 'w', 'wl', 'n_wls_below_w_do', 'n_wls_above_w_do',
                 'n_wls_below_w_up', 'n_wls_above_w_up',
                 'name_wl_below_w_do', 'name_wl_above_w_do',
                 'name_wl_below_w_up', 'name_wl_above_w_up',
                 'w_wl_below_w_do', 'w_wl_above_w_do', 'w_wl_below_w_up',
                 'w_wl_above_w_up', 'weight_up', 'weight_do')
    if ("timespan" %in% colnames(df.gs)) {
        df.gs$mw_timespan <- df.gs$timespan
    }
    df.gs <- df.gs[, columns]
    c_columns <- c("gauging_station", "uuid", "river", "mw_timespan",
                   "name_wl_below_w_do", "name_wl_above_w_do", 
                   "name_wl_below_w_up", "name_wl_above_w_up")
    for (a_column in c_columns) {
        df.gs[ , a_column] <- as.character(df.gs[ , a_column])
    }
    n_columns <- c("km", "km_qps", "w", "longitude", "latitude", "mw", "pnp",
                   "wl", "w_wl_below_w_do", "w_wl_above_w_do",
                   "w_wl_below_w_up", "w_wl_above_w_up", "weight_up",
                   "weight_do")
    for (a_column in n_columns) {
        df.gs[ , a_column] <- as.numeric(df.gs[ , a_column])
    }
    
    wldf <- methods::new("WaterLevelDataFrame",
                         wldf_data,
                         river                    = river,
                         time                     = getTime(wldf),
                         gauging_stations         = df.gs,
                         gauging_stations_missing = gs_missing,
                         comment = paste0("Computed by waterLevelFlood2()"))
    return(wldf)
}

