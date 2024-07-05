#' @name waterLevelFlood1
#' @rdname waterLevelFlood1
#' @aliases waterLevelFlood1
#'
#' @title Compute 1d water level data from the FLYS3 water level MQ and a
#'   gauging station according to the INFORM 3-method Flood1 (Flut1)
#' 
#' @description This function computes a 1d water level according to the
#'   \href{https://www.bafg.de/DE/3_Beraet/4_Exp_oekologie/Flussauenmodell_INFORM_U3/flussauenmodell_inform_node.html}{INFORM}
#'   flood duration method Flood1 (Flut1) and stores it as column \code{w} of an
#'   S4 object of type \linkS4class{WaterLevelDataFrame}. First the function
#'   obtains the reference water level MQ from \code{\link{df.flys}}. This
#'   reference water level is then shifted by the difference between measured
#'   water and the FLYS3 water level for MQ at the specified gauging station.
#'   Here it is provided mainly for historical reasons and more advanced 
#'   functions like \code{\link{waterLevel}} or 
#'   \code{\link{waterLevelPegelonline}} should be used.
#' 
#' @param wldf an object of class \linkS4class{WaterLevelDataFrame}.
#' @eval param_gauging_station_inland()
#' @param w If the \code{wldf} does not supply a valid non-\code{NA} time slot,
#'   it is possible to execute the function with the help of this optional
#'   parameter. Otherwise \code{\link{getGaugingDataW}} or
#'   \code{\link{getPegelonlineW}} provide gauging data internally.
#' @eval param_uuid_inland()
#' @param shiny \code{logical} determing whether columns (\code{section},
#'   \code{weight_x}, \code{weight_y}) relevant for the
#'   \code{\link{plotShiny}()}-function are appended to the resulting
#'   \linkS4class{WaterLevelDataFrame}.
#' 
#' @return An object of class \linkS4class{WaterLevelDataFrame}.
#' 
#' @details This function computes a water level based on the reference water
#'   level MQ from \code{\link{df.flys}}. Since the function only shifts this
#'   single reference water level to make it fit to the measured water level,
#'   no interpolation is needed. Therefore the \code{shiny} columns have
#'   constant values of \code{section <- 1}, \code{weight_x <- 1} and
#'   \code{weight_y <- shift}.
#' 
#' @references 
#'   \insertRef{rosenzweig_inform_2011}{hyd1d}
#'   
#'   \insertRef{bundesanstalt_fur_gewasserkunde_flys_2016}{hyd1d}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf1 <- waterLevelFlood1(wldf, "ROSSLAU")
#' wldf2 <- waterLevelFlood1(wldf, "DESSAU")
#'
#' wldf1$w - wldf2$w
#'
#' @export
#' 
waterLevelFlood1 <- function(wldf, gauging_station, w, uuid, shiny = FALSE) {
    
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
    if (!inherits(wldf, "WaterLevelDataFrame")) {
        errors <- c(errors, paste0("Error ", l(errors), ": 'wldf' ",
                                   "must be type 'WaterLevelDataFrame'."))
    } else {
        
        # wldf variables
        time <- getTime(wldf)
        river   <- getRiver(wldf)
        RIVER   <- toupper(river)
        River <- ifelse(length(unlist(strsplit(river, "_"))) > 1,
                        paste0(toupper(unlist(strsplit(river, "_"))[1]), "_",
                               unlist(strsplit(river, "_"))[2]),
                        toupper(river))
        
        # start
        start_f <- min(wldf$station)
        
        # end
        end_f = max(wldf$station)
        
        # time
        if (is.na(time) & missing(w)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The time slot ",
                                       "of 'wldf' must not be NA or 'w' must ",
                                       "be specified."))
        }
        
        ##
        # gauging_station &| uuid
        #  get the names of all available gauging_stations
        get("df.gauging_station_data", pos = -1)
        df.gsd <- df.gauging_station_data[
            which(df.gauging_station_data$river == River), ]
        gs <- df.gsd$gauging_station
        uuids <- df.gsd$uuid
        
        if (missing(gauging_station) & missing(uuid)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The 'gauging_",
                                       "station' or 'uuid' argument has to ",
                                       "be supplied."))
        } else {
            if (!(missing(gauging_station))) {
                if (!inherits(gauging_station, "character")) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'gaugi",
                                               "ng_station' must be type ",
                                               "'character'."))
                }
                if (length(gauging_station) != 1) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'gaugi",
                                               "ng_station' must have length ",
                                               "1."))
                    stop(paste0(errors, collapse="\n  "))
                }
                if (!(gauging_station %in% gs)) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'gaugi",
                                               "ng_station' must be an element",
                                               " of c('", 
                                               paste0(gs, collapse = "', '"),
                                               "') for the river ", 
                                               getRiver(wldf), "."))
                } else {
                    
                    id_gs <- which(gs == gauging_station)
                    uuid_internal <- uuids[id_gs]
                    df.gs <- df.gsd[which(uuids == uuid_internal),]
                    
                    if (df.gs$km_qps < start_f | df.gs$km_qps > end_f) {
                        id <- which(df.gsd$km_qps > start_f & 
                                    df.gsd$km_qps < end_f)
                        id <- c(min(id) - 1, id, max(id) + 1)
                        gs_possible <- stats::na.omit(
                            df.gsd$gauging_station[id])
                        if (!(df.gs$gauging_station %in% gs_possible)) {
                            errors <- c(errors, paste0("Error ", l(errors), ":",
                                                       " The selected 'gauging",
                                                       "_station' has to be in",
                                                       " the river stretch\n  ",
                                                       "covered by 'wldf' or t",
                                                       "he next to it up- or d",
                                                       "ownstream.\n  Permitte",
                                                       "d gauging stations for",
                                                       " the supplied 'wldf' a",
                                                       "re:\n    '",
                                                       paste0(gs_possible,
                                                             collapse = 
                                                                 "'\n    '"),
                                                       "'"))
                        }
                    }
                }
            }
            
            if (!(missing(uuid))) {
                if (!inherits(uuid, "character")) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'uuid' ",
                                               "must be type 'character'."))
                }
                if (length(uuid) != 1) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'uuid' ",
                                               "must have length 1."))
                    stop(paste0(errors, collapse="\n  "))
                }
                if (!(uuid %in% uuids)) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'uuid' ",
                                               "must be an element of c('", 
                                               paste0(uuids, collapse = "', '"),
                                               "') for the river ", 
                                               getRiver(wldf), "."))
                } else {
                    
                    id_uu <- which(uuids == uuid)
                    uuid_internal <- uuids[id_uu]
                    df.gs <- df.gsd[which(uuids == uuid_internal),]
                    
                    if (df.gs$km_qps < start_f | df.gs$km_qps > end_f) {
                        id <- which(df.gsd$km_qps > start_f & 
                                    df.gsd$km_qps < end_f)
                        id <- c(min(id) - 1, id, max(id) + 1)
                        uuid_possible <- stats::na.omit(df.gsd$uuid[id])
                        if (!(df.gs$uuid %in% uuid_possible)) {
                            errors <- c(errors, paste0("Error ", l(errors), ":",
                                                       " The selected 'uuid' h",
                                                       "as to be in the river ",
                                                       "stretch\n  covered by ",
                                                       "'wldf' or the next to ",
                                                       "it up- or downstream.",
                                                       "\n  Permitted uuid's f",
                                                       "or the supplied 'wldf'",
                                                       " are:\n    '",
                                                       paste0(uuid_possible,
                                                              collapse = 
                                                                  "'\n    '"),
                                                       "'"))
                        }
                    }
                }
            }
            
            if (!(missing(gauging_station)) & !(missing(uuid))) {
                if (id_gs != id_uu) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'gaugin",
                                               "g_station' and 'uuid' must fit",
                                               " to each other.\nThe uuid for ",
                                               "the supplied 'gauging_station'",
                                               " is ", uuids[id_gs], ".\nThe g",
                                               "auging station for the supplie",
                                               "d 'uuid' is ", gs[id_uu], "."))
                }
            }
            
            # get the measured water level
            if (exists("uuid_internal")) {
                df.gs$w <- getGaugingDataW(uuid = uuid_internal, 
                                           time = time)
                df.gs$wl <- round(df.gs$pnp + df.gs$w / 100, 2)
                
                ##
                # w
                if (!(is.na(time))) {
                    if (!missing(w)) {
                        if (length(w) != 1) {
                            errors <- c(errors, paste0("Error ", l(errors), ":",
                                                       " 'w' must have length ",
                                                       "1."))
                            stop(paste0(errors, collapse="\n  "))
                        }
                        if (!inherits(w, "numeric")) {
                            errors <- c(errors, paste0("Error ", l(errors), ":",
                                                       " 'w' must be type 'num",
                                                       "eric'."))
                            stop(paste0(errors, collapse="\n  "))
                        }
                        if (w < 0 | w >= 1000) {
                            errors <- c(errors, paste0("Error ", l(errors), ":",
                                                       " 'w' must be in a rang",
                                                       "e between 0 and 1000."))
                            stop(paste0(errors, collapse="\n  "))
                        }
                        if (w != df.gs$w) {
                            warning("The 'w' computed internally through getGa",
                                    "ugingDataW(gauging_station =\n  'gauging_",
                                    "station', time = getTime('wldf')) and the",
                                    " supplied 'w'\n  differ. Since you specif",
                                    "ically supplied 'w', the internally\n  co",
                                    "mputed 'w' will be overwritten and the wl",
                                    "df's time slot\n  will be reset to NA.")
                            df.gs$w <- w
                            df.gs$wl <- round(df.gs$pnp + df.gs$w/100, 2)
                            time <- as.POSIXct(NA)
                        }
                    }
                }
            }
        }
    }
    
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    }
    
    # add additional result columns to df.gs
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
    
    ##########
    # processing
    ##
    # get the FLYS3 water level for MQ
    wldf_mq <- waterLevelFlys3(wldf, "MQ")
    
    # get the FLYS3 water level MQ for the selected gauging station
    df.flys_temp <- waterLevelFlys3InterpolateX(river = river,
                                                station = df.gs$km_qps)
    wl_mq <- df.flys_temp$w[which(df.flys_temp$name == "MQ")]
    
    # substract the difference between measured water level and FLYS3 MQ water
    # level from wldf_mq
    wldf$w <- wldf_mq$w - (wl_mq - df.gs$wl)
    
    #####
    # assemble and return the final products
    wldf_data <- wldf[ ,c("station", "station_int", "w")]
    row.names(wldf_data) <- row.names(wldf)
    
    # reorder columns
    df.gs <- df.gs[, c('id', 'gauging_station', 'uuid', 'km', 'km_qps',
                       'river', 'longitude', 'latitude', 'mw', 'mw_timespan', 
                       'pnp', 'w', 'wl', 'n_wls_below_w_do', 'n_wls_above_w_do',
                       'n_wls_below_w_up', 'n_wls_above_w_up',
                       'name_wl_below_w_do', 'name_wl_above_w_do',
                       'name_wl_below_w_up', 'name_wl_above_w_up',
                       'w_wl_below_w_do', 'w_wl_above_w_do', 'w_wl_below_w_up',
                       'w_wl_above_w_up', 'weight_up', 'weight_do')]
    
    # convert type of character columns
    c_columns <- c("gauging_station", "uuid", "river", "mw_timespan",
                   "name_wl_below_w_do", "name_wl_above_w_do", 
                   "name_wl_below_w_up", "name_wl_above_w_up")
    for (a_column in c_columns) {
        df.gs[ , a_column] <- as.character(df.gs[ , a_column])
    }
    
    # convert type of integer columns
    c_columns <- c("n_wls_below_w_do", "n_wls_above_w_do", 
                   "n_wls_below_w_up", "n_wls_above_w_up")
    for (a_column in c_columns) {
        df.gs[ , a_column] <- as.integer(df.gs[ , a_column])
    }
    
    # shiny
    if (shiny) {
        wldf_data$section <- rep(1, nrow(wldf_data))
        wldf_data$weight_x <- rep(1, nrow(wldf_data))
        wldf_data$weight_y <- rep(df.gs$wl - wl_mq, nrow(wldf_data))
        
        wldf <- methods::new("WaterLevelDataFrame",
                             wldf_data,
                             river                    = river,
                             time                     = time,
                             gauging_stations         = df.gs,
                             gauging_stations_missing = character(),
                             comment = paste0("Computed by waterLevelFlood1",
                                              "(): gauging_station = ",
                                              df.gs$gauging_station,
                                              ", w = ", df.gs$w))
        
        return(wldf)
    } else {
        wldf <- methods::new("WaterLevelDataFrame",
                             wldf_data,
                             river                    = river,
                             time                     = time,
                             gauging_stations         = df.gs,
                             gauging_stations_missing = character(),
                             comment = paste0("Computed by waterLevelFlood1",
                                              "(): gauging_station = ",
                                              df.gs$gauging_station,
                                              ", w = ", df.gs$w))
        
        return(wldf)
    }
}

