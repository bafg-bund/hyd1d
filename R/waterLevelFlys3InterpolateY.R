#' @name waterLevelFlys3InterpolateY
#' @rdname waterLevelFlys3InterpolateY
#' 
#' @title Compute a 1d water level dataset based on the FLYS3 algorythms
#' 
#' @description Function to compute 1d water level information based on the
#'   original
#'   \href{https://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html}{FLYS3}
#'   algorythms and store it as column \code{w} of an S4 object of type
#'   \linkS4class{WaterLevelDataFrame}.
#' 
#' @param wldf an object of class \linkS4class{WaterLevelDataFrame}.
#' @eval param_gauging_station()
#' @param w If the \code{wldf} does not supply a valid non-\code{NA} time slot,
#'   it is possible to execute the function with the help of this optional 
#'   parameter. Otherwise \code{\link{getGaugingDataW}} or 
#'   \code{\link{getPegelonlineW}} provide gauging data internally.
#' @eval param_uuid()
#' @param shiny \code{logical} determing whether columns (\code{section},
#'   \code{weight_x}, \code{weight_y}) relevant for the
#'   \code{\link{plotShiny}()}-function are appended to the resulting
#'   \linkS4class{WaterLevelDataFrame}.
#' 
#' @return An object of class \linkS4class{WaterLevelDataFrame}.
#' 
#' @seealso \code{\link{df.flys}}
#' 
#' @references 
#'   \insertRef{busch_einheitliche_2009}{hyd1d}
#'   
#'   \insertRef{hkv_hydrokontor_erstellung_2014}{hyd1d}
#'   
#'   \insertRef{deltares_sobek_2018}{hyd1d}
#' 
#' @examples
#' # waterLevelFlys3InterpolateY
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 263, 0.1))
#' wldf <- waterLevelFlys3InterpolateY(wldf, "ROSSLAU", w = 137)
#' 
#' @export
#' 
waterLevelFlys3InterpolateY <- function(wldf, gauging_station, w, uuid, 
                                         shiny = FALSE) {
    
    #####
    # assemble internal variables and check the existence of required data
    ##
    # vector and function to catch error messages
    errors <- character()
    l <- function(x) {as.character(length(x) + 1)}
    
    ## wldf
    # presence
    if (missing(wldf)) {
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": 'wldf' has to be supplied."))
    }
    # WaterLevelDataFrame
    if (!inherits(wldf, "WaterLevelDataFrame")) {
        errors <- c(errors, paste0("Error ", l(errors), ": 'wldf' ",
                                   "must be type 'WaterLevelDataFrame'."))
    } else {
        
        # wldf variables
        time <- getTime(wldf)
        river   <- getRiver(wldf)
        RIVER   <- toupper(river)
        
        # start
        start_f <- min(wldf$station)
        
        # end
        end_f = max(wldf$station)
        
        # time
        if (is.na(time) & missing(w)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The time slot ",
                                       "of 'wldf' must not be NA or 'w' must",
                                       "be specified."))
        }
        
        
        ##
        # gauging_station &| uuid
        #  get the names of all available gauging_stations
        get("df.gauging_station_data", pos = -1)
        id <- which(df.gauging_station_data$data_present & 
                        df.gauging_station_data$river == RIVER)
        df.gauging_station_data_sel <- df.gauging_station_data[id, ]
        gs <- df.gauging_station_data_sel$gauging_station
        uuids <- df.gauging_station_data_sel$uuid
        
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
                                               "ng_station'  must have length",
                                               " 1."))
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
                    df.gs <- df.gauging_station_data_sel[
                        which(uuids == uuid_internal),]
                    
                    if (df.gs$km_qps < start_f | df.gs$km_qps > end_f) {
                        
                        # identify the gauging stations along or next to the wldf
                        id <- which(df.gauging_station_data_sel$km_qps > 
                                        start_f & 
                                    df.gauging_station_data_sel$km_qps < end_f)
                        id <- c(min(id) - 1, id, max(id) + 1)
                        gs_stretch <- stats::na.omit(
                            df.gauging_station_data_sel$gauging_station[id])
                        
                        # identify the reference gauging stations
                        gs_reference <- referenceGaugingStations(wldf)
                        
                        if (!(df.gs$gauging_station %in% gs_stretch  ) & 
                            !(df.gs$gauging_station %in% gs_reference)) {
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
                                                       paste0(gs_stretch,
                                                              collapse = 
                                                                  "'\n    '"),
                                                       "'\n  Or it has to be a",
                                                       " reference gauging sta",
                                                       "tion.\n  Permitted ref",
                                                       "erence gauging station",
                                                       "s are:\n    '",
                                                       paste0(gs_reference,
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
                    df.gs <- df.gauging_station_data_sel[
                        which(uuids == uuid_internal),]
                    
                    if (df.gs$km_qps < start_f | df.gs$km_qps > end_f) {
                        
                        # identify the uuids along or next to the wldf
                        id <- which(df.gauging_station_data_sel$km_qps > 
                                        start_f & 
                                    df.gauging_station_data_sel$km_qps < end_f)
                        id <- c(min(id) - 1, id, max(id) + 1)
                        uuid_stretch <- stats::na.omit(
                            df.gauging_station_data_sel$uuid[id])
                        
                        # identify the reference gauging stations
                        uuid_reference <- referenceUUID(wldf)
                        
                        if (!(df.gs$uuid %in% uuid_stretch  ) & 
                            !(df.gs$uuid %in% uuid_reference)) {
                            errors <- c(errors, paste0("Error ", l(errors), ":",
                                                       " The selected 'uuid' h",
                                                       "as to be in the river ",
                                                       "stretch\n  covered by ",
                                                       "'wldf' or the next to ",
                                                       "it up- or downstream.",
                                                       "\n  Permitted uuid's f",
                                                       "or the supplied 'wldf'",
                                                       " are:\n    '",
                                                       paste0(uuid_stretch,
                                                              collapse = 
                                                                  "'\n    '"),
                                                       "'\n  Or it has to be a",
                                                       " reference gauging sta",
                                                       "tion.\n  Permitted ref",
                                                       "erence uuids",
                                                       " are:\n    '",
                                                       paste0(uuid_reference,
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
                #
                if (!(is.na(time))) {
                    # get the measured water level
                    df.gs$w <- getGaugingDataW(uuid = uuid_internal, 
                                               time = time)
                    df.gs$wl <- round(df.gs$pnp + df.gs$w / 100, 2)
                    
                    if (!missing(w)) {
                        if (length(w) != 1) {
                            errors <- c(errors, paste0("Error ", l(errors), ":",
                                                       " 'w' must have length ",
                                                       "1."))
                        }
                        if (!inherits(w, "numeric")) {
                            errors <- c(errors, paste0("Error ", l(errors), ":",
                                                       " 'w' must be type 'num",
                                                       "eric'."))
                        }
                        if (w < 0 | w >= 1000) {
                            errors <- c(errors, paste0("Error ", l(errors), ":",
                                                       " 'w' must be in a rang",
                                                       "e between 0 and 1000."))
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
    
    #####
    # Processing
    ##
    # get the flys water levels at the selected gauging station
    df.flys_gs <- waterLevelFlys3InterpolateX(river = river, 
                                              station = df.gs$km_qps)
    
    ##
    # determine the computation relevant water levels
    df.gs$n_wls_below_w_up <- sum(df.flys_gs$w <= df.gs$wl)
    df.gs$n_wls_above_w_up <- sum(df.flys_gs$w > df.gs$wl)
    df.gs$n_wls_below_w_do <- sum(df.flys_gs$w <= df.gs$wl)
    df.gs$n_wls_above_w_do <- sum(df.flys_gs$w > df.gs$wl)
    
    wls_below <- sum(df.flys_gs$w <= df.gs$wl)
    wls_above <- 31 - sum(df.flys_gs$w > df.gs$wl)
    
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
    
    df.gs$name_wl_below_w_up <- df.flys_gs$name[wls_below]
    df.gs$name_wl_above_w_up <- df.flys_gs$name[wls_above]
    df.gs$name_wl_below_w_do <- df.flys_gs$name[wls_below]
    df.gs$name_wl_above_w_do <- df.flys_gs$name[wls_above]
    
    wl_le <- df.flys_gs$w[wls_below]
    wl_gr <- df.flys_gs$w[wls_above]
    df.gs$w_wl_below_w_up <- wl_le
    df.gs$w_wl_above_w_up <- wl_gr
    df.gs$w_wl_below_w_do <- wl_le
    df.gs$w_wl_above_w_do <- wl_gr
    
    # calculate the relative positions
    y <- (df.gs$wl - wl_le) / (wl_gr - wl_le)
    df.gs$weight_up <- y
    df.gs$weight_do <- y
    
    ##
    # get the lower stationary water level for a section
    wldf.wl_le <- waterLevelFlys3(wldf, df.flys_gs$name[wls_below])
    
    # get the upper stationary water level for a section
    wldf.wl_gr <- waterLevelFlys3(wldf, df.flys_gs$name[wls_above])
    
    #####
    # interpolate water levels
    wldf$w <- round(wldf.wl_le$w + y*(wldf.wl_gr$w - wldf.wl_le$w), 2)
    
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
    
    if (shiny) {
        wldf_data$section <- rep(1, nrow(wldf_data))
        wldf_data$weight_x <- rep(1, nrow(wldf_data))
        wldf_data$weight_y <- rep(y, nrow(wldf_data))
        
        wldf <- methods::new("WaterLevelDataFrame",
                             wldf_data,
                             river                    = river,
                             time                     = time,
                             gauging_stations         = df.gs,
                             gauging_stations_missing = character(),
                             comment = paste0("Computed by waterLevelFlys3Inte",
                                              "rpolateY(): gauging_station = ",
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
                             comment = paste0("Computed by waterLevelFlys3Inte",
                                              "rpolateY(): gauging_station = ",
                                              df.gs$gauging_station,
                                              ", w = ", df.gs$w))
        
        return(wldf)
    }
}


inFlys3Section <- function(wldf, gauging_station) {
    
    gauging_stations <- referenceGaugingStations(wldf)
    
    if (gauging_station %in% gauging_stations) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


referenceGaugingStations <- function(wldf) {
    
    # make parent environment accessible through the local environment
    e <- environment()
    p_env <- parent.env(e)
    
    #  get the names of all available gauging_stations
    if (exists("df.flys_sections", where = p_env)) {
        get("df.flys_sections", envir = p_env)
    } else {
        utils::data("df.flys_sections", envir = environment())
    }
    
    # wldf variables
    river   <- getRiver(wldf)
    RIVER   <- toupper(river)
    
    # start
    start_f <- min(wldf$station)
    
    # end
    end_f = max(wldf$station)
    
    # get the relevant gauging stations
    gauging_stations <- df.flys_sections$gauging_station[
        which(df.flys_sections$river == RIVER &
             df.flys_sections$to >= start_f &
             df.flys_sections$from <= end_f)]
    
    return(gauging_stations)
}


referenceUUID <- function(wldf) {
    
    # make parent environment accessible through the local environment
    e <- environment()
    p_env <- parent.env(e)
    
    #  get the names of all available gauging_stations
    if (exists("df.flys_sections", where = p_env)) {
        get("df.flys_sections", envir = p_env)
    } else {
        utils::data("df.flys_sections", envir = environment())
    }
    
    # wldf variables
    river   <- getRiver(wldf)
    RIVER   <- toupper(river)
    
    # start
    start_f <- min(wldf$station)
    
    # end
    end_f = max(wldf$station)
    
    # get the relevant gauging stations
    uuids <- df.flys_sections$uuid[which(df.flys_sections$river == RIVER &
                                         df.flys_sections$to >= start_f &
                                         df.flys_sections$from <= end_f)]
    
    return(uuids)
}
