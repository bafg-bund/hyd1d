#' @name getPegelonlineCharacteristicValues
#' @rdname getPegelonlineCharacteristicValues
#' @title Query characteristic values from pegelonline.wsv.de for the specified
#'   gauging station
#' 
#' @description Download characteristic water level data for a gauging station
#'   from \url{https://pegelonline.wsv.de/gast/start}.
#' 
#' @eval param_gauging_station_all()
#' @param value must be type \code{character}. Commonly available values are
#'   \code{c("PNP", "MThw", "MTnw", "HThw", "NTnw", "HHW", "NNW", "MNW", "MW",
#'   "MHW")}.
#' @eval param_uuid_all()
#' @param as_list `boolean` to switch between `list` or `data.frame` output
#' @param abs_height `boolean` to switch between absolute and relative height
#'   output
#' @param verbose `boolean` to switch off warnings
#' 
#' @details This functions queries online data through the
#'   \href{https://en.wikipedia.org/wiki/Representational_state_transfer}{REST}
#'   service of \href{https://pegelonline.wsv.de/gast/start}{PEGELONLINE}.
#' 
#' @return The returned output is a named \code{list} or \code{data.frame} with
#'   the queried characteristic value(s) and potentially other relevant
#'   information such as time spans.
#' 
#' @references \insertRef{wsv_pegelonline_2018}{hyd1d}
#' 
#' @examples
#' getPegelonlineCharacteristicValues(gauging_station = "DESSAU", value = "MW",
#'                                    as_list = FALSE)
#' getPegelonlineCharacteristicValues(gauging_station = "HAMBURG ST. PAULI",
#'                                    value = "MThw")
#' 
#' @export
#' 
getPegelonlineCharacteristicValues <- function(gauging_station, value, uuid,
                                               as_list = TRUE,
                                               abs_height = TRUE,
                                               verbose = TRUE) {
    
    warn <- FALSE
    
    #####
    # assemble internal variables and check the existence of required data
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
    # value
    if (missing(value)) {
        stop("The 'value' argument has to be supplied.")
    }
    if (!inherits(value, "character")) {
        stop("'value' must be type 'character'.")
    }
    
    values <- c("PNP", "MThw", "MTnw", "HThw", "NTnw", "HHW", "NNW", "MNW",
                "MW", "MHW")
    if (length(value) == 1) {
        stopifnot(value %in% c("PNP", "MThw", "MTnw", "HThw", "NTnw", "HHW",
                               "NNW", "MNW", "MW", "MHW"))
    } else {
        if (! any(value %in% values)) {
            stop(paste0("None of your supplied values is among the commonly qu",
                        "eried values:\n  '", paste0(values, collapse = "', '"),
                        "'"))
        } else if (! all(value %in% values)) {
            if (verbose) {
                warning(paste0("Not all of your supplied values are among the ",
                               "commonly queried values:\n  '",
                               paste0(values, collapse = "', '"),
                               "'\n  The function will return data for the ava",
                               "ilable values"))
            }
            warn <- TRUE
        }
    }
    
    ##
    # as_list
    stopifnot(inherits(as_list, "logical"))
    stopifnot(length(as_list) == 1)
    
    ##
    # as_list
    stopifnot(inherits(abs_height, "logical"))
    stopifnot(length(abs_height) == 1)
    
    ##
    # verbose
    stopifnot(inherits(verbose, "logical"))
    stopifnot(length(verbose) == 1)
    
    ## 
    # query the data from pegelonline.wsv.de
    get_cv <- request(paste0("http://www.pegelonline.wsv.de/webservices/rest-a",
                             "pi/v2/stations/", uuid_internal,
                             "/W.json?includeCharacteristicValues=true"))
    get_cv <- req_perform(get_cv)
    list <- resp_body_json(get_cv)
    
    # process queried data
    shortnames <- character()
    
    # PNP
    gz <- list$gaugeZero
    if ("PNP" %in% value) {
        shortnames <- c(shortnames, "PNP")
        if (as_list) {
            l <- vector("list")
            l[["shortname"]] <- "PNP"
            l[["longname"]] <- "Pegelnullpunkt"
            l <- append(l, gz)
            l[["validFrom"]] <- format(
                as.Date.character(l[["validFrom"]]), "%Y-%m-%d")
        } else {
            df_pnp <- data.frame("shortname" = "PNP",
                                 "longname" = "Pegelnullpunkt")
            for (colname in names(gz)) {
                if (grepl("unit", colname)) {
                    df_pnp[1, colname] <- as.character(gz[[colname]])
                    next
                }
                if (grepl("value", colname)) {
                    df_pnp[1, colname] <- as.numeric(gz[[colname]])
                    next
                }
                if (grepl("validFrom", colname)) {
                    df_pnp[1, colname] <- format(
                        as.Date.character(gz[[colname]]), "%Y-%m-%d")
                    next
                }
                df_pnp[1, colname] <- as.character(gz[[colname]])
            }
        }
    }
    
    # convert cv to either a list or a data.frame
    cv <- list$characteristicValues
    if (length(cv) > 0) {
        if (as_list) {
            for (i in 1:length(cv)) {
                shortnames <- c(shortnames, cv[[i]]$shortname)
                id_date <- which(grepl("timespan", names(cv[[i]])) | 
                                     grepl("occurences", names(cv[[i]])))
                for (j in id_date) {
                    cv[[i]][j] <- format(
                        as.Date.character(cv[[i]][j], format = "%Y-%m-%d"),
                        "%Y-%m-%d")
                }
            }
        } else {
            for (i in 1:length(cv)) {
                shortnames <- c(shortnames, cv[[i]]$shortname)
                if (i == 1) {
                    df <- data.frame()
                    for (colname in names(cv[[i]])) {
                        if (grepl("name", colname)) {
                            df[i, colname] <- as.character(cv[[i]][[colname]])
                            next
                        }
                        if (grepl("unit", colname)) {
                            df[i, colname] <- as.character(cv[[i]][[colname]])
                            next
                        }
                        if (grepl("value", colname)) {
                            df[i, colname] <- as.numeric(cv[[i]][[colname]])
                            next
                        }
                        if (grepl("timespan", colname)) {
                            df[i, colname] <- cv[[i]][[colname]]
                            next
                        }
                        df[i, colname] <- as.character(cv[[i]][[colname]])
                    }
                } else {
                    for (colname in names(cv[[i]])) {
                        if (colname %in% names(df)) {
                            df[i, colname] <- cv[[i]][[colname]]
                        } else {
                            df[, colname] <- character(i)
                            df[i, colname] <- cv[[i]][[colname]]
                        }
                    }
                }
            }
        }
    }
    
    # assemble returned list
    if (! any(value %in% shortnames)) {
        if (verbose) {
            warning(paste0("None of the requested values is available for the ",
                           "queried gauging station: ", gauging_station))
        }
        return(NA)
    }
    if (! all(value %in% shortnames)) {
        if (!warn & verbose) {
            warning(paste0("Not all requested values are available for the que",
                           "ried gauging station: ", gauging_station))
        }
    }
    
    if (as_list) {
        if (exists("l")) {
            l <- append(list(l), cv)
            names(l) <- shortnames
            l <- l[value]
        } else {
            l <- cv
            names(l) <- shortnames
            l <- l[value]
        }
        
        if (abs_height) {
            for (i in names(l)) {
                if (i == "PNP" | is.na(i)) {next}
                l[[i]]$unit <- "m. a. NHN"
                l[[i]]$value <- l[[i]]$value/100 + gz$value
            }
        }
        
        l[is.na(names(l))] <- NULL
        
        return(l)
        
    } else {
        if (exists("df_pnp")) {
            df_pnp[setdiff(names(df), names(df_pnp))] <- NA
            df[setdiff(names(df_pnp), names(df))] <- NA
            df <- rbind(df_pnp, df)
            df <- df[which(df$shortname %in% value), ]
            df[, colSums(is.na(df)) == nrow(df)] <- NULL
            row.names(df) <- 1:nrow(df)
        } else {
            df <- df[df$shortname %in% value, ]
            df[, colSums(is.na(df)) == nrow(df)] <- NULL
            row.names(df) <- 1:nrow(df)
        }
        id_dates <- which("validFrom" == names(df) |
                              startsWith(names(df), "timespan"))
        for (i in id_dates) {
            df[, i] <- as.Date.character(df[, i], format = "%Y-%m-%d")
        }
        df[df == ""] <- NA
        df <- df[, !apply(is.na(df), 2, all)]
        
        if (abs_height) {
            id_remain <- which(df$shortname != "PNP")
            
            df$unit <- rep("m. a. NHN", nrow(df))
            df$value[id_remain] <- df$value[id_remain]/100 + 
                rep(gz$value, length(id_remain))
        }
        
        return(df)
        
    }
}

