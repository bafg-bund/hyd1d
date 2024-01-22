#' @name getPegelonlineCharacteristicValues
#' @rdname getPegelonlineCharacteristicValues
#' @title Query characteristic values from pegelonline.wsv.de for the specified
#'   gauging station
#' 
#' @description Download characteristic water level data for a gauging station
#'   from \url{https://pegelonline.wsv.de/gast/start}.
#' 
#' @eval param_gauging_station()
#' @param value must be type \code{character}. Commonly available values are
#'   \code{c("PNP", "MThw", "MTnw", "HThw", "NTnw", "HHW", "NNW", "MNW", "MW",
#'   "MHW")}.
#' @eval param_uuid()
#' @param as_list `boolean` to switch between `list` or `data.frame` output
#' @param abs_height `boolean` to switch between absolute and relative height
#'   output
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
                                               abs_height = TRUE) {
    
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
            warning(paste0("Not all of your supplied values are among the comm",
                           "only queried values:\n  '",
                           paste0(values, collapse = "', '"),
                           "'\n  The function will return data for the availab",
                           "le values"))
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
            res <- vector("list")
            res[["shortname"]] <- "PNP"
            res[["longname"]] <- "Pegelnullpunkt"
            res <- append(res, gz)
            res[["validFrom"]] <- format(
                as.Date.character(res[["validFrom"]]), "%Y-%m-%d")
        } else {
            res <- data.frame("shortname" = "PNP",
                              "longname" = "Pegelnullpunkt")
            for (colname in names(gz)) {
                if (grepl("unit", colname)) {
                    res[1, colname] <- as.character(gz[[colname]])
                    next
                }
                if (grepl("value", colname)) {
                    res[1, colname] <- as.numeric(gz[[colname]])
                    next
                }
                if (grepl("validFrom", colname)) {
                    res[1, colname] <- format(
                        as.Date.character(gz[[colname]]), "%Y-%m-%d")
                    next
                }
                res[1, colname] <- as.character(gz[[colname]])
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
        warning(paste0("None of the requested values is available for the que",
                       "ried gauging station: ", gauging_station))
        return(NA)
    }
    if (! all(value %in% shortnames)) {
        if (!warn) {
            warning(paste0("Not all requested values are available for the que",
                           "ried gauging station: ", gauging_station))
        }
        
    }
    
    if (as_list) {
        if (exists("res")) {
            res <- append(list(res), cv)
            names(res) <- shortnames
            res <- res[value]
        } else {
            res <- cv
            names(res) <- shortnames
            res <- res[value]
        }
        
        if (abs_height) {
            for (i in names(res)) {
                if (i == "PNP" | is.na(i)) {next}
                res[[i]]$unit <- "m. a. NHN"
                res[[i]]$value <- res[[i]]$value/100 + gz$value
            }
        }
        
        res[is.na(names(res))] <- NULL
        
    } else {
        if (exists("res")) {
            res[setdiff(names(df), names(res))] <- NA
            df[setdiff(names(res), names(df))] <- NA
            res <- rbind(res, df)
            res <- res[which(res$shortname %in% value), ]
            res[, colSums(is.na(res)) == nrow(res)] <- NULL
            row.names(res) <- 1:nrow(res)
        } else {
            res <- df[df$shortname %in% value, ]
            res[, colSums(is.na(res)) == nrow(res)] <- NULL
            row.names(res) <- 1:nrow(res)
        }
        id_dates <- which("validFrom" == names(res) |
                              startsWith(names(res), "timespan"))
        for (i in id_dates) {
            res[, i] <- as.Date.character(res[, i], format = "%Y-%m-%d")
        }
        res[res == ""] <- NA
        res <- res[, !apply(is.na(res), 2, all)]
        
        if (abs_height) {
            id_remain <- which(res$shortname != "PNP")
            
            res$unit <- rep("m. a. NHN", nrow(res))
            res$value[id_remain] <- res$value[id_remain]/100 + 
                rep(gz$value, length(id_remain))
        }
    }
    
    return(res)
}

