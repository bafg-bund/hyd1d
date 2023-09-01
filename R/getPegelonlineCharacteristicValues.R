#' @name getPegelonlineCharacteristicValues
#' @rdname getPegelonlineCharacteristicValues
#' @title Query characteristic values from pegelonline.wsv.de for the specified
#'   gauging station
#' 
#' @description Download characteristic water level data for a gauging station
#'   from \url{https://pegelonline.wsv.de/gast/start}.
#' 
#' @eval param_gauging_station()
#' @param value must be type \code{character}.
#' @eval param_uuid()
#' @param as_list `boolean` to switch between `list` or `data.frame` output
#' 
#' @details This functions queries online data through the
#'   \href{https://en.wikipedia.org/wiki/Representational_state_transfer}{REST}
#'   service of \href{https://pegelonline.wsv.de/gast/start}{PEGELONLINE}.
#' 
#' @note Internally \code{\link[utils:download.file]{download.file}} is used to
#'   obtain the gauging data from \url{https://pegelonline.wsv.de/gast/start}.
#'   The download method can be set through the option "\code{download.file.method}":
#'   see \code{\link[base:options]{options()}}.
#' 
#' @return The returned output is a named \code{list} or \code{data.frame} with
#'   the queried characteristic value(s) and potentially other relevant
#'   information such as time spans.
#' 
#' @seealso \code{\link[utils:download.file]{download.file}}
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
                                               as_list = TRUE) {
    
    warn <- FALSE
    
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
    
    # temporarilly append estuary data
    gs <- c(gs, "WEHR GEESTHACHT UP", "ALTENGAMME", "ZOLLENSPIEKER",
            "OVER", "BUNTHAUS", "HAMBURG ST. PAULI", "SEEMANNSHOEFT",
            "BLANKENESE UF", "SCHULAU", "LUEHORT", "STADERSAND",
            "GRAUERORT", "KOLLMAR", "KRAUTSAND", "GLUECKSTADT", "BROKDORF",
            "BRUNSBUETTEL MPM", "OSTERIFF MPM", "OTTERNDORF MPM",
            "CUXHAVEN STEUBENHOEFT", "MITTELGRUND", "SCHARHOERN", "BAKE Z")
    uuids <- c(uuids, "0f7f58a8-411f-43d9-b42a-e897e63c4faa",
               "2ee12b9a-f7fd-4856-82b9-6bdd850c2bba",
               "3de8ea26-ab29-4e46-adad-06198ba2e0b7",
               "b02ce5c0-64e9-4d24-90b9-269a28a1e9f9",
               "ae1b91d0-e746-4f65-9f64-2d2e23603a82",
               "d488c5cc-4de9-4631-8ce1-0db0e700b546",
               "816affba-0118-4668-887f-fb882ed573b2",
               "bacb459b-0f24-4233-bb35-cd224a51678e",
               "f3c6ee73-5561-4068-96ec-364016e7d9ef",
               "8d18d129-07f1-4c4d-adba-a985016be0b0",
               "80f0fc4d-9fc7-449d-9d68-ee89333f0eff",
               "ccf0645d-ddad-4c9e-b4f1-dc1f1edb2aa4",
               "3ed90357-4b01-4119-b1c5-bd2c62871e7b",
               "e651fe4a-d759-49c5-8e00-55137d0f2975",
               "1f1bbed7-c1fa-45b4-90d3-df94b50ad631",
               "610ab204-d3c4-4a11-a38b-e31461fdcf27",
               "d4f5f719-8c52-4f8d-945d-1c31404cc628",
               "eb90bd3f-5405-412d-81e0-7a58be52dcef",
               "5140295e-b93e-4081-a920-642d89c7ca8b",
               "aad49293-242a-43ad-a8b1-e91d7792c4b2",
               "3ff99b92-4396-4fa7-af73-02b9c015dcad",
               "f0197bcf-6846-4c0a-9659-0c2626a9bcf0",
               "104fdc24-1dc6-4cb7-b44f-10bd02e13f40")
    
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
    # query the data from pegelonline.wsv.de
    url <- paste0("http://www.pegelonline.wsv.de/webservices/rest-api/v2/stati",
                  "ons/", uuid_internal,
                  "/W.json?includeCharacteristicValues=true")
    string <- tryCatch({
        tf <- tempfile()
        utils::download.file(url, tf, method = method, quiet = TRUE,
                             extra = getOption("download.file.extra"))
        tf
    }, 
    error = function(e){
        msg <- paste0("It was not possible to access data from\n",
                      "https://pegelonline.wsv.de\n",
                      "Please try again later, if the server was not available.\n",
                      "Please read the notes if you recieve an SSL error.\n",
                      e)
        message(msg)
        return(NA)
    })
    
    # process queried data
    list <- RJSONIO::fromJSON(string)
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
                    res[1, colname] <- as.Date(gz[[colname]])
                    next
                }
                res[1, colname] <- as.character(gz[[colname]])
            }
        }
    }
    
    # convert cv to either a list or a data.frame
    cv <- list$characteristicValues
    if (as_list) {
        for (i in 1:length(cv)) {
            shortnames <- c(shortnames, cv[[i]]$shortname)
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
                        df[i, colname] <- as.Date(cv[[i]][[colname]])
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
    
    # assemble returned list
    if (! any(value %in% shortnames)) {
        message(paste0("None of requested values is available for the queried ",
                       "gauging station."))
        return(NA)
    }
    if (! all(value %in% shortnames)) {
        if (!warn) {
            message(paste0("Not all requested values are available for the que",
                           "ried gauging station."))
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
    } else {
        if (exists("res")) {
            res[setdiff(names(df), names(res))] <- NA
            df[setdiff(names(res), names(df))] <- NA
            res <- rbind(res, df)
            res <- res[which(res$shortname %in% value), ]
            res[, colSums(is.na(res)) == nrow(res)] <- NULL
            row.names(res) <- 1:nrow(res)
        } else {
            res <- df[df$shortname == value, ]
            res[, colSums(is.na(res)) == nrow(res)] <- NULL
            row.names(res) <- 1:nrow(res)
        }
    }
    
    return(res)
}
