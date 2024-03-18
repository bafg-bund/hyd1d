param_gauging_station_inland <- function() {
    
    if (requireNamespace("RPostgreSQL") & requireNamespace("DBI")) {
        
        # credentials
        credentials <- credentials("~/DB_credentials_gauging_data")
        
        # access the gauging_data DB
        con <- DBI::dbConnect(drv      = DBI::dbDriver("PostgreSQL"),
                              host     = credentials["host"], 
                              dbname   = credentials["dbname"], 
                              user     = credentials["user"], 
                              password = credentials["password"], 
                              port     = credentials["port"])
        
        # retrieve the data
        query_string <- 
            "SELECT
                gauging_station
            FROM
                gauging_station_data
            WHERE
                (
                    water_shortname = 'ELBE' OR
                    (water_shortname = 'RHEIN' AND km_qps >= 336.2)
                ) AND data_present
            ORDER BY
                water_shortname,
                km_qps ASC
            "
        
        gauging_stations <- DBI::dbGetQuery(con, query_string)$gauging_station
        
        # close DB connection
        DBI::dbDisconnect(con)
        rm(con)
        
    } else {
        gauging_stations <- c('SCHOENA', 'PIRNA', 'DRESDEN', 'MEISSEN',
            'RIESA', 'MUEHLBERG', 'TORGAU', 'PRETZSCH-MAUKEN', 'ELSTER',
            'WITTENBERG', 'COSWIG', 'VOCKERODE', 'ROSSLAU', 'DESSAU', 'AKEN',
            'BARBY', 'SCHOENEBECK', 'MAGDEBURG-BUCKAU',
            'MAGDEBURG-STROMBRUECKE', 'MAGDEBURG-ROTHENSEE',
            'NIEGRIPP AP', 'ROGAETZ', 'TANGERMUENDE', 'STORKAU',
            'SANDAU', 'SCHARLEUK', 'WITTENBERGE', 'MUEGGENDORF',
            'SCHNACKENBURG', 'LENZEN', 'GORLEBEN', 'DOEMITZ', 'DAMNATZ',
            'HITZACKER', 'NEU DARCHAU', 'BLECKEDE', 'BOIZENBURG', 'HOHNSTORF',
            'ARTLENBURG', 'GEESTHACHT', 'IFFEZHEIM', 'PLITTERSDORF', 'MAXAU',
            'PHILIPPSBURG', 'SPEYER', 'MANNHEIM', 'WORMS',
            'NIERSTEIN-OPPENHEIM', 'MAINZ', 'OESTRICH', 'BINGEN', 'KAUB',
            'SANKT GOAR', 'BOPPARD', 'BRAUBACH', 'KOBLENZ', 'ANDERNACH',
            'OBERWINTER', 'BONN', 'KOELN', 'DUESSELDORF', 'RUHRORT',
            'WESEL', 'REES', 'EMMERICH')
    }
    
    return(paste0("@param gauging_station must be type \\code{character} with ",
                  "a length of one. Permitted values are: '",
                  paste0(gauging_stations, collapse = "', '"), "'."))
}

param_uuid_inland <- function() {
    
    if (requireNamespace("RPostgreSQL") & requireNamespace("DBI")) {
        
        # credentials
        credentials <- credentials("~/DB_credentials_gauging_data")
        
        # access the gauging_data DB
        con <- DBI::dbConnect(drv      = DBI::dbDriver("PostgreSQL"),
                              host     = credentials["host"], 
                              dbname   = credentials["dbname"], 
                              user     = credentials["user"], 
                              password = credentials["password"], 
                              port     = credentials["port"])
        
        # retrieve the data
        query_string <- 
            "SELECT
                uuid
            FROM
                gauging_station_data
            WHERE
                (
                    water_shortname = 'ELBE' OR
                    (water_shortname = 'RHEIN' AND km_qps >= 336.2)
                ) AND data_present
            ORDER BY
                water_shortname,
                km_qps ASC
            "
        
        uuids <- DBI::dbGetQuery(con, query_string)$uuid
        
        # close DB connection
        DBI::dbDisconnect(con)
        rm(con)
        
    } else {
        uuids <- c('7cb7461b-3530-4c01-8978-7f676b8f71ed',
                   '85d686f1-55b2-4d36-8dba-3207b50901a7',
                   '70272185-b2b3-4178-96b8-43bea330dcae',
                   '24440872-5bd2-4fb3-8554-907b49816c49',
                   'b04b739d-7ffa-41ee-9eb9-95cb1b4ef508',
                   '16b9b4e7-be14-41fd-941e-6755c97276cc',
                   '83bbaedb-5d81-4bc6-9f66-3bd700c99c1f',
                   'f3dc8f07-c2bb-4b92-b0b0-4e01a395a2c6',
                   'c093b557-4954-4f05-8f5c-6c6d7916c62d',
                   '070b1eb4-3872-4e07-b2e5-e25fd9251b93',
                   '1ce53a59-33b9-40dc-9b17-3cd2a2414607',
                   'ae93f2a5-612e-4514-b5fd-9c8aecdd73c7',
                   'e97116a4-7d30-4671-8ba1-cdce0a153d1d',
                   '1edc5fa4-88af-47f5-95a4-0e77a06fe8b1',
                   '094b96e5-caeb-46d3-a8ee-d44182add069',
                   '939f82ec-15a9-49c8-8828-dc2f8a2d49e2',
                   '90bcb315-f080-41a8-a0ac-6122331bb4cf',
                   'b8567c1e-8610-4c2b-a240-65e8a74919fa',
                   'ccccb57f-a2f9-4183-ae88-5710d3afaefd',
                   'e30f2e83-b80b-4b96-8f39-fa60317afcc7',
                   '3adf88fd-fd7a-41d0-84f5-1143c98a6564',
                   '133f0f6c-2ca1-4798-9360-5b5f417dd839',
                   '13e91b77-90f3-41a5-a320-641748e9c311',
                   'de4cc1db-51cb-4b62-bee2-9750cbe4f5c4',
                   'f4c55f77-ab80-4e00-bed3-aa6631aba074',
                   'e32b0a28-8cd5-4053-bc86-fff9c6469106',
                   'cbf3cd49-91bd-49cc-8926-ccc6c0e7eca4',
                   '48f2661f-f9cb-4093-9d57-da2418ed656e',
                   '550e3885-a9d1-4e55-bd25-34228bd6d988',
                   'c80a4f21-528c-4771-98d7-10cd591699a4',
                   'ac507f42-1593-49ea-865f-10b2523617c7',
                   '6e3ea719-48b1-408a-bc55-0986c1e94cd5',
                   'c233674f-259a-4304-b81f-dce1f415d85b',
                   'a26e57c9-1cb8-4fca-ba80-9e02abc81df8',
                   '67d6e882-b60c-40d3-975c-a6d7a2b4e40a',
                   '6aa1cd8e-e528-4bcb-ba8e-705b6dcb7da2',
                   '33e0bce0-13df-4ffc-be9d-f1a79e795e1c',
                   'd9289367-c8aa-4b6a-b1ad-857fec94c6bb',
                   'b3492c68-8373-4769-9b29-22f66635a478',
                   '44f7e955-c97d-45c8-9ed7-19406806fb4c',
                   'b02be240-1364-4c97-8bb6-675d7d842332',
                   '6b774802-fcb5-49ae-8ecb-ecaf1a278b1c',
                   'b6c6d5c8-e2d5-4469-8dd8-fa972ef7eaea',
                   '88e972e1-88a0-4eb9-847c-0925e5999a46',
                   '2cb8ae5b-c5c9-4fa8-bac0-bb724f2754f4',
                   '57090802-c51a-4d09-8340-b4453cd0e1f5',
                   '844a620f-f3b8-4b6b-8e3c-783ae2aa232a',
                   'd28e7ed1-3317-41c5-bec6-725369ed1171',
                   'a37a9aa3-45e9-4d90-9df6-109f3a28a5af',
                   '665be0fe-5e38-43f6-8b04-02a93bdbeeb4',
                   '0309cd61-90c9-470e-99d4-2ee4fb2c5f84',
                   '1d26e504-7f9e-480a-b52c-5932be6549ab',
                   '550eb7e9-172e-48e4-ae1e-d1b761b42223',
                   '2ff6379d-d168-4022-8da0-16846d45ef9b',
                   'd6dc44d1-63ac-4871-b175-60ac4040069a',
                   '4c7d796a-39f2-4f26-97a9-3aad01713e29',
                   '5735892a-ec65-4b29-97c5-50939aa9584e',
                   'b45359df-c020-4314-adb1-d1921db642da',
                   '593647aa-9fea-43ec-a7d6-6476a76ae868',
                   'a6ee8177-107b-47dd-bcfd-30960ccc6e9c',
                   '8f7e5f92-1153-4f93-acba-ca48670c8ca9',
                   'c0f51e35-d0e8-4318-afaf-c5fcbc29f4c1',
                   'f33c3cc9-dc4b-4b77-baa9-5a5f10704398',
                   '2f025389-fac8-4557-94d3-7d0428878c86',
                   '9598e4cb-0849-401e-bba0-689234b27644')
    }
    
    return(paste0("@param uuid must be type \\code{character} with a length of",
                  " one. Permitted values are: '",
                  paste0(uuids, collapse = "', '"), "'."))
}

#' @name getGaugingDataW
#' @rdname getGaugingDataW
#' @title Get W from internal dataset df.gauging_data for the specified gauging 
#'   station and time
#' 
#' @description Extract the daily mean water level data from 
#'   \code{\link{df.gauging_data}} for specific gauging station and date.
#' 
#' @eval param_gauging_station_inland()
#' @param time must be type \code{\link[base:POSIXct]{c("POSIXct", "POSIXlt")}}
#'   or \code{\link[base:Date]{Date}} and in the temporal range between
#'   1960-01-01 and now (\code{Sys.time()} or \code{Sys.Date()}).
#' @eval param_uuid_inland()
#' 
#' @details This functions queries package-internal gauging data 
#'   (\code{\link{df.gauging_data}}).
#' 
#' @return If gauging data exist for the specified gauging station and time, a 
#'   water level is returned. If no data exist, \code{NA} is returned.
#' 
#' @references 
#'   \insertRef{wsv_pegeldaten_2021}{hyd1d}
#' 
#' @examples
#' getGaugingDataW(gauging_station = "DESSAU", time = as.Date("2016-12-21"))
#' 
#' @export
#' 
getGaugingDataW <- function(gauging_station, time, uuid) {
    
    #####
    # assemble internal variables and check the existence of required data
    ##
    #  get the names of all available gauging_stations
    get("df.gauging_station_data", pos = -1)
    id <- which(df.gauging_station_data$data_present & 
                    df.gauging_station_data$river %in% c("ELBE", "RHINE"))
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
            gs_internal <- gs[id_gs]
            gs_internal_asc <- iconv(gs_internal, from="UTF-8", to="ASCII",
                                     sub="byte")
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
            gs_internal <- gs[id_uu]
            gs_internal_asc <- iconv(gs_internal, from="UTF-8", to="ASCII",
                                     sub="byte")
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
    date <- as.Date(trunc(time, units = "days"))
    date_min <- as.Date("1960-01-01")
    if (any(date < date_min)) {
        stop(paste0("df.gauging_data provides data for the time period between",
                    " 1960-01-01 and ", strftime(Sys.Date() - 1, "%Y-%m-%d"),
                    ". You requested earlier data. Please adjust the submi",
                    "tted 'time' and retry."))
    }
    if (any(date > Sys.Date())) {
        stop(paste0("df.gauging_data provides data for the time period between",
                    " 1960-01-01 and ", strftime(Sys.Date() - 1, "%Y-%m-%d"),
                    ". You requested data in the future. Please adjust the ",
                    "submitted 'time' and retry."))
    }
    if (any(date == Sys.Date())) {
        stop(paste0("df.gauging_data provides data for the time period ",
                    "between 1960-01-01\n  and ", 
                    strftime(Sys.Date() - 1, "%Y-%m-%d"),
                    ". You requested data of today that are accessible \n  ",
                    "through getPegelonlineW() only."))
    }
    
    #####
    # query df.gauging_data or .df.gauging_data
    w <- numeric()
    if (exists(".df.gauging_data", envir = .pkgenv)) {
        for (a_date in date) {
            b_date <- as.Date(a_date, origin = as.Date("1970-01-01"))
            id <- which(.pkgenv$.df.gauging_data$gauging_station == gs_internal
                        & .pkgenv$.df.gauging_data$date == b_date)
            if (length(id) == 1) {
                w <- append(w, round(.pkgenv$.df.gauging_data$w[id], 0))
            }
        }
    } else {
        for (a_date in date) {
            b_date <- as.Date(a_date, origin = as.Date("1970-01-01"))
            id <- which(df.gauging_data$gauging_station == gs_internal_asc & 
                            df.gauging_data$date == b_date)
            if (length(id) == 1) {
                w <- append(w, round(df.gauging_data$w[id], 0))
            }
        }
    }
    
    if (length(w) == 0) {
        return(NA)
    } else {
        return(w)
    }
}
