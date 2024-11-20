param_gauging_station_all <- function() {
    
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
                    (water_shortname = 'RHEIN' AND km_qps >= 336.2) OR
                    water_shortname = 'ELBE_tidal' OR
                    water_shortname = 'STOER_tidal' OR
                    water_shortname = 'EMS_tidal'
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
        gauging_stations <- c('SCHOENA', 'PIRNA', 'DRESDEN', 'MEISSEN', 'RIESA',
            'MUEHLBERG', 'TORGAU', 'PRETZSCH-MAUKEN', 'ELSTER', 'WITTENBERG',
            'COSWIG', 'VOCKERODE', 'ROSSLAU', 'DESSAU', 'AKEN', 'BARBY',
            'SCHOENEBECK', 'MAGDEBURG-BUCKAU', 'MAGDEBURG-STROMBRUECKE',
            'MAGDEBURG-ROTHENSEE', 'NIEGRIPP AP', 'ROGAETZ', 'TANGERMUENDE',
            'STORKAU', 'SANDAU', 'SCHARLEUK', 'WITTENBERGE', 'MUEGGENDORF',
            'SCHNACKENBURG', 'LENZEN', 'GORLEBEN', 'DOEMITZ', 'DAMNATZ',
            'HITZACKER', 'NEU DARCHAU', 'BLECKEDE', 'BOIZENBURG', 'HOHNSTORF',
            'ARTLENBURG', 'GEESTHACHT', 'WEHR GEESTHACHT UP', 'ALTENGAMME',
            'ZOLLENSPIEKER', 'OVER', 'BUNTHAUS', 'HAMBURG ST. PAULI',
            'SEEMANNSHOEFT', 'BLANKENESE UF', 'SCHULAU', 'LUEHORT',
            'STADERSAND', 'KOLLMAR', 'GLUECKSTADT', 'BROKDORF',
            'BRUNSBUETTEL MPM', 'OSTERIFF MPM', 'OTTERNDORF MPM',
            'CUXHAVEN STEUBENHOEFT', 'MITTELGRUND', 'SCHARHOERN', 'BAKE Z',
            'HERBRUM HAFENDAMM', 'RHEDE', 'PAPENBURG', 'WEENER', 'LEERORT',
            'WANGEROOGE OST', 'TERBORG', 'WANGEROOGE NORD', 'POGUM',
            'EMDEN NEUE SEESCHLEUSE', 'KNOCK', 'DUKEGAT', 'EMSHOERN',
            'BORKUM SUEDSTRAND', 'BORKUM FISCHERBALJE', 'FUESTRUP',
            'NORDERNEY RIFFGAT', 'LANGEOOG', 'SPIEKEROOG',
            'RHEINE UNTERSCHLEUSE', 'LINGEN-DARME', 'DALUM', 'IFFEZHEIM',
            'PLITTERSDORF', 'MAXAU', 'PHILIPPSBURG', 'SPEYER', 'MANNHEIM',
            'WORMS', 'NIERSTEIN-OPPENHEIM', 'MAINZ', 'OESTRICH', 'BINGEN',
            'KAUB', 'SANKT GOAR', 'BOPPARD', 'BRAUBACH', 'KOBLENZ', 'ANDERNACH',
            'OBERWINTER', 'BONN', 'KOELN', 'DUESSELDORF', 'RUHRORT', 'WESEL',
            'REES', 'EMMERICH', 'GROENHUDE', 'BREITENBERG', 'ITZEHOE HAFEN',
            'STOER-SPERRWERK BP')
    }
    
    return(paste0("@param gauging_station must be type \\code{character} with ",
                  "a length of one. Permitted values are: '",
                  paste0(gauging_stations, collapse = "', '"), "'."))
}

param_uuid_all <- function() {
    
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
                    (water_shortname = 'RHEIN' AND km_qps >= 336.2) OR
                    water_shortname = 'ELBE_tidal' OR
                    water_shortname = 'STOER_tidal' OR
                    water_shortname = 'EMS_tidal'
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
                   '0f7f58a8-411f-43d9-b42a-e897e63c4faa',
                   '2ee12b9a-f7fd-4856-82b9-6bdd850c2bba',
                   '3de8ea26-ab29-4e46-adad-06198ba2e0b7',
                   'b02ce5c0-64e9-4d24-90b9-269a28a1e9f9',
                   'ae1b91d0-e746-4f65-9f64-2d2e23603a82',
                   'd488c5cc-4de9-4631-8ce1-0db0e700b546',
                   '816affba-0118-4668-887f-fb882ed573b2',
                   'bacb459b-0f24-4233-bb35-cd224a51678e',
                   'f3c6ee73-5561-4068-96ec-364016e7d9ef',
                   '8d18d129-07f1-4c4d-adba-a985016be0b0',
                   '80f0fc4d-9fc7-449d-9d68-ee89333f0eff',
                   '3ed90357-4b01-4119-b1c5-bd2c62871e7b',
                   '1f1bbed7-c1fa-45b4-90d3-df94b50ad631',
                   '610ab204-d3c4-4a11-a38b-e31461fdcf27',
                   'd4f5f719-8c52-4f8d-945d-1c31404cc628',
                   'eb90bd3f-5405-412d-81e0-7a58be52dcef',
                   '5140295e-b93e-4081-a920-642d89c7ca8b',
                   'aad49293-242a-43ad-a8b1-e91d7792c4b2',
                   '3ff99b92-4396-4fa7-af73-02b9c015dcad',
                   'f0197bcf-6846-4c0a-9659-0c2626a9bcf0',
                   '104fdc24-1dc6-4cb7-b44f-10bd02e13f40',
                   '8177a148-5674-4b8f-8ded-050907f640f3',
                   '16508b11-4349-48f7-be51-1227b7888585',
                   'ec4a598d-773d-44c1-935e-2053b54e45a3',
                   'aa6af4e6-a44f-46c4-abf6-449f8a68bab1',
                   'abb23dad-0880-41ab-8d2d-dd33e11f148f',
                   '26656fda-cacf-4e92-9935-3ae6e717fe5b',
                   '244cae8b-ce75-4c2d-a66e-cb804f8335a2',
                   'c41d42b1-5b0d-47c9-ba53-d58d3d109b64',
                   '5d1e4350-0f39-4428-84c3-6f8f0bbe80d4',
                   'edfdf747-be92-462f-87ed-53d228a33172',
                   '438b565e-f293-43c8-8771-377e555ed5ec',
                   '7753c1fa-34d8-4d09-a7c7-38024079117c',
                   'c8af067c-ba6a-4a76-86d8-1ce8e532ef8b',
                   '478f21e9-906b-4c6f-a009-b5eabb052746',
                   '8727ebfd-e2e1-43da-ab3d-fee48cff9acc',
                   '3a8ed45f-28e7-4263-8437-d926c6a194f4',
                   'c0244c0e-6ae6-40cb-a967-4039b2a0ce7c',
                   'a0c1dcb6-7812-48e6-8c01-f7edad7a2caf',
                   '662c4b5e-0241-456d-ac7d-9f62fd95c0d1',
                   '50a449ba-af4c-42c7-b2c4-9a3eda37e1e3',
                   '200363fc-cdc5-4c22-a271-a25d1ba880ed',
                   'ad357e52-0978-4583-91e6-bc03a222f655',
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
                   '9598e4cb-0849-401e-bba0-689234b27644',
                   '15859426-834c-429e-9c41-2e097b717b1d',
                   '24c6a014-864b-4d53-bd05-0b49106f5412',
                   'd863cbc3-5e5e-4095-855c-026f0850dd58',
                   'e5b8e9f3-f0cc-4ad7-8707-577ee1b25b3e')
    }
    
    return(paste0("@param uuid must be type \\code{character} with a length of",
                  " one. Permitted values are: '",
                  paste0(uuids, collapse = "', '"), "'."))
}


#' @name getPegelonlineW
#' @rdname getPegelonlineW
#' @title Get W from pegelonline.wsv.de for the specified gauging station and
#'   time
#' 
#' @description Download and temporarily interpolate or average water level data
#'   from \url{https://pegelonline.wsv.de/gast/start}.
#' 
#' @eval param_gauging_station_all()
#' @param time must be type \code{c("POSIXct", "POSIXt")} or \code{Date} and
#'   be in the  temporal range between 31 days ago
#'   \code{\link[base:Sys.time]{Sys.time()} - 2678400} or
#'   \code{\link[base:Sys.time]{Sys.Date()} - 31} and now
#'   (\code{\link[base:Sys.time]{Sys.time()}}) or yesterday
#'   (\code{\link[base:Sys.time]{Sys.Date()} - 1}).
#' @eval param_uuid_all()
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
        req <- httr2::req_error(req, is_error = \(resp) FALSE)
        req <- httr2::req_method(req, "GET")
        req <- httr2::req_retry(req, max_tries = 10L)
        
        # perform the request
        resp <- httr2::req_perform(req)
        status_code <- as.character(resp$status_code)
        if (status_code != "200") {
            message(paste0("The webserver of the PEGELONLINE rest-api returned",
                           " a status code of '", status_code, "'.\nTherefore ",
                           "the return value of getPegelonlineW() is NA.\nPlea",
                           "se try again later."))
            return(NA_real_)
        }
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
        req <- httr2::req_error(req, is_error = \(resp) FALSE)
        req <- httr2::req_method(req, "GET")
        req <- httr2::req_retry(req, max_tries = 10L)
        
        # perform the request
        resp <- httr2::req_perform(req)
        status_code <- as.character(resp$status_code)
        if (status_code != "200") {
            message(paste0("The webserver of the PEGELONLINE rest-api returned",
                           " a status code of '", status_code, "'.\nTherefore ",
                           "the return value of getPegelonlineW() is NA.\nPlea",
                           "se try again later."))
            return(NA_real_)
        }
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
    req <- httr2::req_error(req, is_error = \(resp) FALSE)
    resp <- httr2::req_perform(req)
    status_code <- as.character(resp$status_code)
    if (status_code == "200") {
        return(TRUE)
    } else {
        message(paste0("The webserver of the PEGELONLINE rest-api returned a s",
                       "tatus code of '", status_code, "'.\nTherefore the retu",
                       "rn value of getPegelonlineW() is NA.\nPlease try again",
                       " later."))
        return(FALSE)
    }
}


