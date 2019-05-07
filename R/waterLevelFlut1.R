#' @name waterLevelFlut1
#' @rdname waterLevelFlut1
#' @aliases waterLevelFlut1
#'
#' @title Compute 1D water level data from the FLYS3 water level MQ and a
#'   gauging station according to Flut1
#'
#' @description This function computes a 1D water level according to the
#'   \href{https://www.bafg.de/DE/08_Ref/U2/02_analyse/01_INFORM/inform.html}{INFORM}
#'    flood duration method Flut1 and store it as column \code{w} of an S4
#'   object of type \linkS4class{WaterLevelDataFrame}. First the function
#'   obtains the reference water level MQ from the FLYS3 database. This
#'   reference water level is then shifted by the difference between measured
#'   water and the FLYS3 water level for MQ at the specified gauging station.
#'
#' @param wldf an object of class \linkS4class{WaterLevelDataFrame}.
#' @param gauging_station has to be type \code{character} and has to have a
#'   length of one. Permitted values are: 'SCHÖNA', 'PIRNA', 'DRESDEN',
#'   'MEISSEN', 'RIESA', 'MÜHLBERG', 'TORGAU', 'PRETZSCH-MAUKEN', 'ELSTER',
#'   'WITTENBERG', 'COSWIG', 'VOCKERODE', 'ROSSLAU', 'DESSAU', 'AKEN', 'BARBY',
#'   'SCHÖNEBECK', 'MAGDEBURG-BUCKAU', 'MAGDEBURG-STROMBRÜCKE',
#'   'MAGDEBURG-ROTHENSEE', 'NIEGRIPP AP', 'ROGÄTZ', 'TANGERMÜNDE', 'STORKAU',
#'   'SANDAU', 'SCHARLEUK', 'WITTENBERGE', 'MÜGGENDORF', 'SCHNACKENBURG',
#'   'LENZEN', 'GORLEBEN', 'DÖMITZ', 'DAMNATZ', 'HITZACKER', 'NEU DARCHAU',
#'   'BLECKEDE', 'BOIZENBURG', 'HOHNSTORF', 'ARTLENBURG', 'GEESTHACHT',
#'   'RHEINWEILER', 'BREISACH', 'RUST', 'OTTENHEIM', 'KEHL-KRONENHOF',
#'   'IFFEZHEIM', 'PLITTERSDORF', 'MAXAU', 'PHILIPPSBURG', 'SPEYER', 'MANNHEIM',
#'   'WORMS', 'NIERSTEIN-OPPENHEIM', 'MAINZ', 'OESTRICH', 'BINGEN', 'KAUB',
#'   'SANKT GOAR', 'BOPPARD', 'BRAUBACH', 'KOBLENZ', 'ANDERNACH', 'OBERWINTER',
#'   'BONN', 'KÖLN', 'DÜSSELDORF', 'RUHRORT', 'WESEL', 'REES', 'EMMERICH'.
#' @param w If the \code{wldf} does not supply a valid non-\code{NA} time slot,
#'   it is possible to execute the function with the help of this optional
#'   parameter. Otherwise \code{\link{getGaugingDataW}} or
#'   \code{\link{getPegelonlineW}} provide gauging data internally.
#' @param uuid has to be type \code{character} and has to have a length of one.
#'   Permitted values are: '7cb7461b-3530-4c01-8978-7f676b8f71ed',
#'   '85d686f1-55b2-4d36-8dba-3207b50901a7',
#'   '70272185-b2b3-4178-96b8-43bea330dcae',
#'   '24440872-5bd2-4fb3-8554-907b49816c49',
#'   'b04b739d-7ffa-41ee-9eb9-95cb1b4ef508',
#'   '16b9b4e7-be14-41fd-941e-6755c97276cc',
#'   '83bbaedb-5d81-4bc6-9f66-3bd700c99c1f',
#'   'f3dc8f07-c2bb-4b92-b0b0-4e01a395a2c6',
#'   'c093b557-4954-4f05-8f5c-6c6d7916c62d',
#'   '070b1eb4-3872-4e07-b2e5-e25fd9251b93',
#'   '1ce53a59-33b9-40dc-9b17-3cd2a2414607',
#'   'ae93f2a5-612e-4514-b5fd-9c8aecdd73c7',
#'   'e97116a4-7d30-4671-8ba1-cdce0a153d1d',
#'   '1edc5fa4-88af-47f5-95a4-0e77a06fe8b1',
#'   '094b96e5-caeb-46d3-a8ee-d44182add069',
#'   '939f82ec-15a9-49c8-8828-dc2f8a2d49e2',
#'   '90bcb315-f080-41a8-a0ac-6122331bb4cf',
#'   'b8567c1e-8610-4c2b-a240-65e8a74919fa',
#'   'ccccb57f-a2f9-4183-ae88-5710d3afaefd',
#'   'e30f2e83-b80b-4b96-8f39-fa60317afcc7',
#'   '3adf88fd-fd7a-41d0-84f5-1143c98a6564',
#'   '133f0f6c-2ca1-4798-9360-5b5f417dd839',
#'   '13e91b77-90f3-41a5-a320-641748e9c311',
#'   'de4cc1db-51cb-4b62-bee2-9750cbe4f5c4',
#'   'f4c55f77-ab80-4e00-bed3-aa6631aba074',
#'   'e32b0a28-8cd5-4053-bc86-fff9c6469106',
#'   'cbf3cd49-91bd-49cc-8926-ccc6c0e7eca4',
#'   '48f2661f-f9cb-4093-9d57-da2418ed656e',
#'   '550e3885-a9d1-4e55-bd25-34228bd6d988',
#'   'c80a4f21-528c-4771-98d7-10cd591699a4',
#'   'ac507f42-1593-49ea-865f-10b2523617c7',
#'   '6e3ea719-48b1-408a-bc55-0986c1e94cd5',
#'   'c233674f-259a-4304-b81f-dce1f415d85b',
#'   'a26e57c9-1cb8-4fca-ba80-9e02abc81df8',
#'   '67d6e882-b60c-40d3-975c-a6d7a2b4e40a',
#'   '6aa1cd8e-e528-4bcb-ba8e-705b6dcb7da2',
#'   '33e0bce0-13df-4ffc-be9d-f1a79e795e1c',
#'   'd9289367-c8aa-4b6a-b1ad-857fec94c6bb',
#'   'b3492c68-8373-4769-9b29-22f66635a478',
#'   '44f7e955-c97d-45c8-9ed7-19406806fb4c',
#'   '06b978dd-8c4d-48ac-a0c8-2c16681ed281',
#'   '9da1ad2b-88db-4cbb-8132-eddfab07d5ba',
#'   '5389b878-fad5-4f37-bb87-e6cb36b7078b',
#'   '787e5d63-61e2-48cc-acf0-633e2bf923f2',
#'   '23af9b02-5c82-4f6e-acb8-f92a06e5e4da',
#'   'b02be240-1364-4c97-8bb6-675d7d842332',
#'   '6b774802-fcb5-49ae-8ecb-ecaf1a278b1c',
#'   'b6c6d5c8-e2d5-4469-8dd8-fa972ef7eaea',
#'   '88e972e1-88a0-4eb9-847c-0925e5999a46',
#'   '2cb8ae5b-c5c9-4fa8-bac0-bb724f2754f4',
#'   '57090802-c51a-4d09-8340-b4453cd0e1f5',
#'   '844a620f-f3b8-4b6b-8e3c-783ae2aa232a',
#'   'd28e7ed1-3317-41c5-bec6-725369ed1171',
#'   'a37a9aa3-45e9-4d90-9df6-109f3a28a5af',
#'   '665be0fe-5e38-43f6-8b04-02a93bdbeeb4',
#'   '0309cd61-90c9-470e-99d4-2ee4fb2c5f84',
#'   '1d26e504-7f9e-480a-b52c-5932be6549ab',
#'   '550eb7e9-172e-48e4-ae1e-d1b761b42223',
#'   '2ff6379d-d168-4022-8da0-16846d45ef9b',
#'   'd6dc44d1-63ac-4871-b175-60ac4040069a',
#'   '4c7d796a-39f2-4f26-97a9-3aad01713e29',
#'   '5735892a-ec65-4b29-97c5-50939aa9584e',
#'   'b45359df-c020-4314-adb1-d1921db642da',
#'   '593647aa-9fea-43ec-a7d6-6476a76ae868',
#'   'a6ee8177-107b-47dd-bcfd-30960ccc6e9c',
#'   '8f7e5f92-1153-4f93-acba-ca48670c8ca9',
#'   'c0f51e35-d0e8-4318-afaf-c5fcbc29f4c1',
#'   'f33c3cc9-dc4b-4b77-baa9-5a5f10704398',
#'   '2f025389-fac8-4557-94d3-7d0428878c86',
#'   '9598e4cb-0849-401e-bba0-689234b27644'.
#' @param shiny \code{logical}, determing wether columns (\code{section},
#'   \code{weight_x}, \code{weight_y}) relevant for the
#'   \code{\link{plotShiny}()}-function are appended to the resulting
#'   \linkS4class{WaterLevelDataFrame}.
#'
#' @return An object of class \linkS4class{WaterLevelDataFrame}.
#'
#' @details This function computes a water level based on the reference water
#'   level MQ from the FLYS database. Since the function only shifts this single
#'   reference water level so that it fits to the measured water level, no
#'   interpolation is needed. Therefore the \code{shiny} columns have constant
#'   values of \code{section <- 1}, \code{weight_x <- 1} and \code{weight_y <-
#'   shift}.
#' 
#' @references 
#'   \insertRef{rosenzweig_inform_2011}{hyd1d}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf1 <- waterLevelFlut1(wldf, "ROSSLAU")
#' wldf2 <- waterLevelFlut1(wldf, "DESSAU")
#'
#' wldf1$w - wldf2$w
#'
#' @export
#' 
waterLevelFlut1 <- function(wldf, gauging_station, w, uuid, shiny = FALSE) {
    
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
    # presence
    if (missing(wldf)){
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": 'wldf' has to be supplied."))
    }
    # WaterLevelDataFrame
    if (class(wldf) != "WaterLevelDataFrame"){
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
        if (is.na(time) & missing(w)){
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
        gs <- asc2utf8(df.gauging_station_data_sel$gauging_station)
        uuids <- df.gauging_station_data_sel$uuid
        
        if (missing(gauging_station) & missing(uuid)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The 'gauging_",
                                       "station' or 'uuid' argument has to ",
                                       "be supplied."))
        } else {
            if (!(missing(gauging_station))){
                if (class(gauging_station) != "character"){
                    errors <- c(errors, paste0("Error ", l(errors), ": 'gaugi",
                                               "ng_station' must be type ",
                                               "'character'."))
                }
                if (length(gauging_station) != 1){
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
                        id <- which(df.gauging_station_data_sel$km_qps > 
                                        start_f & 
                                    df.gauging_station_data_sel$km_qps < end_f)
                        id <- c(min(id) - 1, id, max(id) + 1)
                        gs_possible <- stats::na.omit(
                            df.gauging_station_data_sel$gauging_station[id])
                        if (!(df.gs$gauging_station %in% gs_possible)){
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
            
            if (!(missing(uuid))){
                if (class(uuid) != "character"){
                    errors <- c(errors, paste0("Error ", l(errors), ": 'uuid' ",
                                               "must be type 'character'."))
                }
                if (length(uuid) != 1){
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
                    
                    if (df.gs$km_qps < start_f | df.gs$km_qps > end_f){
                        id <- which(df.gauging_station_data_sel$km_qps > 
                                        start_f & 
                                    df.gauging_station_data_sel$km_qps < end_f)
                        id <- c(min(id) - 1, id, max(id) + 1)
                        uuid_possible <- stats::na.omit(
                            df.gauging_station_data_sel$uuid[id])
                        if (!(df.gs$uuid %in% uuid_possible)){
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
            
            if (!(missing(gauging_station)) & !(missing(uuid))){
                if (id_gs != id_uu){
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
            if (exists("uuid_internal")){
                df.gs$w <- getGaugingDataW(uuid = uuid_internal, 
                                           time = time)
                df.gs$wl <- round(df.gs$pnp + df.gs$w / 100, 2)
                
                ##
                # w
                if (!(is.na(time))){
                    if (!missing(w)){
                        if (length(w) != 1) {
                            errors <- c(errors, paste0("Error ", l(errors), ":",
                                                       " 'w' must have length ",
                                                       "1."))
                        }
                        if (class(w) != "numeric"){
                            errors <- c(errors, paste0("Error ", l(errors), ":",
                                                       " 'w' must be type 'num",
                                                       "eric'."))
                        }
                        if (w < 0 | w >= 1000) {
                            errors <- c(errors, paste0("Error ", l(errors), ":",
                                                       " 'w' must be in a rang",
                                                       "e between 0 and 1000."))
                        }
                        if (w != df.gs$w){
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
    
    if (l(errors) != "1"){
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
    for (a_column in c_columns){
        df.gs[ , a_column] <- as.character(df.gs[ , a_column])
    }
    
    # convert type of integer columns
    c_columns <- c("n_wls_below_w_do", "n_wls_above_w_do", 
                   "n_wls_below_w_up", "n_wls_above_w_up")
    for (a_column in c_columns){
        df.gs[ , a_column] <- as.integer(df.gs[ , a_column])
    }
    
    # shiny
    if (shiny){
        wldf_data$section <- rep(1, nrow(wldf_data))
        wldf_data$weight_x <- rep(1, nrow(wldf_data))
        wldf_data$weight_y <- rep(df.gs$wl - wl_mq, nrow(wldf_data))
        
        wldf <- methods::new("WaterLevelDataFrame",
                             wldf_data,
                             river                    = river,
                             time                     = time,
                             gauging_stations         = df.gs,
                             gauging_stations_missing = character(),
                             comment = paste0("Computed by waterLevelFlut1",
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
                             comment = paste0("Computed by waterLevelFlut1",
                                              "(): gauging_station = ",
                                              df.gs$gauging_station,
                                              ", w = ", df.gs$w))
        
        return(wldf)
    }
}

