nrow_df.gauging_data <- function() {
    
    if (file.exists("DB_credentials_gauging_data") &
        requireNamespace("RPostgreSQL") & requireNamespace("DBI")) {
        
        # credentials
        credentials <- credentials("DB_credentials_gauging_data")
        
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
                COUNT(*)
            FROM
                gauging_data
            FULL JOIN
                gauging_station_data
            ON
                gauging_data.gauging_station = gauging_station_data.gauging_station
            WHERE
                gauging_data.date >= '1960-01-01' AND 
                gauging_data.date <= '2020-12-31' AND
                (
                    gauging_station_data.water_shortname = 'ELBE' OR
                    gauging_station_data.water_shortname = 'RHEIN'
                )"
        
        n <- DBI::dbGetQuery(con, query_string)
    } else {
        n <- 733449
    }
    
    c(paste0("@format A \\code{data.frame} with ", n, " (", "rows and 3 variables):"),
      "\\describe{",
      "\\item{gauging_station}{name of the gauging station (type \\code{character}).  It is used as JOIN field for dataset \\code{\\link{df.gauging_station_data}}.}",
      "\\item{date}{of the measurement (type \\code{Date}).}",
      "\\item{w}{water level relative to the gauge zero (cm, type \\code{numeric}).}",
      "}"
    )
}

#' @name df.gauging_data
#' @rdname df.gauging_data
#' 
#' @title Gauging data for all WSV-run gauging stations along Elbe and Rhein
#' 
#' @description This dataset contains all \strong{daily-averaged} gauging data
#'   for the gauging stations along \strong{Elbe} and \strong{Rhein} operated by
#'   the waterway and shipping administration (Wasserstraßen- und
#'   Schifffahrtsverwaltung (WSV)) since 1960-01-01. Data from
#'   1960-01-01 until 2021-12-31 are validated and were queried from the
#'   BfG-Wiski
#'   (\href{http://www.bafg.de/DE/08_Ref/M1/03_Pegelwesen/HYDABA/hydaba_node.html}{HyDaBa})
#'    and supplied by \email{Datenstelle-M1@@bafg.de}. Data after 2021-12-31 are
#'   continuously collected from \url{https://pegelonline.wsv.de} and are not
#'   officially validated. Unvalidated recent data will be replaced anually and
#'   distributed through package and/or internal dataset updates.
#'   
#'   The latest version is stored locally under
#'   \code{~/.hyd1d/df.gauging_data_latest.RDS}.
#' 
#' @eval nrow_df.gauging_data()
#' 
#' @seealso \code{\link{updateGaugingData}}
#' 
#' @references
#'    \insertRef{wsv_pegeldaten_2021}{hyd1d}
#'    
#'    \insertRef{wsv_pegelonline_2018}{hyd1d}
#'
"df.gauging_data"

nrow_df.gauging_station_data <- function() {
    
    if (file.exists("DB_credentials_gauging_data") &
        requireNamespace("RPostgreSQL") & requireNamespace("DBI")) {
        
        # credentials
        credentials <- credentials("DB_credentials_gauging_data")
        
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
                COUNT(*)
            FROM
                gauging_station_data
            WHERE
                (
                    water_shortname = 'ELBE' OR
                    water_shortname = 'RHEIN'
                ) AND data_present
            "
        
        n <- DBI::dbGetQuery(con, query_string)
    } else {
        n <- 70
    }
    
    c(paste0("@format A \\code{data.frame} with ", n, " rows and 13 variables:"),
      "\\describe{",
          "\\item{id}{continuous numbering (type \\code{integer}).}",
          paste0("\\item{gauging_station}{name of the gauging station (type \\",
                 "code{character}). It is used as JOIN field for dataset \\cod",
                 "e{\\link{df.gauging_data}}.}"),
          paste0("\\item{uuid}{of the gauging station in the PEGELONLINE syste",
                 "m (type \\code{character}).}"),
          paste0("\\item{agency}{of the waterway and shipping administration i",
                 "n charge of the respective gauging station (type \\code{char",
                 "acter}).}"),
          paste0("\\item{km}{official stationing of the gauging station (type ",
                 "\\code{numeric}).}"),
          paste0("\\item{longitude}{of the gauging stations location (WGS1984,",
                 " type \\code{numeric}).}"),
          paste0("\\item{latitude}{of the gauging stations location (WGS1984, ",
                 "type \\code{numeric}).}"),
          paste0("\\item{mw}{mean water level of the gauging station (m relati",
                 "ve to the gauge zero, type \\code{numeric}).}"),
          paste0("\\item{mw_timespan}{timespan used to derive the gauging stat",
                 "ions mean water level (type \\code{character}).}"),
          paste0("\\item{pnp}{the gauge zero relative to sea level (NHN (DHHN9",
                 "2), type \\code{numeric}).}"),
          paste0("\\item{data_present}{\\code{logical} to separate TRUE (real)",
                 " from section structuring FALSE gauging stations.}"),
          paste0("\\item{km_qps}{corrected stationing used for the water level",
                 " computations of \\code{\\link{waterLevel}} and \\code{\\lin",
                 "k{waterLevelPegelonline}} (type \\code{numeric}).}"),
          paste0("\\item{river}{the gauging station is located on (type \\code",
                 "{character}).}"),
      "}"
    )
}

#' @name df.gauging_station_data
#' @rdname df.gauging_station_data
#' 
#' @title Gauging station data for all WSV-run gauging stations along Elbe and Rhein
#'
#' @description This dataset contains gauging station data for the gauging
#'   stations along \strong{Elbe} and \strong{Rhein} operated by the waterway 
#'   and shipping administration (Wasserstraßen- und Schifffahrtsverwaltung 
#'   (WSV)). The data were originally obtained from 
#'   \url{https://pegelonline.wsv.de} and are updated anually.
#' 
#' @eval nrow_df.gauging_station_data()
#' 
#' @references
#'    \insertRef{wsv_pegelonline_2018}{hyd1d}
#'
"df.gauging_station_data"

nrow_df.flys <- function() {
    
    if (file.exists("DB_credentials_flys3") &
        requireNamespace("ROracle") & requireNamespace("DBI")) {
        
        # credentials
        f3_credentials <- credentials("DB_credentials_flys3")
        
        # read the data
        # access the FLYS3 DB
        f3_string <- paste0("(DESCRIPTION=",
                            "(ADDRESS=(PROTOCOL=tcp)",
                            "(HOST=10.140.79.56)(PORT=1521))",
                            "(CONNECT_DATA=",
                            "(SERVICE_NAME=FLYS3.DBMSDB.BAFG.DE)))")
        f3_con <- tryCatch(
            {
                ROracle::dbConnect(drv      = DBI::dbDriver("Oracle"),
                                   username = f3_credentials["user"],
                                   password = f3_credentials["password"],
                                   dbname   = f3_string)
            },
            error = function(cond) {return(FALSE)},
            warning = function(cond) {return(FALSE)}
        )
        
        if (is.logical(f3_con)) {
            n <- 169980
        } else {
            # retrieve the data
            # for the Elbe
            query_string_elbe <- "
            SELECT
                FLYS3.WST_COLUMNS.NAME AS \"name\",
                FLYS3.WST_COLUMN_VALUES.POSITION AS \"station\",
                FLYS3.WST_COLUMN_VALUES.W AS \"w\"
            FROM
            FLYS3.RIVERS
                INNER JOIN FLYS3.WSTS ON FLYS3.RIVERS.ID = FLYS3.WSTS.RIVER_ID
                INNER JOIN FLYS3.WST_KINDS ON FLYS3.WST_KINDS.ID = FLYS3.WSTS.KIND
                INNER JOIN FLYS3.WST_COLUMNS ON FLYS3.WSTS.ID = 
                    FLYS3.WST_COLUMNS.WST_ID
                INNER JOIN FLYS3.WST_COLUMN_VALUES ON FLYS3.WST_COLUMNS.ID = 
                    FLYS3.WST_COLUMN_VALUES.WST_COLUMN_ID
            WHERE
                FLYS3.WSTS.KIND = 0 AND
                FLYS3.RIVERS.NAME = 'Elbe' AND
                FLYS3.WST_COLUMN_VALUES.POSITION <= 585.7 AND
                FLYS3.WST_COLUMN_VALUES.POSITION >= 0
            ORDER BY
                FLYS3.WST_COLUMN_VALUES.POSITION ASC, FLYS3.WST_COLUMN_VALUES.W"
            
            df.flys_elbe <- DBI::dbGetQuery(f3_con, query_string_elbe)
            df.flys_elbe <- cbind(data.frame(river = as.character("Elbe", 
                                                                  nrow(df.flys_elbe)),
                                             stringsAsFactors = FALSE),
                                  df.flys_elbe)
            
            # for the Rhein
            query_string_rhein <- "
            SELECT
                FLYS3.WST_COLUMNS.NAME AS \"name\",
                FLYS3.WST_COLUMN_VALUES.POSITION AS \"station\",
                FLYS3.WST_COLUMN_VALUES.W AS \"w\"
            FROM
                FLYS3.RIVERS
                INNER JOIN FLYS3.WSTS ON FLYS3.RIVERS.ID = FLYS3.WSTS.RIVER_ID
                INNER JOIN FLYS3.WST_KINDS ON FLYS3.WST_KINDS.ID = FLYS3.WSTS.KIND
                INNER JOIN FLYS3.WST_COLUMNS ON FLYS3.WSTS.ID = 
                    FLYS3.WST_COLUMNS.WST_ID
                INNER JOIN FLYS3.WST_COLUMN_VALUES ON FLYS3.WST_COLUMNS.ID = 
                    FLYS3.WST_COLUMN_VALUES.WST_COLUMN_ID
            WHERE
                FLYS3.WSTS.KIND = 0 AND
                FLYS3.RIVERS.NAME = 'Rhein' AND
                FLYS3.WST_COLUMN_VALUES.POSITION <= 865.7 AND
                FLYS3.WST_COLUMN_VALUES.POSITION >= 336.2
            ORDER BY
                FLYS3.WST_COLUMN_VALUES.POSITION ASC, FLYS3.WST_COLUMN_VALUES.W"
            
            df.flys_rhein <- DBI::dbGetQuery(f3_con, query_string_rhein)
            df.flys_rhein <- cbind(data.frame(river = as.character("Rhein", 
                                                                   nrow(df.flys_rhein)),
                                              stringsAsFactors = FALSE),
                                   df.flys_rhein)
            
            # combine both datasets
            df.flys <- rbind.data.frame(df.flys_elbe, df.flys_rhein,
                                        stringsAsFactors = FALSE)
            
            n <- nrow(df.flys)
            
        }
    } else {
        n <- 169980
    }
    
    c(paste0("@format A \\code{data.frame} with ", n, " rows and 4 variables:"),
      "\\describe{",
          paste0("\\item{river}{name of the relevant water body (type \\code{c",
                 "haracter}).}"),
          paste0("\\item{name}{of the FLYS 3 water level (type \\code{characte",
                 "r}). See details for more information.}"),
          "\\item{station}{rivers stationing (type \\code{numeric}).}",
          paste0("\\item{w}{water level (cm above gauge zero, type \\code{nume",
                 "ric}).}"),
      "}")
}

names_df.flys <- function(river = c("Elbe", "Rhein")) {
    if (file.exists("DB_credentials_flys3") &
        requireNamespace("ROracle") & requireNamespace("DBI")) {
        
        # credentials
        f3_credentials <- credentials("DB_credentials_flys3")
        
        # read the data
        # access the FLYS3 DB
        f3_string <- paste0("(DESCRIPTION=",
                            "(ADDRESS=(PROTOCOL=tcp)",
                            "(HOST=10.140.79.56)(PORT=1521))",
                            "(CONNECT_DATA=",
                            "(SERVICE_NAME=FLYS3.DBMSDB.BAFG.DE)))")
        f3_con <- tryCatch(
          {
            ROracle::dbConnect(drv      = DBI::dbDriver("Oracle"),
                               username = f3_credentials["user"],
                               password = f3_credentials["password"],
                               dbname   = f3_string)
          },
          error = function(cond) {return(FALSE)},
          warning = function(cond) {return(FALSE)}
        )
        
        if (is.logical(f3_con)) {
            if (river == "Elbe") {
                return(c("0.5MNQ", "MNQ", "0.5MQ", "a", "0.75MQ", "b", "MQ",
                         "c","2MQ", "3MQ", "d", "e", "MHQ", "HQ2", "f", "HQ5",
                         "g", "h", "HQ10", "HQ15", "HQ20", "HQ25", "HQ50",
                         "HQ75", "HQ100", "i", "HQ150", "HQ200", "HQ300",
                         "HQ500"))
            } else {
                return(c("Ud=1", "Ud=5", "GlQ2012", "Ud=50", "Ud=80", "Ud=100",
                         "Ud=120", "Ud=183", "MQ", "Ud=240","Ud=270", "Ud=310",
                         "Ud=340", "Ud=356", "Ud=360", "MHQ", "HQ2", "HQ5",
                         "HQ5-10", "HQ10", "HQ10-20", "~HQ20", "HQ20-50",
                         "HQ50", "HQ50-100", "HQ100", "HQ100-200", "HQ200",
                         "HQ200-ex", "HQextr."))
            }
        } else {
          query_string <- paste0("
              SELECT
                  FLYS3.WST_COLUMNS.NAME AS \"name\"
              FROM
              FLYS3.RIVERS
                  INNER JOIN FLYS3.WSTS ON FLYS3.RIVERS.ID = FLYS3.WSTS.RIVER_ID
                  INNER JOIN FLYS3.WST_KINDS ON FLYS3.WST_KINDS.ID = FLYS3.WSTS.KIND
                  INNER JOIN FLYS3.WST_COLUMNS ON FLYS3.WSTS.ID = 
                      FLYS3.WST_COLUMNS.WST_ID
                  INNER JOIN FLYS3.WST_COLUMN_VALUES ON FLYS3.WST_COLUMNS.ID = 
                      FLYS3.WST_COLUMN_VALUES.WST_COLUMN_ID
              WHERE
                  FLYS3.WSTS.KIND = 0 AND
                  FLYS3.RIVERS.NAME = '", river, "' AND
                  FLYS3.WST_COLUMN_VALUES.POSITION = 0
              ORDER BY
                  FLYS3.WST_COLUMN_VALUES.W")
            
            return(DBI::dbGetQuery(f3_con, query_string)$name)
        }
    } else {
        if (river == "Elbe") {
            return(c("0.5MNQ", "MNQ", "0.5MQ", "a", "0.75MQ", "b", "MQ",
                     "c","2MQ", "3MQ", "d", "e", "MHQ", "HQ2", "f", "HQ5",
                     "g", "h", "HQ10", "HQ15", "HQ20", "HQ25", "HQ50",
                     "HQ75", "HQ100", "i", "HQ150", "HQ200", "HQ300",
                     "HQ500"))
        } else {
            return(c("Ud=1", "Ud=5", "GlQ2012", "Ud=50", "Ud=80", "Ud=100",
                     "Ud=120", "Ud=183", "MQ", "Ud=240","Ud=270", "Ud=310",
                     "Ud=340", "Ud=356", "Ud=360", "MHQ", "HQ2", "HQ5",
                     "HQ5-10", "HQ10", "HQ10-20", "~HQ20", "HQ20-50",
                     "HQ50", "HQ50-100", "HQ100", "HQ100-200", "HQ200",
                     "HQ200-ex", "HQextr."))
        }
    }
}

details_df.flys <- function() {
    
    if (file.exists("DB_credentials_flys3") &
        requireNamespace("ROracle") & requireNamespace("DBI")) {
        
        # credentials
        f3_credentials <- credentials("DB_credentials_flys3")
        
        # read the data
        # access the FLYS3 DB
        f3_string <- paste0("(DESCRIPTION=",
                            "(ADDRESS=(PROTOCOL=tcp)",
                            "(HOST=10.140.79.56)(PORT=1521))",
                            "(CONNECT_DATA=",
                            "(SERVICE_NAME=FLYS3.DBMSDB.BAFG.DE)))")
        f3_con <- tryCatch(
            {
                ROracle::dbConnect(drv      = DBI::dbDriver("Oracle"),
                                   username = f3_credentials["user"],
                                   password = f3_credentials["password"],
                                   dbname   = f3_string)
            },
            error = function(cond) {return(FALSE)},
            warning = function(cond) {return(FALSE)}
        )
        
        if (is.logical(f3_con)) {
            wl_elbe <- c("0.5MNQ", "MNQ", "0.5MQ", "a", "0.75MQ", "b", "MQ",
                         "c","2MQ", "3MQ", "d", "e", "MHQ", "HQ2", "f", "HQ5",
                         "g", "h", "HQ10", "HQ15", "HQ20", "HQ25", "HQ50",
                         "HQ75", "HQ100", "i", "HQ150", "HQ200", "HQ300",
                         "HQ500")
            wl_rhein <- c("Ud=1", "Ud=5", "GlQ2012", "Ud=50", "Ud=80", "Ud=100",
                          "Ud=120", "Ud=183", "MQ", "Ud=240","Ud=270", "Ud=310",
                          "Ud=340", "Ud=356", "Ud=360", "MHQ", "HQ2", "HQ5",
                          "HQ5-10", "HQ10", "HQ10-20", "~HQ20", "HQ20-50",
                          "HQ50", "HQ50-100", "HQ100", "HQ100-200", "HQ200",
                          "HQ200-ex", "HQextr.")
        } else {
            # retrieve the data
            # for the Elbe
            wl_elbe <- names_df.flys(river = "Elbe")
            
            # for the Rhein
            wl_rhein <- names_df.flys(river = "Rhein")
        }
    } else {
        wl_elbe <- c("0.5MNQ", "MNQ", "0.5MQ", "a", "0.75MQ", "b", "MQ",
                     "c","2MQ", "3MQ", "d", "e", "MHQ", "HQ2", "f", "HQ5",
                     "g", "h", "HQ10", "HQ15", "HQ20", "HQ25", "HQ50",
                     "HQ75", "HQ100", "i", "HQ150", "HQ200", "HQ300",
                     "HQ500")
        wl_rhein <- c("Ud=1", "Ud=5", "GlQ2012", "Ud=50", "Ud=80", "Ud=100",
                      "Ud=120", "Ud=183", "MQ", "Ud=240","Ud=270", "Ud=310",
                      "Ud=340", "Ud=356", "Ud=360", "MHQ", "HQ2", "HQ5",
                      "HQ5-10", "HQ10", "HQ10-20", "~HQ20", "HQ20-50",
                      "HQ50", "HQ50-100", "HQ100", "HQ100-200", "HQ200",
                      "HQ200-ex", "HQextr.")
    }
    
    c(paste0("@details The \\code{name}ing of the water levels is \\code{river",
             "}-specific:"),
      "", "\\strong{Elbe:}", "",
      paste0("'", paste0(wl_elbe, collapse = "', '"), "'"),
      "", "\\strong{Rhein:}", "",
      paste0("'", paste0(wl_rhein, collapse = "', '"), "'"),
      "",
      "Both lists of water levels are ordered from low to high water levels.")
}

#' @name df.flys
#' @rdname df.flys
#' 
#' @title Stationary water levels from the FLYS 3-database
#' 
#' @description This dataset contains the 30 stationary 1D water levels for the
#'   rivers \strong{Elbe} and \strong{Rhein} originally stored in the 
#'   \href{https://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html}{FLYS3}-database.
#'   
#'   For both rivers 30 stationary water levels have been computed by means of 
#'   the 1D hydraulic model \href{https://www.deltares.nl/en/software/sobek}{SOBEK}.
#'   The water levels cover the full length of the free flowing river sections 
#'   with a spatial resolution of 200 m river stretch along the official 
#'   river stationing. They range from extremely low to extremely high flow 
#'   conditions and are usually separated vertically by 0.2 - 0.6 m.
#'   
#'   \if{html}{\figure{flys3waterlevels.png}{options: width="60\%" alt="Figure: flys3waterlevels.png" style="text-align: center;"}}
#'   \if{latex}{\figure{flys3waterlevels.pdf}{options: width=7cm}}
#' 
#' @eval nrow_df.flys()
#' 
#' @eval details_df.flys()
#' 
#' @references
#'   \insertRef{busch_einheitliche_2009}{hyd1d}
#'   
#'   \insertRef{hkv_hydrokontor_erstellung_2014}{hyd1d}
#'   
#'   \insertRef{bundesanstalt_fur_gewasserkunde_flys_2016}{hyd1d}
#'   
#'   \insertRef{deltares_sobek_2018}{hyd1d}
#' 
"df.flys"


#' @name df.flys_sections
#' @rdname df.flys_sections
#' 
#' @title Reference gauging stations according to FLYS3
#' 
#' @description This dataset relates the reference gauging stations to river
#'   stationing as used within FLYS3
#' 
#' @format A \code{data.frame} with 24 rows and 4 variables:
#' \describe{
#'   \item{river}{name of the FLYS3 water body (type \code{character}).}
#'   \item{gauging_station}{name of the reference gauging station (type \code{character}).}
#'   \item{from}{uppermost station of the river section (type \code{numeric}).}
#'   \item{to}{lowermost station of the river section (type \code{numeric}).}
#'   \item{uuid}{name of the reference gauging station (type \code{character}).}
#' }
#' 
#' @references
#'    \insertRef{bundesanstalt_fur_gewasserkunde_flys_2016}{hyd1d}
#' 
"df.flys_sections"


#' @name df.sections
#' @rdname df.sections
#' 
#' @title Sections with precomputed water level data along Elbe and Rhein
#'
#' @description A dataset containing all precomputed sections and relevant 
#'   descriptive data to locate and import JSON-formatted water level data 
#'   within the \code{\link{readWaterLevelFileDB}()}-function.
#'
#' @format A \code{data.frame} with 89 rows and 8 variables:
#' \describe{
#'   \item{id}{continuous numbering (type \code{integer}).}
#'   \item{river}{a section belongs to (type \code{character}).}
#'   \item{name}{of the section (type \code{character}).}
#'   \item{name_km}{consisting of 0-padded upper and lower km (type \code{character}).}
#'   \item{from_km}{upper km of the section (type \code{numeric}).}
#'   \item{to_km}{lower km of the section (type \code{numeric}).}
#'   \item{gs_upper}{name of the section upstream (type \code{character}).}
#'   \item{gs_lower}{name of the section downstream (type \code{character}).}
#' }
#' 
"df.sections"

