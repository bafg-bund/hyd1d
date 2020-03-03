# @name date_gauging_data
# @rdname date_gauging_data
# @title Date df.gauging_data was saved
# @description Date df.gauging_data was saved. Presently it is the 
#   $RDO_DATE_GAUGING_DATA$.
#"date_gauging_data"


#' @name df.gauging_data
#' @rdname df.gauging_data
#' 
#' @title Gauging data for all WSV-run gauging stations along Elbe and Rhein
#' 
#' @description This dataset contains all \strong{daily-averaged} gauging data
#'   for the gauging stations along \strong{Elbe} and \strong{Rhein} operated by
#'   the waterway and navigation authorities (Wasserstraßen- und
#'   Schifffahrtsverwaltung (WSV)) since 1990-01-01. Data from
#'   the 1990-01-01 until 2019-12-31 are validated and were queried from the
#'   BfG-Wiski
#'   (\href{http://www.bafg.de/DE/08_Ref/M1/03_Pegelwesen/HYDABA/hydaba_node.html}{HyDaBa})
#'    and supplied by \email{Datenstelle-M1@@bafg.de}. Data after 2019-12-31 are
#'   continuously collected from \url{https://pegelonline.wsv.de} and are not
#'   officially validated. Not validated recent data will be replaced anually
#'   and distributed through package updates.
#' 
#' @format A \code{data.frame} with 3 variables: 
#' \describe{
#'   \item{gauging_station}{name of the gauging station (type \code{character}). It is presently used as JOIN field.} 
#'   \item{date}{of the measurement (type \code{Date}).} 
#'   \item{w}{water level relative to the gauging stations null (cm, type \code{numeric}).} 
#' }
#'
#' @references \insertRef{wsv_pegeldaten_2020}{hyd1d}
#'
"df.gauging_data"


#' @name df.gauging_station_data
#' @rdname df.gauging_station_data
#' 
#' @title Gauging station data for all WSV-run gauging stations along Elbe and Rhein
#'
#' @description This dataset contains gauging station data for the gauging
#'   stations along \strong{Elbe} and \strong{Rhein} operated by the waterway 
#'   and navigation authorities (Wasserstraßen- und Schifffahrtsverwaltung 
#'   (WSV)). The data were originally obtained from 
#'   \url{https://pegelonline.wsv.de} and are updated anually.
#' 
#' @format A \code{data.frame} with $RDO_NROW_DF.GAUGING_STATION_DATA$ rows and 
#'   13 variables: 
#' \describe{
#'   \item{id}{continuous numbering (type \code{integer}).}
#'   \item{gauging_station}{name of the gauging station (type \code{character}). It is presently used as JOIN field.}
#'   \item{uuid}{of the gauging station in the PEGELONLINE system (type \code{character}).} 
#'   \item{agency}{of the waterway and navigation authority responsible for the respective gauging station (type \code{character}).} 
#'   \item{km}{official stationing of the gauging station (type \code{numeric}).}
#'   \item{longitude}{of the gauging stations location (WGS1984, type \code{numeric}).}
#'   \item{latitude}{of the gauging stations location (WGS1984, type \code{numeric}).}
#'   \item{mw}{mean water level of the gauging station (m relative to the gauging stations null, type \code{numeric}).}
#'   \item{mw_timespan}{timespan used to derive the gauging stations mean water level (type \code{character}).}
#'   \item{pnp}{the gauging stations null relative to sea level (NHN (DHHN92), type \code{numeric}).}
#'   \item{data_present}{\code{logical} to separate TRUE (real) from section structuring FALSE gauging stations.}
#'   \item{km_qps}{corrected stationing used for the water level computations of \code{\link{waterLevel}} and \code{\link{waterLevelPegelonline}} (type \code{numeric}).}
#'   \item{river}{the gauging station is located on (type \code{character}).}
#' }
#' 
"df.gauging_station_data"


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
#'   with a spacial resolution of 200 m river stretch along the official 
#'   river stationing. They range from extremely low to extremely high flow 
#'   conditions and are usually separated vertically by 0.2 - 0.6 m.
#'   
#'   \if{html}{\figure{flys3waterlevels.png}{options: width="60\%" alt="Figure: flys3waterlevels.png"}}
#'   \if{latex}{\figure{flys3waterlevels.pdf}{options: width=7cm}}
#' 
#' @format A \code{data.frame} with $RDO_NROW_DF.FLYS$ rows and 4 variables: 
#' \describe{
#'   \item{river}{name of the relevant water body (type \code{character}).}
#'   \item{name}{of the FLYS 3 water level (type \code{character}). See details for more information.}
#'   \item{station}{rivers stationing (type \code{numeric}).}
#'   \item{w}{water level (cm above gauging station null, type \code{numeric}).} 
#' }
#' 
#' @details The \code{name}ing of the water levels is \code{river}-specific:
#'   
#'   \strong{Elbe:}
#'    
#'   "0.5MNQ", "MNQ", "0.5MQ", "a", "0.75MQ", "b", "MQ", "c", 
#'   "2MQ", "3MQ", "d", "e", "MHQ", "HQ2", "f", "HQ5", "g", "h", "HQ10", "HQ15",
#'   "HQ20", "HQ25", "HQ50", "HQ75", "HQ100", "i", "HQ150", "HQ200", "HQ300", 
#'   "HQ500"
#'   
#'   \strong{Rhein:}
#'   
#'   "Ud=1", "Ud=5", "GlQ2012", "Ud=50", "Ud=80", "Ud=100", 
#'   "Ud=120", "Ud=183", "MQ", "Ud=240","Ud=270", "Ud=310", "Ud=340", "Ud=356", 
#'   "Ud=360", "MHQ", "HQ2", "HQ5", "HQ5-10", "HQ10", "HQ10-20", "~HQ20",
#'   "HQ20-50", "HQ50", "HQ50-100", "HQ100", "HQ100-200", "HQ200", "HQ200-ex", 
#'   "HQextr."
#'   
#'   Both lists of water levels are ordered from low to high water levels.
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
"df.flys_sections"


#' @name df.sections
#' @rdname df.sections
#' 
#' @title Sections with precomputed water level data along Elbe and Rhein
#'
#' @description A dataset containing all precomputed sections and relevant 
#'   descriptive data to locate and import JSON-formated water level data within 
#'   the \code{\link{readWaterLevelFileDB}()}-function.
#'
#' @format A \code{data.frame} with 89 rows and 8 variables:
#' \describe{
#'   \item{id}{continuous numbering (type \code{integer}).}
#'   \item{river}{a sections belongs to (type \code{character}).}
#'   \item{name}{of the section (type \code{character}).}
#'   \item{name_km}{consisting of 0-padded upper and lower km (type \code{character}).}
#'   \item{from_km}{upper km of the section (type \code{numeric}).}
#'   \item{to_km}{lower km of the section (type \code{numeric}).}
#'   \item{gs_upper}{name of the section upstream (type \code{character}).}
#'   \item{gs_lower}{name of the section downstream (type \code{character}).}
#' }
#' 
#' @source \url{http://r.bafg.de/~WeberA/INFORM 4}
"df.sections"

