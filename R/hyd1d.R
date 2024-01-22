#' @name hyd1d
#' @docType package
#' @aliases hyd1d-package
#' 
#' @title hyd1d - 1d Water Level Interpolation along the Rivers Elbe and Rhine
#' 
#' @description The hyd1d package provides an S4 class, relevant datasets and
#'   functions to compute 1d water levels along the German federal waterways
#'   Elbe and Rhine.
#' 
#' \strong{S4 class WaterLevelDataFrame}
#' 
#' The detailled description of the S4 class \code{WaterLevelDataFrame} is 
#' available \link[=WaterLevelDataFrame-class]{here}. This class structures the 
#' handling and computation of the 1d water levels.
#' 
#' \strong{Datasets}
#' 
#' Datasets delivered with this package are:
#' 
#' \itemize{
#'   \item \code{\link{df.gauging_data}}
#'   \item \code{\link{df.gauging_station_data}}
#'   \item \code{\link{df.flys}}
#'   \item \code{\link{df.flys_sections}}
#' }
#' 
#' \strong{Water level computation}
#' 
#' Water levels are either obtained from the \code{\link{df.flys}}-dataset
#' by the functions \code{\link{waterLevelFlys3}} or 
#' \code{\link{waterLevelFlys3Seq}} or computed by the functions 
#' \code{\link{waterLevel}} and \code{\link{waterLevelPegelonline}}. The later 
#' functions use the datasets \code{\link{df.flys}} and 
#' \code{\link{df.gauging_station_data}} and gauging data provided by 
#' \code{\link{df.gauging_data}} or \url{https://pegelonline.wsv.de/gast/start}
#' to linearily interpolate continuous water levels intersecting with the
#' measured water level data at the gauging stations.
#' 
#' @import httr2
#' @import methods
#' @import utils
#' @importFrom curl has_internet
#' @importFrom Rdpack reprompt
#' @importFrom stats aggregate
#'
NULL

