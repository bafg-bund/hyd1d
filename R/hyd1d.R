#' @name hyd1d
#' @docType package
#' 
#' @title hyd1d: A package to compute 1D water levels along German federal waterways
#' 
#' @description The hyd1d package provides an S4 class, data import and export 
#' functions, relevant datasets and functions to compute 1D water levels along 
#' the German federal waterways Elbe and Rhein.
#' 
#' \strong{S4 class WaterLevelDataFrame}
#' 
#' The detailled description of the S4 class \code{WaterLevelDataFrame} is 
#' available \link[=WaterLevelDataFrame-class]{here}. This class structures the 
#' handling and computation of the 1D water levels.
#' 
#' \strong{Datasets}
#' 
#' Datasets delivered with this package are:
#' 
#' \itemize{
#'   \item \code{\link{df.gauging_data}}
#'   \item \code{\link{df.gauging_station_data}}
#'   \item \code{\link{df.flys}}
#'   \item \code{\link{df.sections}}
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
#' \code{\link{df.gauging_data}} or \url{https://pegelonline.wsv.de} to 
#' linearily interpolate continuous water levels intersecting with the measured 
#' water level data at the gauging stations.
#' 
#' @importFrom Rdpack reprompt
#' @importFrom methods setClass prototype signature setClass setGeneric setMethod setReplaceMethod initialize validObject new
#'
NULL

