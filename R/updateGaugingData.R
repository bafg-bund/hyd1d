#' @name updateGaugingData
#' @rdname updateGaugingData
#'
#' @title Update local copy of \code{df.gauging data}
#'
#' @description Function to overwrite and update the internal dataset
#'   \code{\link{df.gauging_data}}. This function is usually called during the
#'   initial loading of the package. If an update of 
#'   \code{\link{df.gauging_data}} took place more than 8 days ago, an updated
#'   version of \code{\link{df.gauging_data}} will be downloaded and used.
#'
#' @param x path to the file containing \code{\link{df.gauging_data}} 
#'   (type \code{character}).
#'
#' @return \code{invisible(logical)} notifying whether an updated version of 
#'   \code{\link{df.gauging_data}} has been downloaded.
#' 
#' @examples
#'   options("hyd1d.datadir" = tempdir())
#'   updateGaugingData(paste0(options()$hyd1d.datadir,
#'                            "/df.gauging_data_latest.RDS"))
#'
#' @export
#' 
updateGaugingData <- function(x) {
    
    #####
    # assemble internal variables and check the existence of required data
    ##
    if (missing(x)) {
        stop("The 'x' argument has to be supplied.")
    }
    if (!inherits(x, "character")) {
        stop("'x' must be type 'character'.")
    }
    if (length(x) != 1) {
        stop("'x' must have length 1.")
    }
    
    # download url
    url <- paste0("https://hyd1d.bafg.de/downloads/df.gauging_data_latest.RDS")
    
    if (!file.exists(x)) {
        tryCatch({
            utils::download.file(url, x, quiet = TRUE, method = "curl")
            invisible(TRUE)
        }, error = function(e){
            paste0("It was not possible to update the gauging data. Try again ",
                   "later!")
            invisible(FALSE)
            }
        )
    } else {
        file_mtime <- file.info(x)$mtime
        if (file_mtime < Sys.time() - 8 * 24 * 60 * 60) {
            tryCatch({
                utils::download.file(url, x, quiet = TRUE, method = "curl")
                invisible(TRUE)
            }, error = function(e){
                paste0("It was not possible to update the gauging data. Try ag",
                       "ain later!")
                invisible(FALSE)
            }
            )
        } else {
            invisible(FALSE)
        }
    }
}
