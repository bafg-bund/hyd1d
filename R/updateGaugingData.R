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
            .download_bfg(url, x)
            invisible(TRUE)
        }, error = function(e){
            invisible(FALSE)
            }
        )
    } else {
        file_mtime <- file.info(x)$mtime
        if (file_mtime < Sys.time() - 8 * 24 * 60 * 60) {
            tryCatch({
                .download_bfg(url, x)
                invisible(TRUE)
            }, error = function(e){
                invisible(FALSE)
            }
            )
        } else {
            invisible(FALSE)
        }
    }
}

.download_bfg <- function(x, file) {
    # check internet
    if (!curl::has_internet()) {
        stop(paste0("Dataset provided by hyd1d.bafg.de is unavailable without ",
                    "internet."), call. = FALSE)
    }
    
    # assemble request
    req <- httr2::request(x)
    req <- httr2::req_method(req, "GET")
    req <- httr2::req_retry(req, max_tries = 5L)
    req <- httr2::req_timeout(req, seconds = options()$timeout)
    req <- httr2::req_error(req, is_error = \(resp) FALSE)
    
    # perform the request
    resp <- httr2::req_perform(req, path = file, verbosity = 0)
    
    # handle errors
    status_code <- as.character(resp$status_code)
    if (startsWith(status_code, "4")) {
        mess <- paste0("The request to hyd1d.bafg.de returned a status code of",
                       ":\n   '", status_code, " - ", resp_status_desc(resp),
                       "'\nPlease adjust your request accordingly:\n   url: ",
                       x)
        stop(mess, call. = FALSE)
    }
    if (startsWith(status_code, "5")) {
        mess <- paste0("The request to hyd1d.bafg.de returned a status code of",
                       ":\n   '", status_code, " - ", resp_status_desc(resp),
                       "'\nPlease try again later.")
        stop(mess, call. = FALSE)
    }
    return(TRUE)
}
