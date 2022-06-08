#' @name updateGaugingData
#' @rdname updateGaugingData
#'
#' @title Update local copy of \code{df.gauging data}
#'
#' @description Function to overwrite and update the internal dataset
#'   \code{\link{df.gauging_data}}. This function is usually called during the
#'   initial loading of the package. If \code{\link{df.gauging_data}} was
#'   updated more than 29 days ago, an updated version of
#'   \code{\link{df.gauging_data}} will be downloaded and used.
#'
#' @param x date when \code{\link{df.gauging_data}} was updated the last time
#'   (type \code{Date}).
#'
#' @return \code{logical} notifying whether an updated version of 
#'   \code{\link{df.gauging_data}} has been downloaded.
#' 
#' @examples
#' \dontrun{
#' updateGaugingData(as.Date("2016-12-21"))
#' }
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
    if (!inherits(x, "Date")) {
        stop("'x' must be type 'Date'.")
    }
    if (length(x) != 1) {
        stop("'x' must have length 1.")
    }
    
    # set relevant DB variables
    if (utils::compareVersion(as.character(getRversion()), "3.5.0") < 0) {
        file_date <- paste0(path.expand('~'), "/.hyd1d/date_gauging_data_v2.RD",
                            "S")
        file_data <- paste0(path.expand('~'), "/.hyd1d/df.gauging_data_latest_",
                            "v2.RDS")
        url <- paste0("https://www.aqualogy.de/wp-content/uploads/bfg/df.gaugi",
                      "ng_data_latest_v2.RDS")
    } else {
        file_date <- paste0(path.expand('~'), "/.hyd1d/date_gauging_data.RDS")
        file_data <- paste0(path.expand('~'), "/.hyd1d/df.gauging_data_latest.",
                            "RDS")
        url <- paste0("https://www.aqualogy.de/wp-content/uploads/bfg/df.gaugi",
                      "ng_data_latest.RDS")
    }
    
    if((x < Sys.Date() & 
        Sys.time() > trunc.POSIXt(Sys.time(), units = "days") + 60 * 60 * 6.5) | 
       (!file.exists(file_date) & !file.exists(file_data))) {
        
        # download the df.gauging_data.RDS
        dir.create(paste0(path.expand('~'), "/.hyd1d"), FALSE, TRUE, "0777")
        utils::download.file(url, file_data, quiet = TRUE, method = "curl")
        
        # store todays date
        date_gauging_data <- Sys.Date()
        saveRDS(date_gauging_data, file = file_date)
        .db_updated <<- TRUE
        
        if (file.exists(file_data) & file.exists(file_date) & 
            (file.info(file_date)$mtime > Sys.time() - 5)) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    } else {
        return(FALSE)
    }
}
