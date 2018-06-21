#' @name updateGaugingData
#' @rdname updateGaugingData
#'
#' @title Update package internal gauging data
#'
#' @description Function to overwrite and update the internal dataset
#'   \code{\link{df.gauging_data}}. This function is usually called during the
#'   initial loading of the package. If \code{\link{df.gauging_data}} was
#'   updated more than 1 day ago, an updated version of
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
updateGaugingData <- function(x){
    
    #####
    # assemble internal variables and check the existence of required data
    ##
    if (missing(x)) {
        stop("The 'x' argument has to be supplied.")
    }
    if (class(x) != "Date"){
        stop("'x' must be type 'Date'.")
    }
    if (length(x) != 1){
        stop("'x' must have length 1.")
    }
    
    p_source <- find.package("hyd1d")
    file_date <- paste0(p_source, "/data/date_gauging_data.rda")
    file_data <- paste0(p_source, "/data/df.gauging_data_latest.rda")
    
    if(x <= Sys.Date()){
        
        # download the df.gauging_data.rda
        url <- paste0("https://www.aqualogy.de/wp-content/uploads",
                      "/bfg/df.gauging_data_latest.rda")
        utils::download.file(url, file_data, quiet = TRUE)
        
        # store yesterdays date
        date_gauging_data <- Sys.Date()
        save(date_gauging_data, file = file_date)
        
        if (file.exists(file_data) & file.exists(file_date) & 
            (file.info(file_date)$mtime > Sys.time() - 5)){
            return(TRUE)
        } else {
            return(FALSE)
        }
    } else {
        return(FALSE)
    }
}
