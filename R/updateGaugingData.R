#' @name updateGaugingData
#' @rdname updateGaugingData
#'
#' @title Update package internal gauging data
#'
#' @description Function to update the internal dataset
#'   \code{\link{df.gauging_data}}. This function is usually called during the
#'   initial loading of the package. If \code{\link{df.gauging_data}} was
#'   updated more than 2 days ago, an updated version of
#'   \code{\link{date_gauging_data}} will be downloaded and used.
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
    
    date_min <- as.Date(trunc(Sys.time() - as.difftime(2, units = "days"),
                              units = "days"))
    p_source <- find.package("hyd1d")
    file_date <- paste0(p_source, "/data/date_gauging_data.rda")
    file_data <- paste0(p_source, "/data/df.gauging_data.rda")
    
    if(x < date_min){
        
        # download the df.gauging_data.rda
        url <- paste0("http://hpc-service.bafg.de/r-packages/hyd1d/data/",
                      "df.gauging_data_latest.rda")
        utils::download.file(url, file_data, quiet = TRUE)
        
        # store yesterdays date
        date_gauging_data <- Sys.Date() - 1
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
