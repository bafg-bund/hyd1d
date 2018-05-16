#' @name waterLevelFlys3
#' @rdname waterLevelFlys3
#' @aliases waterLevelFlys3
#' 
#' @title Obtain 1D water level data from the FLYS3 database
#' 
#' @description Obtain 1D water level data from the FLYS3 database using either 
#'   a predefined \linkS4class{WaterLevelDataFrame} or \code{river}, \code{from} 
#'   and \code{to} arguments that enable the internal construction of a 
#'   \linkS4class{WaterLevelDataFrame}. The internally constructed 
#'   \linkS4class{WaterLevelDataFrame} contains stations every 0.1 km or 100 m 
#'   between the given range of \code{from} and \code{to}.
#' 
#' @param wldf an object of class \linkS4class{WaterLevelDataFrame}.
#' @param river a required argument to fill the \code{WaterLevelDataFrame}-slot
#'   \code{river}. It has to be type \code{character}, has to have a length of
#'   one and can be either \strong{Elbe} or \strong{Rhein}.
#' @param name a string with the name of a stationary FLYS3 water level. It has 
#'   to be type \code{character}, has to have a length of
#'   one and has to be an element of the \code{river}-specific names specified 
#'   in Details.
#' @param from \code{numeric} or \code{integer} for the upstream station. It
#'   has to have a length of one and has to be within the \code{river}-specific
#'   possible station range specified in Details.
#' @param to \code{numeric} or \code{integer} for the dowstream station. It
#'   has to have the same type as \code{from}, a length of one and has to be 
#'   within the \code{river}-specific possible station range specified in 
#'   Details.
#' 
#' @return An object of class \linkS4class{WaterLevelDataFrame}.
#' 
#' @details Possible \code{name}s of FLYS3 water levels and ranges of 
#'   \code{from} and \code{to} are \code{river}-specific:
#'   
#'   \strong{Elbe:}
#'    
#'   \code{name}: "0.5MNQ", "MNQ", "0.5MQ", "a", "0.75MQ", "b", "MQ", "c", 
#'   "2MQ", "3MQ", "d", "e", "MHQ", "HQ2", "f", "HQ5", "g", "h", "HQ10", "HQ15",
#'   "HQ20", "HQ25", "HQ50", "HQ75", "HQ100", "i", "HQ150", "HQ200", "HQ300", 
#'   "HQ500"
#'   
#'   Possible range of \code{from} and \code{to}: type \code{numeric} (km) 0 
#'   - 585.7, type \code{integer} (m) 0 - 585700
#'   
#'   \strong{Rhein:}
#'   
#'   \code{name}: "Ud=1", "Ud=5", "GlQ2012", "Ud=50", "Ud=80", "Ud=100", 
#'   "Ud=120", "Ud=183", "MQ", "Ud=240","Ud=270", "Ud=310", "Ud=340", "Ud=356", 
#'   "Ud=360", "MHQ", "HQ2", "HQ5", "HQ5-10", "HQ10", "HQ10-20", "~HQ20",
#'   "HQ20-50", "HQ50", "HQ50-100", "HQ100", "HQ100-200", "HQ200", "HQ200-ex", 
#'   "HQextr."
#'   
#'   Possible range of \code{from} and \code{to}: type \code{numeric} (km) 
#'   336.2 - 865.7, type \code{integer} (m) 336200 - 865700.
#'   
#' @seealso \code{\link{plotShiny}}
#' 
#' @references
#'   \insertRef{busch_einheitliche_2009}{hyd1d}
#'   
#'   \insertRef{bundesanstalt_fur_gewasserkunde_flys_2016}{hyd1d}
#' 
#' @examples 
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf1 <- waterLevelFlys3(wldf, "MQ")
#' 
#' wldf2 <- waterLevelFlys3Seq("Elbe", "MQ", 257, 262)
#' 
#' @export
#' 
waterLevelFlys3 <- function(wldf, name){
    
    ##########
    # check arguments
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
        ## name
        # presence
        if (missing(name)){
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'name' has to be supplied."))
        } else {
            # character
            if (class(name) != "character"){
                errors <- c(errors, paste0("Error ", l(errors),
                                           ": 'name' must be type 'character'."))
            }
            # length
            if (length(name) != 1L){
                errors <- c(errors, paste0("Error ", l(errors),
                                           ": 'name' must have length 1."))
            }
            # %in% flys3_water_levels
            if (getRiver(wldf) == "Elbe"){
                flys3_water_levels <- c("0.5MNQ", "MNQ", "0.5MQ", "a", "0.75MQ",
                                        "b", "MQ", "c", "2MQ", "3MQ", "d", "e",
                                        "MHQ", "HQ2", "f", "HQ5", "g", "h",
                                        "HQ10", "HQ15", "HQ20", "HQ25", "HQ50",
                                        "HQ75", "HQ100", "i", "HQ150", "HQ200",
                                        "HQ300", "HQ500")
            }
            if (getRiver(wldf) == "Rhein"){
                flys3_water_levels <- c("Ud=1", "Ud=5", "GlQ2012", "Ud=50",
                                        "Ud=80", "Ud=100", "Ud=120", "Ud=183",
                                        "MQ", "Ud=240","Ud=270", "Ud=310",
                                        "Ud=340", "Ud=356", "Ud=360", "MHQ",
                                        "HQ2", "HQ5", "HQ5-10", "HQ10",
                                        "HQ10-20", "~HQ20","HQ20-50", "HQ50",
                                        "HQ50-100", "HQ100", "HQ100-200",
                                        "HQ200", "HQ200-ex", "HQextr.")
            }
            if (!(name %in% flys3_water_levels)){
                errors <- c(errors, paste0("Error ", l(errors),
                                           ": 'name' must be an element ",
                                           "of c('",
                                           paste0(flys3_water_levels,
                                                  collapse="', '"),
                                           "'). You requested name = '", name,
                                           "'."))
            }
        }
    }
    
    if (l(errors) != "1"){
        stop(paste0(errors, collapse="\n  "))
    }
    
    ##########
    # processing
    #####
    # make parent environment accessible through the local environment
    e <- environment()
    p_env <- parent.env(e)
    
    # access the FLYS3 data
    if (exists("df.flys_data", where = p_env)){
        get("df.flys_data", envir = p_env)
    } else {
        print("data")
        utils::data("df.flys_data")
    }
    
    # select the water level for a specified river and name
    id <- which(df.flys_data$river == getRiver(wldf) &
                df.flys_data$name == name)
    df.flys_data_sel <- df.flys_data[id,]
    
    # identify the relevant river stretch
    id <- which(df.flys_data_sel$station >= min(wldf$station) &
                df.flys_data_sel$station <= max(wldf$station))
    df.wl_left <- df.flys_data_sel[min(id), ]
    df.wl_right <- df.flys_data_sel[max(id), ]
    id <- c(min(id) - 1, id, max(id) + 1)
    df.wl <- stats::na.omit(df.flys_data_sel[id, ])
    
    #####
    # interpolate
    df.data <- stats::approx(x = df.wl$station, y = df.wl$w,
                             xout = wldf$station, method = "linear",
                             yleft = df.wl_left$w, yright = df.wl_right$w,
                             rule = c(2, 2), ties = "ordered")
    
    ##########
    # initialize the resulting WaterLevelDataFrame and return it
    wldf <- WaterLevelDataFrame(river = getRiver(wldf),
                                time = as.POSIXct(NA),
                                gauging_stations_missing = as.character(NA),
                                comment = paste0("Computed by ", 
                                                 "waterLevelFlys3(): ", name),
                                station = df.data$x,
                                w = df.data$y)
    
    return(wldf)
}


