#' @name writeWaterLevelJson
#' @rdname writeWaterLevelJson
#' 
#' @title Export a WaterLevelDataFrame to a JSON-formated ascii file
#' 
#' @description Store water level data in an exchange format compatible with \R
#'   and Python.
#' 
#' @param wldf an object of type \linkS4class{WaterLevelDataFrame}.
#' @param file either a \code{character} string naming a file or a connection
#'   open for writing.
#' @param overwrite \code{logical} to determine whether an existing file should
#'   be overwritten.
#' 
#' @seealso \code{\link{readWaterLevelJson}}
#' 
#' @examples
#' 
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf <- waterLevel(wldf)
#' \dontrun{
#'     writeWaterLevelStationInt(wldf, file = "wldf.txt", overwrite = TRUE)
#' }
#' 
#' @export
#' 
writeWaterLevelJson <- function(wldf, file, overwrite = FALSE) {
    
    ## vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    ##### check arguments
    ## wldf
    # presence
    if (missing(wldf)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'wldf' argument",
                                   "must be supplied."))
    } else {
        if (!inherits(wldf, "WaterLevelDataFrame")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'wldf' must be ",
                                       "type 'WaterLevelDataFrame'."))
        } else {
            # nrow
            if (nrow(wldf) == 0) {
                errors <- c(errors, paste0("Error ", l(errors), ": nrow(wldf)",
                                           " must be above 0."))
            }
        }
    }
    
    ## file
    # presence
    if (missing(file)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'file' argument",
                                   " must be supplied."))
    } else {
        # character
        if (!inherits(file, "character")) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'file' must be type 'character'."))
        }
        # length
        if (length(file) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'file' must have a length equal 1."))
        }
        # check, if directory exists
        dir <- dirname(file)
        if (!(file.exists(dir))) {
            if (!(file.exists(paste(sep = .Platform$file.sep, getwd(), dir)))) {
                errors <- c(errors, paste0("Error ", l(errors), ": The direct",
                                           "ory (", dir, ") does not exist. ",
                                           "Please supply an existing ",
                                           "directory."))
            }
        }
        # check, if the file already exists
        if (file.exists(file) & overwrite == FALSE) {
            errors <- c(errors, paste0("Error ", l(errors), ": The 'file' ",
                                       "already exists and is not supposed to ",
                                       "be overwritten."))
        }
        expand_file <- FALSE
        if (file.exists(paste(sep = .Platform$file.sep, getwd(), file)) &
            overwrite == FALSE) {
            errors <- c(errors, paste0("Error ", l(errors), ": The 'file' ",
                                       "already exists and is not supposed to ",
                                       "be overwritten."))
        }
        if (file.exists(paste(sep = .Platform$file.sep, getwd(), file)) &
            overwrite) {
            expand_file <- TRUE
        }
    }
    
    ## overwrite
    # logical
    if (!inherits(overwrite, "logical")) {
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": 'overwrite' must be type 'logical'."))
    }
    # length
    if (length(overwrite) != 1L) {
        errors <- c(errors, paste0("Error ", l(errors), ": 'overwrite' must ",
                                   "have a length equal 1."))
    }
    
    #####
    # return
    if (l(errors) == "1") {
        # convert wldf to df
        df <- data.frame(id = as.character(as.numeric(row.names(wldf)) - 1), 
                         as.data.frame(wldf), stringsAsFactors = FALSE)
        df$w <- as.character(df$w)
        df$w[is.na(df$w)] <- "NaN"
        
        # convert the df to character vector with length 1
        string <- character()
        id_col <- c("id", "station_int", "w")
        for (a_row in 1:nrow(df)) {
            if (a_row == 1) {
                string <- c(string, paste0("[",
                                           paste0("[",
                                                  paste0(df[a_row, id_col],
                                                         collapse = ", "),
                                                  "],")))
            } else if (a_row == nrow(df)) {
                string <- c(string, paste0(paste0("[",
                                                  paste0(df[a_row, id_col],
                                                         collapse = ", "),
                                                  "]]")))
            } else {
                string <- c(string, paste0(paste0("[",
                                                  paste0(df[a_row, id_col],
                                                         collapse = ", "),
                                                  "],")))
            }
        }
        string <- paste0(string, collapse=" ")
        
        # write the converted df
        if (expand_file) {
            write(string, file = paste(sep = .Platform$file.sep, getwd(), file))
        } else {
            write(string, file = file)
        }
    } else {
        stop(paste0(errors, collapse="\n  "))
    }
}


#' @name writeWaterLevelStationInt
#' @rdname writeWaterLevelStationInt
#' @title Export station data ascii file
#'
#' @description Store station data in a file format exchangeable between \R and
#'   Python.
#'
#' @param wldf an object of type \linkS4class{WaterLevelDataFrame}.
#' @param file either a \code{character} string naming a file or a connection
#'   open for writing.
#' @param overwrite \code{logical} to determine whether an existing file should
#'   be overwritten.
#' @param append \code{logical} to determine whether station data should be
#'   appended to an existing file.
#'
#' @seealso \code{\link{readWaterLevelStationInt}}
#'
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf <- waterLevel(wldf)
#' \dontrun{
#'     writeWaterLevelStationInt(wldf, file = "station_int.txt",
#'                               overwrite = TRUE, append = FALSE)
#' }
#' 
#' @export
#' 
writeWaterLevelStationInt <- function(wldf, file, overwrite = FALSE, 
                                      append = FALSE) {
    
    ## vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    ##### check arguments
    ## wldf
    # presence
    if (missing(wldf)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'wldf' argument",
                                   "must be supplied."))
    } else {
        # WaterLevelDataFrame
        if (!inherits(wldf, "WaterLevelDataFrame")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'wldf' must be ",
                                       "type 'WaterLevelDataFrame'."))
        }
        # nrow
        if (nrow(wldf) == 0) {
            errors <- c(errors, paste0("Error ", l(errors), ": nrow(wldf) must",
                                       " be above 0."))
        }
    }
    
    
    ## file
    # presence
    if (missing(file)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'file' argument",
                                   "must be supplied."))
    } else {
        # character
        if (!inherits(file, "character")) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'file' must be type 'character'."))
        }
        # length
        if (length(file) != 1L) {
            errors <- c(errors, paste0("Error ", l(errors),
                                       ": 'file' must have a length equal 1."))
        }
        # check, if directory exists
        dir <- dirname(file)
        if (!(file.exists(dir))) {
            if (!(file.exists(paste(sep = .Platform$file.sep, getwd(), dir)))) {
                errors <- c(errors, paste0("Error ", l(errors),
                                           ": The directory (", dir,
                                           ") does not exist. Please supply ",
                                           "an existing directory."))
            }
        }
        # check, if the file already exists
        if (file.exists(file) & overwrite == FALSE & append == FALSE) {
            errors <- c(errors, paste0("Error ", l(errors), ": The 'file' alre",
                                       "ady exists and is not supposed to be ",
                                       "overwritten."))
        }
        expand_file <- FALSE
        if (file.exists(paste(sep = .Platform$file.sep, getwd(), file)) &
            overwrite == FALSE & append == FALSE) {
            errors <- c(errors, paste0("Error ", l(errors), ": The 'file' alre",
                                       "ady exists and is not supposed to be ",
                                       "overwritten."))
        }
        if (file.exists(paste(sep = .Platform$file.sep, getwd(), file)) &
            (overwrite | append )) {
            expand_file <- TRUE
        }
    }
    
    ## overwrite
    # logical
    if (!inherits(overwrite, "logical")) {
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": 'overwrite' must be type 'logical'."))
    }
    # length
    if (length(overwrite) != 1L) {
        errors <- c(errors, paste0("Error ", l(errors), ": 'overwrite' must ",
                                   "have a length equal 1."))
    }
    
    ## append
    # logical
    if (!inherits(append, "logical")) {
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": 'append' must be type 'logical'."))
    }
    # length
    if (length(append) != 1L) {
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": 'append' must have a length equal 1."))
    }
    
    ## overwrite & append
    if (overwrite & append) {
        errors <- c(errors, paste0("Error ", l(errors),
                                   ": 'overwrite' and 'append' are TRUE. ",
                                   "Only one of them can be TRUE, since they ",
                                   "exclude each other."))
    }
    
    #####
    # return
    if (l(errors) == "1") {
        if (overwrite) {
            if (expand_file) {
                write(wldf$station_int,
                      file = paste(sep = .Platform$file.sep, getwd(), file),
                      ncolumns = 1,
                      append = FALSE)
            } else {
                write(wldf$station_int,
                      file = file,
                      ncolumns = 1,
                      append = FALSE)
            }
        } else {
            if (append) {
                if (expand_file) {
                    write(wldf$station_int,
                          file = paste(sep = .Platform$file.sep, getwd(), file),
                          ncolumns = 1,
                          append = TRUE)
                } else {
                    write(wldf$station_int,
                          file = file,
                          ncolumns = 1,
                          append = TRUE)
                }
            } else {
                if (expand_file) {
                    write(wldf$station_int,
                          file = paste(sep = .Platform$file.sep, getwd(), file),
                          ncolumns = 1,
                          append = FALSE)
                } else {
                    write(wldf$station_int,
                          file = file,
                          ncolumns = 1,
                          append = FALSE)
                }
            }
        }
        
        # add a trailing \n
        if (expand_file) {
            write("",
                  file = paste(sep = .Platform$file.sep, getwd(), file),
                  ncolumns = 1,
                  append = TRUE)
        } else {
            write("",
                  file = file,
                  ncolumns = 1,
                  append = TRUE)
        }
    } else {
        stop(paste0(errors, collapse="\n  "))
    }
}

