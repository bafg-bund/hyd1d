.pkgenv <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
    
    # load package data
    utils::data("df.flys", "df.flys_sections", "df.gauging_station_data",
                "df.gauging_data", package = pkgname,
                envir = parent.env(environment()))
    
    # add a default hyd1d.datadir to options
    if (!("hyd1d.datadir" %in% names(options()))) {
        d <- Sys.getenv("hyd1d_datadir")
        if (d != "") {
            if (!dir.exists(d)) {
                options("hyd1d.datadir" = tempdir())
            } else {
                options("hyd1d.datadir" = d)
            }
        } else {
            options("hyd1d.datadir" = tempdir())
        }
    } else {
        if (!dir.exists(options()$hyd1d.datadir)) {
            tryCatch(
                dir.create(options()$hyd1d.datadir, TRUE, TRUE, "0700"),
                error = function(e){
                    t <- tempdir()
                    msg <- paste0("It was not possible to create:",
                                  options()$hyd1d.datadir, "\n", t,
                                  " is used instead!")
                    .pkgenv[["msg"]] <- msg
                    options("hyd1d.datadir" = t)
                }
            )
        }
    }
    
    # path to dataset and 
    file_data <- paste0(options()$hyd1d.datadir, "/df.gauging_data_latest.RDS")
    updateGaugingData(file_data)
    
    # load df.gauging_data into .pkgenv
    .pkgenv[[".df.gauging_data"]] <- readRDS(file_data)
}

.onAttach <- function(libname, pkgname) {
    file_data <- paste0(options()$hyd1d.datadir, "/df.gauging_data_latest.RDS")
    file_mtime <- format(file.info(file_data)$mtime, "%Y-%m-%d %H:%M:%S %Z")
    if (exists("msg", envir = .pkgenv)) {
        msg <- paste0(.pkgenv$msg, "\n")
    } else {
        msg <- ""
    }
    msg <- paste0(msg, "'df.gauging_data' locally stored at\n", file_data, "\n",
                  "was last downloaded ", file_mtime)
    packageStartupMessage(msg)
}
