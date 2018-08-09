##################################################
# daily_waterLevels.R
#
# author: arnd.weber@bafg.de
# date:   09.08.2018
#
# purpose: 
#   - compute daily water levels and export them file based in Z:
#
##################################################

# configure output
verbose <- TRUE
quiet <- !verbose

# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")

# check the time
hour <- as.numeric(strftime(Sys.time(), "%H"))

if (hour >= 6 & hour < 7) {
    
    write("waterLevels will be computed", stderr())
    
    # load hyd1d
    library(hyd1d, lib.loc = lib)
    
    # source hyd1d-internal to obtain the credentials function
    source("R/hyd1d-internal.R")
    
    # loop over all sections
    for (i in 1:nrow(df.sections)) {
        
        # create the empty wldf
        river <- simpleCap(df.sections$river[i])
        section <- asc2utf8(df.sections$name[i])
        time <- as.POSIXct(as.character(Sys.Date() - 2))
        if (river == "Elbe") {
            dir <- "EL_000_586_UFD"
        } else if (river == "Rhein") {
            dir <- "RH_336_867_UFD"
        } else {
            write(paste0("The river '", river, "' does not exist."), stderr())
            next
        }
        
        write(paste0(strftime(time, "%Y-%m-%d"), ": ", river, ": ", section), 
              stdout())
        
        f_in <- paste0("/home/WeberA/freigaben/U/U2/", dir, "/data/wl/",
                       section, "/km_values.txt")
        d_out <- paste0("/home/WeberA/freigaben/U/U2/", dir, "/data/wl/",
                        section, "/", strftime(time, "%Y"), "/")
        f_out <- paste0(d_out, strftime(time, "%Y%m%d"), ".txt")
        
        wldf <- readWaterLevelStationInt(file = f_in, river = river, 
                                         time = time)
        
        # compute the water level
        wldf <- waterLevel(wldf)
        
        # and export it
        dir.create(d_out, FALSE, TRUE)
        writeWaterLevelJson(wldf, file = f_out, overwrite = TRUE)
    }
    
} else {
    write("It's not the time to compute waterLevels", stderr())
}

# exit R
q("no")