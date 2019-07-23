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

write("waterLevels will be computed", stderr())

# load hyd1d
library(hyd1d)

# source hyd1d-internal to obtain the credentials function
source("R/hyd1d-internal.R")

# temporal sequence (last X days)
dates <- as.character(seq.Date(Sys.Date() - 8, Sys.Date() - 1, by = "1 day"))

# loop over all sections
for (i in 1:nrow(df.sections)) {
    
    # prepare the empty wldf
    river <- simpleCap(df.sections$river[i])
    section <- asc2utf8(df.sections$name[i])
    
    if (river == "Elbe") {
        dir <- "EL_000_586_UFD"
    } else if (river == "Rhein") {
        dir <- "RH_336_867_UFD"
    } else {
        write(paste0("The river '", river, "' does not exist."), stderr())
        next
    }
    
    # loop over all dates
    for (a_date in dates) {
        
        write(paste0(a_date, ": ", river, ": ", section), stdout())
        
        f_in <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/", dir,
                       "/data/wl/", section, "/km_values.txt")
        d_out <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/", dir,
                        "/data/wl/", section, "/", substr(a_date, 1, 4), "/")
        dir.create(d_out, FALSE, TRUE)
        
        f_out <- paste0(d_out, gsub("-", "", a_date, fixed = TRUE), ".txt")
        
        if (file.exists(f_out)) {
            write("  exists already", stdout())
        } else {
            write("  will be computed", stdout())
            # import stationing
            wldf <- readWaterLevelStationInt(file = f_in, river = river, 
                                             time = as.POSIXct(a_date))
            # compute the water level
            wldf <- waterLevel(wldf)
            # and export it
            writeWaterLevelJson(wldf, file = f_out, overwrite = TRUE)
        }
    }
}

# exit R
q("no")
