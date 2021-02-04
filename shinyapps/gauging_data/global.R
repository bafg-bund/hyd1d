# load the necessary packages
library(shiny)
library(shinyjs)
library(shiny.i18n)
library(hyd1d)
library(sp)
library(plotrix)
library(lubridate)

# access and query the gauging_data DB
df.gd <- readRDS("~/.hyd1d/df.gauging_data_latest.RDS")
df.gsd <- df.gauging_station_data[df.gauging_station_data$data_present, ]

# rivers
rivers <- unique(df.gsd$river)

# spatial gauging station data
spdf.gsd <- SpatialPointsDataFrame(coords = df.gsd[,c("longitude", "latitude")],
    data = df.gsd,
    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# yesterday
yesterday <- Sys.Date() - 1

# translation
translator <- Translator$new(translation_json_path = "translation.json")

# JavaScript to determine browser language
jscode <- paste0("var language =  window.navigator.userLanguage || window.navi",
                 "gator.language;Shiny.onInputChange('lang', language);console",
                 ".log(language);")
de <- function(x) {
    if (is.null(x)) {return(FALSE)}
    if (startsWith(x, "de")) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

