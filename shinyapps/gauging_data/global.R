# load the necessary packages
library(shiny)
library(hyd1d)
library(sp)
library(plotrix)

# access and query the gauging_data DB
df.gd <- readRDS("~/.hyd1d/df.gauging_data_latest.RDS")
df.gd$gauging_station <- hyd1d:::asc2utf8(df.gd$gauging_station)
df.gsd <- df.gauging_station_data[df.gauging_station_data$data_present, ]
df.gsd$gauging_station <- hyd1d:::asc2utf8(df.gsd$gauging_station)

# rivers
rivers <- unique(df.gsd$river)

# spatial gauging station data
spdf.gsd <- SpatialPointsDataFrame(coords = df.gsd[,c("longitude", "latitude")],
    data = df.gsd,
    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# yesterday
yesterday <- Sys.Date() - 1
