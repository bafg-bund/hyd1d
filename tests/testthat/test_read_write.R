library(testthat)
library(hyd1d)

context("read & write")

test_that("readWaterLevelFileDB", {
    if (Sys.info()["nodename"] == "r.bafg.de") {
        wldf <- readWaterLevelFileDB(river = "Elbe",
                                     time = as.POSIXct("2016-12-21"),
                                     from = 257, to = 262)
        expect_equal(class(wldf)[1], "WaterLevelDataFrame")
        expect_equal(is.na(getGaugingStationsMissing(wldf)), TRUE)
        expect_error(wldf <- readWaterLevelFileDB(river = "ELBE",
                                                  time = as.POSIXct("2016-12-21"),
                                                  from = 257, to = 262),
                     " 'river' must be an element of c('Elbe', 'Rhein').",
                     fixed = TRUE)
        expect_error(wldf <- readWaterLevelFileDB(river = "Elbe",
                                                  time = as.POSIXct("1989-12-21"),
                                                  from = 257, to = 262),
                     "'time' must be between 1990-01-01 00:00:00 and now.",
                     fixed = TRUE)
        expect_error(wldf <- readWaterLevelFileDB(river = "Elbe",
                                                  time = as.POSIXct("2016-12-21"),
                                                  from = 262, to = 257),
                     "'to' must be above 'from', since stationing increases",
                     fixed = TRUE)
        expect_error(wldf <- readWaterLevelFileDB(river = "Elbe",
                                                  time = as.POSIXct("2016-12-21"),
                                                  from = 262, to = 800),
                     "'to' must be below km 585.7 for river 'Elbe'", fixed = TRUE)
        expect_error(wldf <- readWaterLevelFileDB(river = "Elbe",
                                                  time = as.POSIXct("2016-12-21"),
                                                  from = 262, to = as.integer(262)),
                     "class(from) must be equal to class(to)", fixed = TRUE)
    }
})

test_that("readWaterLevelJson", {
    if (Sys.info()["nodename"] == "r.bafg.de") {
        file <- "/home/WeberA/ShinyApps/05-waterlevel/www/downloads/elbe/e020_DESSAU/2016/20161221.txt"
        file1 <- "/home/WeberA/ShinyApps/05-waterlevel/www/downloads/elbe/e020_DESSAU/201/20161221.txt"
        wldf <- readWaterLevelJson(file)
        expect_equal(class(wldf)[1], "WaterLevelDataFrame")
        expect_equal(is.na(getGaugingStationsMissing(wldf)), TRUE)
        expect_error(wldf <- readWaterLevelJson(file1),
                     "The file does not exist. Please supply an existing file.",
                     fixed = TRUE)
        expect_error(wldf <- readWaterLevelJson(file, "Rhein"),
                     "from 'file' (Elbe) and the 'river' argument (Rhein)",
                     fixed = TRUE)
        expect_error(wldf <- readWaterLevelJson(file, "Elbe", 
                                                as.POSIXct("2016-12-20")),
                     " 'file' (2016-12-21) and the 'time' argument (2016-12-20)",
                     fixed = TRUE)
        expect_error(wldf <- readWaterLevelJson(file, "Elbe", 
                                                as.Date("2016-12-21")),
                     " 'time' must be type c('POSIXct', 'POSIXt')",
                     fixed = TRUE)
    }
})

test_that("readWaterLevelStationInt", {
    if (Sys.info()["nodename"] == "r.bafg.de") {
        file <- "/home/WeberA/ShinyApps/05-waterlevel/www/downloads/elbe/e020_DESSAU/km_values.txt"
        file1 <- "/home/WeberA/ShinyApps/05-waterlevel/www/downloads/elbe/e020_DESSAU/kmvalues.txt"
        wldf <- readWaterLevelStationInt(file)
        expect_equal(class(wldf)[1], "WaterLevelDataFrame")
        expect_equal(is.na(getGaugingStationsMissing(wldf)), TRUE)
        expect_error(wldf <- readWaterLevelStationInt(file1),
                     "The file does not exist. Please supply an existing file.",
                     fixed = TRUE)
        expect_error(wldf <- readWaterLevelStationInt(file, "Rhein"),
                     "from 'file' (Elbe) and the 'river' argument (Rhein)",
                     fixed = TRUE)
        expect_error(wldf <- readWaterLevelStationInt(file, "Elbe", 
                                                      as.Date("2016-12-21")),
                     " 'time' must be type c('POSIXct', 'POSIXt')",
                     fixed = TRUE)
        expect_error(wldf <- readWaterLevelStationInt(file, "Elbe", 
                                                      as.POSIXct("1989-12-20")),
                     "'time' must be between 1990-01-01 and now or",
                     fixed = TRUE)
    }
})

test_that("writeWaterLevelJson", {
    wldf <- WaterLevelDataFrame(river   = "Elbe",
                                time    = as.POSIXct("2016-12-21"),
                                station = seq(257, 262, 0.1))
    wldf <- waterLevel(wldf)
    file <- tempfile()
    writeWaterLevelJson(wldf, file = file, overwrite = TRUE)
    expect_equal(file.exists(file), TRUE)
    wldf1 <- readWaterLevelJson(file = file, river = "Elbe", 
                                time = as.POSIXct("2016-12-21"))
    expect_equal(wldf$w, wldf1$w)
    expect_error(writeWaterLevelJson(wldf, file = file),
                 " 'file' already exists and is not supposed to be overwri", 
                 fixed = TRUE)
    expect_error(writeWaterLevelJson(file),
                 "wldf' must be type 'WaterLevelDataFrame", 
                 fixed = TRUE)
    expect_error(writeWaterLevelJson(file),
                 "e 'file' argument must be suppl", 
                 fixed = TRUE)
})

test_that("writeWaterLevelStationInt", {
    file1 <- tempfile()
    file2 <- tempfile()
    wldf1 <- WaterLevelDataFrame(river   = "Elbe",
                                 time    = as.POSIXct("2016-12-21"),
                                 station = seq(257, 262, 0.1))
    wldf2 <- WaterLevelDataFrame(river   = "Elbe",
                                 time    = as.POSIXct("2016-12-21"),
                                 station_int = as.integer(seq(262100, 265000, 
                                                              100)))
    wldf3 <- rbind(wldf1, wldf2)
    writeWaterLevelStationInt(wldf1, file1)
    expect_equal(file.exists(file1), TRUE)
    writeWaterLevelStationInt(wldf2, file1, FALSE, TRUE)
    expect_equal(file.exists(file1), TRUE)
    expect_error(writeWaterLevelStationInt(wldf1, file1, FALSE),
                 " 'file' already exists and is not supposed to be overwri", 
                 fixed = TRUE)
    expect_error(writeWaterLevelStationInt(wldf2, file1, TRUE, TRUE),
                 "'overwrite' and 'append' are TRUE. Only one of", fixed = TRUE)
    wldf4 <- readWaterLevelStationInt(file1, river = "Elbe")
    expect_equal(wldf3$station, wldf4$station)
    expect_equal(wldf3$station_int, wldf4$station_int)
})

