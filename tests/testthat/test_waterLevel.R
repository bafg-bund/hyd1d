library(testthat)
library(hyd1d)

context("waterLevel")

test_that("waterLevel: Dessau", {
    wldf <- WaterLevelDataFrame(river = "Elbe", time = as.POSIXct("2016-12-21"),
                                station = seq(257, 262, by = 0.1))
    wldf1 <- waterLevel(wldf, shiny = TRUE)
    
    expect_equal(names(wldf), c("station", "station_int", "w"))
    expect_equal(names(wldf1), c("station", "station_int", "w", "section", 
                                "weight_x", "weight_y"))
    expect_equal(wldf$station, wldf1$station)
    expect_equal(wldf$station_int, wldf1$station_int)
    expect_equal(order(wldf1$station), order(- wldf1$w), 
                 label = "inversed order between station and w")
    
    if (Sys.info()["nodename"] == "hpc-service") {
        wldf2 <- readWaterLevelFileDB(river = getRiver(wldf),
                                      time = getTime(wldf),
                                      from = 257, to = 262)
        expect_equal(wldf1$station, wldf2$station)
        expect_equal(wldf1$station_int, wldf2$station_int)
        diff <- wldf1$w - wldf2$w
        expect_equal(min(diff), -0.01, 
                     label = "minimum difference: computed wl <> stored  wl")
        expect_equal(max(diff), 0.01, 
                     label = "maximum difference: computed wl <> stored  wl")
    }
})


test_that("waterLevel: Geesthacht", {
    wldf <- WaterLevelDataFrame(river = "Elbe", time = as.POSIXct("2016-12-21"),
                                station = seq(570, 585.7, by = 0.1))
    wldf1 <- waterLevel(wldf, shiny = TRUE)
    
    expect_equal(wldf$station, wldf1$station)
    expect_equal(wldf$station_int, wldf1$station_int)
    # due to the small/no slope this test will fail most of the time
    #expect_equal(order(wldf1$station), order(- wldf1$w), 
    #             label = "inversed order between station and w")
    
    if (Sys.info()["nodename"] == "hpc-service") {
        wldf2 <- readWaterLevelFileDB(river = getRiver(wldf),
                                      time = getTime(wldf),
                                      from = 570, to = 585.7)
        expect_equal(wldf1$station, wldf2$station)
        expect_equal(wldf1$station_int, wldf2$station_int)
        diff <- wldf1$w - wldf2$w
        expect_equal(min(diff), -0.01, 
                     label = "minimum difference: computed wl <> stored  wl")
        expect_equal(max(diff), 0.01, 
                     label = "maximum difference: computed wl <> stored  wl")
    }
})


test_that("waterLevel: Schöna", {
    wldf <- WaterLevelDataFrame(river = "Elbe", time = as.POSIXct("2016-12-21"),
                                station_int = as.integer(seq(0, 20000, by = 100)))
    wldf1 <- waterLevel(wldf, shiny = TRUE)
    
    expect_equal(names(wldf), c("station", "station_int", "w"))
    expect_equal(names(wldf1), c("station", "station_int", "w", "section", 
                                 "weight_x", "weight_y"))
    expect_equal(wldf$station, wldf1$station)
    expect_equal(wldf$station_int, wldf1$station_int)
    expect_equal(order(wldf1$station), order(- wldf1$w), 
                 label = "inversed order between station and w")
    
    if (Sys.info()["nodename"] == "hpc-service") {
        wldf2 <- readWaterLevelFileDB(river = getRiver(wldf),
                                      time = getTime(wldf),
                                      from = 0, to = 20)
        expect_equal(wldf1$station, wldf2$station)
        expect_equal(wldf1$station_int, wldf2$station_int)
        diff <- wldf1$w - wldf2$w
        expect_equal(min(diff), -0.01, 
                     label = "minimum difference: computed wl <> stored  wl")
        expect_equal(max(diff), 0.01, 
                     label = "maximum difference: computed wl <> stored  wl")
    }
})


test_that("waterLevel: Iffezheim", {
    if (Sys.info()["nodename"] == "hpc-service") {
        wldf <- readWaterLevelStationInt(file = "/home/WeberA/freigaben/U/U2/RH_336_867_UFD/data/wl/r001_IFFEZHEIM/km_values.txt",
                                         time = as.POSIXct("2016-12-21"))
        #id <- which(wldf$station > 336.2 & wldf$station <= 340)
        wldf1 <- subset(wldf, station > 336.2 & station <= 340)
        wldf2 <- waterLevel(wldf1, shiny = TRUE)
        
        expect_equal(wldf1$station, wldf2$station)
        expect_equal(wldf1$station_int, wldf2$station_int)
        expect_equal(order(wldf2$station), order(- wldf2$w), 
                     label = "inversed order between station and w")
        
        wldf3 <- readWaterLevelFileDB(river = getRiver(wldf),
                                      time = getTime(wldf),
                                      from = 336.2, to = 340)
        
        expect_equal(wldf2$station, wldf3$station)
        expect_equal(wldf2$station_int, wldf3$station_int)
        diff <- wldf2$w - wldf3$w
        expect_equal(min(diff), -0.01, 
                     label = "minimum difference: computed wl <> stored  wl")
        expect_equal(max(diff), 0.01, 
                     label = "maximum difference: computed wl <> stored  wl")
    }
})
 
