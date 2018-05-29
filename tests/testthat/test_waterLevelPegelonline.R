library(testthat)
library(hyd1d)

context("waterLevelPegelonline")

test_that("waterLevelPegelonline: Dessau", {
    wldf <- WaterLevelDataFrame(river = "Elbe", time = Sys.time() - 3600,
                                station = seq(257, 262, by = 0.1))
    wldf1 <- waterLevelPegelonline(wldf, shiny = TRUE)
    
    expect_equal(names(wldf), c("station", "station_int", "w"))
    expect_equal(names(wldf1), c("station", "station_int", "w", "section", 
                                "weight_x", "weight_y"))
    expect_equal(wldf$station, wldf1$station)
    expect_equal(wldf$station_int, wldf1$station_int)
    
    wldf <- WaterLevelDataFrame(river = "Elbe", time = as.POSIXct("2016-12-21"),
                                station = seq(257, 262, by = 0.1))
    expect_error(wldf1 <- waterLevelPegelonline(wldf, shiny = TRUE), 
                 "days in the past. Please adjust the 'time'-slot ", 
                 fixed = TRUE)
})


test_that("waterLevelPegelonline: Geesthacht", {
    wldf <- WaterLevelDataFrame(river = "Elbe", time = Sys.time() - 3600,
                                station = seq(580, 585.7, by = 0.1))
    wldf1 <- waterLevelPegelonline(wldf, shiny = TRUE)
    
    expect_equal(wldf$station, wldf1$station)
    expect_equal(wldf$station_int, wldf1$station_int)
    # due to the small/no slope this test will fail most of the time
    #expect_equal(order(wldf1$station), order(- wldf1$w), 
    #             label = "inversed order between station and w")
    
})


test_that("waterLevelPegelonline: Iffezheim", {
    if (Sys.info()["nodename"] == "hpc-service") {
        wldf <- readWaterLevelStationInt(file = "/home/WeberA/freigaben/U/U2/RH_336_867_UFD/data/wl/r001_IFFEZHEIM/km_values.txt",
                                         time = as.POSIXct("2016-12-21"))
        wldf1 <- subset(wldf, station > 336.2 & station <= 340)
        wldf2 <- waterLevel(wldf1, shiny = TRUE)
        
        expect_equal(wldf1$station, wldf2$station)
        expect_equal(wldf1$station_int, wldf2$station_int)
        expect_equal(order(wldf2$station), order(- wldf2$w), 
                     label = "inversed order between station and w")
    }
})
