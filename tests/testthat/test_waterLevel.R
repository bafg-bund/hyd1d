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
})

