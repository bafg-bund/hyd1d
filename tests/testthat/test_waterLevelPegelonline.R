library(testthat)
library(hyd1d)

context("waterLevelPegelonline")

test_that("waterLevelPegelonline: Dessau", {
    w_old <- WaterLevelDataFrame(river = "Elbe",
                                 time = as.POSIXct("2016-12-21"),
                                 station = seq(257, 262, by = 0.1))
    expect_error(wldf1 <- waterLevelPegelonline(w_old, shiny = TRUE), 
                 "days in the past. Please adjust the 'time'-slot ", 
                 fixed = TRUE)
    
    skip_on_cran()
    wldf <- WaterLevelDataFrame(river = "Elbe", time = Sys.time() - 3600,
                                station = seq(257, 262, by = 0.1))
    wldf1 <- waterLevelPegelonline(wldf, shiny = TRUE)
    expect_equal(names(wldf1), c("station", "station_int", "w", "section", 
                                "weight_x", "weight_y"))
    expect_equal(wldf$station, wldf1$station)
    expect_equal(wldf$station_int, wldf1$station_int)
})

test_that("waterLevelPegelonline: Schöna", {
    skip_on_cran()
    
    wldf <- WaterLevelDataFrame(river = "Elbe", time = Sys.time() - 3600,
                                station_int = 
                                    as.integer(seq(0, 20000, by = 100)))
    wldf1 <- waterLevelPegelonline(wldf, shiny = TRUE)
})

test_that("waterLevelPegelonline: Geesthacht", {
    skip_on_cran()
    
    wldf <- WaterLevelDataFrame(river = "Elbe", time = Sys.time() - 3600,
                                station = seq(580, 585.7, by = 0.1))
    wldf1 <- waterLevelPegelonline(wldf, shiny = TRUE)
})

