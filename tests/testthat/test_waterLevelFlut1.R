library(testthat)
library(hyd1d)

context("waterLevelFlut1")

test_that("waterLevelFlut1: Dessau", {
    t <- as.POSIXct("2016-12-21")
    wldf <- WaterLevelDataFrame(river   = "Elbe",
                                time    = t,
                                station = seq(257, 262, 0.1))
    wldf1 <- waterLevelFlut1(wldf, "ROSSLAU")
    wldf2 <- waterLevelFlut1(wldf, "DESSAU")
    
    expect_equal(wldf1$station, wldf2$station)
    expect_equal(wldf1$station_int, wldf2$station_int)
    #expect_equal(mean(wldf1$w - wldf2$w, na.rm = TRUE), -0.28)
    
    # errors waterLevelFlut1
    expect_error(wldf3 <- waterLevelFlut1("wldf", "DESSAU"), 
                 "'wldf' must be type 'WaterLevelDataFrame'", fixed = TRUE)
    expect_error(wldf3 <- waterLevelFlut1(wldf, "Dessau", w = 180), 
                 "'gauging_station' must be an element of c('SCH", 
                 fixed = TRUE)
    expect_warning(wldf3 <- waterLevelFlut1(wldf, "DESSAU", w = 166), 
                   "'w' computed internally through getGauging", fixed = TRUE)
    
})
