library(testthat)
library(hyd1d)

context("waterLevelFlood1")

test_that("waterLevelFlood1: checks", {
    expect_error(wldf <- waterLevelFlood1("wldf"), 
                 "'wldf' must be type 'WaterLevelDataFrame'", fixed = TRUE)
    wldf <- WaterLevelDataFrame(river   = "Elbe",
                                time    = as.POSIXct(NA),
                                station = seq(257, 262, 0.1))
    expect_error(wldf <- waterLevelFlood1(wldf), 
                 "The time slot of 'wldf' must not be NA or 'w'", fixed = TRUE)
    setTime(wldf) <- as.POSIXct("2016-12-21")
    expect_error(wldf <- waterLevelFlood1(wldf), 
                 "The 'gauging_station' or 'uuid' argument has ", fixed = TRUE)

    expect_error(wldf <- waterLevelFlood1(wldf, gauging_station = 1), 
                 "'gauging_station' must be type 'character'", fixed = TRUE)
    expect_error(wldf <- waterLevelFlood1(wldf, gauging_station = c("a", "b")), 
                 "'gauging_station' must have length 1", fixed = TRUE)
    expect_error(wldf <- waterLevelFlood1(wldf, gauging_station = "a"), 
                 "'gauging_station' must be an element of c('", fixed = TRUE)
    expect_error(wldf <- waterLevelFlood1(wldf, gauging_station = "LENZEN"), 
                 "'gauging_station' has to be in the river stre", fixed = TRUE)
    
    expect_error(wldf <- waterLevelFlood1(wldf, uuid = 1), 
                 "'uuid' must be type 'character'", fixed = TRUE)
    expect_error(wldf <- waterLevelFlood1(wldf, uuid = c("a", "b")), 
                 "'uuid' must have length 1", fixed = TRUE)
    expect_error(wldf <- waterLevelFlood1(wldf, uuid = "a"), 
                 "'uuid' must be an element of c('", fixed = TRUE)
    expect_error(wldf <- waterLevelFlood1(wldf, 
                         uuid = "b04b739d-7ffa-41ee-9eb9-95cb1b4ef508"), 
                 "'uuid' has to be in the river stre", fixed = TRUE)
    
    expect_error(wldf <- waterLevelFlood1(wldf, gauging_station = "LENZEN",
                              uuid = "b04b739d-7ffa-41ee-9eb9-95cb1b4ef508"), 
                 "'gauging_station' and 'uuid' must fit to each", fixed = TRUE)
    
    expect_error(wldf <- waterLevelFlood1(wldf, gauging_station = "DESSAU",
                                          w = c(100, 120)), 
                 "'w' must have length 1", fixed = TRUE)
    expect_error(wldf <- waterLevelFlood1(wldf, gauging_station = "DESSAU",
                                          w = "a"),
                 "'w' must be type 'numeric'", fixed = TRUE)
    expect_error(wldf <- waterLevelFlood1(wldf, gauging_station = "DESSAU",
                                          w = 100000),
                 "'w' must be in a range between 0 and 1000", fixed = TRUE)
    
    expect_warning(wldf <- waterLevelFlood1(wldf, gauging_station = "DESSAU",
                                          w = 120),
                   "Since you specifically supplied 'w', the internally",
                   fixed = TRUE)
})    

test_that("waterLevelFlood1: Dessau", {
    wldf <- WaterLevelDataFrame(river   = "Elbe",
                                time    = as.POSIXct("2016-12-21"),
                                station = seq(257, 262, 0.1))
    wldf1 <- waterLevelFlood1(wldf, "ROSSLAU")
    wldf2 <- waterLevelFlood1(wldf, "DESSAU")
    
    expect_equal(wldf1$station, wldf2$station)
    expect_equal(wldf1$station_int, wldf2$station_int)
    expect_equal(mean(wldf1$w - wldf2$w, na.rm = TRUE), -0.28)
})

test_that("waterLevelFlood1: Dessau", {
    wldf <- WaterLevelDataFrame(river   = "Elbe",
                                time    = as.POSIXct("2016-12-21"),
                                station = seq(257, 262, 0.1))
    wldf1 <- waterLevelFlood1(wldf, "ROSSLAU")
    wldf2 <- waterLevelFlood1(wldf, "DESSAU")
    
    expect_equal(wldf1$station, wldf2$station)
    expect_equal(wldf1$station_int, wldf2$station_int)
    expect_equal(mean(wldf1$w - wldf2$w, na.rm = TRUE), -0.28)
})
