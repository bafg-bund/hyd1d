library(testthat)
library(hyd1d)

context("waterLevelFlood2")

test_that("waterLevelFlood2: checks", {
    expect_error(wldf <- waterLevelFlood2("wldf"), 
                 "'wldf' must be type 'WaterLevelDataFrame'", fixed = TRUE)
    wldf <- WaterLevelDataFrame(river   = "Elbe",
                                time    = as.POSIXct(NA),
                                station = seq(257, 262, 0.1))
    expect_error(wldf <- waterLevelFlood2(wldf), 
                 "The time slot of 'wldf' must not be NA, if value is NULL.",
                 fixed = TRUE)
    
    # value
    
})

test_that("waterLevelFlood2: Dessau", {
    wldf1 <- WaterLevelDataFrame(river   = "Elbe",
                                 time    = as.POSIXct("2016-12-21"),
                                 station = seq(257, 262, 0.1))
    wldf2 <- WaterLevelDataFrame(river   = "Elbe",
                                 time    = as.POSIXct("2016-12-22"),
                                 station = seq(257, 262, 0.1))
    wldf1 <- waterLevelFlood2(wldf1)
    wldf2 <- waterLevelFlood2(wldf2)
    
    expect_equal(wldf1$station, wldf2$station)
    expect_equal(wldf1$station_int, wldf2$station_int)
})

test_that("waterLevelFlood2: Schöna", {
    wldf1 <- WaterLevelDataFrame(river       = "Elbe",
                                 time        = as.POSIXct("2016-12-21"),
                                 station_int = as.integer(seq(0, 25000, 100)))
    expect_silent(wldf1 <- waterLevelFlood2(wldf1))
    
    wldf2 <- WaterLevelDataFrame(river       = "Elbe",
                                 time        = as.POSIXct("2016-12-21"),
                                 station_int = as.integer(seq(0, 1800, 100)))
    expect_silent(wldf2 <- waterLevelFlood2(wldf2))
})

test_that("waterLevelFlood2: Geesthacht", {
    wldf1 <- WaterLevelDataFrame(river       = "Elbe",
                                 time        = as.POSIXct("2016-12-21"),
                                 station_int = as.integer(seq(580000, 585700, 100)))
    expect_silent(wldf1 <- waterLevelFlood2(wldf1))
    
    wldf2 <- WaterLevelDataFrame(river       = "Elbe",
                                 time        = as.POSIXct("2016-12-21"),
                                 station_int = as.integer(seq(585400, 585700, 100)))
    expect_silent(wldf2 <- waterLevelFlood2(wldf2))
})

test_that("waterLevelFlood2: Iffezheim", {
    wldf1 <- WaterLevelDataFrame(river       = "Rhine",
                                 time        = as.POSIXct("2016-12-21"),
                                 station_int = as.integer(seq(336200, 340000, 100)))
    expect_silent(wldf1 <- waterLevelFlood2(wldf1))
})

test_that("waterLevelFlood2: Koblenz", {
    wldf1 <- WaterLevelDataFrame(river       = "Rhine",
                                 time        = as.POSIXct("2016-12-21"),
                                 station_int = as.integer(seq(590000, 600000, 100)))
    expect_silent(wldf1 <- waterLevelFlood2(wldf1))
})

test_that("waterLevelFlood2: Grenze NL", {
    wldf1 <- WaterLevelDataFrame(river       = "Rhine",
                                 time        = as.POSIXct("2016-12-21"),
                                 station_int = as.integer(seq(865000, 865700, 100)))
    expect_silent(wldf1 <- waterLevelFlood2(wldf1))
    
    wldf2 <- WaterLevelDataFrame(river       = "Rhine",
                                 time        = as.POSIXct("2016-12-21"),
                                 station_int = as.integer(seq(860000, 865700, 100)))
    expect_silent(wldf2 <- waterLevelFlood2(wldf2))
})

test_that("waterLevelFlood2: tidal", {
    wldf1 <- WaterLevelDataFrame(river       = "Elbe_tidal",
                                 time        = as.POSIXct(NA),
                                 station_int = as.integer(seq(0, 19000, 1000)))
    expect_no_error(wldf1 <- waterLevelFlood2(wldf1, value = "MThw"))
    
    wldf1 <- WaterLevelDataFrame(river       = "Ems_tidal",
                                 time        = as.POSIXct(NA),
                                 station_int = as.integer(seq(0, 15000, 1000)))
    expect_no_error(wldf1 <- waterLevelFlood2(wldf1, value = "MThw"))
    
    wldf1 <- WaterLevelDataFrame(river       = "Stoer_tidal",
                                 time        = as.POSIXct(NA),
                                 station_int = as.integer(seq(0, 44800, 200)))
    expect_nerror(wldf1 <- waterLevelFlood2(wldf1, value = "MThw"))
})

