library(testthat)
library(hyd1d)

context("get...W")


test_that("getGaugingDataW", {
    times <- c(as.Date('1991-12-16'), as.Date('2016-12-21'))
    expect_equal(length(getGaugingDataW(gauging_station = "DESSAU",
                                        time = times)), 2)
    expect_equal(getGaugingDataW(gauging_station = "DESSAU", time = times),
                 getGaugingDataW(uuid = "1edc5fa4-88af-47f5-95a4-0e77a06fe8b1", 
                                 time = times))
    expect_equal(getGaugingDataW(gauging_station = "DESSAU", time = times),
                 getGaugingDataW(gauging_station = "DESSAU", 
                                 uuid = "1edc5fa4-88af-47f5-95a4-0e77a06fe8b1", 
                                 time = times))
    expect_error(getGaugingDataW(gauging_station = "DESSAU", 
                                 uuid = "7cb7461b-3530-4c01-8978-7f676b8f71ed", 
                                 time = times), 
                 "'gauging_station' and 'uuid' must fit to each other", 
                 fixed = TRUE)
    expect_error(getGaugingDataW(gauging_station = "Dessau",
                                 time = times), 
                 "'gauging_station' must be an element of c('SCHÖNA'", 
                 fixed = TRUE)
    expect_error(getGaugingDataW(uuid = "Dessau",
                                 time = times), 
                 "'uuid' must be an element of c('7cb7461b-3530-4c01-8978-", 
                 fixed = TRUE)
    expect_error(getGaugingDataW(gauging_station = "DESSAU",
                                 time = as.Date("1989-12-31")), 
                 "You requested earlier data. Please ")
    expect_error(getGaugingDataW(gauging_station = "DESSAU",
                                 time = Sys.time() + 3600), 
                 "You requested data of today.")
    expect_equal(getGaugingDataW(gauging_station = "DESSAU",
                                 time = as.Date("2016-12-21")), 165)
    
    # test for Umlaut in gauging_station
    expect_equal(getGaugingDataW("SCHÖNA", as.Date("2016-12-21")), 125)
})


test_that("getPegelonlineW", {
    times <- c(Sys.time() - 3600*4, Sys.time() - 3600*3, Sys.time() - 3600*2, Sys.time() - 3600)
    expect_equal(length(getPegelonlineW(gauging_station = "DESSAU",
                                        time = times)), 4)
    expect_equal(getPegelonlineW(gauging_station = "DESSAU", time = times),
                 getPegelonlineW(uuid = "1edc5fa4-88af-47f5-95a4-0e77a06fe8b1", 
                                 time = times))
    expect_equal(getPegelonlineW(gauging_station = "DESSAU", time = times),
                 getPegelonlineW(gauging_station = "DESSAU", 
                                 uuid = "1edc5fa4-88af-47f5-95a4-0e77a06fe8b1", 
                                 time = times))
    expect_error(getPegelonlineW(gauging_station = "DESSAU", 
                                 uuid = "7cb7461b-3530-4c01-8978-7f676b8f71ed", 
                                 time = times), 
                 "'gauging_station' and 'uuid' must fit to each other", 
                 fixed = TRUE)
    expect_error(getPegelonlineW(gauging_station = "Dessau",
                                 time = times), 
                 "'gauging_station' must be an element of c('SCHÖNA'", 
                 fixed = TRUE)
    expect_error(getPegelonlineW(uuid = "Dessau",
                                 time = times), 
                 "'uuid' must be an element of c('7cb7461b-3530-4c01-8978-", 
                 fixed = TRUE)
    expect_error(getPegelonlineW(gauging_station = "DESSAU",
                                 time = Sys.time() - 60*60*24*40), 
                 "days in the past and out of the allowed range")
    expect_error(getPegelonlineW(gauging_station = "DESSAU",
                                  time = Sys.time() + 3600), 
                 "which is in the future and out of the allowed range")
    expect_error(getPegelonlineW(gauging_station = "DESSAU",
                                 time = Sys.Date() - 40), 
                 "days in the past and out of the allowed range")
    expect_error(getPegelonlineW(gauging_station = "DESSAU",
                                 time = Sys.Date()), 
                 "which is today or in the future and thereby out")
    
    # test for Umlaut in gauging_station
    expect_equal(is.na(getPegelonlineW("SCHÖNA", Sys.Date() - 10)), FALSE)
    
})

