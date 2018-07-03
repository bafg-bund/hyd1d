library(testthat)
library(hyd1d)

context("Class WaterLevelDataFrame")

test_that("Initialisation of a WaterLevelDataFrame", {
    wldf <- WaterLevelDataFrame(river = "Elbe", time = as.POSIXct("2016-12-21"),
                                station = seq(256, 263, by = 0.1))
    
    expect_equal(class(wldf)[1], "WaterLevelDataFrame")
    expect_equal(wldf@.S3Class, "data.frame")
    expect_equal(class(wldf@.Data), "list")
    expect_named(wldf, c("station", "station_int", "w"))
    expect_equal(class(wldf$station), "numeric")
    expect_equal(class(wldf$station_int), "integer")
    expect_equal(class(wldf$w), "numeric")
    expect_equal(class(wldf@river), "character")
    expect_equal(class(wldf@time), c("POSIXct", "POSIXt"))
    expect_equal(class(wldf@gauging_stations), "data.frame")
    col_names <- c("id", "gauging_station", "uuid", "km", "km_qps", "river",
                   "longitude", "latitude", "mw", "mw_timespan", "pnp", "w",
                   "wl", "n_wls_below_w_do", "n_wls_above_w_do",
                   "n_wls_below_w_up", "n_wls_above_w_up", "name_wl_below_w_do",
                   "name_wl_above_w_do", "name_wl_below_w_up",
                   "name_wl_above_w_up", "w_wl_below_w_do", "w_wl_above_w_do",
                   "w_wl_below_w_up", "w_wl_above_w_up", "weight_up",
                   "weight_do")
    expect_named(wldf@gauging_stations, col_names)
    expect_equal(class(wldf@gauging_stations_missing), "character")
    expect_equal(wldf@gauging_stations_missing, as.character(NA))
    expect_equal(class(wldf@comment), "character")
    expect_equal(wldf$station, wldf$station_int/1000)
})


