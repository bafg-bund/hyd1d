library(testthat)
library(hyd1d)

context("get & set")

test_that("river", {
    # Elbe
    wldf <- WaterLevelDataFrame(river = "Elbe", time = as.POSIXct("2016-12-21"),
                                station = seq(256, 263, by = 0.1))
    expect_equal(getRiver(wldf), "Elbe")
    expect_error(setRiver(wldf) <- "Rhein", regexp = "must be above km 336.2")
    expect_error(setRiver(wldf) <- "Rhein", regexp = "must be above 336200 (km", 
                 fixed = TRUE)
    
    # Rhein
    wldf <- WaterLevelDataFrame(river = "Rhein", time = as.POSIXct("2016-12-21"),
                                station = seq(666, 777, by = 0.1))
    expect_equal(getRiver(wldf), "Rhein")
    expect_error(setRiver(wldf) <- "Elbe", regexp = "must be below km 585.7")
    expect_error(setRiver(wldf) <- "Elbe", regexp = "must be below 585700 (km", 
                 fixed = TRUE)
    
    # both directions
    wldf <- WaterLevelDataFrame(river = "Rhein", time = as.POSIXct("2016-12-21"),
                                station = seq(554, 555, by = 0.1))
    expect_equal(getRiver(wldf), "Rhein")
    setRiver(wldf) <- "Elbe"
    expect_equal(getRiver(wldf), "Elbe")
    wldf <- waterLevel(wldf)
    expect_equal(wldf$w, c(5.8, 5.79, 5.77, 5.76, 5.75, 5.74, 5.73, 5.72, 5.7, 
                           5.69, 5.67))
    setRiver(wldf) <- "Rhein"
    expect_equal(all(is.na(wldf$w)), TRUE)
    wldf <- waterLevel(wldf)
    expect_equal(wldf$w, c(66.07, 66.06, 66.07, 66.06, 66.06, 66.01, 65.94, 
                           65.87, 65.78, 65.70, 65.61))
})


test_that("time", {
    wldf <- WaterLevelDataFrame(river = "Elbe", time = as.POSIXct("2016-12-21"),
                                station = seq(256, 263, by = 0.1))
    expect_equal(getTime(wldf), as.POSIXct("2016-12-31"))
    setTime(wldf) <- as.POSIXct("2016-12-30")
    expect_equal(getTime(wldf), as.POSIXct("2016-12-30"))
    wldf <- waterLevel(wldf)
    expect_equal(getTime(wldf), as.POSIXct("2016-12-30"))
    expect_error(setTime(wldf) <- as.POSIXct("1916-12-30"))
    setTime(wldf) <- as.POSIXct("2015-12-30")
    expect_equal(all(is.na(wldf$w)), TRUE)
    setTime(wldf) <- as.POSIXct("2016-12-31")
    wldf <- waterLevel(wldf)
    
    wldf1 <- readWaterLevelFileDB(river = getRiver(wldf),
                                  time = getTime(wldf),
                                  from = 256, to = 263)
    expect_equal(wldf$station, wldf1$station)
    expect_equal(wldf$station_int, wldf1$station_int)
    diff <- wldf$w - wldf1$w
    expect_equal(min(diff), -0.01, 
                 label = "minimum difference: computed wl <> stored  wl")
    expect_equal(max(diff), 0.01, 
                 label = "maximum difference: computed wl <> stored  wl")
    
})


test_that("gauging_stations", {
    wldf <- WaterLevelDataFrame(river = "Elbe", time = as.POSIXct("2016-12-21"),
                                station = seq(256, 263, by = 0.1))
    gs <- data.frame(id = integer(),
                     gauging_station    = character(),
                     uuid               = character(),
                     km                 = numeric(),
                     km_qps             = numeric(),
                     river              = character(),
                     longitude          = numeric(),
                     latitude           = numeric(),
                     mw                 = numeric(),
                     mw_timespan        = character(),
                     pnp                = numeric(),
                     w                  = numeric(),
                     wl                 = numeric(),
                     n_wls_below_w_do   = integer(),
                     n_wls_above_w_do   = integer(),
                     n_wls_below_w_up   = integer(),
                     n_wls_above_w_up   = integer(),
                     name_wl_below_w_do = character(),
                     name_wl_above_w_do = character(),
                     name_wl_below_w_up = character(),
                     name_wl_above_w_up = character(),
                     w_wl_below_w_do    = numeric(),
                     w_wl_above_w_do    = numeric(),
                     w_wl_below_w_up    = numeric(),
                     w_wl_above_w_up    = numeric(),
                     weight_up          = numeric(),
                     weight_do          = numeric(),
                     stringsAsFactors   = FALSE)
    expect_equal(getGaugingStations(wldf), gs)
    wldf <- waterLevel(wldf)
    expect_equal(getGaugingStations(wldf)$gauging_station, c("VOCKERODE", 
                                                             "ROSSLAU", 
                                                             "DESSAU", "AKEN"))
})


test_that("gauging_stations_missing", {
    wldf <- WaterLevelDataFrame(river   = "Elbe",
                                time    = as.POSIXct("1991-12-16"),
                                station = seq(500, 510, 0.1))
    expect_equal(is.na(getGaugingStationsMissing(wldf)), TRUE)
    wldf <- waterLevel(wldf)
    expect_equal(getGaugingStationsMissing(wldf), c("up: GORLEBEN",
                                                    "up: LENZEN",
                                                    "up: SCHNACKENBURG",
                                                    "up: MÜGGENDORF",
                                                    "in: DÖMITZ"))
    expect_error(setGaugingStationsMissing(wldf) <- NA,
                 "unable to find an inherited method for function",
                 fixed = TRUE)
    setGaugingStationsMissing(wldf) <- as.character(NA)
    expect_equal(is.na(getGaugingStationsMissing(wldf)), TRUE)
})

