library(testthat)
library(hyd1d)

context("waterLevelFlys3")

test_that("waterLevelFlys3: Dessau", {
    
    wldf <- WaterLevelDataFrame(river   = "Elbe",
                                time    = as.POSIXct("2016-12-21"),
                                station = seq(257, 262, 0.1))
    wldf1 <- waterLevelFlys3(wldf, "MQ")
    wldf2 <- waterLevelFlys3Seq("Elbe", "MQ", 257, 262)
    
    expect_equal(wldf1$station, wldf2$station)
    expect_equal(wldf1$station_int, wldf2$station_int)
    expect_equal(wldf1$w, wldf2$w)
    
    # errors waterLevelFlys3
    expect_error(wldf1 <- waterLevelFlys3("wldf", "MQ"), 
                 "'wldf' must be type 'WaterLevelDataFrame'", fixed = TRUE)
    expect_error(wldf1 <- waterLevelFlys3(wldf, "MQ5"), 
                 "'name' must be an element of c('0.5MNQ'", fixed = TRUE)
    
    # errors waterLevelFlys3Seq
    expect_error(wldf1 <- waterLevelFlys3Seq("wldf", "MQ"), 
                 "ver' must be an element of c('Elbe', 'Rhine')", fixed = TRUE)
    expect_error(wldf1 <- waterLevelFlys3Seq("wldf", "MQ"), 
                 "e 'from' argument has to be su", fixed = TRUE)
    expect_error(wldf1 <- waterLevelFlys3Seq("wldf", "MQ"), 
                 "e 'to' argument has to be supp", fixed = TRUE)
    expect_error(wldf1 <- waterLevelFlys3Seq(wldf, "MQ5", 257, 262), 
                 "'river' must be type 'character", fixed = TRUE)
    expect_error(wldf1 <- waterLevelFlys3Seq("Elbe", "MQ", 262, 257), 
                 "'to' must be above 'from', since stationing incre", 
                 fixed = TRUE)
    
})


test_that("waterLevelFlys3InterpolateX", {
    expect_equal(df.flys[df.flys$river == "Elbe" & df.flys$station == 261, "w"],
                 waterLevelFlys3InterpolateX("Elbe", 261)$w)
    expect_equal(df.flys[df.flys$river == "Elbe" & 
                         df.flys$station == 261, "name"], 
                 waterLevelFlys3InterpolateX("Elbe", 261)$name)
    expect_equal(names(df.flys), 
                 names(waterLevelFlys3InterpolateX("Elbe", 261.1)))
    expect_error(waterLevelFlys3InterpolateX("ELBE", 261.1),
                 "'river' must be an element of c('Elbe', 'Rhine')", 
                 fixed = TRUE)
    expect_error(waterLevelFlys3InterpolateX(261.1),
                 "'river' must be an element of c('Elbe', 'Rhine')", 
                 fixed = TRUE)
    expect_error(waterLevelFlys3InterpolateX(261.1),
                 "ne station argument ('station_int' or 'station') must be", 
                 fixed = TRUE)
    expect_error(waterLevelFlys3InterpolateX("Elbe", 261100),
                 "max(station) must be below km 585.7 for river 'Elbe'", 
                 fixed = TRUE)
    expect_error(waterLevelFlys3InterpolateX("Elbe", 261100, 261.1),
                 "station arguments ('station_int', 'station') are supplied", 
                 fixed = TRUE)
    expect_error(waterLevelFlys3InterpolateX("Elbe", 261100, 261.1),
                 "'station_int' must be type 'integer'", 
                 fixed = TRUE)
    
})


test_that("waterLevelFlys3InterpolateY", {
    t <- as.POSIXct("2016-12-21")
    wldf <- WaterLevelDataFrame(river   = "Elbe",
                                time    = t,
                                station = seq(257, 262, 0.1))
    gs <- "DESSAU"
    w <- getGaugingDataW(gs, t)
    wldf1 <- waterLevelFlys3InterpolateY(wldf, gs, w)
    expect_equal(wldf1$station[1:5], seq(257, 262, 0.1)[1:5])
    expect_equal(wldf1$w[1:5], c(55.65, 55.63, 55.61, 55.59, 55.57))
    expect_equal(nrow(getGaugingStations(wldf1)), 1)
    expect_equal(getGaugingStations(wldf1)$gauging_station, gs)
    expect_equal(comment(wldf1), 
                 paste0("Computed by waterLevelFlys3InterpolateY(): gauging_s",
                        "tation = DESSAU, w = 165"))
    expect_error(waterLevelFlys3InterpolateY(wldf, "GEESTHACHT"),
                 "covered by 'wldf' or the next to it up- or downstream.",
                 fixed = TRUE)
    expect_error(waterLevelFlys3InterpolateY("wldf", "GEESTHACHT", w),
                 "'wldf' must be type 'WaterLevelDataFrame'")
    expect_error(waterLevelFlys3InterpolateY("wldf", "DESSAU", w),
                 "'wldf' must be type 'WaterLevelDataFrame'")
    expect_warning(wldf3 <- waterLevelFlys3InterpolateY(wldf, "DESSAU", 
                                                        w = 166), 
                   "'w' computed internally through getGauging", fixed = TRUE)
    
})
