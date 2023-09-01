library(testthat)
library(hyd1d)

test_that("getPegelonlineCharacteristicValues", {
    # gauging_station
    expect_error(getPegelonlineCharacteristicValues(value = "DES"),
                 "The 'gauging_station' or 'uuid' argument has to be supplied.",
                 fixed = TRUE)
    expect_error(getPegelonlineCharacteristicValues(gauging_station = 1),
                 "'gauging_station' must be type 'character'",
                 fixed = TRUE)
    expect_error(getPegelonlineCharacteristicValues(
                     gauging_station = c("D", "A")),
                 "'gauging_station' must have length 1", fixed = TRUE)
    expect_error(getPegelonlineCharacteristicValues(gauging_station = "DES"),
                 "'gauging_station' must be an element of c('SCHOENA'",
                 fixed = TRUE)
    
    # uuid
    expect_error(getPegelonlineCharacteristicValues(uuid = 1),
                 "'uuid' must be type 'character'",
                 fixed = TRUE)
    expect_error(getPegelonlineCharacteristicValues(uuid = c("D", "A")),
                 "'uuid' must have length 1", fixed = TRUE)
    expect_error(getPegelonlineCharacteristicValues(uuid = "DES"),
                 "'uuid' must be an element of c('7cb7461b-3530-",
                 fixed = TRUE)
    
    # gauging_station & uuid
    expect_error(getPegelonlineCharacteristicValues(
                     gauging_station = "SCHOENA",
                     uuid = "85d686f1-55b2-4d36-8dba-3207b50901a7"),
                 "'gauging_station' and 'uuid' must fit to each other.",
                 fixed = TRUE)
    
    # value
    expect_error(getPegelonlineCharacteristicValues(gauging_station = "SCHOENA"),
                 "The 'value' argument has to be supplied.",
                 fixed = TRUE)
    expect_error(getPegelonlineCharacteristicValues(gauging_station = "SCHOENA",
                                                    value = 1),
                 "'value' must be type 'character'",
                 fixed = TRUE)
    expect_warning(getPegelonlineCharacteristicValues(gauging_station = "SCHOENA",
                                                      value = c("MW", "MNQ")),
                   "Not all of your supplied values are among the commonly que",
                   fixed = TRUE)
    expect_message(getPegelonlineCharacteristicValues(gauging_station = "SCHOENA",
                                                      value = c("MW", "MThw")),
                   "Not all requested values are available for the queried gau",
                   fixed = TRUE)
    expect_error(getPegelonlineCharacteristicValues(gauging_station = "SCHOENA",
                                                    value = c("MNQ", "MHQ")),
                 "None of your supplied values is among the commonly queried",
                 fixed = TRUE)
    expect_message(getPegelonlineCharacteristicValues(gauging_station = "SCHOENA",
                                                      value = "MThw"),
                   "None of requested values is available for ",
                   fixed = TRUE)
    
    # as_list
    expect_error(getPegelonlineCharacteristicValues(gauging_station = "SCHOENA",
                                                    value = "MHW", as_list = ""),
                 'inherits(as_list, "logical") is not TRUE', fixed = TRUE)
    expect_error(getPegelonlineCharacteristicValues(gauging_station = "SCHOENA",
                                                    value = "MHW",
                                                    as_list = c(TRUE, TRUE)),
                 'length(as_list) == 1 is not TRUE', fixed = TRUE)
    
})

