require(devtools)

date_gauging_data <- Sys.Date()

# store date_gauging_data as external dataset
devtools::use_data(date_gauging_data, pkg = ".", overwrite = TRUE, 
                   compress = "bzip2")

# clean up
rm(date_gauging_data)
