
# define value
date_gauging_data <- Sys.Date()

# store date_gauging_data as external dataset
if (!(file.exists("data/date_gauging_data.rda"))){
    usethis::use_data(date_gauging_data, pkg = ".", overwrite = TRUE, 
                       compress = "bzip2")
} else {
    write("data/date_gauging_data.rda exists already", stderr())
}

# clean up
rm(date_gauging_data)
