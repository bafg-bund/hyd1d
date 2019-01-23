library(usethis)

if (file.exists("data/date_gauging_data.rda")) {
    
    load("data/date_gauging_data.rda")
    
    if (Sys.Date() > date_gauging_data &
        Sys.time() > strptime(paste(Sys.Date(), "06:15"), "%Y-%m-%d %H:%M")) {
        
        print("data will be updated")
        for (a_file in rev(list.files("data-raw", pattern = "data_df.*", 
                                      full.names = TRUE))) {
            print(a_file)
            source(a_file)
        }
        
        # unload the required packages
        detach("package:ROracle", unload = TRUE)
        detach("package:RPostgreSQL", unload = TRUE)
        detach("package:DBI", unload = TRUE)
        
        # store date_gauging_data as external dataset
        date_gauging_data <- Sys.Date()
        usethis::use_data(date_gauging_data, pkg = ".", overwrite = TRUE, 
                           compress = "bzip2")
        
    } else {
        write("data are up to date", stderr())
    }
    
} else {
    
    print("data will be updated")
    for (a_file in rev(list.files("data-raw", pattern = "data_df.*", 
                                  full.names = TRUE))) {
        print(a_file)
        source(a_file)
    }
    
    # unload the required packages
    detach("package:ROracle", unload = TRUE)
    detach("package:RPostgreSQL", unload = TRUE)
    detach("package:DBI", unload = TRUE)
    
    # store date_gauging_data as external dataset
    date_gauging_data <- Sys.Date()
    usethis::use_data(date_gauging_data, pkg = ".", overwrite = TRUE, 
                       compress = "bzip2")
    
}

q("no")
