
if (!(file.exists("data/df.sections.rda"))){
    # read the dataset
    df.sections <- utils::read.table("data-raw/df.sections.csv", header = TRUE, 
                                     sep = ";", dec = ",", 
                                     stringsAsFactors = FALSE)
    
    # replace non-ASCII characters
    for (a in c("name", "gs_upper", "gs_lower")){
        df.sections[, a] <- iconv(df.sections[, a], from = "UTF-8", 
                                  to = "ASCII", sub = "byte")
    }
    
    # store df.sections as external dataset
    usethis::use_data(df.sections, overwrite = TRUE, compress = "bzip2")
    
    # clean up
    rm(df.sections, a)
    
} else {
    write("data/df.sections.rda exists already", stderr())
}
