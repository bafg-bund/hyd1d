require(devtools)

# read the data
df.sections_data <- utils::read.table("data-raw/sections_data.csv", header = TRUE, 
                                      sep = ";", dec = ",", 
                                      stringsAsFactors = FALSE)

# replace non-ASCII characters
for (a in c("name", "gs_upper", "gs_lower")){
    df.sections_data[, a] <- iconv(df.sections_data[, a], from="UTF-8", 
                                   to="ASCII", sub="byte")
}

# store df.sections as external dataset
devtools::use_data(df.sections_data, pkg = ".", overwrite = TRUE, 
                   compress = "bzip2")

# clean up
rm(df.sections_data, a)
