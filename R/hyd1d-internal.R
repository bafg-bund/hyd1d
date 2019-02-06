# convert byte substituted ascii strings to utf-8
# https://en.wikipedia.org/wiki/List_of_Unicode_characters
# 
asc2utf8 <- function(x){
    y <- iconv(x, "ASCII", "UTF-8", sub="byte")
    # Ä
    y <- sub("<c3><84>", "\u00c4", y)
    # ä
    y <- sub("<c3><a4>", "\u00e4", y)
    # Ö
    y <- sub("<c3><96>", "\u00d6", y)
    # ö
    y <- sub("<c3><b6>", "\u00f6", y)
    # Ü
    y <- sub("<c3><9c>", "\u00dc", y)
    # ü
    y <- sub("<c3><bc>", "\u00fc", y)
    return(y)
}

# extract DB credentials from unversioned credential files
# 
credentials <- function(file) {
    if (file.exists(file)) {
        credentials_temp <- utils::read.table(file, header = FALSE, sep = ";", 
                                              stringsAsFactors = FALSE)
    } else {
        if (file.exists(basename(file))) {
            credentials_temp <- utils::read.table(file = basename(file), 
                                                  header = FALSE, sep = ";", 
                                                  stringsAsFactors = FALSE)
        } else {
            stop("'file' could not be found")
        }
    }
    
    credentials <- credentials_temp$V2
    names(credentials) <- credentials_temp$V1
    return(credentials)
}

# cap-function
# http://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
#
simpleCap <- function(x) {
    s <- unlist(strsplit(tolower(x), " "))
    t <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
    u <- unlist(strsplit(t, "-"))
    paste(toupper(substring(u, 1, 1)), substring(u, 2),
          sep = "", collapse = "-")
}

# DBpath
# - locate a writeable location for the gauging_data DB
DBpath <- function() {
    
    # check if DB exists in the package source and is writable
    db_dir <- paste0(find.package("hyd1d"), "/db/")
    dir.create(db_dir, FALSE, TRUE)
    file_date <- paste0(db_dir, "date_gauging_data.rda")
    file_data <- paste0(db_dir, "df.gauging_data_latest.rda")
    
    if (file.exists(file_date) & file.access(file_date, mode = 2) == 0 &
        file.exists(file_data) & file.access(file_data, mode = 2) == 0) {
        return(db_dir)
    } else if (dir.exists(db_dir) & file.access(db_dir, mode = 2) == 0) {
        return(db_dir)
    } else {
    # otherwise create a hidden directory in $HOME 
        db_dir <- paste0(Sys.getenv("HOME"), "/.hyd1d/")
        dir.create(db_dir, FALSE, TRUE)
        return(db_dir)
    }
}

