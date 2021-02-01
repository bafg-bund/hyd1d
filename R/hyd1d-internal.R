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

