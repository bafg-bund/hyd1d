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

# readzrx
# - import zrx files as df
readzrx <- function(x) {
    
    # determine number of columns
    header <- readLines(x, 10)
    n_head <- length(which(startsWith(header, "#")))
    header <- header[startsWith(header, "#")]
    layout <- header[length(header)]
    
    columns <- unlist(strsplit(layout, split = "(",
                               fixed = TRUE))[2]
    columns <- unlist(strsplit(columns, split = ")", fixed = TRUE))[1]
    columns <- unlist(strsplit(columns, split = ",", fixed = TRUE))
    columns <- columns[columns != "status"]
    
    col_names <- append(columns, "rem")
    col_classes <- append(c("character", "numeric"),
                          rep("character", length(col_names) - 2))
    
    # read zrx into data.frame
    df <- read.table(file = x, col.names = col_names, colClasses = col_classes,
                     skip = n_head, comment.char = "#", sep=" ", fill = TRUE,
                     blank.lines.skip = TRUE, header = FALSE)
    df <- df[, c("timestamp", "value")]
    
    # remove all rows with NA values (-777)
    id_na <- which(df$value == -777)
    if(length(id_na) > 0){
        df <- df[-id_na,]
    }
    
    # restructure df
    df_db <- data.frame(
        date = as.Date(strptime(df$timestamp, format = "%Y%m%d%H%M%S")),
        w = df$value, 
        year = format(strptime(df$timestamp, format = "%Y%m%d%H%M%S"), 
                      format = "%Y"),
        month = format(strptime(df$timestamp, format = "%Y%m%d%H%M%S"),
                       format = "%m"),
        day = format(strptime(df$timestamp, format = "%Y%m%d%H%M%S"),
                     format = "%d"))
    
    return(df_db)
}

