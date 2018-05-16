require(devtools)
require(DBI)
require(ROracle)

# source hyd1d-internal to obtain the credentials function
source("R/hyd1d-internal.R")

if (Sys.info()["nodename"] != "lvps46-163-72-150.dedicated.hosteurope.de") {
    
    f3_credentials <- credentials("/home/WeberA/hyd1d/DB_credentials_flys3")
    
    # read the data
    # access the FLYS3 DB
    f3_string <- paste0("(DESCRIPTION=",
                        "(ADDRESS=(PROTOCOL=tcp)(HOST=10.140.79.56)(PORT=1521))",
                        "(CONNECT_DATA=(SERVICE_NAME=FLYS3.DBMSDB.BAFG.DE)))")
    f3_con <- ROracle::dbConnect(drv      = DBI::dbDriver("Oracle"),
                                 username = f3_credentials["user"],
                                 password = f3_credentials["password"],
                                 dbname   = f3_string)
    
    # retrieve the data
    # for the Elbe
    query_string_elbe <- "
    SELECT
    FLYS3.WST_COLUMNS.NAME AS \"name\",
    FLYS3.WST_COLUMN_VALUES.POSITION AS \"station\",
    FLYS3.WST_COLUMN_VALUES.W AS \"w\"
    FROM
    FLYS3.RIVERS
    INNER JOIN FLYS3.WSTS ON FLYS3.RIVERS.ID = FLYS3.WSTS.RIVER_ID
    INNER JOIN FLYS3.WST_KINDS ON FLYS3.WST_KINDS.ID = FLYS3.WSTS.KIND
    INNER JOIN FLYS3.WST_COLUMNS ON FLYS3.WSTS.ID = FLYS3.WST_COLUMNS.WST_ID
    INNER JOIN FLYS3.WST_COLUMN_VALUES ON FLYS3.WST_COLUMNS.ID = 
    FLYS3.WST_COLUMN_VALUES.WST_COLUMN_ID
    WHERE
    FLYS3.WSTS.KIND = 0 AND
    FLYS3.RIVERS.NAME = 'Elbe' AND
    FLYS3.WST_COLUMN_VALUES.POSITION <= 585.7 AND
    FLYS3.WST_COLUMN_VALUES.POSITION >= 0
    ORDER BY
    FLYS3.WST_COLUMN_VALUES.POSITION ASC, FLYS3.WST_COLUMN_VALUES.W"
    
    # CAST(FLYS3.WST_COLUMN_VALUES.W * 100 AS INTEGER) AS \"w_int\",
    # CAST(FLYS3.WST_COLUMN_VALUES.POSITION * 1000 AS INTEGER) AS \"station_int\",
    
    df.flys_elbe <- dbGetQuery(f3_con, query_string_elbe)
    df.flys_elbe <- cbind(data.frame(river = as.character("Elbe", 
                                                          nrow(df.flys_elbe)),
                                     stringsAsFactors = FALSE),
                          df.flys_elbe)
    
    # for the Rhein
    query_string_rhein <- "
    SELECT
    FLYS3.WST_COLUMNS.NAME AS \"name\",
    FLYS3.WST_COLUMN_VALUES.POSITION AS \"station\",
    FLYS3.WST_COLUMN_VALUES.W AS \"w\"
    FROM
    FLYS3.RIVERS
    INNER JOIN FLYS3.WSTS ON FLYS3.RIVERS.ID = FLYS3.WSTS.RIVER_ID
    INNER JOIN FLYS3.WST_KINDS ON FLYS3.WST_KINDS.ID = FLYS3.WSTS.KIND
    INNER JOIN FLYS3.WST_COLUMNS ON FLYS3.WSTS.ID = FLYS3.WST_COLUMNS.WST_ID
    INNER JOIN FLYS3.WST_COLUMN_VALUES ON FLYS3.WST_COLUMNS.ID = 
    FLYS3.WST_COLUMN_VALUES.WST_COLUMN_ID
    WHERE
    FLYS3.WSTS.KIND = 0 AND
    FLYS3.RIVERS.NAME = 'Rhein' AND
    FLYS3.WST_COLUMN_VALUES.POSITION <= 865.7 AND
    FLYS3.WST_COLUMN_VALUES.POSITION >= 336.2
    ORDER BY
    FLYS3.WST_COLUMN_VALUES.POSITION ASC, FLYS3.WST_COLUMN_VALUES.W"
    
    # CAST(FLYS3.WST_COLUMN_VALUES.W * 100 AS INTEGER) AS \"w_int\",
    # CAST(FLYS3.WST_COLUMN_VALUES.POSITION * 1000 AS INTEGER) AS \"station_int\",
    
    df.flys_rhein <- dbGetQuery(f3_con, query_string_rhein)
    df.flys_rhein <- cbind(data.frame(river = as.character("Rhein", 
                                                           nrow(df.flys_rhein)),
                                      stringsAsFactors = FALSE),
                           df.flys_rhein)
    
    # combine both datasets
    df.flys_data <- rbind.data.frame(df.flys_elbe, df.flys_rhein,
                                     stringsAsFactors = FALSE)
    #df.flys_data$station_int <- as.integer(df.flys_data$station_int)
    #df.flys_data$w_int <- as.integer(df.flys_data$w_int)
    
    # store df.flys as external dataset
    devtools::use_data(df.flys_data, pkg = ".", overwrite = TRUE, 
                       compress = "bzip2")
    
    # variables for RDO
    RDO_NROW_DF.FLYS_DATA <- as.character(nrow(df.flys_data))
    
    # clean up
    rm(f3_con, f3_string, query_string_elbe, query_string_rhein, df.flys_elbe, 
       df.flys_rhein, df.flys_data)
    
    # Test 1:
    # - remove redundant columns
    #df.flys_data_test1 <- df.flys_data[, c(1,2,3,5)]
    #devtools::use_data(df.flys_data_test1, pkg = ".",
    #                   overwrite = TRUE, compress = "bzip2")
    #
    # Test2:
    # - convert character columns to factor
    #df.flys_data_test2 <- df.flys_data_test1
    #df.flys_data_test2$river <- as.factor(df.flys_data_test2$river)
    #df.flys_data_test2$name <- as.factor(df.flys_data_test2$name)
    #devtools::use_data(df.flys_data_test2, pkg = ".",
    #                   overwrite = TRUE, compress = "bzip2")
    #
    
} else {
    print("The flys database is not accessible!")
}

