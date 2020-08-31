
if (Sys.info()["nodename"] %in% c("r.bafg.de")) {
    if (!(file.exists("data/df.flys.rda"))){
        
        require(ROracle)
        
        # get credentials
        f3_credentials <- credentials("DB_credentials_flys3")
        
        # read the data
        # access the FLYS3 DB
        f3_string <- paste0("(DESCRIPTION=",
                            "(ADDRESS=(PROTOCOL=tcp)",
                            "(HOST=10.140.79.56)(PORT=1521))",
                            "(CONNECT_DATA=",
                            "(SERVICE_NAME=FLYS3.DBMSDB.BAFG.DE)))")
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
            INNER JOIN FLYS3.WST_COLUMNS ON FLYS3.WSTS.ID = 
                FLYS3.WST_COLUMNS.WST_ID
            INNER JOIN FLYS3.WST_COLUMN_VALUES ON FLYS3.WST_COLUMNS.ID = 
                FLYS3.WST_COLUMN_VALUES.WST_COLUMN_ID
        WHERE
            FLYS3.WSTS.KIND = 0 AND
            FLYS3.RIVERS.NAME = 'Elbe' AND
            FLYS3.WST_COLUMN_VALUES.POSITION <= 585.7 AND
            FLYS3.WST_COLUMN_VALUES.POSITION >= 0
        ORDER BY
            FLYS3.WST_COLUMN_VALUES.POSITION ASC, FLYS3.WST_COLUMN_VALUES.W"
        
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
            INNER JOIN FLYS3.WST_COLUMNS ON FLYS3.WSTS.ID = 
                FLYS3.WST_COLUMNS.WST_ID
            INNER JOIN FLYS3.WST_COLUMN_VALUES ON FLYS3.WST_COLUMNS.ID = 
                FLYS3.WST_COLUMN_VALUES.WST_COLUMN_ID
        WHERE
            FLYS3.WSTS.KIND = 0 AND
            FLYS3.RIVERS.NAME = 'Rhein' AND
            FLYS3.WST_COLUMN_VALUES.POSITION <= 865.7 AND
            FLYS3.WST_COLUMN_VALUES.POSITION >= 336.2
        ORDER BY
            FLYS3.WST_COLUMN_VALUES.POSITION ASC, FLYS3.WST_COLUMN_VALUES.W"
        
        df.flys_rhein <- dbGetQuery(f3_con, query_string_rhein)
        df.flys_rhein <- cbind(data.frame(river = as.character("Rhein", 
                                                      nrow(df.flys_rhein)),
                                          stringsAsFactors = FALSE),
                               df.flys_rhein)
        
        # for the Weser
        query_string_weser <- "
        SELECT
            FLYS3.WST_COLUMNS.NAME AS \"name\",
            FLYS3.WST_COLUMN_VALUES.POSITION AS \"station\",
            FLYS3.WST_COLUMN_VALUES.W AS \"w\"
        FROM
            FLYS3.RIVERS
            INNER JOIN FLYS3.WSTS ON FLYS3.RIVERS.ID = FLYS3.WSTS.RIVER_ID
            INNER JOIN FLYS3.WST_KINDS ON FLYS3.WST_KINDS.ID = FLYS3.WSTS.KIND
            INNER JOIN FLYS3.WST_COLUMNS ON FLYS3.WSTS.ID = 
                FLYS3.WST_COLUMNS.WST_ID
            INNER JOIN FLYS3.WST_COLUMN_VALUES ON FLYS3.WST_COLUMNS.ID = 
                FLYS3.WST_COLUMN_VALUES.WST_COLUMN_ID
        WHERE
            FLYS3.WSTS.KIND = 0 AND
            FLYS3.RIVERS.NAME = 'Weser' AND
            FLYS3.WST_COLUMN_VALUES.POSITION <= 362.125 AND
            FLYS3.WST_COLUMN_VALUES.POSITION >= 0
        ORDER BY
            FLYS3.WST_COLUMN_VALUES.POSITION ASC, FLYS3.WST_COLUMN_VALUES.W"
        
        df.flys_weser <- dbGetQuery(f3_con, query_string_weser)
        df.flys_weser <- cbind(data.frame(river = as.character("Weser", 
                                                               nrow(df.flys_weser)),
                                          stringsAsFactors = FALSE),
                               df.flys_weser)
        # df.flys_sel <- df.flys_weser[which(df.flys_weser$station == 0.1), ]
        # df.flys_sel <- df.flys_sel[order(df.flys_sel$w), ]
        # paste(collapse = '", "', df.flys_sel$name)
        
        # combine both datasets
        df.flys <- rbind.data.frame(df.flys_elbe, df.flys_rhein,
                                    stringsAsFactors = FALSE)
        
        # store df.flys as external dataset
        usethis::use_data(df.flys, overwrite = TRUE, compress = "bzip2")
        
        # clean up
        rm(f3_con, f3_credentials, f3_string, query_string_elbe, 
           query_string_rhein, df.flys_elbe, df.flys_rhein, df.flys)
        
        # detach ROracle
        detach("package:ROracle", unload = TRUE)
        
    } else {
        write("data/df.flys.rda exists already", stderr())
    }
} else {
    if (!(file.exists("data/df.flys.rda"))){
        write(paste0("The flys database is not accessible and data/df.flys",
                     ".rda can't be created!"), stderr())
    } else {
        write("data/df.flys.rda exists already", stderr())
    }
    
}
