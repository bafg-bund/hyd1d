##################################################
# reset_gauging_data.R
#
# author: arnd.weber@bafg.de
# date:   23.05.2018
#
# purpose: 
#   - restore content of gauging_data DB from csv files
#
##################################################

# configure output
verbose <- TRUE

# load required packages
require("DBI")
require("RPostgreSQL")
require("RCurl")

# source hyd1d-internal to obtain the credentials function
source("R/hyd1d-internal.R")

### open the connection using user, password, etc., as
credentials <- credentials("/home/WeberA/hyd1d/DB_credentials_gauging_data")
con <- dbConnect("PostgreSQL", 
                 host = credentials["host"], 
                 dbname = credentials["dbname"], 
                 user = credentials["user"], 
                 password = credentials["password"], 
                 port = credentials["port"])
postgresqlpqExec(con, "SET client_encoding = 'UTF-8'")

### gauging_data table
dbSendQuery(con, "DROP TABLE IF EXISTS \"gauging_data\";")
# dbSendQuery(con, "CREATE TABLE \"gauging_data\" 
#     (
#         \"id\" serial primary key,
#         \"gauging_station\" varchar(50),
#         \"date\" date,
#         \"year\" integer,
#         \"month\" integer,
#         \"day\" integer,
#         \"w\" double precision
#     );")
# path <- getwd()
# dbSendQuery(con, paste0("COPY public.\"gauging_data\" FROM '", path,
#                         "data-raw/gauging_data.csv' WITH (FORMAT CSV, HEADER",
#                         ", DELIMITER ';', NULL 'NULL', ENCODING 'UTF8');"))
df.gd <- read.table("data-raw/gauging_data.csv", header = TRUE, dec = ".",
                    sep = ";", na.strings = "NULL", colClasses = c("integer", 
                    "character", "Date", "integer", "integer", "integer", 
                    "numeric"))
dbWriteTable(con, "gauging_data", df.gd)
# dbSendQuery(con, "SELECT setval('public.gauging_data_id_seq', (SELECT max(id) 
#             FROM public.\"gauging_data\"), true);")
dbSendQuery(con, "ALTER TABLE \"gauging_data\" OWNER TO gauging_data;")
dbSendQuery(con, "ALTER TABLE \"gauging_data\" SET WITH OIDS;")
dbSendQuery(con, paste0("COMMENT ON TABLE \"gauging_data\" IS 'gauging_data co",
                        "llection';"))

### gauging_station_data table
dbSendQuery(con, "DROP TABLE IF EXISTS \"gauging_station_data\";")
# dbSendQuery(con, "CREATE TABLE \"gauging_station_data\"
#     (
#         \"id\" serial primary key,
#         \"gauging_station\" varchar(50),
#         \"uuid\" varchar(50),
#         \"agency\" varchar(50),
#         \"number\" varchar(50),
#         \"km\" double precision,
#         \"water_shortname\" varchar(50),
#         \"water_longname\" varchar(50),
#         \"gauging_station_shortname\" varchar(50),
#         \"gauging_station_longname\" varchar(50),
#         \"longitude\" double precision,
#         \"latitude\" double precision,
#         \"mw\" double precision,
#         \"mw_timespan\" varchar(100),
#         \"pnp\" double precision,
#         \"data_present\" boolean NOT NULL DEFAULT false,
#         \"data_present_timespan\" varchar(100),
#         \"data_missing\" boolean NOT NULL DEFAULT false,
#         \"zrx_timestamp\" timestamp without time zone,
#         \"tiles\" boolean NOT NULL DEFAULT true,
#         \"km_qpf\" double precision,
#         \"km_qps\" double precision,
#         \"zrx_date_min\" timestamp without time zone,
#         \"zrx_date_max\" timestamp without time zone
#     );")
# dbSendQuery(con, paste0("COPY public.\"gauging_station_data\" FROM '", path, 
#                         "data-raw/gauging_station_data.csv' WITH (",
#                         "FORMAT CSV, HEADER, DELIMITER ';', NULL 'NULL', ",
#                         "ENCODING 'UTF8');"))
df.gsd <- read.table("data-raw/gauging_station_data.csv", header = TRUE, 
                     dec = ".", sep = ";", na.strings = "NULL", 
                     colClasses = c("integer", "character", "character", 
                     "character", "character", "numeric", "character", 
                     "character", "character", "character", "numeric", 
                     "numeric", "numeric", "character", "numeric", "character",
                     "character", "character", "POSIXct", "character",
                     "numeric", "numeric", "POSIXct", "POSIXct"))
df.gsd$data_present[df.gsd$data_present == "f"] <- FALSE
df.gsd$data_present[df.gsd$data_present == "t"] <- TRUE
df.gsd$data_present <- as.logical(df.gsd$data_present)
df.gsd$data_missing[df.gsd$data_missing == "f"] <- FALSE
df.gsd$data_missing[df.gsd$data_missing == "t"] <- TRUE
df.gsd$data_missing <- as.logical(df.gsd$data_missing)
df.gsd$tiles[df.gsd$tiles == "f"] <- FALSE
df.gsd$tiles[df.gsd$tiles == "t"] <- TRUE
df.gsd$tiles <- as.logical(df.gsd$tiles)
dbWriteTable(con, "gauging_station_data", df.gsd)
# dbSendQuery(con, "SELECT setval('public.gauging_station_data_id_seq', 
#             (SELECT max(id) FROM public.\"gauging_station_data\"), true);")
dbSendQuery(con, "ALTER TABLE \"gauging_station_data\" OWNER TO gauging_data;")
dbSendQuery(con, "ALTER TABLE \"gauging_station_data\" SET WITH OIDS;")
dbSendQuery(con, "COMMENT ON TABLE \"gauging_station_data\" IS 
            'gauging_station_data collection';")

### gauging_data_missing table
dbSendQuery(con, "DROP TABLE IF EXISTS \"gauging_data_missing\";")
# dbSendQuery(con, "CREATE TABLE \"gauging_data_missing\"
#     (
#         \"id\" serial primary key,
#         \"gauging_station\" varchar(50),
#         \"date\" date
#     );")
# dbSendQuery(con, paste0("COPY public.\"gauging_data_missing\" FROM '", path, 
#                         "data-raw/gauging_data_missing.csv' WITH (",
#                         "FORMAT CSV, HEADER, DELIMITER ';', NULL 'NULL', ",
#                         "ENCODING 'UTF8');"))
df.gdm <- read.table("data-raw/gauging_data_missing.csv", header = TRUE, 
                     dec = ".", sep = ";", na.strings = "NULL", 
                     colClasses = c("integer", "character", "Date"))
dbWriteTable(con, "gauging_data_missing", df.gdm)
# dbSendQuery(con, "SELECT setval('public.gauging_data_missing_id_seq', 
#             (SELECT max(id) FROM public.\"gauging_data_missing\"), true);")
dbSendQuery(con, "ALTER TABLE \"gauging_data_missing\" OWNER TO gauging_data;")
dbSendQuery(con, "ALTER TABLE \"gauging_data_missing\" SET WITH OIDS;")
dbSendQuery(con, "COMMENT ON TABLE \"gauging_data_missing\" IS 
            'dates missing in the gauging_data collection';")

q("no")
