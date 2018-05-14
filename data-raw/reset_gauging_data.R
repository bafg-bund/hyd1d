##################################################
# reset_gauging_data.R
#
# author: arnd.weber@bafg.de
# date:   14.05.2018
#
# purpose: 
#   - restore content of gauging_data DB from csv files
#
##################################################

# make this script executable only on aqualogy-cloud.de
if (Sys.info()["nodename"] != "lvps46-163-72-150.dedicated.hosteurope.de") {
    print("This script has to be executed on aqualogy-cloud.de!")
    q("no")
}

# configure output
verbose <- TRUE

# load required packages
require("DBI")
require("RPostgreSQL")
require("RCurl")

# source hyd1d-internal to obtain the credentials function
path <- "/home/arnd/BfG/"
setwd(path)
source("hyd1d/R/hyd1d-internal.R")

### open the connection using user, password, etc., as
credentials <- credentials("DB_credentials_gauging_data")
con <- dbConnect("PostgreSQL", 
                 host = credentials["host"], 
                 dbname = credentials["dbname"], 
                 user = credentials["user"], 
                 password = credentials["password"], 
                 port = credentials["port"])
postgresqlpqExec(con, "SET client_encoding = 'UTF-8'")

### gauging_data table
dbSendQuery(con, "DROP TABLE IF EXISTS \"gauging_data\";")
dbSendQuery(con, "CREATE TABLE \"gauging_data\" 
    (
        \"id\" serial primary key,
        \"gauging_station\" varchar(50),
        \"date\" date,
        \"year\" integer,
        \"month\" integer,
        \"day\" integer,
        \"w\" double precision
    );")
dbSendQuery(con, "ALTER TABLE \"gauging_data\" OWNER TO gauging_data;")
dbSendQuery(con, "ALTER TABLE \"gauging_data\" SET WITH OIDS;")
dbSendQuery(con, "COMMENT ON TABLE \"gauging_data\" IS 'gauging_data collection
            ';")
dbSendQuery(con, paste0("COPY public.\"gauging_data\" FROM '", path, "hyd1d/R/",
                        "data-raw/gauging_data.csv' WITH (FORMAT CSV, HEADER, ",
                        "DELIMITER ';', NULL 'NULL', ENCODING 'UTF8');"))
dbSendQuery(con, "SELECT setval('public.gauging_data_id_seq', (SELECT max(id) 
            FROM public.\"gauging_data\"), true);")

### gauging_station_data table
dbSendQuery(con, "DROP TABLE IF EXISTS \"gauging_station_data\";")
dbSendQuery(con, "CREATE TABLE \"gauging_station_data\"
    (
        \"id\" serial primary key,
        \"gauging_station\" varchar(50),
        \"uuid\" varchar(50),
        \"agency\" varchar(50),
        \"number\" varchar(50),
        \"km\" double precision,
        \"water_shortname\" varchar(50),
        \"water_longname\" varchar(50),
        \"gauging_station_shortname\" varchar(50),
        \"gauging_station_longname\" varchar(50),
        \"longitude\" double precision,
        \"latitude\" double precision,
        \"mw\" double precision,
        \"mw_timespan\" varchar(100),
        \"pnp\" double precision,
        \"data_present\" boolean NOT NULL DEFAULT false,
        \"data_present_timespan\" varchar(100),
        \"data_missing\" boolean NOT NULL DEFAULT false,
        \"zrx_timestamp\" timestamp without time zone,
        \"tiles\" boolean NOT NULL DEFAULT true,
        \"km_qpf\" double precision,
        \"km_qps\" double precision,
        \"zrx_date_min\" timestamp without time zone,
        \"zrx_date_max\" timestamp without time zone
    );")
dbSendQuery(con, "ALTER TABLE \"gauging_station_data\" OWNER TO gauging_data;")
dbSendQuery(con, "ALTER TABLE \"gauging_station_data\" SET WITH OIDS;")
dbSendQuery(con, "COMMENT ON TABLE \"gauging_station_data\" IS 
            'gauging_station_data collection';")
dbSendQuery(con, paste0("COPY public.\"gauging_station_data\" FROM '", path, 
                        "/hyd1d/data-raw/gauging_station_data.csv' WITH (",
                        "FORMAT CSV, HEADER, DELIMITER ';', NULL 'NULL', ",
                        "ENCODING 'UTF8');"))
dbSendQuery(con, "SELECT setval('public.gauging_station_data_id_seq', 
            (SELECT max(id) FROM public.\"gauging_station_data\"), true);")

### gauging_data_missing table
dbSendQuery(con, "DROP TABLE IF EXISTS \"gauging_data_missing\";")
dbSendQuery(con, "CREATE TABLE \"gauging_data_missing\"
    (
        \"id\" serial primary key,
        \"gauging_station\" varchar(50),
        \"date\" date
    );")
dbSendQuery(con, "ALTER TABLE \"gauging_data_missing\" OWNER TO gauging_data;")
dbSendQuery(con, "ALTER TABLE \"gauging_data_missing\" SET WITH OIDS;")
dbSendQuery(con, "COMMENT ON TABLE \"gauging_data_missing\" IS 
            'dates missing in the gauging_data collection';")
dbSendQuery(con, paste0("COPY public.\"gauging_data_missing\" FROM '", path, 
                        "/hyd1d/data-raw/gauging_data_missing.csv' WITH (",
                        "FORMAT CSV, HEADER, DELIMITER ';', NULL 'NULL', ",
                        "ENCODING 'UTF8');"))
dbSendQuery(con, "SELECT setval('public.gauging_data_missing_id_seq', 
            (SELECT max(id) FROM public.\"gauging_data_missing\"), true);")

q("no")
