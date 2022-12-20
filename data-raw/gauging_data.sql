/*

*/
CREATE ROLE gauging_data LOGIN
  ENCRYPTED PASSWORD 'md5070274cea10d15bd780a06d8fc640846'
  NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;

CREATE DATABASE gauging_data
  WITH OWNER = gauging_data
       ENCODING = 'UTF8'
       TABLESPACE = pg_default
       LC_COLLATE = 'de_DE.UTF-8'
       LC_CTYPE = 'de_DE.UTF-8'
       CONNECTION LIMIT = -1;

\c gauging_data;

CREATE TABLE "gauging_data"
(
  "id" serial primary key,
  "gauging_station" varchar(50),
  "date" date,
  "year" integer,
  "month" integer,
  "day" integer,
  "w" double precision
);
ALTER TABLE "gauging_data" OWNER TO gauging_data;
ALTER TABLE "gauging_data" SET WITH OIDS;
COMMENT ON TABLE "gauging_data" IS 'gauging_data collection';

CREATE TABLE "gauging_station_data"
(
  "id" serial primary key,
  "gauging_station" varchar(50),
  "uuid" varchar(50),
  "agency" varchar(50),
  "number" varchar(50),
  "km" double precision,
  "water_shortname" varchar(50),
  "water_longname" varchar(50),
  "gauging_station_shortname" varchar(50),
  "gauging_station_longname" varchar(50),
  "longitude" double precision,
  "latitude" double precision,
  "mw" double precision,
  "mw_timespan" varchar(100),
  "pnp" double precision,
  "data_present" boolean NOT NULL DEFAULT false,
  "data_present_timespan" varchar(100),
  "data_missing" boolean NOT NULL DEFAULT false,
  "zrx_timestamp" timestamp without time zone,
  "zrx_date_min" timestamp without time zone,
  "zrx_date_max"timestamp without time zone,
  "tiles" boolean NOT NULL DEFAULT true,
  "km_qpf" double precision,
  "km_qps" double precision
);
ALTER TABLE "gauging_station_data" OWNER TO gauging_data;
ALTER TABLE "gauging_station_data" SET WITH OIDS;
COMMENT ON TABLE "gauging_station_data" IS 'gauging_station_data collection';
COPY public."gauging_station_data" FROM '/tmp/gauging_station_data.csv' DELIMITER ';' CSV;
SELECT setval('public.gauging_station_data_id_seq', (SELECT max(id) FROM public."gauging_station_data"), true);


CREATE TABLE "gauging_data_missing"
(
  "id" serial primary key,
  "gauging_station" varchar(50),
  "date" date
);
ALTER TABLE "gauging_data_missing" OWNER TO gauging_data;
ALTER TABLE "gauging_data_missing" SET WITH OIDS;
COMMENT ON TABLE "gauging_data_missing" IS 'dates missing in the gauging_data collection';

