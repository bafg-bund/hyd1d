require(devtools)

df.flys_sections <- data.frame(
    river = c("ELBE", "ELBE", "ELBE", "ELBE", "ELBE", "ELBE", "ELBE", "ELBE", 
              "ELBE", "ELBE", "RHEIN", "RHEIN", "RHEIN", "RHEIN", "RHEIN", 
              "RHEIN", "RHEIN", "RHEIN", "RHEIN", "RHEIN", "RHEIN", "RHEIN", 
              "RHEIN", "RHEIN"),
    gauging_station = c("SCHÖNA", "DRESDEN", "TORGAU", "WITTENBERG", "AKEN", 
                        "BARBY", "MAGDEBURG-STROMBRÜCKE", "TANGERMÜNDE", 
                        "WITTENBERGE", "NEU DARCHAU", "MAXAU", "SPEYER", 
                        "WORMS", "MAINZ", "KAUB", "KOBLENZ", "ANDERNACH", 
                        "BONN", "KÖLN", "DÜSSELDORF", "RUHRORT", "WESEL", 
                        "REES", "EMMERICH"),
    from = c(0, 39.2, 108.4, 198.6, 259.6, 290.8, 300.6, 350.3, 437.9, 474.6, 
             336.2, 384.84, 428.16, 496.63, 529.1, 585.72, 592.3, 629.28, 
             659.35, 703.3, 780.14, 797.72, 814.45, 844.7),
    to = c(39.2, 108.4, 198.6, 259.6, 290.8, 300.6, 350.3, 437.9, 474.6, 598.3, 
           384.84, 428.16, 496.63, 529.1, 585.72, 592.3, 629.28, 659.35, 703.3, 
           780.14, 797.72, 814.45, 844.7, 866.3),
    stringsAsFactors = FALSE)

# replace non-ASCII characters
df.flys_sections$river <- iconv(df.flys_sections$river, from = "UTF-8",
                                 to = "ASCII", sub = "byte")
df.flys_sections$gauging_station <- iconv(df.flys_sections$gauging_station,
                                          from = "UTF-8", to = "ASCII", 
                                          sub = "byte")

# add column uuid
df.flys_sections$uuid <- rep("", nrow(df.flys_sections))
for (a in 1:nrow(df.flys_sections)) {
    df.flys_sections$uuid[a] <- df.gauging_station_data$uuid[
        which(df.gauging_station_data$gauging_station == 
                  df.flys_sections$gauging_station[a])]
}

# store df.flys as external dataset
devtools::use_data(df.flys_sections, pkg = ".", overwrite = TRUE,
                   compress = "bzip2")

# clean up
rm(a, df.flys_sections, df.gauging_station_data)
