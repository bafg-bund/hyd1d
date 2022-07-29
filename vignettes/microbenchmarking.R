library(hyd1d)
library(microbenchmark)

mbm <- microbenchmark(
    "historic" = {
        wldf <- WaterLevelDataFrame(river = "Elbe",
                                    time = as.POSIXct("2016-12-20"),
                                    station_int = seq.int(0L, 585600L,
                                                          by = 100L))
        wldf <- waterLevel(wldf)
    }, 
    "present" = {
        wldf <- WaterLevelDataFrame(river = "Elbe",
                                    time = Sys.time(),
                                    station_int = seq.int(0L, 585600L,
                                                          by = 100L))
        wldf <- waterLevelPegelonline(wldf)
    },
    times = 100
)

mbm
mbm.h <- as.data.frame(mbm)
mbm.p <- mbm.h[mbm.h$expr == "present",]
mbm.h <- mbm.h[mbm.h$expr == "historic",]
mean(mbm.p$time/1000000000)
sd(mbm.p$time/1000000000)
mean(mbm.h$time/1000000000)
sd(mbm.h$time/1000000000)

wldf.h <- WaterLevelDataFrame(river = "Elbe",
                              time = as.POSIXct("2016-12-20"),
                              station_int = seq.int(0L, 585600L, by = 100L))

wldf.p <- WaterLevelDataFrame(river = "Elbe",
                              time = Sys.time(),
                              station_int = seq.int(0L, 585600L, by = 100L))

mbm1 <- microbenchmark(
    "historic" = {
        wldf.out <- waterLevel(wldf.h)
    }, 
    "present" = {
        wldf.out <- waterLevelPegelonline(wldf.p)
    },
    times = 100
)

mbm1
mbm.h <- as.data.frame(mbm.1)
mbm.p <- mbm.h[mbm.h$expr == "present",]
mbm.h <- mbm.h[mbm.h$expr == "historic",]
mean(mbm.p$time/1000000000)
sd(mbm.p$time/1000000000)
mean(mbm.h$time/1000000000)
sd(mbm.h$time/1000000000)

