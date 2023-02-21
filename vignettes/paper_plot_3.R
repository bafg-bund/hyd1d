library(hyd1d)
library(plotrix)
xlim_min <- 257
xlim_max <- 263
ylim_min <- 53.8
ylim_max <- 55.7
wldf <- WaterLevelDataFrame(river = "Elbe",
                            time = as.POSIXct("2016-12-21"),
                            station = seq(257, 262, 0.1))
wldf1 <- waterLevel(wldf, TRUE)
gs <- getGaugingStations(wldf1)
id <- gs$km_qps >= xlim_min & gs$km_qps <= xlim_max

flys3_water_levels <- c("0.5MNQ", "MNQ", "0.5MQ", "a", "0.75MQ", "b", "MQ", "c",
                        "2MQ", "3MQ", "d", "e", "MHQ", "HQ2", "f", "HQ5", "g", 
                        "h", "HQ10", "HQ15", "HQ20", "HQ25", "HQ50", "HQ75", 
                        "HQ100", "i", "HQ150", "HQ200", "HQ300", "HQ500")
mq_0.5 <- waterLevelFlys3(wldf, "0.5MQ")
a <- waterLevelFlys3(wldf, "a")
mq_0.75 <- waterLevelFlys3(wldf, "0.75MQ")
mq <- waterLevelFlys3(wldf, "MQ")
wldf3 <- waterLevelFlys3InterpolateY(wldf, "ROSSLAU", shiny = TRUE)
wldf4 <- waterLevelFlys3InterpolateY(wldf, "DESSAU", shiny = TRUE)
wldf5 <- waterLevelFlood2(wldf)

pdf("vignettes/paper_plot_3.pdf", width = 8, height = 4)
par(oma = c(3, 3, 0.5, 0.5), mar = c(1, 1, 0.2, 0.2))
# Fig. 3
{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max),
         ylim = c(ylim_min, ylim_max), xaxp = c(258, 262, 2),
         xlab = "river kilometer (km)", ylab = "elevation (m a.s.l.)")
    axis(1, at = c(257, 259, 261), labels = FALSE,
         tcl = -0.2)
    
    # polygon
    polygon(c(wldf3$station, rev(wldf4$station)), c(wldf3$w, rev(wldf4$w)),
            col = "grey95", border = NA)
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7, 
                 border = FALSE)
    boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7, 
                 border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    boxed.labels(259.6, 55.5, "Mulde River", cex = 0.7, border = FALSE,
                 col = "blue")
    
    # FLYS ROSSLAU
    lines(wldf3$station, wldf3$w, lty = 2, col = "grey30")
    
    # FLYS DESSAU
    lines(wldf4$station, wldf4$w, lty = 1, col = "grey30")
    
    # waterLevelFlood2
    lines(wldf5$station, wldf5$w, lty = 1, col = "red")
    
    # waterLevel
    lines(wldf1$station, wldf1$w, lty = 1, col = "darkblue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # legend
    legend("topright", 
           col = c("darkblue", "darkblue", "grey30", "grey30", "red"), 
           pch = c(21, NA, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA, NA), 
           lty = c(0, 1, 1, 2, 1), 
           lwd = c(0, 1, 1, 1, 1),
           legend = c("measured water levels", "hyd1d::waterLevel",
                      "FLYS w.l. DESSAU", "FLYS w.l. ROSSLAU",
                      "linear interpolation"),
           text.col = c(1, 1, 1, 1, 1),
           text.font = c(1, 1, 1, 1, 1),
           cex = 0.7, bty = "n")
    #box()
}
dev.off()

summary(wldf4$w - wldf3$w)


#q("no")
