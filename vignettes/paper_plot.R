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
wldf2 <- waterLevelFlys3InterpolateY(wldf, "WITTENBERG", shiny = TRUE)
wldf3 <- waterLevelFlys3InterpolateY(wldf, "ROSSLAU", shiny = TRUE)
wldf4 <- waterLevelFlys3InterpolateY(wldf, "DESSAU", shiny = TRUE)

pdf("vignettes/paper_plot.pdf", width = 8, height = 20)
par(mfrow = c(8, 1), oma = c(0.5, 3, 0.5, 3), mar = c(2, 2, 0.2, 2))
# Fig. 2a
{
    plot(1, 1, type = "n", xlim = c(243, 277), ylim = c(51, 58),
         xaxt = "n", ylab = NA, yaxt = "n", xlab = NA)
    axis(2, at = c(52, 54, 56, 58))
    
    # berechnungsstrecke
    polygon(x = c(257, 262, 262, 257),
            y = c(50.8, 50.8, 58.2, 58.2),
            col = "grey95", border = NA)
    
    # landmarks
    abline(v = gs$km_qps, lty = 3, lwd = 0.5)
    text(gs$km_qps[1:2], c(52, 52), gs$gauging_station[1:2], cex = 0.7)
    text(gs$km_qps[3:4], c(57, 57), gs$gauging_station[3:4], cex = 0.7)
    
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    text(259.6, 56, "Mulde River", cex = 0.7, col = "blue")
    
}

# Fig. 2b
{
    plot(1, 1, type = "n", xlim = c(243, 277), ylim = c(51, 58),
         xaxt = "n", ylab = NA, yaxt = "n", xlab = NA)
    axis(2, at = c(52, 54, 56, 58))
    
    # stretch
    polygon(x = c(257, 262, 262, 257),
            y = c(50.8, 50.8, 58.2, 58.2),
            col = "grey95", border = NA)
    
    # landmarks
    abline(v = gs$km_qps, lty = 3, lwd = 0.5)
    text(gs$km_qps[1:2], c(52, 52), gs$gauging_station[1:2], cex = 0.7)
    text(gs$km_qps[3:4], c(57, 57), gs$gauging_station[3:4], cex = 0.7)
    
    # gauging data
    points(gs$km_qps, gs$wl, pch = 21, col = "darkblue", bg = "darkblue")
    
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    text(259.6, 56, "Mulde River", cex = 0.7, col = "blue")
    
    # legend
    legend("topright", 
           pch = 21, col = "darkblue", pt.bg = "darkblue", pt.cex = 1,
           legend = "water level", cex = 0.7, bty = "o", box.col = "white")
    box()
}

# Fig. 2c
{
    plot(1, 1, type = "n", xlim = c(243, 277), 
         ylim = c(51, 58), xlab = NA, yaxt = "n", ylab = NA)
    axis(2, at = c(52, 54, 56, 58))
    
    # stretch
    polygon(x = c(257, 262, 262, 257),
            y = c(50.8, 50.8, 58.2, 58.2),
            col = "grey95", border = NA)
    
    # sections
    for (i in 1:(nrow(gs) - 1)) {
        rect(gs$km_qps[i], 53, gs$km_qps[i + 1], 54, col = "grey85",
             border = NA)
        text((gs$km_qps[i] + gs$km_qps[i + 1])/2, 53.5, i, font = 2)
        lines(rep(gs$km_qps[i], 2), c(53, 54), lwd = 2)
        lines(rep(gs$km_qps[i + 1], 2), c(53, 54), lwd = 2)
    }
    
    # landmarks
    abline(v = gs$km_qps, lty = 3, lwd = 0.5)
    text(gs$km_qps[1:2], c(52, 52), gs$gauging_station[1:2], cex = 0.7)
    text(gs$km_qps[3:4], c(57, 57), gs$gauging_station[3:4], cex = 0.7)
    
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    text(259.6, 56, "Mulde River", cex = 0.7, col = "blue")
    
    # gauging data
    points(gs$km_qps, gs$wl, pch = 21, col = "darkblue", bg = "darkblue")
    
    # legend
    legend("topright", 
           pch = 21, col = "darkblue", pt.bg = "darkblue", pt.cex = 1,
           legend = "water level", cex = 0.7, bty = "o", box.col = "white")
    box()
}

# Fig. 2d
{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(ylim_min, ylim_max), xlab = NA, ylab = "elevation (m a.s.l.)",
         xaxt = "n", cex.lab = 1.4)
    
    for (a_wl in flys3_water_levels){
        wldf_temp <- waterLevelFlys3(wldf, a_wl)
        lines(wldf_temp$station, wldf_temp$w, lty = 3, lwd = 0.2)
    }
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7, 
                 border = FALSE)
    boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7, 
                 border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    boxed.labels(259.6, 55.5, "Mulde River", cex = 0.7, border = FALSE,
                 col = "blue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # FLYS
    i <- which(mq_0.5$station >= gs$km_qps[2] & mq_0.5$station <= gs$km_qps[3])
    lines(mq_0.5$station[i], mq_0.5$w[i])
    lines(mq_0.75$station[i], mq_0.75$w[i])
    text(261.2, min(mq_0.5$w[i]), "0.5MQ", pos = 4, cex = 0.6)
    text(261.2, min(a$w[i]), "a", pos = 4, cex = 0.6)
    text(261.2, min(mq_0.75$w[i]), "0.75MQ", pos = 4, cex = 0.6)
    
    # legend
    # legend("topright", 
    #        pch = 21, col = "darkblue", pt.bg = "darkblue", pt.cex = 1,
    #        legend = "water level", cex = 0.7, bty = "n")
    legend("topright", 
           col = c("darkblue", "black"), 
           pch = c(21, NA), 
           pt.bg = c("darkblue", NA), 
           pt.cex = c(1, NA), 
           lty = c(0, 1), 
           legend = c("water level", "FLYS water levels"), 
           text.font = c(1, 1), 
           text.col = c(1, 1),
           cex = 0.7, bty = "n")
}

# Fig. 2e
{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(ylim_min, ylim_max), xaxt = "n", xlab = NA, ylab = NA)
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.52)
    boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7, 
                 border = FALSE)
    boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7, 
                 border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    boxed.labels(259.6, 55.5, "Mulde River", cex = 0.7, border = FALSE,
                 col = "blue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # weighting
    text(gs$km_qps[id][1], gs$wl[id][1], round(gs$weight_up[id][1], 2), pos = 4,
         font = 2, col = "darkblue")
    text(gs$km_qps[id][2], gs$wl[id][2], round(gs$weight_do[id][2], 2), pos = 2,
         font = 2, col = "darkblue")
    
    # FLYS
    i <- which(mq_0.5$station >= gs$km_qps[2] & mq_0.5$station <= gs$km_qps[3])
    lines(mq_0.5$station[i], mq_0.5$w[i])
    lines(mq_0.75$station[i], mq_0.75$w[i], col = "red")
    text(261.2, min(mq_0.5$w[i]), "0.5MQ", pos = 4, cex = 0.6)
    text(257.8, max(mq_0.5$w[i]), "0", pos = 2, font = 2)
    text(261.2, min(mq_0.75$w[i]), "0.75MQ", pos = 4, cex = 0.6, col = "red")
    text(257.8, max(mq_0.75$w[i]), "1", pos = 2, font = 2, col = "red")
    
    # legend
    legend("topright", 
           col = c("darkblue", "darkblue", "red", "black"), 
           pch = c(21, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA), 
           lty = c(0, 0, 1, 1), 
           legend = c("water level", "weighting", "upper FLYS water level", 
                      "lower FLYS water level"), 
           text.font = c(1, 2, 1, 1), 
           text.col = c(1, "darkblue", 1, 1),
           cex = 0.7, bty = "n")
}

# Fig. 2f
{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(-0.1, 1.1), xlab = NA, ylab = NA, axes = FALSE)
    axis(4, at = c(0, 0.5, 1))
    mtext("relative weighting", side = 4, line = 3, cex = 1)
    
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    boxed.labels(gs$km_qps[2], -0.05, gs$gauging_station[2], cex = 0.7, 
                 border = FALSE)
    boxed.labels(gs$km_qps[3], 1.05, gs$gauging_station[3], cex = 0.7, 
                 border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    boxed.labels(259.6, 1.05, "Mulde River", cex = 0.7, border = FALSE,
                 col = "blue")
    
    # weighting
    lines(x = c(gs$km_qps[id][1], gs$km_qps[id][2]),
          y = c(gs$weight_up[id][1], gs$weight_do[id][2]))
    points(gs$km_qps[id][1], gs$weight_up[id][1], pch = 21, col = 1, bg = 1)
    points(gs$km_qps[id][2], gs$weight_do[id][2], pch = 21, col = 1, bg = 1)
    box()
}

# Fig. 2g
{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(ylim_min, ylim_max), xaxt = "n", xlab = NA, ylab = NA)
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7, 
                 border = FALSE)
    boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7, 
                 border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    boxed.labels(259.6, 55.5, "Mulde River", cex = 0.7, border = FALSE,
                 col = "blue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # FLYS
    i <- which(mq_0.5$station >= gs$km_qps[2] & mq_0.5$station <= gs$km_qps[3])
    lines(mq_0.5$station[i], mq_0.5$w[i])
    lines(mq_0.75$station[i], mq_0.75$w[i], col = "red")
    lines(wldf1$station[i], wldf1$w[i], col = "darkblue")
    text(261.2, min(mq_0.5$w[i]), "0.5MQ", pos = 4, cex = 0.6)
    text(261.2, min(mq_0.75$w[i]), "0.75MQ", pos = 4, cex = 0.6, col = "red")
    
    # weighting
    text(gs$km_qps[id][1], gs$wl[id][1], round(gs$weight_up[id][1], 2), pos = 2,
         cex = 0.6, font = 2, col = "darkblue")
    text(gs$km_qps[id][2], gs$wl[id][2], round(gs$weight_do[id][2], 2), pos = 4,
         cex = 0.6, font = 2, col = "darkblue")
    
    # legend
    legend("topright", 
           col = c("darkblue", "darkblue", "darkblue", "red", "black"), 
           pch = c(21, NA, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA, NA), 
           lty = c(0, 0, 1, 1, 1), 
           lwd = c(0, 0, 1, 1, 1),
           legend = c("water level", "weighting", "waterLevel",
                      "upper FLYS water level", "lower FLYS water level"), 
           text.col = c(1, "darkblue", 1, 1, 1),
           text.font = c(1, 2, 1, 1, 1),
           cex = 0.7, bty = "n")
    box()
}

# Fig. 2h
{
    plotShiny(wldf1, TRUE, TRUE, TRUE, xlim = c(xlim_min, xlim_max),
              ylim = c(ylim_min, ylim_max), xlab = "river kilometer (km)", 
              ylab = NA, srt = 0, cex.lab = 1.4)
    
    # landmark
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    boxed.labels(259.6, 55.5, "Mulde River", cex = 0.7, border = FALSE,
                 col = "blue")
    
    # legend
    legend("topright", 
           col = c("darkblue", "darkblue", "darkblue", "red", "black"), 
           pch = c(21, NA, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA, NA), 
           lty = c(0, 0, 1, 1, 1), 
           lwd = c(0, 0, 1, 1, 1),
           legend = c("water level", "weighting", "waterLevel",
                      "upper FLYS water level", "lower FLYS water level"), 
           text.col = c(1, "darkblue", 1, 1, 1),
           text.font = c(1, 2, 1, 1, 1),
           cex = 0.7, bty = "n")
    box()
}
dev.off()

png("vignettes/paper_plot.png", width = 16, height = 40, units = "cm",
    pointsize = 12, res = 600)
par(mfrow = c(8, 1), oma = c(0.5, 3, 0.5, 3), mar = c(2, 2, 0.2, 2))
# Fig. 2a
{
    plot(1, 1, type = "n", xlim = c(243, 277), ylim = c(51, 58),
         xaxt = "n", ylab = NA, yaxt = "n", xlab = NA)
    axis(2, at = c(52, 54, 56, 58))
    
    # berechnungsstrecke
    polygon(x = c(257, 262, 262, 257),
            y = c(50.8, 50.8, 58.2, 58.2),
            col = "grey95", border = NA)
    
    # landmarks
    abline(v = gs$km_qps, lty = 3, lwd = 0.5)
    text(gs$km_qps[1:2], c(52, 52), gs$gauging_station[1:2], cex = 0.7)
    text(gs$km_qps[3:4], c(57, 57), gs$gauging_station[3:4], cex = 0.7)
    
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    text(259.6, 56, "Mulde River", cex = 0.7, col = "blue")
    
}

# Fig. 2b
{
    plot(1, 1, type = "n", xlim = c(243, 277), ylim = c(51, 58),
         xaxt = "n", ylab = NA, yaxt = "n", xlab = NA)
    axis(2, at = c(52, 54, 56, 58))
    
    # stretch
    polygon(x = c(257, 262, 262, 257),
            y = c(50.8, 50.8, 58.2, 58.2),
            col = "grey95", border = NA)
    
    # landmarks
    abline(v = gs$km_qps, lty = 3, lwd = 0.5)
    text(gs$km_qps[1:2], c(52, 52), gs$gauging_station[1:2], cex = 0.7)
    text(gs$km_qps[3:4], c(57, 57), gs$gauging_station[3:4], cex = 0.7)
    
    # gauging data
    points(gs$km_qps, gs$wl, pch = 21, col = "darkblue", bg = "darkblue")
    
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    text(259.6, 56, "Mulde River", cex = 0.7, col = "blue")
    
    # legend
    legend("topright", 
           pch = 21, col = "darkblue", pt.bg = "darkblue", pt.cex = 1,
           legend = "water level", cex = 0.7, bty = "o", box.col = "white")
    box()
}

# Fig. 2c
{
    plot(1, 1, type = "n", xlim = c(243, 277), 
         ylim = c(51, 58), xlab = NA, yaxt = "n", ylab = NA)
    axis(2, at = c(52, 54, 56, 58))
    
    # stretch
    polygon(x = c(257, 262, 262, 257),
            y = c(50.8, 50.8, 58.2, 58.2),
            col = "grey95", border = NA)
    
    # sections
    for (i in 1:(nrow(gs) - 1)) {
        rect(gs$km_qps[i], 53, gs$km_qps[i + 1], 54, col = "grey85",
             border = NA)
        text((gs$km_qps[i] + gs$km_qps[i + 1])/2, 53.5, i, font = 2)
        lines(rep(gs$km_qps[i], 2), c(53, 54), lwd = 2)
        lines(rep(gs$km_qps[i + 1], 2), c(53, 54), lwd = 2)
    }
    
    # landmarks
    abline(v = gs$km_qps, lty = 3, lwd = 0.5)
    text(gs$km_qps[1:2], c(52, 52), gs$gauging_station[1:2], cex = 0.7)
    text(gs$km_qps[3:4], c(57, 57), gs$gauging_station[3:4], cex = 0.7)
    
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    text(259.6, 56, "Mulde River", cex = 0.7, col = "blue")
    
    # gauging data
    points(gs$km_qps, gs$wl, pch = 21, col = "darkblue", bg = "darkblue")
    
    # legend
    legend("topright", 
           pch = 21, col = "darkblue", pt.bg = "darkblue", pt.cex = 1,
           legend = "water level", cex = 0.7, bty = "o", box.col = "white")
    box()
}

# Fig. 2d
{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(ylim_min, ylim_max), xlab = NA, ylab = "elevation (m a.s.l.)",
         xaxt = "n", cex.lab = 1.4)
    
    for (a_wl in flys3_water_levels){
        wldf_temp <- waterLevelFlys3(wldf, a_wl)
        lines(wldf_temp$station, wldf_temp$w, lty = 3, lwd = 0.2)
    }
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7, 
                 border = FALSE)
    boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7, 
                 border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    boxed.labels(259.6, 55.5, "Mulde River", cex = 0.7, border = FALSE,
                 col = "blue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # FLYS
    i <- which(mq_0.5$station >= gs$km_qps[2] & mq_0.5$station <= gs$km_qps[3])
    lines(mq_0.5$station[i], mq_0.5$w[i])
    lines(mq_0.75$station[i], mq_0.75$w[i])
    text(261.2, min(mq_0.5$w[i]), "0.5MQ", pos = 4, cex = 0.6)
    text(261.2, min(a$w[i]), "a", pos = 4, cex = 0.6)
    text(261.2, min(mq_0.75$w[i]), "0.75MQ", pos = 4, cex = 0.6)
    
    # legend
    # legend("topright", 
    #        pch = 21, col = "darkblue", pt.bg = "darkblue", pt.cex = 1,
    #        legend = "water level", cex = 0.7, bty = "n")
    legend("topright", 
           col = c("darkblue", "black"), 
           pch = c(21, NA), 
           pt.bg = c("darkblue", NA), 
           pt.cex = c(1, NA), 
           lty = c(0, 1), 
           legend = c("water level", "FLYS water levels"), 
           text.font = c(1, 1), 
           text.col = c(1, 1),
           cex = 0.7, bty = "n")
}

# Fig. 2e
{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(ylim_min, ylim_max), xaxt = "n", xlab = NA, ylab = NA)
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.52)
    boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7, 
                 border = FALSE)
    boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7, 
                 border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    boxed.labels(259.6, 55.5, "Mulde River", cex = 0.7, border = FALSE,
                 col = "blue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # weighting
    text(gs$km_qps[id][1], gs$wl[id][1], round(gs$weight_up[id][1], 2), pos = 4,
         font = 2, col = "darkblue")
    text(gs$km_qps[id][2], gs$wl[id][2], round(gs$weight_do[id][2], 2), pos = 2,
         font = 2, col = "darkblue")
    
    # FLYS
    i <- which(mq_0.5$station >= gs$km_qps[2] & mq_0.5$station <= gs$km_qps[3])
    lines(mq_0.5$station[i], mq_0.5$w[i])
    lines(mq_0.75$station[i], mq_0.75$w[i], col = "red")
    text(261.2, min(mq_0.5$w[i]), "0.5MQ", pos = 4, cex = 0.6)
    text(257.8, max(mq_0.5$w[i]), "0", pos = 2, font = 2)
    text(261.2, min(mq_0.75$w[i]), "0.75MQ", pos = 4, cex = 0.6, col = "red")
    text(257.8, max(mq_0.75$w[i]), "1", pos = 2, font = 2, col = "red")
    
    # legend
    legend("topright", 
           col = c("darkblue", "darkblue", "red", "black"), 
           pch = c(21, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA), 
           lty = c(0, 0, 1, 1), 
           legend = c("water level", "weighting", "upper FLYS water level", 
                      "lower FLYS water level"), 
           text.font = c(1, 2, 1, 1), 
           text.col = c(1, "darkblue", 1, 1),
           cex = 0.7, bty = "n")
}

# Fig. 2f
{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(-0.1, 1.1), xlab = NA, ylab = NA, axes = FALSE)
    axis(4, at = c(0, 0.5, 1))
    mtext("relative weighting", side = 4, line = 3, cex = 1)
    
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    boxed.labels(gs$km_qps[2], -0.05, gs$gauging_station[2], cex = 0.7, 
                 border = FALSE)
    boxed.labels(gs$km_qps[3], 1.05, gs$gauging_station[3], cex = 0.7, 
                 border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    boxed.labels(259.6, 1.05, "Mulde River", cex = 0.7, border = FALSE,
                 col = "blue")
    
    # weighting
    lines(x = c(gs$km_qps[id][1], gs$km_qps[id][2]),
          y = c(gs$weight_up[id][1], gs$weight_do[id][2]))
    points(gs$km_qps[id][1], gs$weight_up[id][1], pch = 21, col = 1, bg = 1)
    points(gs$km_qps[id][2], gs$weight_do[id][2], pch = 21, col = 1, bg = 1)
    box()
}

# Fig. 2g
{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(ylim_min, ylim_max), xaxt = "n", xlab = NA, ylab = NA)
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7, 
                 border = FALSE)
    boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7, 
                 border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    boxed.labels(259.6, 55.5, "Mulde River", cex = 0.7, border = FALSE,
                 col = "blue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # FLYS
    i <- which(mq_0.5$station >= gs$km_qps[2] & mq_0.5$station <= gs$km_qps[3])
    lines(mq_0.5$station[i], mq_0.5$w[i])
    lines(mq_0.75$station[i], mq_0.75$w[i], col = "red")
    lines(wldf1$station[i], wldf1$w[i], col = "darkblue")
    text(261.2, min(mq_0.5$w[i]), "0.5MQ", pos = 4, cex = 0.6)
    text(261.2, min(mq_0.75$w[i]), "0.75MQ", pos = 4, cex = 0.6, col = "red")
    
    # weighting
    text(gs$km_qps[id][1], gs$wl[id][1], round(gs$weight_up[id][1], 2), pos = 2,
         cex = 0.6, font = 2, col = "darkblue")
    text(gs$km_qps[id][2], gs$wl[id][2], round(gs$weight_do[id][2], 2), pos = 4,
         cex = 0.6, font = 2, col = "darkblue")
    
    # legend
    legend("topright", 
           col = c("darkblue", "darkblue", "darkblue", "red", "black"), 
           pch = c(21, NA, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA, NA), 
           lty = c(0, 0, 1, 1, 1), 
           lwd = c(0, 0, 1, 1, 1),
           legend = c("water level", "weighting", "waterLevel",
                      "upper FLYS water level", "lower FLYS water level"), 
           text.col = c(1, "darkblue", 1, 1, 1),
           text.font = c(1, 2, 1, 1, 1),
           cex = 0.7, bty = "n")
    box()
}

# Fig. 2h
{
    plotShiny(wldf1, TRUE, TRUE, TRUE, xlim = c(xlim_min, xlim_max),
              ylim = c(ylim_min, ylim_max), xlab = "river kilometer (km)", 
              ylab = NA, srt = 0, cex.lab = 1.4)
    
    # landmark
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    boxed.labels(259.6, 55.5, "Mulde River", cex = 0.7, border = FALSE,
                 col = "blue")
    
    # legend
    legend("topright", 
           col = c("darkblue", "darkblue", "darkblue", "red", "black"), 
           pch = c(21, NA, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA, NA), 
           lty = c(0, 0, 1, 1, 1), 
           lwd = c(0, 0, 1, 1, 1),
           legend = c("water level", "weighting", "waterLevel",
                      "upper FLYS water level", "lower FLYS water level"), 
           text.col = c(1, "darkblue", 1, 1, 1),
           text.font = c(1, 2, 1, 1, 1),
           cex = 0.7, bty = "n")
    box()
}
dev.off()

#q("no")
