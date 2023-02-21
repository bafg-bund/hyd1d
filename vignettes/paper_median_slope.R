

wldfr <- WaterLevelDataFrame(river = "Rhine",
                             time = as.POSIXct(NA),
                             station_int = as.integer(seq(336200, 865700, by = 100)))
wldfr <- waterLevelFlys3(wldfr, "MQ")
wldfr$river <- "Rhine"
wldfr$diffw <- c(0, diff(wldfr$w, 1))
hist(wldfr$diffw, freq = TRUE)
mean(wldfr$diffw)
max(wldfr$diffw)
min(wldfr$diffw)
wldfr$diffs <- c(0, diff(wldfr$station, 1))
hist(wldfr$diffs, freq = TRUE)
mean(wldfr$diffs)
max(wldfr$diffs)
min(wldfr$diffs)

wldfe <- WaterLevelDataFrame(river = "Elbe",
                             time = as.POSIXct(NA),
                             station_int = as.integer(seq(0, 585700, by = 100)))
wldfe <- waterLevelFlys3(wldfe, "MQ")
wldfe$river <- "Elbe"
wldfe$diffw <- c(0, diff(wldfe$w, 1))
hist(wldfe$diffw, freq = TRUE)
mean(wldfe$diffw)
max(wldfe$diffw)
min(wldfe$diffw)
wldfe$diffs <- c(0, diff(wldfe$station, 1))
hist(wldfe$diffs, freq = TRUE)
mean(wldfe$diffs)
max(wldfe$diffs)
min(wldfe$diffs)

res <- rbind(as.data.frame(wldfr), as.data.frame(wldfe))

summary <- boxplot(diff ~ river, res, main = NULL)
