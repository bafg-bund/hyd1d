#' @name plotShiny
#' @rdname plotShiny
#' @title Plot a WaterLevelDataFrame in Shiny
#' 
#' @description This convinience function enables the easy visualisation of 
#'   interpolated water levels stored as \linkS4class{WaterLevelDataFrame} using
#'   the \R package \href{https://CRAN.R-project.org/package=shiny}{shiny}. The 
#'   results of functions like \code{\link{waterLevel}} and
#'   \code{\link{waterLevelPegelonline}} can be plotted interactively so that 
#'   the computation process itself becomes visible.
#' 
#' @param wldf an object of class \linkS4class{WaterLevelDataFrame}.
#' @param add_flys \code{logical} determining whether the used 
#'   \href{http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html}{FLYS3}
#'   water levels should be plotted.
#' @param add_flys_labels \code{logical} determining whether the used 
#'   \href{http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html}{FLYS3}
#'   water levels should be labelled.
#' @param add_weighting \code{logical} determining whether the weighting of
#'   gauging data at the gauging stations should be labelled.
#' @param \dots further graphical parameters passed to 
#'   \code{\link[graphics]{plot.default}}.
#' 
#' @return A plot of a \linkS4class{WaterLevelDataFrame}.
#' 
#' @references 
#'   \insertRef{bundesanstalt_fur_gewasserkunde_flys_2016}{hyd1d}
#' 
#' @examples
#' wldf <- WaterLevelDataFrame(river   = "Elbe",
#'                             time    = as.POSIXct("2016-12-21"),
#'                             station = seq(257, 262, 0.1))
#' wldf <- waterLevel(wldf, shiny = TRUE)
#' plotShiny(wldf, TRUE, TRUE, TRUE)
#' 
#' @export
#' 
plotShiny <- function(wldf, add_flys = TRUE, add_flys_labels = TRUE,
                      add_weighting = TRUE, ...){
    
    #####
    # check basic requirements
    ##
    # wldf
    if (class(wldf) != "WaterLevelDataFrame"){
        stop("'wldf' must be type 'WaterLevelDataFrame'.")
    }
    if (!(all(names(wldf) == c("station", "station_int", "w", 
                                "section", "weight_x", "weight_y")))){
        stop(paste0("'wldf' needs to be computed by waterLevel() or",
                    " waterLevelPegelonline()\n  with parameter shiny = TRUE.",
                    " Since column wldf$section is missing,\n  it needs to ",
                    "be recomputed."))
    }
    
    # extract the gauging_station slot
    df.gs <- getGaugingStations(wldf)
    
    ##
    # add_flys
    if (!(missing(add_flys))){
        if (class(add_flys) != "logical"){
            stop("'add_flys' must be type 'logical'.")
        }
        if (length(add_flys) != 1){
            stop("'add_flys' must have a length equal 1.")
        }
    }
    
    ##
    # add_flys_labels
    if (!(missing(add_flys_labels))){
        if (class(add_flys_labels) != "logical"){
            stop("'add_flys_labels' must be type 'logical'.")
        }
        if (length(add_flys_labels) != 1){
            stop("'add_flys_labels' must have a length equal 1.")
        }
    }
    
    ##
    # add_weighting
    if (!(missing(add_weighting))){
        if (class(add_weighting) != "logical"){
            stop("'add_weighting' must be type 'logical'.")
        }
        if (length(add_weighting) != 1){
            stop("'add_weighting' must have a length equal 1.")
        }
    }
    
    #####
    # ...
    dots <- list(...)
    
    ###
    # modify known plot.default variables
    # xlim
    if (!("xlim" %in% names(dots))){
        dots$xlim <- c(min(df.gs$km_qps), max(df.gs$km_qps))
        ylim_set_x <- FALSE
    } else {
        ylim_set_x <- TRUE
    }
    
    #####
    # FLYS preprocessing
    # obtain the relevant FLYS water level data
    # for the wldf
    df.flys <- data.frame(station = wldf$station,
                          station_int = wldf$station_int,
                          section = wldf$section)
    
    # for the total stretch
    if (min(df.gs$km_qps) == min(wldf$station) |
        min(df.gs$km_qps) >= min(wldf$station) - 0.1) {
        station_up <- numeric()
    } else {
        station_up <- seq(min(df.gs$km_qps), min(wldf$station) - 0.1, 0.1)
    }
    if (max(df.gs$km_qps) == max(wldf$station) |
        max(df.gs$km_qps) + 0.1 <= max(wldf$station)) {
        station_do <- numeric()
    } else {
        station_do <- seq(max(wldf$station) + 0.1, max(df.gs$km_qps), 0.1)
    }
    station_total <- c(station_up, wldf$station, station_do)
    section_total <- c(rep(min(wldf$section), length(station_up)), 
                       wldf$section,
                       rep(max(wldf$section), length(station_do)))
    df.flys_total <- data.frame(station = station_total,
                                station_int = as.integer(station_total * 
                                                             1000),
                                section = section_total)
    wldf_total <- WaterLevelDataFrame(river = getRiver(wldf),
                                      time = as.POSIXct(NA),
                                      station_int = 
                                          as.integer(station_total * 1000))
    
    # obtain the flys water levels
    flys_wls <- unique(c(as.matrix(df.gs[,c("name_wl_below_w_do",
                                            "name_wl_above_w_do",
                                            "name_wl_below_w_up",
                                            "name_wl_above_w_up")])))
    flys_wls <- flys_wls[!(is.na(flys_wls))]
    flys_wl <- ifelse(length(flys_wls > 1), TRUE, FALSE)
    if (flys_wl) {
        for (a_wls in flys_wls){
            # query the FLYS data from the DB
            wldf_flys <- waterLevelFlys3(wldf, a_wls)
            # bind the w column to df.flys
            temp_names <- names(df.flys)
            df.flys <- cbind(df.flys, wldf_flys$w)
            df.flys_names <- c(temp_names, a_wls)
            names(df.flys) <- df.flys_names
            
            # query the FLYS data from the DB
            wldf_flys_total <- waterLevelFlys3(wldf_total, a_wls)
            # bind the w column to df.flys
            temp_names <- names(df.flys_total)
            df.flys_total <- cbind(df.flys_total, wldf_flys_total$w)
            df.flys_names <- c(temp_names, a_wls)
            names(df.flys_total) <- df.flys_names
        }
        
        if (ylim_set_x) {
            df.flys_total_s <- df.flys_total[
                which(df.flys_total$station >= dots$xlim[1] &
                      df.flys_total$station <= dots$xlim[2]), 
                4:ncol(df.flys_total)]
        }
        
        if (add_flys){
            if (ylim_set_x) {
                ylim_max <- max(df.flys_total_s)
                ylim_min <- min(df.flys_total_s)
            } else {
                ylim_max <- max(df.flys_total[, 4:ncol(df.flys_total)])
                ylim_min <- min(df.flys_total[, 4:ncol(df.flys_total)])
            }
        } else {
            if (ylim_set_x) {
                ylim_max <- max(df.flys_total_s)
                ylim_min <- min(df.flys_total_s)
            } else {
                ylim_max <- max(df.gs$wl, na.rm = TRUE)
                ylim_min <- min(df.gs$wl, na.rm = TRUE)
            }
        }
    } else {
        if (ylim_set_x) {
            ylim_max <- max(wldf$w)
            ylim_min <- min(wldf$w)
        } else {
            ylim_max <- max(df.gs$wl, na.rm = TRUE)
            ylim_min <- min(df.gs$wl, na.rm = TRUE)
        }
    }
    
    # ylim, y_gaugingstations_lab
    if (!("ylim" %in% names(dots))){
        y_gauging_station_lab_max <- ylim_max - (ylim_max - ylim_min) * 0.1
        y_gauging_station_lab_min <- ylim_min + (ylim_max - ylim_min) * 0.1
        ylim_max <- ylim_max + (ylim_max - ylim_min) * 0.2
        ylim_min <- ylim_min - (ylim_max - ylim_min) * 0.2
        dots$ylim <- c(ylim_min, ylim_max)
    } else {
        ylim_max <- max(dots$ylim)
        ylim_min <- min(dots$ylim)
        y_gauging_station_lab_max <- ylim_max - (ylim_max - ylim_min) * 0.1
        y_gauging_station_lab_min <- ylim_min + (ylim_max - ylim_min) * 0.1
    }
    
    # xlab
    if (!("xlab" %in% names(dots))){
        if (startsWith(Sys.getlocale(category = "LC_MESSAGES"), "de_DE")){
            dots$xlab <- "Flusskilometer (km)"
        } else {
            dots$xlab <- "river station (km)"
        }
    }
    
    # ylab
    if (!("ylab" %in% names(dots))) {
        if (startsWith(Sys.getlocale(category = "LC_MESSAGES"), "de_DE")){
            dots$ylab <- "H\u00f6he (m \u00fcber NHN (DHHN92))"
        } else {
            dots$ylab <- "elevation (m a.s.l. (DHHN92))"
        }
    }
    
    # type
    if ("type" %in% names(dots)){
        warning("'type' can not be set.")
        dots$type <- NULL
    }
    
    #####
    # append additional variables to dots
    dots$wldf <- wldf
    dots$add_flys <- add_flys
    if (add_flys & flys_wl) {
        dots$flys_wls <- flys_wls
        dots$df.flys <- df.flys
        dots$df.flys_total <- df.flys_total
    }
    dots$add_flys_labels <- add_flys_labels
    dots$y_gauging_station_lab_max <- y_gauging_station_lab_max
    dots$y_gauging_station_lab_min <- y_gauging_station_lab_min
    dots$add_weighting <- add_weighting
    
    do.call(.plotShiny, dots)
}


.plotShiny <- function(...){
    
    dots <- list(...)
    
    #####
    # remove the additional variables from dots
    wldf <- dots$wldf
    dots$wldf <- NULL
    df.gs <- getGaugingStations(wldf)
    gs_missing <- getGaugingStationsMissing(wldf)
    gs_missing <- unlist(strsplit(gs_missing, ": "))[2 * 1:length(gs_missing)]
    
    add_flys <- dots$add_flys
    dots$add_flys <- NULL
    
    flys_wls <- dots$flys_wls
    dots$flys_wls <- NULL
    
    df.flys <- dots$df.flys
    dots$df.flys <- NULL
    
    df.flys_total <- dots$df.flys_total
    dots$df.flys_total <- NULL
    
    add_flys_labels <- dots$add_flys_labels
    dots$add_flys_labels <- NULL
    
    y_gauging_station_lab_max <- dots$y_gauging_station_lab_max
    dots$y_gauging_station_lab_max <- NULL
    
    y_gauging_station_lab_min <- dots$y_gauging_station_lab_min
    dots$y_gauging_station_lab_min <- NULL
    
    add_weighting <- dots$add_weighting
    dots$add_weighting <- NULL
    
    dots$x <- wldf$station
    dots$y <- wldf$w
    dots$type <- "n" 
    
    #####
    # start with an empty plot
    do.call(.plot, dots)
    
    #####
    # add the flys waterlevels
    if (add_flys){
        for (a_wls in flys_wls){
            graphics::lines(df.flys_total$station, df.flys_total[, a_wls],
                            lty = 1, lwd = 0.3, col = "grey60")
        }
        sections <- unique(wldf$section)
        if (length(sections) > 1){
            for (s in sections){
                # subset df.flys and df.flys_total
                df.flys_temp <- df.flys[which(df.flys$section == s), ]
                df.flys_total_temp <- df.flys_total[
                                            which(df.flys_total$section == s), ]
                
                ## lower wl
                name_below <- df.gs$name_wl_below_w_up[s]
                
                # total
                graphics::lines(df.flys_total_temp$station, 
                                df.flys_total_temp[, name_below], lwd = 0.5)
                
                # in wldf
                df.temp_below <- data.frame(station = numeric(),
                                            w = numeric())
                station_below <- df.gs$km_qps[s]
                w_below <- df.gs$w_wl_below_w_up[s]
                if (df.gs$km_qps[s] >= min(wldf$station) & 
                    df.gs$km_qps[s] <= max(wldf$station)) {
                    df.temp_below <- rbind(df.temp_below,
                        data.frame(station = station_below,
                                   w = w_below))
                } else {
                    station_below <- min(wldf$station)
                    w_below <- df.flys[
                        which(df.flys$station == min(wldf$station)), name_below]
                }
                df.temp_below <- rbind(df.temp_below,
                    data.frame(station = df.flys_temp$station,
                               w = df.flys_temp[, name_below]))
                if (df.gs$km_qps[s + 1] >= min(wldf$station) & 
                    df.gs$km_qps[s + 1] <= max(wldf$station)) {
                    df.temp_below <- rbind(df.temp_below,
                        data.frame(station = df.gs$km_qps[s + 1],
                                   w = df.gs$w_wl_below_w_do[s + 1]))
                }
                df.temp_below <- df.temp_below[
                    df.temp_below$station >= dots$xlim[1] & 
                        df.temp_below$station <= dots$xlim[2], ]
                
                ## upper wl
                name_above <- df.gs$name_wl_above_w_up[s]
                
                # total
                graphics::lines(df.flys_total_temp$station, 
                                df.flys_total_temp[, name_above], 
                                lwd = 0.5, col = "red")
                
                # in wldf
                df.temp_above <- data.frame(station = numeric(),
                                            w = numeric())
                station_above <- df.gs$km_qps[s]
                w_above <- df.gs$w_wl_above_w_up[s]
                if (df.gs$km_qps[s] >= min(wldf$station) & 
                    df.gs$km_qps[s] <= max(wldf$station)) {
                    df.temp_above <- rbind(df.temp_above,
                        data.frame(station = station_above,
                                   w = w_above))
                } else {
                    station_above <- min(wldf$station)
                    w_above <- df.flys[
                        which(df.flys$station == min(wldf$station)), name_above]
                }
                df.temp_above <- rbind(df.temp_above,
                    data.frame(station = df.flys_temp$station,
                               w = df.flys_temp[, name_above]))
                if (df.gs$km_qps[s + 1] >= min(wldf$station) & 
                    df.gs$km_qps[s + 1] <= max(wldf$station)) {
                    df.temp_above <- rbind(df.temp_above,
                        data.frame(station = df.gs$km_qps[s + 1],
                                   w = df.gs$w_wl_above_w_do[s + 1]))
                }
                df.temp_above <- df.temp_above[
                    df.temp_above$station >= dots$xlim[1] & 
                        df.temp_above$station <= dots$xlim[2],]
                
                # add polygons and lines
                df.temp_poly <- data.frame(station = c(df.temp_below$station,
                                                   rev(df.temp_above$station)),
                                           w = c(df.temp_below$w,
                                                 rev(df.temp_above$w)))
                graphics::polygon(df.temp_poly$station, df.temp_poly$w,
                                  col = "grey95", border = NA)
                graphics::lines(df.temp_below$station, df.temp_below$w)
                graphics::lines(df.temp_above$station, df.temp_above$w, 
                                col = "red")
                
                # add letters
                if (add_flys_labels) {
                    if (s == max(sections)){
                        # recalculate coordinates for the last section
                        station_below <- df.gs$km_qps[s]
                        w_below <- df.gs$w_wl_below_w_up[s]
                        station_above <- df.gs$km_qps[s]
                        w_above <- df.gs$w_wl_above_w_up[s]
                        graphics::text(station_below, w_below, name_below, 
                                       pos = 4, offset = 0.5, cex = 0.6)
                        graphics::text(station_above, w_above, name_above, 
                                       pos = 4, offset = 0.5, cex = 0.6,
                                       col = "red")
                    } else {
                        graphics::text(station_below, w_below, name_below, 
                                       pos = 2, offset = 0.5, cex = 0.6)
                        graphics::text(station_above, w_above, name_above, 
                                       pos = 2, offset = 0.5, cex = 0.6, 
                                       col = "red")
                    }
                }
            }
        } else {
            # lower wl
            name_below <- stats::na.omit(df.gs$name_wl_below_w_up)
            
            # total
            graphics::lines(df.flys_total$station, 
                            df.flys_total[, name_below], lwd = 0.5)
            
            # in wldf
            df.temp_below <- data.frame(station = df.flys$station,
                                        w = df.flys[, name_below])
            df.temp_below <- df.temp_below[
                df.temp_below$station >= dots$xlim[1] & 
                    df.temp_below$station <= dots$xlim[2], ]
            
            # upper wl
            name_above <- stats::na.omit(df.gs$name_wl_above_w_up)
            
            # total
            graphics::lines(df.flys_total$station, 
                            df.flys_total[, name_above], 
                            lwd = 0.5, col = "red")
            
            df.temp_above <- data.frame(station = df.flys$station,
                                        w = df.flys[, name_above])
            df.temp_above <- df.temp_above[
                df.temp_above$station >= dots$xlim[1] & 
                    df.temp_above$station <= dots$xlim[2],]
            
            # add polygons and lines
            df.temp_poly <- data.frame(station = c(df.temp_below$station,
                                                   rev(df.temp_above$station)),
                                       w = c(df.temp_below$w,
                                             rev(df.temp_above$w)))
            graphics::polygon(df.temp_poly$station, df.temp_poly$w,
                              col = "grey95", border = NA)
            graphics::lines(df.temp_below$station, df.temp_below$w)
            graphics::lines(df.temp_above$station, df.temp_above$w, col = "red")
            
            # add letters
            if (add_flys_labels) {
                # recalculate coordinates for the last section
                station_below <- df.gs$km_qps
                w_below <- df.gs$w_wl_below_w_up
                station_above <- df.gs$km_qps
                w_above <- df.gs$w_wl_above_w_up
                graphics::text(station_below, w_below, name_below, pos = 4, 
                               offset = 0.5, cex = 0.6)
                graphics::text(station_above, w_above, name_above, pos = 4, 
                               offset = 0.5, cex = 0.6, col = "red")
            }
        }
    }
    
    #####
    # add the gauging station 
    ##
    # access the gauging_station_data
    get("df.gauging_station_data", pos = -1)
    for (a in c("gauging_station", "agency", "river")){
        df.gauging_station_data[, a] <- asc2utf8(df.gauging_station_data[, a])
    }
    id <- which(df.gauging_station_data$river == "RHEIN" & 
                df.gauging_station_data$km_qps < 336.2)
    df.gauging_station_data <- df.gauging_station_data[-id,]
    id <- which(df.gauging_station_data$river == toupper(getRiver(wldf)))
    df.gsm <- df.gauging_station_data[id,]
    
    #####
    # gauging_stations
    # get a data.frame of the relevant gauging stations between start and end
    id <- numeric()
    for (i in 1:nrow(df.gsm)) {
        if (df.gsm$gauging_station[i] %in% gs_missing) {
            id <- append(id, i)
        }
    }
    df.gsm <- df.gsm[id,]
    
    # lines
    df.gs <- df.gs[df.gs$km_qps >= dots$xlim[1] & df.gs$km_qps <= dots$xlim[2],]
    if (nrow(df.gs) > 0){
        for (i in 1:nrow(df.gs)){
            graphics::lines(rep(df.gs$km_qps[i], 2), dots$ylim, lty = 3, 
                            lwd = 0.5)
        }
    }
    df.gsm <- df.gsm[df.gsm$km_qps >= dots$xlim[1] & 
                     df.gsm$km_qps <= dots$xlim[2],]
    if (nrow(df.gsm) > 0){
        for (i in 1:nrow(df.gsm)){
            graphics::lines(rep(df.gsm$km_qps[i], 2), dots$ylim, lty = 3, 
                            lwd = 0.5)
        }
    }
    
    # labels
    id1 <- df.gs$km_qps >= min(dots$xlim) & df.gs$km_qps <= max(dots$xlim)
    for (i in 1:2){
        if (i == 1){
            id2 <- df.gs$km_qps <= (dots$xlim[1] + 
                                        (dots$xlim[2] - dots$xlim[1]) / 2)
            if (any(id1 & id2)){
                plotrix::boxed.labels(df.gs$km_qps[id1 & id2],
                                      rep(y_gauging_station_lab_min, 
                                          nrow(df.gs[id1 & id2, ])),
                                      df.gs$gauging_station[id1 & id2], 
                                      bg="white", srt = 90, border = FALSE, 
                                      xpad = 0.5, ypad = 0.5, cex = 0.7)
            }
        } else {
            id2 <- df.gs$km_qps > (dots$xlim[1] + 
                                       (dots$xlim[2] - dots$xlim[1]) / 2)
            if (any(id1 & id2)){
                plotrix::boxed.labels(df.gs$km_qps[id1 & id2],
                                      rep(y_gauging_station_lab_max, 
                                          nrow(df.gs[id1 & id2, ])),
                                      df.gs$gauging_station[id1 & id2],
                                      bg = "white", srt = 90, border = FALSE,
                                      xpad = 0.5, ypad = 0.5, cex = 0.7)
            }
        }
    }
    
    id3 <- df.gsm$km_qps >= min(dots$xlim) & df.gsm$km_qps <= max(dots$xlim)
    for (i in 1:2){
        if (i == 1){
            id4 <- df.gsm$km_qps <= (dots$xlim[1] + 
                                         (dots$xlim[2] - dots$xlim[1]) / 2)
            if (any(id3 & id4)){
                plotrix::boxed.labels(df.gsm$km_qps[id3 & id4],
                                      rep(y_gauging_station_lab_min, 
                                          nrow(df.gsm[id3 & id4, ])),
                                      df.gsm$gauging_station[id3 & id4], 
                                      bg="white", srt = 90, border = FALSE, 
                                      xpad = 0.5, ypad = 0.5, cex = 0.7)
            }
        } else {
            id4 <- df.gsm$km_qps > (dots$xlim[1] + 
                                        (dots$xlim[2] - dots$xlim[1]) / 2)
            if (any(id3 & id4)){
                plotrix::boxed.labels(df.gsm$km_qps[id3 & id4],
                                      rep(y_gauging_station_lab_max, 
                                          nrow(df.gsm[id3 & id4, ])),
                                      df.gsm$gauging_station[id3 & id4],
                                      bg = "white", srt = 90, border = FALSE,
                                      xpad = 0.5, ypad = 0.5, cex = 0.7)
            }
        }
    }
    
    #####
    # water level data
    graphics::lines(wldf$station, wldf$w, col = "darkblue")
    
    #####
    # gauging_data
    graphics::points(df.gs$km_qps[id1], df.gs$wl[id1], pch=21, col="darkblue",
                     bg="darkblue")
    
    #####
    # weighting
    if (add_weighting){
        df.gs <- df.gs[id1, ]
        if (nrow(df.gs) == 1) {
            graphics::text(x = df.gs$km_qps, y = df.gs$wl,
                           labels = round(df.gs$weight_up, 2), pos = 4, 
                           offset = 0.5, cex = 0.6, col = "darkblue")
        } else if (nrow(df.gs) > 1){
            for (i in 1:nrow(df.gs)) {
                graphics::text(x = df.gs$km_qps[i], y = df.gs$wl[i],
                               labels = round(df.gs$weight_do[i], 2),
                               pos = 2, offset = 0.5, cex = 0.6,
                               col = "darkblue")
                graphics::text(x = df.gs$km_qps[i], y = df.gs$wl[i],
                               labels = round(df.gs$weight_up[i], 2),
                               pos = 4, offset = 0.5, cex = 0.6,
                               col = "darkblue")
            }
        }
    }
    
    graphics::box()
}

.plot <- function(...){
    graphics::plot(...)
}

