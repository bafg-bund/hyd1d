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
#' @param add_flys \code{logical} determining whether the used FLYS3
#'   water levels should be plotted.
#' @param add_flys_labels \code{logical} determining whether the used FLYS3
#'   water levels should be labelled.
#' @param add_weighting \code{logical} determining whether the weighting of
#'   gauging data at the gauging stations should be labelled.
#' @param \dots further graphical parameters passed to 
#'   \code{\link[graphics]{plot.default}}.
#' 
#' @return A plot of a \linkS4class{WaterLevelDataFrame}.
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
    # FLYS preprocessing
    if (add_flys){
        # obtain the relevant FLYS water level data
        df.flys <- data.frame(station = wldf$station,
                              station_int = wldf$station_int,
                              section = wldf$section)
        
        flys_wls <- unique(c(as.matrix(df.gs[,c("name_wl_below_w_do",
                                                "name_wl_above_w_do",
                                                "name_wl_below_w_up",
                                                "name_wl_above_w_up")])))
        flys_wls <- flys_wls[!(is.na(flys_wls))]
        for (a_wls in flys_wls){
            # query the FLYS data from the DB
            wldf_flys <- waterLevelFlys3(wldf, a_wls)
            # bind the w column to df.flys
            temp_names <- names(df.flys)
            df.flys <- cbind(df.flys, wldf_flys$w)
            df.flys_names <- c(temp_names, a_wls)
            names(df.flys) <- df.flys_names
        }
        
        # set ylim depending on flys water levels 
        ylim_max <- max(df.flys[, 4:ncol(df.flys)])
        ylim_min <- min(df.flys[, 4:ncol(df.flys)])
        
    } else {
        
        # set ylim depending on w
        ylim_max <- max(wldf$w)
        ylim_min <- min(wldf$w)
        
    }
    
    #####
    # ...
    dots <- list(...)
    
    ###
    # modify known plot.default variables
    # xlim
    if (!("xlim" %in% names(dots))){
        if (!(any(df.gs$km_qps >= min(df.gs$km_qps) & 
                  df.gs$km_qps <= max(df.gs$km_qps)))){
            if (nrow(df.gs) == 2){
                dots$xlim <- c(min(df.gs$km_qps), max(df.gs$km_qps))
            } else {
                dots$xlim <- c(min(wldf$station), max(wldf$station))
            }
        } else {
            dots$xlim <- c(min(wldf$station), max(wldf$station))
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
    if (add_flys) {
        dots$flys_wls <- flys_wls
        dots$df.flys <- df.flys
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
    
    add_flys <- dots$add_flys
    dots$add_flys <- NULL
    
    flys_wls <- dots$flys_wls
    dots$flys_wls <- NULL
    
    df.flys <- dots$df.flys
    dots$df.flys <- NULL
    
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
            graphics::lines(df.flys$station, df.flys[, a_wls],
                            lty = 1, lwd = 0.3, col = "grey60")
        }
        sections <- unique(wldf$section)
        if (length(sections) > 1){
            for (s in sections){
                df.flys_temp <- df.flys[which(df.flys$section == s), ]
                # lower wl
                name_below <- df.gs$name_wl_below_w_up[s]
                station_below <- df.gs$km_qps[s + 1]
                w_below <- df.gs$w_wl_below_w_do[s + 1]
                df.temp_below <- data.frame(station = c(df.gs$km_qps[s],
                                                        df.flys_temp$station,
                                                        station_below), 
                                            w   = c(df.gs$w_wl_below_w_up[s],
                                                    df.flys_temp[, name_below],
                                                    w_below))
                df.temp_below <- df.temp_below[
                    df.temp_below$station >= dots$xlim[1] & 
                        df.temp_below$station <= dots$xlim[2], ]
                
                # upper wl
                name_above <- df.gs$name_wl_above_w_up[s]
                station_above <- df.gs$km_qps[s + 1]
                w_above <- df.gs$w_wl_above_w_do[s + 1]
                df.temp_above <- data.frame(station = c(df.gs$km_qps[s],
                                                        df.flys_temp$station,
                                                        df.gs$km_qps[s + 1]), 
                                            w = c(df.gs$w_wl_above_w_up[s],
                                                  df.flys_temp[, name_above],
                                                  df.gs$w_wl_above_w_do[s + 1]))
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
                graphics::lines(df.temp_below$station, df.temp_below$w, 
                                lwd = 0.6)
                graphics::lines(df.temp_above$station, df.temp_above$w, 
                                lwd = 0.6, col = "red")
                
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
            name_below <- df.gs$name_wl_below_w_up
            df.temp_below <- data.frame(station = df.flys$station,
                                        w = df.flys[, name_below])
            df.temp_below <- df.temp_below[
                df.temp_below$station >= dots$xlim[1] & 
                    df.temp_below$station <= dots$xlim[2], ]
            
            # upper wl
            name_above <- df.gs$name_wl_above_w_up
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
            graphics::lines(df.temp_below$station, df.temp_below$w, lwd = 0.6)
            graphics::lines(df.temp_above$station, df.temp_above$w, lwd = 0.6, 
                            col = "red")
            
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
    # lines
    df.gs <- df.gs[df.gs$km_qps >= dots$xlim[1] & df.gs$km_qps <= dots$xlim[2],]
    if (nrow(df.gs) > 0){
        for (i in 1:nrow(df.gs)){
            graphics::lines(rep(df.gs$km_qps[i], 2), dots$ylim, lty = 3, 
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
                                      xpad = 4, ypad = 0.7, cex = 0.7)
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
                                      xpad = 4, ypad = 0.7, cex = 0.7)
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
}

.plot <- function(...){
    graphics::plot(...)
}

