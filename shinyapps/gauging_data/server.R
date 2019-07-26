# load the necessary packages
library(shiny)
library(leaflet)

function(input, output, session){
    
    # responsive menu
    output$menu_gauging_station <- renderUI({
        
        # subset df.gauging_station
        gauging_stations <- subset(df.gauging_stations[order(df.gauging_stations$km),],
                                   water_longname == input$river,
                                   gauging_station_longname)[,1]
        
        selectInput(inputId  = "gauging_station",
                    label    = "Pegel:",
                    choices  = gauging_stations,
                    selected = NULL)
    })
    
    # responsive title
    output$title <- renderText(paste("Pegel ", input$gauging_station))
    
    # responsive dataset
    df.data <- reactive({subset(df.gauging_data, 
                                gauging_station == input$gauging_station & 
                                date >= input$daterange[1] & 
                                date <= input$daterange[2])[2:ncol(df.gauging_data)]})
    
    # responsive plot
    output$plot <- renderPlot({
        
        # subset df.gauging_data
        df.data <- subset(df.gauging_data,
                          gauging_station == input$gauging_station &
                          date >= input$daterange[1] &
                          date <= input$daterange[2],
                          select = c(date, w))
        
        id <- which(df.gauging_stations$gauging_station == input$gauging_station)
        
        # missing data
        dates <- seq(input$daterange[1], input$daterange[2], by = "1 day")
        missing_dates <- dates[which(! dates %in% df.data$date)]
        
        # plot
        par(oma=c(2, 2, 0.1, 0.1), mar=c(4, 4, 1, 1))
        plot(w/100 ~ date, data=df.data, 
             type = "l", 
             col = "darkblue", 
             xlab = "Zeit", 
             ylab = "Wasserstand (m)")
        points(missing_dates, rep(max(df.data$w / 100), length(missing_dates)),
               pch = 4 , col = "red")
        abline(h = df.gauging_stations$mw[id], lty=3)
        boxed.labels(max(df.data$date), 
                     df.gauging_stations$mw[id], 
                     "MW",
                     bg="white",
                     border=FALSE, 
                     xpad = 1.5)
    })
    
    # responsive downloadData
    output$downloadData <- downloadHandler(filename = function(){paste0('waterlevel_', 
                                                                        gsub("Ä", "AE", gsub("Ö", "OE", gsub("Ü", "UE", input$gauging_station))), 
                                                                        '_', 
                                                                        strftime(input$daterange[1], format="%Y%m%d"),
                                                                        '-',
                                                                        strftime(input$daterange[2], format="%Y%m%d"),
                                                                        '.csv')},
                                           content = function(file){write.csv(df.data(),
                                                                              file,
                                                                              row.names = FALSE,
                                                                              fileEncoding = "UTF-8")}
                                           )
    
    # responsive table
    output$table <- renderTable(exp = data.frame(parameter=c("Amtsbezirk", "Flusskilometer", "Pegelnullpunkt (Höhe über NN (m))", "MW (m)", "Bezugszeitraum des MW"),
                                                 werte=t(df.gauging_stations[which(df.gauging_stations$gauging_station == input$gauging_station), c("agency", "km", "pnp", "mw", "mw_timespan")])),
                                rownames = FALSE,
                                colnames = FALSE,
                                align    = "lr"
    )
    
    # responsive map
    output$map <- renderLeaflet({
        
        leaflet() %>%
            addProviderTiles("Stamen.TonerLite",
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addCircleMarkers(data        = spdf.gauging_stations[which(spdf.gauging_stations@data$gauging_station_longname != input$gauging_station),],
                             popup       = spdf.gauging_stations@data$gauging_station_longname[which(spdf.gauging_stations@data$gauging_station_longname != input$gauging_station)],
                             radius      = 0.5,
                             color       = "green",
                             opacity     = 1,
                             fill        = TRUE,
                             fillCol     = "green",
                             fillOpacity = 1) %>%
            addCircleMarkers(data        = spdf.gauging_stations[which(spdf.gauging_stations@data$gauging_station_longname == input$gauging_station),],
                             radius      = 5,
                             color       = "darkblue",
                             opacity     = 1,
                             fill        = TRUE,
                             fillCol     = "darkblue",
                             fillOpacity = 1)
    })
    
    session$onSessionEnded(function() {
        dbDisconnect(p_con)
    })
    
}
