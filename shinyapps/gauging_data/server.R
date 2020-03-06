# load the necessary packages
library(shiny)
library(leaflet)

function(input, output, session){
    
    # responsive menu
    output$menu_gauging_station <- renderUI({
        
        # subset df.gauging_station
        gauging_stations <- subset(df.gsd[order(df.gsd$km),],
                                   river == input$river,
                                   gauging_station)[,1]
        
        selectInput(inputId  = "gauging_station",
                    label    = "Pegel:",
                    choices  = gauging_stations,
                    selected = NULL)
    })
    
    # responsive title
    output$title <- renderText({
        req(input$gauging_station)
        paste("Pegel ", input$gauging_station)
    })
    
    # responsive dataset
    df.data <- reactive({
        req(input$gauging_station)
        req(input$daterange)
        df.gd[which(df.gd$gauging_station == 
                                  input$gauging_station & 
                    df.gd$date >= input$daterange[1] & 
                    df.gd$date <= input$daterange[2]),
              c("date", "w")]})
    
    # responsive plot
    output$plot <- renderPlot({
        
        req(input$gauging_station)
        req(input$daterange)
        
        # df.gsd id
        id <- which(df.gsd$gauging_station == input$gauging_station)
        
        # missing data
        dates <- seq(input$daterange[1], input$daterange[2], by = "1 day")
        missing_dates <- dates[which(! dates %in% df.data()$date)]
        
        # plot
        par(oma=c(2, 2, 0.1, 0.1), mar=c(4, 4, 1, 1))
        plot(w/100 ~ date, data = df.data(), type = "l", col = "darkblue",
             xlab = "Zeit", ylab = "Wasserstand (m)")
        points(missing_dates, rep(max(df.data()$w / 100),
                                  length(missing_dates)),
               pch = 4 , col = "red")
        abline(h = df.gsd$mw[id], lty=3)
        boxed.labels(max(df.data()$date), df.gsd$mw[id], "MW",
                     bg = "white", border = FALSE, xpad = 1.5)
    })
    
    # responsive downloadData
    output$downloadData <- downloadHandler(filename = function(){
        req(input$gauging_station)
        req(input$daterange)
        paste0('waterlevel_',
               gsub("Ä", "AE",
                    gsub("Ö", "OE", gsub("Ü", "UE", input$gauging_station))),
               '_',
               strftime(input$daterange[1], format="%Y%m%d"), '-',
               strftime(input$daterange[2], format="%Y%m%d"), '.csv')},
        content = function(file){write.csv(df.data(),
            file, row.names = FALSE, fileEncoding = "UTF-8")})
    
    # responsive table
    output$table <- renderTable(exp = 
        data.frame(
            parameter = c("Amtsbezirk", "Flusskilometer", 
                          "Pegelnullpunkt (Höhe über NN (m))", "MW (m)",
                          "Bezugszeitraum des MW"),
            werte = t(df.gsd[which(df.gsd$gauging_station == 
                                       input$gauging_station), 
                             c("agency", "km", "pnp", "mw", "mw_timespan")])),
        rownames = FALSE, colnames = FALSE, align = "lr")
    
    # responsive map
    output$map <- renderLeaflet({
        
        req(input$gauging_station)
        
        leaflet() %>%
            addProviderTiles("Stamen.TonerLite",
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(
                data = spdf.gsd[which(spdf.gsd@data$gauging_station != 
                                                 input$gauging_station),],
                popup = spdf.gsd@data$gauging_station[
                    which(spdf.gsd@data$gauging_station != 
                              input$gauging_station)],
                radius = 0.5, color = "green", opacity = 1, fill = TRUE,
                fillCol = "green", fillOpacity = 1) %>%
            addCircleMarkers(
                data = spdf.gsd[which(spdf.gsd@data$gauging_station == 
                                          input$gauging_station),],
                radius = 5, color = "darkblue", opacity = 1, fill = TRUE,
                fillCol = "darkblue", fillOpacity = 1)
    })
    
}
