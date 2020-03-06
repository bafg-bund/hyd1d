# load the necessary packages
library(shiny)
library(shinyjs)
library(shiny.i18n)
library(leaflet)
library(lubridate)

function(input, output, session){
    
    # determine browser language
    runjs(jscode)
    german <- reactive({de(input$lang)})
    i18n <- reactive({
        if (length(input$lang) > 0 && german()) {
            translator$set_translation_language("de")
        } else {
            translator$set_translation_language("en")
        }
        translator
    })
    
    # titlepanel
    output$titlepanel <- renderUI(
        titlePanel(i18n()$t("Gauging data for waterLevel()")))
    
    # menutitle
    output$menu_title <- renderUI(h3(i18n()$t("Menu")))
    
    # responsive menu
    output$menu_river <- renderUI({
        selectInput(
            inputId  = "river",
            label    = i18n()$t("River:"),
            choices  = rivers,
            selected = "ELBE"
        )
    })
    
    # responsive menu
    output$menu_gauging_station <- renderUI({
        
        # subset df.gauging_station
        gauging_stations <- subset(df.gsd[order(df.gsd$km),],
                                   river == input$river,
                                   gauging_station)[,1]
        
        selectInput(inputId  = "gauging_station",
                    label    = i18n()$t("Gauging station:"),
                    choices  = gauging_stations,
                    selected = NULL)
    })
    
    # responsive menu
    output$menu_daterange <- renderUI({
        if (german()) {
            label <- paste0(" (01.01.1990 - ",
                            strftime(yesterday, format="%d.%m.%Y"), "):")
        } else {
            label <- paste0(" (1990-01-01 - ",
                            strftime(yesterday, format="%Y-%m-%d"), "):")
        }
        
        dateRangeInput(
            inputId   = "daterange", 
            label     = paste0(i18n()$t("Time period"), label),
            start     = as.character(yesterday - 365),
            end       = as.character(yesterday),
            min       = "1990-01-01",
            max       = as.character(yesterday),
            format    = ifelse(german(), "dd.mm.yyyy", "yyyy-mm-dd"),
            language  = ifelse(german(), "de", "en"),
            separator = " - "
        )
    })
    
    # responsive title
    output$title <- renderUI({
        req(input$gauging_station)
        h1( paste0(i18n()$t("Gauging station"), " ", input$gauging_station),
            style="color: darkblue;")
    })
    
    # responsive table
    output$table_title <- renderUI(h3(i18n()$t("Master data")))
    output$table <- renderTable({
            req(input$gauging_station)
            data.frame(
                parameter = c(i18n()$t("Administrative district"),
                              i18n()$t("River station"), 
                              i18n()$t("Gauge zero (height a.s.l. (m))"),
                              i18n()$t("MW (mean water, m)"),
                              i18n()$t("Reference period for MW")),
                werte = t(df.gsd[which(df.gsd$gauging_station == 
                                           input$gauging_station), 
                                 c("agency", "km", "pnp", "mw",
                                   "mw_timespan")]))
        },
        rownames = FALSE, colnames = FALSE, align = "lr")
    
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
    output$plot_title <- renderUI(h3(i18n()$t("Time series")))
    
    output$plot <- renderPlot({
        
        req(input$gauging_station)
        req(input$daterange)
        
        # df.gsd id
        id <- which(df.gsd$gauging_station == input$gauging_station)
        
        # missing data
        dates <- seq(input$daterange[1], input$daterange[2], by = "1 day")
        missing_dates <- dates[which(! dates %in% df.data()$date)]
        month_ceil <- ceiling_date(input$daterange[1], "month")
        month_floo <- floor_date(input$daterange[2], "month")
        
        # plot
        par(oma=c(2, 2, 0.1, 0.1), mar=c(4, 4, 1, 1))
        plot(w/100 ~ date, data = df.data(), type = "l", col = "darkblue",
             xlab = i18n()$t("time"), ylab = i18n()$t("water level (m)"),
             xlim = c(input$daterange[1], input$daterange[2]), xaxt="n")
        points(missing_dates, rep(max(df.data()$w / 100),
                                  length(missing_dates)),
               pch = 4 , col = "red")
        abline(h = df.gsd$mw[id], lty=3)
        axis.Date(1, at = seq(month_ceil, month_floo, by="1 mon"),
                  format = ifelse(german(), "%m.%Y", "%m-%Y"))
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
    
    # responsive map
    output$map_title <- renderUI({h3(i18n()$t("Location"))})
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
