function(input, output, session) {
    
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
    
    # responsive menues
    output$menu_river <- renderUI({
        selectInput(
            inputId  = "river",
            label    = i18n()$t("River:"),
            choices  = rivers,
            selected = "ELBE"
        )
    })
    
    output$menu_from_to <- renderUI({
        if (is.null(input$river)) {
            id <- 1
        } else {
            id <- which(df.from_to$river == input$river)
        }
        
        sliderInput(
            inputId = "from_to", 
            label   = i18n()$t("Kilometer (from - to):"),
            min     = df.from_to$from[id],
            max     = df.from_to$to[id],
            value   = c(df.from_to$from_val[id], df.from_to$to_val[id]),
            step    = 0.1
        )
    })
    
    output$menu_date <- renderUI({
        dateInput(
            inputId  = "date", 
            label    = i18n()$t("Date:"),
            min      = trunc(Sys.time() - as.difftime(31, units = "days"),
                             units = "days"),
            max      = ceiling_date(Sys.time(), "day"),
            value    = ceiling_date(Sys.time(), "day"),
            format   = ifelse(german(), "dd.mm.yyyy", "yyyy-mm-dd"),
            language = ifelse(german(), "de", "en")
        )
    })
    
    output$menu_time <- renderUI({
        timeInput(
            inputId = "time", 
            label   = i18n()$t("Time:"),
            value   = floor_date(Sys.time(), "15 minutes"),
            seconds = FALSE
        )
    })
    
    output$menu_flys <- renderUI({
        checkboxInput(
            inputId = "flys",
            label   = i18n()$t("FLYS water levels"),
            value = TRUE)
    })
    
    output$menu_weighting <- renderUI({
        checkboxInput(
            inputId = "weighting",
            label   = i18n()$t("Weighting at the gauging stations"),
            value = TRUE)
    })
    
    output$menu_xlim <- renderUI({
        checkboxInput(
            inputId = "xlim",
            label   = i18n()$t(paste0("Gauging stations relevant for the c",
                                      "omputation")),
            value = TRUE)
    })
    
    output$loading <- renderUI({
        conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                         tags$div(
                             i18n()$t(paste0("The water level computation is r",
                                             "unning. Please wait a moment for",
                                             " the results.")),
                             id = "loadmessage"))
    })
    
    ## compute the waterLevelPegelonline()
    wldf <- reactive({
        req(input$river)
        req(input$from_to)
        req(input$date)
        req(input$time)
        
        # create an empty wldf
        date <- as.character(input$date)
        #if (german()) {
            time <- strftime(input$time, "%H:%M:%S")
        #} else {
        #    time <- strftime(input$time, "%H:%M:%S")
        #}
        time_to_query <- as.POSIXct(strptime(paste(sep = " ", date, time), 
                                             format = "%Y-%m-%d %H:%M:%S"))
        station_int <- as.integer(seq(input$from_to[1] * 1000,
                                      input$from_to[2] * 1000, 100))
        wldf <- WaterLevelDataFrame(river = simpleCap(input$river),
                                    time = time_to_query,
                                    station_int = station_int)
        
        # compute waterLevel and fill the wldf
        wldf <- waterLevelPegelonline(wldf, shiny = TRUE)
    })
    
    ## plot the WaterLevelDataFrame
    output$plot <- renderPlot({
        req(wldf())
        
        # plot the filled wlsdf
        if (input$xlim){
            plotShiny(wldf(),
                      add_flys = input$flys,
                      add_flys_labels = input$flys,
                      add_weighting = input$weighting,
                      xlab = i18n()$t("river station (km)"),
                      ylab = i18n()$t("height a.s.l. (m, DHHN92)"))
        } else {
            plotShiny(wldf(),
                      add_flys = input$flys,
                      add_flys_labels = input$flys,
                      add_weighting = input$weighting,
                      xlim = c(min(wldf()$station), max(wldf()$station)),
                      xlab = i18n()$t("river station (km)"),
                      ylab = i18n()$t("height a.s.l. (m, DHHN92)"))
        }
    })
    
    # responsive downloadData
    output$downloadData <- downloadHandler(filename = function(){
        paste0('waterLevelPegelonline_', input$river, "_",
               strftime(input$time, format = "%Y%m%d%H%M"),
               '.csv')},
        content = function(file){
            data <- as.data.frame(wldf())[, 1:3]
            write.table(data,
                        file,
                        quote = FALSE,
                        sep = ";",
                        dec = ",",
                        row.names = FALSE,
                        fileEncoding = "UTF-8")},
        contentType = "text/csv"
    )
    
}
