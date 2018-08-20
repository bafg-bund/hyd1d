# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")

# load the necessary packages
library(shiny, lib.loc = lib)

function(input, output, session) {
    
    ## responsive menu
    observe({
        id <- which(df.from_to$river == input$river)
        
        updateSliderInput(session, 
                          inputId = "from_to", 
                          label   = "Kilometer (von - bis):",
                          min     = df.from_to$from[id],
                          max     = df.from_to$to[id],
                          value   = c(df.from_to$from_val[id], 
                                      df.from_to$to_val[id]),
                          step    = 0.1
        )
    })
    
    ## compute the waterLevelPegelonline()
    wldf <- reactive({
        # create an empty wldf
        date <- as.character(input$date)
        time <- strftime(input$time, "%H:%M:%S")
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
                      add_weighting = input$weighting)
        } else {
            plotShiny(wldf(),
                      add_flys = input$flys,
                      add_flys_labels = input$flys,
                      add_weighting = input$weighting,
                      xlim = c(min(wldf()$station), max(wldf()$station)))
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
