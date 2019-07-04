fluidPage(
    
    titlePanel("waterLevelPegelonline()",
               windowTitle = "hyd1d::waterLevelPegelonline()"),
    
    column(width = 3,
        
        HTML("<BR>"),
        HTML("<BR>"),
        HTML("<BR>"),
        
        # menu item gauging_station
        selectInput(
            inputId  = "river",
            label    = "Fluss:",
            choices  = rivers,
            selected = "ELBE"
        ),
        
        # menu item from - to
        sliderInput(
            inputId = "from_to", 
            label   = "Kilometer (von - bis):",
            min     = df.from_to$from[1],
            max     = df.from_to$to[1],
            value   = c(df.from_to$from_val[1], df.from_to$to_val[1]),
            step    = 0.1
        ),
        
        # menu items date & time
        dateInput(
            inputId  = "date", 
            label    = "Datum:",
            min      = trunc(Sys.time() - as.difftime(31, units = "days"),
                             units = "days"),
            max      = floor_date(Sys.time(), "day"),
            value    = floor_date(Sys.time(), "day"),
            format   = "dd.mm.yyyy",
            language = "de"
        ),
        
        timeInput(
            inputId = "time", 
            label   = "Uhrzeit:",
            value   = floor_date(Sys.time(), "15 minutes"),
            seconds = FALSE
        ),
        
        # checkboxes for plotShiny
        checkboxInput(
            inputId = "flys",
            label   = "FLYS Wasserspiegellagen",
            value = TRUE
        ),
        
        checkboxInput(
            inputId = "weighting",
            label   = "Gewichtung am Pegel",
            value = TRUE
        ),
        
        checkboxInput(
            inputId = "xlim",
            label   = "Berechnungsrelevante Pegel",
            value = TRUE
        )
    ),
    
    column(width = 9,
           align = "center",
           
           tags$style(type="text/css",
                      "#loadmessage {
                       width: 100%;
                       padding: 5px 0px 5px 0px;
                       text-align: center;
                       font-weight: bold;
                       font-size: 100%;
                       color: #000000;
                       background-color: #ffcccc;
                       z-index: 105;}",
                      ".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }"),
           
           conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                            tags$div(
                                paste0("Die Wasserspiegellage wird berechnet. ",
                                       "Es dauert noch einen Moment bis das ",
                                       "Ergebnis vorliegt."),
                                id = "loadmessage")),
           
           plotOutput("plot"),
           
           conditionalPanel("output.plot", 
                            downloadLink("downloadData",
                                         "Download der Wasserspiegellage"))
           
    )
    
)
