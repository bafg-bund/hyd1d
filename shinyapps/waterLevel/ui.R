# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")

# load the necessary packages
library(shiny, lib.loc = lib)

fluidPage(
    
    titlePanel("waterLevel()", windowTitle = "hyd1d::waterLevel()"),
    
    column(width = 3,
        
        HTML("<BR>"),
        HTML("<BR>"),
        HTML("<BR>"),
        
        # menu item gauging_station
        alignRight(
            selectInput(
                inputId  = "river",
                label    = "Fluss:",
                choices  = rivers,
                selected = "ELBE"
            )
        ),
        
        # menu item from - to
        alignRight(
            sliderInput(
                inputId = "from_to", 
                label   = "Kilometer (von - bis):",
                min     = df.from_to$from[1],
                max     = df.from_to$to[1],
                value   = c(df.from_to$from_val[1], df.from_to$to_val[1]),
                step    = 0.1
            )
        ),
        
        # menu item time
        alignRight(
            dateInput(
                inputId  = "time", 
                label    = "Datum:",
                min      = as.POSIXct("1990-01-01"),
                max      = as.POSIXct(Sys.Date() - 1),
                value    = as.POSIXct("2016-12-21"),
                format   = "dd.mm.yyyy",
                language = "de"
            )
        ),
        
        alignRight(
            checkboxInput(
                inputId = "flys",
                label   = "FLYS Wasserspiegellagen",
                value = TRUE)
        ),
        
        alignRight(
            checkboxInput(
                inputId = "weighting",
                label   = "Gewichtung am Pegel",
                value = TRUE)
        ),
        
        alignRight(
            checkboxInput(
                inputId = "xlim",
                label   = "Berechnungsrelevante Pegel",
                value = TRUE)
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
           
           conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
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