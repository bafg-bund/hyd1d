# load the necessary packages
library(shiny)
library(leaflet)

# define UI
fluidPage(
    
    # application title
    titlePanel("Pegeldaten für waterLevel()"),
    
    # application layout
    fluidRow(
        
        # menu
        column(
            width = 3,
            
            h3("Auswahlmenü"),
            
            # menu item RIVER
            selectInput(
                inputId  = "river",
                label    = "Fluss:",
                choices  = rivers,
                selected = "ELBE"
            ),
            
            # menu item GAUGING_STATION
            uiOutput("menu_gauging_station"),
            
            # menu item DATE_RANGE
            dateRangeInput(
                inputId   = "daterange", 
                label     = paste0("Zeitraum (01.01.1990 - ", strftime(yesterday, format="%d.%m.%Y"), "):"),
                start     = as.character(yesterday - 365),
                end       = as.character(yesterday),
                min       = "1990-01-01",
                max       = as.character(yesterday),
                format    = "dd.mm.yyyy",
                language  = "de",
                separator = " - "
            ),
            style = "background-color:#E8E8E8;border-radius:20px;padding:10px 20px 30px 40px;"
        ),
        
        # main
        column(
            width = 9,
            
            # responsive title
            h1(textOutput("title"), style="color: darkblue;"),
            
            # responsive table and map
            fluidRow(
                column(
                    width = 6,
                    h3("Stammdaten"),
                    tableOutput("table")
                ),
                
                column(
                    width = 6,
                    h3("Lage des Pegels"),
                    leafletOutput("map")
                )
            ),
            
            # responsive plot
            h3("Zeitreihe"),
            downloadLink('downloadData', 'Download der dargestellten Zeitreihe'),
            plotOutput("plot"),
            
            style = "padding: 0px 50px 50px 50px;"
            
        ),
        
        style = "padding: 20px;"
        
    )
)
