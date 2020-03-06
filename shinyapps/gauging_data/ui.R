# load the necessary packages
library(shiny)
library(shinyjs)
library(shiny.i18n)
library(leaflet)

# define UI
fluidPage(
    
    useShinyjs(),
    
    # application title
    uiOutput("titlepanel"),
    
    # application layout
    fluidRow(
        
        # menu
        column(
            width = 3,
            
            # menu title
            uiOutput("menu_title"),
            
            # menu item RIVER
            uiOutput("menu_river"),
            
            # menu item GAUGING_STATION
            uiOutput("menu_gauging_station"),
            
            # menu item DATE_RANGE
            uiOutput("menu_daterange"),
            style = paste0("background-color:#E8E8E8;border-radius:20px;",
                           "padding:10px 20px 30px 40px;")
        ),
        
        # main
        column(
            width = 9,
            
            # responsive title
            uiOutput("title"),
            
            # responsive table and map
            fluidRow(
                column(
                    width = 6,
                    uiOutput("table_title"),
                    tableOutput("table")
                ),
                
                column(
                    width = 6,
                    uiOutput("map_title"),
                    leafletOutput("map")
                )
            ),
            
            # responsive plot
            uiOutput("plot_title"),
            downloadLink("downloadData", "Download"),
            plotOutput("plot"),
            style = "padding: 0px 50px 50px 50px;"
            
        ),
        
        style = "padding: 20px;"
        
    )
)
