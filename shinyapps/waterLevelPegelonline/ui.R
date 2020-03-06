fluidPage(
    
    useShinyjs(),
    
    titlePanel("waterLevelPegelonline()",
               windowTitle = "hyd1d::waterLevelPegelonline()"),
    
    column(width = 3,
        
        HTML("<BR>"),
        HTML("<BR>"),
        HTML("<BR>"),
        
        # menu item gauging_station
        uiOutput("menu_river"),
        
        # menu item from - to
        uiOutput("menu_from_to"),
        
        # menu item time
        uiOutput("menu_date"),
        uiOutput("menu_time"),
        
        # menu item flys
        uiOutput("menu_flys"),
        
        # menu item weighting
        uiOutput("menu_weighting"),
        
        # menu item xlim
        uiOutput("menu_xlim")
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
           
           uiOutput("loading"),
           
           plotOutput("plot"),
           
           conditionalPanel("output.plot", 
                            downloadLink("downloadData", "Download"))
           
    )
    
)
