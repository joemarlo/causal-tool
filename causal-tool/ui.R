shinyUI(fluidPage(
    # download roboto font
    HTML(
        '<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'
    ),
    
    # set default slider skin
    chooseSliderSkin(skin = "Flat",
                     color = "#221146"),
    
    # load custom CSS file
    includeCSS("www/custom_css.css"),
    
    # set top left title
    titlePanel(
        title = h1("NYU Causal Inference"),
        windowTitle = "NYU Causal"
    ),
    
    
    br(),
    
    navlistPanel(
        id = "nav",
        widths = c(2, 10),
        
        # welcome page ------------------------------------------------------------
        
        tabPanel("Welcome",
                 includeMarkdown("markdowns/welcome_text.md")),
        
        # randomization page ------------------------------------------------------
        
        tabPanel(title = "Randomization",
                 sidebarLayout(
                     sidebarPanel(
                         width = 4,
                         h4("Upload your dataset that you downloaded in Assignment One"),
                     ),
                     mainPanel(
                         width = 6,
                         tabsetPanel(
                             id = "upload_tabs",
                             type = "tabs",
                             tabPanel("Plots"),
                             tabPanel("Sites invited")
                         )
                     )
                 )),
        
        # matching page -----------------------------------------------------------
        
        tabPanel(title = "Difference in means"),
        
        # matching page -----------------------------------------------------------
        
        tabPanel(title = "Regression"),
        
        # matching page -----------------------------------------------------------
        
        tabPanel(title = "Matching"),
        
        # matching page -----------------------------------------------------------
        
        tabPanel(title = "Regression discontinuity")
        
    )
))
