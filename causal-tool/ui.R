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
        
        tabPanel(title = "Difference in means",
                 sidebarLayout(
                     sidebarPanel(
                         width = 4,
                         h4("Results"),
                         htmlOutput("means_summary"),
                         br(),
                         HTML('<details><summary>Data generation process</summary>'),
                         h4("Data generation process"),
                         numericInput(
                             inputId = "means_select_n",
                             label = "n",
                             value = 1000,
                             min = 10,
                             max = 10000,
                             step = 10
                         ),
                         numericInput(
                             inputId = "means_select_tau",
                             label = "tau",
                             value = 5,
                             min = 0,
                             max = 100,
                             step = 1
                         ),
                         numericInput(
                             inputId = "means_select_slope",
                             label = "slope",
                             value = 1.1,
                             min = 0,
                             max = 10,
                             step = 0.1
                         ),
                         HTML('</details><br>')
                     ),
                     mainPanel(
                         width = 6,
                         tabsetPanel(
                             id = "means_tabs",
                             type = "tabs",
                             tabPanel("Plot",
                                      plotOutput("means_plot", height = 500)
                             )
                         )
                     )
                 )
        ),
        
        # matching page -----------------------------------------------------------
        
        tabPanel(title = "Regression"),
        
        # propensity scores -----------------------------------------------------------
        
        tabPanel(title = "Propensity scores",
                 sidebarLayout(
                     sidebarPanel(
                         width = 4,
                         h4("Propensity scores"),
                         selectInput(inputId = "propensity_select_model",
                                     label = "Model family",
                                     choices = c("Binomial - logit",
                                                 "Binomial - probit",
                                                 "GAM"),
                                     selected = "Binomial - logit"),
                         selectInput(inputId = "propensity_select_independent",
                                     label = "Independent variables:",
                                     choices = setdiff(vars, c("treat", "ppvtr.36")),
                                     selected = setdiff(vars, c("treat", "ppvtr.36")),
                                     multiple = TRUE),
                         radioButtons(inputId = "propensity_replacement_type_input",
                                     label = "Replacement type:",
                                     choices = c("With", "Without"),
                                     selected = "With")
                     ),
                     mainPanel(
                         width = 6,
                         tabsetPanel(
                             id = "upload_tabs",
                             type = "tabs",
                             tabPanel("Matching",
                                      plotOutput("propensity_plot", height = 300)
                                      ),
                             tabPanel("Overlap and balance")
                         )
                     )
                 )),
        
        # matching page -----------------------------------------------------------
        
        tabPanel(title = "Matching"),
        
        # matching page -----------------------------------------------------------
        
        tabPanel(title = "Regression discontinuity")
        
    )
))
