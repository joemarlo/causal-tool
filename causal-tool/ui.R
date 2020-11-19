shinyUI(fluidPage(
    # download roboto font
    HTML(
        '<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'
    ),
    
    # set default slider skin
    chooseSliderSkin(skin = "Flat", color = "#221146"),
    
    # load custom CSS file
    includeCSS("www/custom_css.css"),
    
    # set top left title
    titlePanel(
        title = h1("NYU Causal Inference"),
        windowTitle = "NYU Causal"
    ),
    
    
    br(),
    
    navbarPage(
            title = NULL,
            id = "nav",
        
        # welcome page ------------------------------------------------------------
        
        tabPanel(title = "Welcome",
                 includeMarkdown("markdowns/welcome_text.md")),
        
        # fundamental problem of causal inference ---------------------------------
        
        tabPanel(title = "Fundamental problem",
                 navlistPanel(
                   id = "fundamental_nav",
                   widths = c(1, 11),
                   HTML('<div><h5>Steps</h5></div>'),
                   tabPanel("1",
                            sidebarLayout(
                              sidebarPanel(
                                width = 4,
                                h3("What is the fundamental problem of causal inference?"),
                                h5("Causal inference is a missing data problem. We can only observe one potential outcome. We either see the that the person receives the treatment or doesn't receive the treatment."),
                                h5("For example, if a person smokes, will they have a heart attack? Consider the graph to the right."),
                              ),
                              mainPanel(width = 6,
                                        plotOutput("fundamental_plot_one"))
                            )),
                   tabPanel("2",
                            sidebarLayout(
                              sidebarPanel(
                                width = 4,
                                h3("What is the fundamental problem of causal inference?"),
                                h5("Causal inference is a missing data problem. We can only observe one potential outcome. We either see the that the person receives the treatment or doesn't receive the treatment."),
                                h5("For example, if a person smokes, will they have a heart attack? Consider the graph to the right."),
                                h5("But what if they didn't smoke, would they still have a heart attack?"),
                              ),
                              mainPanel(width = 6,
                                        plotOutput("fundamental_plot_two"))
                            )),
                   tabPanel("3",
                            sidebarLayout(
                              sidebarPanel(
                                width = 4,
                                h3("What is the fundamental problem of causal inference?"),
                                h5("Causal inference is a missing data problem. We can only observe one potential outcome. We either see the that the person receives the treatment or doesn't receive the treatment."),
                                h5("For example, if a person smokes, will they have a heart attack? Consider the graph to the right."),
                                h5("But what if they didn't smoke, would they still have a heart attack?"),
                                h5("Seeing both outcomes, heart attack and no heart attack, is impossible.")
                              ),
                              mainPanel(width = 6,
                                        plotOutput("fundamental_plot_three"))
                            )),
                   tabPanel("4",
                            sidebarLayout(
                              sidebarPanel(
                                width = 4,
                                h3("What is the fundamental problem of causal inference?"),
                                h5("Causal inference is a missing data problem. We can only observe one potential outcome. We either see the that the person receives the treatment or doesn't receive the treatment."),
                                h5("For example, if a person smokes, will they have a heart attack? Consider the graph to the right."),
                                h5("But what if they didn't smoke, would they still have a heart attack?"),
                                h5("Seeing both outcomes, heart attack and no heart attack, is impossible."),
                                h5("And there may be [latent] and [confounding] variables.")
                              ),
                              mainPanel(width = 6,
                                        plotOutput("fundamental_plot_four"))
                            ))
                 )), 
        
        # randomization page ------------------------------------------------------
        
        tabPanel(title = "Randomization",
                 sidebarLayout(
                     sidebarPanel(
                         width = 4,
                         h3("Why do we randomize?"),
                         h5("Randomization balances groups on both observed and unobserved characteristics. See this for yourself below. First set the x and y variables to observe."),
                         br(),
                         selectInput(
                             inputId ="randomization_variable_x",
                             label = "(Observed) x variable: ",
                             multiple = FALSE,
                             choices = setdiff(colnames(mtcars), "treat"),
                             selected = setdiff(colnames(mtcars), "treat")[1]
                         ),
                         selectInput(
                             inputId ="randomization_variable_y",
                             label = "(Observed) y variable: ",
                             multiple = FALSE,
                             choices = setdiff(colnames(mtcars), "treat"),
                             selected = setdiff(colnames(mtcars), "treat")[3]
                         ),
                         h5("Now select which datapoints to include in the treatment group by clicking on points in the plot."),
                         h5("How do the univariate densities compare between treatment and control?"),
                         h5("Now randomize the selections using the below button. How do the densities compare now?"),
                         br(),
                         actionButton(inputId = 'randomize_button',
                                      label = "Randomize the treatment assignment"),
                         br(), br(),
                         actionButton(inputId = 'randomize_reset_button',
                                      label = "Reset the treatment assignment"),
                         br(), br(),
                         HTML('<details><summary>What is this data?</summary>'),
                         HTML('The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models). <a href="https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html">See here for more information.</a'),
                         HTML('</details><br>')
                     ),
                     mainPanel(
                         width = 6,
                         plotOutput('randomization_plot',
                                    click = "randomization_plot_click"),
                         br(),
                         plotOutput('randomization_tc_plot'),
                         absolutePanel(id = "randomization_floating_box", 
                                       class = "floating_message",
                                       top = 50, left = "auto", right = 50, bottom = "auto",
                                       width = "30%", height = "auto", draggable = FALSE,
                                       "Click on points to assign them to treatment!")
                     )
                 )),
        
        
        # difference in means -----------------------------------------------------------
        
        tabPanel(title = "Difference in means",
                 sidebarLayout(
                     sidebarPanel(
                         width = 4,
                         h3("Can we just take the average effect between the treatment and control groups?"),
                         h5("...explain difference in means..."),
                         br(),
                         h4("Results"),
                         htmlOutput("means_summary"),
                         br(),
                         HTML('<details><summary>Data generation process</summary>'),
                         h4("Data generation process"),
                         numericInput(
                             inputId = "means_select_n",
                             label = "n",
                             value = 1000,
                             min = 100,
                             max = 10000,
                             step = 100
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
                             tabPanel("[ATE]",
                                      plotlyOutput("means_plot_ATE", height = 500)), 
                             tabPanel("[Estimating SATE]",
                                      plotlyOutput("means_plot_est_SATE", height = 500)), 
                             tabPanel("[Regression]",
                                      plotOutput('means_plot_regression', height = 500))
                         )
                     )
                 )
        ),
        
        # regression  -----------------------------------------------------------
        
        tabPanel(title = "Regression"),
        
        # common support  -----------------------------------------------------------
        
        tabPanel(title = "Common support"),
        
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
                                     choices = setdiff(colnames(mtcars), "treat"),
                                     selected = setdiff(colnames(mtcars), "treat"),
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