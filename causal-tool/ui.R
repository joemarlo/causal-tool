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
    
    # initiate shinyjs
    useShinyjs(),
    
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
                   tabPanel("1)",
                            sidebarLayout(
                              sidebarPanel(
                                width = 4,
                                h3("What is the fundamental problem of causal inference?"),
                                h5("Causal inference is fundamentally a missing data problem. We can only observe one potential outcome. We see that the person either receives the treatment or doesn't receive the treatment."),
                                h5("For example, if a person smokes, will they have a heart attack?"),
                              ),
                              mainPanel(width = 6,
                                        plotOutput("fundamental_plot_one"))
                            )),
                   tabPanel("2)",
                            sidebarLayout(
                              sidebarPanel(
                                width = 4,
                                h3("What is the fundamental problem of causal inference?"),
                                h5("But what if they didn't smoke, would they still have a heart attack?"),
                              ),
                              mainPanel(width = 6,
                                        plotOutput("fundamental_plot_two"))
                            )),
                   tabPanel("3)",
                            sidebarLayout(
                              sidebarPanel(
                                width = 4,
                                h3("What is the fundamental problem of causal inference?"),
                                h5("Seeing both outcomes, heart attack and no heart attack, is impossible. This is the observed outcome and one potential counterfactual.")
                              ),
                              mainPanel(width = 6,
                                        plotOutput("fundamental_plot_three"))
                            )),
                   tabPanel("4)",
                            sidebarLayout(
                              sidebarPanel(
                                width = 4,
                                h3("What is the fundamental problem of causal inference?"),
                                h5("And, in many cases, there are confounding and latent variables that further complicate the ability to draw inference.")
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
                         h5("Randomization balances groups on both observed and unobserved characteristics. See this for yourself. First set the x and y variables to observe."),
                         br(),
                         selectInput(
                             inputId ="randomization_variable_x",
                             label = "(Observed) x variable: ",
                             multiple = FALSE,
                             choices = setdiff(colnames(master_df), "treat"),
                             selected = setdiff(colnames(master_df), "treat")[1]
                         ),
                         selectInput(
                             inputId ="randomization_variable_y",
                             label = "(Observed) y variable: ",
                             multiple = FALSE,
                             choices = setdiff(colnames(master_df), "treat"),
                             selected = setdiff(colnames(master_df), "treat")[3]
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
                         HTML('<br>The data are simulated. The values and correlations are reasonable but please do not make any material conclusions from the data.</a'),
                         HTML('</details><br>')
                     ),
                     mainPanel(
                         width = 6,
                         plotOutput('randomization_plot',
                                    click = "randomization_plot_click"),
                         br(),
                         plotOutput('randomization_tc_plot', height = 500),
                         absolutePanel(id = "randomization_floating_box", 
                                       class = "floating_message",
                                       top = 50, left = "auto", right = 50, bottom = "auto",
                                       width = "30%", height = "auto", draggable = FALSE,
                                       "Click on points to assign them to treatment!")
                     )
                 )),
        
        
        # difference in means -----------------------------------------------------------
        
        tabPanel(title = "Treatment effects",
                 sidebarLayout(
                     sidebarPanel(
                         width = 4,
                         h3("Can we just take the average effect between the treatment and control groups?"),
                         h5("...explain difference in means..."),
                         br(),
                         h4("Results"),
                         htmlOutput("means_summary"),
                         br(),
                         HTML('<details><summary>More information</summary>'),
                         h4("Data generation process"),
                         sliderInput(
                             inputId = "means_select_n",
                             label = "n:",
                             value = 500,
                             min = 100,
                             max = 2500,
                             step = 100
                         ),
                         sliderInput(
                             inputId = "means_select_tau",
                             label = "Tau:",
                             value = 5,
                             min = 0,
                             max = 100,
                             step = 1
                         ),
                         sliderInput(
                             inputId = "means_select_slope",
                             label = "Slope:",
                             value = 1.1,
                             min = 0,
                             max = 10,
                             step = 0.1
                         ),
                         sliderInput(
                             inputId = "means_slider_error",
                             label = "Error:",
                             value = 1,
                             min = 0,
                             max = 10,
                             step = 1
                         ),
                         HTML('</details><br>')
                     ),
                     mainPanel(
                         width = 6,
                         tabsetPanel(
                             id = "means_tabs",
                             type = "tabs",
                             tabPanel("[SATE]",
                                      plotlyOutput("means_plot_SATE", height = 500)), 
                             tabPanel("[Estimating SATE]",
                                      plotlyOutput("means_plot_est_SATE", height = 500)), 
                             tabPanel("[Regression]",
                                      plotOutput('means_plot_regression', height = 500)),
                             tabPanel("[Bias and efficiency]")
                         )
                     )
                 )
        ),
        

        # propensity scores -----------------------------------------------------------
        
        tabPanel(title = "Propensity scores",
                 sidebarLayout(
                     sidebarPanel(
                         width = 4,
                         h3("Propensity scores are the probability that an observation is assigned to treatment based on the observed covariates"),
                         h5("The scores can reduce selection bias and constrain inference to areas of common support."),
                         h5("In order to illustrate the mechanism, let's first set treatment as either random or a function of the covariates."),
                         radioButtons(inputId = 'propensity_select_method',
                                     label = 'Method to determine treatment:',
                                     choices = c('Random', 'Dependent on covariates'),
                                     selected = 'Random'),
                         conditionalPanel(
                             condition = "input.propensity_select_method == 'Dependent on covariates'",
                             selectInput(inputId = "propensity_select_covariates",
                                         label = "Covariates:",
                                         choices = setdiff(colnames(master_df), "treat"),
                                         selected = setdiff(colnames(master_df), "treat"),
                                         multiple = TRUE),
                             strong("Formula:"),
                             uiOutput("propensity_select_treat_formula"),
                             br()
                         ),
                         actionButton(inputId = 'propensity_button_set_treat',
                                      label = 'Set treatment assignment'),
                         br(), br(),
                         selectInput(inputId = "propensity_select_model",
                                     label = "Model family",
                                     choices = c("Binomial - logit",
                                                 "Binomial - probit",
                                                 "GAM"),
                                     selected = "Binomial - logit"),
                         selectInput(inputId = "propensity_select_independent",
                                     label = "Independent variables:",
                                     choices = setdiff(colnames(master_df), "treat"),
                                     selected = setdiff(colnames(master_df), "treat"),
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
                             tabPanel("Propensity scores",
                                      # tableOutput('p_score_table')),
                                      br(),
                                      plotOutput('propensity_plot_scores',
                                                 brush = 'propensity_plot_scores_brush')),
                             tabPanel("Matching",
                                      br(),
                                      plotOutput("propensity_plot_matching", height = 300)
                                      ),
                             tabPanel("Overlap and balance")
                         )
                     )
                 )),
        
        
        # common support  -----------------------------------------------------------
        
        # tabPanel(title = "Common support"),
        
        # matching page -----------------------------------------------------------
        
        # tabPanel(title = "Matching"),
        
        # matching page -----------------------------------------------------------

        tabPanel(title = "Regression discontinuity",
                 sidebarLayout(
                     sidebarPanel(
                         width = 4,
                         h3("Regression discontinuity"),
                         h5("Explain regression discontinuity design ...."),
                         br(),
                         h4("Analysis tools"),
                         selectInput(inputId = "disc_select_model",
                                     label = "Modeled relationship:",
                                     choices = c("Linear", "Polynomial - second order", "Polynomial - third order"),
                                     selected = "Linear",
                                     multiple = FALSE),
                         sliderInput(inputId = 'disc_numeric_window',
                                     label = 'Bandwidth',
                                     min = 1,
                                     max = 80,
                                     value = 10,
                                     step = 1,
                                     animate = animationOptions(interval = 400, loop = FALSE)),
                         strong("Tau estimate:"),
                         htmlOutput('disc_table'),
                         br(),
                         h4("Data generation process"),
                         selectInput(inputId = "disc_select_DGP",
                                     label = "True relationship:",
                                     choices = c("Linear", "Polynomial - second order", "Polynomial - third order"),
                                     selected = "Linear",
                                     multiple = FALSE),
                         sliderInput(inputId = "disc_numeric_tau",
                                      label = "True difference (tau) at the cutoff:",
                                      min = 0, 
                                      max = 20, 
                                      step = 1,
                                      value = 5),
                         sliderInput(inputId = "disc_numeric_cutoff",
                                     label = "Cutoff age:",
                                     min = 30, 
                                     max = 70, 
                                     step = 1,
                                     value = 50),
                         sliderInput(inputId = "disc_numeric_n",
                                      label = "n:",
                                      min = 100, 
                                      max = 1000, 
                                      step = 50,
                                      value = 250),
                         sliderInput(
                             inputId = "disc_slider_error",
                             label = "Error:",
                             value = 5,
                             min = 0,
                             max = 10,
                             step = 1
                         ),
                         conditionalPanel(
                             condition = "input.disc_select_DGP == 'Linear'",
                             sliderInput(inputId = "disc_slider_slope",
                                         label = "Slope:",
                                         min = 0,
                                         max = 1,
                                         step = 0.1,
                                         value = 1),
                         ),
                         HTML('<details><summary>Pseudocode to generate the data</summary>'),
                         uiOutput("disc_select_DGP_formula"),
                         HTML('</details><br>')
                         ),
                     mainPanel(
                         width = 6,
                         tabsetPanel(
                             id = 'disc_tabs',
                             type = 'tabs',
                             tabPanel("Observable data",
                                      br(),
                                      plotOutput("disc_plot_observable", height = 600)),
                             tabPanel("All data",
                                      br(),
                                      plotOutput("disc_plot_all", height = 600))
                         )
                        )
                     )
                 )
)))
