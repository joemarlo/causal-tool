UI_propensity_scores <- navbarMenu(
  title = 'Propensity scores',
  tabPanel(title = "[Observational studies]"),
  tabPanel(title = "Propensity scores and matching",
         sidebarLayout(
           sidebarPanel(
             width = 5,
             h3("Propensity scores are the probability that an observation is assigned to treatment based on the observed covariates"),
             h5("The scores can reduce selection bias and allow us to constrain inference to areas of common support."),
             h5("In order to illustrate the mechanism, let's first set treatment as either random or a function of the covariates. Unequal distributions indicate that the observations one group may have higher probability of being selected into treatment."),
             h5("Second, see how the scores change by adjusting the covariates that determine treatment and independent variables within the model"),
             h5("Third, cycle through the matching types to visualize how each each treatment observation is matched to a control observations"),
             br(),
             tabsetPanel(
               id = 'propensity_tabs_left',
               type = 'tabs',
               tabPanel(
                 "Set treatment",
                 br(),
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
                              label = 'Set treatment assignment')
               ),
               tabPanel(
                 "Propensity method",
                 br(),
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
                             multiple = TRUE)
               ),
               tabPanel(
                 'Matching method',
                 br(),
                 radioButtons(inputId = "propensity_match_type_input",
                              label = "Matching type:",
                              choices = c("With replacement", "Without replacement", "Radius matching"),
                              selected = "With replacement"),
                 conditionalPanel(
                   condition = "input.propensity_match_type_input == 'Radius matching'",
                   sliderInput(inputId = "propensity_slider_caliper",
                               label = "Caliper width:",
                               min = 0,
                               max = 0.2,
                               step = 0.01,
                               value = 0.02,
                               animate = animationOptions(interval = 400, loop = FALSE))
                 )
               )
             )
           ),
           mainPanel(
             width = 7,
             tabsetPanel(
               id = "propensity_tabs_scores",
               type = "tabs",
               tabPanel("Propensity scores",
                        br(),
                        plotOutput('propensity_plot_scores')),
                                   # brush = 'propensity_plot_scores_brush')),
               tabPanel("Matching",
                        br(),
                        plotOutput("propensity_plot_matching", height = 400)
               )
             )
           )
         )),
  tabPanel(title = "[Overlap and balance]",
           sidebarLayout(
             sidebarPanel(
               width = 5,
               h3("Assessing overlap and balance"),
               h5("The goal is to have empirical counterfactuals for each member of our treatment group (for ATT) or control group (for ATE). This can be assessed by plotting the propensity scores for each group and assessing the overlap. If there isn't sufficient overlap, then we can surmise the two groups are too different to be compared."),
               br()
             ),
             mainPanel(
               width = 7,
               tabsetPanel(
                 id = "propensity_tabs_overlap",
                 type = "tabs",
                 tabPanel("[Overlap]"),
                 tabPanel("[Balance]")
               )
             )
           ))
)