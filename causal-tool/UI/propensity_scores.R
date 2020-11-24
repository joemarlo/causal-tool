UI_propensity_scores <- tabPanel(title = "Propensity scores",
         sidebarLayout(
           sidebarPanel(
             width = 4,
             h3("Propensity scores are the probability that an observation is assigned to treatment based on the observed covariates"),
             h5("The scores can reduce selection bias and constrain inference to areas of common support."),
             h5("In order to illustrate the mechanism, let's first set treatment as either random or a function of the covariates."),
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
                 radioButtons(inputId = "propensity_replacement_type_input",
                              label = "Replacement type:",
                              choices = c("With", "Without"),
                              selected = "With")
               )
             )
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
         ))