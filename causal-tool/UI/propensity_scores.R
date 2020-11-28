UI_propensity_scores <- navbarMenu(
  title = 'Propensity scores',
  tabPanel(title = "Propensity scores and matching",
         sidebarLayout(
           sidebarPanel(
             width = 5,
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
             width = 7,
             tabsetPanel(
               id = "propensity_tabs_scores",
               type = "tabs",
               tabPanel("Propensity scores",
                        br(),
                        plotOutput('propensity_plot_scores',
                                   brush = 'propensity_plot_scores_brush')),
               tabPanel("Matching",
                        br(),
                        plotOutput("propensity_plot_matching", height = 300)
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