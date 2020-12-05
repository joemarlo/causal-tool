UI_propensity_scores <- tabPanel(
  title = "Propensity scores and matching",
         sidebarLayout(
           sidebarPanel(
             width = 5,
             includeMarkdown('markdowns/propensity_scores.md'),
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
                              choices = c('Nearest neighbor', "Radius matching"),
                              selected = "Nearest neighbor"),
                 conditionalPanel(
                   condition = "input.propensity_match_type_input == 'Nearest neighbor'",
                   radioButtons(inputId = "propensity_radio_replacement",
                                label = "Replacement type:",
                                choices = c("With replacement", "Without replacement"),
                                selected = "With replacement")
                 ),
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
         )
)