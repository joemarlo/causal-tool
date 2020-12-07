UI_bias <- tabPanel(
  "Bias",
         sidebarLayout(
           sidebarPanel(
             width = 5,
             includeMarkdown('markdowns/bias.md'),
             br(),
             tabsetPanel(
               id = 'means_bias_tabs_right',
               type = 'tabs',
               tabPanel(
                 'Simulation',
                 br(),
                 sliderTextInput(
                   inputId = 'means_bias_slider_n_sims',
                   label = "n simulations:",
                   selected = 250,
                   choices = c(50, 100, 250, 500, 1000, 5000),
                   grid = TRUE
                 ), 
                 actionButton(
                   inputId = 'means_bias_button_run_sim',
                   label = 'Run simulations')
               ),
               tabPanel(
                 'Data generation process',
                 br(),
                 sliderInput(
                   inputId = 'means_bias_slider_smoker',
                   label = 'Proportion of observations that are smokers:',
                   value = 0.5,
                   min = 0,
                   max = 1,
                   step = 0.05
                 ),
                 sliderInput(
                   inputId = 'means_bias_slider_conditional',
                   label = 'Conditional probability of being assigned to treatment if a smoker:',
                   value = 0.2,
                   min = 0.05,
                   max = 0.95,
                   step = 0.05
                 ),
                 sliderInput(
                   inputId = "means_bias_slider_tau_smoker",
                   label = "Effect for observations that are smokers:",
                   value = 5,
                   min = 0,
                   max = 10,
                   step = 1
                 ),
                 sliderInput(
                   inputId = "means_bias_slider_tau_nonsmoker",
                   label = "Effect for observations that are not smokers:",
                   value = 3,
                   min = 0,
                   max = 20,
                   step = 1
                 ),
                 sliderInput(
                   inputId = "means_bias_slider_error",
                   label = "Error:",
                   value = 3,
                   min = 0,
                   max = 10,
                   step = 1
                 ),
                 sliderInput(
                   inputId = "means_bias_select_n",
                   label = "n:",
                   value = 250,
                   min = 100,
                   max = 1000,
                   step = 50
                 ),
                 HTML('<details><summary>Pseudocode to generate the data</summary>'),
                 uiOutput(outputId = "means_bias_DGP_formula"),
                 HTML('</details><br>')
               )
             )
           ),
           mainPanel(
             width = 7,
             tabsetPanel(
               id = "means_bias_tabs_right",
               type = "tabs",
               tabPanel("Randomization distribution",
                        br(),
                        plotOutput("means_bias_plot_randomization", height = 500)),
               tabPanel("Sampling distribution",
                        br(),
                        plotOutput("means_bias_plot_sampling", height = 500))
             )
           )
         )
)