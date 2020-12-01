UI_treatment_effects <- navbarMenu(
  title = "Treatment effects",
  tabPanel("Estimating treatment effects",
      sidebarLayout(
        sidebarPanel(
             width = 5,
             h3("How do we estimate the treatment effect between two different groups?"),
             h5("Can we just take the average effect between the treatment and control groups?"),
             h5("...explain difference in means..."),
             h5("Pretium viverra suspendisse potenti nullam ac tortor vitae. Eros donec ac odio tempor orci dapibus ultrices in. Etiam erat velit scelerisque in dictum non. Sed viverra tellus in hac habitasse. Quis risus sed vulputate odio ut enim blandit volutpat maecenas."),
             h5("Tellus id interdum velit laoreet id donec ultrices tincidunt arcu. Imperdiet nulla malesuada pellentesque elit eget. Commodo sed egestas egestas fringilla phasellus faucibus scelerisque eleifend."),
             br(), br(),
             tabsetPanel(
               id = 'means_tabs_left',
               type = 'tabs',
               tabPanel(
                 "Results",
                 br(),
                 htmlOutput("means_summary")
               ),
               tabPanel(
                 'Data generation process',
                 br(),
                 dgpUI("means_")
               ),
               tabPanel(
                 "Animation options",
                 br(),
                 tags$div(
                   id = 'div_means_slider_frame_SATE',
                   sliderInput(
                     inputId = 'means_slider_frame_SATE',
                     label = "SATE frame:",
                     value = 1,
                     min = 1,
                     max = 20,
                     step = 1,
                     animate = animationOptions(interval = 125, loop = FALSE)
                   )
                 ),
                 tags$div(
                   id = 'div_means_slider_frame_est_SATE',
                   sliderInput(
                     inputId = 'means_slider_frame_est_SATE',
                     label = "Estimating SATE frame:",
                     value = 1,
                     min = 1,
                     max = 26,
                     step = 1,
                     animate = animationOptions(interval = 200, loop = FALSE)
                   )
                 ),
                 tags$div(
                   id = 'div_means_slider_frame_regression',
                   sliderInput(
                     inputId = 'means_slider_frame_regression',
                     label = "Regression frame:",
                     value = 1,
                     min = 1,
                     max = 15,
                     step = 1,
                     animate = animationOptions(interval = 200, loop = FALSE)
                   )
                 )
               )
             )),
           mainPanel(
             width = 7,
             tabsetPanel(
               id = "means_tabs",
               type = "tabs",
               tabPanel("SATE",
                        br(),
                        plotOutput("means_plot_SATE", height = 500),
                        br(),
                        actionButton(inputId = "means_button_play_SATE",
                                     label = "Animate"),
                        br(), br(),
                        actionButton(inputId = "means_button_reset_SATE",
                                     label = "Reset animation"),
                        tags$script(
                          # this is a jquery script that executes the play button on means_slider_frame when the user
                          # clicks the #means_button_play. Its a bit of a hack
                          "$('#means_button_play_SATE').click(function(){
                             {$('#div_means_slider_frame_SATE .slider-animate-button').click()};
                             });"
                        )), 
               tabPanel("Estimating SATE",
                        br(),
                        plotOutput("means_plot_est_SATE", height = 500),
                        br(),
                        actionButton(inputId = "means_button_play_est_SATE",
                                     label = "Animate"),
                        br(), br(),
                        actionButton(inputId = "means_button_reset_est_SATE",
                                     label = "Reset animation"),
                        tags$script(
                          # this is a jquery script that executes the play button on means_slider_frame when the user
                          # clicks the #means_button_play. Its a bit of a hack
                          "$('#means_button_play_est_SATE').click(function(){
                             {$('#div_means_slider_frame_est_SATE .slider-animate-button').click()};
                             });"
                        )), 
               tabPanel("Regression",
                        br(),
                        plotOutput('means_plot_regression', height = 500),
                        br(),
                        actionButton(inputId = "means_button_play_regression",
                                     label = "Animate"),
                        br(), br(),
                        actionButton(inputId = "means_button_reset_regression",
                                     label = "Reset animation"),
                        tags$script(
                          # this is a jquery script that executes the play button on means_slider_frame when the user
                          # clicks the #means_button_play. Its a bit of a hack
                          "$('#means_button_play_regression').click(function(){
                             {$('#div_means_slider_frame_regression .slider-animate-button').click()};
                             });"
                        ))
             )
           )
         )
    ),
      tabPanel("Efficiency",
          sidebarLayout(
            sidebarPanel(
              width = 5,
              h3("Why bother with regression estimates if they seem to give us a similar answer?"),
              h5("Regression results in much more efficient estimates. Playing statistics god, we can see how the observed outcomes and estimates change if we relabel the treatment assignment n amount of times. This is a randomization distribution. We then calculate SATE and the regression estimate for each of these n simulations."),
              h5('You can think of this simulation process as repetitively taking samples of the same dataset, but each time randomly assigning treatment to a new group of observations and estimating the treatment effect from the resulting groups.'),
              h5("The resulting plot shows how the regression estimate is unbiased for SATE and the variance of the distribution is much smaller (i.e. more efficient)."),
              h5("This dynamic holds with even as few as 50 simulations but it differs as slope and error changes. Play with data generation process settings to see how it affects the simulation results."),
              br(),
              tabsetPanel(
                id = 'means_efficiency_tabs_left',
                type = 'tabs',
                tabPanel(
                  'Simulation',
                  br(),
                  sliderTextInput(
                    inputId = 'means_EB_slider_n_sims',
                    label = "n simulations:",
                    selected = 250,
                    choices = c(50, 100, 250, 500, 1000, 5000),
                    grid = TRUE
                  ), 
                  actionButton(
                    inputId = 'means_EB_button_run_sim',
                    label = 'Run simulation')
                ),
                tabPanel(
                  'Data generation process',
                  br(),
                  dgpUI("means_EB_")
                  )
                )
            ),
            mainPanel(
              width = 7,
              tabsetPanel(
                id = "means_efficiency_tabs_right",
                type = "tabs",
                tabPanel("Randomization distribution",
                         br(),
                         plotOutput("means_efficiency_plot_randomization", height = 500)),
                tabPanel("Sampling distribution",
                         br(),
                         plotOutput("means_efficiency_plot_sampling", height = 500))
              )
            )
          )
      ),
  tabPanel("Bias",
           sidebarLayout(
             sidebarPanel(
               width = 5,
               h3("Bias: Why bother with regression estimates if they seem to give us a similar answer?"),
               h5("Assume that a randomized control trial is designed to measure the effect of a drug on cholesterol levels. Smokers and non-smokers are included in the trial but their status is not accounted for in the analysis. How would this affect the results?"),
               h5("Regression ..."),
               h5('double check the output'),
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
                     min = 0,
                     max = 1,
                     step = 0.05
                   ),
                   sliderInput(
                     inputId = "means_bias_select_slope",
                     label = "Effect of being a smoker:",
                     value = 5,
                     min = 0,
                     max = 10,
                     step = 1
                   ),
                   sliderInput(
                     inputId = "means_bias_select_tau",
                     label = "Tau:",
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
)
