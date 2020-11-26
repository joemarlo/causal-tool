UI_treatment_effects <- tabPanel(title = "Treatment effects",
         sidebarLayout(
           sidebarPanel(
             width = 4,
             h3("How do we estimate the treatment effect between two different groups?"),
             h5("Can we just take the average effect between the treatment and control groups?"),
             h5("...explain difference in means..."),
             h5("Pretium viverra suspendisse potenti nullam ac tortor vitae. Eros donec ac odio tempor orci dapibus ultrices in. Etiam erat velit scelerisque in dictum non. Sed viverra tellus in hac habitasse. Quis risus sed vulputate odio ut enim blandit volutpat maecenas."),
             h5("Tellus id interdum velit laoreet id donec ultrices tincidunt arcu. Imperdiet nulla malesuada pellentesque elit eget. Commodo sed egestas egestas fringilla phasellus faucibus scelerisque eleifend."),
             br(), br(),
             tabsetPanel(
               id = 'disc_tabs_left',
               type = 'tabs',
               tabPanel(
                 "Results",
                 br(),
                 htmlOutput("means_summary")
               ),
               tabPanel(
                 "Data generation process",
                 br(),
                 sliderInput(
                   inputId = "means_select_n",
                   label = "n:",
                   value = 500,
                   min = 100,
                   max = 1000,
                   step = 50
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
                   value = 2,
                   min = 0,
                   max = 10,
                   step = 1
                 )
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
                     max = n_frames,
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
                     max = n_frames,
                     step = 1,
                     animate = animationOptions(interval = 125, loop = FALSE)
                   )
                 )
               )
             )),
           mainPanel(
             width = 6,
             tabsetPanel(
               id = "means_tabs",
               type = "tabs",
               tabPanel("[SATE]",
                        plotOutput("means_plot_SATE", height = 500),
                        br(),
                        actionButton(inputId = "means_button_play",
                                     label = "Animate"),
                        br(), br(),
                        actionButton(inputId = "means_button_reset_SATE",
                                     label = "Reset animation"),
                        tags$script(
                          # this is a jquery script that executes the play button on means_slider_frame when the user
                          # clicks the #means_button_play. Its a bit of a hack
                          "$('#means_button_play').click(function(){
                             {$('#div_means_slider_frame_SATE .slider-animate-button').click()};
                             });")
                        ), 
               tabPanel("[Estimating SATE]",
                        plotOutput("means_plot_est_SATE", height = 500)), 
               tabPanel("[Regression]",
                        plotOutput('means_plot_regression', height = 500)),
               tabPanel("[Bias and efficiency]")
             )
           )
         )
)