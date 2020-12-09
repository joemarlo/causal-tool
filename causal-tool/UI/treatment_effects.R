UI_treatment_effects <- tabPanel(
  "Estimating treatment effects",
      sidebarLayout(
        sidebarPanel(
             width = 5,
             includeMarkdown('markdowns/treatment_effects.md'),
             br(), 
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
                     label = "Calculating SATE frame:",
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
               tabPanel("Calulating SATE",
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
    )
