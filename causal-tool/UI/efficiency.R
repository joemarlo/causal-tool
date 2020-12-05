UI_efficiency <- tabPanel(
  "Efficiency",
  sidebarLayout(
    sidebarPanel(
      width = 5,
      includeMarkdown('markdowns/efficiency.md'),
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
)