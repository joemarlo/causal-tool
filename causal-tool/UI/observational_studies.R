UI_observational_studies <- tabPanel(
    title = "[Observational studies]",
    sidebarLayout(
      sidebarPanel(
        width = 5,
        img(src = 'under_construction.gif'),
        h3("Observational studies"),
        h5("Tellus id interdum velit laoreet id donec ultrices tincidunt arcu. Imperdiet nulla malesuada pellentesque elit eget. Commodo sed egestas egestas fringilla phasellus faucibus scelerisque eleifend."),
        h5("Draw a rectangle on the plot to select which data you believe is overlapping"),
        br(),
        tabsetPanel(
          id = 'obs_tabs_left',
          type = 'tabs',
          tabPanel(
            "Analysis tools",
            br(),
            ),
          tabPanel(
            'Data generation process',
            br()
          ))
      ),
      mainPanel(
        width = 7,
        tabsetPanel(
          id = "obs_tabs_right",
          type = "tabs",
          tabPanel("[Observable data]",
                   br(),
                   plotOutput('obs_plot_observable', height = 600,
                              brush = 'obs_plot_observable_brush')),
          tabPanel("[All data]",
                   br(),
                   plotOutput("obs_plot_all", height = 600)
          )
        )
        )
      )
  )

