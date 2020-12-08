UI_welcome <- tabPanel(
  title = "Welcome",
  sidebarLayout(
    sidebarPanel(
      width = 6,
      includeMarkdown("markdowns/welcome.md")),
    mainPanel(
      width = 6,
      img(src = 'workers.gif'),
      br(), br(),
      plotOutput('welcome_plot', height = 500),
      br(),
      actionButton(
        inputId = 'welcome_button_update_plot', 
        label = "Get next plot"
        )
      )
    )
  )