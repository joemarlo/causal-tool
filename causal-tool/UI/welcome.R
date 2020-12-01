UI_welcome <- tabPanel(
  title = "Welcome",
  sidebarLayout(
    sidebarPanel(
      width = 6,
      includeMarkdown("markdowns/welcome_text.md")),
    mainPanel(
      width = 6,
      plotOutput('welcome_plot', height = 500),
      br(),
      actionButton(
        inputId = 'welcome_button_update_plot', 
        label = "Get next plot"
        )
      )
    )
  )