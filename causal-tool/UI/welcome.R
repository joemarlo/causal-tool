UI_welcome <- tabPanel(title = "Welcome",
                       sidebarLayout(
                         sidebarPanel(width = 7,
                                      includeMarkdown("markdowns/welcome_text.md")),
                         mainPanel(
                           width = 5,
                           plotOutput('welcome_plot', height = 500),
                           br(),
                           actionButton(inputId = 'welcome_button_update_plot',
                                        label = "Refresh plot")
                         )
                       ))