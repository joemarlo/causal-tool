UI_randomization <- tabPanel(title = "Randomization",
         sidebarLayout(
           sidebarPanel(
             width = 5,
             h3("Why do we randomize?"),
             h5("Randomization balances groups on both observed and unobserved characteristics. See this for yourself. First set the x and y variables to observe."),
             br(),
             selectInput(
               inputId ="randomization_variable_x",
               label = "(Observed) x variable: ",
               multiple = FALSE,
               choices = setdiff(colnames(master_df), "treat"),
               selected = setdiff(colnames(master_df), "treat")[1]
             ),
             selectInput(
               inputId ="randomization_variable_y",
               label = "(Observed) y variable: ",
               multiple = FALSE,
               choices = setdiff(colnames(master_df), "treat"),
               selected = setdiff(colnames(master_df), "treat")[3]
             ),
             h5("Now select which datapoints to include in the treatment group by clicking on points in the plot."),
             h5("How do the univariate densities compare between treatment and control?"),
             h5("Now randomize the selections using the below button. How do the densities compare now?"),
             br(),
             actionButton(inputId = 'randomize_button',
                          label = "Randomize the treatment assignment"),
             br(), br(),
             actionButton(inputId = 'randomize_reset_button',
                          label = "Reset the treatment assignment"),
             br(), br(),
             HTML('<details><summary>What is this data?</summary>'),
             HTML('<br>The data are simulated. The values and correlations are reasonable but please do not make any material conclusions from the data.</a'),
             HTML('</details><br>')
           ),
           mainPanel(
             width = 7,
             plotOutput('randomization_plot',
                        click = "randomization_plot_click"),
             br(),
             plotOutput('randomization_tc_plot', height = 500),
             absolutePanel(id = "randomization_floating_box", 
                           class = "floating_message",
                           top = 50, left = "auto", right = 50, bottom = "auto",
                           width = "30%", height = "auto", draggable = FALSE,
                           "Click on points to assign them to treatment!")
           )
         ))