UI_fundamental_problem <- tabPanel(title = "Fundamental problem",
         navlistPanel(
           id = "fundamental_nav",
           widths = c(1, 11),
           tabPanel("1)",
                    sidebarLayout(
                      sidebarPanel(
                        width = 4,
                        h3("What is the fundamental problem of causal inference?"),
                        h5("Causal inference is fundamentally a missing data problem. We can only observe one potential outcome. We see that the person either receives the treatment or doesn't receive the treatment."),
                        h5("For example, if a person smokes, will they have a heart attack?"),
                      ),
                      mainPanel(width = 6,
                                plotOutput("fundamental_plot_one"))
                    )),
           tabPanel("2)",
                    sidebarLayout(
                      sidebarPanel(
                        width = 4,
                        h3("What is the fundamental problem of causal inference?"),
                        h5("But what if they didn't smoke, would they still have a heart attack?"),
                      ),
                      mainPanel(width = 6,
                                plotOutput("fundamental_plot_two"))
                    )),
           tabPanel("3)",
                    sidebarLayout(
                      sidebarPanel(
                        width = 4,
                        h3("What is the fundamental problem of causal inference?"),
                        h5("Seeing both outcomes, heart attack and no heart attack, is impossible. This is the observed outcome and one potential counterfactual.")
                      ),
                      mainPanel(width = 6,
                                plotOutput("fundamental_plot_three"))
                    )),
           tabPanel("4)",
                    sidebarLayout(
                      sidebarPanel(
                        width = 4,
                        h3("What is the fundamental problem of causal inference?"),
                        h5("And, in many cases, there are confounding and latent variables that further complicate the ability to draw inference.")
                      ),
                      mainPanel(width = 6,
                                plotOutput("fundamental_plot_four"))
                    ))
         ))