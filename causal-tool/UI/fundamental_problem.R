UI_fundamental_problem <- tabPanel(
  title = "Fundamental problem",
  fluidPage(
    fluidRow(
      column(
        width = 12,
        scrolly_container(
          "fundamental_scroll",
          scrolly_graph(plotOutput("DAG", height = 500)),
          scrolly_sections(
            h3("What is the fundamental problem of causal inference?"),
            scrolly_section(
              id = "fundamental_scroll_one",
              width = '50%',
              h5("Causal inference is fundamentally a missing data problem. We can only observe one potential outcome. We see that the person either receives the treatment or doesn't receive the treatment."),
              h5("For example, if a person smokes, will they have a heart disease?"),
              br(),br()
              ),
            scrolly_section(
              id = "fundamental_scroll_two",
              width = '50%',
              h5("But what if they didn't smoke, would they still have a heart disease?"),
              br(),br(),br()
              ),
            scrolly_section(
              id = "fundamental_scroll_three",
              width = '50%',
              h5("Seeing both outcomes, heart disease and no heart disease, is impossible. This is the observed outcome and one potential counterfactual."),
              br(),br(),br()
              ),
            scrolly_section(
              id = "fundamental_scroll_four", 
              width = '50%',
              h5("And, in many cases, there are confounding and latent variables that further complicate the ability to draw inference."),
              br(),br(),br(),br(),br(),br(),br(),br(),br()
              )
            )
          )
        )
      )
    )
  )