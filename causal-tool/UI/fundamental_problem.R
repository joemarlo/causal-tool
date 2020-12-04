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
              h5("Causal inference is a missing data problem. We can only observe one potential outcome. We see that the person either receives the treatment or doesn't receive the treatment."),
              h5("For example, if a recieves an experiment drug, will they develop heart disease?"),
              br(),br()
              ),
            scrolly_section(
              id = "fundamental_scroll_two",
              width = '50%',
              h5("But what if they don't receive the drug, would they still have heart disease?"),
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
              h5("And, in many cases, there are confounding and latent variables that further complicate the ability to draw causal inference."),
              br(),br(),br(),br(),br(),br(),br(),br(),br()
              )
            )
          ),
        # concluding text
        div(fluidRow(id = 'fundamental_conclusion',
                    column(width = 3),
                    column(width = 6,
                           "We can formalize the fundamental problem by assigning labels based on treatment. For example, assume we're conducting a drug trial to reduce heart disease. Olivia is a participant in this trial and her outcomes can be represented:",
                           withMathJax(),
                           helpText('$$ \\begin{aligned}
                                    Y_{Olivia} &= \\begin{cases}
                                     Y_{Olivia}(0) \\; if \\; Z_{Olivia} = 0 \\\\
                                     Y_{Olivia}(1) \\; if \\; Z_{Olivia} = 1
                                     \\end{cases} \\\\
                                     Y &= \\text{heart disease} \\\\
                                     Z &= \\text{assignment to treatment group}
                                     \\end{aligned}\\!$$'),
                           br(),
                           "And it is often simplified to just:",
                           helpText('$$Y = \\begin{cases}
                                    Y(0) \\; if \\; Z = 0 \\\\
                                    Y(1) \\; if \\; Z = 1
                                    \\end{cases}\\!$$'),
                           br(),
                           "The fundamental problem is we can only observe one of Olivia's potential outcomes but not both.",
                           br(), br()
                    ),
                    column(width = 3)
        ),
        style = 'margin-top: -300px; text-align: center;'
        )
      )
      )
    )
  )