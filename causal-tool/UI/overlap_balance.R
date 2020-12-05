UI_overlap_balance <- tabPanel(title = "[Overlap and balance]",
         sidebarLayout(
           sidebarPanel(
             width = 5,
             img(src = 'stripes.gif'),
             h3("Assessing overlap and balance"),
             h5("The goal is to have empirical counterfactuals for each member of our treatment group (for ATT) or control group (for ATE). This can be assessed by plotting the propensity scores for each group and assessing the overlap. If there isn't sufficient overlap, then we can surmise the two groups are too different to be compared."),
             br()
           ),
           mainPanel(
             width = 7,
             tabsetPanel(
               id = "propensity_tabs_overlap",
               type = "tabs",
               tabPanel("[Overlap]"),
               tabPanel("[Balance]")
             )
           )
         )
)