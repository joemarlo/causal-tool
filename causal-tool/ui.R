shinyUI(fluidPage(
    # download roboto font
    HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
    
    # set default slider skin
    chooseSliderSkin(skin = "Flat", color = "#221146"),
    
    # load custom CSS file
    includeCSS("www/custom_css.css"),
    
    # set top left title
    titlePanel(
        title = h1("NYU Causal Inference"),
        windowTitle = "NYU Causal"
    ),
    
    # initiate shinyjs
    useShinyjs(),

    # prevents flickering on plot refresh
    tags$style(type="text/css", "#output_id.recalculating { opacity: 1.0; }"),
    
    navbarPage(
        title = 'NYU',
        id = "nav",
        UI_welcome,
        UI_fundamental_problem,
        UI_randomization,
        UI_treatment_effects,
        UI_propensity_scores,
        UI_regression_discontinuity
        )
))
