shinyUI(fluidPage(
    # download roboto font
    HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
    
    # set default slider skin
    chooseSliderSkin(skin = "Flat", color = "#221146"),
    
    # initiate shinyjs
    useShinyjs(),
    
    # load custom CSS file
    includeCSS("www/custom_css.css"),
    
    # set top left title
    titlePanel(
        title = h1("Causal Inference"),
        windowTitle = "Causal Inference"
    ),
    
    # overall UI structure
    navbarPage(
        title = 'NYU',
        id = "nav",
        UI_welcome,
        navbarMenu(
            title = "Basics",
            UI_fundamental_problem,
            UI_randomization,
            UI_assumptions
        ),
        navbarMenu(
            title = "Treatment effects",
            UI_treatment_effects,
            UI_efficiency,
            UI_bias
        ),
        navbarMenu(
            title = "Observational studies",
            UI_observational_studies,
            UI_propensity_scores,
            UI_overlap_balance
        ),
        UI_regression_discontinuity,
        UI_BART
        )
))
