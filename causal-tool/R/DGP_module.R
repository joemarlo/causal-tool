# this defines the repeatedly used 'Data generation process' UI and server-side
  # code used on both 'Treatment effects' pages

dgpUI <- function(id) {
  ns <- NS(id)
  tagList(
      sliderInput(
        inputId = ns("select_tau"),
        label = "Tau:",
        value = 3,
        min = 0,
        max = 20,
        step = 1
      ),
      sliderInput(
        inputId = ns("slider_error"),
        label = "Error:",
        value = 3,
        min = 0,
        max = 10,
        step = 1
      ),
      sliderInput(
        inputId = ns("select_n"),
        label = "n:",
        value = 250,
        min = 100,
        max = 1000,
        step = 50
      ),
      sliderInput(
        inputId = ns("select_slope"),
        label = "Slope:",
        value = 1.1,
        min = 0,
        max = 1,
        step = 0.1
      ),
      HTML('<details><summary>Pseudocode to generate the data</summary>'),
      uiOutput(outputId = ns("DGP_formula")),
      HTML('</details><br>')
  )
}

dgpServer <- function(id) {
  # data generating process
  moduleServer(
    id,
    function(input, output, session) {
      
      # render the DGP pseudocode text
      output$DGP_formula <- renderText({
        paste0(
          'n = ', input$select_n,
          '<br>',
          'x = rnorm(n, mean = 65, sd = 3)',
          '<br>',
          'y_0 = 10 + 0 + ', input$select_slope, ' * x + rnorm(n, mean = 0, sd = ', input$slider_error, ')',
          '<br>',
          'y_1 = 10 + ', input$select_tau, ' + ', input$select_slope, ' * x + rnorm(n, mean = 0, sd = ', input$slider_error, ')'
        )
      })

      # calculate the DGP
      reactive({
        set.seed(1234)
        N <- input$select_n
        pre_test_scores <- rnorm(N, 65, 3)
        y_0 <- 10 + input$select_slope * pre_test_scores + 0 + rnorm(N, 0, input$slider_error)
        y_1 <- 10 + input$select_slope * pre_test_scores + input$select_tau + rnorm(N, 0, input$slider_error)
        
        # randomly assign treatment
        z <- rbinom(n = N, 1, p = 0.5)
        
        # add observed Y
        Y <- (y_0 * -(z - 1)) + (y_1 * z)
        
        return(tibble(x = pre_test_scores, y_0, y_1, z, Y, index = 1:N))
      })
    }
  )
}

