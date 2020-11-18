shinyServer(function(input, output) {



# fundamental problem -----------------------------------------------------

  output$fundamental_plot_one <- renderPlot({
    
    simple_dag <- dagify(
      Heart_attack ~ Smokes
    )
    
    ggdag(
      simple_dag,
      node_size = 20,
      stylized = TRUE,
      text_size = 2
    ) + theme_void()
    
  })
  
  output$fundamental_plot_two <- renderPlot({
    
    simple_not_dag <- dagify(
      No_heart_attack ~ Does_not_smoke
    )
    
    ggdag(
      simple_not_dag,
      node_size = 20,
      stylized = TRUE,
      text_size = 2
    ) + theme_void()
    
  })  
  
  output$fundamental_plot_three <- renderPlot({
    
    tree_dag <- dagify(
      Heart_attack ~ Smokes,
      No_heart_attack ~ Does_not_smoke,
      Smokes ~ Person,
      Does_not_smoke ~ Person
    ) %>% tidy_dagitty(layout = "tree")
    
    ggdag(
      tree_dag,
      node_size = 20,
      stylized = TRUE,
      text_size = 2
    ) + theme_void()
    
  })
  
  output$fundamental_plot_four <- renderPlot({
    
    heart_dag <- dagify(
      Heart_attack ~ Cholesterol,
      Cholesterol ~ Smoking + Age,
      Smoking ~ Genetics,
      Cholesterol ~ Genetics,
      outcome = "Heart_attack"
    )
    
    ggdag(
      heart_dag,
      node_size = 20,
      stylized = TRUE,
      text_size = 2.5
    ) + theme_void()
    
  })
  
  
# randomization -----------------------------------------------------------

    selected_points <- reactiveValues(car_names = NULL)
    
    observeEvent(input$randomization_plot_click, {
        
        # for each click on the plot, add or remove that datapoint from the list
        #   of selected datapoints
        
        tryCatch({
            event_car <- rownames(nearPoints(mtcars, input$randomization_plot_click, threshold = 7))[1]
            if (event_car %in% selected_points$car_names){
              # remove point from selected list  
              selected_points$car_names <- setdiff(selected_points$car_names, event_car)
            } else if (event_car %in% rownames(mtcars)) {
              # add point to selected list  
              selected_points$car_names <- c(selected_points$car_names, event_car)
              # remove floating UI box
              removeUI(selector = "#randomization_floating_box")
            }},
            error = function(e) e
        )
      
    })
    
    # randomize assignment when user clicks button
    observeEvent(input$randomize_button, {
        selected_points$car_names <- sample(rownames(mtcars), size = nrow(mtcars)/2)
    })

    # remove assignment when user clicks button    
    observeEvent(input$randomize_reset_button, {
        selected_points$car_names <- c()
    })
    
    # top plot    
    output$randomization_plot <- renderPlot({
      mtcars %>%
        dplyr::select(-treat) %>%
        rownames_to_column() %>% 
        mutate(Group = if_else(rowname %in% selected_points$car_names,
                                   'Treatment', 'Control')) %>% 
        ggplot(aes_string(x = sym(input$randomization_variable_x), 
                          y = sym(input$randomization_variable_y))) +
        geom_point(aes(fill = Group), alpha = 0.7, size = 7, 
                       color = 'black', pch = 21, stroke = 1)
    })
    
    # bottom plot
    output$randomization_tc_plot <- renderPlot({
      mtcars %>%
        dplyr::select(-treat) %>%
        rownames_to_column() %>%
        mutate(Group = if_else(rowname %in% selected_points$car_names,
                               'Treatment', 'Control')) %>%
        pivot_longer(cols = where(is.numeric)) %>%
        ggplot(aes(x = value, group = Group, fill = Group)) +
        geom_density(alpha = 0.5) +
        facet_wrap(~name, scales = 'free') +
        labs(title = "Univariate densities of the observed and unobserved variables",
             x = NULL,
             y = NULL) +
        theme(legend.position = 'none')
    })

# difference in means -----------------------------------------------------

    # data generating process
    DGP <- reactive({
        set.seed(1234)
        N <- input$means_select_n
        pre_test_scores <- rnorm(N, 65, 3)
        y_0 <- 10 + input$means_select_slope * pre_test_scores + 0 + rnorm(N, 0, 1)
        y_1 <- 10 + input$means_select_slope * pre_test_scores + input$means_select_tau + rnorm(N, 0, 1)
        
        # randomly assign treatment
        z <- rbinom(n = N, 1, p = 0.5)
        
        # add observed Y
        Y <- (y_0 * -(z - 1)) + (y_1 * z)
        
        return(tibble(x = pre_test_scores, y_0, y_1, z, Y))
    })
    

    
    # summary output
    output$means_summary <- renderText({
        
        dat <- DGP()
        
        SATE <- round(mean(dat$y_1 - dat$y_0), 2)
        SATE_est <-  round(mean(dat$Y[dat$z == 1]) - mean(dat$Y[dat$z == 0]), 2)
        reg <- round(coef(lm(Y ~ x + z, data = dat))[['z']], 2)
        
        paste0("SATE: ", SATE, "<br>",
               "Estimated SATE: ", SATE_est, "<br>",
               "Estimated SATE from regression: ", reg)
    })  
        
    output$means_plot <- renderPlot({
        DGP() %>% 
            pivot_longer(cols = c('y_0', 'y_1')) %>% 
            ggplot(aes(x = x, y = value, group = name, color = name)) +
            geom_point(alpha = 0.5) +
            geom_smooth(method = 'lm', formula = y ~ x)
    })
    
    

# propensity scores -------------------------------------------------------

    output$propensity_plot <- renderPlot({
        
        # fit model
        user_formula <- reformulate(input$propensity_select_independent, 'treat')
            
        if (input$propensity_select_model == "Binomial - logit"){
            model <- glm(user_formula, family = binomial('logit'), data = mtcars)
        } else if (input$propensity_select_model == "Binomial - probit"){
            model <- glm(user_formula,  family = binomial('probit'), data = mtcars)
        } else if (input$propensity_select_model == "GAM"){
            model <- gam::gam(user_formula, data = mtcars)
        } else stop("No model selected")
        
        # get propensity scores
        propensity_scores <- predict(model, type = 'response')
        
        # perform matching
        if (input$propensity_replacement_type_input == 'With'){
            
            # matching with replacement
            matches_with <- arm::matching(z = mtcars$treat, score = propensity_scores, replace = TRUE)
            
            # df of matches
            matches <- matches_with$pairs %>% 
                as_tibble() %>% 
                rename(index = V1, match = V2)
            
        } else {
            # matching without replacement
            matches_wo <- arm::matching(z = mtcars$treat, score = propensity_scores, replace = FALSE)
            
            # df of matches
            matches <- matches_wo %>% 
                as_tibble() %>% 
                mutate(index = row_number()) %>% 
                .[mtcars$treat == 1,] %>% 
                dplyr::select(index, match = matched)
        }
        
        # df of propensity scores
        scores <- tibble(Z = mtcars$treat,
                         score = propensity_scores,
                         index = 1:nrow(mtcars))
        
        # plot
        scores %>%
            left_join(matches,
                      by = 'index') %>%
            left_join(scores %>%
                          dplyr::select(score_match = score,
                                        match = index),
                      by = 'match') %>% 
            mutate(Z = recode(Z, `1` = 'Treatment', `0` = "Control")) %>% 
            ggplot(aes(x = score, y = Z)) +
            geom_point(alpha = 0.5) +
            geom_segment(aes(x = score, xend = score_match,
                             y = 'Treatment', yend = 'Control'),
                         alpha = 0.5,
                         arrow = arrow(length = unit(0.03, "npc"))) +
            labs(x = 'Propensity score',
                 y = NULL)

    })

})
