shinyServer(function(input, output) {


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
            model <- glm(user_formula, family = binomial('logit'), data = final_df)
        } else if (input$propensity_select_model == "Binomial - probit"){
            model <- glm(user_formula,  family = binomial('probit'), data = final_df)
        } else if (input$propensity_select_model == "GAM"){
            model <- gam::gam(user_formula, data = final_df)
        } else stop("No model selected")
        
        # get propensity scores
        propensity_scores <- predict(model, type = 'response')
        
        # perform matching
        if (input$propensity_replacement_type_input == 'With'){
            
            # matching with replacement
            matches_with <- arm::matching(z = final_df$treat, score = propensity_scores, replace = TRUE)
            
            # df of matches
            matches <- matches_with$pairs %>% 
                as_tibble() %>% 
                rename(index = V1, match = V2)
            
        } else {
            # matching without replacement
            matches_wo <- arm::matching(z = final_df$treat, score = propensity_scores, replace = FALSE)
            
            # df of matches
            matches <- matches_wo %>% 
                as_tibble() %>% 
                mutate(index = row_number()) %>% 
                .[final_df$treat == 1,] %>% 
                dplyr::select(index, match = matched)
        }
        
        # df of propensity scores
        scores <- tibble(Z = final_df$treat,
                         score = propensity_scores,
                         index = 1:nrow(final_df))
        
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
