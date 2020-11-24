shinyServer(function(input, output) {


# welcome page ------------------------------------------------------------

  
  observeEvent(input$welcome_button_update_plot, {
    output$welcome_plot <- renderPlot({
      # placeholder plot until spurious correlation dataset can be made
      
      x <- rnorm(100)
      y <- rnorm(100, x)
      
      tibble(x = x, y = y) %>%
        ggplot(aes(x = x, y = y)) +
        geom_point(shape = 21, color = 'grey20', fill = '#4c2e61', 
                   size = 4, alpha = 0.5) +
        geom_smooth(color = 'grey20',
                    method = 'lm',
                    formula = y ~ x) +
        labs(title = paste0("[PLACEHOLDER] Spurious correlations: ", round(cor(x, y), 2), ' correlation'),
             x = NULL,
             y = NULL)
      
    })
  })
  
  # initiate welcome_plot by simulating button click
  shinyjs::click("welcome_button_update_plot")
  

# fundamental problem -----------------------------------------------------

  output$fundamental_plot_one <- renderPlot({
    
    tibble(label = c('Heart\nattack', 'Smokes'),
           x = c(0, 1),
           y = c(0, 1)) %>% 
      ggplot(aes(x = x, y = y)) +
      geom_point(shape = 21, size = 30) +
      geom_text(aes(label = label)) +
      geom_segment(data = tibble(x = 0.9, xend = 0.1,
                                 y = 0.9, yend = 0.1),
                   aes(x = x, xend = xend, y = y, yend = yend),
                   alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                   size = 1.2,
                   arrow = arrow(length = unit(0.04, "npc"))) +
      coord_cartesian(xlim = c(-0.25, 1.25), ylim = c(-0.25, 1.25)) +
      theme_void()
    
    
  })
  
  output$fundamental_plot_two <- renderPlot({
    
    tibble(label = c('No heart\nattack', 'Does not\nsmoke'),
           x = c(0, -1),
           y = c(0, 1)) %>% 
      ggplot(aes(x = x, y = y)) +
      geom_point(shape = 21, size = 30) +
      geom_text(aes(label = label)) +
      geom_segment(data = tibble(x = -0.9, xend = -0.1,
                                 y = 0.9, yend = 0.1),
                   aes(x = x, xend = xend, y = y, yend = yend),
                   alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                   size = 1.2,
                   arrow = arrow(length = unit(0.04, "npc"))) +
      coord_cartesian(xlim = c(-1.25, 0.25), ylim = c(-0.25, 1.25)) +
      theme_void()
    
  })  
  
  output$fundamental_plot_three <- renderPlot({
    
    tibble(label = c('Smokes', 'Heart\nattack', 'Does not\nsmoke', 'No heart\n attack'),
           x = c(-1, -1, 1, 1),
           y = c(-1, -2, -1, -2)) %>% 
      ggplot(aes(x = x, y = y)) +
      geom_point(shape = 21, size = 30) +
      geom_text(aes(label = label)) +
      geom_segment(data = tibble(x = c(-1, 1), xend = c(-1, 1),
                                 y = c(-1.3, -1.3), yend = c(-1.7, -1.7)),
                   aes(x = x, xend = xend, y = y, yend = yend),
                   alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                   size = 1.2,
                   arrow = arrow(length = unit(0.04, "npc"))) +
      geom_segment(data = tibble(x = 0, xend = 0,
                                 y = -0.75, yend = -2.25),
                   aes(x = x, xend = xend, y = y, yend = yend),
                   alpha = 0.5, linetype = 'dashed', 
                   lineend = 'round', linejoin = 'mitre', size = 1.2) +
      coord_cartesian(xlim = c(-1.25, 1.25), ylim = c(-2.3, -0.7)) +
      theme_void()
  
    
  })
  
  output$fundamental_plot_four <- renderPlot({
    
    tibble(label = c('Cholesterol', 'Age', 'Smokes', 'Genetics', 'Heart\nattack'),
           x = c(0, 0, -1, -1, 1),
           y = c(0, 1, -0, -1, 0)) %>% 
      ggplot(aes(x = x, y = y)) +
      geom_point(shape = 21, size = 30) +
      geom_text(aes(label = label)) +
      geom_segment(data = tibble(x = c(-0.8, -1, -0.8, 0.2, 0), xend = c(-0.2, -1, -0.2, 0.8, 0),
                                 y = c(-0.8, -0.7, 0, 0, 0.7), yend = c(-0.2, -0.3, 0, 0, 0.3)),
                   aes(x = x, xend = xend, y = y, yend = yend),
                   alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                   size = 1.2,
                   arrow = arrow(length = unit(0.04, "npc"))) +
      coord_cartesian(xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.25)) +
      theme_void()
    
  })
  
  
# randomization -----------------------------------------------------------

    # initiate list of treatment observations
    selected_points <- reactiveValues(car_names = NULL)
    
    observeEvent(input$randomization_plot_click, {
        
        # for each click on the plot, add or remove that data point from the list
        #   of selected data points
        
        tryCatch({
            event_car <- rownames(nearPoints(master_df, input$randomization_plot_click, threshold = 15, maxpoints = 1))
            if (event_car %in% selected_points$car_names){
              # remove point from selected list  
              selected_points$car_names <- setdiff(selected_points$car_names, event_car)
            } else if (event_car %in% rownames(master_df)) {
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
        selected_points$car_names <- sample(rownames(master_df), size = round(nrow(master_df)/2))
    })

    # remove assignment when user clicks button    
    observeEvent(input$randomize_reset_button, {
        selected_points$car_names <- c()
    })
    
    # top plot    
    output$randomization_plot <- renderPlot({
      master_df %>%
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
      master_df %>%
        dplyr::select(-treat) %>%
        rownames_to_column() %>%
        mutate(Group = if_else(rowname %in% selected_points$car_names,
                               'Treatment', 'Control')) %>%
        pivot_longer(cols = where(is.numeric)) %>%
        ggplot(aes(x = value, group = Group, fill = Group)) +
        geom_density(alpha = 0.5) +
        scale_y_continuous(labels = NULL) +
        facet_wrap(~name, scales = 'free', ncol = 3) +
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
        y_0 <- 10 + input$means_select_slope * pre_test_scores + 0 + rnorm(N, 0, input$means_slider_error)
        y_1 <- 10 + input$means_select_slope * pre_test_scores + input$means_select_tau + rnorm(N, 0, input$means_slider_error)
        
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
        
        tibble(Method = c('SATE', 'Estimated SATE', 'Estimated SATE from regression'),
               Tau = c(SATE, SATE_est, reg)) %>% 
        knitr::kable(digits = 2, format = 'html') %>% 
          kableExtra::kable_styling(
            bootstrap_options = c("striped", "hover", "condensed")
          )
    })  
        
    output$means_plot_SATE <- renderPlotly({
        # DGP() %>% 
        #     pivot_longer(cols = c('y_0', 'y_1')) %>% 
        #     ggplot(aes(x = x, y = value, group = name, color = name)) +
        #     geom_point(alpha = 0.5) +
        #     geom_smooth(method = 'lm', formula = y ~ x)
      
      dat <- DGP()
      
      # create first frame
      base_data <- dat %>%
        mutate(frame = 1) %>%
        pivot_longer(cols = c('y_0', 'y_1'))
      
      # create second frame by summarize the y axis
      mean_y <- dat %>%
        mutate(frame = 2) %>%
        pivot_longer(cols = c('y_0', 'y_1')) %>%
        group_by(name) %>%
        mutate(value = mean(value)) %>% 
        ungroup()
      
      # create third frame
      mean_x <- mean_y %>% 
        mutate(x = mean(x),
               frame = 3)
      
      # create frame for error bars
      # diff <- mean_y %>% 
      #   group_by(name) %>% 
      #   summarize(y_diff = mean(value),
      #             x_diff = mean(x)*1.01,
      #             .groups = 'drop') %>% 
      #   pivot_wider(values_from = c('y_diff', 'x_diff')) %>% 
      #   mutate(frame = 3)
      
      # build the plot
      anim_plot <- base_data %>%
        bind_rows(mean_y, mean_x) %>%
        ggplot() +
        geom_point(aes(frame = frame, x = x, y = value, 
                       group = name, fill = name), 
                   alpha = 0.3, color = 'grey20', pch = 21, 
                   stroke = 0.3, alpha = 0.3, size = 2) +
        # geom_errorbar(data = diff,
        #               aes(frame = frame, x = x_diff_y_0,
        #                   ymin = y_diff_y_0, ymax = y_diff_y_1),
        #               color = 'grey20', size = 2) +
        labs(x = 'x',
             y = 'y')
      
      # animate the plot
      anim_plot <- ggplotly(anim_plot, tooltip = FALSE) %>% 
        config(displayModeBar = FALSE) %>% 
        layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1.1),
               xaxis = list(fixedrange = TRUE),
               yaxis = list(fixedrange = TRUE)) %>% 
        animation_opts(frame = 1500, redraw = FALSE) %>% 
        animation_slider(hide = TRUE) %>% 
        animation_button(label = "Animate")
      
      anim_plot
      
    })

    output$means_plot_est_SATE <- renderPlotly({
      
      dat <- DGP()
      
      # create first frame
      base_data <- dat %>%
        mutate(frame = 1) %>%
        pivot_longer(cols = c('y_0', 'y_1'))
      
      # create second frame that removes the unobserved
      second_frame <- base_data
      second_frame$frame <- 2
      # second_frame[(second_frame$z == 0 & second_frame$name == 'y_0') | (second_frame$z == 1 & second_frame$name == "y_1")] <-
        
      
      # create second frame by summarize the y axis
      mean_y <- second_frame %>%
        mutate(frame = 3) %>% 
        group_by(name) %>%
        mutate(value = mean(value)) %>% 
        ungroup()
      
      # create third frame
      mean_x <- mean_y %>% 
        mutate(x = mean(x),
               frame = 4)
      
      # build the plot
      anim_plot <- base_data %>%
        bind_rows(mean_y, mean_x) %>%
        # bind_rows(second_frame, mean_y, mean_x) %>%
        ggplot() +
        geom_point(aes(frame = frame, x = x, y = value, 
                       group = name, fill = name), 
                   alpha = 0.3, color = 'grey20', pch = 21, 
                   stroke = 0.3, alpha = 0.3, size = 2) +
        labs(x = 'x',
             y = 'y')
      
      # animate the plot
      anim_plot <- ggplotly(anim_plot, tooltip = FALSE) %>% 
        config(displayModeBar = FALSE) %>% 
        layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1.1),
               xaxis = list(fixedrange = TRUE),
               yaxis = list(fixedrange = TRUE)) %>% 
        animation_opts(frame = 1500, redraw = FALSE) %>% 
        animation_slider(hide = TRUE) %>% 
        animation_button(label = "Animate")
      
      anim_plot
      
    }) 
    
    output$means_plot_regression <- renderPlot({
      DGP() %>% 
        pivot_longer(cols = c('y_0', 'y_1')) %>% 
        ggplot(aes(x = x, y = value, group = name, color = name)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = 'lm', formula = y ~ x)
    })
  

# propensity scores -------------------------------------------------------

    # update formula so user can see formula for determine propensity score
    observeEvent(input$propensity_select_covariates, {
      output$propensity_select_treat_formula <- renderText({
        
        validate(
          need(length(input$propensity_select_covariates) > 0, 
               "Please enter at least one covariate")
        )
        
        # get user input covariates and remove last '+'
        covs <- paste0('scale(', paste0(input$propensity_select_covariates, collapse = ") + scale("), ')')
        
        return(c('treat ~ ', covs))
      })
    })
    
    treat <- eventReactive(input$propensity_button_set_treat, {
      # replace treatment vector with a new one derived from users inputs
      if (input$propensity_select_method == 'Dependent on covariates'){
        
        validate(
          need(length(input$propensity_select_covariates) > 0, 
               "Please enter at least one covariate")
        )
        
        # set probability of treatment as linear sum of scaled covariates
        probs <- master_df %>%
          select(all_of(input$propensity_select_covariates)) %>%
          mutate_all(scale) %>% 
          rowSums() %>%
          as.vector()
        
        # scale probs b/t 0:1
        probs <- (probs - min(probs)) / (max(probs) - min(probs))
        
        # calculate treatment
        treat <- rbinom(
          n = nrow(master_df),
          size = 1,
          prob = probs
        )
      } else {
        # random
        treat <- rbinom(nrow(master_df), size = 1, prob = 0.5)
      }
      
      return(treat)
    })
    
    # initiate treat() vector by simulating button click
    shinyjs::click("propensity_button_set_treat")

    p_scores <- reactive({
      # calculate propensity scores based on user input
      
      # update treatment assignment based on user inputs
      master_df$treat <- treat()
      
      # fit model
      user_formula <- reformulate(input$propensity_select_independent,
                                  response = 'treat', intercept = TRUE)
      
      if (input$propensity_select_model == "Binomial - logit") {
        model <- glm(user_formula, family = binomial('logit'), data = master_df)
      } else if (input$propensity_select_model == "Binomial - probit") {
        model <- glm(user_formula, family = binomial('probit'), data = master_df)
      } else if (input$propensity_select_model == "GAM") {
        model <- gam::gam(user_formula, data = master_df)
      } else stop("No model selected")
      
      # get propensity scores
      propensity_scores <- predict(model, type = 'response')
      
      # perform matching
      if (input$propensity_replacement_type_input == 'With') {
        # matching with replacement
        matches_with <- arm::matching(z = master_df$treat, 
                                      score = propensity_scores,
                                      replace = TRUE)
        
        # df of matches
        matches <- matches_with$pairs %>%
          as_tibble() %>%
          rename(index = V1, match = V2)
        
      } else {
        # matching without replacement
        matches_wo <- arm::matching(z = master_df$treat,
                                    score = propensity_scores,
                                    replace = FALSE)
        
        # df of matches
        matches <- matches_wo %>%
          as_tibble() %>%
          mutate(index = row_number()) %>%
          .[master_df$treat == 1, ] %>%
          dplyr::select(index, match = matched)
      }
      
      # df of propensity scores
      scores <- tibble(Z = master_df$treat,
                       score = propensity_scores,
                       index = 1:nrow(master_df))
      
      # combine with matches
      scores <- scores %>%
        left_join(matches, by = 'index') %>%
        left_join(scores %>% dplyr::select(score_match = score, match = index),
                  by = 'match')
      
      return(scores)
      
    })

    # set list to hold user drawn rectangle data
    user_drawn_rectangle <-
      reactiveValues(data = data.frame(
        box_id = numeric(),
        xmin = numeric(), ymin = numeric(),
        xmax = numeric(), ymax = numeric()
      ))
    
    observe({
      # capture the user drawn rectangle
      e <- input$propensity_plot_scores_brush
      if (!is.null(e)) {
        user_drawn_rectangle$data <- data.frame(xmin = e$xmin, ymin = e$ymin, xmax = e$xmax, ymax = e$ymax)
      }
    })
    
    # plot of propensity score densities grouped by treatment status
    output$propensity_plot_scores <- renderPlot({
      
      p_scores() %>%
        mutate(Z = recode(Z, `1` = 'Treatment', `0` = "Control")) %>%
        ggplot() +
        geom_density(aes(x = score, fill = Z, group = Z), alpha = 0.7) +
        geom_rect(data = user_drawn_rectangle$data, 
                  aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), 
                  color = "red", fill = NA) +
        labs(title = 'Unequal distributions indicate observations in one group may have higher probability of being selected into treatment',
             x = 'Propensity score',
             y = NULL) +
        theme(legend.title = element_blank())
      
    })
    
    # plot of propensity score with arrows indicating match b/t treatment and control
    output$propensity_plot_matching <- renderPlot({

        p_scores() %>%
            mutate(Z = recode(Z, `1` = 'Treatment', `0` = "Control")) %>% 
            ggplot(aes(x = score, y = Z)) +
            geom_point(alpha = 0.5) +
            geom_segment(aes(x = score, xend = score_match,
                             y = 'Treatment', yend = 'Control'),
                         alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                         size = 1.2,
                         arrow = arrow(length = unit(0.04, "npc"))) +
            labs(title = 'The arrows show how the treatment observations are matched to the control observations',
                 x = 'Propensity score',
                 y = NULL)

    })


# regression discontinuity ------------------------------------------------

    # update text so user can see formula for DGP
    observeEvent(input$disc_select_DGP, {
      output$disc_select_DGP_formula <- renderText({

        n <- input$disc_numeric_n
        tau <- input$disc_numeric_tau
        e <- input$disc_slider_error
        cutoff <- input$disc_numeric_cutoff
        slope <- input$disc_slider_slope
        slope_correction <- 50 - (50 * slope)
                
        if (input$disc_select_DGP == 'Linear'){
          text <- paste0(
            'n = ', n,
            '<br>',
            'age = rnorm(n, 50, 12)',
            '<br>',
            'eligibility = (age > ', cutoff, ')',
            '<br>',
            'y_0 = ', slope_correction, ' + ', slope, ' * age + rnorm(n, 0, ', e, ')',
            '<br>',
            'y_1 = ', slope_correction, ' + ', tau, ' + ', slope, ' + age + rnorm(n, 0, ', e, ')'
          )
        } else if (input$disc_select_DGP == 'Polynomial - quadratic'){
          text <- paste0(
            'n = ', n,
            '<br>',
            'age = rnorm(n, 50, 12)',
            '<br>',
            'eligibility = (age > ', cutoff, ')',
            '<br>',
            'y_0 = 30 + 0 + 0.03 * (age - 40) + 0.03 * (age - 40)^2 + rnorm(n, 0, ', e, ')',
            '<br>',
            'y_1 = 30 + ', tau, ' + 0.03 * (age - 40) + 0.03 * (age - 40)^2 + rnorm(n, 0, ', e, ')'
          )
        } else if (input$disc_select_DGP == 'Polynomial - cubic'){
          text <- paste0(
            'n = ', n,
            '<br>',
            'age = rnorm(n, 50, 18)',
            '<br>',
            'eligibility = (age > ', cutoff, ')',
            '<br>',
            'y_0 = 35 + 0 + 0.0003 * (age - 45) + 0.0003 * (age - 45)^2 + 0.0003 * (age - 45)^3 + rnorm(n, 0, ', e, ')',
            '<br>',
            'y_1 = 35 + ', tau, ' + 0.0003 * (age - 45) + 0.0003 * (age - 45)^2 + 0.0003 * (age - 45)^3 + rnorm(n, 0, ', e, ')'
          )
          
        } else {text <- 'No DGP code available'}
        
        return(text)
      })
    })
    
    disc_data <- reactive({

      # the data generating process for regression discontinuity
      
      # set user inputs
      cutoff <- input$disc_numeric_cutoff
      tau <- input$disc_numeric_tau
      n <- input$disc_numeric_n
      e <- input$disc_slider_error
      slope <- input$disc_slider_slope
      
      # if slope is reduced from 1 then the y values are reduced
      # this adds a correction so they still plot in roughly the same area
      slope_correction <- 50 - (50 * slope)
      
      # generate age and elgibility vectors
      age <- rnorm(n, 50, 12)
      eligible <- (age > cutoff)
      
      # generate the data per the user input
      if (input$disc_select_DGP == 'Linear'){
        y_0 <- 0 + (slope * age) + rnorm(n, 0, e) + slope_correction
        y_1 <- 0 + tau + (slope * age) + rnorm(n, 0, e) + slope_correction
      } else if (input$disc_select_DGP == 'Polynomial - quadratic'){
        y_0 <- 30 + 0 + 0.03 * (age - 40) + 0.03 * (age - 40)^2 + rnorm(n, 0, e)
        y_1 <- 30 + tau + 0.03 * (age - 40) + 0.03 * (age - 40)^2 + rnorm(n, 0, e)
      } else if (input$disc_select_DGP == 'Polynomial - cubic'){
        # regenerate a wider age vector b/c the polynomial 'squishs' the data along the x
        age <- rnorm(n, 50, 18)
        eligible <- (age > cutoff)
        y_0 <- 35 + 0 + 0.0003 * (age - 45) + 0.0003 * (age - 45)^2 + 0.0003 * (age - 45)^3 + rnorm(n, 0, e)
        y_1 <- 35 + tau + 0.0003 * (age - 45) + 0.0003 * (age - 45)^2 + 0.0003 * (age - 45)^3 + rnorm(n, 0, e)
      }
      
      # scale the numbers between 0 and 100. This may throw off the numbers
        # a little but ensures everything is within the plot limits
      # x <- c(y_0, y_1)
      # scaled <- ((x - min(x)) / (max(x) - min(x))) * 100
      # rm(x)
      # y_0 <- scaled[0:length(y_0)]
      # y_1 <- scaled[(length(y_0)+1):(length(y_0) + length(y_1))]
      
      # store the results in separate dataframes
      full <- data.frame(age, eligible, y_0, y_1)
      obs <- data.frame(age, eligible, y = y_0 * (eligible == 0) + y_1 * eligible)
      
      return(list('full' = full, 'observed' = obs))
    })
      
    # observable data only plot
    output$disc_plot_observable <- renderPlot({

      cutoff <- input$disc_numeric_cutoff 
      min_age <- cutoff - (input$disc_numeric_window / 2)
      max_age <- cutoff + (input$disc_numeric_window / 2)
      
      data_obs <- disc_data()[['observed']]
      dat_cut <- data_obs[data_obs$age >= min_age & data_obs$age <= max_age,]
      
      p <- data_obs %>% 
        ggplot(aes(x = age, y = y, fill = as.logical(eligible))) +
        geom_rect(data = tibble(xmin = min_age, xmax = 0, ymin = 0, ymax = 100),
                  inherit.aes = FALSE,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = 'grey70', alpha = 0.4) +
        geom_rect(data = tibble(xmin = max_age, xmax = 100, ymin = 0, ymax = 100),
                  inherit.aes = FALSE,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = 'grey70', alpha = 0.4) +
        geom_vline(xintercept = input$disc_numeric_cutoff, 
                   color = 'black', linetype = 'dashed') +
        geom_point(color = 'grey40', pch = 21, stroke = 1, alpha = 0.3, size = 3) +
        scale_x_continuous(breaks = seq(0, 100, by = 10)) +
        coord_cartesian(xlim = c(10, 90), ylim = c(0, 100)) +
        labs(title = "The researcher's perspective: only the observable data",
             x = "Age",
             y = "Health",
             fill = "Eligibility")
        
      # add regression lines per user input
      if (input$disc_select_model == 'Linear'){
        p <- p +
          geom_smooth(data = dat_cut, color = 'grey10',
                      method = 'lm', formula = y ~ x)
      } else if (input$disc_select_model == 'Polynomial - quadratic'){
        p <- p +
          geom_smooth(data = dat_cut, color = 'grey10',
                      method = 'lm', formula = y ~ poly(x, 2, raw = TRUE))
      } else if (input$disc_select_model == 'Polynomial - cubic'){
        p <- p +
          geom_smooth(data = dat_cut, color = 'grey10',
                      method = 'lm', formula = y ~ poly(x, 3, raw = TRUE))
      }
      
      return(p)
    })
    
    # all-seeing plot
    output$disc_plot_all <- renderPlot({
      
      cutoff <- input$disc_numeric_cutoff 
      min_age <- cutoff - (input$disc_numeric_window / 2)
      max_age <- cutoff + (input$disc_numeric_window / 2)
      
      data_full <- disc_data()[['full']]

      p <- data_full %>% 
        pivot_longer(cols = c('y_1', "y_0")) %>% 
        ggplot(aes(x = age, y = value, fill = name, group = name)) +
        geom_vline(xintercept = input$disc_numeric_cutoff, 
                   color = 'black', linetype = 'dashed') +
        geom_point(color = 'grey40', pch = 21, stroke = 1, alpha = 0.3, size = 3) +
        scale_x_continuous(breaks = seq(0, 100, by = 10)) +
        coord_cartesian(xlim = c(10, 90), ylim = c(0, 100)) +
        labs(title = "An all-seeing entity's perspective: all the data",
             x = "Age",
             y = "Health",
             fill = NULL)
      
      # add regression lines per user input
      if (input$disc_select_model == 'Linear'){
        p <- p +
          geom_smooth(color = 'grey10', method = 'lm', formula = y ~ x)
      }

      if (input$disc_select_model == 'Polynomial - quadratic'){
        p <- p +
          geom_smooth(color = 'grey10', method = 'lm', 
                      formula = y ~ poly(x, 2, raw = TRUE))
      }

      if (input$disc_select_model == 'Polynomial - cubic'){
        p <- p +
          geom_smooth(color = 'grey10', method = 'lm', 
                      formula = y ~ poly(x, 3, raw = TRUE))
      }
      
      return(p)
    })

    # the table of regression estimates
    output$disc_table <- renderText({
      
      data <- disc_data()
      data_full <- data[['full']]
      data_obs <- data[['observed']]

      cutoff <- input$disc_numeric_cutoff 
      min_age <- cutoff - (input$disc_numeric_window / 2)
      max_age <- cutoff + (input$disc_numeric_window / 2)
      data_window <- data_obs[data_obs$age >= min_age & data_obs$age <= max_age,]
      
      # all observable data within the window
      model_A_lm <- lm(y ~ age + eligible,  data = data_window)
      # model_A_int <- lm(y ~ age * eligible, data = data_window)
      model_A_quad <- lm(y ~ age * eligible + I(age^2) * eligible, data = data_window)
      model_A_cubic <- lm(y ~ age * eligible + I(age^2) * eligible + I(age^3) * eligible, 
                          data = data_window)
      
      # all observable data
      model_A_lm_all <- lm(y ~ age + eligible, data = data_obs)
      # model_A_int_all <- lm(y ~ age * eligible, data = data_obs)
      model_A_quad_all <- lm(y ~ age * eligible + I(age^2) * eligible, data = data_obs)
      model_A_cubic_all <- lm(y ~ age * eligible + I(age^2) * eligible + I(age^3) * eligible, data = data_obs)
  
      # all data
      # model_A_lm_god <- lm(y ~ age + eligible, data = data_full)
      # model_A_int_god <- lm(y ~ age * eligible, data = data_full)
      # model_A_quad_god <- lm(y ~ age * eligible + I(age^2) * eligible, data = data_full)
      
      # TODO make sure these estimates match the possible user-input relationship types
      # add 'all data' god-view column
      estimates <- tibble(
        # Model = c("Linear model", "Linear model w/interaction", 'Quadratic model', 'Cubic model', 'Difference in means'),
        Model = c("Linear model", 'Quadratic model', 'Cubic model', 'Difference in means'),
        `Data within bandwidth` = c(coef(model_A_lm)[['eligibleTRUE']],
                                 # coef(model_A_int)[['eligibleTRUE']] 
                                 #  + (cutoff * coef(model_A_int)[['age:eligibleTRUE']]),
                                 coef(model_A_quad)[['eligibleTRUE']]
                                   + (cutoff * coef(model_A_quad)[['age:eligibleTRUE']])
                                   + (cutoff^2 * coef(model_A_quad)[['eligibleTRUE:I(age^2)']]),
                                 coef(model_A_cubic)[['eligibleTRUE']]
                                  + (cutoff * coef(model_A_cubic)[['age:eligibleTRUE']])
                                  + (cutoff^2 * coef(model_A_cubic)[['eligibleTRUE:I(age^2)']])
                                  + (cutoff^3 * coef(model_A_cubic)[['eligibleTRUE:I(age^3)']]),
                                 diff(tapply(data_window$y, INDEX = data_window$eligible, FUN = mean))),
        `All observable` = c(coef(model_A_lm_all)[['eligibleTRUE']],
                                  # coef(model_A_int_all)[['eligibleTRUE']] 
                                  #   + (cutoff * coef(model_A_int_all)[['age:eligibleTRUE']]),
                                  coef(model_A_quad_all)[['eligibleTRUE']]
                                    + (cutoff * coef(model_A_quad_all)[['age:eligibleTRUE']])
                                    + (cutoff^2 * coef(model_A_quad_all)[['eligibleTRUE:I(age^2)']]),
                                  coef(model_A_cubic_all)[['eligibleTRUE']]
                                    + (cutoff * coef(model_A_cubic_all)[['age:eligibleTRUE']])
                                    + (cutoff^2 * coef(model_A_cubic_all)[['eligibleTRUE:I(age^2)']])
                                    + (cutoff^3 * coef(model_A_cubic_all)[['eligibleTRUE:I(age^3)']]),
                                  diff(tapply(data_obs$y, INDEX = data_obs$eligible, FUN = mean))),
        ` ` = c(rep(NA, 3),
                       mean(data_full$y_1 - data_full$y_0))) %>% 
        knitr::kable(digits = 2, format = 'html') %>% 
        kableExtra::add_header_above(c("", "Observable data" = 2, 'All data' = 1)) %>% 
        kableExtra::kable_styling(
          bootstrap_options = c("striped", "hover", "condensed")
        )

    return(estimates)
    
    })
    
})
