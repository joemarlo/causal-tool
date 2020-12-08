shinyServer(function(input, output, session) {

# welcome page ------------------------------------------------------------

  # initialize list of used ids that determines which series have been plotted previously
  used_series <- reactiveValues(id = NULL)
  
  # on button click, choose which series to plot and then plot it
  observeEvent(input$welcome_button_update_plot, {
    
    # select a series to plot. this should be random but the first two should
      # be sleep paralysis and then nyu:causal_inference
    if (length(used_series$id) == 0){
      current_id <- 'causal_inference:sleep_paralysis'
    } else if (length(used_series$id) == 1){
      current_id <- 'new_york_university:causal_inference'
    } else{
      current_id <- sample(unique(spurious_df$id), size = 1)
    }
    
    # added used ids to list
    used_series$id <- c(used_series$id, current_id)
    
    # make the plot
    output$welcome_plot <- renderPlot({

      # filter df to that series and clean up names
      df <- spurious_df %>% 
        filter(id == current_id) %>% 
        mutate(article = str_replace_all(article, "_", " "),
               article = str_to_title(article))
      
      # calculate correlation between the series
      corr <- df %>% pivot_wider(names_from = 'article', values_from = 'views')
      corr <- cor(corr[,3], corr[,4])
      
      # plot title
      article_names <- paste0(unique(df$article), collapse = ' vs. ')
      
      # plot it
      df %>% 
        group_by(article) %>% 
        mutate(views = scale(views)) %>% 
        ggplot(aes(x = date, y = views, color = article, fill = article)) +
        geom_line() +
        geom_point(shape = 21, color = 'black', size = 4) +
        scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
        scale_y_continuous(labels = NULL) +
        labs(title = paste0("Spurious correlations: ", article_names),
             subtitle = 'Wikipedia page views over time',
             x = NULL,
             y = NULL,
             fill = NULL,
             color = NULL,
             caption = 'Source: Wikimedia Foundation')

    })
  })
  
  # initiate welcome_plot by simulating button click
  shinyjs::click("welcome_button_update_plot")
  

# fundamental problem -----------------------------------------------------

  # render the DAG based on scroll position
  output$DAG <- renderPlot({
    
    # which plot is the scroll on
    which_plot <- match(input$fundamental_scroll, 
                        paste0('fundamental_scroll_', 
                               c("one", "two", "three", "four", "five")))
    
    # return that plot
    plot <- list(DAG_one, DAG_two, DAG_three, DAG_four, DAG_five)[which_plot]
    
    return(plot)
  })
  
  # render scrollytell
  output$fundamental_scroll <- renderScrollytell(scrollytell())

  
# randomization -----------------------------------------------------------

    # initiate list of treatment observations
    selected_points <- reactiveValues(row_names = NULL)
    
    observeEvent(input$randomization_plot_click, {
      
      # for each click on the plot, add or remove that data point from the list
      #   of selected data points
      
      tryCatch({
        # capture the point form the user click
        event_row <- rownames(
          nearPoints(
              master_df,
              input$randomization_plot_click,
              threshold = 15,
              maxpoints = 1
            )
          )
        
        if (event_row %in% selected_points$row_names) {
          # remove point from selected list
          selected_points$row_names <- setdiff(selected_points$row_names, event_row)
        } else if (event_row %in% rownames(master_df)) {
          # add point to selected list
          selected_points$row_names <- c(selected_points$row_names, event_row)
          # remove floating UI box
          removeUI(selector = "#randomization_floating_box")
        }
      },
      error = function(e) e)
      
    })
    
    # randomize assignment when user clicks button
    observeEvent(input$randomize_button, {
        selected_points$row_names <- sample(rownames(master_df), size = round(nrow(master_df)/2))
    })

    # remove assignment when user clicks button    
    observeEvent(input$randomize_reset_button, {
        selected_points$row_names <- c()
    })
    
    # top plot    
    output$randomization_plot <- renderPlot({
      master_df %>%
        dplyr::select(-treat) %>%
        rownames_to_column() %>% 
        mutate(Group = if_else(rowname %in% selected_points$row_names,
                                   'Treatment', 'Control')) %>% 
        ggplot(aes_string(x = sym(input$randomization_variable_x), 
                          y = sym(input$randomization_variable_y))) +
        geom_point(aes(fill = Group), alpha = 0.7, size = 7, 
                       color = 'black', pch = 21, stroke = 1) +
        labs(fill = NULL)
    })
    
    # bottom plot
    output$randomization_tc_plot <- renderPlot({
      master_df %>%
        dplyr::select(-treat) %>%
        rownames_to_column() %>%
        mutate(Group = if_else(rowname %in% selected_points$row_names,
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
    


# treatment effects -------------------------------------------------------

    # define the data generating process generations using the predefined module
    DGP <- dgpServer(id = "means_")

    # reset animation slider on click
    observeEvent(input$means_button_reset_SATE, {
      updateSliderInput(session = session,
                        inputId = 'means_slider_frame_SATE',
                        value = 1)
    })
    observeEvent(input$means_button_reset_est_SATE, {
      updateSliderInput(session = session,
                        inputId = 'means_slider_frame_est_SATE',
                        value = 1)
    })
    observeEvent(input$means_button_reset_regression, {
      updateSliderInput(session = session,
                        inputId = 'means_slider_frame_regression',
                        value = 1)
    })
    
    # summary output
    output$means_summary <- renderText({
        
        dat <- DGP()
        
        SATE <- round(mean(dat$y_1 - dat$y_0), 2)
        SATE_est <- round(mean(dat$Y[dat$z == 1]) - mean(dat$Y[dat$z == 0]), 2)
        reg <- round(coef(lm(Y ~ x + z, data = dat))[['z']], 2)
        
        tibble(Method = c('Calculated SATE', 'Estimated SATE', 'Regression'),
               Tau = c(SATE, SATE_est, reg)) %>% 
        knitr::kable(digits = 2, format = 'html') %>% 
          kableExtra::kable_styling(
            bootstrap_options = c("striped", "hover", "condensed")
          )
    })  
    
    # render the frames for SATE    
    render_frames_reactive <- reactive(render_frames(dat = DGP()))

    # animate the SATE plot
    output$means_plot_SATE <- renderPlot({
      
      # this animates by interpolating the data and only render the frame
      # according to the slider input
      
      # get data and filter to just one frame
      dat_all <- render_frames_reactive()
      dat <- dat_all %>% filter(frame == input$means_slider_frame_SATE) 
      
      # plot it
      p <- dat %>%
        pivot_longer(cols = c("y_0", "y_1")) %>% 
        ggplot() +
        geom_point(aes(x = x, y = value, fill = name),
                   shape = 21, color = 'grey40', size = 3, stroke = 1, alpha = 0.3) + 
        coord_cartesian(xlim = range(dat_all$x), ylim = range(c(dat_all$y_0, dat_all$y_1))) +
        labs(title = "An all-seeing entity's perspective: all the data",
             x = 'x',
             y = 'y',
             fill = NULL)
      
      # on the last frame, add error bars to highlight difference in points
      if (input$means_slider_frame_SATE == 20){
        bar_position <- tibble(x = mean(dat$x) + 1, ymin = mean(dat$y_0),
                               ymax = mean(dat$y_1))
        p <- p +
          geom_errorbar(data = bar_position,
                        aes(x = x, ymin = ymin, ymax = ymax)) +
          annotate('text', x = bar_position$x + 0.5, y = mean(c(bar_position$ymin, bar_position$ymax)),
                   label = paste0("Difference in means: ", round(mean(dat$y_1 - dat$y_0), 2)),
                   hjust = 0, size = 5)
      }
      
      return(p)
    })
    
    # estimating SATE animation
    output$means_plot_est_SATE <- renderPlot({

      # this animates by interpolating the data and only render the frame
      # according to the slider input
      
      # get data and filter to just one frame
      dat_all <- render_frames_reactive()

      # filter the interpolated data to just the observable data
      all_frames <- dat_all %>% 
        pivot_longer(cols = c('y_0', 'y_1')) %>% 
        filter((z == 0 & name == 'y_0') | (z == 1 & name == 'y_1')) %>%  
        mutate(frame = frame + 6)
      
      # plot the first two frames
      if (input$means_slider_frame_est_SATE %in% 1:2){
        p <- dat_all %>% 
          filter(frame == 1) %>% 
          pivot_longer(cols = c("y_0", "y_1")) %>% 
          ggplot() +
          geom_point(aes(x = x, y = value, fill = name),
                     shape = 21, color = 'grey40', size = 3, stroke = 1, alpha = 0.3) + 
          coord_cartesian(xlim = range(dat_all$x), ylim = range(c(dat_all$y_0, dat_all$y_1))) +
          labs(title = "The researcher's perspective: only the observable data",
               x = 'x',
               y = 'y',
               fill = NULL)
      }
      
      # plot the intermediary frames highlighting the observable data
      if (input$means_slider_frame_est_SATE %in% 2:6){
        p <- dat_all %>% 
          filter(frame == 1) %>% 
          pivot_longer(cols = c("y_0", "y_1")) %>% 
          mutate(observable = Y == value) %>% 
          ggplot() +
          geom_point(aes(x = x, y = value, fill = name, alpha = observable),
                     shape = 21, size = 3, stroke = 1) +
          coord_cartesian(xlim = range(dat_all$x), ylim = range(c(dat_all$y_0, dat_all$y_1))) +
          labs(title = "The researcher's perspective: only the observable data",
               x = 'x',
               y = 'y',
               fill = NULL) +
          scale_fill_discrete(labels = c("y_0", "y_1")) +
          guides(alpha = FALSE)
      }
      
      # plot the rest of the frames
      if (input$means_slider_frame_est_SATE >= 7){
        p <- all_frames %>% 
          filter(frame == input$means_slider_frame_est_SATE) %>%
          ggplot() +
          geom_point(aes(x = x, y = value, fill = as.logical(z)),
                     shape = 21, color = 'grey40', size = 3, stroke = 1, alpha = 0.9) +
          coord_cartesian(xlim = range(dat_all$x), ylim = range(c(dat_all$y_0, dat_all$y_1))) +
          labs(title = "The researcher's perspective: only the observable data",
               x = 'x',
               y = 'y',
               fill = NULL) +
          scale_fill_discrete(labels = c("y_0", "y_1"))
        
      }
 
      # on the last frame, add error bars to highlight difference in points
      if (input$means_slider_frame_est_SATE == 26){

        dat <- dat_all %>% filter(frame == max(frame))
        
        SATE_est <- round(mean(dat$Y[dat$z == 1]) - mean(dat$Y[dat$z == 0]), 2)
        bar_position <- tibble(x = mean(dat$x) + 1, ymin = mean(dat$y_0),
                               ymax = mean(dat$y_1))
        p <- p +
          geom_errorbar(data = bar_position,
                        aes(x = x, ymin = ymin, ymax = ymax)) +
          annotate('text', x = bar_position$x + 0.5, y = mean(c(bar_position$ymin, bar_position$ymax)),
                   label = paste0("Difference in means: ", SATE_est),
                   hjust = 0, size = 5)
      }

      return(p)
    })
    
    # regression plot animation
    output$means_plot_regression <- renderPlot({
      
      # get data and filter to just one frame
      dat_all <- render_frames_reactive()
      
      # filter the interpolated data to just the observable data
      all_frames <- dat_all %>% 
        pivot_longer(cols = c('y_0', 'y_1')) %>% 
        filter((z == 0 & name == 'y_0') | (z == 1 & name == 'y_1')) %>%  
        mutate(frame = frame + 6)
      
      # calculate regression lines
      if (input$means_slider_frame_regression >= 7){
        reg <- coef(lm(Y ~ x + z, data = all_frames %>% filter(frame == 7)))
      }
      
      # plot the first two frames
      if (input$means_slider_frame_regression %in% 1:2){
        p <- dat_all %>% 
          filter(frame == 1) %>% 
          pivot_longer(cols = c("y_0", "y_1")) %>% 
          ggplot() +
          geom_point(aes(x = x, y = value, fill = name),
                     shape = 21, color = 'grey40', size = 3, stroke = 1, alpha = 0.3) + 
          coord_cartesian(xlim = range(dat_all$x), ylim = range(c(dat_all$y_0, dat_all$y_1))) +
          labs(title = "The researcher's perspective: only the observable data",
               x = 'x',
               y = 'y',
               fill = NULL)
      }
      
      # plot the intermediary frames highlighting the observable data
      if (input$means_slider_frame_regression %in% 2:6){
        p <- dat_all %>% 
          filter(frame == 1) %>% 
          pivot_longer(cols = c("y_0", "y_1")) %>% 
          mutate(observable = Y == value) %>% 
          ggplot() +
          geom_point(aes(x = x, y = value, fill = name, alpha = observable),
                     shape = 21, size = 3, stroke = 1) +
          coord_cartesian(xlim = range(dat_all$x), ylim = range(c(dat_all$y_0, dat_all$y_1))) +
          labs(title = "The researcher's perspective: only the observable data",
               x = 'x',
               y = 'y',
               fill = NULL) +
          scale_fill_discrete(labels = c("y_0", "y_1")) +
          guides(alpha = FALSE)
      }
      
      # plot the regression lines
      if (input$means_slider_frame_regression %in% 7:11){
        p <- all_frames %>% 
          filter(frame == 7) %>%
          ggplot(aes(x = x, y = value, fill = as.logical(z))) +
          geom_point(shape = 21, color = 'grey40', size = 3, stroke = 1, alpha = 0.9) +
          geom_abline(intercept = reg['(Intercept)'], slope = reg['x'], 
                      color = '#440154FF', size = 1.1) +
          geom_abline(intercept = reg['(Intercept)'] + reg['z'], slope = reg['x'], 
                      color = '#FDE725FF', size = 1.1) +
          coord_cartesian(xlim = range(dat_all$x), ylim = range(c(dat_all$y_0, dat_all$y_1))) +
          labs(title = "The researcher's perspective: only the observable data",
               x = 'x',
               y = 'y',
               fill = NULL) +
          scale_fill_discrete(labels = c("y_0", "y_1"))
        
      }
      
      # plot the regression lines and remove the data
      if (input$means_slider_frame_regression >= 12){
        p <- all_frames %>% 
          filter(frame == input$means_slider_frame_regression) %>%
          ggplot(aes(x = x, y = value, fill = as.logical(z))) +
          geom_point(alpha = 0) +
          geom_abline(intercept = reg['(Intercept)'], slope = reg['x'], 
                      color = '#440154FF', size = 1.1) +
          geom_abline(intercept = reg['(Intercept)'] + reg['z'], slope = reg['x'], 
                      color = '#FDE725FF', size = 1.1) +
          coord_cartesian(xlim = range(dat_all$x), ylim = range(c(dat_all$y_0, dat_all$y_1))) +
          labs(title = "The researcher's perspective: only the observable data",
               x = 'x',
               y = 'y',
               fill = NULL) +
          scale_fill_discrete(labels = c("y_0", "y_1"))
      }
      
      # on the last frame, add error bars to highlight difference in points
      if (input$means_slider_frame_regression == 15){
        
        dat <- dat_all %>% filter(frame == max(frame))
        bar_position <- tibble(x = mean(dat$x), 
                               ymin = mean(dat$y_0),
                               ymax = mean(dat$y_1))
        p <- p +
          geom_errorbar(aes(x = bar_position$x, ymin = bar_position$ymin, ymax = bar_position$ymax)) +
          annotate('label', x = bar_position$x + 0.5, y = mean(c(bar_position$ymin, bar_position$ymax)),
                   label = paste0("Regression estimate: ", round(reg['z'], 2)),
                   hjust = 0, size = 5)
      }
      
      return(p)
    })
    

    # define the data generating process generations using the predefined module
    DGP_EB <- dgpServer(id = "means_EB_")

    
    observeEvent(input$means_EB_button_run_sim, {
      
      # build randomization distribution plot when user clicks run simulation
      output$means_efficiency_plot_randomization <- renderPlot({
        
        dat <- isolate(DGP_EB())
        n_sims <- isolate(input$means_EB_slider_n_sims)
        n <- nrow(dat)
  
        # run simulation, shuffling the treatment label each time
        sims <- map_dfr(1:n_sims, function(i){
          
          # randomly assign treatment status to each individual
          dat$z <- rbinom(n = n, size = 1, p = 0.5)
          
          # overwrite Y with new 'observed' values
          dat$Y <- (dat$y_0 * -(dat$z - 1)) + (dat$y_1 * dat$z)
          
          # estimate the treatment effect
          SATE_est <- mean(dat$Y[dat$z == 1]) - mean(dat$Y[dat$z == 0])
          reg <- coef(lm(Y ~ x + z, data = dat))[['z']]
          
          return(tibble(`SATE estimate` = SATE_est, `Regression estimate` = reg, ID = i))
        })
        
        # calculate mean per estimator
        group_means <- sims %>%
          pivot_longer(cols = -ID) %>% 
          group_by(name) %>% 
          summarize(mean = mean(value, na.rm = TRUE),
                    .groups = 'drop')
  
        # plot it
        sims %>% 
          pivot_longer(cols = -ID) %>% 
          ggplot(aes(x = value, fill = name)) +
          geom_density(alpha = 0.7) +
          geom_vline(data = group_means, aes(xintercept = mean, color = name)) +
          scale_fill_brewer(palette = 'Dark2') +
          scale_color_brewer(palette = 'Dark2') +
          guides(color = FALSE) +
          labs(x = NULL,
               y = NULL,
               fill = NULL)
  
      })

      # build sampling distribution plot when user clicks run simulation
      output$means_efficiency_plot_sampling <- renderPlot({
        
        n_sims <- isolate(input$means_EB_slider_n_sims)
        
        # run simulation, generating a new dataset each time
        sims <- map_dfr(1:n_sims, function(i){
          
          # generate the data
          dat <- isolate(dgpServer(id = "means_EB_")())
          
          # estimate the treatment effect
          SATE_est <- mean(dat$Y[dat$z == 1]) - mean(dat$Y[dat$z == 0])
          reg <- coef(lm(Y ~ x + z, data = dat))[['z']]
          
          return(tibble(`SATE estimate` = SATE_est, `Regression estimate` = reg, ID = i))
        })
        
        # calculate mean per estimator
        group_means <- sims %>%
          pivot_longer(cols = -ID) %>% 
          group_by(name) %>% 
          summarize(mean = mean(value, na.rm = TRUE),
                    .groups = 'drop')
        
        # plot it
        sims %>% 
          pivot_longer(cols = -ID) %>% 
          ggplot(aes(x = value, fill = name)) +
          geom_density(alpha = 0.7) +
          geom_vline(data = group_means, aes(xintercept = mean, color = name)) +
          scale_fill_brewer(palette = 'Dark2') +
          scale_color_brewer(palette = 'Dark2') +
          guides(color = FALSE) +
          labs(x = NULL,
               y = NULL,
               fill = NULL)
        
      })
    })
    
    # render the DGP pseudocode text
    output$means_bias_DGP_formula <- renderText({
      paste0(
        'n = ', input$means_bias_select_n,
        '<br>',
        'smoker = rbinom(n, size = 1, prob = ', input$means_bias_slider_smoker, ')',
        '<br>',
        'probs = ifelse(smoker == 1, ', input$means_bias_slider_conditional, ', ', 1 - input$means_bias_slider_conditional, ')',
        '<br>',
        'z = rbinom(n, size = 1, p = probs)',
        '<br>',
        'y_0 = 10 + 0 + rnorm(n, mean = 0, sd = ', input$means_bias_slider_error, ')',
        '<br>',
        'y_1 = 10 + (', input$means_bias_slider_tau_nonsmoker, ' * (1 - smoker)) + (', input$means_bias_slider_tau_smoker, ' * smoker) + rnorm(n, mean = 0, sd = ', input$means_bias_slider_error, ')'
      )
    })

    observeEvent(input$means_bias_button_run_sim, {
      
      # build randomization distribution plot when user clicks run simulation
      output$means_bias_plot_randomization <- renderPlot({
        
        # generate the data
        dat <- isolate(
          means_bias_DGP(
            means_bias_select_n = input$means_bias_select_n,
            means_bias_slider_smoker = input$means_bias_slider_smoker,
            means_bias_slider_error = input$means_bias_slider_error,
            means_bias_slider_tau_smoker = input$means_bias_slider_tau_smoker,
            means_bias_slider_tau_nonsmoker = input$means_bias_slider_tau_nonsmoker,
            means_bias_slider_conditional = input$means_bias_slider_conditional
          )
        )
        n_sims <- isolate(input$means_bias_slider_n_sims)
        n <- nrow(dat)
        cond_pr <- isolate(input$means_bias_slider_conditional)
        
        # run simulation, shuffling the treatment label each time
        sims <- map_dfr(1:n_sims, function(i){
          
          # randomly assign treatment but make it conditional on smoker status
          probs <- ifelse(dat$smoker == 1, cond_pr, 1 - cond_pr)
          dat$z <- rbinom(n = n, 1, p = probs)
          
          # overwrite Y with new 'observed' values
          dat$Y <- (dat$y_0 * -(dat$z - 1)) + (dat$y_1 * dat$z)
          
          # estimate the treatment effect
          SATE_est <- mean(dat$Y[dat$z == 1]) - mean(dat$Y[dat$z == 0])
          reg <- coef(lm(Y ~ smoker + z, data = dat))[['z']]
          
          return(tibble(`SATE estimate` = SATE_est, `Regression estimate` = reg, ID = i))
        })
        
        # estimates in the table
        output$means_bias_table <- renderText({
          
          # summary stats: bias, efficiency, RMSE
          true_sate <- mean(dat$y_1 - dat$y_0)
          SATE_bias <- mean(sims$`SATE estimate`) - true_sate
          lm_bias <- mean(sims$`Regression estimate`) - true_sate
          SATE_efficiency <- var(sims$`SATE estimate`)
          lm_efficiency <- var(sims$`Regression estimate`)
          SATE_rmse <- sqrt(mean((sims$`SATE estimate` - true_sate)^2))
          lm_rmse <- sqrt(mean((sims$`Regression estimate` - true_sate)^2))

           # create estimates table
          tibble(
            Description = c("Bias (estimate minus true SATE)", "Efficiency (variance of estimate)", 'RMSE (estimate vs true SATE)'),
            SATE = c(SATE_bias, SATE_efficiency, SATE_rmse),
            Regression = c(lm_bias, lm_efficiency, lm_rmse)
            ) %>%
            knitr::kable(digits = 2, format = 'html', 
                         col.names = c(paste0('True SATE: ', round(true_sate, 2)), "SATE", 'Regression')) %>%
            kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"))
        })
        
        # calculate mean per estimator
        group_means <- sims %>%
          pivot_longer(cols = -ID) %>% 
          group_by(name) %>% 
          summarize(mean = mean(value, na.rm = TRUE),
                    .groups = 'drop')
        
        # plot it
        sims %>% 
          pivot_longer(cols = -ID) %>% 
          ggplot(aes(x = value, fill = name)) +
          geom_density(alpha = 0.7) +
          geom_vline(data = group_means, aes(xintercept = mean, color = name)) +
          geom_vline(xintercept = mean(dat$y_1 - dat$y_0),
                     color = 'black', linetype = 'dashed') +
          annotate(x = mean(dat$y_1 - dat$y_0) * 1.005, y = 0.15, geom = 'text',
                    fill = 'white', angle = 90, label = 'True SATE') +
          scale_fill_brewer(palette = 'Dark2') +
          scale_color_brewer(palette = 'Dark2') +
          guides(color = FALSE) +
          labs(x = NULL,
               y = NULL,
               fill = NULL)
      })
      
      # build sampling distribution plot when user clicks run simulation
      output$means_bias_plot_sampling <- renderPlot({
        
        n_sims <- isolate(input$means_bias_slider_n_sims)

        # run simulation, generating a new dataset each time
        sims <- map_dfr(1:n_sims, function(i){
          
          # generate the data
          dat <- isolate(
            means_bias_DGP(
              means_bias_select_n = input$means_bias_select_n,
              means_bias_slider_smoker = input$means_bias_slider_smoker,
              means_bias_slider_error = input$means_bias_slider_error,
              means_bias_slider_tau_smoker = input$means_bias_slider_tau_smoker,
              means_bias_slider_tau_nonsmoker = input$means_bias_slider_tau_nonsmoker,
              means_bias_slider_conditional = input$means_bias_slider_conditional
            )
          )
                  
          # estimate the treatment effect
          SATE_est <- mean(dat$Y[dat$z == 1]) - mean(dat$Y[dat$z == 0])
          reg <- coef(lm(Y ~ smoker + z, data = dat))[['z']]
          
          return(tibble(`SATE estimate` = SATE_est, `Regression estimate` = reg, ID = i))
        })
        
        # calculate mean per estimator
        group_means <- sims %>%
          pivot_longer(cols = -ID) %>% 
          group_by(name) %>% 
          summarize(mean = mean(value, na.rm = TRUE),
                    .groups = 'drop')
        
        # plot it
        sims %>% 
          pivot_longer(cols = -ID) %>% 
          ggplot(aes(x = value, fill = name)) +
          geom_density(alpha = 0.7) +
          geom_vline(data = group_means, aes(xintercept = mean, color = name)) +
          scale_fill_brewer(palette = 'Dark2') +
          scale_color_brewer(palette = 'Dark2') +
          guides(color = FALSE) +
          labs(x = NULL,
               y = NULL,
               fill = NULL)
        
      })
      
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
        
        # assign treatment 
        treat <- rbinom(
          n = nrow(master_df),
          size = 1,
          prob = probs
        )
      } else if (input$propensity_select_method == 'Random'){
        # random
        treat <- rbinom(nrow(master_df), size = 1, prob = 0.5)
      } else {stop("No selection for 'method to determine treatment'")}
      
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
      
      # calculate propensity scores
      propensity_scores <- predict(model, type = 'response')
      
      # perform matching
      if (input$propensity_match_type_input == 'Nearest neighbor') {
        if (input$propensity_radio_replacement == 'With replacement'){
        # matching with replacement
        matches_with <- arm::matching(z = master_df$treat, 
                                      score = propensity_scores,
                                      replace = TRUE)
        
        # df of matches
        matches <- matches_with$pairs %>%
          as_tibble() %>%
          rename(index = V1, match = V2)
        
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
        
        } else if (input$propensity_radio_replacement == 'Without replacement'){
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
        
        }
      } else if (input$propensity_match_type_input == 'Radius matching'){
        # for radius matching
        # this returns a dataset that has a row for every treatment:control match
          # and is longer than the previous two datasets
        
        scores <- tibble(
          Z = master_df$treat,
          score = propensity_scores
        ) %>% 
          mutate(index = row_number())
        
        # split into one df for controls and one for treatment
        groups <- scores %>% 
          group_by(Z) %>% 
          group_split()
        
        # check to see which control observation are within the caliper width of each treatment observation
        matches <- map2_dfr(.x = groups[[2]]$score, .y = groups[[2]]$index, .f = function(treatment, index){
          in_caliper <- (groups[[1]]$score >= (treatment - (input$propensity_slider_caliper / 2))) & 
            (groups[[1]]$score <= (treatment + (input$propensity_slider_caliper / 2)))
          matches <- groups[[1]]$score[in_caliper]
          return(tibble(index = index, score_match = matches))
        })
        
        # add matches back to scores df
        scores <- scores %>% 
          left_join(matches, by = 'index')
        
        return(scores)
        
      } else stop("No selection for 'Matching type'")
      
    })

    # # set list to hold user drawn rectangle data
    # user_drawn_rectangle <-
    #   reactiveValues(data = data.frame(
    #     box_id = numeric(),
    #     xmin = numeric(), ymin = numeric(),
    #     xmax = numeric(), ymax = numeric()
    #   ))
    # 
    # observe({
    #   # capture the user drawn rectangle
    #   e <- input$propensity_plot_scores_brush
    #   if (!is.null(e)) {
    #     user_drawn_rectangle$data <- data.frame(xmin = e$xmin, ymin = e$ymin, xmax = e$xmax, ymax = e$ymax)
    #   }
    # })
    
    # plot of propensity score densities grouped by treatment status
    output$propensity_plot_scores <- renderPlot({
      # get propensity scores and calculate group means
      dat <- p_scores() %>%
        distinct(score, .keep_all = TRUE) %>%
        mutate(Z = recode(Z, `1` = 'Treatment', `0` = "Control"))
      group_means <- dat %>% 
        group_by(Z) %>% 
        summarize(score = mean(score, na.rm = TRUE), 
                  .groups = 'drop')
      
      # plot it
      dat %>%
        ggplot() +
        geom_density(aes(x = score, fill = Z, group = Z), alpha = 0.7) +
        geom_vline(data = group_means, aes(xintercept = score, color = Z)) +
        scale_x_continuous(limits = 0:1) +
        scale_y_continuous(limits = c(0, 8), labels = scales::comma_format(accuracy = 0.1)) +
        # geom_rect(data = user_drawn_rectangle$data, 
        #           aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), 
        #           color = "red", fill = NA) +
        labs(title = 'Unequal distributions indicate observations in one group have higher probability of being assigned to treatment',
             x = 'Propensity score',
             y = NULL) +
        theme(legend.title = element_blank())
      
    })
    
    # plot of propensity score with arrows indicating match b/t treatment and control
    output$propensity_plot_matching <- renderPlot({

    if (input$propensity_match_type_input == 'Nearest neighbor'){
      p_scores() %>%
        mutate(Z = recode(Z, `1` = 'Treatment', `0` = "Control")) %>%
        ggplot(aes(x = score, y = Z, fill = Z)) +
        geom_segment(aes(x = score, xend = score_match,
                         y = 'Treatment', yend = 'Control'),
                     alpha = 0.4, lineend = 'round', linejoin = 'mitre', color = 'grey20',
                     size = 1.2, arrow = arrow(length = unit(0.06, "npc"))) +
        geom_point(shape = 21, color = 'grey40', size = 3, stroke = 1, alpha = 1) +
        scale_x_continuous(limits = 0:1) +
        labs(title = 'The arrows show how the treatment observations are matched to the control observations\n',
             x = 'Propensity score',
             y = NULL,
             fill = NULL)
      
    } else if (input$propensity_match_type_input == 'Radius matching'){
      
      # pull data and add left and right points for plotting the geom_rect
      dat <- p_scores() %>% 
        mutate(Z = recode(Z, `1` = 'Treatment', `0` = "Control"),
               Z = factor(Z, levels = c('Low', 'Control', 'Treatment', 'High')),
               left = ifelse(Z == 'Treatment', score - (input$propensity_slider_caliper/2), NA),
               right = ifelse(Z == 'Treatment', score + (input$propensity_slider_caliper/2), NA))
      
      dat %>% 
        distinct(index, .keep_all = TRUE) %>%
        ggplot(aes(x = score, y = Z, fill = Z)) +
        geom_vline(aes(xintercept = left), alpha = 0.2) +
        geom_vline(aes(xintercept = right), alpha = 0.2) +
        geom_rect(aes(xmin = left, xmax = right, ymin = 'Low', ymax = 'High'),
                  fill = 'grey80', alpha = 0.1) +
        geom_segment(data = dat,
                     aes(x = score, xend = score_match,
                         y = 'Treatment', yend = 'Control'),
                     alpha = 0.4, lineend = 'round', linejoin = 'mitre', color = 'grey20',
                     size = 1.2, arrow = arrow(length = unit(0.06, "npc"))) +
        geom_point(shape = 21, color = 'grey40', size = 3, stroke = 1, alpha = 1) +
        scale_x_continuous(limits = 0:1) +
        scale_y_discrete(drop = FALSE, labels = c("", "Control", "Treatment", "")) +
        coord_cartesian(ylim = c(2, 3)) +
        labs(title = "The arrows show how the treatment observations are matched to the control observations\nThe bands are the width of the caliper",
             x = 'Propensity score',
             y = NULL,
             fill = NULL)
      
    } else stop("No selection for 'Matching type'")

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
            'age = rnorm(n, mean = 50, sd = 12)',
            '<br>',
            'eligibility = (age > ', cutoff, ')',
            '<br>',
            'y_0 = ', slope_correction, ' + ', slope, ' * age + rnorm(n, mean = 0, sd = ', e, ')',
            '<br>',
            'y_1 = ', slope_correction, ' + ', tau, ' + ', slope, ' * age + rnorm(n, mean = 0, sd = ', e, ')'
          )
        } else if (input$disc_select_DGP == 'Polynomial - quadratic'){
          text <- paste0(
            'n = ', n,
            '<br>',
            'age = rnorm(n, mean = 50, sd = 12)',
            '<br>',
            'eligibility = (age > ', cutoff, ')',
            '<br>',
            'y_0 = 30 + 0 + 0.03 * (age - 40) + 0.03 * (age - 40)^2 + rnorm(n, mean = 0, sd = ', e, ')',
            '<br>',
            'y_1 = 30 + ', tau, ' + 0.03 * (age - 40) + 0.03 * (age - 40)^2 + rnorm(n, mean = 0, sd = ', e, ')'
          )
        } else if (input$disc_select_DGP == 'Polynomial - cubic'){
          text <- paste0(
            'n = ', n,
            '<br>',
            'age = rnorm(n, mean = 50, sd = 18)',
            '<br>',
            'eligibility = (age > ', cutoff, ')',
            '<br>',
            'y_0 = 35 + 0 + 0.0003 * (age - 45) + 0.0003 * (age - 45)^2 + 0.0003 * (age - 45)^3 + rnorm(n, mean = 0, sd = ', e, ')',
            '<br>',
            'y_1 = 35 + ', tau, ' + 0.0003 * (age - 45) + 0.0003 * (age - 45)^2 + 0.0003 * (age - 45)^3 + rnorm(n, mean = 0, sd = ', e, ')'
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
         p <- p  +
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
      } else if (input$disc_select_model == 'Difference in means'){

        # calculate mean per eligibility group
        mean_eligible_0 <- mean(data_obs[data_obs$eligible == 0 & data_obs$age >= min_age & data_obs$age <= max_age, 'y'])
        mean_eligible_1 <- mean(data_obs[data_obs$eligible == 1 & data_obs$age >= min_age & data_obs$age <= max_age, 'y'])
        
        p <- p +
          geom_segment(data = tibble(x = min_age, xend = cutoff, y = mean_eligible_0, yend = mean_eligible_0),
                    inherit.aes = FALSE,
                    aes(x = x, xend = xend, y = y, yend = yend), 
                    size = 1.1, color = 'grey10') +
          geom_segment(data = tibble(x = cutoff, xend = max_age, y = mean_eligible_1, yend = mean_eligible_1),
                       inherit.aes = FALSE,
                       aes(x = x, xend = xend, y = y, yend = yend),
                       size = 1.1, color = 'grey10')
    }
      
      return(p)
    })
    
    # all-seeing plot
    output$disc_plot_all <- renderPlot({
      
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
      
     if (input$disc_select_model == 'Difference in means'){

       # calculate min and max ages for plotting
       min_age <- min(data_full$age, na.rm = TRUE)
       max_age <- max(data_full$age, na.rm = TRUE)
       
       # calculate mean per eligibility group
       mean_eligible_0 <- mean(data_full$y_0, na.rm = TRUE)
       mean_eligible_1 <- mean(data_full$y_1, na.rm = TRUE)
       
       p <- p +
         geom_segment(data = tibble(x = min_age, xend = max_age, y = mean_eligible_0, yend = mean_eligible_0),
                     inherit.aes = FALSE,
                     aes(x = x, xend = xend, y = y, yend = yend),
                     size = 1.1, color = 'grey10') +
         geom_segment(data = tibble(x = min_age, xend = max_age, y = mean_eligible_1, yend = mean_eligible_1),
                     inherit.aes = FALSE,
                     aes(x = x, xend = xend, y = y, yend = yend),
                     size = 1.1, color = 'grey10')
    }
      
      return(p)
    })

    # the table of regression estimates
    output$disc_table <- renderText({
      
      # get the data
      data <- disc_data()
      data_full <- data[['full']]
      data_obs <- data[['observed']]

      # get the user inputs
      cutoff <- input$disc_numeric_cutoff 
      min_age <- cutoff - (input$disc_numeric_window / 2)
      max_age <- cutoff + (input$disc_numeric_window / 2)

      # only the data within the window
      data_window <- data_obs[data_obs$age >= min_age & data_obs$age <= max_age,]
      
      # god view data
      data_god <- data_full %>% 
        select(-eligible) %>% 
        pivot_longer(cols = c("y_0", "y_1"), names_to = 'eligible', values_to = 'y') %>% 
        mutate(eligible = recode(eligible, "y_0" = FALSE, "y_1" = TRUE))
      
      # calculate estimates for each set of data and model type
      estimates <- map2_dfc(
        .x = list(data_window, data_obs, data_god),
        .y = c('Data within bandwidth', 'All Observable', '-'), 
        .f = function(data, name){
        
        # build models
        model_lm <- lm(y ~ age * eligible,  data = data)
        model_quad <- lm(y ~ age * eligible + I(age^2) * eligible, data = data)
        model_cubic <- lm(y ~ age * eligible + I(age^2) * eligible + I(age^3) * eligible, 
                          data = data)
        
        # extract estimates
        coef_lm <- coef(model_lm)[['eligibleTRUE']] + 
          (cutoff * coef(model_lm)[['age:eligibleTRUE']])
        coef_quad <- coef(model_quad)[['eligibleTRUE']] + 
          (cutoff * coef(model_quad)[['age:eligibleTRUE']]) + 
          (cutoff^2 * coef(model_quad)[['eligibleTRUE:I(age^2)']])
        coef_cubic <- coef(model_cubic)[['eligibleTRUE']] + 
          (cutoff * coef(model_cubic)[['age:eligibleTRUE']]) + 
          (cutoff^2 * coef(model_cubic)[['eligibleTRUE:I(age^2)']]) + 
          (cutoff^3 * coef(model_cubic)[['eligibleTRUE:I(age^3)']])
        diff <- diff(tapply(data$y, INDEX = data$eligible, FUN = mean))
        
        # combine results into dataframe
        results <- tibble(tmp = c(coef_lm, coef_quad, coef_cubic, diff)) %>% 
          setNames(name)
        
        return(results)
      })
      
      # which row is currently selected by the user in the 'modeled relationship' dropdown
      selected_row <- match(input$disc_select_model,  
                            c("Linear", "Polynomial - quadratic", "Polynomial - cubic", 'Difference in means'))
     
      # clean up estimates table
      estimates <- estimates %>% 
        bind_cols(Model = c("Linear model", 'Quadratic model', 'Cubic model', 'Difference in means'), .) %>% 
        knitr::kable(digits = 2, format = 'html') %>% 
        kableExtra::add_header_above(c("", "Observable data" = 2, 'All data' = 1)) %>% 
        kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
        kableExtra::row_spec(row = selected_row, bold = TRUE, 
                             italic = TRUE, background = '#ebebfa')
      
    return(estimates)
    })
    
})
