

# filter for observations with birthweight less than 3000g
filtered_df <- hw4[hw4$bw < 3000,]

# define variables of interest
vars <- unique(setdiff(c("ppvtr.36", colnames(filtered_df)), c("momed", "st99")))
final_df <- filtered_df[, vars]

# fit model
prop_glm <- glm(treat ~ . -ppvtr.36, family = binomial('logit'), data = final_df)
# get propensity scores
propensity_scores <- predict(prop_glm, type = 'response')

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
  labs(title = "Matching with replacement",
       x = 'Propensity score',
       y = NULL)

