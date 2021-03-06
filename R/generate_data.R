library(tidyverse)
set.seed(44)

n <- 100

scale_between <- function(x, min, max){
  scaled_01 <- (x - min(x)) / (max(x) - min(x))
  out <- (scaled_01 * (max - min)) + min
  return(out)
}

# create base dataframe
master_df <- tibble(
  Packs_per_day = rbinom(n, 1, 0.3) * rpois(n, 5),
  Genetic_disposition = scale_between(rnorm(n, 0, 1), min = 0, max = 1),
  Age = round(rnorm(n, 60, 10))
)

# make exercise dependent on smoking
master_df$Exercise_per_week <-
  rpois(n, scale_between(max(master_df$Packs_per_day) - master_df$Packs_per_day, 
                         min = 0, max = 5))

# make cholesterol dependent on age, smoking, genetics, exercise
mean_vec <- scale_between(master_df$Age, 0.2, 10) * 
  scale_between(master_df$Genetic_disposition, 0.5, 1.5) * 
  scale_between(master_df$Packs_per_day, 0.7, 1.2) *
  scale_between(max(master_df$Exercise_per_week) - master_df$Exercise_per_week, 0.5, 1.5)
mean_vec <- scale_between(mean_vec, 60, 140)
master_df$Cholesterol_HDL <- rnorm(n, mean_vec, 15)
mean_vec <- scale_between(master_df$Cholesterol_HDL, 33, 47)
master_df$Cholesterol_LDL <- rnorm(n, mean_vec, 2)
rm(mean_vec)

# rearrange columns
master_df <- master_df[, c('Cholesterol_LDL', 'Cholesterol_HDL', "Age", 'Genetic_disposition', 'Packs_per_day', 'Exercise_per_week')]

# plot it
pairs(master_df)
cor(master_df) %>% round(3) %>% View

# set initial treatment
master_df$treat <- rbinom(nrow(master_df), size = 1, prob = 0.5)

# write out df
write_csv(master_df, 'causal-tool/data/master_df.csv')
