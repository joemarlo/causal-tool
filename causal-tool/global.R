library(tidyverse) # for ggplot, dplyr data munging
library(shiny)
library(shinyWidgets) # for slider skins
library(shinyjs) # for running javascript on the server side
library(shinyBS) # for popovers
library(viridis) # for better colors for color blind people
set.seed(44)

# read in master df
master_df <- read_csv(
  'data/master_df.csv',
  col_types = cols(
    Cholesterol_LDL = col_double(),
    Cholesterol_HDL = col_double(),
    Age = col_double(),
    Genetic_disposition = col_double(),
    Packs_per_day = col_double(),
    Exercise_per_week = col_double(),
    treat = col_double()
  )
) %>% as.data.frame()
rownames(master_df) <- 1:nrow(master_df)

# set number of frames to render for animated ggplots
n_frames <- 30

# source UI code
map(list.files('UI'), function(file) source(file.path("UI", file)))
