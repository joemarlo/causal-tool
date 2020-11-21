library(tidyverse) # for ggplot, dplyr data munging
library(shiny)
library(shinyWidgets) # for alerts
# library(DT) # for UI tables
# library(gridExtra) # for combining multiple ggplots into one object 
# library(shinyjs) # for running javascript on the server side
library(shinyBS) # for popovers
library(viridis) # for better colors for color blind people
library(ggdag) # for DAG plots
library(plotly) # for animated plots
set.seed(44)

# load data for propensitiy scores
# load('data/hw4.Rdata')

# filter for observations with birthweight less than 3000g
# filtered_df <- hw4[hw4$bw < 3000,]

# define variables of interest
# vars <- unique(setdiff(c("ppvtr.36", colnames(filtered_df)), c("momed", "st99")))
# final_df <- filtered_df[, vars]

# read in master df
master_df <- read_csv('data/master_df.csv') %>% as.data.frame()
rownames(master_df) <- 1:nrow(master_df)

# # set master dataset
# master_df <- diamonds %>%
#   filter(cut == 'Good',
#          color == 'G',
#          clarity == 'VS2') %>%
#   filter(!(x == 0 | y == 0 | z == 0)) %>% 
#   dplyr::select(price, carat, depth, x, y, z) %>% 
#   as.data.frame()
# rownames(master_df) <- 1:nrow(master_df)
# 
# # set initial treatment
# master_df$treat <- rbinom(nrow(master_df), size = 1, prob = 0.5)
# 
