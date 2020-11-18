library(tidyverse) # for ggplot, dplyr data munging
library(shiny)
library(shinyWidgets) # for alerts
# library(DT) # for UI tables
# library(gridExtra) # for combining multiple ggplots into one object 
# library(shinyjs) # for running javascript on the server side
library(shinyBS) # for popovers
library(viridis) # for better colors for color blind people
set.seed(44)

ggplot2::theme_set(theme_minimal())

# load data for propensitiy scores
load('data/hw4.Rdata')

# filter for observations with birthweight less than 3000g
filtered_df <- hw4[hw4$bw < 3000,]

# define variables of interest
vars <- unique(setdiff(c("ppvtr.36", colnames(filtered_df)), c("momed", "st99")))
final_df <- filtered_df[, vars]

