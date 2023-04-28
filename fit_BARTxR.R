
# fits BART run expectancy model
# flexBART download: https://github.com/skdeshpande91/flexBART

# Setup -------------------------------------------------------------------

# SPECIFY DIRECTORY WHERE DATA IS LOCATED
data_dir <- "../data/"

# SPECIFY DIRECTORY TO SAVE MODEL
model_dir <- "../models/"

library(tidyverse)
library(flexBART)

# Load Data ---------------------------------------------------------------

# xR data
load(paste0(data_dir, "xR_data.RData"))

# flexBART Arguments ------------------------------------------------------

Y <- xR_data$rem_runs %>% as.numeric()

X_cont <- xR_data %>% 
  select(
    swing, balls, strikes, outs_when_up, inning,deficit, is_on_3b, is_on_2b, 
    is_on_1b, inning_topbot, outcome, gamestate_cat
    ) %>% 
  as.matrix()

unif_cuts <- c(T,F,F,F,F,F,F,F,F,F,F,F)

cutpoints_list <- list(c(0,1), #swing
                       seq(0,3), # balls
                       seq(0,2), # strikes
                       seq(0,2), # outs_when_up
                       seq(1,19), # inning
                       seq(min(xR_data$deficit),max(xR_data$deficit)),
                       seq(0,1), # is_on_3b
                       seq(0,1), # is_on_2b
                       seq(0,1), # is_on_1b
                       seq(0,1), # inning_topbot
                       seq(0,3), # outcome
                       seq(0,2)) # gamestate_cat

cat_levels_list <- NULL

# Fit probit_flexBART -----------------------------------------------------

BARTxR_model <- flexBART(Y_train = Y,
                         X_cont_train = X_cont,
                         unif_cuts = unif_cuts,
                         cutpoints_list = cutpoints_list,
                         cat_levels_list = cat_levels_list,
                         save_samples = FALSE,
                         save_trees = TRUE)

save(BARTxR_mode, file = paste0(model_dir, "BARTxR_mode.RData"))

