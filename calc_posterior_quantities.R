
# calculates EVdiff and xR_optimal for every pitch in the dataset
# flexBART download: https://github.com/skdeshpande91/flexBART

# Setup -------------------------------------------------------------------

# SPECIFY DIRECTORY WHERE DATA IS LOCATED
data_dir <- "../data/"

# SPECIFY DIRECTORY WHERE MODELS ARE SAVED
model_dir <- "../models/"

# packages
library(tidyverse)
library(flexBART)
library(scales)

# Load Data ---------------------------------------------------------------

# pitch data
load(paste0(data_dir, "pitch_data.RData"))

# re-scale pitch data to same scaling used to fit models
pitch_data$plate_x <- scales::rescale(pitch_data$plate_x, to = c(-1,1), from = range(pitch_data$plate_x))
pitch_data$plate_z <- scales::rescale(pitch_data$plate_z, to = c(-1,1), from = range(pitch_data$plate_z))
pitch_data$b_cwoba <- scales::rescale(pitch_data$b_cwoba, to = c(-1,1), from = range(pitch_data$b_cwoba))
pitch_data$p_cwoba <- scales::rescale(pitch_data$p_cwoba, to = c(-1,1), from = range(pitch_data$p_cwoba))

# strike model
load(paste0(model_dir, "probit_BART_strike_model.RData"))

# contact model
load(paste0(model_dir, "probit_BART_contact_model.RData"))

# BARTxR
load(paste0(model_dir, "BARTxR_model.RData"))

# Get Predictions ---------------------------------------------------------

### EVENT PROBABILITIES ###

X_cont <- pitch_data %>% 
  select(
    balls, strikes, outs_when_up, inning, stand, p_throws, is_same_hand, 
    is_on_3b, is_on_2b, is_on_1b, inning_topbot, plate_x, plate_z, deficit,
    b_cwoba, p_cwoba
    ) %>% 
  as.matrix()

X_cat <- pitch_data %>% 
  select(batter, pitcher, catcher, umpire) %>% 
  as.matrix()

strike_probability <- predict_flexBART(probit_BART_strike_model, 
                                       X_cont = X_cont, 
                                       X_cat = X_cat, 
                                       print_every = 250)

contact_probability <- predict_flexBART(probit_BART_contact_model,
                                        X_cont = X_cont, 
                                        X_cat = X_cat, 
                                        print_every = 250)

### EXPECTED RUNS ###

# Outcomes:
#   miss          = 0
#   contact       = 1
#   called strike = 2
#   called ball   = 3

# Game-States:
#   miss or called strike = 0
#   called ball           = 1
#   contact               = 2

# get predictions for a given outcome
get_xR_samples <- function(model, data, outcome) {
  
  # assign game-state based on outcome
  gamestate_cat <- case_when(
    outcome == 0 ~ 0,
    outcome == 1 ~ 2,
    outcome == 2 ~ 0,
    outcome == 3 ~ 1,
  )
  
  X_cont <- data %>% 
    mutate(
      outcome = outcome, 
      gamestate_cat = gamestate_cat) %>%
    select(
      swing, balls, strikes, outs_when_up, inning,deficit, is_on_3b, is_on_2b,
      is_on_1b, inning_topbot, outcome, gamestate_cat) %>% 
    as.matrix()
  
  xR_samples <- predict_flexBART(model, X_cont = X)
  
  return(xR_samples)
}

xR_miss <- get_xR_samples(BARTxR_model, pitch_data, 0)
xR_contact <- get_xR_samples(BARTxR_model, pitch_data, 1)
xR_strike <- get_xR_samples(BARTxR_model, pitch_data, 2)
xR_ball <- get_xR_samples(BARTxR_model, pitch_data, 3)

# Compute Statistics ------------------------------------------------------

# compute xR_swing and xR_take for each sample
xR_swing <- pcontact * pruns_contact + (1 -  pcontact) * pruns_miss
xR_take <- pstrike * pruns_strike + (1 - pstrike) * pruns_ball

# compute ev_diff for each sample
ev_diff_samples <- xR_swing - xR_take

# compute posterior mean EVdiff
ev_diff <- apply(ev_diff_samples, MARGIN = 1, mean)

# compute xR_optimal
xR_optimal <- as.numeric(ev_diff > 0)

# compute P(xR_optimal = swing)
prob_xR_optimal_swing <- apply(as.matrix(ev_diff_samples), MARGIN = 1, mean)

