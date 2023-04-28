
# fits BART probit model to estimate strike probability
# flexBART download: https://github.com/skdeshpande91/flexBART

# Setup -------------------------------------------------------------------

# SPECIFY DIRECTORY WHERE DATA IS LOCATED
data_dir <- "../data/"

# SPECIFY DIRECTORY TO SAVE MODEL
model_dir <- "../models/"

# packages
library(tidyverse)
library(scales)
library(flexBART)

# Load Data ---------------------------------------------------------------

# pitch data
load(paste0(data_dir, "pitch_data.RData"))

# list of batter IDs
load(paste0(data_dir, "unik_batters.RData"))

# list of pitcher IDs
load(paste0(data_dir, "unik_pitchers.RData"))

# list of catcher IDs
load(paste0(data_dir, "unik_catchers.RData"))

# list of umpire IDs
load(paste0(data_dir, "unik_umpires.RData"))

# remove pitches where the batter swings
taken_pitches <- pitch_data %>% filter(swing==0)

# Rescale Variables -------------------------------------------------------

# flexBART works better when continuous variables are rescaled to (-1, 1)
taken_pitches$plate_x <- rescale(pitch_data$plate_x, to = c(-1,1), from = range(pitch_data$plate_x))
taken_pitches$plate_z <- rescale(pitch_data$plate_z, to = c(-1,1), from = range(pitch_data$plate_z))
taken_pitches$b_cwoba <- rescale(pitch_data$b_cwoba, to = c(-1,1), from = range(pitch_data$b_cwoba))
taken_pitches$p_cwoba <- rescale(pitch_data$p_cwoba, to = c(-1,1), from = range(pitch_data$p_cwoba))

# flexBART Arguments ------------------------------------------------------

Y <- taken_pitches$called_strike %>% as.integer()

X_cont <- taken_pitches %>% select(balls,strikes,outs_when_up,inning,stand,p_throws,
                               is_same_hand,is_on_3b,is_on_2b,is_on_1b,inning_topbot,
                               plate_x,plate_z,deficit,b_cwoba,p_cwoba) %>% as.matrix()

X_cat <- taken_pitches %>% select(batter,pitcher,catcher,umpire)

unif_cuts <- c(F,F,F,F,F,F,F,F,F,F,F,T,T,F,T,T)

cutpoints_list <- list(seq(0,3), # balls
                       seq(0,2), # strikes
                       seq(0,2), # outs_when_up
                       seq(1,19), # inning
                       seq(0,1), # stand
                       seq(0,1), # p_throws
                       seq(0,1), # is_same_hand
                       seq(0,1), # is_on_3b
                       seq(0,1), # is_on_2b
                       seq(0,1), # is_on_1b
                       seq(0,1), # inning_topbot
                       c(0), # plate_x
                       c(0), # plate_z
                       seq(min(pitch_data$deficit),max(pitch_data$deficit)),
                       c(0), # b_cwoba
                       c(0)) # p_cwoba

cat_levels_list <- list(seq(0,length(unik_batters)),
                        seq(0,length(unik_pitchers)),
                        seq(0,length(unik_catchers)),
                        seq(0,length(unik_umpires)))

# Fit probit_flexBART -----------------------------------------------------

probit_BART_strike_model <- probit_flexBART(Y_train = Y,
                                            X_cont_train = X_cont,
                                            X_cat_train = X_cat,
                                            unif_cuts = unif_cuts,
                                            cutpoints_list = cutpoints_list,
                                            cat_levels_list = cat_levels_list,
                                            save_samples = FALSE,
                                            save_trees = TRUE)

save(probit_BART_strike_model, file = paste0(model_dir, "probit_BART_strike_model.RData"))

