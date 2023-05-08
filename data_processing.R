
# processes raw data from scrape_statcast_data.R 
# pitch_data uses 2019 pitches and is used to fit strike and contact probability models
# xR_data uses 2015 - 2018 pitches and is used to fit BARTxR

# Setup -------------------------------------------------------------------

# SPECIFY DIRECTORY WHERE DATA IS SAVED
data_dir <- "../data/"

# load packages
library(tidyverse)
library(baseballr)

# Helper Functions --------------------------------------------------------

# function removes observations with missing or inconsistent data and 
#   transforms certain variables, and selects relevant variables
get_clean_data <- function(data) {
  
  out <- data %>% 
    
    # use only regular season games
    filter(game_type=='R') %>%
    
    # remove pathological observations with 4 balls (~1 per season)
    filter(balls < 4) %>%
    
    # remove observations with no location data
    filter(!is.na(plate_x) & !is.na(plate_z)) %>%
    
    # transform on-base data to categorical
    mutate(is_on_3b=ifelse(is.na(on_3b), 0, 1),
           is_on_2b=ifelse(is.na(on_2b), 0, 1),
           is_on_1b=ifelse(is.na(on_1b), 0, 1),) %>%
    
    # create useful event indicators
    mutate(swing=ifelse(type=='B',0,ifelse(description=='called_strike',0,1)),
           contact=ifelse(swing==0,0,ifelse(description %in% c("swinging_strike",
                                                               "swinging_strike_blocked",
                                                               "missed_bunt"),0,1))) %>%
    # create game state variables
    mutate(is_same_hand=ifelse(stand==p_throws,1,0),
           deficit=fld_score-bat_score,
           rbi=post_bat_score-bat_score) %>%
    
    # create some useful indicator variables
    mutate(called_strike=ifelse(swing==0 & type=="S",1,0),
           called_ball=ifelse(swing==0 & type=="B",1,0),
           inning_topbot=as.numeric(factor(inning_topbot,levels=c('Top','Bot')))-1,
           stand=as.numeric(factor(stand,levels=c('R','L')))-1,
           p_throws=as.numeric(factor(p_throws,levels=c('R','L')))-1) %>%
    
    # arrange by order of pitches
    arrange(game_date, game_pk, inning, at_bat_number, pitch_number) %>%
    
    # calculate runs scored in remainder of half inning
    group_by(game_pk, inning, inning_topbot) %>%
    mutate(rem_runs=ifelse(inning_topbot=="Bot", 
                           post_home_score[n()]-home_score,
                           post_away_score[n()]-away_score)) %>% ungroup %>%
    
    # give fielder_2 a more descriptive name
    rename(catcher=fielder_2) %>%
    
    # select relevant variables
    select(game_date, batter, pitcher, events, stand, p_throws, is_same_hand, type, 
           swing, contact, balls, strikes, game_year, plate_x, plate_z, is_on_3b, 
           is_on_2b, is_on_1b, outs_when_up, inning, inning_topbot, catcher, 
           game_pk, at_bat_number, pitch_number, woba_value, home_score, away_score, 
           post_home_score, post_away_score, deficit, rbi, delta_run_exp,rem_runs)
  
  return(out)
  
}

# gets relevant stats to calculate running wOBA from get_clean_data()
get_woba_stats <- function(data) {
  
  # expected runs based on outs and base configuration
  E <- data %>%
    group_by(outs_when_up, is_on_3b, is_on_2b, is_on_1b) %>%
    mutate(woba_exp_runs=mean(rem_runs), n=length(rem_runs)) %>% ungroup %>%
    arrange(game_pk, game_date, inning, at_bat_number, pitch_number) %>%
    group_by(game_pk, inning, inning_topbot) %>%
    mutate(woba_delta=ifelse(is.na(lead(woba_exp_runs)),-woba_exp_runs+rbi,
                             lead(woba_exp_runs)-woba_exp_runs+rbi)) %>% ungroup
  
  out <- E %>%
    
    # filter out pitches that don't change outs or base configuration
    filter(woba_delta!=0,
           
           # ignore events that don't involve outs or batter production
           !events %in% c("","catcher_interf","ejection","field_error","fielders_choice",
                          "game_advisory","passed_ball","intent_walk","stolen_base_2b",
                          "stolen_base_3b","stolen_base_home","wild_pitch"),
           
           # ignore drop third strikes
           !(events=="strikeout" & woba_value>0)) %>% 
    
    # everything else is either an out or a batter statistic
    mutate(woba_cat=ifelse(events %in% c("walk","hit_by_pitch","single","double", 
                                         "triple","home_run"),events,"out")) %>%
    
    # ignore outs that don't involve the batter when calculating wOBA
    mutate(woba_d=ifelse(woba_cat!="out",1,
                         ifelse(events %in% c("caught_stealing_2b","caught_stealing_3b",
                                              "caught_stealing_home","other_out",
                                              "pickoff_1b","pickoff_2b","pickoff_3b",
                                              "pickoff_caught_stealing_2b",
                                              "pickoff_caught_stealing_3b",
                                              "pickoff_caught_stealing_home"),0,1))) %>%
    
    # calculate the value of the out based on how many outs were made during a given play
    mutate(out_val=ifelse(woba_cat!="out",0,
                          ifelse(!events %in% c("double_play","grounded_into_double_play",
                                                "sac_bunt_double_play","sac_fly_double_play",
                                                "strikeout_double_play","triple_play"),woba_delta,
                                 ifelse(events=="triple_play",woba_delta/3,woba_delta/2))))
  
  return(out)
  
}

# calculates the weights of each wOBA category using get_woba_stats
get_woba_weights <- function(woba_stats) {
  
  # get value of batting events and outs
  W <- woba_stats %>%
    group_by(woba_cat) %>%
    summarize(value=mean(woba_delta), 
              n=sum(woba_d),
              ov=mean(out_val), .groups='drop')
  
  # adjust value of batting event releative to the value of an out
  i <- which(W$woba_cat=='out')
  out_val <- W$ov[i]
  O <- data.frame(woba_cat=W$woba_cat[-i],
                  value=W$value[-i]-out_val,
                  n=W$n[-i])
  
  # scale wOBA to OBP
  league_woba <- sum(O$value*O$n)/sum(W$n)
  league_obp <- sum(O$n)/sum(W$n)
  scale <- league_obp/league_woba
  weights <- data.frame(woba_cat=O$woba_cat,
                        value=O$value*scale,
                        n=O$n) %>% arrange(value)
  
  out <- list(weights=weights,
              scale=scale,
              league_woba=league_woba)
  
  return(out)
  
}

# calculates batters' running wOBA from get_woba_stats, calls get_woba_weights
get_batter_cwoba <- function(woba_stats) {
  
  W <- get_woba_weights(woba_stats)
  weights <- W[["weights"]]$value
  league_woba <- W[["league_woba"]]*W[["scale"]]
  
  out <- woba_stats %>%
    
    arrange(game_date) %>%
    group_by(game_pk, batter) %>%
    
    summarize(walks=sum(woba_cat=="walk"),
              hbp=sum(woba_cat=="hit_by_pitch"),
              singles=sum(woba_cat=="single"),
              doubles=sum(woba_cat=="double"),
              triples=sum(woba_cat=="triple"),
              home_runs=sum(woba_cat=="home_run"),
              n=sum(woba_d), .groups='drop') %>% ungroup %>%
    
    group_by(batter) %>%
    
    mutate(cbb=cumsum(walks)-walks,
           chbp=cumsum(hbp)-hbp,
           c1b=cumsum(singles)-singles,
           c2b=cumsum(doubles)-doubles,
           c3b=cumsum(triples)-triples,
           chr=cumsum(home_runs)-home_runs,
           N=cumsum(n)-n) %>%
    
    mutate(b_cwoba=ifelse(N==0,league_woba,(cbb*weights[1]+
                                              chbp*weights[2]+
                                              c1b*weights[3]+
                                              c2b*weights[4]+
                                              c3b*weights[5]+
                                              chr*weights[6])/N))
  
  return(out)
  
}

# calculates pitchers' running wOBA from get_woba_stats, calls get_woba_weights
get_pitcher_cwoba <- function(woba_stats) {
  
  W <- get_woba_weights(woba_stats)
  weights <- W[["weights"]]$value
  league_woba <- W[["league_woba"]]*W[["scale"]]
  
  out <- woba_stats %>%
    
    arrange(game_date) %>%
    group_by(game_pk, pitcher) %>%
    
    summarize(walks=sum(woba_cat=="walk"),
              hbp=sum(woba_cat=="hit_by_pitch"),
              singles=sum(woba_cat=="single"),
              doubles=sum(woba_cat=="double"),
              triples=sum(woba_cat=="triple"),
              home_runs=sum(woba_cat=="home_run"),
              n=sum(woba_d), .groups='drop') %>% ungroup %>%
    
    group_by(pitcher) %>%
    
    mutate(cbb=cumsum(walks)-walks,
           chbp=cumsum(hbp)-hbp,
           c1b=cumsum(singles)-singles,
           c2b=cumsum(doubles)-doubles,
           c3b=cumsum(triples)-triples,
           chr=cumsum(home_runs)-home_runs,
           N=cumsum(n)-n) %>%
    
    mutate(p_cwoba=ifelse(N==0,league_woba,(cbb*weights[1]+
                                              chbp*weights[2]+
                                              c1b*weights[3]+
                                              c2b*weights[4]+
                                              c3b*weights[5]+
                                              chr*weights[6])/N))
  
  return(out)
  
}

# wapper of previous functions that outputs a clean df with b_cwoba and p_cwoba
get_woba_data <- function(data) {
  
  S <- get_woba_stats(data)
  B <- get_batter_cwoba(S)[,c("game_pk","batter","b_cwoba")]
  P <- get_pitcher_cwoba(S)[,c("game_pk","pitcher","p_cwoba")]
  
  out <- data %>%
    left_join(B,by=c("game_pk"="game_pk","batter"="batter")) %>%
    arrange(batter, game_date) %>% fill(b_cwoba) %>%
    left_join(P,by=c("game_pk"="game_pk","pitcher"="pitcher")) %>%
    arrange(pitcher, game_date) %>% fill(p_cwoba)
  
  return(out)
  
}

# combines data cleaning and wOBA calculation functions
get_data <- function(data) {
  
  C <- get_clean_data(data)
  W <- get_woba_data(C)
  out <- W %>% select(-c(events,game_year,at_bat_number,pitch_number,
                         woba_value,delta_run_exp))
  
  return(out)
  
}

# Load Data ---------------------------------------------------------------

load(paste0(data_dir, "statcast_query_2015.RData"))
load(paste0(data_dir, "statcast_query_2016.RData"))
load(paste0(data_dir, "statcast_query_2017.RData"))
load(paste0(data_dir, "statcast_query_2018.RData"))
load(paste0(data_dir, "statcast_query_2019.RData"))

# Process Data ------------------------------------------------------------

data_2015 <- get_data(statcast_query_2015)
data_2016 <- get_data(statcast_query_2016)
data_2017 <- get_data(statcast_query_2017)
data_2018 <- get_data(statcast_query_2018)
data_2019 <- get_data(statcast_query_2019)

# Event Probability Data --------------------------------------------------

# get list of umpires
ump_data <- load_umpire_ids()

# get list of home plate umpires
hp_ump_data <- ump_data %>% 
  rename(umpire=id) %>% 
  filter(position=="HP") %>% 
  mutate(game_pk=as.numeric(game_pk),
         game_date=as.character(game_date)) %>%
  select(umpire, game_pk, game_date)

# add umpires to data
pitch_data <- data_2019 %>% 
  left_join(hp_ump_data, by=c('game_pk', 'game_date')) %>%
  filter(!is.na(umpire))

# get lists of unique batters, pitchers, catchers, and umpires
unik_batters <- unique(pitch_data$batter)
unik_pitchers <- unique(pitch_data$pitcher)
unik_catchers <- unique(pitch_data$catcher)
unik_umpires <- unique(pitch_data$umpire)

# save lists of unique batters, pitchers, catchers, and umpires
save(unik_batters, file=paste0(data_dir, "unik_batters.RData"))
save(unik_pitchers, file=paste0(data_dir, "unik_pitchers.RData"))
save(unik_catchers, file=paste0(data_dir, "unik_catchers.RData"))
save(unik_umpires, file=paste0(data_dir, "unik_umpires.RData"))

# index personnel starting at 0 (friendlier for flexBART)
pitch_data <- pitch_data %>%
  mutate(batter=as.numeric(factor(batter,levels=unik_batters))-1,
         pitcher=as.numeric(factor(pitcher,levels=unik_pitchers))-1,
         catcher=as.numeric(factor(catcher,levels=unik_catchers))-1,
         umpire=as.numeric(factor(umpire,levels=unik_umpires))-1)

# save event probability data
save(pitch_data, file=paste0(data_dir, "pitch_data.RData"))

# Expected Runs Data ------------------------------------------------------

xR_data <- bind_rows(data_2015, data_2016, data_2017, data_2018) %>%
  mutate(
    # Outcomes:
    # miss = 0
    # contact = 1
    # called strike = 2
    # called ball = 3
    outcome = contact + called_strike * 2 + called_ball * 3,
    # Game-States:
    # miss or called strike = 0
    # called ball = 1
    # contact = 2
    gamestate_cat=ifelse(outcome%%2==0,0,swing+called_ball+contact)
    )

save(xR_data, file = paste0(data_dir, "xR_data.RData"))
