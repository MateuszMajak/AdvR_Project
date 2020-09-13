library(dplyr)
library(tidyr)

setwd("C:/Users/mateu/Documents/GIT/AdvR_Project")
files <- list.files(pattern="*.csv")
files_list <- lapply(files, read.csv, sep=",", dec=".", header=TRUE, 
                     stringsAsFactors=TRUE)
atp <- do.call(rbind, files_list)

atp <- 
  atp %>%   
  dplyr::select(surface, tourney_level, winner_hand, winner_ht, winner_age, winner_rank, winner_rank_points, loser_hand, loser_ht, loser_age, loser_rank,
         loser_rank_points, minutes, w_ace, w_df, w_svpt, w_1stIn, w_1stWon, w_2ndWon, w_SvGms, w_bpSaved, w_bpFaced, l_ace, l_df, l_svpt,
         l_1stIn, l_1stWon, l_2ndWon, l_SvGms, l_bpSaved, l_bpFaced) %>%
  drop_na() %>%
  dplyr::filter(minutes >= 18, #shortest atp tennis match lasted 18 minutes so any time shorter than that is not possible and must be a mistake
         minutes <= 665, #longest documented tennis match lasted 11:05h, so this is an upper bound
         loser_hand != "U") %>% #because of only 1 obs left in this category
  dplyr::filter_if(is.factor, all_vars(.!="")) %>% #this is the way NA's were coded for factors
  dplyr::mutate(., surface = droplevels(surface), winner_hand = droplevels(winner_hand), loser_hand = droplevels(loser_hand))

summary(atp)






