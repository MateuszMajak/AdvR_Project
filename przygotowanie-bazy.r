d2000 <- read.csv("atp_matches_2000.csv", sep=",", dec=".", header=TRUE) 
d2001 <- read.csv("atp_matches_2001.csv", sep=",", dec=".", header=TRUE) 
d2002 <- read.csv("atp_matches_2002.csv", sep=",", dec=".", header=TRUE) 
d2003 <- read.csv("atp_matches_2003.csv", sep=",", dec=".", header=TRUE) 
d2004 <- read.csv("atp_matches_2004.csv", sep=",", dec=".", header=TRUE) 
d2005 <- read.csv("atp_matches_2005.csv", sep=",", dec=".", header=TRUE) 
d2006 <- read.csv("atp_matches_2006.csv", sep=",", dec=".", header=TRUE) 
d2007 <- read.csv("atp_matches_2007.csv", sep=",", dec=".", header=TRUE) 
d2008 <- read.csv("atp_matches_2008.csv", sep=",", dec=".", header=TRUE) 
d2009 <- read.csv("atp_matches_2009.csv", sep=",", dec=".", header=TRUE) 
d2010 <- read.csv("atp_matches_2010.csv", sep=",", dec=".", header=TRUE) 
d2011 <- read.csv("atp_matches_2011.csv", sep=",", dec=".", header=TRUE) 
d2012 <- read.csv("atp_matches_2012.csv", sep=",", dec=".", header=TRUE) 
d2013 <- read.csv("atp_matches_2013.csv", sep=",", dec=".", header=TRUE) 
d2014 <- read.csv("atp_matches_2014.csv", sep=",", dec=".", header=TRUE) 
d2015 <- read.csv("atp_matches_2015.csv", sep=",", dec=".", header=TRUE) 
d2016 <- read.csv("atp_matches_2016.csv", sep=",", dec=".", header=TRUE) 
d2017 <- read.csv("atp_matches_2017.csv", sep=",", dec=".", header=TRUE) 

library(dplyr)
library(tidyr)


atp <- bind_rows(d2000, d2001, d2002, d2003, d2004, d2005, d2006, d2007, d2008, d2009, d2010, d2011, d2012, d2013, d2014, d2015, d2016, d2017) %>%
  select(surface, tourney_level, winner_hand, winner_ht, winner_age, winner_rank, winner_rank_points, loser_hand, loser_ht, loser_age, loser_rank,
         loser_rank_points, minutes, w_ace, w_df, w_svpt, w_1stIn, w_1stWon, w_2ndWon, w_SvGms, w_bpSaved, w_bpFaced, l_ace, l_df, l_svpt,
         l_1stIn, l_1stWon, l_2ndWon, l_SvGms, l_bpSaved, l_bpFaced) %>%
  drop_na() %>%
  filter(minutes >= 18, #shortest atp tennis match lasted 18 minutes so any time shorter than that is not possible and must be a mistake
         minutes <= 665, #longest documented tennis match lasted 11:05h, so this is an upper bound
         loser_hand != "U") %>% #because of only 1 obs left in this category
  filter_if(is.factor, all_vars(.!="")) %>% #this is the way NA's were coded for factors
  mutate(., surface = droplevels(surface), winner_hand = droplevels(winner_hand), loser_hand = droplevels(loser_hand))
  

summary(atp)




















