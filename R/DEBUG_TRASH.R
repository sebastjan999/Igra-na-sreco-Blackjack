setwd("C:/Users/sebas/OneDrive/Namizje/MZR/Igra-na-sreco-Blackjack")

source("R/01_cards.R")
source("R/02_shoe.R")
source("R/03_strategy.R")
source("R/04_play_hand.R")
source("R/05_simulation.R")

BS_TABLE <- read.csv("basic_strategy_full.csv", stringsAsFactors = FALSE)

a  <- basic_action_bs(c(11,6), dealer_up = 6)  # A,6 vs 6
b  <- basic_action_bs(c(11,6), dealer_up = 2)  # A,6 vs 2
a2 <- basic_action_bs(c(11,7), dealer_up = 6)  # A,7 vs 6
b2 <- basic_action_bs(c(11,7), dealer_up = 2)  # A,7 vs 2

c  <- basic_action_bs(c(8,8),  dealer_up = 10, can_split = TRUE)
d  <- basic_action_bs(c(10,10), dealer_up = 6,  can_split = TRUE)

a; b; a2; b2; c; d


