setwd("C:/Users/sebas/OneDrive/Namizje/MZR/Igra-na-sreco-Blackjack")

source("R/01_cards.R")
source("R/02_shoe.R")
source("R/03_strategy.R")
source("R/04_play_hand.R")
source("R/05_simulation.R")

BS_TABLE_S17 <- read.csv("basic_strategy.csv", stringsAsFactors = FALSE)
BS_TABLE_H17 <- read.csv("basic_strategy_H17.csv", stringsAsFactors = FALSE)

basic_action_bs(c(5,6), dealer_up = 11, can_double=TRUE, can_split=FALSE, can_surrender=TRUE,   bs_table  = BS_TABLE_H17)

a  <- basic_action_bs(c(11,6), dealer_up = 6)  # A,6 vs 6
b  <- basic_action_bs(c(11,6), dealer_up = 2)  # A,6 vs 2
a2 <- basic_action_bs(c(11,7), dealer_up = 6)  # A,7 vs 6
b2 <- basic_action_bs(c(11,7), dealer_up = 2)  # A,7 vs 2

c  <- basic_action_bs(c(8,8),  dealer_up = 11, can_split = TRUE)
c2 <- basic_action_bs(
  c(8,8),
  dealer_up = 11,
  can_split = TRUE,
  bs_table  = BS_TABLE_H17
)

c;c2

d  <- basic_action_bs(c(10,10), dealer_up = 6,  can_split = TRUE)

e <- basic_action_bs(c(9,7), dealer_up = 10,  can_split = TRUE)
a; b; a2; b2; c; d; e

f <-  basic_action_bs(c(11,7), dealer_up = 3,  can_split = TRUE)
f

