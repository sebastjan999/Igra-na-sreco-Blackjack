setwd("C:/Users/sebas/OneDrive/Namizje/MZR/Igra-na-sreco-Blackjack")

source("R/01_cards.R")
source("R/02_shoe.R")
source("R/03_strategy.R")
source("R/04_play_hand.R")
source("R/05_simulation.R")


set.seed(2025)
res <- simulate_with_shoe(N = 1000, n_decks = 6,
                          penetration = 0.75,
                          hit_soft_17 = FALSE,
                          bet = 1,
                          payout_bj = 1.5)
res$EV
res$CI95