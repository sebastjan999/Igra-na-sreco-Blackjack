setwd("C:/Users/sebas/OneDrive/Namizje/MZR/Igra-na-sreco-Blackjack")

source("R/01_cards.R")
source("R/02_shoe.R")
source("R/03_strategy.R")
source("R/04_play_hand.R")
source("R/05_simulation.R")

#ZA poganjanje in izbiro parametrov:

#i) Demo verzija
set.seed(100)
demo = simulate_n(N=100, n_decks = 6, hit_soft_17 = FALSE, bet = 1, payout_bj = 1.5)

#ii) Simulacija iz kupcka z reshuffle brez stetja kart
set.seed(100)
basic = simulate_with_shoe(N = 1000, n_decks = 6,
                        penetration = 0.75,
                        hit_soft_17 = TRUE,
                        bet = 1,
                        payout_bj = 1.5,
                        can_double = FALSE,
                        can_split = FALSE,
                        can_surrender = FALSE)

#iii) Simulacija iz kupcka z reshuffle z stetjem kart
set.seed(100)
napredna = simulate_with_shoe_hilo(N = 1000, n_decks = 6,
                        penetration = 0.75,
                        hit_soft_17 = TRUE,
                        bet = 1,
                        payout_bj = 1.5,
                        can_double = FALSE,
                        can_split = FALSE,
                        can_surrender = FALSE)


