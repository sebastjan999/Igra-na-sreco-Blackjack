setwd("C:/Users/sebas/OneDrive/Namizje/MZR/Igra-na-sreco-Blackjack")

source("R/01_cards.R")
source("R/02_shoe.R")
source("R/03_strategy.R")
source("R/04_play_hand.R")
source("R/05_simulation.R")

#ZA poganjanje in izbiro parametrov:

#i) Demo verzija
set.seed(100)
simulate_n(N=100, n_decks = 6, hit_soft_17 = FALSE, bet = 1, payout_bj = 1.5)

#ii) Simulacija iz kupcka z reshuffle brez stetja kart
set.seed(100)
simulate_with_shoe(N = 1000, n_decks = 6,
                        penetration = 0.75,
                        hit_soft_17 = TRUE,
                        bet = 1,
                        payout_bj = 1.5,
                        can_double = FALSE,
                        can_split = FALSE,
                        can_surrender = FALSE)

#iii) Simulacija iz kupcka z reshuffle z stetjem kart
set.seed(100)
simulate_with_shoe_hilo(N = 1000, n_decks = 6,
                        penetration = 0.75,
                        hit_soft_17 = TRUE,
                        bet = 1,
                        payout_bj = 1.5,
                        can_double = FALSE,
                        can_split = FALSE,
                        can_surrender = FALSE)


#==============================================================================================
####DO KLE JE ZA TESTERJE; NAPREJ JE BOLJ ZAME KER PROJEKT SE NI FULLY FUNCIONAL IN KONCAN ^^
#==============================================================================================







set.seed(2025)
res <- simulate_with_shoe(N = 1000, n_decks = 6,
                          penetration = 0.75,
                          hit_soft_17 = FALSE,
                          bet = 1,
                          payout_bj = 1.5)
res$EV
res$CI95

set.seed(2025)
res_no_split <- simulate_with_shoe(N = 1000, n_decks = 6,
                          penetration = 0.75,
                          hit_soft_17 = FALSE,
                          bet = 1,
                          payout_bj = 1.5,
                          can_split = FALSE)
res_no_split$EV
res_no_split$CI95


res1 <- simulate_with_shoe_hilo(N = 1000, n_decks = 6,
                               penetration = 0.75,
                               hit_soft_17 = FALSE,
                               bet = 1,
                               payout_bj = 1.5)
res1$EV
res1$CI95
res1
set.seed(2025)
res2_all <- simulate_with_shoe_hilo(N = 1000, n_decks = 6,
                          penetration = 0.75,
                          hit_soft_17 = TRUE,
                          bet = 1,
                          payout_bj = 1.5,
                          can_split = TRUE)
res2_all$EV
res2_all$CI95

set.seed(2025)
res2_no_split <- simulate_with_shoe_hilo(N = 1000, n_decks = 6,
                                penetration = 0.75,
                                hit_soft_17 = TRUE,
                                bet = 1,
                                payout_bj = 1.5,
                                can_split = FALSE)
res2_no_split$EV
res2_no_split$CI95

set.seed(2025)
res2_no_split_no_surr <- simulate_with_shoe_hilo(N = 1000, n_decks = 6,
                                penetration = 0.75,
                                hit_soft_17 = TRUE,
                                bet = 1,
                                payout_bj = 1.5,
                                can_split = FALSE,
                                can_surrender = FALSE)
res2_no_split_no_surr$EV
res2_no_split_no_surr$CI95

set.seed(2025)
res2_no_split_no_surr_no_doub <- simulate_with_shoe_hilo(N = 1000, n_decks = 6,
                                                         penetration = 0.75,
                                                         hit_soft_17 = TRUE,
                                                         bet = 1,
                                                         payout_bj = 1.5,
                                                         can_double = FALSE,
                                                         can_split = FALSE,
                                                         can_surrender = FALSE)

res2_no_split_no_surr_no_doub$EV
res2_no_split_no_surr_no_doub$CI95
