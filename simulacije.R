setwd("C:/Users/sebas/OneDrive/Namizje/MZR/Igra-na-sreco-Blackjack")

source("R/01_cards.R")
source("R/02_shoe.R")
source("R/03_strategy.R")
source("R/04_play_hand.R")
source("R/05_simulation.R")
source("R/06_experiments.R")

#primerjava razlicnih pravil igralnice (H17 vs S17 in diff penetration)
scenario_grid_A <- expand.grid(
  hit_soft_17   = c(FALSE, TRUE),
  penetration   = c(0.5, 0.75, 0.9),
  can_double    = TRUE,
  can_split     = TRUE,
  can_surrender = TRUE,
  stringsAsFactors = FALSE
)

#Primerjava dovoljenih igralcevih potez (double, split, surrender)
scenario_grid_B <- expand.grid(
  hit_soft_17   = FALSE,       
  penetration   = 0.75,        
  can_double    = c(TRUE, FALSE),
  can_split     = c(TRUE, FALSE),
  can_surrender = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

#==============================================================================================
#### SCENARIJ A
#==============================================================================================
