setwd("C:/Users/sebas/OneDrive/Namizje/MZR/Igra-na-sreco-Blackjack")

source("R/01_cards.R")
source("R/02_shoe.R")
source("R/03_strategy.R")
source("R/04_play_hand.R")
source("R/05_simulation.R")
source("R/06_experiments.R")


# -----------------------------------------------------------------------------
#  1.) Primerjava 3eh strategij z istimi pravili
# -----------------------------------------------------------------------------
N = 10000
set.seed(1)

# 1) Demo/naključna simulacija (simulate_n + simulate_hand)
res_demo <- simulate_n(
  N          = N,
  n_decks    = 6,
  hit_soft_17= FALSE,
  bet        = 1,
  payout_bj  = 1.5
)

set.seed(1)
# 2) Basic strategija + shoe + reshuffle (brez Hi-Lo, flat bet)
res_basic <- simulate_with_shoe(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,   # S17
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

set.seed(1)
# 3) Napredna strategija: Hi-Lo + bet spread
res_hilo <- simulate_with_shoe_hilo(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

# Hiter povzetek (EV in SE)
data.frame(
  strategija = c("Demo / random", "Basic (shoe)", "Hi-Lo (shoe + spread)"),
  EV         = c(res_demo$EV,     res_basic$EV,   res_hilo$EV),
  SE         = c(res_demo$SE,     res_basic$SE,   res_hilo$SE)
)

res_basic$win_rate
res_basic$loss_rate
res_basic$push_rate

res_hilo$ROI
res_hilo$HE   # house edge na enoto stave pri Hi-Lo

# -----------------------------------------------------------------------------
#  2.) Primerjava S17 vs H17 za basic strategijo in Hi-Lo strategijo
# -----------------------------------------------------------------------------

set.seed(2)

# S17 – dealer stoji na soft 17
res_basic_S17 <- simulate_with_shoe(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,   # S17
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

set.seed(2)
# H17 – dealer vleče tudi na soft 17
res_basic_H17 <- simulate_with_shoe(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = TRUE,    # H17
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

data.frame(
  pravilo = c("S17", "H17"),
  EV      = c(res_basic_S17$EV, res_basic_H17$EV),
  SE      = c(res_basic_S17$SE, res_basic_H17$SE),
  HE      = c(res_basic_S17$HE, res_basic_H17$HE)
)

set.seed(3)

res_hilo_S17 <- simulate_with_shoe_hilo(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5
)
set.seed(3)
res_hilo_H17 <- simulate_with_shoe_hilo(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = TRUE,
  bet          = 1,
  payout_bj    = 1.5
)

data.frame(
  pravilo = c("S17", "H17"),
  EV      = c(res_hilo_S17$EV, res_hilo_H17$EV),
  HE      = c(res_hilo_S17$HE, res_hilo_H17$HE),
  ROI     = c(res_hilo_S17$ROI, res_hilo_H17$ROI)
)

# -----------------------------------------------------------------------------
#  3.) Vpliv igralčevih pravil: douvle/split/surrenderON/OFF
# -----------------------------------------------------------------------------

#a) DOUBLE ON/OFF
set.seed(4)

res_basic_split_on <- simulate_with_shoe(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

set.seed(4)
res_basic_split_off <- simulate_with_shoe(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = FALSE,
  can_split    = FALSE,
  can_surrender= TRUE
)

data.frame(
  split = c("ON", "OFF"),
  EV    = c(res_basic_split_on$EV, res_basic_split_off$EV),
  SE    = c(res_basic_split_on$SE, res_basic_split_off$SE)
)

#b) CAN SURRENDER ON/OFF
set.seed(5)

res_basic_R_on <- simulate_with_shoe(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

set.seed(5)
res_basic_R_off <- simulate_with_shoe(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= FALSE
)

data.frame(
  surrender = c("ON", "OFF"),
  EV        = c(res_basic_R_on$EV, res_basic_R_off$EV),
  SE        = c(res_basic_R_on$SE, res_basic_R_off$SE),
  surrender_rate = c(res_basic_R_on$surrender_rate,
                     res_basic_R_off$surrender_rate)
)

set.seed(6)

#c) CAN SPLIT ON/OFF
res_basic_split_on <- simulate_with_shoe(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = TRUE,
  can_surrender= TRUE
)
set.seed(6)
res_basic_split_off <- simulate_with_shoe(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

data.frame(
  split = c("ON", "OFF"),
  EV    = c(res_basic_split_on$EV, res_basic_split_off$EV),
  SE    = c(res_basic_split_on$SE, res_basic_split_off$SE)
)

# -----------------------------------------------------------------------------
#  4.) Vpliv izplačila Blackjack 3:2 vs 6:5
# -----------------------------------------------------------------------------

# 3:2 payout (1.5)
set.seed(7)
res_basic_3to2 <- simulate_with_shoe(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

# 6:5 payout (1.2)
set.seed(7)
res_basic_6to5 <- simulate_with_shoe(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.2,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

data.frame(
  payout_bj = c("3:2", "6:5"),
  EV        = c(res_basic_3to2$EV, res_basic_6to5$EV),
  SE        = c(res_basic_3to2$SE, res_basic_6to5$SE),
  bj_rate   = c(res_basic_3to2$bj_rate_player, res_basic_6to5$bj_rate_player)
)

# -----------------------------------------------------------------------------
#  5.) Hi-Lo: can_double ON/OFF
# -----------------------------------------------------------------------------

set.seed(8)
res_hilo_double_on <- simulate_with_shoe_hilo(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

set.seed(8)
res_hilo_double_off <- simulate_with_shoe_hilo(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = FALSE,
  can_split    = FALSE,
  can_surrender= TRUE
)

data.frame(
  can_double = c("ON", "OFF"),
  EV              = c(res_hilo_double_on$EV, res_hilo_double_off$EV),
  HE              = c(res_hilo_double_on$HE, res_hilo_double_off$HE),
  HE_per_hand     = c(res_hilo_double_on$HE_per_hand, res_hilo_double_off$HE_per_hand),
  ROI             = c(res_hilo_double_on$ROI, res_hilo_double_off$ROI)
)

# -----------------------------------------------------------------------------
#  6.) Hi-Lo: can_surrender ON/OFF
# -----------------------------------------------------------------------------


set.seed(9)
res_hilo_R_on <- simulate_with_shoe_hilo(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

set.seed(9)
res_hilo_R_off <- simulate_with_shoe_hilo(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= FALSE
)

data.frame(
  surrender    = c("ON", "OFF"),
  EV           = c(res_hilo_R_on$EV, res_hilo_R_off$EV),
  HE           = c(res_hilo_R_on$HE, res_hilo_R_off$HE),
  HE_per_hand  = c(res_hilo_R_on$HE_per_hand, res_hilo_R_off$HE_per_hand),
  ROI          = c(res_hilo_R_on$ROI, res_hilo_R_off$ROI),
  surrender_rate = c(res_hilo_R_on$surrender_rate,
                     res_hilo_R_off$surrender_rate)
)

# -----------------------------------------------------------------------------
#  7.) Hi-Lo: vpliv penetracije shoe-a
# -----------------------------------------------------------------------------
penetracije <- c(0.5, 0.75, 0.9)
set.seed(10)

res_hilo_pen <- lapply(penetracije, function(pen) {
  simulate_with_shoe_hilo(
    N            = N,
    n_decks      = 6,
    penetration  = pen,
    hit_soft_17  = FALSE,  # S17
    bet          = 1,
    payout_bj    = 1.5,
    can_double   = TRUE,
    can_split    = FALSE,
    can_surrender= TRUE
  )
})

df_hilo_pen <- data.frame(
  penetration = penetracije,
  EV          = sapply(res_hilo_pen, `[[`, "EV"),
  HE          = sapply(res_hilo_pen, `[[`, "HE"),   # house edge per bet
  HE_per_hand = sapply(res_hilo_pen, `[[`, "HE_per_hand"),   # house edge per bet
  ROI         = sapply(res_hilo_pen, `[[`, "ROI")   # player ROI
)

df_hilo_pen

# -----------------------------------------------------------------------------
#  8.) Hi-Lo: EV po True Count (za graf)
# -----------------------------------------------------------------------------

set.seed(11)
res_hilo_big <- simulate_with_shoe_hilo(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

tc_round <- round(res_hilo_big$true_count)
EV_by_TC <- tapply(res_hilo_big$gains, tc_round, mean)
N_by_TC  <- table(tc_round)

ev_tc_df <- data.frame(
  TC = as.numeric(names(EV_by_TC)),
  EV = as.numeric(EV_by_TC),
  N  = as.numeric(N_by_TC[names(EV_by_TC)])
)

ev_tc_df



















