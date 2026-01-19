setwd("C:/Users/sebas/OneDrive/Namizje/MZR/Igra-na-sreco-Blackjack")

source("R/01_cards.R")
source("R/02_shoe.R")
source("R/03_strategy.R")
source("R/04_play_hand.R")
source("R/05_simulation.R")

#=======================HELPER FUNK ZA SKUPNI DF PO SIMULACIJAH============================================
make_row <- function(
    name,                # ime scenarija (npr. "demo_basic", "hilo_S17")
    strategy,            # "demo", "basic", "hilo"
    res,                 # seznam ki ga vrne simulacija
    N,
    n_decks,
    penetration = NA_real_,
    hit_soft_17 = NA,
    payout_bj   = NA_real_,
    can_double  = NA,
    can_split   = NA,
    can_surrender = NA
) {
  # pravilo delivca kot string
  rule <- if (is.na(hit_soft_17)) NA_character_
  else if (isTRUE(hit_soft_17)) "H17" else "S17"
  
  data.frame(
    scenario      = name,         # ime scenarija
    strategy      = strategy,     # demo / basic / hilo
    N             = N,
    n_decks       = n_decks,
    penetration   = penetration,
    rule          = rule,
    payout_bj     = payout_bj,
    can_double    = can_double,
    can_split     = can_split,
    can_surrender = can_surrender,
    
    # osnovne metrike (če jih ni v res, damo NA)
    EV        = if (!is.null(res$EV)) res$EV else NA_real_,
    SE        = if (!is.null(res$SE)) res$SE else NA_real_,
    HE_per_hand        = if (!is.null(res$HE)) res$HE else 
      if (!is.null(res$HE_per_hand)) res$HE_per_hand else NA_real_,
    HE_per_bet        = if (!is.null(res$HE_per_bet)) res$HE_per_bet else -res$EV,
    ROI       = if (!is.null(res$ROI)) res$ROI else res$EV,
    
    win_rate   = if (!is.null(res$win_rate)) res$win_rate else NA_real_,
    loss_rate  = if (!is.null(res$loss_rate)) res$loss_rate else NA_real_,
    push_rate  = if (!is.null(res$push_rate)) res$push_rate else NA_real_,
    
    bj_rate_player   = if (!is.null(res$bj_rate_player)) res$bj_rate_player else NA_real_,
    bj_rate_dealer   = if (!is.null(res$bj_rate_dealer)) res$bj_rate_dealer else NA_real_,
    surrender_rate   = if (!is.null(res$surrender_rate)) res$surrender_rate else NA_real_,
    player_bust_rate = if (!is.null(res$player_bust_rate)) res$player_bust_rate else NA_real_,
    dealer_bust_rate = if (!is.null(res$dealer_bust_rate)) res$dealer_bust_rate else NA_real_,
    double_rate      = if (!is.null(res$double_rate)) res$double_rate else NA_real_,
    
    avg_bet_per_hand = if (!is.null(res$avg_bet_per_hand)) res$avg_bet_per_hand else NA_real_,
    max_drawdown     = if (!is.null(res$max_drawdown)) res$max_drawdown else NA_real_
  )
}

#=========================================================================================================

#==================================ZAČETEK SIMULACIJ=====================================================0

# -----------------------------------------------------------------------------
#  1.) Primerjava 3eh strategij z istimi pravili
# -----------------------------------------------------------------------------
N = 1e6
set.seed(1090)

# 1) Demo/naključna simulacija (simulate_n + simulate_hand)
res_demo <- simulate_n(
  N          = N,
  n_decks    = 6,
  hit_soft_17= FALSE,
  bet        = 1,
  payout_bj  = 1.5
)

set.seed(1090)
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

set.seed(1090)
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
df1 = data.frame(
  strategija = c("Demo / random", "Basic (shoe)", "Hi-Lo (shoe + spread)"),
  
  EV_per_hand = c(res_demo$EV, res_basic$EV, res_hilo$EV),
  
  # house edge na roko (samo -EV)
  HE_per_hand = c(-res_demo$EV, res_basic$HE_per_hand, res_hilo$HE_per_hand),
  
  # primerljivo med strategijami: HE na enoto vložka
  HE_per_bet  = c(-res_demo$EV, res_basic$HE_per_bet, res_hilo$HE_per_bet),
  
  # še bolj intuitivno: ROI (enako kot -HE_per_bet)
  ROI         = c(res_demo$EV, res_basic$ROI, res_hilo$ROI),
  
  avg_bet     = c(1, res_basic$avg_bet_per_hand, res_hilo$avg_bet_per_hand)
)

write.csv(df1, "demo_basic_hilo.csv", row.names = FALSE)

#Hi-Lo strategija sama po sebi ne garantira pozitivnega EV, vendar zmanjša house edge na enoto stave, kar pomeni, da igralec dolgoročno izgublja počasneje oziroma pri ugodnih pogojih (večja penetracija, boljši spread, index plays) lahko doseže pozitiven EV.
res_basic$win_rate
res_basic$loss_rate
res_basic$push_rate


# -----------------------------------------------------------------------------
#  2.) Primerjava S17 vs H17 za basic strategijo in Hi-Lo strategijo
# -----------------------------------------------------------------------------

set.seed(1)

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

set.seed(1)
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

basic_S17_vs_H17 = data.frame(
  pravilo = c("S17", "H17"),
  EV      = c(res_basic_S17$EV, res_basic_H17$EV),
  SE      = c(res_basic_S17$SE, res_basic_H17$SE),
  HE_per_bet      = c(res_basic_S17$HE_per_bet, res_basic_H17$HE_per_bet),
  bet = c(res_basic_S17$avg_bet_per_hand, res_basic_H17$avg_bet_per_hand)
)

write.csv(basic_S17_vs_H17, "basic_S17_vs_H17.csv", row.names = FALSE)


set.seed(1)

res_hilo_S17 <- simulate_with_shoe_hilo(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5
)
set.seed(1)
res_hilo_H17 <- simulate_with_shoe_hilo(
  N            = N,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = TRUE,
  bet          = 1,
  payout_bj    = 1.5
)

hilo_S17_vs_H17 = data.frame(
  pravilo = c("S17", "H17"),
  EV      = c(res_hilo_S17$EV, res_hilo_H17$EV),
  HE_per_bet      = c(res_hilo_S17$HE_per_bet, res_hilo_H17$HE_per_bet),
  ROI     = c(res_hilo_S17$ROI, res_hilo_H17$ROI),
  bet = c (res_hilo_S17$avg_bet_per_hand,res_hilo_H17$avg_bet_per_hand)
)

write.csv(hilo_S17_vs_H17, "hilo_S17_vs_H17.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
#  3.) Vpliv igralčevih pravil: douvle/split/surrenderON/OFF
# -----------------------------------------------------------------------------
#DO tuki proper simulacije sam tok casa traja da mal pauzice XD
#a) DOUBLE ON/OFF  - IS a bit weird krkol sprobam se zdi da se double pravilo dejansko igralcu ne splaca, bom ceknu cde je kej v kosi al je pac zto k ni hilo taktika
set.seed(20)
res_basic_double_on <- simulate_with_shoe(
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

set.seed(20)
res_basic_double_off <- simulate_with_shoe(
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

basic_double = data.frame(
  double = c("ON", "OFF"),
  EV_per_hand = c(res_basic_double_on$EV,      res_basic_double_off$EV),
  HE_per_bet  = c(res_basic_double_on$HE_per_bet,
                  res_basic_double_off$HE_per_bet),
  avg_bet     = c(res_basic_double_on$avg_bet_per_hand,
                  res_basic_double_off$avg_bet_per_hand),
  double_rate = c(res_basic_double_on$double_rate,
                  res_basic_double_off$double_rate)
  
)

write.csv(basic_double, "basic_double.csv", row.names = FALSE)

idx <- res_basic_double_on$doubled
c(
  EV_per_bet_doubled = mean(res_basic_double_on$gains[idx] / 2),
  EV_per_bet_nodouble = mean(res_basic_double_on$gains[!idx])
)

idx_double <- res_basic_double_on$doubled

mean(res_basic_double_on$gains[idx_double])      # EV na double roke
mean(res_basic_double_on$gains[!idx_double])     # EV na roke brez double
mean(idx_double)                                 # frekvenca double-ov
mean(res_basic_double_off$gains)
#Double izboljša posamezne roke, vendar ne spremeni dejstva, da ima igralec dolgoročno negativno prednost. Zaradi večjega povprečnega vložka se skupna izguba celo nekoliko poveča.

#b) CAN SURRENDER ON/OFF
set.seed(20)

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

set.seed(20)
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

basic_surrender = data.frame(
  surrender = c("ON", "OFF"),
  EV        = c(res_basic_R_on$EV, res_basic_R_off$EV),
  HE_per_bet  = c(res_basic_R_on$HE_per_bet,
                  res_basic_R_off$HE_per_bet),
  surrender_rate = c(res_basic_R_on$surrender_rate,
                     res_basic_R_off$surrender_rate)
)
write.csv(basic_surrender, "basic_surrender.csv", row.names = FALSE)

#c) CAN SPLIT ON/OFF

set.seed(20)
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

set.seed(20)
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

basic_split = data.frame(
  split = c("ON", "OFF"),
  EV    = c(res_basic_split_on$EV, res_basic_split_off$EV),
  HE_per_bet  = c(res_basic_split_on$HE_per_bet,
                  res_basic_split_off$HE_per_bet)
)
write.csv(basic_split, "basic_split.csv", row.names = FALSE)
# -----------------------------------------------------------------------------
#  5.) Hi-Lo: can_double ON/OFF
# -----------------------------------------------------------------------------

set.seed(20)
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

set.seed(20)
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

hilo_double = data.frame(
  can_double = c("ON", "OFF"),
  EV              = c(res_hilo_double_on$EV, res_hilo_double_off$EV),
  HE_per_bet              = c(res_hilo_double_on$HE_per_bet, res_hilo_double_off$HE_per_bet),
  HE_per_hand     = c(res_hilo_double_on$HE_per_hand, res_hilo_double_off$HE_per_hand),
  ROI             = c(res_hilo_double_on$ROI, res_hilo_double_off$ROI),
  double_rate             = c(res_hilo_double_on$double_rate, res_hilo_double_off$double_rate)
)
write.csv(hilo_double, "hilo_double.csv", row.names = FALSE)
#Čeprav je double kot posamezna poteza pozitivna v okviru osnovne strategije, kombinacija Hi-Lo stavnega razpona in osnovne strategije brez index prilagoditev povzroči poslabšanje house edge na enoto vložka.
#Razlog je v tem, da se double pojavlja predvsem v situacijah z visokim vložkom, kjer osnovna strategija ne upošteva trenutnega true counta. Za optimalno uporabo štetja kart bi bilo potrebno implementirati t. i. index plays.
#torej rabil bi ločeno CSV tabelo za Hi-Lo strategijo, ampak to je ze too much.
# -----------------------------------------------------------------------------
#  6.) Hi-Lo: can_surrender ON/OFF
# -----------------------------------------------------------------------------

set.seed(20)
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

set.seed(20)
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

hilo_surrender = data.frame(
  surrender    = c("ON", "OFF"),
  EV           = c(res_hilo_R_on$EV, res_hilo_R_off$EV),
  HE           = c(res_hilo_R_on$HE_per_bet, res_hilo_R_off$HE_per_bet),
  HE_per_hand  = c(res_hilo_R_on$HE_per_hand, res_hilo_R_off$HE_per_hand),
  ROI          = c(res_hilo_R_on$ROI, res_hilo_R_off$ROI),
  surrender_rate = c(res_hilo_R_on$surrender_rate,
                     res_hilo_R_off$surrender_rate)
)
write.csv(hilo_surrender, "hilo_surrender.csv", row.names = FALSE)
set.seed(20)

#c) CAN SPLIT ON/OFF
res_hilo_split_on <- simulate_with_shoe_hilo(
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
set.seed(20)
res_hilo_split_off <- simulate_with_shoe_hilo(
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

hilo_split = data.frame(
  split = c("ON", "OFF"),
  EV    = c(res_hilo_split_on$EV, res_hilo_split_off$EV),
  HE_per_bet  = c(res_hilo_split_on$HE_per_bet,
                  res_hilo_split_off$HE_per_bet)
)
write.csv(hilo_split, "hilo_split.csv", row.names = FALSE)
# -----------------------------------------------------------------------------
#  DoD.) Hi-Lo: igra s parametri
# -----------------------------------------------------------------------------

set.seed(1)
res_hilo_all_on <- simulate_with_shoe_hilo(
  N            = N,
  n_decks      = 2,
  penetration  = 0.90,
  hit_soft_17  = FALSE,
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = FALSE,
  can_split    = FALSE,
  can_surrender= TRUE
)
res_hilo_all_on$EV

set.seed(1)
res_hilo_all_off <- simulate_with_shoe_hilo(
  N            = N,
  n_decks      = 6,
  penetration  = 0.5,
  hit_soft_17  = TRUE,
  bet          = 1,
  payout_bj    = 1.2,
  can_double   = FALSE,
  can_split    = FALSE,
  can_surrender= FALSE
)

hilo_extreme = data.frame(
  pravila    = c("best", "worst"),
  EV           = c(res_hilo_all_on$EV, res_hilo_all_off$EV),
  HE_per_bet           = c(res_hilo_all_on$HE_per_bet, res_hilo_all_off$HE_per_bet),
  HE_per_hand  = c(res_hilo_all_on$HE_per_hand, res_hilo_all_off$HE_per_hand),
  ROI          = c(res_hilo_all_on$ROI, res_hilo_all_off$ROI),
  avg_bet_per_hand          = c(res_hilo_all_on$avg_bet_per_hand, res_hilo_all_off$avg_bet_per_hand)
)
write.csv(hilo_extreme, "hilo_extreme.csv", row.names = FALSE)
# -----------------------------------------------------------------------------
#  4.) Vpliv izplačila Blackjack 3:2 vs 6:5
# -----------------------------------------------------------------------------

# 3:2 payout (1.5)
set.seed(1000)
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
set.seed(1000)
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

basic_bj =data.frame(
  payout_bj = c("3:2", "6:5"),
  EV        = c(res_basic_3to2$EV, res_basic_6to5$EV),
  HE_per_bet        = c(res_basic_3to2$HE_per_bet, res_basic_6to5$HE_per_bet),
  bj_rate   = c(res_basic_3to2$bj_rate_player, res_basic_6to5$bj_rate_player)
)
write.csv(basic_bj, "basic_bj.csv", row.names = FALSE)
#PA ŠE ZA Hi-Lo

# 3:2 payout (1.5)
set.seed(1000)
res_hilo_3to2 <- simulate_with_shoe_hilo(
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
set.seed(1000)
res_hilo_6to5 <- simulate_with_shoe_hilo(
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

hilo_bj = data.frame(
  payout_bj = c("3:2", "6:5"),
  EV        = c(res_hilo_3to2$EV, res_hilo_6to5$EV),
  HE_per_bet        = c(res_hilo_3to2$HE_per_bet, res_hilo_6to5$HE_per_bet),
  bj_rate   = c(res_hilo_3to2$bj_rate_player, res_hilo_6to5$bj_rate_player),
  bet = c(res_hilo_3to2$avg_bet_per_hand, res_hilo_6to5$avg_bet_per_hand)
)
write.csv(hilo_bj, "hilo_bj.csv", row.names = FALSE)
# -----------------------------------------------------------------------------
#  7.) Hi-Lo: vpliv penetracije shoe-a
# -----------------------------------------------------------------------------
penetracije <- c(0.5, 0.75, 0.9)
set.seed(777)

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
  HE_per_bet  = sapply(res_hilo_pen, `[[`, "HE_per_bet"),   # house edge per bet
  HE_per_hand = sapply(res_hilo_pen, `[[`, "HE_per_hand"),   # house edge per hand
  ROI         = sapply(res_hilo_pen, `[[`, "ROI"),   # player ROI
  bet         = sapply(res_hilo_pen,`[[`, "avg_bet_per_hand")
  )

df_hilo_pen
write.csv(df_hilo_pen, "df_hilo_pen.csv", row.names = FALSE)
# -----------------------------------------------------------------------------
#  8.) Hi-Lo: EV po True Count (za graf)
# -----------------------------------------------------------------------------

set.seed(123)
res_hilo_big <- simulate_with_shoe_hilo(
  N            = 1e6,
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

#=======================================================================================================

# Zbiramo vse scenarije v en seznam
rows <- list()

k <- 1

# 1.) Primerjava 3 strategij
rows[[k]] <- make_row(
  name          = "demo_random",
  strategy      = "demo",
  res           = res_demo,
  N             = N,
  n_decks       = 6,
  penetration   = NA,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "basic_S17",
  strategy      = "basic",
  res           = res_basic,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "hilo_S17",
  strategy      = "hilo",
  res           = res_hilo,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1


# 2.) S17 vs H17 – basic
rows[[k]] <- make_row(
  name          = "basic_S17_rule",
  strategy      = "basic",
  res           = res_basic_S17,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "basic_H17_rule",
  strategy      = "basic",
  res           = res_basic_H17,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = TRUE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

# 2.) S17 vs H17 – Hi-Lo
rows[[k]] <- make_row(
  name          = "hilo_S17_rule",
  strategy      = "hilo",
  res           = res_hilo_S17,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "hilo_H17_rule",
  strategy      = "hilo",
  res           = res_hilo_H17,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = TRUE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1


# 3.) Vpliv igralčevih pravil – basic

# double ON/OFF
rows[[k]] <- make_row(
  name          = "basic_double_ON",
  strategy      = "basic",
  res           = res_basic_double_on,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "basic_double_OFF",
  strategy      = "basic",
  res           = res_basic_double_off,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = FALSE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1


# surrender ON/OFF
rows[[k]] <- make_row(
  name          = "basic_surrender_ON",
  strategy      = "basic",
  res           = res_basic_R_on,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "basic_surrender_OFF",
  strategy      = "basic",
  res           = res_basic_R_off,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = FALSE
); k <- k + 1


# split ON/OFF
rows[[k]] <- make_row(
  name          = "basic_split_ON",
  strategy      = "basic",
  res           = res_basic_split_on,  
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = TRUE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "basic_split_OFF",
  strategy      = "basic",
  res           = res_basic_split_off, 
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1


# 4.) Blackjack payout 3:2 vs 6:5 (basic)
rows[[k]] <- make_row(
  name          = "basic_BJ_3to2",
  strategy      = "basic",
  res           = res_basic_3to2,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "basic_BJ_6to5",
  strategy      = "basic",
  res           = res_basic_6to5,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.2,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

# 4.) Blackjack payout 3:2 vs 6:5 (basic)
rows[[k]] <- make_row(
  name          = "hilo_BJ_3to2",
  strategy      = "hilo",
  res           = res_hilo_3to2,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "hilo_BJ_6to5",
  strategy      = "hilo",
  res           = res_hilo_6to5,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.2,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

for (j in seq_along(penetracije)) {
  pen <- penetracije[j]
  res_pen <- res_hilo_pen[[j]]
  
  rows[[k]] <- make_row(
    name          = paste0("hilo_pen_", pen),
    strategy      = "hilo",
    res           = res_pen,
    N             = N,
    n_decks       = 6,
    penetration   = pen,
    hit_soft_17   = FALSE,
    payout_bj     = 1.5,
    can_double    = TRUE,
    can_split     = FALSE,
    can_surrender = TRUE
  )
  k <- k + 1
}

rows[[k]] <- make_row(
  name          = "hilo_double_ON",
  strategy      = "hilo",
  res           = res_hilo_double_on,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "hilo_double_OFF",
  strategy      = "hilo",
  res           = res_hilo_double_off,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = FALSE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "hilo_surrender_ON",
  strategy      = "hilo",
  res           = res_hilo_R_on,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "hilo_surrender_OFF",
  strategy      = "hilo",
  res           = res_hilo_R_off,
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = FALSE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "hilo_split_ON",
  strategy      = "hilo",
  res           = res_hilo_split_on,  
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = TRUE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "hilo_split_OFF",
  strategy      = "hilo",
  res           = res_hilo_split_off, 
  N             = N,
  n_decks       = 6,
  penetration   = 0.75,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = FALSE,
  can_surrender = TRUE
); k <- k + 1
rows[[k]] <- make_row(
  name          = "hilo_best",
  strategy      = "hilo",
  res           = res_hilo_all_on,
  N             = N,
  n_decks       = 2,
  penetration   = 0.9,
  hit_soft_17   = FALSE,
  payout_bj     = 1.5,
  can_double    = TRUE,
  can_split     = TRUE,
  can_surrender = TRUE
); k <- k + 1

rows[[k]] <- make_row(
  name          = "hilo_worst",
  strategy      = "hilo",
  res           = res_hilo_all_off,
  N             = N,
  n_decks       = 6,
  penetration   = 0.5,
  hit_soft_17   = TRUE,
  payout_bj     = 1.2,
  can_double    = FALSE,
  can_split     = FALSE,
  can_surrender = FALSE
); k <- k + 1
# -------------------------------------------------------
# Končni veliki data.frame:
# -------------------------------------------------------
results_all <- do.call(rbind, rows)

write.csv(results_all, "results_all.csv", row.names = FALSE)

results_all

subset(results_all, strategy == "hilo")

subset(results_all, grepl("pen_", scenario))

subset(results_all, scenario %in% c("hilo_S17_rule","hilo_H17_rule"))



set.seed(123)

res_hilo_big <- simulate_with_shoe_hilo(
  N            = 1e6,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,  # S17
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

ev_by_TC_df <- data.frame(
  true_count = res_hilo_big$true_count,
  gains      = res_hilo_big$gains,
  bet        = res_hilo_big$bets,
  gains = res_hilo_big$gains,
  bankroll = res_hilo_big$bankroll
  
)

write.csv(res_hilo_big, "ev_by_TC_df.csv", row.names = FALSE)












