run_hilo_scenario <- function(
    N            = 1e5,     # število iger v eni simulaciji
    R_reps       = 10,      # ponovitve simulacije (različni semeni)
    seeds        = NULL,    
    n_decks      = 6,       # število kompletov kart v shoe
    penetration  = 0.75,    # delež odigranih kart pred reshuffle
    hit_soft_17  = FALSE,   # S17 = FALSE, H17 = TRUE
    bet          = 1,       # osnovna stava, preden se uporabi bet spread
    payout_bj    = 1.5,     # izplačilo za blackjack (3:2)
    can_double   = TRUE,    # ali igralec lahko double-a
    can_split    = TRUE,    # ali je split dovoljen
    can_surrender= TRUE     # ali je surrender dovoljen
) {
  
  # -----------------------------
  #  Seeds setup
  # -----------------------------
  if (is.null(seeds)) {
    seeds <- seq_len(R_reps)
  } else {
    R_reps <- length(seeds)
  }
  
  # -----------------------------
  #  Rezultati posameznih ponovitev
  # -----------------------------
  EV_vec   <- numeric(R_reps)   # expected value (povprečni dobiček na igro)
  ROI_vec  <- numeric(R_reps)   # return on investment: sum(gain) / sum(bet)
  HE_vec   <- numeric(R_reps)   # house edge(_per_bet) = EV / average bet
  SD_vec   <- numeric(R_reps)   # standardni odklon posamezne simulacije
  DD_vec   <- numeric(R_reps)   # max drawdown (največji padec bankrolla)
  
  # -----------------------------
  #  Zanka preko R_reps ponovitev
  # -----------------------------
  for (r in seq_len(R_reps)) {
    set.seed(seeds[r])
    
    res <- simulate_with_shoe_hilo(
      N            = N,
      n_decks      = n_decks,
      penetration  = penetration,
      hit_soft_17  = hit_soft_17,
      bet          = bet,
      payout_bj    = payout_bj,
      can_double   = can_double,
      can_split    = can_split,
      can_surrender= can_surrender
    )
    
    # osnovne statistike simulacije
    EV_vec[r]  <- res$EV      # expected value (povprečje dobičkov)
    ROI_vec[r] <- res$ROI     # ROI = sum(gain) / sum(bet)
    SD_vec[r]  <- res$SD      # standard deviation of gains
    DD_vec[r]  <- res$max_drawdown   # max drawdown over cumsum(gains)
    
    # house edge = EV / average bet
    avg_bet_per_hand <- mean(res$bets)
    HE_vec[r] <- if (avg_bet_per_hand > 0) -res$EV / avg_bet_per_hand else NA_real_
  }
  
  # -----------------------------
  #  KONČNI REZULTAT: data.frame
  # -----------------------------
  data.frame(
    # --------------------------
    # Parametri scenarija
    # --------------------------
    N            = N,               # število iger na ponovitev
    R_reps       = R_reps,          # število ponovitev simulacije
    n_decks      = n_decks,         # # kompletov kart
    penetration  = penetration,     # cutoff točke za reshuffle
    hit_soft_17  = hit_soft_17,     # ali delilec vleče na soft 17 (H17)
    payout_bj    = payout_bj,       # izplačilo za natural blackjack
    can_double   = can_double,      # pravilo: double allowed
    can_split    = can_split,       # pravilo: split pairs allowed
    can_surrender= can_surrender,   # pravilo: late surrender allowed
    
    # --------------------------
    # Glavne metrike (povprečja čez R_reps)
    # --------------------------
    EV_mean      = mean(EV_vec),    # povprečen EV čez ponovitve
    EV_sd        = sd(EV_vec),      # standardna napaka EV med ponovitvami
    
    ROI_mean     = mean(ROI_vec),   # povprečen ROI (profit / vložek)
    ROI_sd       = sd(ROI_vec),     # variabilnost ROI
    
    HE_mean      = mean(HE_vec, na.rm = TRUE),  # povprečen house edge
    HE_sd        = sd(HE_vec, na.rm = TRUE),    # odklon house edge
    
    SD_mean      = mean(SD_vec),    # povprečni standardni odklon dobičkov
    DD_mean      = mean(DD_vec)     # povprečni max drawdown bankrolla
  )
}

