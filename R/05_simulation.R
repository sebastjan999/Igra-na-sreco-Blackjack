#(EV, SE, IZ, dobicki) za eno igro

simulate_n <- function(N = 1e5, ...) {
  gains <- numeric(N)
  for (i in seq_len(N)) gains[i] <- simulate_hand(...)
  mu <- mean(gains); s <- sd(gains); se <- s/sqrt(N)
  ci <- c(mu - 1.96*se, mu + 1.96*se)
  list(EV = mu, SE = se, CI95 = ci, gains = gains)
}


# MONTE CARLO preko istega SHOE (penetration + reshuffle)
simulate_with_shoe <- function(N = 1e5,
                               n_decks = 6,
                               penetration = 0.75,
                               hit_soft_17 = FALSE,
                               bet = 1,
                               payout_bj = 1.5,
                               can_double = TRUE,
                               can_split = TRUE,
                               can_surrender = TRUE) {
  # izberi pravo strategijo za igralca glede na pravila delivca
  if (isTRUE(hit_soft_17)) {
    BS_TABLE_CURRENT <<- BS_TABLE_H17
  } else {
    BS_TABLE_CURRENT <<- BS_TABLE_S17
  }
  shoe <- init_shoe(n_decks = n_decks, penetration = penetration)
  
  gains        <- numeric(N)
  bets         <- numeric(N)
  bj_player    <- logical(N)
  bj_dealer    <- logical(N)
  surrendered  <- logical(N)
  player_bust  <- logical(N)
  dealer_bust  <- logical(N)
  doubled      <- logical(N)
  
  for (i in seq_len(N)) {
    shoe <- maybe_reshuffle(shoe)  # preveri cut pred vsako roko
    
    res <- deal_hand_from_shoe(
      shoe          = shoe,
      hit_soft_17   = hit_soft_17,
      bet           = bet,
      payout_bj     = payout_bj,
      can_double    = can_double,
      can_split     = can_split,
      can_surrender = can_surrender
    )
    
    gains[i]       <- res$gain
    shoe           <- res$shoe
    bj_player[i]   <- isTRUE(res$player_bj)
    bj_dealer[i]   <- isTRUE(res$dealer_bj)
    surrendered[i] <- isTRUE(res$surrendered)
    player_bust[i] <- isTRUE(res$player_bust)
    dealer_bust[i] <- isTRUE(res$dealer_bust)
    doubled[i]     <- isTRUE(res$doubled)
    bet_mult <- if (isTRUE(res$doubled)) 2 else 1
    bets[i]  <- bet * bet_mult
  }
  
  # osnovne statistike
  mu  <- mean(gains)
  he  <- -mu
  v   <- var(gains)
  s   <- sd(gains)
  se  <- s / sqrt(N)
  ci  <- c(mu - 1.96 * se, mu + 1.96 * se)
  
  #stake metrike
  avg_bet_per_hand <- mean(bets)
  total_bet        <- sum(bets)
  ROI              <- if (total_bet > 0) sum(gains) / total_bet else NA_real_
  
  # house edge na enoto stave
  HE_per_bet <- if (avg_bet_per_hand > 0) -mu / avg_bet_per_hand else NA_real_
  
  #izidi
  win_rate  <- mean(gains > 0)
  loss_rate <- mean(gains < 0)
  push_rate <- mean(gains == 0)
  
  # posebni eventi
  bj_rate_player   <- mean(bj_player)
  bj_rate_dealer   <- mean(bj_dealer)
  surrender_rate   <- mean(surrendered)
  player_bust_rate <- mean(player_bust)
  dealer_bust_rate <- mean(dealer_bust)
  double_rate      <- mean(doubled)
  
  # bankroll potek + max drawdown
  bankroll <- cumsum(gains)
  dd       <- cummax(bankroll) - bankroll
  max_drawdown <- max(dd)   # največji absolutni padec
  
  list(
    # osnovne metrike
    N                   = N,
    EV                  = mu,
    HE_per_hand         = he,
    Var                 = v,
    SD                  = s,
    SE                  = se,
    CI95                = ci,
    
    #stake metrike
    avg_bet_per_hand = avg_bet_per_hand,
    HE_per_bet       = HE_per_bet,
    total_bet        = total_bet,
    ROI              = ROI,
    bets             = bets,
    
    # izidi
    win_rate   = win_rate,
    loss_rate  = loss_rate,
    push_rate  = push_rate,
    
    # posebni eventi
    bj_rate_player   = bj_rate_player,
    bj_rate_dealer   = bj_rate_dealer,
    surrender_rate   = surrender_rate,
    player_bust_rate = player_bust_rate,
    dealer_bust_rate = dealer_bust_rate,
    double_rate      = double_rate,
    
    # bankroll
    bankroll     = bankroll,
    max_drawdown = max_drawdown,
    
    # surovi podatki (za grafe itd.)
    gains        = gains,
    bj_player    = bj_player,
    bj_dealer    = bj_dealer,
    surrendered  = surrendered,
    player_bust  = player_bust,
    dealer_bust  = dealer_bust,
    doubled      = doubled
  )
}



# MONTE CARLO preko istega SHOE z Hi-Lo štetjem  + bet spread
simulate_with_shoe_hilo <- function(N = 1e5,
                                    n_decks = 6,
                                    penetration = 0.75,
                                    hit_soft_17 = FALSE,
                                    bet = 1,
                                    payout_bj = 1.5,
                                    can_double = TRUE,
                                    can_split = TRUE,
                                    can_surrender = TRUE) {
  # izberi pravo strategijo za igralca glede na pravila delivca
  if (isTRUE(hit_soft_17)) {
    BS_TABLE_CURRENT <<- BS_TABLE_H17
  } else {
    BS_TABLE_CURRENT <<- BS_TABLE_S17
  }
  shoe <- init_shoe(n_decks = n_decks, penetration = penetration)
  
  gains        <- numeric(N)
  running_cnt  <- numeric(N)
  true_cnt     <- numeric(N)
  bet_s        <- numeric(N)
  bet_base  <- numeric(N)
  
  bj_player    <- logical(N)
  bj_dealer    <- logical(N)
  surrendered  <- logical(N)
  player_bust  <- logical(N)
  dealer_bust  <- logical(N)
  doubled      <- logical(N)
  
  rc <- 0L  # running count
  
  for (i in seq_len(N)) {
    shoe <- maybe_reshuffle(shoe)
    if (isTRUE(shoe$reshuffled)) {
      rc <- 0L  # nov shoe -> reset
    }
    
    # decks remaining pred deljenjem te roke
    decks_remaining <- (shoe$total - shoe$pos + 1) / 52
    tc_i <- if (decks_remaining > 0) rc / decks_remaining else 0
    
    true_cnt[i]    <- tc_i
    running_cnt[i] <- rc
    
    # bet spread
    spread_mult <- bet_spread(tc_i)  # npr. 1, 2, 4, 8 ...
    bet_i       <- bet * spread_mult
    
    res <- deal_hand_from_shoe_hilo(
      shoe          = shoe,
      running_count = rc,
      hit_soft_17   = hit_soft_17,
      bet           = bet_i,
      payout_bj     = payout_bj,
      can_double    = can_double,
      can_split     = can_split,
      can_surrender = can_surrender,
      verbose       = FALSE
    )
    
    gains[i]       <- res$gain
    bet_base[i]    <- bet_i
    bet_mult       <- if (isTRUE(res$doubled)) 2 else 1
    bet_s[i]       <- bet_i * bet_mult
    shoe           <- res$shoe
    rc             <- res$running_count
  
    bj_player[i]   <- isTRUE(res$player_bj)
    bj_dealer[i]   <- isTRUE(res$dealer_bj)
    surrendered[i] <- isTRUE(res$surrendered)
    player_bust[i] <- isTRUE(res$player_bust)
    dealer_bust[i] <- isTRUE(res$dealer_bust)
    doubled[i]     <- isTRUE(res$doubled)

  }
  
  # osnovne statistike
  mu  <- mean(gains)
  he  <- -mu
  v   <- var(gains)
  s   <- sd(gains)
  se  <- s / sqrt(N)
  ci  <- c(mu - 1.96 * se, mu + 1.96 * se)
  
  win_rate  <- mean(gains > 0)
  loss_rate <- mean(gains < 0)
  push_rate <- mean(gains == 0)
  
  # povprečna stava na roko
  avg_bet_per_hand <- mean(bet_s)
  
  # house edge na enoto stave (kot v literaturi)
  HE_per_bet <- if (avg_bet_per_hand > 0) -mu / avg_bet_per_hand else NA_real_
  
  bj_rate_player   <- mean(bj_player)
  bj_rate_dealer   <- mean(bj_dealer)
  surrender_rate   <- mean(surrendered)
  player_bust_rate <- mean(player_bust)
  dealer_bust_rate <- mean(dealer_bust)
  double_rate      <- mean(doubled)
  
  # bankroll in drawdown
  bankroll <- cumsum(gains)
  dd       <- cummax(bankroll) - bankroll
  max_drawdown <- max(dd)   # največji absolutni padec
  
  # ROI in EV na 100 iger
  total_bet <- sum(bet_s)
  ROI       <- if (total_bet > 0) sum(gains) / total_bet else NA_real_
  EV_100    <- 100 * mu
  
  # EV po true count (za graf)
  tc_round <- round(true_cnt)
  EV_by_TC <- tapply(gains, tc_round, mean)
  
  list(
    # osnovne metrike
    N                = N,
    EV               = mu,
    avg_bet_per_hand = avg_bet_per_hand,
    HE_per_bet       = HE_per_bet,
    HE_per_hand      = he,
    Var              = v,
    SD               = s,
    SE               = se,
    CI95             = ci,
    EV_100           = EV_100,
    
    # izidi
    win_rate   = win_rate,
    loss_rate  = loss_rate,
    push_rate  = push_rate,
    
    # posebni eventi
    bj_rate_player   = bj_rate_player,
    bj_rate_dealer   = bj_rate_dealer,
    surrender_rate   = surrender_rate,
    player_bust_rate = player_bust_rate,
    dealer_bust_rate = dealer_bust_rate,
    double_rate      = double_rate,
    
    # Hi-Lo stvari
    running_count = running_cnt,
    true_count    = true_cnt,
    EV_by_TC      = EV_by_TC,
    
    # stave & ROI
    bet_base = bet_base,
    bets       = bet_s,
    total_bet  = total_bet,
    ROI        = ROI,
    
    # bankroll
    bankroll     = bankroll,
    max_drawdown = max_drawdown,
    
    # surovi podatki
    gains        = gains,
    bj_player    = bj_player,
    bj_dealer    = bj_dealer,
    surrendered  = surrendered,
    player_bust  = player_bust,
    dealer_bust  = dealer_bust,
    doubled      = doubled
  )
}


