
#(EV, SE, IZ, dobicki) ene odigrane igre
simulate_n <- function(N = 1e5, ...) {
  gains <- numeric(N)
  for (i in seq_len(N)) gains[i] <- simulate_hand(...)
  mu <- mean(gains); s <- sd(gains); se <- s/sqrt(N)
  ci <- c(mu - 1.96*se, mu + 1.96*se)
  list(EV = mu, SE = se, CI95 = ci, gains = gains)
}


#MONTE CARLO preko istega SHOE (penetration + reshuffle)
simulate_with_shoe <- function(N = 1e5,
                               n_decks = 6,
                               penetration = 0.75,
                               hit_soft_17 = FALSE,
                               bet = 1,
                               payout_bj = 1.5) {
  shoe <- init_shoe(n_decks = n_decks, penetration = penetration)
  gains <- numeric(N)
  
  for (i in seq_len(N)) {
    shoe <- maybe_reshuffle(shoe)  # preveri cut pred vsako roko
    res <- deal_hand_from_shoe(shoe, hit_soft_17 = hit_soft_17, bet = bet, payout_bj = payout_bj)
    gains[i] <- res$gain
    shoe <- res$shoe
  }
  
  mu <- mean(gains); s <- sd(gains); se <- s/sqrt(N)
  ci <- c(mu - 1.96*se, mu + 1.96*se)
  list(EV = mu, SE = se, CI95 = ci, gains = gains)
}


#MONTE CARLO preko istega SHOE z Hi-Lo štetjem  + bet spread
simulate_with_shoe_hilo <- function(N = 1e5,
                                    n_decks = 6,
                                    penetration = 0.75,
                                    hit_soft_17 = FALSE,
                                    bet = 1,
                                    payout_bj = 1.5) {
  shoe <- init_shoe(n_decks = n_decks, penetration = penetration)
  gains <- numeric(N)
  running_count <- integer(N)
  true_count    <- numeric(N)
  bet_s <- numeric(N)
  rc <- 0L
  
  for (i in seq_len(N)) {
    shoe <- maybe_reshuffle(shoe)
    if (isTRUE(shoe$reshuffled)) {
      rc <- 0L  # nov shoe -> reset
    }
    
    # decks_remaining pred deljenjem te roke
    decks_remaining <- (shoe$total - shoe$pos + 1) / 52
    true_count[i]   <- if (decks_remaining > 0) rc / decks_remaining else 0
    running_count[i] <- rc
    
    # določi bet_i glede na tc (bet spread)
    spread_mult <- bet_spread(true_count[i])   # 1, 2, 4, 8 ...
    bet_i <- bet * spread_mult      # realna stava v tej roki
    bet_s[i] <- bet_i
    
    res <- deal_hand_from_shoe_hilo(
      shoe          = shoe,
      running_count = rc,
      hit_soft_17   = hit_soft_17,
      bet           = bet_i,
      payout_bj     = payout_bj,
      verbose       = FALSE
    )
    gains[i] <- res$gain
    shoe     <- res$shoe
    rc       <- res$running_count
  }
  
  mu <- mean(gains); s <- sd(gains); se <- s/sqrt(N)
  ci <- c(mu - 1.96*se, mu + 1.96*se)
  
  list(
    EV = mu,
    SE = se,
    CI95 = ci,
    gains = gains,
    running_count = running_count,
    true_count = true_count, print(true_count),
    bet = bet_s, print(bet_s)
  )
}
=======
#(EV, SE, IZ, dobicki) ene odigrane igre
simulate_n <- function(N = 1e5, ...) {
  gains <- numeric(N)
  for (i in seq_len(N)) gains[i] <- simulate_hand(...)
  mu <- mean(gains); s <- sd(gains); se <- s/sqrt(N)
  ci <- c(mu - 1.96*se, mu + 1.96*se)
  list(EV = mu, SE = se, CI95 = ci, gains = gains)
}


#MONTE CARLO preko istega SHOE (penetration + reshuffle)
simulate_with_shoe <- function(N = 1e5,
                               n_decks = 6,
                               penetration = 0.75,
                               hit_soft_17 = FALSE,
                               bet = 1,
                               payout_bj = 1.5) {
  shoe <- init_shoe(n_decks = n_decks, penetration = penetration)
  gains <- numeric(N)
  
  for (i in seq_len(N)) {
    shoe <- maybe_reshuffle(shoe)  # preveri cut pred vsako roko
    res <- deal_hand_from_shoe(shoe, hit_soft_17 = hit_soft_17, bet = bet, payout_bj = payout_bj)
    gains[i] <- res$gain
    shoe <- res$shoe
  }
  
  mu <- mean(gains); s <- sd(gains); se <- s/sqrt(N)
  ci <- c(mu - 1.96*se, mu + 1.96*se)
  list(EV = mu, SE = se, CI95 = ci, gains = gains)
}


#MONTE CARLO preko istega SHOE z Hi-Lo štetjem  + bet spread
simulate_with_shoe_hilo <- function(N = 1e5,
                                    n_decks = 6,
                                    penetration = 0.75,
                                    hit_soft_17 = FALSE,
                                    bet = 1,
                                    payout_bj = 1.5) {
  shoe <- init_shoe(n_decks = n_decks, penetration = penetration)
  gains <- numeric(N)
  running_count <- integer(N)
  true_count    <- numeric(N)
  bet_s <- numeric(N)
  rc <- 0L
  
  for (i in seq_len(N)) {
    shoe <- maybe_reshuffle(shoe)
    if (isTRUE(shoe$reshuffled)) {
      rc <- 0L  # nov shoe -> reset
    }
    
    # decks_remaining pred deljenjem te roke
    decks_remaining <- (shoe$total - shoe$pos + 1) / 52
    true_count[i]   <- if (decks_remaining > 0) rc / decks_remaining else 0
    running_count[i] <- rc
    
    # določi bet_i glede na tc (bet spread)
    spread_mult <- bet_spread(true_count[i])   # 1, 2, 4, 8 ...
    bet_i <- bet * spread_mult      # realna stava v tej roki
    bet_s[i] <- bet_i
    
    res <- deal_hand_from_shoe_hilo(
      shoe          = shoe,
      running_count = rc,
      hit_soft_17   = hit_soft_17,
      bet           = bet_i,
      payout_bj     = payout_bj,
      verbose       = FALSE
    )
    gains[i] <- res$gain
    shoe     <- res$shoe
    rc       <- res$running_count
  }
  
  mu <- mean(gains); s <- sd(gains); se <- s/sqrt(N)
  ci <- c(mu - 1.96*se, mu + 1.96*se)
  
  list(
    EV = mu,
    SE = se,
    CI95 = ci,
    gains = gains,
    running_count = running_count,
    true_count = true_count, print(true_count),
    bet = bet_s, print(bet_s)
  )
}
>>>>>>> 0296a7da9f3f19124a1bef82f51fc27adebc5b49
