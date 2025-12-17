# Pravila delivca (S17/H17)
dealer_play <- function(shoe, up, hole, hit_soft_17 = FALSE) {
  hand <- c(up, hole)
  i <- 1
  repeat {
    v <- hand_value(hand)
    soft <- is_soft(hand)
    if (v > 21) break
    # S17: stoj na kateremkoli 17 (tudi soft)
    # H17: vleci pri soft 17, stoj pri hard 17
    if (v > 17) break
    if (v == 17 && !hit_soft_17) break
    if (v == 17 && hit_soft_17 && !soft) break
    hand <- c(hand, shoe$val[i]); i <- i + 1
  }
  list(value = hand_value(hand), cards = hand, next_idx = i)
}

# Igralčev potek roke (brez splitov, demo) ----------------------
play_player <- function(shoe, up_card, strategy = basic_action) {
  idx <- 1 #stevec naslednje karte v kupcku
  player <- c(shoe$val[idx], shoe$val[idx+1]); idx <- idx + 2 #damo mu 2 karti iz deka
  action_hist <- c() #zgodovina potez
  doubled <- FALSE
  
  repeat {
    a <- strategy(player, up_card, can_double = (length(player) == 2)) #strategija kle je pac una basic demo zaenkrat
    action_hist <- c(action_hist, a)
    
    if (a == "stand") break
    
    if (a == "double") { 
      player <- c(player, shoe$val[idx]); idx <- idx + 1
      doubled <- TRUE
      break 
    }
    
    if (a == "hit")    { player <- c(player, shoe$val[idx]); idx <- idx + 1 
    
    if (hand_value(player) > 21) break 
    }
  }
  
  list(value = hand_value(player),
       cards = player,
       next_idx = idx,
       actions = action_hist,
       doubled = doubled)
}

# Igralčev potek roke z Hi-Lo stetjem in CSV strategijo
play_player_from_hand <- function(start_hand, shoe, up_card,
                                  strategy = basic_action,
                                  verbose = FALSE) {
  hand <- start_hand
  idx  <- 1
  action_hist <- character(0)
  doubled <- FALSE
  
  if (verbose) {
    cat(sprintf("PLAYER start: cards=%s (total=%d)\n",
                paste(hand, collapse=","), hand_value(hand)))
  }
  
  surrendered <- FALSE 
  
  repeat {
    a <- strategy(hand, up_card, can_double = (length(hand) == 2))
    action_hist <- c(action_hist, a)
    if (verbose) cat(sprintf(" -> action: %s\n", a))
    
    if (a == "surrender") {
      surrendered <- TRUE
      break
    }
    
    if (a == "stand") break
    
    if (a == "double") {
      hand <- c(hand, shoe$val[idx]); idx <- idx + 1
      doubled <- TRUE
      if (verbose) {
        cat(sprintf("    drew: %d   (total=%d) [DOUBLE]\n",
                    tail(hand,1), hand_value(hand)))
      }
      break
    }
    
    if (a == "hit") {
      hand <- c(hand, shoe$val[idx]); idx <- idx + 1
      if (verbose) {
        cat(sprintf("    drew: %d   (total=%d)\n",
                    tail(hand,1), hand_value(hand)))
      }
      if (hand_value(hand) > 21) break
    }
  }
  
  if (verbose) {
    cat(sprintf("PLAYER final: cards=%s (total=%d) doubled=%s\n",
                paste(hand, collapse=","), hand_value(hand),
                ifelse(doubled,"TRUE","FALSE")))
  }
  
  list(
    value   = hand_value(hand),
    cards   = hand,
    next_idx = idx,
    actions = action_hist,
    doubled = doubled,
    surrendered = surrendered
  )
}
#bj payout nastavmo na 3:2 (kksni casinoji majo sicer slabs 6:5...kasneje za metrike pa HE bomo mal spreminjal)
simulate_hand <- function(n_decks = 6, hit_soft_17 = FALSE, bet = 1, payout_bj = 1.5) {
  shoe <- make_deck(n_decks)
  shoe <- shoe[sample(nrow(shoe)),]  # premešamo
  
  # po premešanju
  p1 <- shoe$val[1];  up   <- shoe$val[2]
  p2 <- shoe$val[3];  hole <- shoe$val[4]
  idx <- 5
  player_start <- c(p1, p2)
  dealer_start <- c(up, hole)
  
  # blackjack check (pred igralčevimi potezami)
  p_bj <- is_blackjack(player_start)
  d_bj <- is_blackjack(dealer_start)
  if (p_bj || d_bj) {
    if (p_bj && d_bj) return(0)
    if (p_bj && !d_bj) return(bet * payout_bj)  # npr. 1.5 za 3:2
    if (!p_bj && d_bj) return(-bet)
  }
  
  # player
  pl <- play_player(shoe[idx:nrow(shoe), ], up_card = up, strategy = basic_action)
  idx <- idx + pl$next_idx - 1
  
  # Če je igralec bust
  if (pl$value > 21) {
    bet_mult <- if (isTRUE(pl$doubled)) 2 else 1
    return(-bet * bet_mult)
  }
  
  # dealer
  dl <- dealer_play(shoe[idx:nrow(shoe), ], up, hole, hit_soft_17)
  idx <- idx + dl$next_idx - 1
  
  # izid
  pv <- pl$value; dv <- dl$value
  bet_mult <- if (isTRUE(pl$doubled)) 2 else 1
  res <-  if (dv > 21) bet * bet_mult 
  else if (pv > dv) bet * bet_mult
  else if (pv < dv) -bet * bet_mult
  else 0
  res
}

#helper za Izračuna dobiček ene igralčeve roke proti delivcu(need for split)
hand_gain_vs_dealer <- function(player_value, dealer_value, bet, doubled_flag) {
  # Če dealer sploh ni igral (NA), privzamemo 0 prispevek
  if (is.na(dealer_value)) return(0)
  
  bet_mult <- if (isTRUE(doubled_flag)) 2 else 1
  
  if (dealer_value > 21) {
    return(bet * bet_mult)
  }
  if (player_value > dealer_value) {
    return(bet * bet_mult)
  }
  if (player_value < dealer_value) {
    return(-bet * bet_mult)
  }
  return(0)
}


# ENA IGRA IZ OBSTOJEČEGA SHOE (z BJ in double)
deal_hand_from_shoe <- function(shoe,
                                hit_soft_17 = FALSE,
                                bet = 1,
                                payout_bj = 1.5,
                                can_double = TRUE,
                                can_split = TRUE,
                                can_surrender = TRUE) {
  
  can_double_global    <- can_double
  can_split_global     <- can_split
  can_surrender_global <- can_surrender
  # poskrbi za dovolj kart za začetno deljenje
  slice <- shoe_slice_df(shoe)
  if (nrow(slice) < 4) { shoe <- maybe_reshuffle(shoe); slice <- shoe_slice_df(shoe) }
  
  # casino vrstni red: P1, D_up, P2, D_hole
  p1   <- slice$val[1];  up   <- slice$val[2]
  p2   <- slice$val[3];  hole <- slice$val[4]
  shoe <- advance_shoe(shoe, 4)
  
  player_start <- c(p1, p2)
  dealer_start <- c(up, hole)
  
  # Blackjack check
  psbj <- is_blackjack(player_start)
  dsbj <- is_blackjack(dealer_start)
  if (psbj || dsbj) {
    gain <- if (psbj && dsbj) 0
    else if (psbj && !dsbj) bet * payout_bj
    else -bet
    
    return(list(
      gain         = gain,
      shoe         = shoe,
      player_bj    = psbj,
      dealer_bj    = dsbj,
      player_bust  = FALSE,
      dealer_bust  = FALSE,
      surrendered  = FALSE,
      doubled      = FALSE
    ))
  }
  
  ### [NEW] SPLIT LOGIC: preveri, če naj začetno roko splitemo
  # (enkratni split, brez re-splita)
  initial_action <- basic_action_bs(
    player_vals    = player_start,
    dealer_up      = up,
    can_double     = can_double,
    can_split      = can_split,
    can_surrender  = can_surrender,
    bs_table       = BS_TABLE_CURRENT
  )
  
  if (initial_action == "split" && can_split) {
    # nardimo 2 roki: (p1, nova karta), (p2, nova karta)
    slice2 <- shoe_slice_df(shoe)
    if (nrow(slice2) < 2) {
      shoe   <- maybe_reshuffle(shoe)
      slice2 <- shoe_slice_df(shoe)
    }
    
    new1 <- slice2$val[1]
    new2 <- slice2$val[2]
    shoe <- advance_shoe(shoe, 2)
    
    hand1_start <- c(p1, new1)
    hand2_start <- c(p2, new2)
    
    # hand1
    slice_h1 <- shoe_slice_df(shoe)
    pl1 <- play_player_from_hand(
      start_hand = hand1_start,
      shoe       = slice_h1,
      up_card    = up,
      strategy   = function(hand, upc, can_double = TRUE, ...) {
        eff_double <- isTRUE(can_double_global) && isTRUE(can_double)
        basic_action_bs(
          player_vals    = hand,
          dealer_up      = upc,
          can_double     = eff_double,
          can_split      = FALSE,          # NE dovolimo re-splita
          can_surrender  = isTRUE(can_surrender_global),
          bs_table       = BS_TABLE_CURRENT
        )
      },
      verbose    = FALSE
    )
    shoe <- advance_shoe(shoe, pl1$next_idx - 1)
    
    # hand2
    slice_h2 <- shoe_slice_df(shoe)
    pl2 <- play_player_from_hand(
      start_hand = hand2_start,
      shoe       = slice_h2,
      up_card    = up,
      strategy   = function(hand, upc, can_double = TRUE, ...) {
        eff_double <- isTRUE(can_double_global) && isTRUE(can_double)
        basic_action_bs(
          player_vals    = hand,
          dealer_up      = upc,
          can_double     = eff_double,
          can_split      = FALSE,          # NE dovolimo re-splita
          can_surrender  = isTRUE(can_surrender_global),
          bs_table       = BS_TABLE_CURRENT
        )
      },
      verbose    = FALSE
    )
    shoe <- advance_shoe(shoe, pl2$next_idx - 1)
    
    # Če sta obe roki bust/surrender, dealer sploh ne rabi igrat, ampak za enostavnost:
    # - če vsaj ena roka še stoji, delivec igra normalno
    need_dealer <- !(pl1$value > 21 && pl2$value > 21) &&
      !(pl1$surrendered && pl2$surrendered)
    
    if (need_dealer) {
      slice_d <- shoe_slice_df(shoe)
      dl <- dealer_play(slice_d, up = up, hole = hole, hit_soft_17 = hit_soft_17)
      shoe <- advance_shoe(shoe, dl$next_idx - 1)
      dv <- dl$value
    } else {
      dv <- NA_integer_
    }
    
    # izračun dobička za obe roki
    gain1 <- 0
    gain2 <- 0
    
    # hand1
    if (pl1$surrendered) {
      bet_mult1 <- if (isTRUE(pl1$doubled)) 2 else 1
      gain1 <- -0.5 * bet_mult1 * bet
    } else if (pl1$value > 21) {
      bet_mult1 <- if (isTRUE(pl1$doubled)) 2 else 1
      gain1 <- -bet_mult1 * bet
    } else if (!is.na(dv)) {
      gain1 <- hand_gain_vs_dealer(pl1$value, dv, bet, pl1$doubled)
    }
    
    # hand2
    if (pl2$surrendered) {
      bet_mult2 <- if (isTRUE(pl2$doubled)) 2 else 1
      gain2 <- -0.5 * bet_mult2 * bet
    } else if (pl2$value > 21) {
      bet_mult2 <- if (isTRUE(pl2$doubled)) 2 else 1
      gain2 <- -bet_mult2 * bet
    } else if (!is.na(dv)) {
      gain2 <- hand_gain_vs_dealer(pl2$value, dv, bet, pl2$doubled)
    }
    
    gain_total <- gain1 + gain2
    
    return(list(
      gain         = gain_total,
      shoe         = shoe,
      player_bj    = FALSE,
      dealer_bj    = FALSE,
      player_bust  = (pl1$value > 21 || pl2$value > 21),
      dealer_bust  = (!is.na(dv) && dv > 21),
      surrendered  = (pl1$surrendered || pl2$surrendered),
      doubled      = (pl1$doubled || pl2$doubled)
    ))
  }
  ### [END SPLIT LOGIC]
  
  # ===== če NI splita, nadaljujemo po stari logiki =====
  # Igralec odigra (iz trenutnega shoe)
  slice <- shoe_slice_df(shoe)
  pl <- play_player_from_hand(
    start_hand = player_start,
    shoe       = slice,
    up_card    = up,
    strategy   = function(hand, upc, can_double = TRUE, ...) {
      # efektivno: globalni & lokalni (npr. po prvih dveh kartah še dovoli double)
      eff_double <- isTRUE(can_double_global) && isTRUE(can_double)
      eff_split  <- FALSE
      
      action <- basic_action_bs(
        player_vals = hand,
        dealer_up   = upc,
        can_double  = eff_double,
        can_split   = eff_split,
        can_surrender  = isTRUE(can_surrender_global),
        bs_table = BS_TABLE_CURRENT
      )
      
      action
    },
    verbose    = FALSE
  )
  
  shoe <- advance_shoe(shoe, pl$next_idx - 1)
  
  #Opcija za surrender
  if (pl$surrendered) {
    bet_mult <- if (isTRUE(pl$doubled)) 2 else 1  # surrender po double je v praksi malo tricky, lahko pa recimo prepoveš "R" po dvojitvi v tabeli
    gain <- -0.5 * bet_mult * bet  # izgubiš pol stave
    return(list(
      gain         = gain,
      shoe         = shoe,
      player_bj    = FALSE,
      dealer_bj    = FALSE,
      player_bust  = FALSE,
      dealer_bust  = FALSE,
      surrendered  = TRUE,
      doubled      = pl$doubled
    ))
  }
  
  # Če bust, takoj vrni
  if (pl$value > 21) {
    bet_mult <- if (isTRUE(pl$doubled)) 2 else 1
    gain <- -bet * bet_mult
    return(list(
      gain         = gain,
      shoe         = shoe,
      player_bj    = FALSE,
      dealer_bj    = FALSE,
      player_bust  = TRUE,
      dealer_bust  = FALSE,
      surrendered  = FALSE,
      doubled      = pl$doubled
    ))
  }
  
  # Delivec odigra (po S17/H17 pravilih) iz preostanka čevlja
  slice <- shoe_slice_df(shoe)
  dl <- dealer_play(slice, up = up, hole = hole, hit_soft_17 = hit_soft_17)
  shoe <- advance_shoe(shoe, dl$next_idx - 1)
  
  # Rezultat (tuki vse isto kukr pr 1 normalni roki)
  pv <- pl$value; dv <- dl$value
  bet_mult <- if (isTRUE(pl$doubled)) 2 else 1
  gain <- if (dv > 21)  bet * bet_mult
  else if (pv > dv)  bet * bet_mult
  else if (pv < dv) -bet * bet_mult
  else 0
  
  list(
    gain         = gain,
    shoe         = shoe,
    player_bj    = FALSE,
    dealer_bj    = FALSE,
    player_bust  = FALSE,
    dealer_bust  = (dv > 21),
    surrendered  = FALSE,
    doubled      = pl$doubled
  )
}

#ENA IGRA IZ SHOE z Hi-Lo štetjem (+ verbose za debuging)
deal_hand_from_shoe_hilo <- function(shoe, running_count,
                                     hit_soft_17 = FALSE,
                                     bet = 1,
                                     payout_bj = 1.5,
                                     can_double = TRUE,
                                     can_split = TRUE,
                                     can_surrender = TRUE,
                                     verbose = FALSE) {
  
  can_double_global    <- can_double
  can_split_global     <- can_split
  can_surrender_global <- can_surrender
  
  if (verbose) {
    cat("\n=== NEW HAND (Hi-Lo) ===\n")
    cat(sprintf("shoe pos=%d  cut=%d  total=%d  running_count=%d\n",
                shoe$pos, shoe$cut, shoe$total, running_count))
  }
  
  # poskrbi za dovolj kart za začetno deljenje
  slice <- shoe_slice_df(shoe)
  if (nrow(slice) < 4) {
    shoe <- maybe_reshuffle(shoe)
    slice <- shoe_slice_df(shoe)
    running_count <- 0L  # nov shoe -> reset count
    if (verbose) cat("RESHUFFLE (premalo kart za začetno deljenje)\n")
  }
  
  # casino vrstni red: P1, D_up, P2, D_hole
  p1   <- slice$val[1];  up   <- slice$val[2]
  p2   <- slice$val[3];  hole <- slice$val[4]
  shoe <- advance_shoe(shoe, 4)
  
  player_start <- c(p1, p2)
  dealer_start <- c(up, hole)
  
  # Hi-Lo: upoštevaj začetne 4 karte
  running_count <- running_count + sum(hi_lo_delta(c(p1, up, p2, hole)))
  
  if (verbose) {
    cat(sprintf("Initial deal -> Player: %d,%d (total=%d) | Dealer: up=%d, hole=?  | RC=%d\n",
                p1, p2, hand_value(player_start), up, running_count))
    cat(sprintf("shoe pos after initial: %d\n", shoe$pos))
  }
  
  # Blackjack check
  psbj <- is_blackjack(player_start)
  dsbj <- is_blackjack(dealer_start)
  if (psbj || dsbj) {
    if (verbose) cat(sprintf("Blackjack check -> playerBJ=%s, dealerBJ=%s\n", psbj, dsbj))
    gain <- if (psbj && dsbj) 0
    else if (psbj && !dsbj) bet * payout_bj
    else -bet
    return(list(
      gain          = gain,
      shoe          = shoe,
      running_count = running_count,
      player_bj     = psbj,
      dealer_bj     = dsbj,
      player_bust   = FALSE,
      dealer_bust   = FALSE,
      surrendered   = FALSE,
      doubled       = FALSE
    ))
  }
  
  ### --- [NEW] SPLIT LOGIC (Hi-Lo) ---
  initial_action <- basic_action_bs(
    player_vals    = player_start,
    dealer_up      = up,
    can_double     = can_double,
    can_split      = can_split,
    can_surrender  = can_surrender,
    bs_table       = BS_TABLE_CURRENT
  )
  
  if (initial_action == "split" && can_split) {
    
    # nove karte za split roki
    slice2 <- shoe_slice_df(shoe)
    if (nrow(slice2) < 2) {
      shoe  <- maybe_reshuffle(shoe)
      slice2 <- shoe_slice_df(shoe)
      running_count <- 0L
    }
    
    new1 <- slice2$val[1]
    new2 <- slice2$val[2]
    shoe <- advance_shoe(shoe, 2)
    
    # Hi-Lo: dodaj delta za novi karti
    running_count <- running_count + sum(hi_lo_delta(c(new1, new2)))
    
    hand1_start <- c(p1, new1)
    hand2_start <- c(p2, new2)
    
    # --- HAND 1 ---
    slice_h1 <- shoe_slice_df(shoe)
    pl1 <- play_player_from_hand(
      start_hand = hand1_start,
      shoe       = slice_h1,
      up_card    = up,
      strategy   = function(hand, upc, can_double = TRUE, ...) {
        eff_double <- isTRUE(can_double_global) && isTRUE(can_double)
        basic_action_bs(
          player_vals    = hand,
          dealer_up      = upc,
          can_double     = eff_double,
          can_split      = FALSE,         # brez re-splita
          can_surrender  = isTRUE(can_surrender_global),
          bs_table       = BS_TABLE_CURRENT
        )
      },
      verbose    = FALSE
    )
    shoe <- advance_shoe(shoe, pl1$next_idx - 1)
    
    # Hi-Lo: dodatne karte hand1 (nad začetnima dvema)
    if (length(pl1$cards) > 2) {
      extra1 <- pl1$cards[3:length(pl1$cards)]
      running_count <- running_count + sum(hi_lo_delta(extra1))
    }
    
    # --- HAND 2 ---
    slice_h2 <- shoe_slice_df(shoe)
    pl2 <- play_player_from_hand(
      start_hand = hand2_start,
      shoe       = slice_h2,
      up_card    = up,
      strategy   = function(hand, upc, can_double= TRUE, ...) {
        eff_double <- isTRUE(can_double_global) && isTRUE(can_double)
        basic_action_bs(
          player_vals    = hand,
          dealer_up      = upc,
          can_double     = eff_double,
          can_split      = FALSE,
          can_surrender  = isTRUE(can_surrender_global),
          bs_table       = BS_TABLE_CURRENT
        )
      },
      verbose    = FALSE
    )
    shoe <- advance_shoe(shoe, pl2$next_idx - 1)
    
    # Hi-Lo: dodatne karte hand2
    if (length(pl2$cards) > 2) {
      extra2 <- pl2$cards[3:length(pl2$cards)]
      running_count <- running_count + sum(hi_lo_delta(extra2))
    }
    
    # Ali moramo sploh igrati dealerja?
    need_dealer <-
      !(pl1$value > 21 || pl1$surrendered) ||
      !(pl2$value > 21 || pl2$surrendered)
    
    if (need_dealer) {
      slice_d <- shoe_slice_df(shoe)
      dl <- dealer_play(slice_d, up = up, hole = hole, hit_soft_17 = hit_soft_17)
      shoe <- advance_shoe(shoe, dl$next_idx - 1)
      
      # Hi-Lo: dodatne karte delivca
      if (length(dl$cards) > 2) {
        extraD <- dl$cards[3:length(dl$cards)]
        running_count <- running_count + sum(hi_lo_delta(extraD))
      }
      
      dv <- dl$value
    } else {
      dv <- NA_integer_
    }
    
    # --- IZRAČUN DOBIČKA ZA OBE ROKI ---
    gain1 <- 0
    gain2 <- 0
    
    # hand 1
    if (pl1$surrendered) {
      bet_mult1 <- if (isTRUE(pl1$doubled)) 2 else 1
      gain1 <- -0.5 * bet_mult1 * bet
    } else if (pl1$value > 21) {
      bet_mult1 <- if (isTRUE(pl1$doubled)) 2 else 1
      gain1 <- -bet_mult1 * bet
    } else if (!is.na(dv)) {
      gain1 <- hand_gain_vs_dealer(pl1$value, dv, bet, pl1$doubled)
    }
    
    # hand 2
    if (pl2$surrendered) {
      bet_mult2 <- if (isTRUE(pl2$doubled)) 2 else 1
      gain2 <- -0.5 * bet_mult2 * bet
    } else if (pl2$value > 21) {
      bet_mult2 <- if (isTRUE(pl2$doubled)) 2 else 1
      gain2 <- -bet_mult2 * bet
    } else if (!is.na(dv)) {
      gain2 <- hand_gain_vs_dealer(pl2$value, dv, bet, pl2$doubled)
    }
    
    gain_total <- gain1 + gain2
    
    return(list(
      gain          = gain_total,
      shoe          = shoe,
      running_count = running_count,
      player_bj     = FALSE,
      dealer_bj     = FALSE,
      player_bust   = (pl1$value > 21 || pl2$value > 21),
      dealer_bust   = (!is.na(dv) && dv > 21),
      surrendered   = (pl1$surrendered || pl2$surrendered),
      doubled       = (pl1$doubled || pl2$doubled)
    ))
  }
  ### --- END SPLIT LOGIC ---
  
# Igralec odigra (iz trenutnega shoe)
  slice <- shoe_slice_df(shoe)
  pl <- play_player_from_hand(
    start_hand = player_start,
    shoe       = slice,
    up_card    = up,
    strategy   = function(hand, upc, can_double = TRUE, ...) {
      # efektivno: globalni & lokalni (npr. po prvih dveh kartah še dovoli double)
      eff_double <- isTRUE(can_double_global) && isTRUE(can_double)
      eff_split  <- FALSE
      
      action <- basic_action_bs(
        player_vals = hand,
        dealer_up   = upc,
        can_double  = eff_double,
        can_split   = eff_split,
        can_surrender = isTRUE(can_surrender_global),
        bs_table = BS_TABLE_CURRENT
      )
      
      action
    },
    verbose    = FALSE
  )
  shoe <- advance_shoe(shoe, pl$next_idx - 1)
  
  # Hi-Lo: dodatne karte igralca (po začetnih dveh)
  if (length(pl$cards) > length(player_start)) {
    extra_player <- pl$cards[(length(player_start) + 1):length(pl$cards)]
    running_count <- running_count + sum(hi_lo_delta(extra_player))
  }
  
  # Opcija za surrender
  if (pl$surrendered) {
    bet_mult <- if (isTRUE(pl$doubled)) 2 else 1
    gain <- -0.5 * bet_mult * bet  # izgubiš pol stave
    return(list(
      gain          = gain,
      shoe          = shoe,
      running_count = running_count,
      player_bj     = FALSE,
      dealer_bj     = FALSE,
      player_bust   = FALSE,
      dealer_bust   = FALSE,
      surrendered   = TRUE,
      doubled       = pl$doubled
    ))
  }
  
  #Bust
  if (pl$value > 21) {
    bet_mult <- if (isTRUE(pl$doubled)) 2 else 1
    if (verbose) {
      cat(sprintf("PLAYER BUST (total=%d) | RC=%d | gain=%+.1f\n",
                  pl$value, running_count, -bet * bet_mult))
    }
    return(list(
      gain          = -bet * bet_mult,
      shoe          = shoe,
      running_count = running_count,
      player_bj     = FALSE,
      dealer_bj     = FALSE,
      player_bust   = TRUE,
      dealer_bust   = FALSE,
      surrendered   = FALSE,
      doubled       = pl$doubled
    ))
  }
  
  # Delivec odigra
  slice <- shoe_slice_df(shoe)
  dl <- dealer_play(slice, up = up, hole = hole, hit_soft_17 = hit_soft_17)
  shoe <- advance_shoe(shoe, dl$next_idx - 1)
  
  # Hi-Lo: dodatne karte delivca (tretja, četrta, ...)
  if (length(dl$cards) > 2) {
    extra_dealer <- dl$cards[3:length(dl$cards)]
    running_count <- running_count + sum(hi_lo_delta(extra_dealer))
  }
  
  # Rezultat
  pv <- pl$value; dv <- dl$value
  bet_mult <- if (isTRUE(pl$doubled)) 2 else 1
  gain <- if (dv > 21)  bet * bet_mult
  else if (pv > dv)  bet * bet_mult
  else if (pv < dv) -bet * bet_mult
  else 0
  
  if (verbose) {
    cat(sprintf("FINAL: player=%d  dealer=%d  doubled=%s  RC=%d  -> gain=%+.1f\n",
                pv, dv, ifelse(isTRUE(pl$doubled),"TRUE","FALSE"),
                running_count, gain))
  }
  
  list(
    gain          = gain,
    shoe          = shoe,
    running_count = running_count,
    player_bj     = FALSE,
    dealer_bj     = FALSE,
    player_bust   = FALSE,
    dealer_bust   = (dv > 21),
    surrendered   = FALSE,
    doubled       = pl$doubled
  )
}