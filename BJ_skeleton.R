# 0) Osnova: karte, vrednosti, vrednost roke -----------------------------
#Za hrajenje n kupčkov kart
make_deck <- function(n_decks = 6) {
  ranks <- c(2:10, "J","Q","K","A")
  vals  <- c(2:10, rep(10,3), 11)
  deck  <- rep(ranks, each = 4 * n_decks)
  data.frame(rank = deck, 
             val  = rep(vals, each = 4 * n_decks),
             stringsAsFactors = FALSE)
}

#vrednost roke
hand_value <- function(vals) {
  total <- sum(vals)
  aces  <- sum(vals == 11)
  while (total > 21 && aces > 0) {
    total <- total - 10
    aces  <- aces - 1
  }
  total
}

#T/F ali roka soft
is_soft <- function(vals) {
  total <- sum(vals);
  aces <- sum(vals == 11)
  while (total > 21 && aces > 0) { total <- total - 10; aces <- aces - 1 }
  aces > 0
}

#T/F ali imamo iz prve BJ (AS + value 10 karta)
is_blackjack <- function(vals) {
  length(vals) == 2 && (11 %in% vals) && (10 %in% vals)
}

# =========================================================
# A) SHOE: init, slice, advance, reshuffle (penetration)
# =========================================================

init_shoe <- function(n_decks = 6, penetration = 0.75) {
  stopifnot(penetration > 0, penetration < 1) #pac penetrartion more bit v (0,1)
  df <- make_deck(n_decks) #nardimo n deckov
  df <- df[sample(nrow(df)), ] #nakljucno premesamo dek
  total <- nrow(df) #skupno st kart
  cut   <- floor(total * penetration) #pri kaksnem delezu porabljenih kart (penetrartion npr 75%) zelimo reshuffle
  list(df = df, pos = 1L, total = total, cut = cut,
       n_decks = n_decks, penetration = penetration) #vrnemo seznam
}

#vrnemo df s preostankom deka od trenutne pozicije dalje
shoe_slice_df <- function(shoe) {
  if (shoe$pos > shoe$total) return(shoe$df[0, , drop = FALSE])
  shoe$df[shoe$pos:shoe$total, , drop = FALSE]
}

#premaknemoc kazalec "pos" za k kart naprej
advance_shoe <- function(shoe, k) {
  shoe$pos <- shoe$pos + k
  shoe
}

#preveri ce smo ze presegl cut/penetration mejo (uporaba ce T reshuffle ce F pol pa nc ne nardi)
maybe_reshuffle <- function(shoe) {
  if (shoe$pos > shoe$cut) {
    df <- make_deck(shoe$n_decks)
    df <- df[sample(nrow(df)), ]
    shoe$df <- df
    shoe$total <- nrow(df)
    shoe$pos <- 1L
    shoe$cut <- floor(shoe$total * shoe$penetration)
    shoe$reshuffled <- TRUE
  } else {
    shoe$reshuffled <- FALSE
  }
  shoe
}

# 1) Pravila delivca (S17/H17) ---------------------------------------
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

# 2) Osnovna strategija (poenostavljen lookup) --------------------------_
#DEMO VERSION: kasneje CSV datoteka za basic strategy, popa se advance strategy z Hi-Lo štetjem kart in bet spreadom
basic_action <- function(player_vals, dealer_up, can_double = TRUE, can_split = FALSE) {
  v <- hand_value(player_vals)
  # zelo grob demo:
  if (v <= 11 && can_double) return("double")
  if (v <= 16 && dealer_up >= 7) return("hit")
  if (v >= 17) return("stand")
  return("hit")
}

# 3) Igralčev potek roke (brez splitov, demo) ----------------------
play_player <- function(shoe, up_card, strategy = basic_action) {
  idx <- 1 #stevec naslednje karte v kupcku
  player <- c(shoe$val[idx], shoe$val[idx+1]); idx <- idx + 2 #damo mu 2 karti iz dekq
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

# =========================================================
# 4) ENA IGRA (roka)
#    - vključen blackjack check (3:2 ali 6:5 preko payout_bj)
#    - upošteva double (2x stava)
# =========================================================

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

# =========================================================
# B) ENA IGRA IZ OBSTOJEČEGA SHOE (z BJ in double)
# =========================================================
deal_hand_from_shoe <- function(shoe, hit_soft_17 = FALSE, bet = 1, payout_bj = 1.5) {
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
  if (is_blackjack(player_start) || is_blackjack(dealer_start)) {
    if (is_blackjack(player_start) && is_blackjack(dealer_start)) return(list(gain = 0, shoe = shoe))
    if (is_blackjack(player_start)) return(list(gain =  bet * payout_bj, shoe = shoe))
    if (is_blackjack(dealer_start)) return(list(gain = -bet,            shoe = shoe))
  }
  
  # Igralec odigra (iz trenutnega shoe)
  slice <- shoe_slice_df(shoe)
  pl <- play_player(  #pl$value (končna vsota), pl$cards (karte igralca),pl$doubled (TRUE/FALSE),pl$next_idx (koliko kart iz slice je porabil). )
    shoe = slice,
    up_card = up,
    strategy = function(hand, upc, can_double = TRUE, ...) {
      basic_action(hand, upc, can_double = (length(hand) == 2))
    }
  )
  shoe <- advance_shoe(shoe, pl$next_idx - 1)
  
  # Če bust, takoj vrni
  if (pl$value > 21) {
    bet_mult <- if (isTRUE(pl$doubled)) 2 else 1
    return(list(gain = -bet * bet_mult, shoe = shoe))
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
  
  list(gain = gain, shoe = shoe)
}

#kaj naredimo torej:
#vzemi slice → če premau kart, reshuffle + spet slice

#razdeli P1, D_up, P2, D_hole → advance_shoe(..., 4)

#BJ check → mejbi return(...)

#player igra iz slice → advance_shoe(..., pl$next_idx - 1)

#če bust → return takoj

#dealer igra iz slice → advance_shoe(..., dl$next_idx - 1)

#primerjava in gain (+ bet_mult za double) → vrni list(gain, shoe)

#OK zgleda ql :)

# 5) Monte Carlo ------------------------------------------------------
#(EV, SE, IZ, dobicki)

simulate_n <- function(N = 1e5, ...) {
  gains <- numeric(N)
  for (i in seq_len(N)) gains[i] <- simulate_hand(...)
  mu <- mean(gains); s <- sd(gains); se <- s/sqrt(N)
  ci <- c(mu - 1.96*se, mu + 1.96*se)
  list(EV = mu, SE = se, CI95 = ci, gains = gains)
}

# =========================================================
# C) MONTE CARLO preko istega SHOE (penetration + reshuffle)
# =========================================================
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
#__________________________________________________________________________

#test
set.seed(121)
out <- replicate(1000, simulate_hand(n_decks = 6, hit_soft_17 = FALSE))
out
table(out)

set.seed(121)
out <- replicate(1000, simulate_hand(n_decks = 6, hit_soft_17 = TRUE))
out
table(out)

#___________________________________________________________________________
#Testi

set.seed(121)
res <- simulate_n(N = 1000, n_decks = 6, hit_soft_17 = FALSE)
res$EV      # ocenjen pričakovani dobiček na roko
res$SE      # standardna napaka
res$CI95    # 95% interval zaupanja za EV

set.seed(121)
res <- simulate_n(N = 1000, n_decks = 6, hit_soft_17 = TRUE)
res$EV      # ocenjen pričakovani dobiček na roko
res$SE      # standardna napaka
res$CI95    # 95% interval zaupanja za EV

set.seed(2025)
# 1 roka iz shoe
S <- init_shoe(n_decks = 6, penetration = 0.75)
h1 <- deal_hand_from_shoe(S, hit_soft_17 = FALSE); h1$gain; h1$shoe$pos

# serija z reshuffle
set.seed(2025)
res <- simulate_with_shoe(N = 1000, n_decks = 6, penetration = 0.75, hit_soft_17 = FALSE)
res$EV; res$CI95
