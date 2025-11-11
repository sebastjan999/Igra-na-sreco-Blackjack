# 0) Osnova: karte, vrednosti, vrednost roke -----------------------------
make_deck <- function(n_decks = 6) {
  ranks <- c(2:10, "J","Q","K","A")
  vals  <- c(2:10, rep(10,3), 11)          # A kot 11, kasneje prilagodimo
  deck  <- rep(ranks, each = 4 * n_decks)
  data.frame(rank = deck, 
             val  = rep(vals, each = 4 * n_decks),
             stringsAsFactors = FALSE)
}

hand_value <- function(vals) {
  # vals = vektor kartnih vrednosti z asi kot 11
  total <- sum(vals)
  aces  <- sum(vals == 11)
  while (total > 21 && aces > 0) {
    total <- total - 10
    aces  <- aces - 1
  }
  total
}

is_soft <- function(vals) {
  total <- sum(vals);
  aces <- sum(vals == 11)
  # znižuj ase iz 11 -> 1, dokler ne neha bustati
  while (total > 21 && aces > 0) { total <- total - 10; aces <- aces - 1 }
  # če je ostal vsaj en as, ki še šteje 11, je roka soft
  aces > 0
}

is_blackjack <- function(vals) {
  length(vals) == 2 && (11 %in% vals) && (10 %in% vals)
}

# 1) Pravila delivca (S17/H17) ---------------------------------------

#naprej nardim samo za S17...popa dodam H17
#(shoe = make_deck(n_decks), popa rabmo met in mind se delivcev hand, aka. up pa hole karti)
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
    # sicer vleci in povecaj i za 1(da bo ready nasledna karta)
    hand <- c(hand, shoe$val[i]); i <- i + 1
  }
  list(value = hand_value(hand), cards = hand, next_idx = i)
}

# 2) Osnovna strategija (poenostavljen lookup) --------------------------_
# Tuki lahk pol dam CSV datoteko k je itk ze predetermined, na kere vrednosti + delaer card se hita i kaj ne(internet).
#ampak najprj dejmo sam neki nahitr tok da bo delal, pa pol na tem buildam naprej, also s spilt se prolly nam zajebavu v osnovni strategiji
#lets call it DEMO VERSION XD
basic_action <- function(player_vals, dealer_up, can_double = TRUE, can_split = FALSE) {
  v <- hand_value(player_vals)
  # zelo grob demo:
  if (v <= 11 && can_double) return("double")
  if (v <= 16 && dealer_up >= 7) return("hit")  #omg dealer je ze tko al tko numeric po seb haha
  if (v >= 17) return("stand")
  "hit"
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
#!!! popravi realisticn vrstni red deljenja kart, zdj je 2 delaer 2 player, mogl bi bit pa igralc, dealer, igralc dealer
#sj simpl, p1=shoe$val[1], up =shoe$val[2],.....
# =========================================================

#bj payout nastavmo na 3:2 (kksni casinoji majo sicer slabs 6:5...kasneje za metrike pa HE bomo mal spreminjal)
simulate_hand <- function(n_decks = 6, hit_soft_17 = FALSE, bet = 1, payout_bj = 1.5) {
  shoe <- make_deck(n_decks)
  shoe <- shoe[sample(nrow(shoe)),]  # premešamo
  
  # deal dealer
  up   <- shoe$val[1]
  hole <- shoe$val[2]
  idx <- 3
  
  # deal player (dve karti)
  p1 <- shoe$val[idx]; p2 <- shoe$val[idx+1]; idx <- idx + 2
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
# 5) Monte Carlo ------------------------------------------------------
#(EV, SE, IZ)

simulate_n <- function(N = 1e5, ...) {
  gains <- numeric(N)
  for (i in seq_len(N)) gains[i] <- simulate_hand(...)
  mu <- mean(gains); s <- sd(gains); se <- s/sqrt(N)
  ci <- c(mu - 1.96*se, mu + 1.96*se)
  list(EV = mu, SE = se, CI95 = ci, gains = gains)
}














#__________________________________________________________________________

#test
set.seed(121)
out <- replicate(10, simulate_hand(n_decks = 6, hit_soft_17 = FALSE))
out
table(out)

set.seed(121)
out <- replicate(10, simulate_hand(n_decks = 6, hit_soft_17 = TRUE))
out
table(out)

set.seed(1111111)
out <- replicate(10, simulate_hand(n_decks = 6, hit_soft_17 = FALSE))
out
table(out)

set.seed(1111111)
out <- replicate(10, simulate_hand(n_decks = 6, hit_soft_17 = TRUE))
out
table(out)
#___________________________________________________________________________


set.seed(123)
res <- simulate_n(N = 100, n_decks = 6, hit_soft_17 = FALSE)
res$EV      # ocenjen pričakovani dobiček na roko
res$SE      # standardna napaka
res$CI95    # 95% interval zaupanja za EV

set.seed(123)
res <- simulate_n(N = 100, n_decks = 6, hit_soft_17 = TRUE)
res$EV      # ocenjen pričakovani dobiček na roko
res$SE      # standardna napaka
res$CI95    # 95% interval zaupanja za EV