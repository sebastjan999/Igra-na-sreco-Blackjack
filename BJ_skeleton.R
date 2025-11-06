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

# 1) Pravila delivca (S17/H17) ---------------------------------------
#is_soft <- function(vals){}

#naprej nardim samo za H17...popa dodam S17
#(shie = make_deck(n_decks), popa rabmo met in mind se delivcev hand, aka. up pa hole karti)
dealer_play <- function(shoe, up, hole, hit_soft_17 = FALSE) {
  hand <- c(up, hole)
  i <- 1
  repeat {
    v <- hand_value(hand)
    #soft <- is_soft(hand)
    if (v > 21) break
    # S17: stoj na kateremkoli 17 (tudi soft)
    # H17: vleci pri soft 17, stoj pri hard 17
    if (v > 17) break
    if (v == 17 && !hit_soft_17) break
    #if (v == 17 && hit_soft_17 && !soft) break
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
  repeat {
    a <- strategy(player, up_card) #strategija kle je pac una basic demo zaenkrat
    action_hist <- c(action_hist, a)
    if (a == "stand") break
    if (a == "double") { player <- c(player, shoe$val[idx]); idx <- idx + 1; break }
    if (a == "hit")    { player <- c(player, shoe$val[idx]); idx <- idx + 1 }
  }
  list(value = hand_value(player), cards = player, next_idx = idx, actions = action_hist)
}

# 4) Simulacija ene roke ---------------------------------------
#bj payout nastavmo na 3:2 (kksni casinoji majo sicer slabs 6:5...kasneje za metrike pa HE bomo mal spreminjal)
simulate_hand <- function(n_decks = 6, hit_soft_17 = FALSE, bet = 1, payout_bj = 1.5) {
  shoe <- make_deck(n_decks)
  shoe <- shoe[sample(nrow(shoe)),]  # premešamo
  
  # deal
  up   <- shoe$val[1]
  hole <- shoe$val[2]
  idx <- 3
  
  # player
  pl <- play_player(shoe[idx:nrow(shoe), ], up)
  idx <- idx + pl$next_idx - 1
  
  # dealer
  dl <- dealer_play(shoe[idx:nrow(shoe), ], up, hole, hit_soft_17)
  idx <- idx + dl$next_idx - 1
  
  # izid
  pv <- pl$value; dv <- dl$value
  res <- if (pv > 21) -bet else if (dv > 21) bet else if (pv > dv) bet else if (pv < dv) -bet else 0
  res
}

#__________________________________________________________________________
#test
set.seed(121)
out <- replicate(10, simulate_hand(n_decks = 6, hit_soft_17 = FALSE))
out
table(out)

set.seed(1)
out <- replicate(10, simulate_hand(n_decks = 6, hit_soft_17 = FALSE))
out
table(out)
#___________________________________________________________________________
