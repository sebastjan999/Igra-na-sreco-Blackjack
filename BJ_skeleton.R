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