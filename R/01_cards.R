<<<<<<< HEAD
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

# Hi-Lo štetje: 2-6 = +1, 7-9 = 0, 10/J/Q/K/A = -1
hi_lo_delta <- function(vals) {
  # vals = numeric vektor vrednosti kart (2-11)
  out <- integer(length(vals))
  out[vals >= 2 & vals <= 6] <- 1L
  out[vals == 10 | vals == 11] <- -1L  # 10, J, Q, K, A
  out
=======
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

# Hi-Lo štetje: 2-6 = +1, 7-9 = 0, 10/J/Q/K/A = -1
hi_lo_delta <- function(vals) {
  # vals = numeric vektor vrednosti kart (2-11)
  out <- integer(length(vals))
  out[vals >= 2 & vals <= 6] <- 1L
  out[vals == 10 | vals == 11] <- -1L  # 10, J, Q, K, A
  out
>>>>>>> 0296a7da9f3f19124a1bef82f51fc27adebc5b49
}