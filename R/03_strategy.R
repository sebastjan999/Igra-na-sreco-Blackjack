<<<<<<< HEAD
#Branje CSV tabele basic actions
BS_TABLE <- read.csv("basic_strategy.csv", stringsAsFactors = FALSE)

# Bet spread glede na Hi-Lo true count (kle se lah tut mal igram pa se visje stave delam 12, 16,... bo kasneje k bom metrike zbirou mejbi zanimiv)
bet_spread <- function(true_count) {
  tc <- floor(true_count[1])  # delamo z zaokroženim navzdol---tc je vektor hah
  
  if (tc <= 0) {
    return(1)   # TC <= 0 : minimalna stava
  } else if (tc == 1) {
    return(2)   # TC = 1 : 2 enoti
  } else if (tc == 2) {
    return(4)   # TC = 2 : 4 enote
  } else {
    return(8)   # TC >= 3 : 8 enot
  }
}

#Helper za klasifikacijo roke
classify_hand <- function(vals, can_split = FALSE) {
  v     <- hand_value(vals)
  soft  <- is_soft(vals)
  pair  <- (length(vals) == 2L) && (vals[1] == vals[2]) && can_split
  
  if (pair) {
    list(
      group      = "pair",
      player_total = NA_integer_,
      pair_rank  = vals[1]
    )
  } else if (soft) {
    list(
      group      = "soft",
      player_total = v,
      pair_rank  = NA_integer_
    )
  } else {
    list(
      group      = "hard",
      player_total = v,
      pair_rank  = NA_integer_
    )
  }
}

#DEMO VERSION: kasneje CSV datoteka za basic strategy, popa se advance strategy z Hi-Lo štetjem kart in bet spreadom
basic_action <- function(player_vals, dealer_up, can_double = TRUE, can_split = FALSE) {
  v <- hand_value(player_vals)
  # zelo grob demo:  "Recimo temu nakljucna strategija"
  if (v <= 11 && can_double) return("double")
  if (v <= 16 && dealer_up >= 7) return("hit")
  if (v >= 17) return("stand")
  return("hit")
}

#Osnovna strategija CSV
basic_action_bs <- function(player_vals,
                            dealer_up,
                            can_double = TRUE,
                            can_split  = FALSE,
                            bs_table   = BS_TABLE) {
  # 1) klasifikacija roke
  cl <- classify_hand(player_vals, can_split = can_split)
  
  # 2) izberemo ustrezne vrstice iz tabele
  if (cl$group == "pair") {
    sub <- subset(bs_table,
                  player_group == "pair" &
                    pair_rank    == cl$pair_rank &
                    dealer_up    == dealer_up)
  } else {
    sub <- subset(bs_table,
                  player_group == cl$group &
                    player_total == cl$player_total &
                    dealer_up    == dealer_up)
  }
  
  if (nrow(sub) == 0L) {
    # ce ni nic od tega pa demoverzija kr XD
    v <- hand_value(player_vals)
    if (v <= 11 && can_double) return("double")
    if (v <= 16 && dealer_up >= 7) return("hit")
    if (v >= 17) return("stand")
    return("hit")
  }
  
  code <- sub$action[1]  # predpostavimo, da je ena enolična vrstica
  
  # 3) pretvori kodo v naše stringe ("hit", "stand", "double", ...)
  #    + upoštevaj can_double/can_split
  total <- hand_value(player_vals)
  
  action <- switch(code,
                   "H" = "hit",
                   "S" = "stand",
                   "D" = if (can_double) "double" else "hit",
                   "P" = if (can_split) {
                     "split"
                   } else {
                     if (total >= 17) "stand" else "hit"
                   },
                   "R" = "surrender",
                   "hit"
  )
}

=======
#Branje CSV tabele basic actions
BS_TABLE <- read.csv("basic_strategy.csv", stringsAsFactors = FALSE)

# Bet spread glede na Hi-Lo true count (kle se lah tut mal igram pa se visje stave delam 12, 16,... bo kasneje k bom metrike zbirou mejbi zanimiv)
bet_spread <- function(true_count) {
  tc <- floor(true_count[1])  # delamo z zaokroženim navzdol---tc je vektor hah
  
  if (tc <= 0) {
    return(1)   # TC <= 0 : minimalna stava
  } else if (tc == 1) {
    return(2)   # TC = 1 : 2 enoti
  } else if (tc == 2) {
    return(4)   # TC = 2 : 4 enote
  } else {
    return(8)   # TC >= 3 : 8 enot
  }
}

#Helper za klasifikacijo roke
classify_hand <- function(vals, can_split = FALSE) {
  v     <- hand_value(vals)
  soft  <- is_soft(vals)
  pair  <- (length(vals) == 2L) && (vals[1] == vals[2]) && can_split
  
  if (pair) {
    list(
      group      = "pair",
      player_total = NA_integer_,
      pair_rank  = vals[1]
    )
  } else if (soft) {
    list(
      group      = "soft",
      player_total = v,
      pair_rank  = NA_integer_
    )
  } else {
    list(
      group      = "hard",
      player_total = v,
      pair_rank  = NA_integer_
    )
  }
}

#DEMO VERSION: kasneje CSV datoteka za basic strategy, popa se advance strategy z Hi-Lo štetjem kart in bet spreadom
basic_action <- function(player_vals, dealer_up, can_double = TRUE, can_split = FALSE) {
  v <- hand_value(player_vals)
  # zelo grob demo:  "Recimo temu nakljucna strategija"
  if (v <= 11 && can_double) return("double")
  if (v <= 16 && dealer_up >= 7) return("hit")
  if (v >= 17) return("stand")
  return("hit")
}

#Osnovna strategija CSV
basic_action_bs <- function(player_vals,
                            dealer_up,
                            can_double = TRUE,
                            can_split  = FALSE,
                            bs_table   = BS_TABLE) {
  # 1) klasifikacija roke
  cl <- classify_hand(player_vals, can_split = can_split)
  
  # 2) izberemo ustrezne vrstice iz tabele
  if (cl$group == "pair") {
    sub <- subset(bs_table,
                  player_group == "pair" &
                    pair_rank    == cl$pair_rank &
                    dealer_up    == dealer_up)
  } else {
    sub <- subset(bs_table,
                  player_group == cl$group &
                    player_total == cl$player_total &
                    dealer_up    == dealer_up)
  }
  
  if (nrow(sub) == 0L) {
    # ce ni nic od tega pa demoverzija kr XD
    v <- hand_value(player_vals)
    if (v <= 11 && can_double) return("double")
    if (v <= 16 && dealer_up >= 7) return("hit")
    if (v >= 17) return("stand")
    return("hit")
  }
  
  code <- sub$action[1]  # predpostavimo, da je ena enolična vrstica
  
  # 3) pretvori kodo v naše stringe ("hit", "stand", "double", ...)
  #    + upoštevaj can_double/can_split
  total <- hand_value(player_vals)
  
  action <- switch(code,
                   "H" = "hit",
                   "S" = "stand",
                   "D" = if (can_double) "double" else "hit",
                   "P" = if (can_split) {
                     "split"
                   } else {
                     if (total >= 17) "stand" else "hit"
                   },
                   "R" = "surrender",
                   "hit"
  )
}

>>>>>>> 0296a7da9f3f19124a1bef82f51fc27adebc5b49
