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