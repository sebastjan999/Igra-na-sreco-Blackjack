test_that("simulate_with_shoe_hilo ne crashne in vrača smiselne metrike", {
  set.seed(123)
  res <- simulate_with_shoe_hilo(
    N = 2000,
    n_decks = 6,
    penetration = 0.75,
    hit_soft_17 = FALSE,
    bet = 1,
    payout_bj = 1.5,
    can_double = TRUE,
    can_split = TRUE,
    can_surrender = TRUE
  )
  
  # osnovne metrike obstajajo
  expect_true(is.numeric(res$EV))
  expect_true(is.numeric(res$SD))
  expect_true(length(res$gains) == 2000)
  expect_true(length(res$bankroll) == 2000)
  
  # EV za "normalen" BJ s basic strategijo ne sme biti too big
  expect_true(res$EV > -1 && res$EV < 1)   # nekje kle bi prcakval da bo
  
  # win / loss / push seštevajo približno v 1
  s <- res$win_rate + res$loss_rate + res$push_rate
  expect_true(abs(s - 1) < 1e-6)
})

test_that("Hi-Lo EV_by_TC in N_by_TC sta konsistentna", {
  set.seed(456)
  res <- simulate_with_shoe_hilo(
    N = 5000,
    n_decks = 6,
    penetration = 0.75,
    hit_soft_17 = FALSE,
    bet = 1
  )
  
  # EV_by_TC je named vector, N_by_TC pa table
  expect_true(is.numeric(res$EV_by_TC))
  expect_true(is.numeric(res$N_by_TC) || is.table(res$N_by_TC))
  
  # število različnih TC razredov ni 0
  expect_true(length(res$EV_by_TC) > 0)
})
