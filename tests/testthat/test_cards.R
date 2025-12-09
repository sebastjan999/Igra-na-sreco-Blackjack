test_that("make_deck ustvari pravilen komplet kart", {
  deck1 <- make_deck(1)
  expect_equal(nrow(deck1), 52L)
  expect_true(all(c("val") %in% names(deck1)))
  
  deck6 <- make_deck(6)
  expect_equal(nrow(deck6), 6L * 52L)
})

test_that("hand_value pravilno računa vrednosti", {
  expect_equal(hand_value(c(10, 5)), 15)
  expect_equal(hand_value(c(10, 10)), 20)
  expect_equal(hand_value(c(11, 10)), 21)   # A + 10 -> 21
  expect_equal(hand_value(c(11, 11)), 12)   # A + A -> 12
  expect_equal(hand_value(c(11, 5, 10)), 16) # A,5,10 -> 16 (11+5+10=26 -> A kot 1)
})

test_that("hi_lo_delta daje pravilne vrednosti", {
  # predpostavljam, da val = 2..11 (11 = as)
  expect_equal(hi_lo_delta(2:6), rep(1L, 5)) #2-6 daje RC 1
  expect_equal(hi_lo_delta(7:9), rep(0L, 3)) #7-9 daje RC 0
  expect_equal(hi_lo_delta(c(10, 11)), c(-1L, -1L)) #10 in As daje TC -1
})
