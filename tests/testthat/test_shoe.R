test_that("init_shoe ustvari pravilen shoe", {
  shoe <- init_shoe(n_decks = 6, penetration = 0.75)
  
  expect_equal(shoe$total, 6L * 52L)
  expect_true(shoe$cut > 0)
  expect_true(shoe$cut <= shoe$total)
  expect_equal(shoe$pos, 1L)
})

test_that("maybe_reshuffle resetira shoe pri preseženi penetraciji", {
  shoe <- init_shoe(n_decks = 1, penetration = 0.2)
  # ročno nastavimo pos tako, da ga presežemo
  shoe$pos <- shoe$cut + 1L
  
  shoe2 <- maybe_reshuffle(shoe)
  expect_true(isTRUE(shoe2$reshuffled))
  expect_equal(shoe2$pos, 1L)
})
