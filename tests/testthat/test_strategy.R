test_that("basic_action_bs daje smiselne akcije za roke po S17 strategiji", {
  # predpostavljam, da je BS_TABLE_CURRENT pravilno nastavljena v main.R
  BS_TABLE_CURRENT <<- BS_TABLE_S17
  
  # klasični primeri iz tabele
  expect_equal(
    basic_action_bs(c(10, 6), dealer_up = 8, can_double = TRUE, can_split = FALSE),
    "hit"
  )
  
  expect_equal(
    basic_action_bs(c(10, 6), dealer_up = 2, can_double = TRUE, can_split = FALSE),
    "stand"
  )
  
  expect_equal(
    basic_action_bs(c(11, 2), dealer_up = 6, can_double = TRUE, can_split = FALSE),
    "double"
  )
  
  expect_equal(
    basic_action_bs(c(11, 2), dealer_up = 6, can_double = FALSE, can_split = FALSE),
    "hit"
  )
  
  expect_equal(
    basic_action_bs(c(11, 7), dealer_up = 3, can_double = TRUE, can_split = FALSE),
    "double"
  )
  
  expect_equal(
    basic_action_bs(c(11, 7), dealer_up = 3, can_double = FALSE, can_split = FALSE),
    "stand"
  )
  
  expect_equal(
    basic_action_bs(c(3, 3), dealer_up = 3, can_double = FALSE, can_split = TRUE),
    "split"
  )
  
  expect_equal(
    basic_action_bs(c(3, 3), dealer_up = 3, can_double = FALSE, can_split = FALSE),
    "hit"
  )
  
  expect_equal(
    basic_action_bs(c(9, 9), dealer_up = 3, can_double = FALSE, can_split = FALSE),
    "stand"
  )
  
  # surrender, če ga dovolimo
  act_R <- basic_action_bs(c(10, 6), dealer_up = 11,
                           can_double = TRUE, can_split = FALSE, can_surrender = TRUE)
  expect_true(act_R %in% c("surrender"))  
  
  # če can_surrender = FALSE, ne smemo nikoli dobiti "surrender"
  act_no_R <- basic_action_bs(c(10, 6), dealer_up = 11,
                              can_double = TRUE, can_split = FALSE, can_surrender = FALSE)
  expect_false(act_no_R == "surrender")
})

#----------------------------------------------------------------------------------------------

test_that("basic_action_bs daje smiselne akcije za roke po H17 strategiji", {
  # predpostavljam, da je BS_TABLE_CURRENT pravilno nastavljena v main.R
  BS_TABLE_CURRENT <<- BS_TABLE_H17
  
  # klasični primeri iz tabele
  expect_equal(
    basic_action_bs(c(10, 6), dealer_up = 8, can_double = TRUE, can_split = FALSE),
    "hit"
  )
  
  expect_equal(
    basic_action_bs(c(10, 6), dealer_up = 2, can_double = TRUE, can_split = FALSE),
    "stand"
  )
  
  expect_equal(
    basic_action_bs(c(9, 2), dealer_up = 6, can_double = TRUE, can_split = FALSE),
    "double"
  )
  
  expect_equal(
    basic_action_bs(c(9, 2), dealer_up = 6, can_double = FALSE, can_split = FALSE),
    "hit"
  )
  
  expect_equal(
    basic_action_bs(c(11, 8), dealer_up = 6, can_double = TRUE, can_split = FALSE),
    "double"
  )
  
  expect_equal(
    basic_action_bs(c(11, 8), dealer_up = 6, can_double = FALSE, can_split = FALSE),
    "stand"
  )
  
  expect_equal(
    basic_action_bs(c(3, 3), dealer_up = 3, can_double = FALSE, can_split = TRUE),
    "split"
  )
  
  expect_equal(
    basic_action_bs(c(3, 3), dealer_up = 3, can_double = FALSE, can_split = FALSE),
    "hit"
  )
  
  expect_equal(
    basic_action_bs(c(9, 9), dealer_up = 3, can_double = FALSE, can_split = FALSE),
    "stand"
  )
  
  # surrender, če ga dovolimo
  act_R <- basic_action_bs(c(10, 6), dealer_up = 11,
                           can_double = TRUE, can_split = FALSE, can_surrender = TRUE)
  expect_true(act_R %in% c("surrender")) 
  
  # če can_surrender = FALSE, ne smemo nikoli dobiti "surrender"
  act_no_R <- basic_action_bs(c(10, 6), dealer_up = 11,
                              can_double = TRUE, can_split = FALSE, can_surrender = FALSE)
  expect_false(act_no_R == "surrender")
})
