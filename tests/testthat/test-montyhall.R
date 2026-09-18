test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("create_game creates a valid Monty Hall game", {

  game <- create_game()

  expect_length(game, 3)
  expect_equal(sum(game == "goat"), 2)
  expect_equal(sum(game == "car"), 1)

})

test_that("select_door selects a valid door", {

  door <- select_door()

  expect_length(door, 1)
  expect_true(door %in% c(1, 2, 3))

})

test_that("open_goat_door opens a valid goat door", {

  game <- c("goat", "car", "goat")

  # Contestant selects a goat
  opened <- open_goat_door(game, 1)
  expect_equal(opened, 3)

  # Contestant selects the car
  opened <- open_goat_door(game, 2)
  expect_true(opened %in% c(1, 3))
  expect_equal(game[opened], "goat")

})

test_that("change_door correctly applies stay and switch strategies", {

  # Stay with original selection
  stay.pick <- change_door(
    stay = TRUE,
    opened.door = 3,
    a.pick = 1
  )

  expect_equal(stay.pick, 1)

  # Switch to remaining unopened door
  switch.pick <- change_door(
    stay = FALSE,
    opened.door = 3,
    a.pick = 1
  )

  expect_equal(switch.pick, 2)

})

test_that("determine_winner correctly identifies wins and losses", {

  game <- c("goat", "car", "goat")

  # Selecting the car results in a win
  expect_equal(
    determine_winner(final.pick = 2, game = game),
    "WIN"
  )

  # Selecting a goat results in a loss
  expect_equal(
    determine_winner(final.pick = 1, game = game),
    "LOSE"
  )

})

test_that("play_game returns valid results for both strategies", {

  results <- play_game()

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 2)
  expect_equal(ncol(results), 2)
  expect_equal(names(results), c("strategy", "outcome"))
  expect_equal(results$strategy, c("stay", "switch"))
  expect_true(all(results$outcome %in% c("WIN", "LOSE")))

})

test_that("play_n_games returns results for the requested number of games", {

  results <- play_n_games(n = 10)

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 20)
  expect_equal(ncol(results), 2)
  expect_equal(names(results), c("strategy", "outcome"))

  expect_equal(sum(results$strategy == "stay"), 10)
  expect_equal(sum(results$strategy == "switch"), 10)

  expect_true(all(results$outcome %in% c("WIN", "LOSE")))

})
