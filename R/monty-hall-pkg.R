#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title Select a Door
#'
#' @description
#'   `select_door()` randomly selects one of the three doors in a
#'   Monty Hall game as the contestant's initial choice.
#'
#' @details
#'   The function creates a numeric vector representing doors 1, 2,
#'   and 3, then randomly samples one door from the vector.
#'
#' @return A numeric value indicating the selected door. The returned
#'   value will be 1, 2, or 3.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title Open a Goat Door
#'
#' @description
#'   `open_goat_door()` selects a door containing a goat for the
#'   host to open after the contestant makes an initial selection.
#'
#' @details
#'   If the contestant initially selects the car, the function
#'   randomly selects one of the two goat doors. If the contestant
#'   initially selects a goat, the function opens the other goat
#'   door so that neither the car nor the contestant's selected
#'   door is opened.
#'
#' @param game A length 3 character vector representing the location
#'   of two goats and one car behind the three doors.
#' @param a.pick A numeric value indicating the contestant's initial
#'   door selection. The value should be 1, 2, or 3.
#'
#' @return A numeric value indicating the goat door opened by the
#'   host. The returned value will be 1, 2, or 3.
#'
#' @examples
#'   game <- c("goat", "car", "goat")
#'   open_goat_door(game, 1)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title Stay With or Switch Doors
#'
#' @description
#'   `change_door()` determines the contestant's final door selection
#'   based on whether the contestant chooses to stay or switch.
#'
#' @details
#'   If `stay` is TRUE, the contestant keeps the original door
#'   selection. If `stay` is FALSE, the function selects the remaining
#'   unopened door that was neither the contestant's initial choice
#'   nor the door opened by the host.
#'
#' @param stay A logical value indicating whether the contestant stays
#'   with the original selection. TRUE means stay and FALSE means switch.
#' @param opened.door A numeric value indicating the door opened by
#'   the host. The value should be 1, 2, or 3.
#' @param a.pick A numeric value indicating the contestant's initial
#'   door selection. The value should be 1, 2, or 3.
#'
#' @return A numeric value indicating the contestant's final door
#'   selection. The returned value will be 1, 2, or 3.
#'
#' @examples
#'   change_door(stay = TRUE, opened.door = 2, a.pick = 1)
#'   change_door(stay = FALSE, opened.door = 2, a.pick = 1)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title Determine the Game Winner
#'
#' @description
#'   `determine_winner()` determines whether the contestant wins or
#'   loses the Monty Hall game based on the final door selection.
#'
#' @details
#'   The function checks the prize located behind the contestant's
#'   final selected door. If the selected door contains the car, the
#'   contestant wins. If the selected door contains a goat, the
#'   contestant loses.
#'
#' @param final.pick A numeric value indicating the contestant's final
#'   door selection. The value should be 1, 2, or 3.
#' @param game A length 3 character vector representing the location
#'   of two goats and one car behind the three doors.
#'
#' @return A character value of `"WIN"` if the final selected door
#'   contains the car or `"LOSE"` if it contains a goat.
#'
#' @examples
#'   game <- c("goat", "car", "goat")
#'   determine_winner(final.pick = 2, game = game)
#'   determine_winner(final.pick = 1, game = game)
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title Play One Monty Hall Game
#'
#' @description
#'   `play_game()` plays one complete Monty Hall game and compares
#'   the outcomes of staying with the initial selection and switching
#'   to the remaining unopened door.
#'
#' @details
#'   The function creates a new game, randomly selects an initial door,
#'   opens a goat door, and determines the final selection under both
#'   the stay and switch strategies. It then determines whether each
#'   strategy results in a win or loss.
#'
#' @return A data frame with two rows and two columns. The `strategy`
#'   column identifies the `"stay"` and `"switch"` strategies, and the
#'   `outcome` column reports `"WIN"` or `"LOSE"` for each strategy.
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title Play Multiple Monty Hall Games
#'
#' @description
#'   `play_n_games()` simulates multiple Monty Hall games and collects
#'   the outcomes of the stay and switch strategies.
#'
#' @details
#'   The function repeatedly calls `play_game()` for the number of games
#'   specified by `n`. Results from each game are collected and combined
#'   into a single data frame. The function also prints a table showing
#'   the proportion of wins and losses for each strategy.
#'
#' @param n A numeric value indicating the number of Monty Hall games
#'   to simulate. The default is 100.
#'
#' @return A data frame containing the results of all simulated games.
#'   The `strategy` column identifies the `"stay"` or `"switch"` strategy,
#'   and the `outcome` column reports `"WIN"` or `"LOSE"`. Each simulated
#'   game contributes two rows to the returned data frame.
#'
#' @examples
#'   play_n_games(n = 100)
#'
#' @import dplyr
#' @export
play_n_games <- function( n=100 )
{

  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  print(
    round(
      prop.table(table(results.df), margin = 1),
      2
    )
  )

  return( results.df )

}
