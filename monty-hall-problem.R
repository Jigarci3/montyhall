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
#' @param ... no arguments are used by the function.
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



#' @title
#'	Contestant selects a door
#' @description
#'	the 'select_door()' function selects one door at random
#' @details
#'	This function replicates the randome selection a real person 		might make when playing this game. They have three doors to 		choose from.
#' @param 
#'	... no arguments are used for this function
#' @return 
#'	... a random door will be selected
#' @examples
#'	doors<- c(1,2,3)
#'	a.pick<- sample( doors, size=1)
#'	return(a.pick)
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'	Host opens a goat door
#' @description
#'	After the contestant has chosen a door, this function will open up the remaining goat door.
#' @details
#'	The remaining goat door that is not the same as the contestants
#' initial selection, will be opened.
#' @param 
#'	If the contestant initially selected a goat door, then the other 	door will be opened. If the contestant selected the car door, 		then one of the two goat doors will be opened.
#' @return
#'	the remaining goat door will be opened or either one of the two
#'	goat doors will be opened. 
#' @examples
#'	If the doors are as follows (goat, goat, car) and the initiaql 
#'	selection is door #1, then door # 2 will be opened
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



#' @title
#'	Contestant is give the opportunity to change doors
#' @description
#'	Once the goat door has been opened, the contestant has the 
#'	oppotunity to keep their initial selection or change to the other
#'	remaining door
#' @details
#'	the 'change_doors()' function will represent the game-playing 
#'	strategy as the argument stay=true or stay=false
#' @param 
#'	if the contestant selects to stay then stay=true
#'	if the contestant selects to switch then stay=false
#' @return 
#' the function will return the same door or will return the other remaining door
#' @examples
#'	change_door()
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



#' @title
#' The outcome of contestant as winner or loser is determined
#' @description
#' Depending on the final pick, the game will determine if the selected door contains a goat or a car
#' @details
#' 	If the final pick after given the option to stay or change doors 
#' is a car then the contestant has won the game. If not, then the contestant has lost the game
#' @param 
#'	if selected door is a car, contestant wins. If remaining door is
#' goat then contestant lost
#' @return 
#'	win or lose
#' @examples
#'	determine_winner()
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





#' @title
#' 	Wrap is created for the game is played from start to finish
#' @description
#' 	A new game is created and all steps are completed to determine if
#'	contestant has won or lost
#' @details
#'	Game is created, a door is selected, a goat door is revealed, the #'	contestant has the option to stay with their initial pick or 
#'	switch to remaining closed door. Depending on that selection, 
#'	game determines whether or not the contestant has won or lost.
#' @param 
#'	If contestant choses to switch their door during the change_door
#'	function, then remainng closed door becomes their selection
#'	If final pick contains a car, then contestant has won
#' @return 
#'	final outcome is 'win' or 'lost'
#' @examples
#'	play_game()
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






#' @title
#'	A loop is created to simulate playing the game for a determined
#'	amount of times.
#' @description
#'	The loop will run the game simulation as many times as specified
#' @details
#'	Loop will run the game simulation
#' @param 
#' arguments specified inside the wrap will be applied
#' @return 
#'	a total number of wins and losses will be determined
#' @examples
#'	play_n_games()
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
