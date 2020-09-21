#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   The 'create_game' function generates a new game that consists of two doors 
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
#' @return 
#'	The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( game )
} 



#' @title
#'	Choose a door for the Goat Game. 
#' @description
#'	The 'select_door' function provides the initial door choice for the 
#'	Goat Game. 
#'	
#' @details
#'	This is a simulation of when a contestant choooses  a door. 
#'	In the real Goat Game, a human would be given the choice between 
#'	3 doors. The person will choose either 1, 2, or 3. This first 
#'	choice is done with no knowledge of what is behind the doors.
#'
#' @param ... no arguments are used by the function. 
#'	 
#' @return 
#'	The function returns a length 1 numeric vector to indicate 
#'	which door was choosen. It will either be 1, 2, or 3.
#'	 
#' @examples
#'	select_door()
#'	
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'	Open a door and expose a goat. 
#'
#' @description
#'	'open_goat_door' function reveals that a goat is behind one of 
#'	the doors, but not the previous door chosen from 'select_door' function. 
#'	
#' @details
#'	After the contestant chooses a door, now the host of the 
#'	game will expose a goat behind a different door. The 3 
#'	length character variable 'game' holds the randomized 
#'	order of 2 goats and 1 car. 'open_goat_door' function needs 2 inputs 
#'	to know which doors not to open and which one to reveal. The function 
#'	takes in the first choice 'a.pick' and makes sure to
#'	not expose that door, even if there is a goat behind the 
#'	contestant's choice. Now, with the goat door exposed, the 
#'	contestant has knowledge about what is behind 1 of the doors. 
#'	
#' @param 
#'	The variables 'game' and 'a.pick' are inserted into the 
#'	'open_goat_door' function. 
#'
#' @return 
#'	The function returns a length 1 numeric vector to indicate 
#'	a door with a goat behind it. It will either be 1, 2, or 3. 
#'	The chosen door will not be the same door from 'select_door' function. 
#'
#' @examples
#'	open_goat_door( game, a.pick)
#'	open_goat_door( game= '"goat", "car", "goat"', a.pick = 1)
#'	open_goat_door( '"car", "goat", "goat"' , 2)
#'	open_goat_door( '"goat", "goat", "car"' , 3)
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



#' @title
#'	Choice to stay with first choice or switch doors. 
#'	
#' @description
#'	Can either stay with original door 'a.pick', or switch 
#'	to the final door not exposed during 'open_goat_door' function. 
#'	
#' @details
#'	This function provides a choice to the player, to either
#'	stay with their first pick 'a.pick', or to switch to 
#'	the the other door. By this point in the game the host 
#'	has already revealed a goat door. The logic behind 
#'	'change_door' function is to either 'stay' and maintain 'a.pick', 
#'	or to choose the door that is not 'a.pick' and also not 
#'	the revealed 'opened.door' with a goat. Whichever door is 
#'	chosen will be forwarded into the function as 'final.pick'. 
#'	The variable 'stay' can either be T or F, which is TRUE or
#'	FALSE. T is default, where the 'final.pick' will stay with 
#'	the original choice of 'a.pick'. F would change the door
#'	for 'final.pick'. 
#'	
#'	
#' @param 
#'	The variables 'stay, 'a.pick', and 'opened.door' are inserted 
#'	into the 'change_door' function. 
#'	
#' @return 
#'	The function returns a length 1 numeric vector to indicate 
#'	a final door pick to finish the game. It will either 
#'	be 1, 2, or 3. 
#'	
#' @examples
#'	change_door( stay=T, opened.door=2 , a.pick=3 )
#'	change_door(F, 3, 1)
#'	change_door(T, 1, 3)
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



#' @title
#'	The contestant's door is opened to reveal what is behind. 
#'
#' @description
#'	The game ends with a reveal. Behind the final chosen door 
#'	will either be a goat or a car. A car is a "WIN" and a 
#'	goat is a "LOSE". 
#'
#' @details
#'	All of the previous functions performed the game 
#'	logic. The final 'determine_winner' function 
#'	matches the 'final.pick' to the 'game' variable. The 'game'
#'	parameter was randomly assigned at the beginning of the 
#'	game at the 'create_game' function. Because 'game' is
#'	an index of 3 strings, the 'final.pick' number 1-3 will
#'	be the chosen index character of 'game'. If the index 
#'	matches to a "car" string, then the contestant earns a 
#'	"WIN". If the index  matches to a "goat" string, then 
#'	the contestant earns a "LOSE". 
#'
#' @param 
#'	The variables 'final.pick' and 'game' are inserted 
#'	into the 'determine_winner' function. 
#'
#' @return 
#'	The function returns a string that either states "WIN"
#'	or "LOSE". 
#'
#' @examples
#'	determine_winner( final.pick= 2, game= '"car", "goat", "goat"')
#'	determine_winner( 1, '"goat", "goat", "cat"')
#'	determine_winner ( 3, '"goat", "car", "goat"')
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




#' @title
#'	The entire Goat Game in a single function from start
#'	to finish.
#'
#' @description
#'	This combined function incorporates both choices shown
#'	as results- to either stay with the original choice of 
#'	door or to change doors. 
#'
#' @details
#'	This comprehensive function incorporates all of the 
#'	functions that were previously stated above. The 
#'	variables are all held within and carry through from 
#'	beginning to end. An added feature is the inclusion
#'	of 2 different outcomes. 'outcome.stay' and 'outcome.switch' 
#'	provides the results from both staying with the original
#'	'final.pick.stay' door and the 'final.pick.switch' door. 
#'	After the two different results are gathered, a data 
#'	frame is created with 'strategy' in one column and 'outcome'
#'	in the other. The very end of this 'play_game' function 
#'	returns the 'game.results' which includes both outcomes of 
#'	staying and switching doors. 
#'
#' @param 
#'	There are no paramaters for the 'play_game' function. 
#'	The parameters are all within the main function and the 
#'	inner functions were declared and defined above. 
#'
#' @return 
#'	The function returns a dataframe named 'game.results'. 
#'	This data frame is made up of two columns, 'strategy' 
#'	and 'outcome'. The 'strategy' vector is a character string
#'	and will either be "stay" or "switch". The 'outcome' 
#'	vector is also a character string that can be "WIN" or 
#'	"LOSE". 
#'
#' @examples
#'	play_game()
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




#' @title
#'	Run the game for desired number of times in a loop.
#'
#' @description
#'	This function simulates the Monty Hall goat game many
#'	times and builds a dataset from the combined results. 
#'	It will return a list and then a table is created 
#'	from that list for viewing. 
#'
#' @details
#'	This loop simulation runs the 'play_game' function for
#'	a number of times declared with the 'n' variable.
#'	The default value of 'n' is 100. 
#'	The 'dplyr' library is used to display the 
#'	results of the loop in a table. 'results.list' is the 
#'	collector vector that each additional loop appends to 
#'	form the dataset. 'game.outcome' is just another vector 
#'	for the 'game.results' in the 'play_game' function. 
#'	'game.outcome' is appended into 'results.list'. 
#'	The variable 'i' is the iterator variable 
#'	that counts the number of loops that the 'play_game' 
#'	function runs. This iterator also gives the 'results.list'
#'	its index values and corresponding results per loop number. 
#'	'results.df' is the data frame that is made up of 
#'	'results.list' and is manipulated by the 'dplyr' library 
#'	to be viewable as a data table. 
#'
#' @param 
#'	The 'play_n_games' function requires a numeric 
#'	value for 'n'. This number is necessary to loop 
#'	the 'play_game' function more than once. 
#'
#' @return 
#'	This function returns a data frame made of a list with 
#'	character vectors. The data frame matrix holds the values of 
#'	'results.df' from each loop of 'i' for 'n' number of times. 
#'	There are 'strategy' and 'outcome' columns with strings. 
#'
#' @examples
#'	play_n_games(n=100)
#'	play_n_games(n=5)
#'	play_n_games(n=3000)
#'

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
