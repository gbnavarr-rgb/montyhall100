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
#'   Select a door.
#'
#' @description
#'   'select_door()` randomly selects one of the three doors
#'   for the contestant's initial pick.
#'
#' @details
#'   Simulates the contestant's first, choice among
#'   the three closed doors before the host reveals a goat
#'   behind one of the other doors.
#'
#' @param ... no arguments are used
#'
#' @return The function returns a numeric vector with length 1
#'   indicating the door number (1, 2, or 3) selected.
#'
#' @examples
#'   select_door()
#'   select_door()
#'   select_door()
#'   

select_door <- function( )
{
  
  doors <- c(1:3) 
  a.pick <- sample(doors, size=1)
  return( a.pick )  # number between 1 and 3
  
}



#' @title
#' open a goat door 
#' 
#' @description
#' open_goat_door () host opens one of the two unselected doors that has a goat
#' 
#' @details
#' if the contestant originally picks the door with the car the host will randomly open 
#' 1/2 remaining doors. If the contestant initially picked the door with the goat
#' the host opens the other door with the goat making sure to not reveal the door with the car 
#' 
#' @param 
#' game obtains a length of a 3 character vector and returned by 'create_game()'
#' indicating the locations of the goats and the car
#' 
#' @param 
#' a.pick obtains a length of 1 numeric vector indicating the contestants original door pick 
#' and returned as 'select_door()'
#' 
#' @return 
#' the function returns a length 1 numeric vector indicating the door numbers (1-3)
#' opened by the host which will always reveal a goat 
#' 
#' @examples
#' this.game <- create_game()
#' this.game
#' my.initial.pick <- select_door()
#' my.initial.pick
#'open_goat_door(this.game, my.initial.pick)
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # next function is if contestant selected car, or 
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
#' change doors 
#' 
#' @description
#' `change_doors()" determines the contestant's final door
#' selection, based on whether they stay with their original
#' choice or switch to the remaining unopened door.
#' 
#' @details
#' After the host opens a goat door, the contestant may
#' either stay with their original selection or switch to
#' the other unopened door. This function returns the final
#' decision. 
#' 
#' @param 
#' 'stay' is a logical value that indicates whether the
#' contestant stayed with their original pick (TRUE) which is the
#' default or switches to the other unopened door (`FALSE).
#' 
#' @param
#' 'opened.door' A length 1 numeric vector indicating the
#' door opened by the host, as returned by 'open_goat_door()`.
#' also located in the ELSE statement as something to exclude 
#' (!doors %in% c(a.pick, opened.door)) — so it must be the door 
#' that's no longer a valid choice because the host already opened it.
#'  
#' @param
#' 'a.pick' A length 1 numeric vector indicating the
#' contestant's initial door pick, as returned by
#' 'select_door()`.
#' 
#' @return 
#' The function returns a length 1 numeric vector
#' indicating the contestant's final door pick (1, 2, or 3).
#' 
#' @examples
#' opened.door <- open_goat_door( this.game, my.initial.pick )
#' change_door( stay=T, 
#'             opened.door=opened.door, 
#'             a.pick=my.initial.pick )
#' change_door( stay=F, 
#'            opened.door=opened.door, 
#'             a.pick=my.initial.pick )
#'my.final.pick <- change_door( stay=F, 
#'                              opened.door=opened.door, 
#'                              a.pick=my.initial.pick )
#'this.game
#'my.initial.pick
#'my.final.pick 
#' 
#' @export

change_door <- function( stay=T, opened.door, a.pick ) {
  
  doors <- c(1:3)
  
  if (stay == TRUE) {
    final.pick <- a.pick
  } else {
    final.pick <- doors[!doors %in% c(a.pick, opened.door)]
  }
  
  return( final.pick )  # number between 1 and 3
}

#' @title
#' determines if contestant won 
#' 
#' @description
#' determine_winner() checks whether the contestant's final selection 
#' has the car
#' 
#' @details
#' compares the final door pick with the underlying game to determine whether 
#' the outcome is the winner 'car' or loser 'goat'. 
#' 
#' @param 
#' 'final.pick' is a length of 1 numeric vector indicating the contestants final 
#' pick and resturned as 'change_door()'
#' 
#' @param 
#' game is a length of 3 character vector and returned as 'create_game' indicating 
#' the locations of goats and the car.
#' 
#' @return 
#' The function returns a character string, either
#' "WIN" or "LOSE" based on whether the final pick was the car or a goat.
#' 
#' @examples
#' this.game
#'my.initial.pick
#'my.final.pick <- change_door( stay=T, 
#'                              opened.door=opened.door, 
#'                              a.pick=my.initial.pick )
#'determine_winner( final.pick=my.final.pick, 
#'                  game=this.game )
#'my.final.pick <- change_door( stay=F, 
#'                              opened.door=opened.door, 
#'                              a.pick=my.initial.pick )
#'determine_winner( final.pick=my.final.pick, 
#'                 game=this.game )

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
#' the complete game of Monty hall 
#' 
#' @description
#' 'play_game()` only runs one complete simulation of the Monty
#'   Hall game, testing both the stay and switch strategies
#'   on the same game setup.
#'   
#' @details
#' Creates a new game, makes an initial pick, has the host
#' open a goat door, and then computes the outcome for both
#' the "stay" and "switch"
#' 
#' @param 
#' no arguments are used by the function.
#' 
#' @return 
#' The function returns a data frame with two columns,
#'  'strategy' ("stay" or "switch") and 'outcome`
#'   ("WIN" or 'LOSE`), with one row per strategy.
#'   
#' @examples
#'  play_game()
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
#' multiple games of Monty Hall. 
#' 
#' @description
#' play_n_games()` runs many simulations of the Monty Hall
#' game and summarizes the win/loss proportions for the stay
#' and switch strategies.
#' 
#' @details
#' Repeats 'play_game()' 'n` times, collects the results
#' into a single data frame, and prints a table of the
#' proportion of wins and losses for each strategy.
#' 
#' @param 
#' 'n' is a length of 1 numeric vector indicating the number of games to simulate 
#' defaulting to 100
#' 
#' @return 
#' The function returns a data frame with '2 * n` rows
#' and two columns, 'strategy' and 'outcome`, containing the
#' results of every simulated game.
#' 
#' @examples
#' play_n_games(100)
#' 
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

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}



#################################################################################
#' @title
#'   builds doors for the new Monty Hall Problem game with 5 doors- 3 goats and 2 cars
#'
#' @description
#'   build_doors() generates two more doors n =5 
#'   3 doors with goats behind them, and two doors with cars.
#'
#' @details
#'   The game setup. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return 
#' returns a numeric vector of door numbers (1:n), not goat/car labels
#'
#' @examples
#'   build_doors()
#'
#' @export

build_doors <- function( n=5 ){ return( 1:n ) }


#' @title
#' Create a new Monty Hall Problem game with 5 doors- 3 goats and 2 cars
#'
#' @description
#' create_game1() generates a new game that consists of five doors 
#' with goats behind them, and two with cars.
#'
#' @details
#' The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens two door to reveal a goat and car, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 5 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game1()
#'
#' @export

create_game1 <- function( )
{
  a.game <- sample( x=rep( c("goat","car"), c(3,2) ), size=5, replace=F )
  return( a.game )
}


#' @title
#' Select a door
#'
#' @description
#' select_door1() randomly selects one of the five doors as the
#' contestant's initial pick.
#'
#' @details
#' This simulates the contestant's first move in the game to choosing
#' one door out of five, before any doors have been opened by the
#' host. The selection is made completely at random. 
#'
#' @param ... no arguments are used by the function.
#'
#' @return 
#' The function returns a length 1 numeric vector, a number
#' between 1 and 5 representing the door the contestant picked.
#'
#' @examples
#' select_door1()
#'
#' @export
select_door1 <- function( )
{
  doors <- build_doors() 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 5
}


#' @title
#' Open doors to reveal a car and a goat
#'
#' @description
#' open_doors1() simulates the host opening two of the remaining,
#' un selected doors one always revealing one car and the other one goat. This is after
#' the contestant has made their initial pick.
#'
#' @details
#' Depending on whether the contestant's initial pick was a car or a
#' goat, this function selects two doors from among the doors not
#' picked by the contestant. Meaning one door with a car behind it, and one
#' door with a goat behind it. This is suppose to be the host's role in the
#' Monty Hall game, who always reveals a combination of doors
#' without revealing the contestant's own door.
#'
#' @param 
#' game 
#' A length 5 character vector, as returned by
#' create_game1(), indicating what is behind each door.
#'   
#' @param 
#' a.pick 
#' A length 1 numeric vector, as returned by
#' select_door1(), indicating the door the contestant initially
#' selected.
#'
#' @return 
#' The function returns a length 2 numeric vector containing
#' the numbers of the two doors that were opened: one with a car,
#' one with a goat.
#'
#' @examples
#' this.game <- create_game1()
#' my.pick <- select_door1()
#' open_doors1( this.game, my.pick )
#'
#' @export
open_doors1 <- function( game, a.pick )
{
  # reveal one car and one goat
  
  doors <- build_doors()
  
  if( game[ a.pick ] == "car" )
  { 
    opened.car.door <- doors[ game == "car" & doors != a.pick ]
    goat.doors <- doors[ game != "car" ] 
    opened.goat.door <- sample( goat.doors, size=1 )
    opened.doors <- c( opened.car.door, opened.goat.door )
  }
  
  if( game[ a.pick ] == "goat" )
  { 
    opened.car.door <- sample( doors[game=="car"], size=1 )
    available.goat.doors <- doors[ game != "car" & doors != a.pick ] 
    opened.goat.door <- sample( available.goat.doors, size=1 )
    opened.doors <- c( opened.car.door, opened.goat.door )
  }
  return( opened.doors ) # two numbers
}


#' @title
#' Stay with initial pick or switch to another door
#'
#' @description
#' change_door1() determines the contestant's final door choice,
#' based on whether they choose to stay with their original pick or
#' switch to another unopened door.
#'
#' @details
#' After the host opens two doors (one car, one goat), the
#' contestant is given the option to stay with their original
#' selection or switch to one of the other unopened,
#' doors. This function implements both statements  if stay=TRUE,
#' the contestant's final pick is unchanged; if stay=FALSE, a new
#' door is randomly selected from the doors that were neither
#' opened by the host or originally picked by the contestant.
#'
#' @param  
#' (stay)
#' a logical value indicating whether the contestant
#'   stays with their original pick (TRUE, the default) or switches
#'   to a different door (FALSE).
#'   
#' @param 
#' opened.doors 
#' A length 2 numeric vector, as returned by
#'  open_doors1(), indicating the two doors that were opened by the
#'  host.
#'   
#' @param 
#' a.pick
#' A length 1 numeric vector, as returned by
#' select_door1(), indicating the contestant's original door pick.
#'
#' @return 
#' The function returns a length 1 numeric vector, a number
#' between 1 and 5 representing the contestant's final door
#' selection.
#'
#' @examples
#' this.game <- create_game1()
#' my.pick <- select_door1()
#' opened <- open_doors1( this.game, my.pick )
#' change_door1( stay=TRUE, opened.doors=opened, a.pick=my.pick )
#' change_door1( stay=FALSE, opened.doors=opened, a.pick=my.pick )
#'
#' @export
change_door1 <- function( stay=T, opened.doors, a.pick )
{
  doors <- build_doors()
  
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    available.doors <- doors[ ! ( doors %in% opened.doors | doors == a.pick ) ]
    final.pick  <- sample( available.doors, size=1 ) 
  }
  
  return( final.pick )  # number between 1 and 5
}



#' @title
#' Determine if the contestant has won or lost
#'
#' @description
#' determine_winner1() checks the contestant's final door pick
#' against the game setup to determine whether they won a car or
#' lost with a goat.
#'
#' @details
#' This function compares what is actually behind the contestant's
#' final chosen door to determine the outcome of the game, after the
#' contestant has decided whether to stay with their original pick
#' or switch doors.
#'
#' @param 
#' final.pick A length 1 numeric vector, as returned by
#' change_door1()`, indicating the contestant's final door
#'   selection.
#' 
#' @param 
#' game 
#' A length 5 character vector, as returned by
#' create_game1(), indicating what is behind each door.
#'
#' @return 
#' The function returns a character string, either "WIN" if
#'  the final pick was a car, or "LOSE" if the final pick was a
#'  goat.
#'
#' @examples
#' this.game <- create_game1()
#' my.pick <- select_door1()
#' opened <- open_doors1( this.game, my.pick )
#' final <- change_door1( stay=TRUE, opened.doors=opened, a.pick=my.pick )
#' determine_winner1( final, this.game )
#'
#' @export
determine_winner1 <- function( final.pick, game )
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
#' Play one round of the 5-door Monty Hall game
#'
#' @description
#' play_game1() runs a single simulation of the 5-door Monty Hall
#' game (3 goats, 2 cars), returning the outcome for both the "stay"
#' and "switch" strategies.
#'
#' @details
#' This function ties together the individual steps of the 5-door
#' Monty Hall game: creating the game, having the contestant pick a
#' door, having the host open two doors (one car, one goat), and then
#' determining the outcome under both possible strategies — staying
#' with the original pick, or switching to another unopened door.
#' Running both strategies on the same game setup allows for a direct
#' comparison of which approach is more successful over repeated simulations.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#' The function returns a named character vector of length 2,
#' with the outcomes ("WIN" or "LOSE") for the "stay" and "switch"
#' strategies. 
#'
#' @examples
#' play_game1()
#'
#' @export
play_game1 <- function() {
  this.game1 <- create_game1()
  my.initial.pick <- select_door1()
  opened.doors <- open_doors1( this.game1, my.initial.pick )
  
  # save results for both strategies for the game
  my.final.pick.stay <- change_door1( stay=T, opened.doors=opened.doors, a.pick=my.initial.pick )
  my.final.pick.switch <- change_door1( stay=F, opened.doors=opened.doors, a.pick=my.initial.pick )
  
  game.outcome.stay <- determine_winner1( final.pick=my.final.pick.stay, game=this.game1 )
  game.outcome.switch <- determine_winner1( final.pick=my.final.pick.switch, game=this.game1 )
  
  game.outcome <- c( stay=game.outcome.stay, switch=game.outcome.switch )
  return( game.outcome )
}


#' @title
#' Play the Monty Hall game
#'
#' @description
#' play_game5() runs a single simulation of the Monty Hall game,
#' returning the outcome for both the "stay" and "switch" strategies.
#'
#' @details
#' This function ties together the individual steps of the Monty Hall
#' game: creating the game, having the contestant pick a door, having
#' the host open a non-winning, non-selected door, and then determining
#' the outcome under both possible strategies — staying with the
#' original pick, or switching to the remaining unopened door. Running
#' both strategies on the same game setup allows for a direct comparison
#' of which approach is more successful over repeated simulations. The
#' results are bundled into a two-row data frame, one row per strategy.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#' The function returns a data frame with two columns: strategy
#' ("stay" or "switch") and outcome ("WIN" or "LOSE"), with one row
#' for each strategy.
#'
#' @examples
#' play_game5()
#'
#' @export
play_game5 <- function( )
{
  new.game <- create_game1()
  first.pick <- select_door1()
  opened.doors <- open_doors1( new.game, first.pick ) 
  final.pick.stay <- change_door1( stay=T, opened.doors, first.pick )
  final.pick.switch <- change_door1( stay=F, opened.doors, first.pick )
  outcome.stay <- determine_winner1( final.pick.stay, new.game  )
  outcome.switch <- determine_winner1( final.pick.switch, new.game )
  
  # game.results <- bundle the results
  # return( <<< game.results >>> )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}





#####################################################################
#' @title
#' Create a Dynamic World of the Monty Hall Problem game
#'
#' @description
#' create_game2() generates a new game consisting of a mix of
#' "goat" and "car" doors, based on the specified number of each.
#'
#' @details
#' The game setup replicates the game on the TV show "Let's
#' Make a Deal" where there are doors for a contestant to choose
#' from, some of which have a car behind them and some have goats.
#' This version generalizes the classic 3-door setup, allowing any
#' number of goat doors and car doors to be specified, and the total
#' number of doors is the sum of the two. The doors are shuffled
#' randomly so the position of the car(s) and goat(s) is not
#' predictable.
#'
#' @param 
#' num.goats Integer the number of "goat" doors to include
#'   in the game.
#'
#' @param 
#' num.cars Integer rhe number of "car" doors to include in
#'   the game.
#'
#' @return 
#' The function returns a character vector of length
#'  num.goats + num.cars containing a random arrangement of
#'   "goat" and "car" values.
#'
#' @examples
#' create_game2( num.goats=2, num.cars=1 )
#'
#' @export
create_game2 <- function(num.goats, num.cars) {
  num.doors <- num.goats + num.cars
  a.game <- sample(x = rep(c("goat", "car"), times = c(num.goats, num.cars)),
                   size = num.doors, replace = FALSE)
  return(a.game)
}

#' @title
#'   Select a door.
#'
#' @description
#' select_door2() randomly selects one door out of the total
#' number of doors available, simulating the contestant's initial pick.
#'
#' @details
#' In the Monty Hall game, the contestant's first step is to pick
#' one of the doors without knowing what is behind any of them.
#' This function simulates that random initial choice.
#'
#' @param 
#' num.doors Integer the total number of doors in the game.
#'
#' @return 
#' The function returns a length 1 integer between 1 and
#' num.doors , indicating the contestant's initial door pick.
#'
#' @examples
#'  select_door2( num.doors=3 )
#'
#' @export
select_door2 <- function(num.doors) {
  doors <- 1:num.doors
  a.pick <- sample(doors, size = 1)
  return(a.pick)
}


#' @title
#' Host opens a goat door.
#'
#' @description
#' open_goat_door2() simulates the host opening exactly one door
#' that has a "goat" behind it, excluding the door already picked
#' by the contestant.
#'
#' @details
#' After the contestant makes their initial selection, the host
#' opens one of the remaining doors known to hide a goat. If more
#' than one such door is available (excluding the contestant's
#' pick), the host chooses one of them at random. If only one
#' eligible goat door remains, the host opens that door.
#'
#' @param 
#' game 
#' A character vector representing the game setup, as
#' returned by create_game2()`, containing "goat" and "car" values.
#' 
#' @param 
#' a.pick 
#' Integer the door number initially selected by the
#'   contestant, as returned by select_door2().
#'
#' @return 
#' The function returns a length 1 integer indicating the
#'door number the host opens, revealing a goat.
#'
#' @examples
#' this.game <- create_game2( num.goats=2, num.cars=1 )
#' my.pick <- select_door2( num.doors=length( this.game ) )
#' open_goat_door2( game=this.game, a.pick=my.pick )
#'
#' @export
open_goat_door2 <- function( game, a.pick )
{
  goat.doors <- which(game == "goat")
  
  host.doors <- goat.doors[ !goat.doors %in% c(a.pick)]
  
  if( length (host.doors) > 1)
  {
    
    host.choice<- sample(host.doors, size =1)
  } else {
    
    host.choice <- host.doors
    
  }
  
  return( host.choice ) #number between 1 and 3
  
}


#' @title
#'   Change doors, or stay with the original pick.
#'
#' @description
#' change_door2() determines the contestant's final door pick,
#' based on whether they choose to stay with their original pick
#' or switch to the remaining unopened door.
#'
#' @details
#' This function simulates the decision point in the Monty Hall
#' game where the contestant, having seen the host reveal a goat
#' door, must decide whether to stay with their original selection
#' or switch to the other unopened door. When stay=TRUE, the
#' final pick is unchanged. When stay=FALSE, the final pick is
#' randomly selected from the door(s) that are neither the
#' contestant's original pick nor the door the host opened.
#'
#' @param 
#' stay 
#' Logical. If TRUE (the default), the contestant keeps
#' their original pick. If FALSE, the contestant switches doors.
#'
#' @param 
#' opened.door 
#' Integer the door number the host opened, as
#'   returned by open_goat_door2().
#'   
#' @param 
#' a.pick 
#' Integer the door number initially selected by the
#'   contestant, as returned by select_door2().
#'   
#' @param 
#' num.doors 
#' Integer the total number of doors in the game.
#'
#' @return 
#' The function returns a length 1 integer indicating the
#' contestant's final door pick.
#'
#' @examples
#' this.game <- create_game2( num.goats=2, num.cars=1 )
#' my.pick <- select_door2( num.doors=length( this.game ) )
#' opened <- open_goat_door2( game=this.game, a.pick=my.pick )
#' change_door2( stay=FALSE, opened.door=opened, a.pick=my.pick,
#'                num.doors=length( this.game ) )
#'
#' @export
change_door2 <- function(stay = TRUE, opened.door, a.pick, num.doors) {
  doors <- 1:num.doors
  if (stay) {
    final.pick <- a.pick
  } else {
    remaining <- doors[!doors %in% c(a.pick, opened.door)]
    final.pick <- sample(remaining, size = 1)
  }
  return(final.pick)
}


#' @title
#'   Determine if the contestant has won.
#'
#' @description
#' determine_winner2() determines whether the contestant's final
#' door pick results in a win or a loss, based on whether the door
#' hides a car or a goat.
#'
#' @details
#' Once the contestant has made their final decision, this function
#' checks what is behind their chosen door and reports the outcome
#' of the game.
#'
#' @param 
#' final.pick 
#' Integer the contestant's final door pick, as
#' returned by change_door2().
#' 
#' @param 
#' game 
#' A character vector representing the game setup, as
#' returned by create_game2(), containing "goat" and "car" values.
#'
#' @return 
#' The function returns a length 1 character string, either
#' "WIN" if the final pick is a car, or "LOSE" if the final pick is
#' a goat.
#'
#' @examples
#' this.game <- create_game2( num.goats=2, num.cars=1 )
#' determine_winner2( final.pick=3, game=this.game )
#'
#' @export
determine_winner2 <- function( final.pick, game )
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
#'   Play a full round of the Monty Hall game, using both strategies.
#'
#' @description
#' play_game3() runs one complete simulation of the Monty Hall
#' game from start to finish, and returns the outcome for both the
#' "stay" and "switch" strategies for that single game.
#'
#' @details
#' This function ties together all of the individual game steps —
#' creating the game, selecting a door, having the host open a
#' goat door, and applying both the "stay" and "switch" strategies
#' into a single simulated round. Because both strategies are
#' evaluated against the same underlying game setup and the same
#' initial pick, this allows for a direct, fair comparison of how
#' each strategy performs on that round.
#'
#' @param 
#' num.goats 
#' Integer the number of "goat" doors to include
#'   in the game.
#'   
#' @param 
#' num.cars 
#' Integer the number of "car" doors to include in
#'   the game.
#'
#' @return 
#' The function returns a data frame with two rows and two
#' columns: strategy ("stay" or "switch") and outcome ("WIN" or
#' "LOSE"), showing the result of each strategy for that round.
#'
#' @examples
#' play_game3( num.goats=2, num.cars=1 )
#'
#' @export
play_game3 <- function(num.goats, num.cars) 
{
  num.doors <- num.goats + num.cars
  
  new.game     <- create_game2(num.goats, num.cars)
  first.pick   <- select_door2(num.doors)
  opened.door  <- open_goat_door2(new.game, first.pick)
  
  final.pick.stay   <- change_door2(stay = TRUE,  opened.door, first.pick, num.doors)
  final.pick.switch <- change_door2(stay = FALSE, opened.door, first.pick, num.doors)
  
  outcome.stay   <- determine_winner2(final.pick.stay, new.game)
  outcome.switch <- determine_winner2(final.pick.switch, new.game)
  
  strategy <- c("stay", "switch")
  outcome  <- c(outcome.stay, outcome.switch)
  game.results <- data.frame(strategy, outcome, stringsAsFactors = FALSE)
  return(game.results)
}


#' @title
#'   Simulate many rounds of the Monty Hall game.
#'
#' @description
#' monty.simulation() repeatedly plays the Monty Hall game and
#' collects the outcomes of the "stay" and "switch" strategies
#' across many trials, in order to estimate how each strategy
#' performs over the long run.
#'
#' @details
#' A single round of the Monty Hall game does not tell you much
#' about which strategy is better, since either strategy could win
#' or lose on any given round. This function addresses that by
#' running play_game3() repeatedly — by default, 10,000 times —
#' and combining the results of every round into a single data
#' frame. The resulting collection of outcomes can then be
#' summarized (for example, by calculating win rates) to compare
#' the "stay" and "switch" strategies.
#'
#' @param 
#' num.goats 
#' Integer the number of "goat" doors to include
#' in each game.
#'   
#' @param 
#' num.cars 
#' Integer the number of "car" doors to include in
#' each game.
#' 
#' @param 
#' trials 
#' Integer the number of times to simulate the game.
#' Defaults to 10000.
#'
#' @return 
#' The function returns a data frame with 2 * trials` rows
#' and two columns, strategy ("stay" or "switch") and outcome
#' ("WIN" or "LOSE"), containing the results of every strategy from
#' every simulated round.
#'
#' @examples
#' monty.simulation( num.goats=2, num.cars=1, trials=10000 )
#' monty.simulation( num.goats=2, num.cars=1, trials=100 )
#'
#' @export
monty.simulation <- function (num.goats, num.cars, trials = 10000 )
{
  
  results.df <- NULL   # collector
  
  for( i in 1:trials )  # iterator
  {
    game.outcome <- play_game3(num.goats, num.cars)                 #creating the data frame 
    # binding step
    results.df <- rbind( results.df, game.outcome )
  }
  
  return(results.df)
  
}













