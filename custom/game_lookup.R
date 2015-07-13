    #  Functions to cross reference game animal types
    #  Josh Nowak
    #  02/2015
################################################################################
    #  Convert numeric to character representation
    numchar_game <- function(x){
      #  Takes a vector representing the numeric game ID
      #  Returns a vector of character representations of the game ID
      load("data/gameID.RData")
      game_list <- list( gname = gameID$Game, gid = gameID$GameID )
      out <- as.character(game_list$gname[x])
	  out
    }
################################################################################
    #  Convert character to numeric representation
    charnum_game <- function(x){
      #  Takes a vector representing the game ID
      #  Returns a numeric vector representing game ID
      load("data/gameID.RData")
      game_list <- list( gname = gameID$Game, gid = gameID$GameID )
      out <- match(x, game_list$gname)
	  out
    }
################################################################################
    #  End