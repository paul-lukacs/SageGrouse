      #  API related functions
      #  Josh Nowak
      #  02/2015
################################################################################
      #  Load required packages - development
      require(RCurl)
################################################################################
      api_connect <- function(dtype, game, place){
        #  Base URL
        base_url <- "https://fishandgame.idaho.gov/ifwis/rest/services/wildlife/popmodel/view/"

        #  Convert game type
        num_game <- charnum_game(game)
        
        #  Convert place name
		num_place <- charnum_gmu(place)
        
        #  Create query string
        final_url <- paste(baseurl, dtype, "/", paste(dtype, "csv", sep = "."),
                            "?game=", )
      
      }
      
      