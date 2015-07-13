		#  API related functions
		#  Josh Nowak
		#  02/2015
################################################################################
		#  Load required packages - development
		# require(downloader)
#################################################################################
		#  For development
		# setwd("C:/PopR_Master_dev")
		# source("custom/game_lookup.R")
		# source("custom/dau_lookup.R")
		
		# dtype <- "Sightability"
		# input <- list()
		# input$critter <- "Mule Deer"
		# input$dau <- "Boise River"
#################################################################################
		#  Function to translate user inputs to values meaningful to api 
		#   references
		api_dau <- function(species, dau){
			#  Mule Deer
			if(species == "Mule Deer"){
				out <- paste("MD_", sub(" ", "+", dau), sep = "")
			}
			#  Elk
			if(species == "Elk"){
				out <- paste("Elk_", sub(" ", "+", dau), sep = "")
			}			
		out		
		}
#################################################################################
		#  Function to download files
		down_file <- function(final_url, method){
			xx <- getURL(final_url, ssl.verifypeer = F)
			out <- read.csv(textConnection(xx), as.is = T)
		}
#################################################################################
		#  Function that actually connects to the API
		api_connect <- function(dtype, species, dau, progress = T){
			if(progress){
				withProgress(message = "Preparing Data", value = 0.2, {
				
				incProgress(0.1, message = "Contacting Server...")
				
				#  Base URL
				base_url <- "https://fishandgame.idaho.gov/ifwis/rest/services/wildlife/popmodel/view/"

				#  Convert game type
				num_game <- charnum_game(species)

				#  Convert place name
				place <- api_dau(species, dau)

				#  Create query string
				final_url <- paste(base_url, tolower(dtype), "/", "myfile.csv", 
									"?game=", num_game, "&area=", place, 
									sep = "")

				incProgress(0.3, message = "Downloading Data...")

				#  Connect to API, read data and return R object
				#  Update this line and function arguments to accomodate username 
				#  and passwords once security is setup
				out <- down_file(final_url)	

				setProgress(1, message = "Finished")
				})
			}else{
				#  Base URL
				base_url <- "https://fishandgame.idaho.gov/ifwis/rest/services/wildlife/popmodel/view/"

				#  Convert game type
				num_game <- charnum_game(species)

				#  Convert place name
				place <- api_dau(species, dau)

				#  Create query string
				final_url <- paste(base_url, tolower(dtype), "/", "myfile.csv", 
									"?game=", num_game, "&area=", place, 
									sep = "")
									
				#  Connect to API, read data and return R object
				#  Update this line and function arguments to accomodate username 
				#  and passwords once security is setup
				out <- down_file(final_url)	
			}

		return(out)
		}
#################################################################################
		#  End
      
      