#
# nmix_funs.R
# custom functions for N-mixutre model fitting within PopR
# Paul M. Lukacs  07-2015
#

nmix_wrapper <- function( data, input ){
# process the nmix workflow after fit model button is clicked
	dat <- get_nmix_dat(input)
	
	model_name <- "nmix_pijk.txt" 	#get_ipm_name(input)
	inits <- replicate(3, get_nmix_init(dat, input, model_name), 
									simplify = F)
}


#################################################################################
		get_nmix_dat <- function(input){
			#  Takes database name and gui inputs, returns data prepared for
			#  JAGS, calls format jagsd internally
			
			#  Read data from database
			sg <- read_nmix_db(input)
			
			
			#  Format data for JAGS
			out <- sg
			
		return(out)		
		}
#################################################################################

read_nmix_db <- function(input){
      #  takes shiny input object
      #  returns data subset by species, dau and year
      #  part of nmix workflow
  		if(is.null(input$dbname))
  			return()

  		#  Read data from database
  		load(file.path("data", input$dbname))
  		#  Subset and order
  		sg <- lekCountData %>%
  		#	mutate(DAU = numchar_dau(DAU),
			mutate(state = numchar_state(Statename),
  					Species = rename_sp(Species)) %>%
  			arrange(DAU, Year) %>%
  			filter(DAU == input$state & Species == input$critter &
  					 Year >= input$year[1] & Year <= input$year[2])
	  return(sg)
}


#################################################################################
		#  Define initial values
		get_nmix_init <- function(x, input = input, model_name){

			out <- gen_init(x, model_name)

		return(out)
		}
#################################################################################
		# Generate initial values
		gen_nmix_init <- function(x, model_name){
		
			nsite <- length(unique(sg$Site)) #number of sites
			nyear <- length(unique(sg$Year)) #number of years
			J <- length(unique(sg$Survey)) #surveys within years
		
			
			N.tmp <- (tapply(x$Count, site, max) + 10)/.5
			N.init <- matrix(rep(N.tmp, 13), nsite, nyear)

			out <- list(N = N.init, sd.lam = runif(1,0,1), sd.p = runif(1, 0, 1), int.lam = runif (13, -5,5), p0 = runif(13, 0,1))
			out
		}