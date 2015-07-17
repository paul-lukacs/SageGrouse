#
# nmix_funs.R
# custom functions for N-mixutre model fitting within PopR
# Paul M. Lukacs  07-2015
#

		nmix_wrapper <- function( input ){
	#		withProgress(message = "Preparing Data", value = 0.2, {
	#		  incProgress(0.1, message = "Preparing Data")
			# process the nmix workflow after fit model button is clicked
			sg <- get_nmix_dat(input)  # read data
			dat <- format_nmix_jagsd( sg, input ) # format data for jags
	
		#	showshinyalert(session, "nmix_runalert", 
         #                paste(input$nmix_year[1],
        #                       input$nmix_dau, 
        #                       "N-mixture model",
        #                       input$nmix_fitgo,
        #                       "test"),
        #                 styleclass = "success")
		#	Sys.sleep(5)

			model_name <- "custom/models/nmix/nmix_pijk.txt" 	#get_ipm_name(input)
			inits <- replicate(3, get_nmix_init(dat, input, model_name), 
									simplify = F) # set the initial values for each chain
									
			parms <- get_nmix_parms(input)	# set the parameters to track in jags.
			# check for valid iterations and burnin length - fix if burnin > iterations.
			if(input$nmix_ipmburn > input$nmix_ipmiter){
				ipmiter <- input$nmix_ipmburn
				ipmburn <- round(input$nmix_ipmburn/2)
			}else{
				ipmiter <- input$nmix_ipmiter
				ipmburn <- input$nmix_ipmburn				
			}
			
			out <- try(jags(data = dat,
					inits = inits,
					parameters.to.save = parms,
					model.file = model_name,
					n.chains = 3,
					n.iter = ipmiter,
					n.burnin = ipmburn,
					n.thin = input$nmix_ipmthin,
					progress.bar = "none"), silent = F)
	#			incProgress(0.2, message = "Error Check")  	
				#  Retry if failed, up to 10 times
				tryit <- 0
				while(class(out) == "try-error" & tryit < 10){
	#				incProgress(0.05, 
	#							message = paste("Error, Retry", tryit, "/10"))
					tryit <- tryit + 1
					out <- try(jags(data = dat,
						inits = inits,
						parameters.to.save = parms,
						model.file = model_name,
						n.chains = 3,
						n.iter = ipmiter,
						n.burnin = ipmburn,
						n.thin = input$nmix_ipmthin,
						progress.bar = "none"), silent = T)	
				}
				#  Auto convergence if desired
		#		if(class(out) != "try-error" & input$autoup){
		#		out <- autojags(out,
		#						n.iter = 5000,
		#						n.thin = 2,
		#						Rhat = 1.1,
		#						n.update = 10,
		#						progress.bar = "none")
		#		}
				#  Add relevant information to out object, used later for 
				#  plotting and report generation
				if(class(out) != "try-error"){
					out$dau <- input$nmix_dau
					out$year <- input$nmix_year
					out$model_name <- model_name
					out$burn <- ipmburn
					out$iter <- ipmiter
					out$thin <- input$ipmthin
				}
	#		setProgress(1, message = "Finished")  	
	#		})
			if(class(out) != "try-error"){
				z<-print(out)
				body<-list(paste("Crushing it from ", out$year[1], "to", out$year[2]), mime_part(z$summary, name="out") )
				sendmail( "paul.lukacs@umontana.edu", input$nmix_email, "PopR N-mixture Results", body , control=list(smtpServer="messaging.umt.edu")) 
	#			sendmail( "paul.lukacs@umontana.edu", "james.nowak@mso.umt.edu", "PopR N-mixture Results", body , control=list(smtpServer="messaging.umt.edu")) 
			} else {
				body<-list("Crushing it! (even though the model failed)" )
				sendmail( "paul.lukacs@umontana.edu", input$nmix_email, "PopR N-mixture Results", body , control=list(smtpServer="messaging.umt.edu")) 
			}
			return(out)
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
			if(is.null(input$nmix_dbname))
				return()

			#  Read data from database
			load(file.path("data", input$nmix_dbname))
			#  Subset and order
			sg <- lekCountData %>%
			#	mutate(DAU = numchar_dau(DAU),
				mutate(state = numchar_state(StateID),
  					Species = rename_sp(Species)) %>%
				arrange(StateID, Year) %>%
				filter(state == input$nmix_dau & Species == input$nmix_critter &
  					 Year >= input$nmix_year[1] & Year <= input$nmix_year[2])	
		
			return(sg)
		}


#################################################################################
		#  Define initial values
		get_nmix_init <- function(x, input = input, model_name){

			out <- gen_nmix_init(x, model_name)

			return(out)
		}
#################################################################################
		# Generate initial values
		gen_nmix_init <- function(x, model_name){
		
			#nsite <- length(unique(x$Site)) #number of sites
			#nyear <- length(unique(x$Year)) #number of years
			#J <- length(unique(x$Survey)) #surveys within years
		
			
			N.tmp <- (tapply(x$y, x$site, max) + 10)/.5
			N.init <- matrix(rep(N.tmp, 13), x$nsite, x$nyear)

			out <- list(N = N.init, sd.lam = runif(1,0,1), sd.p = runif(1, 0, 1), int.lam = runif (13, -5,5), p0 = runif(13, 0,1))
			return(out)
		}
		
#################################################################################
		#  Define parameters to monitor
		get_nmix_parms <- function(input){
			#  Using user inputs decide which parameters to monitor, returns
			#  character string used by JAGS
						
			
			# for now everything is the same so hard code.
			out <- c("totalN", "mean.abundance", "sd.lam", "sd.p", "fit", "fit.new", "p0", "mp")
			
			return(out)	
		}
#################################################################################
		#  A function to format data for jags
		format_nmix_jagsd <- function(x, input){
			#  takes a data.frame and returns a list
			
			nsite <- length(unique(x$Site)) #number of sites
			nyear <- length(unique(x$Year)) #number of years
			J <- length(unique(x$Survey)) #surveys within years
			nobs <- nrow(x)
		
			dat <- list( "y" = x$Count,
						 "site" = x$Site,
						 "year" = I(x$Year-min(x$Year, na.rm=TRUE)+1),
						 "nyear" = nyear,
						 "nsite" = nsite,
						 "nobs" = nobs
						)
			
			
		return(dat)
		}