		#  Utilities for ipm model runs in PopR analysis page
		#  Josh Nowak
		#  02/2015
#################################################################################
		#  A function to format data for jags
		format_jagsd <- function(x, input){
			#  takes a data.frame and returns a list
			
			#  If harvest data is not modeled insert mean value for harvest 
			#  where missing
			dem <- x[,c("MaleHarvest", "FemaleHarvest")]
			xx <- apply(dem, 2, function(y){
				mu <- mean(y, na.rm = T)
				y[is.na(y)] <- mu
				y
			})
			
			x[x == 0] <- NA 
			
			#  Initial Population Size and Proportions
			p1 <- x$PopulationSize[min(which(!is.na(x$PopulationSize)))]
			if(is.na(p1)){
				muf <- 5000
				muy <- muf * 0.6
				mum <- muf * 0.2
			}else{
				muf <- p1 
				muy <- p1 * 0.6
				mum <- p1 * 0.2
			}
			#  Mean informative prior
			meanr <- mean(x$YFratio, na.rm = T)/100
			if(is.na(meanr)){
				meanr <- 0.61
			}
			meanjs <- qlogis(mean(x$YoungSurvival, na.rm = T) )
			if(is.na(meanjs)){
				meanjs <- qlogis(0.56)
			}
			meanfs <- qlogis( mean(x$FemaleSurvival, na.rm = T) )
			if(is.na(meanfs)){
				meanfs <- qlogis( 0.85 )
			}
			
			#  Time varying mean calculations
						
			
			#  Remaining data can be formatted directly because NA's are not
			#  retained
			dat <- list("nyr" = nrow(x),

						"nSf" = sum(!is.na(x$SEFemaleSurvival)),	
						"fSdat" = as.numeric(x$FemaleSurvival[!is.na(x$SEFemaleSurvival)]),
						"fSse" = 1/(as.numeric(na.omit(x$SEFemaleSurvival))^2),
						"fSyr" = which(!is.na(x$SEFemaleSurvival)),

						"nSj" = sum(!is.na(x$SEYoungSurvival)),	
						"jSdat" = as.numeric(x$YoungSurvival[!is.na(x$SEYoungSurvival)]),
						"jSse" = 1/(as.numeric(na.omit(x$SEYoungSurvival))^2),
						"jSyr" = which(!is.na(x$SEYoungSurvival)),

						"nyf" = sum(!is.na(x$SEYFratio)),
						"yfdat" = as.numeric(x$YFratio[!is.na(x$SEYFratio)]),
						"yfse" = 1/(as.numeric(na.omit(x$SEYFratio))^2),
						"yfyr" = which(!is.na(x$SEYFratio)),
						
						"nmf" = sum(!is.na(x$SEMFratio)),
						"mfdat" = as.numeric(x$MFratio[!is.na(x$SEMFratio)]),
						"mfse" = 1/(as.numeric(na.omit(x$SEMFratio))^2),
						"mfyr" = which(!is.na(x$SEMFratio)),

						"nair" = sum(!is.na(x$SEPopulationSize)),
						"counts" = as.numeric(x$PopulationSize[!is.na(x$SEPopulationSize)]),
						"nse" = 1/(as.numeric(na.omit(x$SEPopulationSize))^2),						
						"airyr" = which(!is.na(x$SEPopulationSize)),
						
						"muy1" = muy,
						"muf1" = muf,
						"mum1" = mum,
						
						"mH" = as.numeric(xx[,"MaleHarvest"]),
						"fH" = as.numeric(xx[,"FemaleHarvest"]),
						
						"meanr" = meanr,
						"meanfs" = meanfs,
						"meanjs" = meanjs
			)
			
		return(dat)
		}
#################################################################################
		get_ipm_dat <- function(input){
			#  Takes database name and gui inputs, returns data prepared for
			#  JAGS, calls format jagsd internally
			
			#  Read data from database
			md <- read_db(input)
			
			
			
			#  Add future harvest data to md
			future <- input$year[2] - max(md$Year)
			if(future > 0){
				tmp <- md[1:future,]
				tmp[] <- NA
				tmp$MaleHarvest <- 0  #rep(input$mharv, future)
				tmp$FemaleHarvest <- 0  #rep(input$fharv, future)
				md <- data.frame(rbind(md, tmp))
			}
			
			#  Format data for JAGS
			out <- format_jagsd(md)
			
		return(out)		
		}
#################################################################################
		get_ipm_name <- function(input){
			#  Create a character string with the name of the model from gui
			#  inputs
			
			#  Gather values
			#  Recruitment
			rr <- switch(input$recruitMod,
							"Constant" = "cR",
							"Time Varying" = "tR")
			#  YOY survival
			ys <- switch(input$juvSmod,
							"Constant" = "cjS",
							"Time Varying" = "tjS")

			#  Female survival
			fs <- switch(input$adultSmod,
							"Constant" = "cfS",
							"Time Varying" = "tfS")
			#  Male survival, IDFG does not have male survival data
			# ms <- "cmS"
			# for grouse make male surival model = female survival model
			ms <- switch(input$adultSmod,
							"Constant" = "cmS",
							"Time Varying" = "tmS")
			#  Process error, only models with process error currently 
			#   implemented
			pe <- "pe"
			
			#  Paste values together
			mn <- paste(paste(rr, ys, fs, ms, pe, sep = "_"), ".txt", sep = "")
			out <- paste("custom/models/ipm/", mn, sep = "") 
			
		return(out)	
		}
#################################################################################
		#  Define parameters to monitor
		get_parms <- function(input){
			#  Using user inputs decide which parameters to monitor, returns
			#  character string used by JAGS
						
			#  For now everything is the same, so hard code
			#out <- c("R", "jS", "fS", "mS", "Ny", "Nf", "Nm", "mf", "yf", 
			#			"lambda", "geoLambda", "Ntot")
			# so much for hard coding - welcome to sage-grouse
			out <- c( "jS", "fS", "mS", "Ntotf", "Ntotm",
						"lambda", "Ntot")
			
		return(out)	
		}
#################################################################################
		#  Define initial values
		get_init <- function(x, input = input, model_name){

			out <- gen_init(x, model_name)

		return(out)
		}
################################################################################
		#  Define a function to summarize the output in a friendly format
		summ_fun <- function(out, parms){
		  #  Takes a rjags object and parameters of interest and returns a table,
		  #  plot or markdown document
		  
		}
#################################################################################
		#  Call JAGS
		call_ipm <- function(input){
			withProgress(message = "Preparing Data", value = 0.2, {
			  incProgress(0.1, message = "Preparing Data")
				#  Get data together
				dat <- get_ipm_dat(input)
				#  Get Model name
				model_name <- get_ipm_name(input)
				#  Initial Values 
				inits <- replicate(3, get_init(dat, input, model_name), 
									simplify = F)
			#	dat$muy1 <- inits[[1]]$Ny[1]
			#	dat$muf1 <- inits[[1]]$Nf[1]
			#	dat$mum1 <- inits[[1]]$Nm[1]
				#  Parameters to monitor
				parms <- get_parms(input)
				incProgress(0.2, message = "Data ready, running model...")
				#  Check that iter is > burnin
				if(input$ipmburn > input$ipmiter){
					ipmiter <- input$ipmburn
					ipmburn <- round(input$ipmburn/2)
				}else{
					ipmiter <- input$ipmiter
					ipmburn <- input$ipmburn				
				}
				#  JAGS
				out <- try(jags(data = dat,
					inits = inits,
					parameters.to.save = parms,
					model.file = model_name,
					n.chains = 3,
					n.iter = ipmiter,
					n.burnin = ipmburn,
					n.thin = input$ipmthin,
					progress.bar = "none"), silent = F)
				incProgress(0.2, message = "Error Check")  	
				#  Retry if failed, up to 10 times
				tryit <- 1
				while(class(out) == "try-error" & tryit <= 10){
					incProgress(0.05, 
								message = paste("Error, Retry", tryit, "/10"))
					tryit <- tryit + 1
					out <- try(jags(data = dat,
						inits = inits,
						parameters.to.save = parms,
						model.file = model_name,
						n.chains = 3,
						n.iter = ipmiter,
						n.burnin = ipmburn,
						n.thin = input$ipmthin,
						progress.bar = "none"), silent = T)	
				}
				#  Auto convergence if desired
				if(class(out) != "try-error" & input$autoup){
				out <- autojags(out,
								n.iter = 5000,
								n.thin = 2,
								Rhat = 1.1,
								n.update = 10,
								progress.bar = "none")
				}
				#  Add relevant information to out object, used later for 
				#  plotting and report generation
				if(class(out) != "try-error"){
					out$dau <- input$dau
					out$year <- input$year
					out$juvSmod <- input$juvSmod
					out$recruitMod <- input$recruitMod
					out$adultSmod <- input$adultSmod
					out$mharv <- input$mharv
					out$fharv <- input$fharv
					out$burn <- ipmburn
					out$iter <- ipmiter
					out$thin <- input$ipmthin
				}
			setProgress(1, message = "Finished")  	
			})
		return(out)	
		}
#################################################################################
	#  End