		#  Functions to get, manipulate and analyze sightability data
		#  Josh Nowak
		#  02/2015
#################################################################################
		# #  For development
		# require(RCurl)
		# require(dplyr)
		# source("C:/PopR_Master_dev/custom/api_funs.R")
		# source("C:/PopR_Master_dev/custom/game_lookup.R")
		# input <- list()
		# input$sight_critter <- "Elk"
		# input$sight_dau <- "Boise River"
		# input$sight_year <- "2010-11"
		# input$sight_surveytype <- "Sightability-Abundance"
		# input$sight_surveytype <- "Winter Comp"
		# # input$sight_aircraft <- "Hiller 12E"
		# # input$sight_aircraft <- "Hiller 12E no snow"
		# # input$sight_aircraft <- "Bell 47G"
		# #
		# tst <- sight.wrapper(input)
#################################################################################
		sight_wrapper <- function(sdata, input){
		#
		# coordinates the execution of sightability modeling
		#
		withProgress(message = "Preparing Data", value = 0.2, {
		
			# rawd <- api_connect("sightability", input$sight_critter, 
			#					  input$sight_dau)
			# sdata <- get_sight(rawd, input)

			if( input$sight_critter == "Mule Deer" ){
				sdata2 <- sight.muleDeer( input, sdata )
			} else if ( input$sight_critter == "Elk" && 
						input$sight_aircraft == "Hiller 12E" ){
				sdata2 <- sight.elkHillerSnow( input, sdata )
			} else if ( input$sight_critter == "Elk" && 
						input$sight_aircraft == "Hiller 12E no snow" ){
				sdata2 <- sight.elkHillerNoSnow( input, sdata )
			} else if ( input$sight_critter == "Elk" && 
						input$sight_aircraft == "Bell 47G" ){
				sdata2 <- sight.elkBell47G( input, sdata )
			} else if ( input$sight_critter == "Elk" && 
						input$sight_aircraft == "Bell 206" ){
				sdata2 <- sight.elkBell206( input, sdata )
			} else {
				cat( "Model unavailable for ", input$sight_critter, ".\n"  )
				return(NULL)
			}
				
			incProgress(0.3, message = "Data ready, running model...")
		
			tot <- total.sight( input, sdata2 )
		#	sightabliltyReport(tot, input, sdata2)
			setProgress(1, message = "Finished")
			})
			return( tot )
		}
#################################################################################
		#  A function to get composition surveys because the
		#   API gives you both, always
		get_sight <- function(x, input){
			
			convert <- function(x, fix.na = F){
				if(fix.na){
					x[is.na(x)] <- 0
				}
				tmp <- try(as.numeric(as.character(x)), silent = T)
				
			}
			
			out <- x %>%
				mutate(Species = as.character(Species),
						Survey_Type = as.character(Survey_Type),
						Popn_SU = convert(Popn_SU),
						Sample_SU = convert(Sample_SU),
						Standard_SU = convert(Standard_SU),
						Bio_Year = as.character(Bio_Year),
						Total = convert(Total, T),
						Ad_Females = convert(Ad_Females, T),
						Young = convert(Young, T),
						Yrlg_Males = convert(Yrlg_Males, T),
						SubAd_Males = convert(SubAd_Males, T),
						Ad_Males = convert(Ad_Males, T),
						UnClass = convert(UnClass, T),
						Activity = convert(Activity, T),
						Perc_Snow_Cover = convert(Perc_Snow_Cover, T),
						Perc_Screen_Cover = convert(Perc_Screen_Cover, T),
						Veg_Type = convert(Veg_Type, T)) %>%
				filter(Survey_Type == input$sight_surveytype & 
						Bio_Year == input$sight_year)		

		out
		}
#################################################################################
		# add Elk Bell 47, Hiller w/ snow, Hiller w/o snow, Bell 206 jetranger

		sight.muleDeer <- function( input, data ){
		#
		# sightability model of Unsworth and Ackerman from ID Aerial survey manual
		#
			pp <- c( -0.254, 1.562, 4.430, -0.888, -2.383,  -0.602, -0.634, 
					-1.368, -0.598, 0.047 )
		#	p.int <- -0.254
		#	p.act <- c(0,1.562, 4.430)
		#	p.veg <- c(0, -0.888, -2.383,  -0.602, -0.634)
		#	p.snw <- c(0, -1.368, -0.598)
		#	p.grp <- c(0.047)
			
			p.int <- -0.249895
			p.act <- c(-1.355053, 0, 2.874367)
			p.veg <- c(0.676739, 0, -1.428629,  0.330026, -0.655615)
			p.snw <- c(0.495283, -0.630864, 0)
			p.grp <- c(0.059483)
			
			data$p <- rep( NA, length( data$Total ) )
			data$sep <- rep( NA, length( data$Total ) )
			data$logitp <- rep( NA, length( data$Total ) )
			data$selogitp <- rep( NA, length( data$Total ) )
			data$snow<-ifelse( data$Perc_Snow_Cover <= 20, 1, 2)
			data$snow<-ifelse( data$Perc_Snow_Cover > 80, 3, data$snow)
			
			dm <- matrix( 0, length(data$Total), 10 )
			dm[,1] <- 1

			for(i in 1:length(data$Total)){
				if(data$Activity[i] > 0){	
					dm[i,1+data$Activity[i]] <- 1
				}
				if(data$Veg_Type[i] > 0){
					dm[i,3+data$Veg_Type[i]] <- 1
				}
				if(data$snow[i] > 0){
					dm[i,7+data$snow[i]] <- 1
				}
			}
			dm[,10] <- data$Total
			
			options(warn=-1)
			data$logitp <- p.int + p.act[data$Activity] + 
									p.veg[data$Veg_Type] +
									p.snw[data$snow] + 
									p.grp * data$Total
			data$p <- 1/(1+exp(-(data$logitp)))
			for( i in 1:length(data$Total) ){
				data$selogitp[i] <- sqrt( t(dm[i,]) %*% sigma(input) %*% dm[i,] )
			}
			data$sep <- (exp(-data$logitp)/(1+exp(-data$logitp))^2)*data$selogitp
			options(warn=0)
			return(list(data=data, dm=dm))
		}
#################################################################################
		#
		sigma <- function( input ){
		#
		# var-covar matrix for deer sightability model
		#

			if( input$sight_critter == "Mule Deer" ){
				npar <- 10
				Sigma <- matrix( NA, npar, npar )
				Sigma[1,1] <- c( 0.186060 )
				Sigma[2,1:2] <- c( -0.145235,  0.260146 )
				Sigma[3,1:3] <- c( -0.054089,  0.061771,  0.225844 )
				Sigma[4,1:4] <- c( -0.095923,  0.094492,  0.067682,  0.159123 )
				Sigma[5,1:5] <- c( -0.098954,  0.097812,  0.070843,  0.081735,  0.225602 )
				Sigma[6,1:6] <- c( -0.038595, -0.003313, -0.007641, -0.003577, -0.020323,  0.128883 )
				Sigma[7,1:7] <- c( -0.033196,  0.009060,  0.009274, -0.037959,  0.007367,  0.032444,  0.159595 )
				Sigma[8,1:8] <- c( -0.088862,  0.073935, -0.016978,  0.027960,  0.028791,  0.000187,  0.011292,  0.125519 )
				Sigma[9,1:9] <- c( -0.072512,  0.051251, -0.000898,  0.021428,  0.008140,  0.002709, -0.006940,  0.068382,  0.191415 )
				Sigma[10,] <- c( -0.003347,  0.001511, -0.001222,  0.000507,  0.000689,  0.001104, -0.000030, -0.000212, -0.000309,  0.000361 )
				
				for( i in 1:npar ) Sigma[i,] <- Sigma[,i]
			} else if( input$sight_critter == "Elk" && 
						input$sight_aircraft == "Hiller 12E" ) { # elk hiller 12e with snow
				npar <- 4
				Sigma <- matrix( NA, npar, npar )
				Sigma[1,1] <- c( 0.00187 )
				Sigma[2,1:2] <- c( -0.00024,  0.00882 )
				Sigma[3,1:3] <- c( -0.054089,  0.061771,  0.225844 )
				Sigma[4,1:4] <- c( -0.00884, -0.03474, -0.00091,  0.28272 )
			
				for( i in 1:npar ) Sigma[i,] <- Sigma[,i]
			} else if( input$sight_critter == "Elk" && 
						input$sight_aircraft == "Hiller 12E no snow" ) { # elk hiller 12e no snow
				npar <- 3
				Sigma <- matrix( NA, npar, npar )
				Sigma[1,1] <- c( 0.00504 )
				Sigma[2,1:2] <- c( -0.00143,  0.01225 )
				Sigma[3,1:3] <- c( -0.01354, -0.04497, 0.28134 )
				
				for( i in 1:npar ) Sigma[i,]<-Sigma[,i]
			} else if( input$sight_critter == "Elk" && 
						input$sight_aircraft == "Bell 47G" ) { # elk hiller 12e with snow
				npar <- 3
				Sigma <- matrix( NA, npar, npar )
				Sigma[1,1] <- c( 0.00411 )
				Sigma[2,1:2] <- c( -0.00099,  0.01263 )
				Sigma[3,1:3] <- c( -0.01270, -0.05214,  0.32301 )
				
				for( i in 1:npar ) Sigma[i,]<-Sigma[,i]
			}
			return( Sigma )	
		}
#################################################################################
		### elk, Hiller 12e with snow
		sight.elkHillerSnow <- function( input, data ){
		#
		# sightability model for Elk Hiller 12e with snow Idaho
		#
			
			p.int <- 1.433
			p.veg <- c(-0.7002)
			p.snw <- c(0.008451)
			p.grp <- c(0.2041)
			
			data$p <- rep( NA, length( data$Total ) )
			data$sep <- rep( NA, length( data$Total ) )
			data$logitp <- rep( NA, length( data$Total ) )
			data$selogitp <- rep( NA, length( data$Total ) )
			data$snowCube<-(data$Perc_Snow_Cover^3)/10000  # no, really... that's what is in the model
			data$VegClass <- rep( NA, length( data$Total ) )
			data$VegClass <- ifelse( data$Perc_Screen_Cover < 12.5, 1, 2 )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 27.5 && 
									data$Perc_Screen_Cover < 42.5 , 3, 
									data$VegClass )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 42.5 && 
									data$Perc_Screen_Cover < 57.5 , 4, 
									data$VegClass )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 57.5 && 
									data$Perc_Screen_Cover < 72.5 , 5, 
									data$VegClass )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 72.5 && 
									data$Perc_Screen_Cover < 87.5 , 6, 
									data$VegClass )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 87.5, 7, 
									data$VegClass )
			
			dm <- matrix( 0, length(data$Total), 4 )
			dm[,1] <- 1
			dm[,2] <- data$VegClass
			dm[,3] <- data$snowCube
			dm[,4] <- data$Total
			
			options(warn=-1)
			data$logitp <- p.int + p.veg * data$VegClass + 
							p.snw * data$snowCube + p.grp * data$Total
			data$p <- 1/(1+exp(-(data$logitp)))
			for( i in 1:length(data$Total) )
				data$selogitp[i] <- sqrt( t(dm[i,]) %*% sigma(input) %*% dm[i,] )
			data$sep <- (exp(-data$logitp)/(1+exp(-data$logitp))^2)*data$selogitp
			options(warn=0)
			return(list(data=data, dm=dm))
		}
#################################################################################
		sight.elkHillerNoSnow <- function( input, data ){
		#
		# sightability model for Elk Hiller 12e with snow Idaho
		#
			
			p.int <- 2.1630
			p.veg <- c(-0.7618)
			p.grp <- c(0.2960)
			
			data$p <- rep( NA, length( data$Total ) )
			data$sep <- rep( NA, length( data$Total ) )
			data$logitp <- rep( NA, length( data$Total ) )
			data$selogitp <- rep( NA, length( data$Total ) )
			data$VegClass <- rep( NA, length( data$Total ) )
			data$VegClass <- ifelse( data$Perc_Screen_Cover < 12.5, 1, 2 )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 27.5 && 
									data$Perc_Screen_Cover < 42.5 , 3, 
									data$VegClass )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 42.5 && 
									data$Perc_Screen_Cover < 57.5 , 4, 
									data$VegClass )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 57.5 && 
									data$Perc_Screen_Cover < 72.5 , 5, 
									data$VegClass )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 72.5 && 
									data$Perc_Screen_Cover < 87.5 , 6, 
									data$VegClass )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 87.5, 7, 
									data$VegClass )
			
			dm <- matrix( 0, length(data$Total), 3 )
			dm[,1] <- 1
			dm[,2] <- data$VegClass
			dm[,3] <- data$Total
			
			options(warn=-1)
			data$logitp <- p.int + p.veg * data$VegClass + 
							p.snw * data$snowCube + p.grp * data$Total
			data$p <- 1/(1+exp(-(data$logitp)))
			for( i in 1:length(data$Total) )
				data$selogitp[i] <- sqrt( t(dm[i,]) %*% sigma(input) %*% dm[i,] )
			data$sep <- (exp(-data$logitp)/(1+exp(-data$logitp))^2)*data$selogitp
			options(warn=0)
			return(list(data=data, dm=dm))
		}

#################################################################################
		sight.elkBell47G <- function( input, data ){
		#
		# sightability model for Elk Bell 47G 
		#
			
			p.int <-  1.61520
			p.veg <- c(-0.65796)
			p.grp <- c(0.29818)
			
			data$p <- rep( NA, length( data$Total ) )
			data$sep <- rep( NA, length( data$Total ) )
			data$logitp <- rep( NA, length( data$Total ) )
			data$selogitp <- rep( NA, length( data$Total ) )
			data$VegClass <- rep( NA, length( data$Total ) )
			data$VegClass <- ifelse( data$Perc_Screen_Cover < 12.5, 1, 2 )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 27.5 && 
									data$Perc_Screen_Cover < 42.5 , 3, 
									data$VegClass )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 42.5 && 
									data$Perc_Screen_Cover < 57.5 , 4, 
									data$VegClass )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 57.5 && 
									data$Perc_Screen_Cover < 72.5 , 5, 
									data$VegClass )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 72.5 && 
									data$Perc_Screen_Cover < 87.5 , 6, 
									data$VegClass )
			data$VegClass <- ifelse( data$Perc_Screen_Cover > 87.5, 7, 
									data$VegClass )
			
			dm <- matrix( 0, length(data$Total), 3 )
			dm[,1] <- 1
			dm[,2] <- data$VegClass
			dm[,3] <- data$Total
			
			options(warn=-1)
			data$logitp <- p.int+p.veg*data$VegClass+p.grp*data$Total
			data$p <- 1/(1+exp(-(data$logitp)))
			for( i in 1:length(data$Total) )
				data$selogitp[i] <- sqrt( t(dm[i,]) %*% sigma(input) %*% dm[i,] )
			data$sep <- (exp(-data$logitp)/(1+exp(-data$logitp))^2)*data$selogitp
			options(warn=0)
			return(list(data=data, dm=dm))
		}

#################################################################################
		total.sight <- function( input, sight.data ){
		#
		# requires list with element of data frame with p already estimated and element of design matrix for the sightability model
		#
			dataAll <- sight.data$data
			dmAll <- sight.data$dm
			
			strats <- sort(unique(dataAll$Stratum))
			nStrat <- length(strats)
			
			Total <- matrix( NA, nStrat, 2 )
			females <- matrix( NA, nStrat, 2 )
			Young <- matrix( NA, nStrat, 2 )
			Yrlg_Males <- matrix( NA, nStrat, 2 )
			SubAd_Males <- matrix( NA, nStrat, 2 )
			Ad_Males <- matrix( NA, nStrat, 2 )
			Unclass <- matrix( NA, nStrat, 2 )
			
			for( iStrat in strats ){
				#############  Added comma to both lines below  #################
				data <- dataAll[dataAll$Stratum==iStrat,]
				dm <- dmAll[dataAll$Stratum==iStrat,]
				Tot.SU <- by( data$Total/data$p, data$Standard_SU, sum, na.rm=T )	# estimate by sample unit
				Total[iStrat,1] <- (data$Popn_SU[1]/data$Sample_SU[1])*
									sum( Tot.SU )	# total population size
#				females.SU <- by( data$Ad_Females/data$p, data$Standard_SU, 
#									sum, na.rm=T )
#				females[iStrat,1] <- (data$Popn_SU[1]/data$Sample_SU[1]) * 
#										sum( females.SU )
			 	females[iStrat,1] <- sum( data$Ad_Females/data$p, na.rm=T ) # female pop size
				Young[iStrat,1] <- sum( data$Young/data$p, na.rm=T )  		# young pop size
				Yrlg_Males[iStrat,1] <- sum( data$Yrlg_Males/data$p, na.rm=T ) # yearling pop size
				SubAd_Males[iStrat,1] <- sum( data$SubAd_Males/data$p, na.rm=T ) # sub adult pop size
				Ad_Males[iStrat,1] <- sum( data$Ad_Males/data$p, na.rm=T ) # adult males 
				Unclass[iStrat,1] <- sum( data$UnClass/data$p, na.rm=T ) # unclassified
				vc.logitP <- dm %*% sigma(input) %*% t(dm)			# VC matrix of logit p
				vc.P <- diag(c(exp(-data$logitp)/(1+exp(-data$logitp))^2))%*%vc.logitP%*%t(diag(c(exp(-data$logitp)/(1+exp(-data$logitp))^2))) # VC matrix of p
				Total[iStrat,2] <- sqrt(t(c(-data$Total/data$p^2))%*%vc.P%*%c(-data$Total/data$p^2))	# standard errors for each estimate.
				females[iStrat,2] <- sqrt(t(c(-data$Ad_Females/data$p^2))%*%vc.P%*%c(-data$Ad_Females/data$p^2))
				Young[iStrat,2] <- sqrt(t(c(-data$Young/data$p^2))%*%vc.P%*%c(-data$Young/data$p^2))
				Yrlg_Males[iStrat,2] <- sqrt(t(c(-data$Yrlg_Males/data$p^2))%*%vc.P%*%c(-data$Yrlg_Males/data$p^2))
				SubAd_Males[iStrat,2] <- sqrt(t(c(-data$SubAd_Males/data$p^2))%*%vc.P%*%c(-data$SubAd_Males/data$p^2))
				Ad_Males[iStrat,2] <- sqrt(t(c(-data$Ad_Males/data$p^2))%*%vc.P%*%c(-data$Ad_Males/data$p^2))
				Unclass[iStrat,2] <- sqrt(t(c(-data$UnClass/data$p^2))%*%vc.P%*%c(-data$UnClass/data$p^2))
			}
			
			summ <- data.frame(Mean = rep(NA, 7), SE = rep(NA, 7))
			rownames(summ) <- c("Young", "Ad Females", "Yrlg Males", 
								"SubAd Males", "Ad Males", "Unclass", "Total")
			summ[1,1] <- sum(Young[,1])
			summ[1,2] <- sqrt(sum(Young[,2]^2))
			summ[2,1] <- sum(females[,1])
			summ[2,2] <- sqrt(sum(females[,2]^2))
			summ[3,1] <- sum(Yrlg_Males[,1])
			summ[3,2] <- sqrt(sum(Yrlg_Males[,2]^2))
			summ[4,1] <- sum(SubAd_Males[,1])
			summ[4,2] <- sqrt(sum(SubAd_Males[,2]^2))
			summ[5,1] <- sum(Ad_Males[,1])
			summ[5,2] <- sqrt(sum(Ad_Males[,2]^2))
			summ[6,1] <- sum(Unclass[,1])
			summ[6,2] <- sqrt(sum(Unclass[,2]^2))			
			summ[7,1] <- sum(Total[,1])
			summ[7,2] <- sqrt(sum(Total[,2]^2))
			
			
			lst <- list(Young = Young,
						Females = females,
						Yrlg_Males = Yrlg_Males, 
						SubAd_Males = SubAd_Males, 
						Ad_Males = Ad_Males, 
						Unclass = Unclass,
						Total = Total) 

		list(summ = summ, 
				lst = lst, 
				sdata2 = sight.data,
				critter = input$sight_critter,
				dau = input$sight_dau,
				year = input$sight_year,
				aircraft = input$sight_aircraft,
				survey_type = input$sight_surveytype)
		}  
#################################################################################
		#  Sight error catching function
		sight_err <- function(x, input){
			if(input$sight_critter == "Mule Deer"){
				#  Check veg, activity and snow
				#  Missing values, i.e. 0 or NA
				#  Values too large veg > 5, act > 3, snow > 3 
				
				sdata <- x %>%
							filter(Survey_Type == input$sight_surveytype & 
									Bio_Year == input$sight_year)
				
				cat("Unique values of Veg Type\n", sort(unique(sdata$Veg_Type)),
					"\n\nAll values of Veg Type 0 <= x <= 5?\n", 
					all(sdata$Veg_Type >= 0 & sdata$Veg_Type <= 5),
					"\n\nUnique values for Snow Cover\n", 
					sort(unique(sdata$Perc_Snow_Cover)),
					"\n\nAll values of Snow Cover 0 <= x <= 100?\n",
					all(sdata$Perc_Snow_Cover >= 0 & 
						sdata$Perc_Snow_Cover <= 100),
					"\n\nUnique values for Activity\n", 
					sort(unique(sdata$Activity)),
					"\n\nAll values of Activity 0 <= x <= 3?\n",
					all(sdata$Activity >= 0 & sdata$Activity <= 3),
					"\n\nMissing values in Counts (e.g. NA)?  These will be converted to 0 internally.\n",
					anyNA(sdata[,c("Young", "Ad_Females", "Yrlg_Males", 
									"SubAd_Males", "Ad_Males", "UnClass", 
									"Total")]),
					"\n\nNA's present in Popn_SU data?  This will cause the estimate of Total to be NA.\n", 
					anyNA(sdata$Popn_SU),
					"\n\nNA's present in Sample_SU data?  This will cause the estimate of Total to be NA.\n", 
					anyNA(sdata$Sample_SU),
					"\n\nNA's present in Standard_SU data?  This will cause the estimate of Total to be NA.\n", 
					anyNA(sdata$Standard_SU)
				)
			
				#  Summarize Total 	
			}
		
		
		}
		