
	fillDaus <- function(spp){

		if(is.null(spp)){
			return(NULL)
		}else{		
			spc <- switch(spp, 
							"Elk" = "E",
							"Mule Deer" = "MD",
							"White-tailed Deer" = "WD")
			idfg <- odbcConnectAccess2007( idfgDbName )
			gmuDau <- sqlQuery( idfg, paste("SELECT DauName, DAU FROM qrDauSpecies WHERE SpeciesCode='",
				spc, "'", sep=""))
			dauList <<- list( DauName=gmuDau$DauName,
						 dau=gmuDau$DAU )
			odbcClose(idfg)
		return(dauList)
		}
	}
	
as.popData <- function( input = input){
#
# creates and object to hold population modeling data
# this object is used throughout the population modeling functions to access data components.
# arguments are the start and end year of the modeling time series
# returns a list with full general structure of the model data set
#
	years <- (input$year[2]-input$year[1])+1
	popData <- list( year=c(input$year[1]:input$year[2] ), 			# series of years for the model
				  fN=cbind(rep(NA, years),rep(NA, years)) ,	# to hold population estimates and SE, NA if missing
				  fBD=cbind(rep(NA, years),rep(NA, years)), # to hold male:female ratio estimates and SE, NA if missing
				  fFD=cbind(rep(NA, years),rep(NA, years)), # to hold young:female estimates and SE, NA if missing
				  fFS=cbind(rep(NA, years),rep(NA, years)), # to hold young survival estimates and SE, NA if missing
				  fAS=cbind(rep(NA, years),rep(NA, years)), # to hold adult female survival and SE, NA if missing
				  fBH=cbind(rep(NA, years),rep(NA, years)), # to hold male harvest estimates and SE, NA if missing
				  fDH=cbind(rep(NA, years),rep(NA, years)), # to hold female harvest estimates and SE, NA if missing
				  fFH=cbind(rep(NA, years),rep(NA, years)), # to hold young harvest estimates and SE, NA if missing
				  nYears=years								# to hold the number of years in the series
				 )
	return( popData )
}

coerceImpPopData <- function( impData, popObject ){
#
# coerceImpPopData inserts queried IMP population data into a popData object
# arguments: impData - a list from a query of the IMP database
#            popObject - a list of type popData defined in as.popData - predefine the object, defining in the 
#                        argument line may have unintended consequences depending on when the argument is evaluated.
# returns: popObject with data inserted
#
	popObject$fN[na.omit(match(impData$Year,popObject$year)),1]  <- impData$PopulationSize
	popObject$fN[na.omit(match(impData$Year,popObject$year)),2]  <- impData$SEPopulationSize
	popObject$fBD[na.omit(match(impData$Year,popObject$year)),1] <- impData$MFratio
	popObject$fBD[na.omit(match(impData$Year,popObject$year)),2] <- impData$SEMFratio
	popObject$fFD[na.omit(match(impData$Year,popObject$year)),1] <- impData$YFratio
	popObject$fFD[na.omit(match(impData$Year,popObject$year)),2] <- impData$SEYFratio
	popObject$fFS[na.omit(match(impData$Year,popObject$year)),1] <- impData$YoungSurvival
	popObject$fFS[na.omit(match(impData$Year,popObject$year)),2] <- impData$SEYoungSurvival
	popObject$fAS[na.omit(match(impData$Year,popObject$year)),1] <- impData$FemaleSurvival
	popObject$fAS[na.omit(match(impData$Year,popObject$year)),2] <- impData$SEFemaleSurvival
	popObject$fBH[na.omit(match(impData$Year,popObject$year)),1] <- impData$MaleHarvest 
	popObject$fDH[na.omit(match(impData$Year,popObject$year)),1] <- impData$FemaleHarvest
	popObject$fFH[na.omit(match(impData$Year,popObject$year)),1] <- impData$YoungHarvest

	return( popObject )
}


popSizeModel <- function( N, S, H, r, R, wf, wm, time=1){
#
# popSizeModel projects population in year t based on t-1
# N - populatin at t-1
# NN - population at t
# wm, wf - wounding loss rate
# S - survival
# R - fawns/100 does
#
	NN <- matrix(NA,3,1)
		
	if( time != 0){
		NN[2] <- (1-r)*S[2]*N[3]+S[1]*N[2]-H[2]*(1+wf)
		NN[1] <- r*S[2]*N[3]+S[1]*N[1]-H[1]*(1+wm)
		NN[3] <- R*NN[2]*0.01
	}
	if( time == 0 ){
		NN[1:2] <- N
		NN[3] <- R*NN[2]*0.01
	}

	return( NN )
}

mfRatio <- function( N ){
#
# computes the male:female ratio
#
	return(100*N[1]/N[2])
}

popModLikelihood <- function(pp, data, model="A(.)J(t)", rec.model="R(t)", r=0.5, sight=1 ){
#
# model form selected in if statement
#
# Alternative models are:
#	"A(.)J(.)" -- constant adult survival, constant juvenile survival
#	"A(.)J(t)" -- constant adult survival, time varying juvenile survival	
#	"A(.)+J(t)" -- constant adult survival, time varying juvenile survival as additive offset
#
	years <- nrow(data$fN)			# number of years in the model
	N <- matrix(NA, years, 3)		# matrix of population sizes
	N0 <- c(exp(pp[1]),exp(pp[2]))	# initial population size
	wf <- 0.1						# female wounding loss
	wm <- 0.1						# male wounding loss
	if( model == "A(.)J(.)" ){
		# constant adult and juvenile survival
		S <- 1/(1+exp(-pp[3:4]))
	} else if( model == "A(.)J(t)" ){
		# Constant adult and time varying juvenile survival
		S <- 1/(1+exp(-pp[3:(years+2)]))
	} else if( model == "A(.)+J(t)" ){
		# constant adult and time varying juvenile survival as offset
		S=matrix(NA, years)
		S[1] <- 1/(1+exp(-pp[3]))
		for( i in 1:(years-1) ){
			S[i+1] <- 1/(1+exp(-(pp[3]+pp[3+i])))
		}
	} else if( model == "A(fixed)J(t)" ){
		# fixed adult survival juvenile time varying
		S <- matrix( NA, 2*(years) )
#		S[1:(years-1)] <- ifelse( is.na(data$fAS[,1]), rep( 0.83, length(data$fas[,1])) ,data$fAS[,1] ) #1/(1+exp(-rnorm(years-1,mean=1.5, sd=0.4))) 
		S[1:(years)] <- ifelse( is.na(data$fAS[,1]), rep( 0.83, length(data$fAS[,1])) ,data$fAS[,1] )
		S[(years+1):(2*years)] <- 1/(1+exp(-pp[3:(years+2)]))
	}
	# Recruitment parameters
	# Constant recruitment R(.)
	if( rec.model == "R(t)" ){
		if( model == "A(.)J(t)" ) {
			R=exp(pp[(years+3):(years+3+years)])
		}
	} else if( rec.model == "R(.)" ){
		if( model == "A(.)J(t)" ) {
			R=exp(pp[(years+3)])
		}
	}
#	print(N0)
#	print(S)
#	print(R)
	
	# replace missing harvest with mean harvest
	dh <- ifelse( is.na(data$fDH[1,1]), mean(data$fDH[,1], na.rm=T), data$fDH[1,1] )
	bh <- ifelse( is.na(data$fBH[1,1]), mean(data$fBH[,1], na.rm=T), data$fBH[1,1] )
	fd <- ifelse( is.na(data$fFD[1,1]), mean(data$fFD[,1], na.rm=T), data$fFD[1,1] ) # need to improve the imputation process
	
#	dh <- ifelse( is.na(data$fDH[1,1]), round(rnorm( 1, mean=mean(data$fDH[,1], na.rm=T), sd=sd(data$fDH[,1], na.rm=T))), data$fDH[1,1] )
#	bh <- ifelse( is.na(data$fBH[1,1]), round(rnorm( 1, mean=mean(data$fBH[,1], na.rm=T), sd=sd(data$fBH[,1], na.rm=T))), data$fBH[1,1] )
#	fd <- ifelse( is.na(data$fFD[1,1]), round(rnorm( 1, mean=mean(data$fFD[,1], na.rm=T), sd=sd(data$fBH[,1], na.rm=T))), data$fFD[1,1] )

	if( rec.model == "R(fixed)" ){
		if( model == "A(.)J(.)" || model == "A(.)J(t)" ){
			N[1,] <- popSizeModel( N0, S[1:2], c(bh,dh), r, fd, wf, wm, time=0 )
		}  else if( model == "A(fixed)J(t)" ){
			N[1,] <- popSizeModel( N0, c(S[1],S[years+1]), c(bh,dh) , r, fd, wf, wm, time=0 )
		}

		for( i in 2:years ){
			# need to improve the imputation process
			dh <- ifelse( is.na(data$fDH[i,1]), mean(data$fDH[,1], na.rm=T), data$fDH[i,1] )
			bh <- ifelse( is.na(data$fBH[i,1]), mean(data$fBH[,1], na.rm=T), data$fBH[i,1] )	
			fd <- ifelse( is.na(data$fFD[i,1]), mean(data$fFD[,1], na.rm=T), data$fFD[i,1] )
		
			if( model == "A(.)J(.)" ){
				N[i,] <- popSizeModel( N[i-1,], S, c(bh,dh) , r, fd, wf, wm )
			} else if( model == "A(.)J(t)" ) {
				N[i,] <- popSizeModel( N[i-1,], c(S[1],S[i]), c(bh,dh) , r, fd, wf, wm )
			} else if( model == "A(fixed)J(t)"  ){
				N[i,] <- popSizeModel( N[i-1,], c(S[i],S[i+years]), c(bh,dh) , r, fd, wf, wm )
			}
#			cat( N[i, ], "\n" )
		}
	} else if( rec.model == "R(.)" ){
		if( model == "A(.)J(.)" || model == "A(.)J(t)" ){
			N[1,] <- popSizeModel( N0, S[1:2], c(bh,dh), r, R, wf, wm, time=0 )
		}  else if( model == "A(fixed)J(t)" ){
			N[1,] <- popSizeModel( N0, c(S[1],S[years+1]), c(bh,dh) , r, R, wf, wm, time=0 )
		}

		for( i in 2:years ){
			# need to improve the imputation process
			dh <- ifelse( is.na(data$fDH[i,1]), mean(data$fDH[,1], na.rm=T), data$fDH[i,1] )
			bh <- ifelse( is.na(data$fBH[i,1]), mean(data$fBH[,1], na.rm=T), data$fBH[i,1] )	
			fd <- ifelse( is.na(data$fFD[i,1]), mean(data$fFD[,1], na.rm=T), data$fFD[i,1] )
		
			if( model == "A(.)J(.)" ){
				N[i,] <- popSizeModel( N[i-1,], S, c(bh,dh) , r, R, wf, wm )
			} else if( model == "A(.)J(t)" ) {
				N[i,] <- popSizeModel( N[i-1,], c(S[1],S[i]), c(bh,dh) , r, R, wf, wm )
			} else if( model == "A(fixed)J(t)"  ){
				N[i,] <- popSizeModel( N[i-1,], c(S[i],S[i+years]), c(bh,dh) , r, R, wf, wm )
			}
#			cat( N[i, ], "\n" )
		}
	}
	
	totN <- (apply(N,1,sum)/sight) # sum population over classes and divide by sightability
	bd <- apply(N,1,mfRatio)
	
	sse <- sum( ((data$fN[,1] - totN)/data$fN[,2])^2, na.rm=T )
	if( model == "A(.)J(.)" || model == "A(.)J(t)" )
		sse <- sse+sum( ((data$fAS[,1] - S[1])/data$fAS[,2])^2, na.rm=T )
	if( model == "A(.)J(.)" ){
		sse <- sse+sum( ((data$fFS[,1] - S[2])/data$fFS[,2])^2, na.rm=T )
	} else {
		sse <- sse+sum( ((data$fFS[,1] - S[2:(years+1)])/data$fFS[,2])^2, na.rm=T )
	}
	if(  rec.model == "R(.)" ){
		sse <- sse+sum( ((data$fFD[,1] - R)/data$fFD[,2])^2, na.rm=T )
	}
	sse <- sse+sum( ((data$fBD[,1] - bd)/data$fBD[,2])^2, na.rm=T )
	
	nSam <- sum( !is.na(data$fFS), !is.na(data$fAS), !is.na(data$fBD), !is.na(data$fN), !is.na(data$fFD) )
	logL <- -0.5*nSam*log(sse)
	return(logL)	
}

pop.plot <- function(pp, data, model="A(.)J(t)", rec.model="R(t)", r=0.5, sight=1 ){
#
# plot the projected population
#
	years <- nrow(data$fN)
	N <- matrix(NA, years, 3)
	N0 <- c(exp(pp[1]),exp(pp[2]))
	wm <- 0.1
	wf <- 0.1
	if( model == "A(.)J(.)" ){
		# constant adult and juvenile survival
		S <- 1/(1+exp(-pp[3:4]))
	} else if( model == "A(.)J(t)" ){
		# Constant adult and time varying juvenile survival
		S <- 1/(1+exp(-pp[3:(years+3)]))
	} else if( model == "A(.)+J(t)" ){
		# constant adult and time varying juvenile survival as offset
		S <- matrix(NA, years)
		S[1] <- 1/(1+exp(-pp[3]))
		for( i in 1:(years-1) ){
			S[i+1] <- 1/(1+exp(-(pp[3]+pp[3+i])))
		}
	} else if( model == "A(fixed)J(t)" ){
		# fixed adult survival juvenile time varying
		S <- matrix( NA, 2*(years) )
#		S[1:(years-1)] <- ifelse( is.na(data$fAS[,1]), rep( 0.83, length(data$fas[,1])) ,data$fAS[,1] ) #1/(1+exp(-rnorm(years-1,mean=1.5, sd=0.4))) 
		S[1:(years)] <- ifelse( is.na(data$fAS[,1]), rep( 0.83, length(data$fAS[,1])) ,data$fAS[,1] )
		S[(years+1):(2*years)] <- 1/(1+exp(-pp[3:(years+2)]))
	} #else if( model == "A(fixed)J(t)" ){
	#	# fixed adult survival juvenile time varying
	#	S <- matrix( NA, 2*(years-1) )
	#	S[1:(years-1)] <- ifelse( is.na(data$fAS[,1]), rep( 0.83, length(data$fas[,1])) ,data$fAS[,1] ) #1/(1+exp(-rnorm(years-1,mean=1.5, sd=0.4))) 
	#	S[years:(2*years-2)] <- 1/(1+exp(-pp[3:(years+1)]))
	#}

	# replace missing harvest with mean harvest
	dh <- ifelse( is.na(data$fDH[1,1]), mean(data$fDH[,1], na.rm=T), data$fDH[1,1] )
	bh <- ifelse( is.na(data$fBH[1,1]), mean(data$fBH[,1], na.rm=T), data$fBH[1,1] )
	fd <- ifelse( is.na(data$fFD[1,1]), mean(data$fFD[,1], na.rm=T), data$fFD[1,1] ) # need to improve the imputation process

	if( model == "A(.)J(.)" ||model == "A(.)J(t)" ){
		N[1,] <- popSizeModel( N0, S[1:2], c(bh,dh), r, fd, wf, wm, time=0 )
	} else if( model == "A(fixed)J(t)" ){
		N[1,] <- popSizeModel( N0, c(S[1],S[years+1]), c(bh,dh) , r, fd, wf, wm, time=0 )
	}

	for( i in 2:years ){
		# need to improve the imputation process
		dh <- ifelse( is.na(data$fDH[i,1]), mean(data$fDH[,1], na.rm=T), data$fDH[i,1] )
		bh <- ifelse( is.na(data$fBH[i,1]), mean(data$fBH[,1], na.rm=T), data$fBH[i,1] )
		fd <- ifelse( is.na(data$fFD[i,1]), mean(data$fFD[,1], na.rm=T), data$fFD[i,1] )
		
		if( model == "A(.)J(.)" ){
			N[i,] <- popSizeModel( N[i-1,], S, c(bh,dh) , r, fd, wf, wm )
		}else if( model == "A(.)J(t)" ) {
			N[i,] <- popSizeModel( N[i-1,], c(S[1],S[i]), c(bh,dh) , r, fd, wf, wm )
		}else if( model == "A(fixed)J(t)"  ){
			N[i,] <- popSizeModel( N[i-1,], c(S[i],S[i+years]), c(bh,dh) , r, fd, wf, wm )
		}
	}

	totN <- (apply(N,1,sum)/sight) # sum population over classes and divide by sightability
	bd <- apply(N,1,mfRatio)
	d <- matrix(NA, years,1)
	for( i in 1:years ){
		d[i] <- ifelse( is.na(data$fN[i,1]), 0, data$fN[i,1] )
	}	

	par(mfrow=c(2,2))
	plot( c(1:years), totN, xlab="Year", ylab="Total Population Size" )
	plot( c(1:years), data$fN[,1], xlab="Year", ylab="Total Population Size/Sightability" )	
	lines( totN/1.5, type="l" )
	plot( c(1:years), data$fBD[,1], xlab="Year", ylab="Buck:Doe Ratio" )
	lines( bd, type="l" )
	plot( c(1:years), data$fAS[,1], xlab="Year", ylab="Adult Survival" )
	lines( rep(pp[3],years), type="l" )

	return( cbind(N, totN) )	
}

pop.project <- function( pp, data, model="A(.)J(t)", rec.model="R(t)", r=0.5, sight=1 ){
#
# project the population size given a set of parameters
#
	years <- nrow(data$fN)
	N <- matrix(NA, years, 3)
	N0 <- c(exp(pp[1]),exp(pp[2]))
	wm <- 0.1
	wf <- 0.1
	if( model == "A(.)J(.)" ){
		# constant adult and juvenile survival
		S <- 1/(1+exp(-pp[3:4]))
	} else if( model == "A(.)J(t)" ){
		# Constant adult and time varying juvenile survival
		S <- 1/(1+exp(-pp[3:(years+2)]))
	} else if( model == "A(.)+J(t)" ){
		# constant adult and time varying juvenile survival as offset
		S <- matrix(NA, years)
		S[1] <- 1/(1+exp(-pp[3]))
		for( i in 1:(years-1) ){
			S[i+1] <- 1/(1+exp(-(pp[3]+pp[3+i])))
		}
	} else if( model == "A(fixed)J(t)" ){
		# fixed adult survival juvenile time varying
		S <- matrix( NA, 2*(years) )
#		S[1:(years-1)] <- ifelse( is.na(data$fAS[,1]), rep( 0.83, length(data$fas[,1])) ,data$fAS[,1] ) #1/(1+exp(-rnorm(years-1,mean=1.5, sd=0.4))) 
		S[1:(years)] <- ifelse( is.na(data$fAS[,1]), rep( 0.83, length(data$fAS[,1])) ,data$fAS[,1] )
		S[(years+1):(2*years)] <- 1/(1+exp(-pp[3:(years+2)]))
	}#else if( model == "A(fixed)J(t)" ){
	#	# fixed adult survival juvenile time varying
	#	S <- matrix( NA, 2*(years-1) )
	#	S[1:(years-1)] <- ifelse( is.na(data$fAS[,1]), rep( 0.83, length(data$fas[,1])) ,data$fAS[,1] ) #1/(1+exp(-rnorm(years-1,mean=1.5, sd=0.4))) 
	#	S[years:(2*years-2)] <- 1/(1+exp(-pp[3:(years+1)]))
	#}
	# Recruitment parameters
	# Constant recruitment R(.)
	if( rec.model == "R(t)" ){
		if( model == "A(.)J(t)" ) {
			R=exp(pp[(years+3):(years+3+years)])
		}
	} else if( rec.model == "R(.)" ){
		if( model == "A(.)J(t)" ) {
			R=exp(pp[(years+3)])
		}
	}
	
	# replace missing harvest with mean harvest
	dh <- ifelse( is.na(data$fDH[1,1]), mean(data$fDH[,1], na.rm=T), data$fDH[1,1] )
	bh <- ifelse( is.na(data$fBH[1,1]), mean(data$fBH[,1], na.rm=T), data$fBH[1,1] )
	fd <- ifelse( is.na(data$fFD[1,1]), mean(data$fFD[,1], na.rm=T), data$fFD[1,1] ) # need to improve the imputation process
	
	if( rec.model == "R(fixed)" ){
		if( model == "A(.)J(.)" || model == "A(.)J(t)" ){
			N[1,] <- popSizeModel( N0, S[1:2], c(bh,dh), r, fd, wf, wm, time=0 )
		}  else if( model == "A(fixed)J(t)" ){
			N[1,] <- popSizeModel( N0, c(S[1],S[years+1]), c(bh,dh) , r, fd, wf, wm, time=0 )
		}

		for( i in 2:years ){
			# need to improve the imputation process
			dh <- ifelse( is.na(data$fDH[i,1]), mean(data$fDH[,1], na.rm=T), data$fDH[i,1] )
			bh <- ifelse( is.na(data$fBH[i,1]), mean(data$fBH[,1], na.rm=T), data$fBH[i,1] )	
			fd <- ifelse( is.na(data$fFD[i,1]), mean(data$fFD[,1], na.rm=T), data$fFD[i,1] )
		
			if( model == "A(.)J(.)" ){
				N[i,] <- popSizeModel( N[i-1,], S, c(bh,dh) , r, fd, wf, wm )
			} else if( model == "A(.)J(t)" ) {
				N[i,] <- popSizeModel( N[i-1,], c(S[1],S[i]), c(bh,dh) , r, fd, wf, wm )
			} else if( model == "A(fixed)J(t)"  ){
				N[i,] <- popSizeModel( N[i-1,], c(S[i],S[i+years]), c(bh,dh) , r, fd, wf, wm )
			}
#			cat( N[i, ], "\n" )
		}
	} else if( rec.model == "R(.)" ){
		if( model == "A(.)J(.)" || model == "A(.)J(t)" ){
			N[1,] <- popSizeModel( N0, S[1:2], c(bh,dh), r, R, wf, wm, time=0 )
		}  else if( model == "A(fixed)J(t)" ){
			N[1,] <- popSizeModel( N0, c(S[1],S[years+1]), c(bh,dh) , r, R, wf, wm, time=0 )
		}

		for( i in 2:years ){
			# need to improve the imputation process
			dh <- ifelse( is.na(data$fDH[i,1]), mean(data$fDH[,1], na.rm=T), data$fDH[i,1] )
			bh <- ifelse( is.na(data$fBH[i,1]), mean(data$fBH[,1], na.rm=T), data$fBH[i,1] )	
			fd <- ifelse( is.na(data$fFD[i,1]), mean(data$fFD[,1], na.rm=T), data$fFD[i,1] )
		
			if( model == "A(.)J(.)" ){
				N[i,] <- popSizeModel( N[i-1,], S, c(bh,dh) , r, R, wf, wm )
			} else if( model == "A(.)J(t)" ) {
				N[i,] <- popSizeModel( N[i-1,], c(S[1],S[i]), c(bh,dh) , r, R, wf, wm )
			} else if( model == "A(fixed)J(t)"  ){
				N[i,] <- popSizeModel( N[i-1,], c(S[i],S[i+years]), c(bh,dh) , r, R, wf, wm )
			}
#			cat( N[i, ], "\n" )
		}
	}
	
	totN <- (apply(N,1,sum)/sight)
	
	if( rec.model=="R(fixed)" ){
		R <- ifelse( is.na(data$fFD[i,1]), mean(data$fFD[,1], na.rm=T), data$fFD[i,1] )
	} 
	
	return( list(totN=totN, N=N, S=S, R=R) )
	
}

#
# Bayesian population models with random walk
#
#

#
# prior distributions for parameters
#
priorNB <- function( x, min=0, max=100000 ){
	p <- ifelse( x >= min && x <= max, 1, 0 )
	return( p )
}
priorND <- function( x, min=0, max=100000 ){
	p=ifelse( x >= min && x <= max, 1, 0 )
	return( p )
}
#priorAS <- function( x, mu=2.7251851, sig=0.3412543 ){
#	return( dnorm(x, mu, sig ) )
#}

priorAS <- function( x, mu=1.5, sig=1){
#
# prior distn for adult survival for fitted S models
#
	return( dnorm(x, mu, sig ) )
}

priorAS.fixed <- function( x, mu=1.5, sig=0.4 ){
#
# prior distn for adult survival in fixed S models - to be table driven after completion
#
	return( dnorm(x, mu, sig ) )
}

priorFS <- function( x, mu=1.0535116, sig=0.6962351 ){
#
# prior distn for fawn survival for fitted S models
#
	return( dnorm(x, mu, sig ) )
}

priorFS.fixed <- function( x, mu=1.0535116, sig=0.6962351 ){
#
# prior distn for fawn survival for fitted S models - to be table driven after completion
#
	return( dnorm(x, mu, sig ) )
}

priorFS.add <- function( x, min=-6, max=0 ){
	p <- ifelse( x >= min && x <= max, 1, 0 )
	return( p )
}
priorR <- function( x, min=0, max=5.5 ){
	p <- ifelse( x >= min && x <= max, 1, 0 )
	return( p )
}
priorW <- function( x, mu=0, sig=1000 ){
	return( dnorm(x, mu, sig ) )
}

posterior <- function( pp, data, model="A(.)J(t)", rec.model="R(t)" ){
#
# combines the loglikelihood and log( priors ) to obtain log-posterior
#
	years <- nrow(data$fN)
	if( model == "A(.)J(.)" ){
		z <- popModLikelihood( pp, data, model, rec.model ) +
				log( priorNB(pp[1]) ) + 
				log( priorND(pp[2]) ) +
				log( priorAS(pp[3]) ) +
				log( priorFS(pp[4]) )
	} else if( model == "A(.)J(t)" ){
		z <- popModLikelihood( pp, data, model, rec.model ) +
			 log( priorNB(pp[1]) ) +
			 log( priorND(pp[2]) ) +
			 log( priorAS(pp[3]) )
		for( i in 4:(years+2) )
			z <- z+log( priorFS(pp[i]) ) 
	} else if( model == "A(.)+J(t)" ){
		z <- popModLikelihood( pp, data, model, rec.model ) +
				log( priorNB(pp[1]) ) +
				log( priorND(pp[2]) ) +
				log( priorAS(pp[3]) )
		for( i in 4:(years+2) )
			z <- z+log( priorFS.add(pp[i]) ) 
	} else if( model == "A(fixed)J(.)" ){
		z <- popModLikelihood( pp, data, mode, rec.modell ) +
				log( priorNB(pp[1]) ) + 
				log( priorND(pp[2]) ) +
				log( priorFS(pp[3]) )
	} else if( model == "A(fixed)J(t)" ){
		z <- popModLikelihood( pp, data, model, rec.model ) +
			 log( priorNB(pp[1]) ) +
			 log( priorND(pp[2]) ) 
		for( i in 3:(years+1) )
			z <- z+log( priorFS(pp[i]) ) 
	}
	if( rec.model=="R(.)" ){
		z<-z+log(priorR(pp[length(pp)]))
	} else if( rec.model=="R(t)" ){
		z<-z+sum(log(priorR(pp[(length(pp)-years+1):length(pp)])))
	}
	return(z)
}

mcmc <- function( pp, data, model="A(.)J(t)", rec.model="R(t)", b=0, M=10000, adaptive=TRUE ) { 
#
# 	Metropolis-Hastings algorithm with a random walk update
#
#	pp   = initial parameter vector
#	data = data set
#	b    = number of burn-in iterations, not used if adaptive algorithm=TRUE
#	M    = number of MCMC chain iterations
#	adaptive = if TRUE, then adaptive proposal method of Gelman et al. is used
#		   if FALSE, then no iniatial covariance structure used for entire simulation
#

	if( adaptive && b>0 ) {
		cat("Error: Burn-in cannot be set with adaptive algorithm.")
		cat("  Add b to M and remove burn-in samples upon completion.\n" )
	}
		
	require(MASS)			# library needed for ginv()
	require(corpcor)
	require(mvtnorm)			# library needed for rmvnorm()

	jump <- matrix(0,M,1)
	
	sample <- matrix( NA, M, length(pp) )			# instantiate memory for chain
	pop <- matrix( NA, M, nrow(data$fN) )			# instantiate memory for population projection
	popMales <- matrix( NA, M, nrow(data$fN) )		# instantiate memory for population projection males
	popFemales <- matrix( NA, M, nrow(data$fN) )	# instantiate memory for population projection females
	popYoung <- matrix( NA, M, nrow(data$fN) )		# instantiate memory for population projection young
	popYFratioMay <- matrix( NA, M, nrow(data$fN) )	# instantiate memory for population projection young:female ratio May 31
	adultS <- matrix( NA, M, nrow(data$fN) )		# instantiate memory adult survival
	juvS <- matrix( NA, M, nrow(data$fN) )			# instantiate memory juvenile survival
	juvR <- matrix( NA, M, nrow(data$fN) )			# instantiate memory for recruitment
	logPost <- matrix( NA, M, 1 )					# instantiate memory for log-posterior
	
	z <- runif(b+M)								# generate all needed random numbers
	res <- optim(pp, popModLikelihood, control=list(fnscale=-1),
			      hessian = TRUE, data=data, model=model ) 		# compute MLE
	
#	cat( "mle: ", res$par, "\n" )
	
	Sigma <- 2.4*(-ginv(res$hessian))/length(pp)		# approximate posterior cov from Hessian matrix and scale
	for( i in 1:length(pp) ){						# check for positive definiteness of cov matrix
		if(  Sigma[i,i] < 0 ){
			cat( "Warning: -ginv(Hessian) not positve definite for parm", i, ".  Changing sign.\n" )
			Sigma[i,i] <- -Sigma[i,i]
		}
		if( Sigma[i,i] + 1.0 == 1.0 ){
			cat( "Warning: -ginv(Hessian) = 0 for parm", i, ".  Set = 0.1.\n" )
			Sigma[i,i] <- 0.1
		}
	}

#	Sigma <- diag( 1, length(pp) )

	if( is.positive.definite( Sigma ) == FALSE ){
		Sigma <- make.positive.definite( Sigma )
		cat( "Sigma not positive definite.  Used make.positive.definite to fix.\n" )
	}

	cat( "\nComputed starting Sigma\n" )
	lastpp <- QfunctionRW(pp, Sigma)			# start the chain
	lastpost <- posterior( t(lastpp), data, model, rec.model )	# store posterior
	
	if( adaptive ){							# adaptive algorithm stores all samples
		for( i in 1:(b+M) ){				# loop over reps
			pp <- QfunctionRW( lastpp, Sigma )	# generate proposal
			post <- posterior( t(pp), data, model, rec.model )	# store posterior	
			if( is.na( post ) ) cat("Post NA\n", pp, post, "\n" )
			if( is.na( lastpost ) ) cat("lastPost NA\n", lastpp, lastpost, "\n" )
			R <- exp( post-lastpost )			# M-H ratio on log scale and exp
			if( z[i]<R ){					# test R					
				lastpp <- pp					# keep proposal
				lastpost <- post				# always store, don't recalculate
				sample[i,] <- pp
				logPost[i] <- post
				jump[i] <- 1
			}
			else{							# reject proposal
				sample[i,] <- lastpp	
				logPost[i] <- lastpost
			}
			if( i %% 1000 == 0 && i < 10001 ){	# tune proposal dist'n every 1000 iterations for the
				pj <- sum(jump[(i-999):i])/1000	# first 10,000 iterations
				cat( "In adaptive calculation: Jump % =", pj, 
					"Adapt =", ifelse( pj<0.2 || pj>0.5, "Yes", "No"), "\n" )
				if( pj > 0.5 )
					Sigma <- cov(sample[(i-999):i,])*2/sqrt(length(pp))
				if( pj < 0.2 )
					Sigma <- cov(sample[(i-999):i,])*0.5/sqrt(length(pp))
				if( i == 10000 )
					cat( "Completed adaptive phase.\n" )
			}
#			cat( "sample: ", sample[i,], "\n" )
			projN <- pop.project( sample[i,], data, model, rec.model )
			pop[i,] <- projN$totN
			popMales[i,] <- projN$N[,1]
			popFemales[i,] <- projN$N[,2]
			popYoung[i,] <- projN$N[,3]	# project population size based on current parms
			
			if( model=="A(.)J(t)" ){
				adultS[i,] <- rep(projN$S[1], ncol(adultS) )  # fix this for more general adult S models
				juvS[i,] <- c(projN$S[-1],NA)  # fix this for more general adult S models
			}
			if( model=="A(.)J(.)" ){
				adultS[i,] <- rep(projN$S[1], ncol(adultS) )  # fix this for more general adult S models
				juvS[i,] <- rep(projN$S[2], ncol(juvS) )  # fix this for more general adult S models
			}
			if( model=="A(fixed)J(t)" ){
				adultS[i,] <- ifelse( is.na(data$fAS[,1]), rep( 0.83, length(data$fAS[,1])), data$fAS[,1] )  # fix this for more general adult S models
		#		cat( projN$S, "\n" )
				juvS[i,] <- projN$S[(data$nYears+1):(2*data$nYears)] # fix this for more general adult S models
			}
			if( rec.model == "R(.)" ){
				juvR[i,] <- rep(projN$R[1], ncol(juvR) )
			}
			if( rec.model == "R(t)" || rec.model == "R(fixed)" ){
				juvR[i,] <- projN$R[1] 
			}
			popYFratioMay[i,] <- (popYoung[i,]*juvS[i,])/(popFemales[i,]*(adultS[i,]^0.65)) # May 31 fawn:doe ratio
		}
	}
	else {
		for( i in 1:(b+M) ){				# loop over reps
			pp <- QfunctionRW( lastpp, Sigma )	# generate proposal
			post <- posterior( t(pp), data, model, rec.model )	# store posterior
			R <- exp( post-lastpost )			# M-H ratio on log scale and exp

			if( z[i]<R ){					# test R					
				lastpp <- pp					# keep proposal
				lastpost <- post				# always store, don't recalculate
				if( i > b ){
					sample[i-b,] <- pp
					logPost[i-b] <- post
				}
			}
			else{							# reject proposal
				if( i > b ){
					sample[i-b,] <- lastpp	
					logPost[i-b] <- lastpost
				}
			}
			projN <-pop.project( sample[i,], data, model, rec.model )
			pop[i,] <- projN$totN
			popMales[i,] <- projN$N[,1]
			popFemales[i,] <- projN$N[,2]
			popYoung[i,] <- projN$N[,3]	# project population size based on current parms
			if( model=="A(.)J(t)" ){
				adultS[i,] <- rep(projN$S[1], ncol(adultS) )  # fix this for more general adult S models
				juvS[i,] <- projN$S[-1]  # fix this for more general adult S models
			}
			if( model=="A(.)J(.)" ){
				adultS[i,] <- rep(projN$S[1], ncol(adultS) )  # fix this for more general adult S models
				juvS[i,] <- rep(projN$S[2], ncol(juvS) )  # fix this for more general adult S models
			}
			if( model=="A(fixed)J(t)" ){
				adultS[i,] <- ifelse( is.na(data$fAS[,1]), rep( 0.83, length(data$fas[,1])), data$fAS[,1] )  # fix this for more general adult S models
				juvS[i,] <- projN$S # fix this for more general adult S models
			}
			if( rec.model == "R(.)" ){
				juvR <- rep(projN$R[1], ncol(juvR) )
			}
			if( rec.model == "R(t)" || rec.model == "R(fixed)" ){
				juvR <- projN$R[1] 
			}
			popYFratioMay[i,] <- (popYoung[i,]*juvS[i,])/(popFemales[i,]*(adultS[i,]^0.65)) # May 31 fawn:doe ratio
		}
	}
	out <- list( sample, pop, popMales, popFemales, popYoung, adultS, juvS, juvR, popYFratioMay, Sigma )
	names(out) <- c("sample", "pop.projection", "popMales", "popFemales", "popYoung", "adultS", "juvS", "juvR", "MayFDratio", "Sigma")
	return( out )						# return the chain and the projected population
}


QfunctionRW <- function( lastpp, Sigma ){
#
# multivariate normal random walk proposal distribution candidate generation
#
	return( rmvnorm( 1, lastpp, Sigma ) )
}

	
mcmc.plots <- function ( popModel.res, popData, DAUplan, input, dirFile, sight=1 ) {	
#
#	 produce graphs and save to postscript files.
#	arguments: popModel.res - list of model results
#			popData - list of data used for the model
#			DAUplan - vector of DAU objectives
#			input - user input
#			sight - sightablility factor
#	return: none
#
	
#	postscript("c://temp//mcmc//full_chain_inform.ps")
	jpeg(paste( dirFile, "_plot1.jpg", sep="") )
	#
	# plot population trajectory over time
	#
	NN <- matrix( NA, popData$nYears, 3 )
	M <- nrow( popModel.res$pop.projection )

	
	NN <- matrix( NA, popData$nYears, 3 )
	M <- nrow( popModel.res$pop.projection )

	for( i in 1:popData$nYears ){
		NN[i,] <- quantile( popModel.res$pop.projection[ ((M/2+1):M),i], probs=c(0.025, 0.5, 0.975) )
	}
	NN <- NN*sight
	par(ask=FALSE)
#	plot( x=popData$year, y=rep(DAUplan[1], popData$nYears), type="l", lwd="1", col="red", xlab="Year", ylab="Population Size",
#		  ylim=c(0, I(max(NN)+500) ), main=paste( input$spp, "-", input$dau, "Population Trajectory" ) )
#	lines( x=popData$year, y=rep(DAUplan[2], popData$nYears), type="l", lwd="1", col="green" )
#	lines( x=popData$year, y=rep(DAUplan[3], popData$nYears), type="l", lwd="1", col="orange" )
#	lines( x=popData$year, y=(popData$fN[,1]*sight), type="p" )
	plot( x=popData$year, y=(popData$fN[,1]*sight), type="p", 
			ylim=c(0, I(max(NN)+500) ), main=paste( input$spp, "-", input$dau, "Population Trajectory" ), 
			xlab="Year", ylab="Population Size", lwd=2, col="red" )
	lines( x=popData$year, y=NN[,1], type="l", lwd=1 )
	lines( x=popData$year, y=NN[,3], type="l", lwd=1 )
	lines( x=popData$year, y=NN[,2], type="l", lwd=2 )


	dev.off()
	jpeg(paste( dirFile, "_plot2.jpg", sep="") )
	par( mfrow=c(2,2), ask=FALSE )
	plot( density( popModel.res$pop.projection[ ((M/2+1):M),(popData$nYears-3)]*sight ), xlab="Population Size", ylab="Probability Density",
			main=paste( input$spp, "-", input$dau, popData$year[popData$nYears-3] ))
#	lines( x=rep(DAUplan[2], 2), y=c(0,1), type="l", lwd="1", col="green" )
#	lines( x=rep(DAUplan[1], 2), y=c(0,1), type="l", lwd="1", col="red" )
#	lines( x=rep(DAUplan[3], 2), y=c(0,1), type="l", lwd="1", col="orange" )
	plot( density( popModel.res$pop.projection[ ((M/2+1):M),(popData$nYears-2)]*sight ), xlab="Population Size", ylab="Probability Density",
			main=paste( input$spp, "-", input$dau, popData$year[popData$nYears-2] )) 
#	lines( x=rep(DAUplan[2], 2), y=c(0,1), type="l", lwd="1", col="green" )
#	lines( x=rep(DAUplan[1], 2), y=c(0,1), type="l", lwd="1", col="red" )
#	lines( x=rep(DAUplan[3], 2), y=c(0,1), type="l", lwd="1", col="orange" )
	plot( density( popModel.res$pop.projection[ ((M/2+1):M),(popData$nYears-1)]*sight ), xlab="Population Size", ylab="Probability Density",
			main=paste( input$spp, "-", input$dau, popData$year[popData$nYears-1] ))
#	lines( x=rep(DAUplan[2], 2), y=c(0,1), type="l", lwd="1", col="green" )
#	lines( x=rep(DAUplan[1], 2), y=c(0,1), type="l", lwd="1", col="red" )
#	lines( x=rep(DAUplan[3], 2), y=c(0,1), type="l", lwd="1", col="orange" )
	plot( density( popModel.res$pop.projection[ ((M/2+1):M),(popData$nYears)]*sight ), xlab="Population Size", ylab="Probability Density",
			main=paste( input$spp, "-", input$dau, popData$year[popData$nYears] ))
#	lines( x=rep(DAUplan[2], 2), y=c(0,1), type="l", lwd="1", col="green" )
#	lines( x=rep(DAUplan[1], 2), y=c(0,1), type="l", lwd="1", col="red" )
#	lines( x=rep(DAUplan[3], 2), y=c(0,1), type="l", lwd="1", col="orange" )
	dev.off()
	
	jpeg(paste( dirFile, "_plot3.jpg", sep="") )
	#
	# plot B:D trajectory over time
	#
	for( i in 1:popData$nYears ){
		NN[i,] <- quantile( I(100*popModel.res$popMales[ ((M/2+1):M),i]/popModel.res$popFemales[ ((M/2+1):M),i]), probs=c(0.025, 0.5, 0.975) )
	}
	
	par(ask=FALSE)
	plot( x=popData$year, y=(popData$fBD[,1]), type="p", 
			ylim=c(0, I(max(NN)+20) ), main=paste( input$spp, "-", input$dau, "Buck:Doe Ratio" ), 
			xlab="Year", ylab="Buck:Doe Ratio", lwd=2, col="red" )
	lines( x=popData$year, y=NN[,1], type="l", lwd=1 )
	lines( x=popData$year, y=NN[,3], type="l", lwd=1 )
	lines( x=popData$year, y=NN[,2], type="l", lwd=2 )
	
	dev.off()
	
	jpeg(paste( dirFile, "_plot4.jpg", sep="") )
	#
	# plot population survival over time
	#
	NN2 <- matrix( NA, popData$nYears, 3 )
	for( i in 1:popData$nYears ){
		if( input$adultSmod != "A(fixed)" )
			NN[i,] <- quantile(popModel.res$adultS[ ((M/2+1):M),i], probs=c(0.025, 0.5, 0.975) )
		NN2[i,] <- quantile(popModel.res$juvS[ ((M/2+1):M),i], probs=c(0.025, 0.5, 0.975), na.rm=TRUE )
	}
	
	par(ask=FALSE)
	plot( x=popData$year, y=(popData$fAS[,1]), type="p", 
			ylim=c(0, 1 ), main=paste( input$spp, "-", input$dau, "Survival Probability" ), 
			xlab="Year", ylab="Survival", lwd=2, col="red" )
	lines( x=popData$year, y=(popData$fFS[,1]), type="p", lwd=2, col="purple" )
	if( input$adultSmod != "A(fixed)" ){
		lines( x=popData$year, y=NN[,1], type="l", lwd=1 )
		lines( x=popData$year, y=NN[,3], type="l", lwd=1 )
		lines( x=popData$year, y=NN[,2], type="l", lwd=2 )
	}
	lines( x=popData$year, y=NN2[,1], type="l", lwd=1, col="blue" )
	lines( x=popData$year, y=NN2[,3], type="l", lwd=1, col="blue" )
	lines( x=popData$year, y=NN2[,2], type="l", lwd=2, col="blue" )
	
	dev.off()
}

mcmc.EvalPlots <- function( popModel.res ){
#
#  produces plots to examine the functioning of the mcmc chain
#  argument: popModel.res - model output object (list)
#  return: none
#
	M <- length( popModel.res$sample[,1] )
	# ACF plots
	par( mfrow=c(3,3), ask=TRUE )
	for( i in 1:9 ){
		acf( popModel.res$sample[,i] )
	}
	par( mfrow=c(3,3), ask=TRUE )
	for( i in 1:9 ){
		plot( x=c((M/2+1):M), y=popModel.res$sample[(M/2+1):M,i], type="l" )
	}
	par( mfrow=c(3,3), ask=TRUE )
	for( i in 1:9 ){
		plot( density( popModel.res$sample[(M/2+1):M,i] ) )
	}
}



