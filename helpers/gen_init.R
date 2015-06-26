    #  Function to generate consistent initial values for JAGS models
    #  Josh Nowak
    #  01/2015
################################################################################
    gen_init <- function(x, model_name){
        Ny <- Nf <- Nm <- fS <- mS <- NULL
        Ny[1] <- x$muy1
        Nf[1] <- x$muf1
        Nm[1] <- x$mum1
        fS <- runif(1, quantile(x$fSdat, 0.4), quantile(x$fSdat, 0.6))
        mS <- fS
		jS <- runif(1, quantile(x$jSdat, 0.4), quantile(x$jSdat, 0.6))
        R <- runif(1, quantile(x$yfdat/100, 0.4), quantile(x$yfdat/100, 0.6))
        
		#  Catch NA values and add reasonable value
		if(is.na(fS)){
			fS <- runif(1, 0.8, 0.9)
		}
		if(is.na(jS)){
			jS <- runif(1, 0.4, 0.6)
		}
		if(is.na(mS)){
			mS <- runif(1, 0.8, 0.9)
		}
		if(is.na(R)){
			R <- runif(1, 0.5, 0.7)
		}		
		
		
        for(i in 2:x$nyr){
          Nf[i] <- abs(round((Nf[i-1] * fS + jS * Ny[i-1] * 0.5) + x$fH[i]/2)) 
          Ny[i] <- abs(round(Nf[i] * R))
          Nm[i] <- abs(round((Nm[i-1] * fS + jS * Ny[i-1] * 0.5) + x$mH[i]/2))
        }
		
		#  Catch Time Varying models and repeat initial values when necessary
		if(grepl("tR", model_name)){
			R <- rep(R, x$nyr)
		}else{
			R <- log(R)
		}
		if(grepl("tjS", model_name)){
			jS <- rep(jS, x$nyr)
		}
		

		#  Setup for process error models and deterministic models
		if(grepl("pe", model_name)){
			out <- list("Nf" = Nf, 
						"Nm" = Nm, 
						"Ny" = Ny, 
						"mufS" = qlogis(fS), 
						"muR" = R,
						"mujS" = qlogis(jS), 
						"mumS" = qlogis(mS))
		}else{
		    out <- list("mufS" = qlogis(fS), 
						"muR" = R, 
						"mujS" = qlogis(jS), 
						"mumS" = qlogis(mS))
		}
    out
	}