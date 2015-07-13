    #  Jags development script
    require(R2jags)
	require(dplyr)
    setwd("C:/PopR_Master")
    source("C:/PopR_Master/helpers/gen_init.R")
    source("C:/PopR_Master/custom/get_ipm.R")
    source("C:/PopR_Master/custom/dau_lookup.R")
    source("C:/PopR_Master/custom/rename_sp.R")
    source("C:/PopR_Master/helpers/gen_init.R")
    source("C:/PopR_Master/helpers/read_db.R")
	
    input <- list()
    input$dbname <- "C:/PopR_Master/IDFGdatabase.RData"
    input$critter <- "Mule Deer"
    input$year <- c(1998, 2019)
    input$mharv <- 1000
    input$fharv <- 968
    input$dau <- "Weiser McCall"
    input$recruitMod <- "Time Varying"
    input$adultSmod <- "Constant"
    input$juvSmod <- "Time Varying"
    input$ipmiter <- 45000
    input$ipmthin <- 1
    input$ipmburn <- 25000
	input$pe <- TRUE
    
    #
    dat <- get_ipm_dat(input)
    #  Get Model name
    model_name <- get_ipm_name(input)
    #  Initial Values 
    inits <- replicate(3, get_init(dat, input, model_name), simplify = F)
    #  Parameters to monitor
    parms <- get_parms(input)
    
    out <- jags(data = dat,
                inits = inits,
                parameters.to.save = c(parms, "muR"),
                model.file = paste("C:/PopR_Master/", 
                                   model_name, sep = ""),
                n.chains = 3,
                n.iter = input$ipmiter,
                n.burnin = input$ipmburn,
                n.thin = input$ipmthin,
				DIC = T)
				
	#  Mimic output object as shiny would see it			
	out$dau <- input$dau
	out$year <- input$year
	out$juvSmod <- input$juvSmod
	out$recruitMod <- input$recruitMod
	out$adultSmod <- input$adultSmod
	out$mharv <- input$mharv
	out$fharv <- input$fharv
	out$burn <- input$ipmburn
	out$iter <- input$ipmiter
	out$thin <- input$ipmthin
	
	
	#  Plotting function
	po <- function(x, dat, dem){
		vals <- apply(x$BUGS$sims.list[[dem]], 2, quantile, c(0.025, 0.5, 0.975))
		plot(1:ncol(vals), vals[2,], type = "l", main = dem, 
				ylim = c(0, max(vals)))
		lines(1:ncol(vals), vals[1,], lty = 2)
		lines(1:ncol(vals), vals[3,], lty = 2)
		points(dat$airyr, dat$counts, pch = 19, col = "red")
	}
	#  Plots
	par(mfrow = c(2, 2))
	po(out, dat, "Ntot")
	po(out, dat, "Ny")
	po(out, dat, "Nf")
	po(out, dat, "Nm")
	
	po(out, dat, "R")
	po(out, dat, "jS")
	po(out, dat, "fS")
	po(out, dat, "mS")

	po(out, dat, "yf")
	points(dat$yfyr, dat$yfdat/100, pch = 19, col = "blue")
	po(out, dat, "mf")
	points(dat$mfyr, dat$mfdat/100, pch = 19, col = "blue")
	po(out, dat, "lambda")

				
				
	adapt.run <- jags.model(paste("C:/PopR_Master/", 
							model_name, sep = ""),
				data = dat,
				inits = inits,
				n.chains = 3,
				n.adapt = input$ipmburn)
	update(adapt.run, n.iter = input$ipmiter)
	out <- coda.samples(adapt.run, parms, n.iter = 10000) 
	

	

				
	
				
