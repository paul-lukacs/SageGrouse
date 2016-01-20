#
# Grouse IPM workflow test
# 
#
	setwd("C:/Users/paul.lukacs/Documents/GitHub/SageGrouse" )
	source("custom/db_connections.R")
	source("custom/dau_lookup.R")
	source("misc/packages.R")
	source("custom/db_connections.R")
	source("helpers/plot_harv.R")
    source("helpers/read_db.R")
    source("custom/rename_sp.R")
    source("custom/dau_lookup.R")
    source("custom/get_ipm.R")
    source("custom/mark_html.r")
    source("custom/process_ipm.R")
    source("custom/api_funs.r")
    source("custom/sight_funs.R")
    source("custom/surv_funs.R")
    source("custom/surv_html.R")
    source("custom/sight_html.R")
    source("custom/spp_area.R")
    source("custom/process_surv.R")
    source("custom/process_sight.R")
    source("custom/game_lookup.R")
    #source("custom/dic_tbl.r")
    source("helpers/name_outputs.r")
    source("helpers/gen_init.R")
    source("misc/packages.R")
    load("grouseStates.RData")

	input <- list( "critter"="Sage Grouse",
					"dbname"=DbName,
					"state"="Montana",
					"year"=c(2002,2014),
					"recruitMod"="Time Varying",
					"juvSmod"="Time Varying",
					"adultSmod"="Time Varying",
					"ipmburn"=1,
					"ipmiter"=100,
					"ipmthin"=1,
					"autoup"=FALSE,
					fitgo=1
#					"mharv"=0,
#					"fharv"=0
				)
					
					
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
				
				
				
call_ipm <- function(input){
#			withProgress(message = "Preparing Data", value = 0.2, {
#			  incProgress(0.1, message = "Preparing Data")
				#  Get data together
				dat <- get_ipm_dat(input)
				#  Get Model name
				model_name <- get_ipm_name(input)
				#  Initial Values 
				inits <- replicate(3, get_init(dat, input, model_name), 
									simplify = F)
				dat$muy1 <- inits[[1]]$Ny[1]
				dat$muf1 <- inits[[1]]$Nf[1]
				dat$mum1 <- inits[[1]]$Nm[1]
				#  Parameters to monitor
				parms <- get_parms(input)
#				incProgress(0.2, message = "Data ready, running model...")
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
#				incProgress(0.2, message = "Error Check")  	
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
 # 			setProgress(1, message = "Finished")  	
#			})
		return(out)	
		}