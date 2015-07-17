#
# N-mixture workflow for PopR testing.
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
	source("custom/nmix_funcs.R")
    load("grouseStates.RData")
	load("data/lekCountData.RData")

	input <- list( "nmix_critter"="Sage Grouse",
					"nmix_dbname"=DbNameLek,
					"nmix_dau"="Montana",
					"nmix_year"=c(2002,2014),
					"nmix_ipmburn"=1,
					"nmix_ipmiter"=100,
					"nmix_ipmthin"=1,
					"autoup"=FALSE,
					fitgo=1
#					"mharv"=0,
#					"fharv"=0
				)
				
			sg <- get_nmix_dat(input)
			dat <- format_nmix_jagsd( sg, input )
	
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
			
			# test without try()
			out <- jags(data = dat,
					inits = inits,
					parameters.to.save = parms,
					model.file = model_name,
					n.chains = 3,
					n.iter = ipmiter,
					n.burnin = ipmburn,
					n.thin = input$nmix_ipmthin )
			body=list("Crushing it!", mime_part(z$summary)
			sendmail( from, "paul.lukacs@umontana.edu", "PopR N-mixture Results", body , control=list(smtpServer="messaging.umt.edu")) 
			sendmail( from, "james.nowak@umontana.edu", "PopR N-mixture Results", body , control=list(smtpServer="messaging.umt.edu")) 

			
			out <- try(jags(data = dat,
					inits = inits,
					parameters.to.save = parms,
					model.file = model_name,
					n.chains = 3,
					n.iter = ipmiter,
					n.burnin = ipmburn,
					n.thin = input$ipmthin,
					progress.bar = "none"), silent = F)
		#		incProgress(0.2, message = "Error Check")  	
				#  Retry if failed, up to 10 times
				tryit <- 0
				while(class(out) == "try-error" & tryit < 10){
		#			incProgress(0.05, 
		#						message = paste("Error, Retry", tryit, "/10"))
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
					