		#  Debugging script for survival data and analyses
		#  Josh Nowak
		#  03/2015
#################################################################################
		#  Packages
		require(dplyr)
		require(RCurl)
		require(R2jags)
		require(ggplot2)
#################################################################################
		#  Source functions and create shiny input object
		setwd("C:/PopR_Master_dev")
		source("C:/PopR_Master_dev/custom/api_funs.R")
		source("C:/PopR_Master_dev/custom/game_lookup.R")
		source("C:/PopR_Master_dev/custom/dau_lookup.R")
		source("C:/PopR_Master_dev/custom/surv_funs.R")
		source("C:/PopR_Master_dev/custom/process_surv.R")
		source("C:/PopR_Master_dev/custom/surv_html.R")
		#  Shiny input
		input <- list()
		input$critter <- "Mule Deer"
		input$dau <- "Bannock"
		input$surv_critter <- "Mule Deer"
		input$surv_age <- "Fawn"
		input$surv_dau <- "Boise River"
		input$surv_year <- 2009
		input$surv_season <- "Winter"
		input$surv_exclude_cens <- c("Unknown")
		input$surv_exclude_mort <- c("Harvested")
		input$surv_niter <- 10000
		input$surv_burnin <- 5000
		input$surv_thin <- 1
#################################################################################
		#  Call functions in order they occur in shiny
		#  Connect to db 
		rawd <- api_connect("survival", input$surv_critter, input$surv_dau, 
							FALSE)
		
		#  Subset data to ageclass, year
		tmp <- get_survd(rawd, input)
		table(tmp$out$FateDesc)
		table(tmp$out$CensorType)
		
		head(tmp$out)

		tst <- make_eh(tmp, input)
		tst$y

		out <- call_surv( c(0,1), tst, input = input)
		print(out, 2)
		
		#  Call survival models
		out <- surv_wrapper(tmp, input, F)
		print(out, 2)
		
		#  Plot
		xx <- summ_surv(out)
		mark_surv_ts(xx)
		mark_surv_post(out, "Sann", "darkblue")
		mark_surv_post(out, "Smon", "darkgreen")
		mark_surv_post(out, "tau", "darkgreen")
		
		#  Table
		mark_surv_tbl(xx)
		
		#  Report
		surv <- list()
		surv$fit <- out
		surv$fit_summ <- xx
		subdat <- tmp
		report_type <- word_document()
		
		surv_html(surv, subdat, input)
		
		
		
		