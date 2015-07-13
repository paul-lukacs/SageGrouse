		#  Code to run sightability models outside of shiny, the workflows 
		#  mirrors that which occurs within shiny
		#  Josh Nowak
		#  03/2015
#################################################################################
		require(RCurl)
		require(dplyr)
		require(ggplot2)
		setwd("C:/PopR_Master_dev")
		source("C:/PopR_Master_dev/custom/api_funs.R")
		source("C:/PopR_Master_dev/custom/game_lookup.R")
		source("C:/PopR_Master_dev/custom/sight_funs.R")
		source("C:/PopR_Master_dev/custom/process_sight.R")
		input <- list()
		input$sight_critter <- "Mule Deer"
		input$sight_dau <- "Bannock"
		input$sight_aircraft <- "Hiller 12E"
		#input$sight_aircraft <- "Hiller 12E no snow"
		#input$sight_aircraft <- "Bell 47G"
#################################################################################
		#  Get data
		hold <- api_connect("sightability", 
                                input$sight_critter, 
                                input$sight_dau,
								F)
		
		#  Create object to hold output
		sight <- list()
		sight$data <- hold
		print(unique(sight$data$Bio_Year))

		#  Extract survey type by year
		input$sight_year <- "2010-11"
		tmp <- sight$data %>% 
              filter(Bio_Year == input$sight_year)
			  
		#  Print useful bits for later decision making	  
		print(input$sight_critter)
		print(input$sight_dau)
		print(sort(unique(tmp$Survey_Type)))
		print(sort(unique(tmp$Aircraft)))
		
		#  Choose 1 survey type
		input$sight_surveytype <- "Sightability-Abundance"
		input$sight_surveytype <- "Winter Comp"
		
		#  If elk choose one aircraft model
		input$sight_aircraft <- "Hiller 12E"	
		input$sight_aircraft <- "Hiller 12E no snow"
		input$sight_aircraft <- "Bell 47G"		
		
		#  Subset data to survey type and year
		sight_subdata <- get_sight(sight$data, input)
		
		#  Next call is to surv_wrapper
		#hold_sightmodel <- sight_wrapper(sight_subdata(), input)
		#  Break out the pieces
		sdata <- sight_subdata
		
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
			
		tot <- total.sight( input, sdata2 )
		
		#  Tabular Outputs
		mark_sight_tab(tot, sight_subdata)
		mark_sightp_tabs(sdata2, input)
		mark_sight_rep(sdata2)
		
		#  Plots
		mark_sight_plot_comp(tot, sdata)
		mark_sight_plot_total(tot, sdata)
		
		#  Report
#		surv_html(sight, sight_subdat, input, html_document(), session)
		
		