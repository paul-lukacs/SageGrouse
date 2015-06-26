		#  Naming function for download button on reports tab of ipm tab
		#  Josh Nowak
		#  01/2015
#################################################################################
		name_ipm <- function(input){
			#  Uses shiny input object (i.e. user inputs) to create a unique and
			#  informative name for the output file when downloading 
			#  ipm output
			tmp <- paste(input$critter, "_", input$dau, "_", input$year[1], "_", 
				input$year[2], "_", input$juvSmod, "_", input$recruitMod, "_", 
				input$adultSmod, "_", input$mharv, "_", input$fharv, "_", 
				format(as.POSIXct(Sys.time()), "%Y.%m.%d_%H%M%S"),  
				sep = "")
			out <- gsub(" ", "_", tmp)
		return(out)
		}