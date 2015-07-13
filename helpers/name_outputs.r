		#  Naming function for download button on reports tab of ipm tab
		#  Josh Nowak
		#  01/2015
#################################################################################
		name_ipm <- function(ipmrun){
			#  Uses fit model output to create a unique and
			#  informative name for the output file when downloading 
			#  ipm output
			x <- ipmrun$ipm
			tmp <- paste(x$critter, "_", x$dau, "_", x$year[1], "_", 
							x$year[2], "_", x$juvSmod, "_", x$recruitMod, "_", 
							x$adultSmod, "_", x$mharv, "_", x$fharv, "_", 
							format(as.POSIXct(Sys.time()), "%Y.%m.%d_%H%M%S"),  
							sep = "")
			out <- gsub(" ", "_", tmp)
		return(out)
		}
		name_surv <- function(surv){
			#  Uses shiny input object (i.e. user inputs) to create a unique and
			#  informative name for the output file when downloading 
			#  survival output
			x <- surv$fit_summ
			tmp <- paste("Survival_", x$critter, "_", x$dau, "_", x$year, "_",
							x$age, "_", x$season, "_",
				format(as.POSIXct(Sys.time()), "%Y.%m.%d_%H%M%S"),  
				sep = "")
			out <- gsub(" ", "_", tmp)
		return(out)		
		}
		name_sight <- function(sight){
			#  Uses shiny input object (i.e. user inputs) to create a unique and
			#  informative name for the output file when downloading 
			#  survival output
			x <- sight
			tmp <- paste("Sightability_", x$critter, "_", x$dau, "_", x$year, "_",
							x$survey_type, "_", x$aircraft, "_",
				format(as.POSIXct(Sys.time()), "%Y.%m.%d_%H%M%S"),  
				sep = "")
			out <- gsub(" ", "_", tmp)
		return(out)		
		}