		#  A function to convert raw area names to presentable names and do the
		#  reverse
		#  Josh Nowak
		#  3/2015
#################################################################################
		#  For development
		# require(dplyr)
		# source("C:/PopR_Master/custom/api_funs.r")
		# input <- list()
		# input$critter <- "Mule Deer"
#################################################################################
		#  Subset areas by species
		spp_area <- function(x, species){
			#  Takes daus object and species
			#  Returns subset data frame with species prefix removed
			
			sp <- switch(species,
						 "Mule Deer" = "MD",
						 "Elk" = "Elk")
						 
			out <- x %>% 
					filter(grepl(sp, Area) & 
							SampleAreaType %in% c("Zone", "PMU")) %>%
					mutate(Area = gsub(paste(sp, "_", sep = ""),
										"", Area)) %>%
					arrange(Area)
		out
		}
		