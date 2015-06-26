    #  A function to rename species to align data and gui
    #  Josh Nowak
    #  1/2015
################################################################################
    rename_sp <- function(x){
      trans_vec <- c("MD" = "Mule Deer", 
	                 "WT" = "White-tailed Deer", 
					 "SG" = "Sage Grouse"
					 )
      out <- as.character(trans_vec[x])
	out
    }