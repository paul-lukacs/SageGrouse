    #  Functions to cross reference dau's
    #  Josh Nowak
    #  02/2015
	#  modified by Paul Lukacs
	#  06/2015
################################################################################
    #  Convert numeric to character representation
    numchar_dau <- function(x){
      #  Takes a vector representing the numeric mule deer PMU's
      #  Returns a vector of character representations of the mule deer PMU's
      load("gmuDau.RData")
      dauList <- list( dau_name = gmuDau$DauName, dau = gmuDau$DAU )
      out <- as.character(dauList$dau_name[x])
	  out
    }
################################################################################
    #  Convert character to numeric representation
    charnum_dau <- function(x){
      #  Takes a vector representing the names of mule deer PMU's
      #  Returns a numeric vector representing mule deer PMU's
      load("gmuDau.RData")
      dauList <- list( dau_name = gmuDau$DauName, dau = gmuDau$DAU )
      out <- match(x, dauList$dau_name)
	  out
    }
################################################################################
   #  Convert numeric to character representation
    numchar_state <- function(x){
      #  Takes a vector representing the numeric sage grouse States
      #  Returns a vector of character representations of the sage grouse States
      load("grouseStates.RData")
      dauList <- list( dau_name = grouseStates$StateNames, dau = grouseStates$StateID )
      out <- as.character(dauList$dau_name[x])
	  out
    }
################################################################################
    #  Convert character to numeric representation
    charnum_state <- function(x){
      #  Takes a vector representing the names of sage grouse States
      #  Returns a numeric vector representing sage grouse States
      load("grouseStates.RData")
      dauList <- list( dau_name = grouseStates$StateNames, dau = grouseStates$StateID )
      out <- match(x, dauList$dau_name)
	  out
    }
################################################################################
    #  End