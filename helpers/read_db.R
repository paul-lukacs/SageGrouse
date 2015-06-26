    #  Function to read data from database and wrangle
    #  Josh Nowak
    #  01/2015
################################################################################
    read_db <- function(input){
      #  takes shiny input object
      #  returns data subset by species, dau and year
      #  part of ipm workflow
  		if(is.null(input$dbname))
  			return()

  		#  Read data from database
		setwd( "C:/Users/paul.lukacs/Documents/GitHub/SageGrouse")
  		load(file.path("data", input$dbname))
  		#  Subset and order
  		md <- sg_data %>%
  		#	mutate(DAU = numchar_dau(DAU),
			mutate(DAU = numchar_state(DAU),
  					Species = rename_sp(Species)) %>%
  			arrange(DAU, Year) %>%
  			filter(DAU == input$state & Species == input$critter &
  					 Year >= input$year[1] & Year <= input$year[2])
	  return(md)
    }
