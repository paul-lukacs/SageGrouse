    #  Function to read data from database and wrangle
    #  Josh Nowak
    #  01/2015
################################################################################
    read_db <- function(input){
      #  takes shiny input object
      #  returns data subset by species, dau and year
      #  part of ipm workflow
	  
	  ####################################
		if( is.null( input$ipmDataFile ) ){
			#cat( "in read db\n" )
			if(is.null(input$dbname))
				return()

  		#  Read data from database
#		setwd( "C:/Users/paul.lukacs/Documents/GitHub/SageGrouse")
			load(file.path("data", input$dbname))
			#print(sg_data)
		} else {
			#cat( "in read csv\n" )
			inFile <- input$ipmDataFile

			if (is.null(inFile))
				return(NULL)
    
			sg_data <- read.csv(inFile$datapath, header=TRUE, sep=",",  quote="")
			#print(sg_data)
		}
  		#  Subset and order
		#cat( "inputs: ", input$state, " ,", input$mzone , " ,", input$popn, "\n" )
		if( input$state != "" ){ 
			#cat( "in state\n" )
			md <- sg_data  %>%
  		#	mutate(DAU = numchar_dau(DAU),
			mutate(DAU = numchar_state(DAU),
  					Species = rename_sp(Species)) %>%
  			arrange(DAU, Year) %>%
  			filter(DAU == input$state & Species == input$critter &
  					 Year >= input$year[1] & Year <= input$year[2])
		} else if ( input$mzone != "" ){
			md <- sg_data %>%
			mutate(	Species = rename_sp(Species)) %>%
  			arrange(ManagementZone, Year) %>%
  			filter( ManagementZone == input$mzone & Species == input$critter &
  					 Year >= input$year[1] & Year <= input$year[2])
		} else if ( input$popn != "" ){
			md <- sg_data %>%
			mutate(	Species = rename_sp(Species)) %>%
  			arrange(Population, Year) %>%
  			filter( Population == input$popn & Species == input$critter &
  					 Year >= input$year[1] & Year <= input$year[2])
		}
	  return(md)
    }
