		#  Functions to get, manipulate and analyze sightability data
		#  Josh Nowak
		#  02/2015
#################################################################################
		# #  For development
		# require(RCurl)
		# require(dplyr)
		# source("C:/PopR_Master_dev/custom/api_funs.R")
		# source("C:/PopR_Master_dev/custom/game_lookup.R")
		# input <- list()
		# input$surv_critter <- "Mule Deer"
		# input$surv_age <- "Adult"
		# input$surv_dau <- "Bannock"
		# input$surv_year <- 2009
		# input$surv_season <- "Annual"
		# input$surv_niter <- 1000
		# input$surv_burnin <- 500
		# input$surv_thin <- 1
		# 
		# tst <- surv.wrapper(input)
#################################################################################
		#  A function to extract survey data from database API
		get_survd <- function(rawd, input){
			
			#  Calculate start dates
			strt_yr <- as.numeric(input$surv_year) - 1
			end_yr <- as.numeric(input$surv_year)
			#  Fawns
			if( input$surv_age == "Fawn" ){
				surv_start <- paste("12/15/", strt_yr, sep="" )
				surv_end <- paste("6/1/", end_yr, sep="" )
				#  Convert class of dates
				surv_start2 <- as.Date(surv_start, "%m/%d/%Y")
				surv_end <- as.Date(surv_end, "%m/%d/%Y")
			
				#  Subset data
				tmp <- rawd %>%
					mutate(Capture_Date = as.Date(Capture_Date, "%m/%d/%Y"),
							Date = as.Date(Date, "%m/%d/%Y"),
							FateDate = ifelse(FateDate == "", NA, FateDate),
							FateDate = as.Date(FateDate, "%m/%d/%Y"),
							CensorDate = ifelse(CensorDate == "", NA, 
												CensorDate),
							CensorDate = as.Date(CensorDate, "%m/%d/%Y")) %>%
							filter(Capture_Date >= surv_start2 & 
									Date <= surv_end & 
									Age_Class == input$surv_age)
			}
			
			#  Adults
			if( input$surv_age == "Adult" ){
				if( input$surv_season == "Annual" ){
					surv_start <- paste("12/15/", strt_yr, sep="" )
					surv_end <- paste("12/14/", end_yr, sep="" )
				} else if(  input$surv_season == "Summer" ){
					surv_start <- paste("6/1/", end_yr, sep="" )
					surv_end <- paste("12/14/", end_yr, sep="" )
				} else if(  input$surv_season == "Winter" ){
					surv_start <- paste("12/15/", strt_yr, sep="" )
					surv_end <- paste("6/1/", end_yr, sep="" )
				} else if(  input$surv_season == "Fall" ){
					surv_start <- paste("9/1/", end_yr, sep="" )
					surv_end <- paste("12/14/", end_yr, sep="" )
				}
				#  Convert class of dates
				surv_start2 <- as.Date(surv_start, "%m/%d/%Y")
				surv_end <- as.Date(surv_end, "%m/%d/%Y")
				#  Subset data
				tmp <- rawd %>%
					mutate(Capture_Date = as.Date(Capture_Date, "%m/%d/%Y"),
							Date = as.Date(Date, "%m/%d/%Y"),
							FateDate = ifelse(FateDate == "", NA, FateDate),
							FateDate = as.Date(FateDate, "%m/%d/%Y"),
							CensorDate = ifelse(CensorDate == "", NA, 
												CensorDate),
							CensorDate = as.Date(CensorDate, "%m/%d/%Y")) %>%
							filter(Date >= surv_start2 & 
									Date <= surv_end & 
									Age_Class == input$surv_age)
				
			}
			
			if(!is.null(input$surv_exclude_cens)){
				tmp <- tmp %>%
						filter(!CensorType %in% input$surv_exclude_cens)
			}
			if(!is.null(input$surv_exclude_mort)){
				tmp <- tmp %>%
						filter(!FateDesc %in% input$surv_exclude_mort)
			}
			if(!is.null(input$surv_exclude_sex)){
				tmp <- tmp %>%
						filter(!Sex %in% input$surv_exclude_sex)
			}			
						
		list(out = tmp, surv_start = surv_start)
		}
#################################################################################
		#
		surv_wrapper <- function(x, input, progress = T){
			#  coordinates survival analysis
			#  takes shiny input object and uses elements surv_critter,
			#   surv_year, surv_age, surv_dau
			#  also takes data previously retrieved from database and morphed with
			#  get_survd
			if(progress){
				withProgress(message = "Preparing Data", value = 0.2, {
				
				# generate encounter histories based on x and input
				eh <- make_eh(x, input)

				incProgress(0.3, message = "Data ready, running model...")						
				
				# Call Model
				out <- call_surv( c(0,1), eh , input = input)
				
				setProgress(1, message = "Finished")
				})
			}else{
				# generate encounter histories based on x and input
				eh <- make_eh(x, input)
				
				# Call Model
				out <- call_surv( c(0,1), eh , input = input)
			}
		return( out )
		}
#################################################################################
		#  Two functions to build encounter histories
		surv.MakeEHfawn <- function( sdata, surv_start, input ){
		#
		# generate encounter histories for known fate data
		# 1= live, 2=dead, 0=censor
		#
			uAnimal <- unique(sdata$Animal_ID)
			nAnimal <- length(uAnimal)
			
			eh <- matrix( 0, nAnimal, 7 )
			
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y"),
								1, 0 )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y"),
								2, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y"),
								3, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y"),
								4, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y"),
								5, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y"),
								6, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y"),
								7, sdata$occ )
			
			j <- 1
			for( i in uAnimal ){
				tmpData <- subset( sdata, Animal_ID == i )
				eh[j,tmpData$occ] <- 1
				if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <  as.Date(surv_start,"%m/%d/%Y")){
					eh[j,1] <- 1
				} else if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
					as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(surv_start,"%m/%d/%Y") ){
					eh[j,2] <- 1
				} else if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
					as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
					eh[j,3] <- 1
				} else if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
					as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
					eh[j,4] <- 1
				} else if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
					as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
					eh[j,5] <- 1
				} else if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
					as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
					eh[j,6] <- 1
				} else if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
					as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
					eh[j,7] <- 1
				}
				
				if( !is.na(tmpData$FateDate[1]) && !is.na(tmpData$CensorDate[1]) ){
				if( tmpData$FateDate[1] < tmpData$CensorDate[1] ){
					if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(surv_start,"%m/%d/%Y") ){
						eh[j,1] <- 2
						eh[j,2:7] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,2] <- 2
						eh[j,3:7] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,3] <- 2
						eh[j,4:7] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,4] <- 2
						eh[j,5:7] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,5] <- 2
						eh[j,6:7] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,6] <- 2
						eh[j,7] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,7] <- 2
					}
				} else {
					if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(surv_start,"%m/%d/%Y") ){
						eh[j,1:7] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,2:7] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,3:7] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,4:7] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,5:7] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,6:7] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,7] <- 0
					}
				}
				}
				if( !is.na(tmpData$FateDate[1]) && is.na(tmpData$CensorDate[1]) ){
					if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(surv_start,"%m/%d/%Y") ){
						eh[j,1] <- 2
						eh[j,2:7] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,2] <- 2
						eh[j,3:7] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,3] <- 2
						eh[j,4:7] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,4] <- 2
						eh[j,5:7] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,5] <- 2
						eh[j,6:7] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,6] <- 2
						eh[j,7] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,7] <- 2
					}
				}
				if( is.na(tmpData$FateDate[1]) && !is.na(tmpData$CensorDate[1]) ){
					if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(surv_start,"%m/%d/%Y") ){
						eh[j,1:7] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,2:7] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,3:7] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,4:7] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,5:7] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,6:7] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,7] <- 0
					}
				}
				j <- j+1
			}
			eh <- subset( eh, apply( eh, 1, sum) > 0 )
			if( sum(eh[,1]) == 0 )
				eh <- eh[,2:7]
			return(eh)
		}


		surv.MakeEHadult <- function( sdata, surv_start, input ){
		#
		# generate encounter histories for known fate data
		# 1= live, 2=dead, 0=censor
		#
			uAnimal <- unique(sdata$Animal_ID)
			nAnimal <- length(uAnimal)
			nPer <- 13
			eh <- matrix( 0, nAnimal, nPer )
			
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y"),
								1, 0 )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y"),
								2, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y"),
								3, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y"),
								4, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y"),
								5, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y"),
								6, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y"),
								7, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "7/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y"),
								8, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "8/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "7/1/", input$surv_year, sep="" ), "%m/%d/%Y"),
								9, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "9/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "8/1/", input$surv_year, sep="" ), "%m/%d/%Y"),
								10, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "10/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "9/1/", input$surv_year, sep="" ), "%m/%d/%Y"),
								11, sdata$occ )		
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "11/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "10/1/", input$surv_year, sep="" ), "%m/%d/%Y"),
								12, sdata$occ )
			sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "12/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
								as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "11/1/", input$surv_year, sep="" ), "%m/%d/%Y"),
								13, sdata$occ )
		#	sdata$occ <- ifelse( as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) <  as.Date(paste( "12/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
		#						as.Date(sdata$Date, "%m/%d/%Y", tz="MST" ) >=  as.Date(paste( "12/1/", input$surv_year, sep="" ), "%m/%d/%Y"),
		#						14, sdata$occ )
			j <- 1
			for( i in uAnimal ){
				tmpData <- subset( sdata, Animal_ID == i )
				eh[j,tmpData$occ] <- 1
				if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <  as.Date(surv_start,"%m/%d/%Y")){
					eh[j,1] <- 1
				} else if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
					as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(surv_start,"%m/%d/%Y") ){
					eh[j,2] <- 1
				} else if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
					as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
					eh[j,3] <- 1
				} else if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
					as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
					eh[j,4] <- 1
				} else if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
					as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
					eh[j,5] <- 1
				} else if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
					as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
					eh[j,6] <- 1
				} else if( as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
					as.Date(tmpData$Capture_Date[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
					eh[j,7] <- 1
				}
				
				if( !is.na(tmpData$FateDate[1]) && !is.na(tmpData$CensorDate[1]) ){
				if( tmpData$FateDate[1] < tmpData$CensorDate[1] ){
					if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(surv_start,"%m/%d/%Y") ){
						eh[j,1] <- 2
						eh[j,2:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,2] <- 2
						eh[j,3:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,3] <- 2
						eh[j,4:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,4] <- 2
						eh[j,5:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,5] <- 2
						eh[j,6:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,6] <- 2
						eh[j,7:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,7] <- 2
						eh[j,8:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "7/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,8] <- 2
						eh[j,9:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "8/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "7/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,9] <- 2
						eh[j,10:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "9/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "8/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,10] <- 2
						eh[j,11:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "10/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "9/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,11] <- 2
						eh[j,12:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "11/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "10/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,12] <- 2
						eh[j,13:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "12/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "11/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,13] <- 2
		#				eh[j,nPer] <- 0
					} #else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "12/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
		#				as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "12/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
		#				eh[j,14] <- 2
		#			}
				} else {
					if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(surv_start,"%m/%d/%Y") ){
						eh[j,1:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,2:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,3:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,4:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,5:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,6:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,7:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "7/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,8:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "8/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "7/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,9:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "9/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "8/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,10:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "10/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "9/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,11:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "11/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "10/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,12:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "12/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "11/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,13:nPer] <- 0
					} #else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "12/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
		#				as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "12/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
		#				eh[j,14] <- 0
		#			}
				}
				}
				if( !is.na(tmpData$FateDate[1]) && is.na(tmpData$CensorDate[1]) ){
					if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(surv_start,"%m/%d/%Y") ){
						eh[j,1] <- 2
						eh[j,2:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,2] <- 2
						eh[j,3:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,3] <- 2
						eh[j,4:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,4] <- 2
						eh[j,5:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,5] <- 2
						eh[j,6:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,6] <- 2
						eh[j,7:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,7] <- 2
						eh[j,8:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "7/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,8] <- 2
						eh[j,9:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "8/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "7/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,9] <- 2
						eh[j,10:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "9/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "8/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,10] <- 2
						eh[j,11:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "10/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "9/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,11] <- 2
						eh[j,12:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "11/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "10/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,12] <- 2
						eh[j,13:nPer] <- 0
					} else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "12/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "11/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,13] <- 2
			#			eh[j,nPer] <- 0
					}# else if( as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "12/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
		#				as.Date(tmpData$FateDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "12/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
		#				eh[j,14] <- 2
		#			}
				}
				if( is.na(tmpData$FateDate[1]) && !is.na(tmpData$CensorDate[1]) ){
					if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(surv_start,"%m/%d/%Y") ){
						eh[j,1:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,2:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "1/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,3:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "2/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,4:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "3/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,5:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "4/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,6:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "5/15/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,7:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "7/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "6/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,8:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "8/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "7/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,9:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "9/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "8/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,10:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "10/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "9/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,11:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "11/1/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "10/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,12:nPer] <- 0
					} else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "12/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
						as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "11/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
						eh[j,13:nPer] <- 0
					} #else if( as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) <   as.Date(paste( "12/15/", input$surv_year, sep="" ), "%m/%d/%Y") &
		#				as.Date(tmpData$CensorDate[1], "%m/%d/%Y", tz="MST" ) >=    as.Date(paste( "12/1/", input$surv_year, sep="" ), "%m/%d/%Y") ){
		#				eh[j,14] <- 0
		#			}
				}
				j <- j+1
			}
			eh <- subset( eh, apply( eh, 1, sum) > 0 )
		#	if( sum(eh[,1]) == 0 )
		#		eh <- eh[,2:nPer]
			if( input$surv_season == "Summer" ){
				eh <- eh[,7:9]
			} else if( input$surv_season == "Winter" ){
				eh <- eh[,1:6]
			} else if( input$surv_season == "Fall" ){
				eh <- eh[,10:13]
			}
			return(eh)
		}
		
#################################################################################
		#  JAGS wrapper
		#  pp is a placeholder for models with covariates
		
		call_surv <- function( pp, eh, DM=NA, input ){ 
			#
			#  Call JAGS for Bayesian known fate discrete time survival 
			#  estimation with random effect for occasion
			#  arguments: pp - survival parameter vector
			#		      eh - matrix of encounter histories 
			#					1 = live 
			#					0 = dead
			#					NA = not observed (e.g. pre-capture)
			#			  DM - design matrix, defaults to NA
			#  return: JAGS object
		
#			if( is.na(DM) ){
#				#  If no design matrix is provided use an identity
#				DM <- diag( rep(1, length(pp) ) ) 
#			}
#			
#			S <- 1/(1+exp(-DM%*%pp))	# logit transformation

			#  Check that iter is > burnin
			if(input$surv_burnin > input$surv_niter){
				surviter <- input$surv_burnin
				survburn <- round(input$surv_burnin/2)
			}else{
				surviter <- input$surv_niter
				survburn <- input$surv_burnin				
			}

			#  Data
			dat <- list(eh = eh$y, 
						nind = eh$nind, 
						nocc = eh$nocc,
						first = eh$first,
						last = eh$last)

			#  Model name
			model_name <- "custom/models/surv/s_r_c_n.txt"
							
			#  Parameters to track
			parms <- c("b0", "tau", "Sann", "Smon", "monthly")
			
			#  Initial Values
			inits <- function() {list(
				tau = runif( 1, 0, 1 ),
				b0 = rnorm( 1, 0, 1 )
			)}
			
			#  Call the model
			out <- try(jags(data = dat, 
							inits = inits, 
							parameters.to.save = parms, 
							model.file = model_name, 
							n.chains = 3, 
							n.iter = surviter, 
							n.burnin = survburn, 
							n.thin = input$surv_thin,
							progress.bar = "none"), silent = T)
								
			#  Add relevant information to out object, used later for 
			#  plotting and report generation
			if(class(out) != "try-error"){
				out$critter <- input$surv_critter
				out$dau <- input$surv_dau
				out$year <- input$surv_year
				out$age <- input$surv_age
				out$season <- input$surv_season
				out$burn <- survburn
				out$iter <- surviter
				out$thin <- input$surv_thin
			}
								
		return( out )
		}
#################################################################################
      make_eh <- function(tel.dat, input, ms = F){
		#  A function to build encounter histories from summaries of collar
		#  deployments
		#  Takes output of get_survd
		#  Returns encounter histories with one row per animal
        
		#  Throw out future dates and animals without id
        x <- tel.dat$out %>%
				filter(Capture_Date < as.Date(Sys.time()) & Animal_ID != "" &
						Animal_ID != " " & !is.na(Animal_ID))
        
		#  Create sequence == number of columns
		if(input$surv_age == "Fawn"){
			cols <- as.Date(c(paste("12/15/", as.numeric(input$surv_year)-1),
							paste(c("1/1/", "1/15/", "2/15/", "3/15/", "4/15/", 
									"5/15/", "6/1/"), input$surv_year, 
									sep = "")), "%m/%d/%Y") 
        }
		
		if(input$surv_age == "Adult"){
			cols.tmp <- as.Date(paste(c("1/1/", "1/15/", "2/15/", "3/15/",
									"4/15/", "5/15/", "6/1/", "7/1/", "8/1/",
									"9/1/", "10/1/", "11/1/", "12/14/"),
									input$surv_year), "%m/%d/%Y")
			cols <- switch(input$surv_season,
							"Annual" = cols.tmp,
							"Summer" = cols.tmp[7:length(cols.tmp)],
							"Winter" = cols.tmp[1:7],
							"Fall" = cols.tmp[10:length(cols.tmp)])
        }

		#  Get first/capture occasion for each animal
		if(input$surv_age == "Fawn"){
			cap <- x %>%
					group_by(Animal_ID) %>%
					summarise(first = min(Capture_Date)) %>%
					mutate(first_occ = cut(first, cols, 
											labels = 1:(length(cols)-1)))
        }
		
		if(input$surv_age == "Adult"){
			cap <- x %>%
					group_by(Animal_ID) %>%
					summarise(first = min(Capture_Date)) %>%
					mutate(first_occ = ifelse(first < cols[1], 
										1,
										cut(first, cols, 
											labels = 1:(length(cols)-1))))
        }
		
		#  Get last occasion for each animal
		fate <- x %>%
				group_by(Animal_ID) %>%
				summarise(date = max(Date, na.rm = T),
							fate = min(FateDate, na.rm = T),
							cens = min(CensorDate, na.rm = T)) %>%
				mutate(fate_date = NA)

		#  Ugly switch of syntax, but most readable solution to date		
		fate$fate_date <- as.Date(apply(fate, 1, function(x){
			
			if(is.na(x["fate"]) & is.na(x["cens"])){
				fate_date <- x["date"]
			}
			if(!is.na(x["fate"]) & is.na(x["cens"])){
				fate_date <- x["fate"]
			}
			if(is.na(x["fate"]) & !is.na(x["cens"])){
				fate_date <- min(x["date"], x["cens"], na.rm = T)
			}
			if(!is.na(x["fate"]) & !is.na(x["cens"])){
				fate_date <- min(x["fate"], x["cens"], na.rm = T)
			}
			
		fate_date
		}), origin = "1970-01-01")
		
		#  Add indicator for animals that died, used to determine value of last
		#  occ in eh
		fate <- fate %>%
					mutate(dead = ifelse(fate_date == fate, 1, 0))
		fate$dead[is.na(fate$fate)] <- 0
		
		#  Return occassion for buildling eh
		fate <- fate %>%
				mutate(fate_occ = cut(fate_date, 
										cols,
										labels = 1:(length(cols)-1)),
						fate_occ = ifelse(is.na(fate_occ), length(cols), 
											fate_occ))

		#  Model will fail if data is missing dates
		hold <- data.frame(first_occ = cap$first_occ, 
                       fate_occ = fate$fate_occ, 
                       dead = fate$dead) %>%
				filter(!is.na(first_occ) & !is.na(fate_occ) & !is.na(dead)) %>%
				select(first_occ, fate_occ, dead)
					
		#  Convert parameters to numeric vectors to save typing
		first <- as.numeric(as.character(hold$first_occ))
		dead <- as.numeric(as.character(hold$dead))
		last <- as.numeric(as.character(hold$fate_occ))
		rows <- nrow(hold)
					
        #  Create matrix
        if(ms == F){
          mat <- matrix(NA, nrow = rows, ncol = length(cols))
          for(i in 1:rows){
			if(first[i] == last[i] & last[i] < length(cols)){
				last[i] <- last[i] + 1
			}
            mat[i, first[i]:last[i]] <- 1
            if(last[i] < length(cols)){
              if(as.numeric(dead[i]) == 1){
                mat[i, last[i]] <- 0
              }
            }
          }
        }
        # if(ms == T){
          # n.fates <- length(table(tel.dat[,"fatecode"])) + 1
          # mat <- matrix(NA, nrow = rows, ncol = length(cols))
          # for(i in 1:rows){
            # mat[i, cap[i]:last[i]] <- 1
            # if(last[i] < length(cols)){
              # if(as.numeric(tel.dat[i, "dead"]) == 1){
                # mat[i,last[i]] <- as.numeric(tel.dat[i,"fatecode"])
                # mat[i,(last[i]+1):length(cols)] <- n.fates
                # last[i] <- length(cols)
              # }
              # if(as.numeric(tel.dat[i, "dead"]) == 0){
                # mat[i,(last[i]+1):length(cols)] <- NA
              # }
            # }
          # }
        # }
        # colnames(mat) <- paste("X", seq(min.date, max.date, by = step.time),
                                # sep = "")
        #  Create year variable
#        yr <- as.numeric(format.POSIXct(seq(min.date, max.date, by = step.time),
#            "%Y")) - (as.numeric(format.POSIXct(min.date, "%Y")) - 1) + age.inc
        #  Create sex indicator
#        sex <- ifelse(str_detect(tel.dat$sex, ignore.case("F")), 0, 1)
        #  Return a list
        list("first" = first, 
      			"last" = last, 
			    "y" = mat,
				"nind" = rows,
				"nocc" = length(cols),
				"dead" = dead)
      }