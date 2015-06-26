      #  Function to make known fate data from summaries of individual
      #    encounter histories (i.e. the period of monitoring by collar)
      #  03/20/2013
      #  Author:  Josh Nowak
################################################################################
      #  Requires:
      #  tel.dat a data frame with...
        #  1) a column called start formatted as POSIXct, tz = "GMT", this is
        #     the capture date of the animal
        #  2) a column called end formatted as POSIXct, tz = "GMT", this is
        #     the last time the animal was seen dead or alive, sometimes
        #     called the censor date or fate date, etc.
        #  3) a column called id formatted as a character or numeric
        #  4) a column named dead that denotes with a 1 if the individual was
        #     dead at the last observation or if it should be censored 0
        #  5) if known fate multi-state data is desired then a column called
        #     fatecode with codes from 1 to the number of unique fate types, in
        #     this case call the function with ms set to TRUE.  The fatecode
        #     column should treat alive as equal to 1.  For example, if the
        #     animals is censored then its fate is 1, if it died by hunting 2,
        #     if other 3.
        
        #  Other columns, minus x and y coordinates, are added to the end of the
        #  matrix so they may be used later as covariates

        #  step.time is the time interval at which individuals were monitored
        #  possible options include: "sec", "min", "hour", "day", "DSTday",
        #  "week", "month" or "year", in this context only week, month and year
        #  make sense.

        #  Animals without a unique id will be eliminated from the data

        #  NA will preceed the "active" portion of the encounter history
        #  If the animal is censored NA will succeed the encounter history, but
        #  if the animal died then 0 will succeed the encounter history and
        #  last will be equal to the number of time occassions of the study.
        #  In the multi state format the number of unique states +1 will
        #  succeed the active encounter history as this represents an abosorbing
        #  state.

        #  In the output, last references the last occassion on which the
        #  animal's fate is known, so for those animals that died it is the end
        #  of the study and last.occ is the actual last observation,
        #  which is equivalent to the date of death or censoring....different
        #  models may require different end points
################################################################################
      make_eh <- function(tel.dat, input, ms = F){
		#  Takes output of get_survd
		#  Returns encounter histories with one row per animal
        
		#  Throw out future dates and animals without id
        x <- tel.dat$out %>%
				filter(Capture_Date < as.Date(Sys.time()) & Animal_ID != "" &
						Animal_ID != " " & !is.na(Animal_ID))
        
		#  Create sequence == number of columns
		if(input$surv_age == "Fawn"){
			cols <- as.Date(c(paste("12/15/", input$surv_year-1),
							paste(c("1/1/", "1/15/", "2/15/", "3/15/", "4/15/", 
									"5/15/", "6/1/"), input$surv_year, 
									sep = "")), "%m/%d/%Y") 
        }
		
		if(input$surv_age == "Adult"){
			cols <- as.Date(paste(c("1/1/", "1/15/", "2/15/", "3/15/",
									"4/15/", "5/15/", "6/1/", "7/1/", "8/1/",
									"9/1/", "10/1/", "11/1/", "12/15/"),
									input$surv_year), "%m/%d/%Y")
        }

		#  Unique id index
        uid <- unique(x$Animal_ID)
        
		#  Find number of rows
        rows <- length(uid)
		
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
						fate_occ = ifelse(is.na(fate_occ), length(cols)-1, 
											fate_occ))

        #  Model will fail if data is missing dates
		cap <- cap %>%
				filter(!is.na(first_occ)) %>%
				select(first_occ)
		dead <- fate %>%
					filter(!is.na(dead)) %>%
					select(dead)
		last <- fate %>%
					filter(!is.na(fate_occ)) %>%
					select(fate_occ)
					
		#  Convert parameters to numeric vectors to save typing
		first <- as.numeric(as.character(cap$first_occ))
		dead <- as.numeric(as.character(dead$dead))
		last <- as.numeric(as.character(last$fate_occ))		
					
        #  Create matrix
        if(ms == F){
          mat <- matrix(NA, nrow = rows, ncol = length(cols))
          for(i in 1:rows){
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
        rownames(mat) <- uid

        #  Create year variable
#        yr <- as.numeric(format.POSIXct(seq(min.date, max.date, by = step.time),
#            "%Y")) - (as.numeric(format.POSIXct(min.date, "%Y")) - 1) + age.inc
        #  Create sex indicator
#        sex <- ifelse(str_detect(tel.dat$sex, ignore.case("F")), 0, 1)
        #  Return a list
        list("first" = cap, 
				"last" = last, 
				"y" = mat,
				"nAnimal" = rows)
      }
