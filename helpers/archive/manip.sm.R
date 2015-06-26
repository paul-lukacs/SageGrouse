      #  Manipulate separate survival capture and mortality files to create a
      #  single row for each animal.
      manip.sm <- function(cap, mort){
        
        #  miss.fun takes a vector of "any" class and fills in missing values 
        #  with NA.  The second argument rand, when true, will sample missing
        #  values from the observed frequency of values in the data.
        miss.fun <- function(x, rand = F){
          if(rand){
            prop <- table(x)/length(x)
            n <- sum(x %in% c("", " ", "NULL", NULL, NaN))
            x[x %in% c("", " ", "NULL", NULL, NaN)] <- sample(sort(unique(x)), 
                                                 n, 
                                                 prob = prop, 
                                                 replace = T)
          }else{
            x[x %in% c("", " ", "NULL", NULL, NaN)] <- NA
          }
          return(x)
        }
        
        #########################  Capture  ####################################
        #  Capture date, missing values get NA
        cap.date <- strptime(miss.fun(cap$Date, F), "%m/%d/%Y", tz = "GMT")
        #  Capture region next, missing values get sampled value
        rand.reg <- miss.fun(cap$Region, rand = T)
        cap.region <- as.numeric(regmatches(rand.reg, 
                                            regexpr("[1-4]", rand.reg)))
        
        #  Capture species next, missing values get sampled value
        cap.species <- miss.fun(cap$Species, rand = T)
        cap.species[grep("[w]", cap.species, ignore.case = T)] <- "Whitetail Deer"
        
        #  Capture frequency
        cap$Frequency[cap$Frequency < 140] <- NA
        cap$Frequency[cap$Frequency > 155 & nchar(cap$Frequency) == 6] <- 
          paste(substr(cap$Frequency, 1, 3), 
                substr(cap$Frequency, 4, 6),
                sep = ".")
        cap$Frequency[cap$Frequency > 155 & nchar(cap$Frequency) != 6] <- NA
        cap.freq <- cap$Frequency
        
        #  Mother collared, categorical
        cap.momtag <- ifelse(as.numeric(cap$Collared.Mother.Frequency) > 0, 1, 0)
        
        #  Sex, missing values sampled
        cap.sex <- miss.fun(cap$Sex, rand = T)
        
        #  Hoof length
        cap.hoof <- miss.fun(cap$Hoof, rand = F)
        
        #  Weight
        cap.weight <- miss.fun(cap$Weight, rand = F)
        
        #  Age
        cap.age <- as.factor(regmatches(cap$Age, 
                              regexpr("[1-5]", miss.fun(cap$Age, rand = F))))
        
        #  Habitat Location
        cap.habitat <- as.factor(cap$Habitat.Location)
        
        #  Nocturnal Capture
        cap.nocturnal <- as.factor(cap$NocturnalCapture)
        
        #  Twin?
        cap.twin <- rep(0, nrow(cap))
        cap.twin[grep("[twin|twins|Twin|Twins|sibling|brother|sister]", 
                      cap$comments)] <- 1
        
        #  Capture latitude
        cap.lat <- miss.fun(cap$Latitude, rand = F)
        
        #  Capture longitude
        cap.long <- miss.fun(cap$Longitude, rand = F)
        
        ##########################  Mortality  #################################
        #  Remove duplicates
        mort <- mort[!duplicated(mort[,-1]),]
        
        #  Mortality date
        mort.date <- strptime(mort$Date, "%m/%d/%Y", tz = "GMT")
        
        #  Mortality region
        rand.reg <- miss.fun(mort$Region, rand = T)
        mort.reg <- as.factor(regmatches(rand.reg, 
                                          regexpr("[1-4]", rand.reg)))
        #  Mortality frequency
        mort$Frequency[mort$Frequency < 140] <- NA
        mort$Frequency[mort$Frequency > 155 & nchar(mort$Frequency) == 6] <- 
          paste(substr(mort$Frequency, 1, 3), 
                substr(mort$Frequency, 4, 6),
                sep = ".")
        mort$Frequency[mort$Frequency > 155 & nchar(mort$Frequency) != 6] <- NA
        mort.freq <- as.numeric(mort$Frequency)
        
        #  Mortality, cause of death
        mort.cause <- as.factor(miss.fun(mort$Cause.of.Death, rand = F))
        mc <- mort.cause
        levels(mc) <- c(1, 3, 2, 1, 1, 1, 1)

        #  Mortality latitude
        mort.lat <- as.numeric(miss.fun(mort$Latitude, rand = F))
        
        #  Mortality longitude
        mort.long <- as.numeric(miss.fun(mort$Longitude, rand = F)) 
        
        #######################  Combine the two datasets  #####################
        #  Create a capture data.frame and add mortality information when it 
        #  matches the capture records
        sdat <- data.frame(cap.freq = as.numeric(cap.freq), 
                           cap.region = as.factor(cap.region), 
                           cap.date = as.POSIXct(cap.date), 
                           cap.age = cap.age, 
                           sex = as.factor(cap.sex),
                           species = as.factor(cap.species), 
                           cap.lat  = cap.lat, 
                           cap.long = cap.long, 
                           cap.habitat = cap.habitat, 
                           cap.weight = as.numeric(cap.weight), 
                           cap.hoof = as.numeric(cap.hoof), 
                           momtag = cap.momtag, 
                           cap.nocturnal = as.numeric(cap.nocturnal) - 1, 
                           twin = cap.twin                            
                           )
        
        #  add columns for mortality information
        sdat$mort.date <- sdat$mort.cause <- sdat$mort.code <- sdat$mort.lat <-
          sdat$mort.long <- sdat$mort.freq <- NA
        #  declare classes, necessary for dates
        sdat$mort.date <- as.POSIXct(sdat$mort.date)
        #  match and fill columns
        for(i in 1:nrow(sdat)){
          tmp <- mort.freq %in% sdat$cap.freq[i]
          if(sum(tmp) == 0){
            next
          }else{
            if(sum(tmp) > 1){
              print(which(tmp))
            }
            if(sum(tmp) == 1){
              sdat$mort.date[i] <- as.POSIXct(mort.date[which(tmp)])
              sdat$mort.cause[i] <- as.character(mort.cause[which(tmp)])
              sdat$mort.code[i] <- mort.code[which(tmp)]
              sdat$mort.lat[i] <- mort.lat[which(tmp)]
              sdat$mort.long[i] <- mort.long[which(tmp)]
              sdat$mort.freq[i] <- mort.freq[which(tmp)] 
            }
          }
        }
        
        #  Column for censoring
        sdat$dead <- ifelse(sdat$mort.code %in% c(1, 3), 1, 0)
        sdat$dead[is.na(sdat$mort.code)] <- NA
        
        #  subset data to "known fate" and species of interest
        out <- subset(sdat, !is.na(sdat$mort.freq) &
                        sdat$species == ctrl$species, drop = T)
        
        
        #  return fawn survival data
        assing("ctrl$Yoy.Survival", out, envir = .GlobalEnv)
        
        NULL
      }