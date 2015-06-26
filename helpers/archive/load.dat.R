      #  Script to load all data for chosen species
      #  Species chosen on Home screen of gui
      load.dat <- function(sp, ctrl = ctrl){
        ctrl$species <<- sp
        #  Navigate to species data
        
        #  Load capture and mortality records
        cap <- read.csv("data/StatewideFawnResearch2013.csv", 
                         as.is = T, header = T)
        mort <- read.csv("data/StatewideFawnMortality2013.csv", 
                         as.is = T, 
                         header = T)
        #  Fawn survival
        source("helpers/manip.yoy.S.deer.R")
        manip.yoy.S.deer(cap, mort)
        
        
      }