      #  Dygraphs for IPM setup, harvest
      #  Josh Nowak
      #  01/2015
################################################################################
#       library("xts")
#       library("dygraphs")
#       #  For development
#       idfgDbName <- "C:/IDFG/IDFGdatabase.accdb"
#       modelCon <- odbcConnectAccess2007(idfgDbName)                    
#       queryTxt <- "SELECT * FROM tblPopModelData WHERE species='MD'"
#       tmp <- sqlQuery( modelCon, queryTxt )
#       tmp2 <- tmp[order(tmp$DAU, tmp$Year),]
#       mdout <- tmp2[tmp2$DAU == 5,]
#       odbcClose(modelCon)
#      x <- mdout
################################################################################
      harv_dygraph <- function(x, sex, input){
        yrdiff <- input$year[2] - max(x$Year)
        
        if(sex == "male"){
        
          inobs <- xts(x[,"MaleHarvest"],
            order.by = strptime(paste("01-01-", x$Year, sep = ""), "%m-%d-%Y"))
          colnames(inobs) <- "Male Harvest"  
          inpred <- xts(rep(input$mharv, yrdiff),
            order.by = strptime(paste("01-01-",
                                max(x$Year) + 1:yrdiff, 
                                sep = ""), "%m-%d-%Y"))
          ip <- cbind(inobs, inpred)
          colnames(ip) <- c("Observed", "Proposed")
          out <- dygraph(ip, main = "Male Harvest") %>%
            dySeries("Observed", fillGraph = T) %>%
            dySeries("Proposed", fillGraph = T)
          
        }else{
          inobs <- xts(x[,"FemaleHarvest"],
            order.by = strptime(paste("01-01-", x$Year, sep = ""), "%m-%d-%Y"))          
          colnames(inobs) <- "Female Harvest"
          inpred <- xts(rep(input$fharv, yrdiff),
            order.by = strptime(paste("01-01-", 
                                max(x$Year) + 1:yrdiff, 
                                sep = ""), "%m-%d-%Y"))
          ip <- cbind(inobs, inpred)
          colnames(ip) <- c("Observed", "Proposed")
          out <- dygraph(ip, main = "Female Harvest") %>%
            dySeries("Observed", fillGraph = T) %>%
            dySeries("Proposed", fillGraph = T)
        }
      print(out)             
      }
################################################################################
     harv_gg <- function(x, sex, input){

       yrdiff <- input$year[2] - max(x$Year, na.rm = T)
       yrs <- c(x$Year, max(x$Year) + 1:yrdiff)

       if(sex == "male"){
         
         inobs <- data.frame(Year = format(strptime(paste("01-01-", yrs, 
                                                          sep = ""), 
                                             "%m-%d-%Y"), "%Y"), 
                             Observed = c(x[,"MaleHarvest"], 
                                          rep(input$mharv, yrdiff)),
                             group = c(rep(1, length(x$Year)), rep(2, yrdiff)),
                             stringsAsFactors = F)

         out <- ggplot(inobs, aes(x = Year, y = Observed)) +
           geom_line(aes(group = group, colour = group)) +
           geom_point(aes(colour = group)) +
           xlab("") +
           ylab("") +
		   ggtitle("Male Harvest") +
           theme(axis.text.x = element_text(angle = 90, hjust = 1),
                 legend.position = "none")
       
       }
       if(sex == "female"){
         
         inobs <- data.frame(Year = format(strptime(paste("01-01-", yrs, 
                                                          sep = ""), 
                                                    "%m-%d-%Y"), "%Y"), 
                             Observed = c(x[,"FemaleHarvest"], 
                                          rep(input$fharv, yrdiff)),
                             group = c(rep(1, length(x$Year)), rep(2, yrdiff)),
                             stringsAsFactors = F)
         out <- ggplot(inobs, aes(x = Year, y = Observed)) +
           geom_line(aes(group = group, colour = group)) +
           geom_point(aes(colour = group)) +
           xlab("") +
           ylab("") +
		   ggtitle("Female Harvest") +
           theme(axis.text.x = element_text(angle = 90, hjust = 1),
                 legend.position = "none")
       }
     out
     }