		#  Functions to summarize JAGS output create table, plot and markdown
		#  document
		#  Survival Models
		#  Josh Nowak
		#  03/2015
#################################################################################
		#  Summarize
		summ_surv <- function(out){
			#  Takes a jags object and returns a data frame with columns for 
			#  each parameter's mean and quantiles where each row is a year
			x <- out$BUGS$sims.list
			#  Summarize each parameter
			nms <- names(x)
			xx <- lapply(1:length(x), function(i){
				tmp <- t(apply(x[[i]], 2, quantile, c(0.025, 0.5, 0.975), 
							na.rm = T))
			tmp
			})
		
			names(xx) <- nms
			xx$critter <- out$critter
			xx$dau <- out$dau
			xx$year <- out$year
			xx$age <- out$age
			xx$season <- out$season
			xx$burn <- out$burn
			xx$iter <- out$iter
			xx$thin <- out$thin
		
		xx				
		}
#################################################################################
		#  Table
		mark_surv_tbl <- function(x){
			#  Takes a summarized JAGS object and creates a table with a column 
			#  for each parameter's lower cri, median and upper cri.  
			
			#  Create row names using standardized dates
			if(x$age == "Fawn"){
				cols <- as.Date(c(paste("12/15/", as.numeric(x$year)-1),
								paste(c("1/1/", "1/15/", "2/15/", "3/15/", 
										"4/15/", "5/15/", "6/1/"), 
										x$year, sep = "")), "%m/%d/%Y") 
			}
			
			if(x$age == "Adult"){
				cols.tmp <- as.Date(paste(c("1/1/", "1/15/", "2/15/", "3/15/",
										"4/15/", "5/15/", "6/1/", "7/1/", "8/1/",
										"9/1/", "10/1/", "11/1/", "12/14/"),
										x$year), "%m/%d/%Y")
				cols <- switch(x$season,
								"Annual" = cols.tmp,
								"Summer" = cols.tmp[7:length(cols.tmp)],
								"Winter" = cols.tmp[1:7],
								"Fall" = cols.tmp[10:length(cols.tmp)])
			}

			#  Put into data.frame
			out <- data.frame("Period" = c(as.character(cols), "Monthly Mean", 
											"Study Period"), 
								"LowerCI" = round(c(x$monthly[,1], 
													x$Smon[,1],
													x$Sann[,1]), 2), 
								"Median" = round(c(x$monthly[,2], 
													x$Smon[,2],
													x$Sann[,2]), 2),
								"UpperCI" = round(c(x$monthly[,3], 
													x$Smon[,3],
													x$Sann[,3]), 2))
			
		out
		}
#################################################################################
		#  Plots
		mark_surv_ts <- function(x){
			#  Takes summarized JAGS survival model output
			#  Returns time series plot of survival
			
			#  Create row names using standardized dates
			if(x$age == "Fawn"){
				cols <- as.Date(c(paste("12/15/", as.numeric(x$year)-1),
								paste(c("1/1/", "1/15/", "2/15/", "3/15/", 
										"4/15/", "5/15/", "6/1/"), 
										x$year, sep = "")), "%m/%d/%Y") 
			}
			
			if(x$age == "Adult"){
				cols.tmp <- as.Date(paste(c("1/1/", "1/15/", "2/15/", "3/15/",
										"4/15/", "5/15/", "6/1/", "7/1/", "8/1/",
										"9/1/", "10/1/", "11/1/", "12/14/"),
										x$year), "%m/%d/%Y")
				cols <- switch(x$season,
								"Annual" = cols.tmp,
								"Summer" = cols.tmp[7:length(cols.tmp)],
								"Winter" = cols.tmp[1:7],
								"Fall" = cols.tmp[10:length(cols.tmp)])
			}

			pin <- data.frame(Month = cols, 
								LCI = x$monthly[,1], 
								Median = x$monthly[,2], 
								UCI = x$monthly[,3])

			yy <- c(min(pin$LCI) * 0.95, max(pin$UCI))
								
			ggplot(pin) + 
				ylim(yy) +
				geom_line(aes(x = Month, y = Median), colour = "dodgerblue4") +
				geom_ribbon(aes(x = Month, ymax = UCI, ymin = LCI), alpha = 0.2)+
				theme_bw() +
				ylab("") +
				xlab("") + 
				theme(legend.position = "none") + 
				theme(panel.border = element_blank(),
						axis.line = element_line(color = "black"))
		}	
		mark_surv_post <- function(x, parm, fill_col = "darkgreen"){
			#  Plot of posterior of a single parameter
			#  Takes raw JAGS output object
			#  Returns density plot 
			
			#  Extract values of parameter
			d <- as.data.frame(x$BUGS$sims.list[[parm]])
			colnames(d) <- "values"
			
			ggplot(d, aes(x = values)) + 
				geom_density(colour = NA, fill = fill_col, alpha = 0.2) + 
				theme_bw() +
				ylab("Relative Support") +
				xlab("Survival Probability") +
				theme(legend.position = "none") + 
				theme(panel.border = element_blank(),
						axis.line = element_line(color = "black"))
				
				
		
		}
		mark_surv_pmort <- function(x, censor = T){
			#  Takes surv_subdata()
			#  Returns pie charts of censor and mortality proportions
			if(censor){
				tmp <- table(x$out$CensorType)
			}else{
				tmp <- table(x$out$FateDesc)
			}
			tab <- tmp[names(tmp) != ""]
			dat <- data.frame("Cause" = names(tab),
								"Frequency" = as.numeric(tab),
								stringsAsFactors = F)
			rownames(dat) <- NULL
			gvisPieChart(dat, options = list(gvis.editor = "Edit", 
											 theme = "maximized",
											 legend = "{position: 'right'}"))
		
		}
#################################################################################