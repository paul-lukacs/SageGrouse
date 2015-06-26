		#  Functions to summarize JAGS output create table, plot and markdown
		#  document
		#  Josh Nowak
		#  01/2015
#################################################################################
		#  Summarize
		summ_ipm <- function(out){
			#  Takes a jags object and returns a data frame with columns for 
			#  each parameter's mean and quantiles where each row is a year
			x <- out$BUGS$sims.list
			#  Summarize each parameter
			nms <- names(x)
			xx <- lapply(1:length(x), function(i){
				if(any(nms[i] %in% c("mf", "yf"))){
					tmp <- t(apply(x[[i]]*100, 2, quantile, c(0.025, 0.5, 0.975), 
						na.rm = T))				
				}else{
					tmp <- t(apply(x[[i]], 2, quantile, c(0.025, 0.5, 0.975), 
						na.rm = T))
				}
			tmp
			})
			
		attributes(xx)$year <- out$year	
		names(xx) <- nms
		xx				
		}
#################################################################################
		#  Table
		table_ipm <- function(x){
			#  Takes a jags object and creates a table with a column for each
			#  parameter's mean and standard deviation.  
			mu <- x$BUGS$mean
			sds <- x$BUGS$sd
			
			out1 <- data.frame(
				Yoy = mu$Ny, Yoy.SD = sds$Ny,
				Female = mu$Nf, Female.SD = sds$Nf,
				Male = mu$Nm, Male.SD = sds$Nm,
				Total = mu$Ntot, Total.SD = sds$Ntot)
			out2 <- data.frame(Lambda = mu$lambda, Lambda.SD = sds$lambda,
				Recruitment = mu$R, Rec.SD = sds$R,
				Surv.Juv = mu$jS, SJ.SD = sds$jS,
				Surv.Female = mu$fS, SF.SD = sds$fS,
				Surv.Male = mu$mS, SM.SD = sds$mS,
				YoungFemale = mu$yf, YF.SD = sds$yf,
				MaleFemale = mu$mf, MF.SD = sds$mf)
			out <- data.frame(Year = x$year[1]:x$year[2],
								round(out1), 
								round(out2, 2))
			
		out
		}
#################################################################################
		#  Plots
		#  Abundance
		ipm_plot_N <- function(x, dat, input){
			#  Takes output of summ_ipm
			#  Create plot, returns NULL
			#  Format model output
			tmp_yrs <- attributes(x)$year
			yrs <- tmp_yrs[1]:tmp_yrs[2]
			pin <- data.frame(yr = yrs, round(x$Ntot))
			colnames(pin) <- c("yr", "low", "mid", "hi")
			
			yy <- c(min(pin$low)*0.9, max(pin$hi)*1.1)

			#  Format raw data
			raw <- data.frame(x = (yrs[1] - 1) + dat$airyr, y = dat$counts,
								se = 1/sqrt(dat$nse))

			gn <- ggplot(pin, aes(x = yr, y = mid)) + 
				ylim(yy) +
				geom_line() +
				geom_ribbon(aes(ymax = hi, ymin = low), alpha = 0.2) + 
				theme_bw() +
				ylab("") +
				xlab("") + 
				theme(panel.border = element_blank(),
						axis.line = element_line(color = "black"))
			if(input$ipm_nfd){
				if(input$ipm_nserr){
					gn <- gn + geom_pointrange(data = raw, aes(x=x, y=y, 
							ymin = y - 1.96 * se, ymax = y + 1.96 * se), 
							colour = "blue")			
				}else{
					gn <- gn + geom_point(data = raw, aes(x=x, y=y), 
							colour = "blue")
				}
			}

		gn					
		}	
	
		# plot_ipmN <- function(x){
			# #  Takes output of summ_ipm and plots the time-series' using dygraphs
			# pdat <- do.call(cbind, x[c("Ny", "Nf", "Nm")])
			# yrs <- attributes(x)$year[1]:attributes(x)$year[2]
			# cur_yr <- paste(as.POSIXlt(Sys.time())$year + 1900, "01-01", 
				# sep = "-")
			# last_yr <- paste(attributes(x)$year[2], "01-01", 
				# sep = "-")
			# pdatin <- xts(pdat,
				# order.by = as.yearmon(paste("01-01-", yrs, sep = ""), 
					# "%m-%d-%Y"))
			# colnames(pdatin) <- c(paste(rep(c("Ny", "Nf", "Nm"), each = 3), 
									# c("low", "mid", "hi"), sep = "."))
			# #  Dygraph
			# dygraph(pdatin, main = "Abundance") %>%
				# dySeries(c("Ny.low", "Ny.mid", "Ny.hi"), 
					# label = "N YOY") %>%
				# dySeries(c("Nf.low", "Nf.mid", "Nf.hi"), 
					# label = "N Female") %>%
				# dySeries(c("Nm.low", "Nm.mid", "Nm.hi"), 
					# label = "N Male") %>%
				# dyAxis("x", rangePad = 20) %>%
				# dyAxis("y", valueRange = c(0, max(pdat)*1.3)) %>%
			    # dyOptions(strokeWidth = 2,
                    # drawGapEdgePoints = T,
                    # gridLineColor = "gray",
                    # animatedZooms = T,
                    # colors = RColorBrewer::brewer.pal(5, "Set1")[c(3,1,2,4,5)])  %>%
				# dyLegend(show = "always", hideOnMouseOut = FALSE, width = 450) %>%
				# dyRangeSelector() %>%
				# dyEvent(date = cur_yr, "Current Year", labelLoc = "bottom") %>%
				# dyShading(from = cur_yr, to = last_yr)
		# }
		#  Lambda
		ipm_plot_L <- function(x){
			#  Format model output
			tmp_yrs <- attributes(x)$year
			yrs <- tmp_yrs[1]:tmp_yrs[2]
			pin <- data.frame(yr = yrs, x$lambda)
			colnames(pin) <- c("yr", "low", "mid", "hi")
			
			yy <- c(0.7, 1.3)
			#  Takes output of summ_ipm
			#  Create plot, returns NULL
			ggplot(pin, aes(x = yr, y = mid)) +
				geom_hline(aes(yintercept = 1), colour = "darkred") +
				ylim(yy) +
				geom_line() +
				geom_ribbon(aes(ymax = hi, ymin = low), alpha = 0.2) + 
				theme_bw() +
				ylab("") +
				xlab("") + 
				theme(legend.position = "none") + 
				theme(panel.border = element_blank(),
						axis.line = element_line(color = "black"))

		}	
		# plot_ipmL <- function(x){
			# #  Takes output of summ_ipm and plots the time-series' using dygraphs
			# pdat <- do.call(cbind, x["lambda"])
			# yrs <- attributes(x)$year[1]:attributes(x)$year[2]
			# cur_yr <- paste(as.POSIXlt(Sys.time())$year + 1900, "01-01", 
				# sep = "-")
			# last_yr <- paste(attributes(x)$year[2], "01-01", 
				# sep = "-")
			# pdatin <- xts(pdat,
				# order.by = as.yearmon(paste("01-01-", yrs, sep = ""), 
					# "%m-%d-%Y"))
			# colnames(pdatin) <- c("low", "mid", "hi")
			# #  Dygraph
			# dygraph(pdatin, main = "Population Growth Rate") %>%
				# dySeries(c("low", "mid", "hi"), 
					# label = "Growth Rate") %>%
				# dyAxis("x", rangePad = 20) %>%
				# dyAxis("y", valueRange = c(0, 2)) %>%
			    # dyOptions(strokeWidth = 2,
                    # drawGapEdgePoints = T,
                    # gridLineColor = "gray",
                    # animatedZooms = T,
					# colors = RColorBrewer::brewer.pal(5, "Set1")[5:1])  %>%
				# dyLegend(show = "always", hideOnMouseOut = FALSE, width = 450) %>%
				# dyRangeSelector() %>%
				# dyEvent(date = cur_yr, "Current Year", labelLoc = "bottom") %>%
				# dyShading(from = cur_yr, to = last_yr)
		# }		
		#  Survival
		ipm_plot_S <- function(x, dat, input){
			#  Format model output
			tmp_yrs <- attributes(x)$year
			yrs <- tmp_yrs[1]:tmp_yrs[2]
			pin <- data.frame(yr = yrs, x$fS, x$mS, x$jS)
			colnames(pin) <- c("yr", "fl", "fm", "fh", "ml", "mm", "mh", "jl", "jm", "jh")

			#  Format raw data
			rawfs <- data.frame(x = (yrs[1] - 1) + dat$fSyr, y = dat$fSdat, 
								ymin = dat$fSdat - 1.96 * (1/sqrt(dat$fSse)),
								ymax = dat$fSdat + 1.96 * (1/sqrt(dat$fSse)))
			rawfs$ymax[rawfs$ymax > 1] <- 0.999
			rawfs$ymin[rawfs$ymin < 0] <- 0.001		
			rawjs <- data.frame(x = (yrs[1] - 1) + dat$jSyr, y = dat$jSdat, 
								ymin = dat$jSdat - 1.96 * (1/sqrt(dat$jSse)),
								ymax = dat$jSdat + 1.96 * (1/sqrt(dat$jSse)))
			rawjs$ymax[rawjs$ymax > 1] <- 0.999
			rawjs$ymin[rawjs$ymin < 0] <- 0.001	
								
			gs <- ggplot(pin) + 
				ylim(c(0, 1)) +
				geom_line(aes(x = yr, y = fm), colour = "red") +
				geom_ribbon(aes(x = yr, ymax = fh, ymin = fl), alpha = 0.2) + 
				geom_line(aes(x = yr, y = mm), colour = "blue") +
				geom_ribbon(aes(x = yr, ymax = mh, ymin = ml), alpha = 0.2) + 
				geom_line(aes(x = yr, y = jm), colour = "green") +
				geom_ribbon(aes(x = yr, ymax = jh, ymin = jl), alpha = 0.2) + 
				theme_bw() +
				ylab("") +
				xlab("") + 
				theme(panel.border = element_blank(),
						axis.line = element_line(color = "black"))
			if(input$ipm_sfd){
				if(input$ipm_sserr){
					gs <- gs + geom_pointrange(data = rawfs, aes(x=x, y=y, 
							ymin = ymin, ymax = ymax), colour = "red") +
						geom_pointrange(data = rawjs, aes(x=x, y=y, ymin = ymin, 
							ymax = ymax), colour = "green")
				}else{
					gs <- gs + geom_point(data = rawfs, aes(x=x, y=y), 
								colour = "red") +
							geom_point(data = rawjs, aes(x=x, y=y), 
								colour = "green")
				}
			}
		gs	
		}	
		# plot_ipmS <- function(x){
			# #  Takes output of summ_ipm and plots the time-series' using dygraphs
			# pdat <- do.call(cbind, x[c("jS", "fS", "mS")])
			# yrs <- attributes(x)$year[1]:attributes(x)$year[2]
			# cur_yr <- paste(as.POSIXlt(Sys.time())$year + 1900, "01-01", 
				# sep = "-")
			# last_yr <- paste(attributes(x)$year[2], "01-01", 
				# sep = "-")
			# pdatin <- xts(pdat,
				# order.by = as.yearmon(paste("01-01-", yrs, sep = ""), 
					# "%m-%d-%Y"))
			# colnames(pdatin) <- c(paste(rep(c("jS", "fS", "mS"), each = 3), 
									# c("low", "mid", "hi"), sep = "."))
			# #  Dygraph
			# dygraph(pdatin, main = "Survival") %>%
				# dySeries(c("jS.low", "jS.mid", "jS.hi"),
					# label = "Juvenile Survival") %>%
				# dySeries(c("fS.low", "fS.mid", "fS.hi"), 
					# label = "Female Survival") %>%
				# dySeries(c("mS.low", "mS.mid", "mS.hi"), 
					# label = "Male Survival") %>%
				# dyAxis("x", rangePad = 20) %>%
				# dyAxis("y", valueRange = c(-0.1, 1.1)) %>%
			    # dyOptions(strokeWidth = 2,
                    # drawGapEdgePoints = T,
                    # gridLineColor = "gray",
                    # animatedZooms = T,
                    # colors = RColorBrewer::brewer.pal(5, "Set1"))  %>%
				# dyLegend(show = "always", hideOnMouseOut = FALSE, width = 450) %>%
				# dyRangeSelector() %>%
				# dyEvent(date = cur_yr, "Current Year", labelLoc = "bottom") %>%
				# dyShading(from = cur_yr, to = last_yr)
		# }
		#  Recruitment
		ipm_plot_rat <- function(x, dat, input){
			#  Format model output
			tmp_yrs <- attributes(x)$year
			yrs <- tmp_yrs[1]:tmp_yrs[2]
			pin <- data.frame(yr = yrs, x$mf, x$yf)
			colnames(pin) <- c("yr", "mfl", "mfm", "mfh", "yfl", "yfm", "yfh")

			#  Format raw data
			rawyf <- data.frame(x = (yrs[1] - 1) + dat$yfyr, y = dat$yfdat,
					ymin = dat$yfdat - 1.96 * (1/sqrt(dat$yfse)),
					ymax = dat$yfdat + 1.96 * (1/sqrt(dat$yfse)))
			rawmf <- data.frame(x = (yrs[1] - 1) + dat$mfyr, y = dat$mfdat,
					ymin = dat$mfdat - 1.96 * (1/sqrt(dat$mfse)),
					ymax = dat$mfdat + 1.96 * (1/sqrt(dat$mfse)))
			
			gr <- ggplot(pin) + 
				ylim(c(0, 100)) +
				geom_line(aes(x = yr, y = mfm), colour = "red") +
				geom_ribbon(aes(x = yr, ymax = mfh, ymin = mfl), alpha = 0.2) + 
				geom_line(aes(x = yr, y = yfm), colour = "blue") +
				geom_ribbon(aes(x = yr, ymax = yfh, ymin = yfl), alpha = 0.2) + 
				theme_bw() +
				ylab("") +
				xlab("") + 
				theme(legend.position = "none") + 
				theme(panel.border = element_blank(),
						axis.line = element_line(color = "black"))
			if(input$ipm_rfd){
				if(input$ipm_rserr){
					gr <- gr + geom_pointrange(data = rawmf, aes(x=x, y=y, 
							ymin = ymin, ymax = ymax), colour = "red") +
							geom_pointrange(data = rawyf, aes(x=x, y=y, 
								ymin = ymin, ymax = ymax), colour = "blue")
				}else{
					gr <- gr + geom_point(data = rawmf, aes(x=x, y=y), 
								colour = "red") +
							geom_point(data = rawyf, aes(x=x, y=y), 
								colour = "blue")		
				}
			}
		gr
		}
		# plot_ipmR <- function(x){
			# #  Takes output of summ_ipm and plots the time-series' using dygraphs
			# pdat <- do.call(cbind, x["R"])
			# yrs <- attributes(x)$year[1]:attributes(x)$year[2]
			# cur_yr <- paste(as.POSIXlt(Sys.time())$year + 1900, "01-01", 
				# sep = "-")
			# last_yr <- paste(attributes(x)$year[2], "01-01", 
				# sep = "-")
			# pdatin <- xts(pdat,
				# order.by = as.yearmon(paste("01-01-", yrs, sep = ""), 
					# "%m-%d-%Y"))
			# colnames(pdatin) <- c("low", "mid", "hi")
			# #  Dygraph
			# dygraph(pdatin, main = "Recruitment") %>%
				# dySeries(c("low", "mid", "hi"), 
					# label = "Recruitment") %>%
				# dyAxis("x", rangePad = 20) %>%
				# dyAxis("y", valueRange = c(-0.1, 2.5)) %>%
			    # dyOptions(includeZero = T,
					# strokeWidth = 2,
                    # drawGapEdgePoints = T,
                    # gridLineColor = "gray",
                    # animatedZooms = T,
					# colors = RColorBrewer::brewer.pal(5, "Set1")[c(4, 5, 1:3)])  %>%
				# dyLegend(show = "always", hideOnMouseOut = FALSE, width = 450) %>%
				# dyRangeSelector() %>%
				# dyEvent(date = cur_yr, "Current Year", labelLoc = "bottom") %>%
				# dyShading(from = cur_yr, to = last_yr)
		# }		
		
################################################################################
		#  Markdown
		#  Population Table
		mark_ipm_pop_tbl <- function(out){
			#  Small function to create column names
			cnm <- function(dem){
				c("LowerCI", dem, "UpperCI")
			}
			x <- summ_ipm(out)
			yr <- attributes(x)$year
			x_tbl <- as.data.frame(cbind(yr[1]:yr[2], x$Ntot, x$Nm, x$Nf, x$Ny))
			colnames(x_tbl) <- c("Year", cnm("Total"), cnm("Male"), 
									cnm("Female"), cnm("Young"))
		round(x_tbl)
		}
		#  Ratio Table
		mark_ipm_rat_tbl <- function(out){
			#  Small function to create column names
			cnm <- function(dem){
				c("LowerCI", dem, "UpperCI")
			}
			x <- summ_ipm(out)
			yr <- attributes(x)$year
			x_tbl <- as.data.frame(cbind(yr[1]:yr[2], x$mf, x$yf))
			colnames(x_tbl) <- c("Year", cnm("M:F Ratio"), cnm("Y:F Ratio"))
		round(x_tbl, 2)
		}
		#  Survival Table
		mark_ipm_surv_tbl <- function(out){
			#  Small function to create column names
			cnm <- function(dem){
				c("LowerCI", dem, "UpperCI")
			}
			x <- summ_ipm(out)
			yr <- attributes(x)$year
			x_tbl <- as.data.frame(cbind(yr[1]:yr[2], x$jS, x$fS, x$mS))
			colnames(x_tbl) <- c("Year", cnm("Juvenile Survival"), 
								cnm("Female Survival"), cnm("Male Survival"))
		round(x_tbl, 2)
		}
		## Markdown plots
		mark_plot_N <- function(x, dat){
		  #  Takes output of summ_ipm
		  #  Create plot, returns NULL
			#  Format model output
			tmp_yrs <- attributes(x)$year
			yrs <- tmp_yrs[1]:tmp_yrs[2]
			pin <- data.frame(yr = yrs, round(x$Ntot))
			colnames(pin) <- c("yr", "low", "mid", "hi")

			#  Format raw data
			raw <- data.frame(x = (yrs[1] - 1) + dat$airyr, y = dat$counts,
								se = 1/sqrt(dat$nse))
			
			yy <- c(min(pin$low)*0.9, max(pin$hi)*1.1)

			ggplot(pin, aes(x = yr, y = mid)) + 
				ylim(yy) +
				geom_line() +
				geom_ribbon(aes(ymax = hi, ymin = low), alpha = 0.2) + 
				theme_bw() +
				ylab("") +
				xlab("") + 
				theme(legend.position = "none") +
				geom_point(data = raw, aes(x=x, y=y), colour = "blue")
					
		}	

		mark_plot_L <- function(x, dat){
			#  Format model output
			tmp_yrs <- attributes(x)$year
			yrs <- tmp_yrs[1]:tmp_yrs[2]
			pin <- data.frame(yr = yrs, x$lambda)
			colnames(pin) <- c("yr", "low", "mid", "hi")
			
			yy <- c(0.7, 1.3)
			#  Takes output of summ_ipm
			#  Create plot, returns NULL
			ggplot(pin, aes(x = yr, y = mid)) +
				geom_hline(aes(yintercept = 1), colour = "darkgray") +
				ylim(yy) +
				geom_line() +
				geom_ribbon(aes(ymax = hi, ymin = low), alpha = 0.2) + 
				theme_bw() +
				ylab("") +
				xlab("") + 
				theme(legend.position = "none")

		}	

		mark_plot_S <- function(x, dat){
			#  Format model output
			tmp_yrs <- attributes(x)$year
			yrs <- tmp_yrs[1]:tmp_yrs[2]
			pin <- data.frame(yr = yrs, x$fS, x$mS, x$jS)
			colnames(pin) <- c("yr", "fl", "fm", "fh", "ml", "mm", "mh", "jl", "jm", "jh")

			#  Format raw data
			rawfs <- data.frame(x = (yrs[1] - 1) + dat$fSyr, y = dat$fSdat)
			rawjs <- data.frame(x = (yrs[1] - 1) + dat$jSyr, y = dat$jSdat)
			
			ggplot(pin) + 
				ylim(c(0, 1)) +
				geom_line(aes(x = yr, y = fm), colour = "red") +
				geom_ribbon(aes(x = yr, ymax = fh, ymin = fl), alpha = 0.2) + 
				geom_line(aes(x = yr, y = mm), colour = "blue") +
				geom_ribbon(aes(x = yr, ymax = mh, ymin = ml), alpha = 0.2) + 
				geom_line(aes(x = yr, y = jm), colour = "green") +
				geom_ribbon(aes(x = yr, ymax = jh, ymin = jl), alpha = 0.2) + 
				theme_bw() +
				ylab("") +
				xlab("") + 
				theme(legend.position = "none") +
				geom_point(data = rawfs, aes(x=x, y=y), colour = "red") +
				geom_point(data = rawjs, aes(x=x, y=y), colour = "green")
	
		}	

		mark_plot_rat <- function(x, dat){
			#  Format model output
			tmp_yrs <- attributes(x)$year
			yrs <- tmp_yrs[1]:tmp_yrs[2]
			pin <- data.frame(yr = yrs, x$mf, x$yf)
			colnames(pin) <- c("yr", "mfl", "mfm", "mfh", "yfl", "yfm", "yfh")

			#  Format raw data
			rawyf <- data.frame(x = (yrs[1] - 1) + dat$yfyr, y = dat$yfdat)
			rawmf <- data.frame(x = (yrs[1] - 1) + dat$mfyr, y = dat$mfdat)
			
			ggplot(pin) + 
				ylim(c(0, 100)) +
				geom_line(aes(x = yr, y = mfm), colour = "red") +
				geom_ribbon(aes(x = yr, ymax = mfh, ymin = mfl), alpha = 0.2) + 
				geom_line(aes(x = yr, y = yfm), colour = "blue") +
				geom_ribbon(aes(x = yr, ymax = yfh, ymin = yfl), alpha = 0.2) + 
				theme_bw() +
				ylab("") +
				xlab("") + 
				theme(legend.position = "none") +
				geom_point(data = rawmf, aes(x=x, y=y), colour = "red") +
				geom_point(data = rawyf, aes(x=x, y=y), colour = "blue")
	
		}
#################################################################################

		