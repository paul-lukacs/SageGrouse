			#  Functions to create tabular outputs and plots from sightability
			#  models
			#  Josh Nowak
			#  03/2015
#################################################################################
			#  Tabular output
			mark_sight_tab <- function(x, dat){
				#  Takes the output of the total.sight function (x) and 
				#  sight_subdata() (dat)
				#  Returns a table with lower and upper CI's
				#  Estimates
				pin_est <- x$summ %>%
							mutate(Demographic = rownames(.), 
									LCI = round(Mean - 1.96 * SE),
									UCI = round(Mean + 1.96 * SE),
									Mean = round(Mean)) %>%
							select(Demographic, LCI, Mean, UCI)
				#  Raw Data
				rawd <- dat %>%
						select(Young, Ad_Females, Yrlg_Males, SubAd_Males,
								Ad_Males, UnClass, Total) %>%
						summarise_each(funs(sum))
				pin_est$Field_Obs <- as.numeric(rawd) 
			pin_est
			}
			mark_sightp_tabs <- function(sdata2, input){
				#  Takes output of sight.muleDeer and shiny input object
				#  Returns table counting the number of observations in each 
				#  detection probability class
				if( input$sight_critter == "Mule Deer" ){
					acts <- c("No Data", "Bedded", "Standing", "Moving")
					act_tab <- sdata2$data %>%
								group_by(Activity) %>%
								summarise(Count = n()) %>%
								mutate(Activity = acts[Activity+1])
					vegs <- c("No Data", "Grassland/Open", "Sagebrush", 
								"Juniper/Mahogany",	"Aspen/Riparian/Brush", 
								"Conifer")
					veg_tab <- sdata2$data %>%
								group_by(Veg_Type) %>%
								summarise(Count = n()) %>%
								mutate(Veg_Type = vegs[Veg_Type+1])
					snows <- c("0%-20%", "20%-80%", "80%-100%")
					snow_tab <- sdata2$data %>%
								mutate(Snow_Cover = 
										ifelse(Perc_Snow_Cover <= 20, 1, 2),
									   Snow_Cover = 
										ifelse(Perc_Snow_Cover > 80, 3, 
												Snow_Cover)) %>%
								group_by(Snow_Cover) %>%
								summarise(Count = n()) %>%
								mutate(Snow_Cover = snows[Snow_Cover])
								
					grp_mat <- matrix(
						c(quantile(sdata2$data$Total, c(.25, .75)),
							quantile(sdata2$data$Total, c(.025, .975))),
						ncol = 2,
						byrow = T
					)
					
					grp_stat <- c(
						paste(round(quantile(sdata2$data$Total, .25, na.rm = T)), 
							round(quantile(sdata2$data$Total, .75, na.rm = T)), 
							sep = "-"),
						paste(round(quantile(sdata2$data$Total, .025, na.rm = T)), 
							round(quantile(sdata2$data$Total, .975, na.rm = T)), 
							sep = "-"),
						as.character(round(mean(sdata2$data$Total, na.rm = T))),
						as.character(round(quantile(sdata2$data$Total, .5, 
							na.rm = T))),
						as.character(round(min(sdata2$data$Total, na.rm = T))),
						as.character(round(max(sdata2$data$Total, na.rm = T)))
					)

					grp_nms <- c("50% of group sizes between",
									"95% of group sizes between",
									"Mean group size",
									"Median group size",
									"Minimum group size",
									"Maximum group size")
					
					grp_tab <- data.frame("Metric" = grp_nms,
											"Value" = grp_stat)
					
					p_tab <- table(cut(sdata2$data$p, 
									breaks = c(0, .05, .25, .5, .75, .95, 1), 
									labels = c("0-0.05", 
												"0.05-0.25", 
												"0.25-0.50", 
												"0.50-0.75", 
												"0.75-0.95", 
												"0.95-1")))
					p_out <- data.frame(p_tab)
					colnames(p_out) <- c("Detection Probability", "Count")
					out <- list(p_out = p_out, 
								act_tab = act_tab, 
								veg_tab = veg_tab, 
								snow_tab = snow_tab, 
								grp_tab = grp_tab)
								
				} else if ( input$sight_critter == "Elk" && 
							input$sight_aircraft == "Hiller 12E" ){
					sdata2 <- sight.elkHillerSnow( input, sdata )
				} else if ( input$sight_critter == "Elk" && 
							input$sight_aircraft == "Hiller 12E no snow" ){
					sdata2 <- sight.elkHillerNoSnow( input, sdata )
				} else if ( input$sight_critter == "Elk" && 
							input$sight_aircraft == "Bell 47G" ){
					sdata2 <- sight.elkBell47G( input, sdata )
				} else if ( input$sight_critter == "Elk" && 
							input$sight_aircraft == "Bell 206" ){
					sdata2 <- sight.elkBell206( input, sdata )
				} else {
					cat( "Model unavailable for ", input$sight_critter, ".\n"  )
					return(NULL)
				}
			out
			}
			mark_sight_rep <- function(sdata2){
				
				tmp <- sdata2$data %>%
						group_by(Stratum) %>%
						summarise(Young = sum(Young, na.rm = T),
									Ad_Females = sum(Ad_Females, na.rm = T),
									Yrlg_Males = sum(Yrlg_Males, na.rm = T),
									SubAd_Males = sum(SubAd_Males, na.rm = T),
									Ad_Males = sum(Ad_Males, na.rm = T),
									UnClass = sum(UnClass, na.rm = T),
									Total = sum(Total, na.rm = T)) %>%
						select(Young, Ad_Females, Yrlg_Males, SubAd_Males, 
								Ad_Males, UnClass, Total)
				
				out <- t(tmp)
				colnames(out) <- paste("Stratum", 
										sort(unique(sdata2$data$Stratum)))
			out
			}
#################################################################################
			#  Plot
			mark_sight_plot_comp <- function(x, dat){
				#  Takes the output of the total.sight function (x) and the 
				#   sight_subdata() object
				#  Returns plot
				rnms <- c("Young", "Ad Females", "Yrlg Males", "SubAd Males", 
							"Ad Males", "Unclass")
				#  Estimates
				pin_est <- x$summ[rownames(x$summ) != "Total",] %>%
							mutate(Demographic = rnms, 
									LCI = Mean - 1.96 * SE,
									UCI = Mean + 1.96 * SE,
									Obs = 2) %>%
							select(Demographic, LCI, Mean, UCI, Obs)
				#  Raw Data
				rawd <- dat %>%
						select(Young, Ad_Females, Yrlg_Males, SubAd_Males,
								Ad_Males, UnClass) %>%
						summarise_each(funs(sum))
				pin_raw <- data.frame("Demographic" = rnms,
										"LCI" = NA,
										"Mean" = as.numeric(rawd),
										"UCI" = NA,
										"Obs" = 1)
				
				pin <- rbind(pin_est, pin_raw)
				pin[,1] <- factor(pin[,1], levels = rnms)
				
				dodge <- position_dodge(width = 0.3)	 
				
				ggplot(pin, aes(x = Demographic, y = Mean, 
								ymax = UCI, ymin = LCI, group = Obs, 
								colour = Obs)) + 
					geom_pointrange(alpha = 0.8, na.rm = T, position = dodge,
									size = 0.6, na.rm = T) +
					theme_bw() +
					ylab("") +
					xlab("") + 
					theme(legend.position = "none") + 
					theme(panel.border = element_blank(),
							axis.line = element_line(color = "black"))
				
			
			}
			mark_sight_plot_total <- function(x, dat){
				#  Takes the output of the total.sight function (x) and the 
				#   sight_subdata() object
				#  Returns plot
				rnms <- c("Total")
				#  Estimates
				pin_est <- x$summ[rownames(x$summ) == "Total",] %>%
							mutate(Demographic = rnms, 
									LCI = round(Mean - 1.96 * SE),
									UCI = round(Mean + 1.96 * SE),
									Mean = round(Mean),
									Obs = 2) %>%
							select(Demographic, LCI, Mean, UCI, Obs)
				#  Raw Data
				rawd <- dat %>%
						select(Total) %>%
						summarise_each(funs(sum))
				pin_raw <- data.frame("Demographic" = rnms,
										"LCI" = NA,
										"Mean" = as.numeric(rawd),
										"UCI" = NA,
										"Obs" = 1)
				
				pin <- rbind(pin_est, pin_raw)
				
				dodge <- position_dodge(width = 0.3)	 
				
				ggplot(pin, aes(x = Demographic, y = Mean, 
								ymax = UCI, ymin = LCI, group = Obs, 
								colour = Obs)) + 
					geom_pointrange(alpha = 0.8, na.rm = T, position = dodge,
									size = 0.6, na.rm = T) +
					theme_bw() +
					ylab("") +
					xlab("") + 
					theme(legend.position = "none") + 
					theme(panel.border = element_blank(),
							axis.line = element_line(color = "black"))
				
			
			}
			
#################################################################################