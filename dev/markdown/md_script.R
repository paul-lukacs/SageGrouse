		#  Run code in jags_call_dev.R before using this script to develop a
		#  Markdown document
		setwd("C:/PopR_Master/dev/markdown")
		
		#  Function to italics
		italics <- function(x){
			paste("*", x, "*", sep = "")
		}
		
		#  Initiate document
		dt <- Sys.time()
		doc_name <- paste(input$critter, "_IPM_", 
				format(dt, "%d%b%Y_%H%M%S"),
				".Rmd", sep = "")
				
		#  Define YAML header
		cat("---", 
			"\ntitle: 'Idaho Department of Fish and Game'", 
			"\nauthor: 'PopR Population Model Report'",
			paste("\noutput:\n", input$ipm_stype),
			"\n---\n",
			file = doc_name)

		#  Define model settings
		cat("***",
			"\n\n\n####Model Definition",
			paste("\n**Species:**", italics(input$critter)),
			paste("\n**Unit:**", italics(input$dau)),
			paste("\n**Years:** ", italics(input$year[1]), "-", 
					italics(input$year[2]), sep = ""),
			"\n\n####Future Harvest",
			paste("\n\n**Males:**", italics(input$mharv)),
			paste("\n**Females:**", italics(input$fharv)),
			"\n\n####Demographic Settings",
			paste("\n\n**Adult Survival:**", italics(input$adultSmod)),
			paste("\n**Juvenile Survival:**", italics(input$juvSmod)),
			paste("\n**Recruitment:**", italics(input$recruitMod)),
			"\n\n####Model Fitting",
			paste("\n\n**MCMC Burnin:**", italics(input$ipmburn)),
			paste("\n**MCMC Reps:**", italics(input$ipmiter)),
			paste("\n**Thinning Rate:**", italics(input$ipmthin)),			
			"\n\n***",
			append = T, file = doc_name)
		
		#  Insert population size table
		cat("\n\n\n####Population Size\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"source('C:/PopR_Master/custom/process_ipm.R')\n\n",
			"load('C:/PopR_Master/trash/tmp_model.RData')",
			"\n\nkable(mark_pop_tbl(out))\n\n",
			"```",
			"\n\n***\n\n",
			append = T, file = doc_name)
			
		#  Insert table of sex and age ratios
		cat("\n\n\n####Sex & Age Ratios\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"\n\nkable(mark_rat_tbl(out))\n\n",
			"```",
			"\n\n***\n\n",
			append = T, file = doc_name)		

		#  Insert table of sex and age ratios
		cat("\n\n\n####Survival\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"\n\nkable(mark_surv_tbl(out))\n\n",
			"```",
			"\n\n***\n\n",
			append = T, file = doc_name)	
			
		#  Plots 
		#  N
		cat("\n\n\n####Population Trajectory\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"\n\nmark_plot_N(summ_ipm(out), dat)\n\n",
			"```",
			"\n\nFigure 1. Model predicted population trajectory.  Solid line
			represents the median estimate.  The shaded area depcits the 95%
			prediction interval.  Blue dots represent the mean of field base 
			estimates.",
			"\n\n***\n\n",
			append = T, file = doc_name)
		#  Population growth rate
		cat("\n\n\n####Population Growth Rate\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"\n\nmark_plot_L(summ_ipm(out), dat)\n\n",
			"```",
			"\n\nFigure 2. Model predicted population growth rate.  The solid 
			line represents the median estimate.  The shaded area depcits the 95% 
			prediction interval.",
			"\n\n***\n\n",
			append = T, file = doc_name)	
		#  Survival
		cat("\n\n\n####Survival\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"\n\nmark_plot_S(summ_ipm(out), dat)\n\n",
			"```",
			"\n\nFigure 3. Model predicted survival of adult females (red), 
			adult males (blue) and juveniles (green).  Solid lines	represents the median 
			estimate.  The shaded area depcits the 95% prediction interval for
			each series.  Dots represent the mean of field base estimates.",
			"\n\n***\n\n",
			append = T, file = doc_name)
		#  Ratios
		cat("\n\n\n####Sex & Age Ratios\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"\n\nmark_plot_rat(summ_ipm(out), dat)\n\n",
			"```",
			"\n\nFigure 4. Model predicted ratios of young to adult females (blue)
			and adult males to adult females(red).  Solid lines	represents the median 
			estimate while shaded area depcit 95% prediction intervals for
			each series.  Dots represent the mean of field base estimates.",
			"\n\n***\n\n",
			append = T, file = doc_name)		
		#  
		render(doc_name)

				
		