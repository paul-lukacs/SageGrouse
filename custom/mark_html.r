		
	mark_html <- function(input, out, report_type, session){	
		
		owd <- setwd(tempdir())
        on.exit(setwd(owd))
        knitr::opts_knit$set(root.dir = owd)

		
		#  Italics function
		italics <- function(x){
			paste("*", x, "*", sep = "")
		}
    
		withProgress(message = "Preparing Report", value = 0.2, {
			  incProgress(0.1, message = "Preparing Data", session = session)
		#  Initiate document
		dt <- Sys.time()
		doc_name <- "report.Rmd"
				
		#  Define YAML header
		cat("---", 
			"\ntitle: 'Sage-Grouse'", 
			"\nauthor: 'PopR Population Model Report'",
			paste("\ndate:", format(Sys.time(), "%b %d, %Y")), 
#			paste("\noutput:\n", 'html_document'),
			"\n---\n",
			file = paste(doc_name, sep = ""))

		#  Define model settings
		cat("-----",
			"\n\n\n####Model Definition",
			paste("\n**Species:** ", italics(input$critter)),
			paste("\n**Unit:** ", italics(input$dau)),
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
			"\n\n-----",
			append = T, file = doc_name)
		
		#  Insert population size table
		cat("\n\n\n####Population Size\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"source('custom/process_ipm.R')\n\n",
			"dat <- get_ipm_dat(input)\n\n",
			"\n\nkable(mark_ipm_pop_tbl(out), align = 'c')\n\n",
			"```",
			"\n\n-----\n\n",
			append = T, file = doc_name)
			
#		#  Insert table of sex and age ratios
#		cat("\n\n\n####Sex & Age Ratios\n\n",
#			"```{r, echo = FALSE, results='asis'}\n\n",
#			"\n\nkable(mark_ipm_rat_tbl(out), align = 'c')\n\n",
#			"```",
#			"\n\n-----\n\n",
#			append = T, file = doc_name)		

#		#  Insert table of sex and age ratios
#		cat("\n\n\n####Survival\n\n",
#			"```{r, echo = FALSE, results='asis'}\n\n",
#			"\n\nkable(mark_ipm_surv_tbl(out), align = 'c')\n\n",
#			"```",
#			"\n\n-----\n\n",
#			append = T, file = doc_name)	
		incProgress(0.1, message = "Preparing Data", session = session)	
		#  Plots 
		#  N
		cat("\n\n\n####Population Trajectory\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"\n\nmark_plot_N(summ_ipm(out), dat)\n\n",
			"```",
			"\n\nFigure 1. Model predicted population trajectory.  Solid line
			represents the median estimate.  The shaded area depcits the 95%
			prediction interval.  Blue dots represent the mean of field based 
			estimates.",
			"\n\n-----\n\n",
			append = T, file = doc_name)
		#  Population growth rate
		cat("\n\n\n####Population Growth Rate\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"\n\nmark_plot_L(summ_ipm(out), dat)\n\n",
			"```",
			"\n\nFigure 2. Model predicted population growth rate.  The solid 
			line represents the median estimate.  The shaded area depcits the 95% 
			prediction interval.",
			"\n\n-----\n\n",
			append = T, file = doc_name)	
		#  Survival
		cat("\n\n\n####Survival\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"\n\nmark_plot_S(summ_ipm(out), dat)\n\n",
			"```",
			"\n\nFigure 3. Model predicted survival of adult females (red), 
			adult males (blue) and juveniles (green).  Solid lines	represents the median 
			estimate.  The shaded area depcits the 95% prediction interval for
			each series.  Dots represent the mean of field based estimates.",
			"\n\n-----\n\n",
			append = T, file = doc_name)
		#  Ratios
#		cat("\n\n\n####Sex & Age Ratios\n\n",
#			"```{r, echo = FALSE, results='asis'}\n\n",
#			"\n\nmark_plot_rat(summ_ipm(out), dat)\n\n",
#			"```",
#			"\n\nFigure 4. Model predicted ratios of young to adult females (blue)
#			and adult males to adult females(red).  Solid lines	represents the median 
#			estimate while shaded area depcit 95% prediction intervals for
#			each series.  Dots represent the mean of field based estimates.",
#			"\n\n-----\n\n",
#			append = T, file = doc_name)
		incProgress(0.1, message = "Data ready, compiling report...", 
					session = session)
		#
		return(render(doc_name, report_type))

		incProgress(1, message = "Report ready!", session = session)
		}, session = session)
	}
