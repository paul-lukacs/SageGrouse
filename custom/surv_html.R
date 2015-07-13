		#  Markdown survival analysis report
		#  Josh Nowak
		#  03/2015
#################################################################################
		surv_html <- function(surv = surv, subdat, input, report_type, session){
			# Takes
			#
			# Returns
			
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
		doc_name <- "surv_report.Rmd"
		uni <- length(unique(subdat$out$Animal_ID))
		meh <- make_eh(subdat, input)
        dead <- sum(meh$dead == 1)
		cens <- sum(meh$dead == 0)
		
		#  Define YAML header
		cat("---", 
			"\ntitle: 'Idaho Department of Fish and Game'", 
			"\nauthor: 'PopR Survival Analysis Report'",
			paste("\ndate:", format(Sys.time(), "%b %d, %Y")), 
			"\n---\n",
			file = paste(doc_name, sep = ""))

		#  Define model settings
		cat("-----",
			"\n\n\n####Model Definition",
			paste("\n**Species:** ", italics(surv$fit$critter)),
			paste("\n**Unit:** ", italics(surv$fit$dau)),
			paste("\n**Year:** ", italics(surv$fit$year)),
			paste("\n\n**Ageclass:**", italics(surv$fit$age)),
			paste("\n\n**Season:**", italics(surv$fit$season)),
			paste("\n\n**Marked Animals:**", italics(uni)),
			paste("\n\n**Deaths:**", italics(dead)),
			paste("\n\n**Censored:**", italics(cens)),
			"\n\n####Model Fitting",
			paste("\n\n**MCMC Burnin:**", italics(surv$fit$burn)),
			paste("\n**MCMC Reps:**", italics(surv$fit$iter)),
			paste("\n**Thinning Rate:**", italics(surv$fit$thin)),			
			"\n\n-----",
			append = T, file = doc_name)

		#  Insert survival table
		cat("\n\n\n####Survival\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"source('custom/process_surv.R')\n\n",
			"\nkable(mark_surv_tbl(surv$fit_summ), align = 'c')\n\n",
			"```",
			"\n\n-----\n\n",
			append = T, file = doc_name)	

		#  Insert Plots
		cat("\n\n\n####Survival Time-Series\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"\n\nmark_surv_ts(surv$fit_summ)\n\n",
			"```",
			"\n\nFigure 1. Model predicted survival through time.  Solid line
			represents the median estimate.  The shaded area depcits the 95%
			prediction interval.",
			"\n\n-----\n\n",
			append = T, file = doc_name)		
			
		cat("\n\n\n####Monthly Survival\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"\n\nmark_surv_post(surv$fit, 'Smon', 'darkred')\n\n",
			"```",
			"\n\nFigure 2. Mean monthly survival estimate.  The x-axis represents
			values of survival while the height of the distribution shows the 
			relative amount of support for a given value of survival.",
			"\n\n-----\n\n",
			append = T, file = doc_name)
			
		cat("\n\n\n####Survival over the study period\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"\n\nmark_surv_post(surv$fit, 'Sann', 'darkgreen')\n\n",
			"```",
			"\n\nFigure 3. Study period survival estimate.  The length of the 
			study period is determined by the season parameter reported above.
			Similar to the monthly survival plot, the x-axis represents
			values of survival while the height of the distribution shows the 
			relative amount of support for a given value of survival.",
			"\n\n-----\n\n",
			append = T, file = doc_name)
			
		incProgress(0.1, message = "Data ready, compiling report...", 
					session = session)
		#
		return(render(doc_name, report_type))

		incProgress(1, message = "Report ready!", session = session)
		}, session = session)			
		
		}