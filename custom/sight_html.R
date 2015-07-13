		#  Markdown survival analysis report
		#  Josh Nowak
		#  03/2015
#################################################################################
		sight_html <- function(sight, subdat, input, report_type, session){
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
		doc_name <- "sight_report.Rmd"
		
		#  Define YAML header
		cat("---", 
			"\ntitle: 'Idaho Department of Fish and Game'", 
			"\nauthor: 'PopR Sightability Analysis Report'",
			paste("\ndate:", format(Sys.time(), "%b %d, %Y")), 
			"\n---\n",
			file = paste(doc_name, sep = ""))

		#  Define model settings
		cat("-----",
			"\n\n\n####Model Definition",
			paste("\n**Species:** ", italics(sight$fit$critter)),
			paste("\n**Unit:** ", italics(sight$fit$dau)),
			paste("\n**Year:** ", italics(sight$fit$year)),
			paste("\n**Aircraft:** ", unique(subdat$Aircraft),
			paste("\n**Model:** "), italics(sight$fit$aircraft)),
			paste("\n\n**Survey Type:**", italics(sight$fit$survey_type)),
			"\n\n-----",
			append = T, file = doc_name)

		#  Insert survival table
		cat("\n\n\n####Sightability Estimates\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"source('custom/process_sight.R')\n\n",
			"\nkable(mark_sight_tab(sight$fit, subdat), align = 'c')\n\n",
			"```",
			"\n\n-----\n\n",
			append = T, file = doc_name)	

		cat("\n\n\n####Observations by Stratum\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"source('custom/process_sight.R')\n\n",
			"\nkable(mark_sight_rep(sight$fit$sdata2), align = 'c')\n\n",
			"```",
			"\n\n-----\n\n",
			append = T, file = doc_name)	
			
		incProgress(0.1, message = "Data ready, compiling report...", 
					session = session)
		#
		return(render(doc_name, report_type))

		incProgress(1, message = "Report ready!", session)
		}, session = session)			
		
		}