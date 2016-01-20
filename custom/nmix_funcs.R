#
# nmix_funs.R
# custom functions for N-mixutre model fitting within PopR
# Paul M. Lukacs  07-2015
#

		nmix_wrapper <- function( input ){
	#		withProgress(message = "Preparing Data", value = 0.2, {
	#		  incProgress(0.1, message = "Preparing Data")
			# process the nmix workflow after fit model button is clicked
			sg <- get_nmix_dat(input)  # read data
			dat <- format_nmix_jagsd( sg, input ) # format data for jags
	
		#	showshinyalert(session, "nmix_runalert", 
         #                paste(input$nmix_year[1],
        #                       input$nmix_dau, 
        #                       "N-mixture model",
        #                       input$nmix_fitgo,
        #                       "test"),
        #                 styleclass = "success")
		#	Sys.sleep(5)

			model_name <- "custom/models/nmix/nmix_pijk.txt" 	#get_ipm_name(input)
			inits <- replicate(3, get_nmix_init(dat, input, model_name), 
									simplify = F) # set the initial values for each chain
									
			parms <- get_nmix_parms(input)	# set the parameters to track in jags.
			# check for valid iterations and burnin length - fix if burnin > iterations.
			if(input$nmix_ipmburn > input$nmix_ipmiter){
				ipmiter <- input$nmix_ipmburn
				ipmburn <- round(input$nmix_ipmburn/2)
			}else{
				ipmiter <- input$nmix_ipmiter
				ipmburn <- input$nmix_ipmburn				
			}
			
			out <- try(jags(data = dat,
					inits = inits,
					parameters.to.save = parms,
					model.file = model_name,
					n.chains = 3,
					n.iter = ipmiter,
					n.burnin = ipmburn,
					n.thin = input$nmix_ipmthin,
					progress.bar = "none"), silent = F)
	#			incProgress(0.2, message = "Error Check")  	
				#  Retry if failed, up to 10 times
				tryit <- 0
				while(class(out) == "try-error" & tryit < 10){
	#				incProgress(0.05, 
	#							message = paste("Error, Retry", tryit, "/10"))
					tryit <- tryit + 1
					out <- try(jags(data = dat,
						inits = inits,
						parameters.to.save = parms,
						model.file = model_name,
						n.chains = 3,
						n.iter = ipmiter,
						n.burnin = ipmburn,
						n.thin = input$nmix_ipmthin,
						progress.bar = "none"), silent = T)	
				}
				#  Auto convergence if desired
		#		if(class(out) != "try-error" & input$autoup){
		#		out <- autojags(out,
		#						n.iter = 5000,
		#						n.thin = 2,
		#						Rhat = 1.1,
		#						n.update = 10,
		#						progress.bar = "none")
		#		}
				#  Add relevant information to out object, used later for 
				#  plotting and report generation
				if(class(out) != "try-error"){
					out$dau <- input$nmix_dau
					out$year <- input$nmix_year
					out$model_name <- model_name
					out$burn <- ipmburn
					out$iter <- ipmiter
					out$thin <- input$ipmthin
				}
	#		setProgress(1, message = "Finished")  	
	#		})
	
			ntot <- out$BUGSoutput$mean$totalN
			sdtot <- out$BUGSoutput$sd$totalN
			p <- out$BUGSoutput$mean$p0
			sdp <- out$BUGSoutput$sd$p0
			RGN <- out$BUGSoutput$summary[ grep("totalN", rownames(out$BUGSoutput$summary)),"Rhat"]
			RGp <- out$BUGSoutput$summary[ grep("p0", rownames(out$BUGSoutput$summary)),"Rhat"]
			outtab <- data.frame( year=c(input$nmix_year[1]:input$nmix_year[2]),Ntotal=ntot, Nsd=sdtot, rgn=RGN, 
						p=p, sdp=sdp, rgp=RGp  )
			colnames(outtab) <- c("Year", "Abundance", "SD(Abundance)", "R-hat(N)", "Detection", "SD(detection)", "R-hat (p)"  )
			rownames(outtab) <- NULL
								
			setwd( "c:\\temp" )  		## fix for server implementation
			owd="c:\\temp"
			#on.exit(setwd(owd))
			knitr::opts_knit$set(root.dir = owd)
		
			#  Italics function
			italics <- function(x){
				paste("*", x, "*", sep = "")
			}
    
			dt <- Sys.time()
			doc_name <- "nmixReport.Rmd"
				
		#  Define YAML header
			cat("---", 
			"\ntitle: 'PopR Sage-Grouse Module'", 
			"\nauthor: 'N-mixture Model Report'",
			paste("\ndate:", format(Sys.time(), "%b %d, %Y")), 
			paste("\noutput:\n", 'html_document'),
			"\n---\n",
			file = paste(doc_name, sep = ""))
		
			cat("\n\n\n####Population Size\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"\n\nkable(outtab, align = 'c')\n\n",
			"```",
			"\n\n-----\n\n",
			append = T, file = doc_name)
			render( doc_name, "html_document" )
	
			if(class(out) != "try-error"){
			
				#send.mail(from = "popr.results@gmail.com",
				#	to = input$nmix_email,
				#	subject = "PopR: N-mixture results",
				#	body = "c:/temp/nmixReport.html",
				#	html= TRUE,
				#	smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "popr.results", passwd = "MDueleer!", ssl = TRUE),
				#	authenticate = TRUE,
				#	send = TRUE)
			
				send.mail(from = "PopR.results@cfc.umt.edu",
                   to = input$nmix_email,
                   subject="PopR: N-mixture results",
                   body = "nmixReport.html",
				   html= TRUE,
                   smtp = list(host.name = "smtp.umt.edu", port = 25),
                   authenticate = FALSE, 
                   send = TRUE)
				   		
			
			#	z<-print(out)
			#	report <- mark_nmix_report(input, z )
	#		#	body<-list(paste("N-mixture model results from ", out$year[1], "to", out$year[2], "\n", z$summary), mime_part(z$summary, name="out") )
			#	body<-list(paste("N-mixture model results from ", out$year[1], "to", out$year[2], "\n", z$summary), mime_part(report, name="nmix_report.html") )
			#	sendmail( "paul.lukacs@umontana.edu", input$nmix_email, "PopR N-mixture Results", body , control=list(smtpServer="messaging.umt.edu")) 
				#send.mail( from="paul.lukacs@umontana.edu", to=input$nmix_email, subject="PopR N-mixture Results", body=body[[1]] ) 
	#			sendmail( "paul.lukacs@umontana.edu", "james.nowak@mso.umt.edu", "PopR N-mixture Results", body , control=list(smtpServer="messaging.umt.edu")) 
			} else {
				send.mail(from = "popr.results@gmail.com",
					to = c("paul.lukacs@gmail.com"),
					subject = "PopR: N-mixture results",
					body = paste("Sorry, unfortunately the analysis failed.", out),
					html= TRUE,
					smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "popr.results", passwd = "MDueleer!", ssl = TRUE),
					authenticate = TRUE,
					send = TRUE) 
			}
			return(out)
		}

#################################################################################

		mark_nmix_report <- function( input, results, report_type="html_document" ){
			# generate HTML file to attach to email displaying the JAGS output for the N-mixture model
			#
			owd <- setwd(tempdir())
			on.exit(setwd(owd))
			knitr::opts_knit$set(root.dir = owd)

		
			#  Italics function
			italics <- function(x){
				paste("*", x, "*", sep = "")
			}
    
			dt <- Sys.time()
			doc_name <- "nmixReport.Rmd"
				
		#  Define YAML header
			cat("---", 
			"\ntitle: 'Sage-Grouse'", 
			"\nauthor: 'PopR Population Model Report'",
			paste("\ndate:", format(Sys.time(), "%b %d, %Y")), 
			paste("\noutput:\n", 'html_document'),
			"\n---\n",
			file = paste(doc_name, sep = ""))
		
			cat("\n\n\n####Population Size\n\n",
			"```{r, echo = FALSE, results='asis'}\n\n",
			"source('custom/nmix_funcs.R')\n\n",
			"results$summary",
			"```",
			"\n\n-----\n\n",
			append = T, file = doc_name)
			return(render(doc_name, report_type))
		}
		

#################################################################################
		get_nmix_dat <- function(input){
			#  Takes database name and gui inputs, returns data prepared for
			#  JAGS, calls format jagsd internally
			
			#  Read data from database
			sg <- read_nmix_db(input)
			
			
			#  Format data for JAGS
			out <- sg
			
		return(out)		
		}
#################################################################################

		read_nmix_db <- function(input){
			#  takes shiny input object
			#  returns data subset by species, dau and year
			#  part of nmix workflow
			
			if( is.null( input$lekDataFile ) ){
				
				if(is.null(input$nmix_dbname))
					return()

				#  Read data from database
				load(file.path("data", input$nmix_dbname))
				
			} else {
				
				inFile <- input$lekDataFile

				if (is.null(inFile))
					return(NULL)
    
				lekCountData <- read.csv(inFile$datapath, header=TRUE, sep=",",  quote="")
				
				
			}
			#  Subset and order
			sg <- lekCountData  %>%
			##	mutate(DAU = numchar_dau(DAU),
				mutate(state = numchar_state(StateID),
  					Species = rename_sp(Species)) %>%
				arrange(StateID, Year) %>%
				filter(state == input$nmix_dau & Species == input$nmix_critter &
 				  	 Year >= input$nmix_year[1] & Year <= input$nmix_year[2])	
		
			return(sg)
		}


#################################################################################
		#  Define initial values
		get_nmix_init <- function(x, input = input, model_name){

			out <- gen_nmix_init(x, model_name)

			return(out)
		}
#################################################################################
		# Generate initial values
		gen_nmix_init <- function(x, model_name){
		
			#nsite <- length(unique(x$Site)) #number of sites
			#nyear <- length(unique(x$Year)) #number of years
			#J <- length(unique(x$Survey)) #surveys within years
		
			
			N.tmp <- (tapply(x$y, x$site, max) + 10)/.5
			N.init <- matrix(rep(N.tmp, 13), x$nsite, x$nyear)

			out <- list(N = N.init, sd.lam = runif(1,0,1), sd.p = runif(1, 0, 1), int.lam = runif (13, -5,5), p0 = runif(13, 0,1))
			return(out)
		}
		
#################################################################################
		#  Define parameters to monitor
		get_nmix_parms <- function(input){
			#  Using user inputs decide which parameters to monitor, returns
			#  character string used by JAGS
						
			
			# for now everything is the same so hard code.
			out <- c("totalN", "mean.abundance", "sd.lam", "sd.p", "fit", "fit.new", "p0", "mp")
			
			return(out)	
		}
#################################################################################
		#  A function to format data for jags
		format_nmix_jagsd <- function(x, input){
			#  takes a data.frame and returns a list
			
			nsite <- length(unique(x$Site)) #number of sites
			nyear <- length(unique(x$Year)) #number of years
			J <- length(unique(x$Survey)) #surveys within years
			nobs <- nrow(x)
		
			dat <- list( "y" = x$Count,
						 "site" = x$Site,
						 "year" = I(x$Year-min(x$Year, na.rm=TRUE)+1),
						 "nyear" = nyear,
						 "nsite" = nsite,
						 "nobs" = nobs
						)
			
			
		return(dat)
		}