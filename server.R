    source("custom/db_connections.R")
    source("helpers/plot_harv.R")
    source("helpers/read_db.R")
    source("custom/rename_sp.R")
    source("custom/dau_lookup.R")
    source("custom/get_ipm.R")
    source("custom/mark_html.r")
    source("custom/process_ipm.R")
    source("custom/api_funs.r")
    source("custom/sight_funs.R")
    source("custom/surv_funs.R")
    source("custom/surv_html.R")
    source("custom/sight_html.R")
    source("custom/spp_area.R")
    source("custom/process_surv.R")
    source("custom/process_sight.R")
    source("custom/game_lookup.R")
	source("custom/nmix_funcs.R")
    #source("custom/dic_tbl.r")
    source("helpers/name_outputs.r")
    source("helpers/gen_init.R")
    source("misc/packages.R")
    load("grouseStates.RData")	# IPM demographic data
	load("grousePopulations.RData")
	load("grouseZones.RData")
#	load("lekCountData.RData")	# N mixture lek count data

    daus <- read.csv("daus.csv", as.is = T)
    
    # simParams <- c("nyr", 
    #                "muN1", "sdN1",
    #                "muN2", "sdN2",
    #                "muN3", "sdN3",
    #                "mupreg", "sdpreg",
    #                "phif", "sdphif",
    #                "phiadf", "sdphiadf",
    #                "phiadm", "sdphiadm")

    Logged <- FALSE
    PASSWORD <- data.frame(Brukernavn = "popr.user", 
                           Passord = "25d55ad283aa400af464c76d713c07ad")
      
################################################################################
    #  Shiny Server Script
    shinyServer(function(input, output, session){
      #  Login page
     source("pages/login_page/login.r", local = T)$value
     observe({
      if(USER$Logged == TRUE){
      
        ##  Analysis Pages
        #  IPM
        source("pages/analysis_page/analysis_ipm_server.R", local = T)$value      
#        source("pages/analysis_page/analysis_sight_server.R", local = T)$value      
#        source("pages/analysis_page/analysis_surv_server.R", local = T)$value
		source("pages/analysis_page/analysis_nmix_server.R", local = T)$value
        #  Survival
        #  Pregnancy
        #  Litter Size
        #  Abundance
        #  Harvest
        #  By default shiny haults actions on tabs once the user navigates away,
        #  this is undesirable behavior that is fixed by the code below
        output$activeTab <- reactive({
          return(input$tab)
        })
        outputOptions(output, 'activeTab', suspendWhenHidden = FALSE)
      }
    })  #  observe
  })