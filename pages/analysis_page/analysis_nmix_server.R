    #  Server code for running sightability models
    #  Josh Nowak
    #  01/2015
	#  Paul Lukacs
	#  07/2015
###############################################################################
    #  Header subtitle

    output$nmix_sp <- renderText({    
      #  Update daus when species changes
      if(input$dbname == "" | is.null(input$dbname))
        return()
  #    load("grouseStates.RData")
      updateSelectInput(session, "nmix_dau", 
		    choices = levels(grouseStates$StateNames),
		    selected = "Montana")
			
      paste(input$nmix_critter, "N-Mixture - In beta!")
      
    })

    #  Header model description
    output$nmix_sub <- renderText({
      
      paste(input$nmix_dau,
            " / Year: ", input$nmix_year[1]
            , sep = "")

    })

    #  Create a container to hold the sightability data
    nmix <- reactiveValues()
 ##########################################################   
 #   #  Get data from database when requested by user
 #   observe({
 #     # Don't do anything if the button has never been pressed
 #     if(input$nmix_getdata == 0)
 #       return()
 #     
 #     isolate({
 #       hold <- try(api_connect("nmix", 
 #                               input$nmix_critter, 
 #                               input$nmix_dau, T), silent = T)
 #       if(class(hold) != "try-error"){
 #         nmix$data <- hold
 #         updateSelectInput(session, "nmix_year", 
 #                           choices = sort(unique(hold$Bio_Year),
 #                                         decreasing = T))
 #         showshinyalert(session, "nmix_runalert", 
 #                        paste(input$nmix_dau, 
 #                              "N-mixture data successfully downloaded!"),
 #                        styleclass = "success")                                          
#
#        }else{
#          showshinyalert(session, "nmix_runalert", 
#                         paste("Download failed!"),
#                         styleclass = "danger")
#        }
#      })
#    })

    #  Subset data to year and survey type
    nmix_subdata <- reactive({
      if(input$nmix_getdata == 0)
        return()
      
     tmp <- get_nmix_dat(input)
      
    return(tmp)
    })

    #  call model when requested by user
    observe({
      # Don't do anything if the button has never been pressed
      if(input$nmix_fitgo == 0)
        return()
      
      isolate({
		  
        hold_nmixmodel <- try(nmix_wrapper(input), 
                               silent = T)
        
        if(class(hold_nmixmodel) != "try-error"){
          

          showshinyalert(session, "nmix_runalert", 
                         paste(input$nmix_year[1],
								"-",
								input$nmix_year[2],
                               input$nmix_dau, 
                               "N-mixture model",
                               input$nmix_fitgo,
                               "successfully ran!"),
                         styleclass = "success")

          nmix$fit <- hold_nmixmodel
          #nmix$summ <- try(nmix.muleDeer(input, nmix_subdata()))
          
        }else{

          showshinyalert(session, "nmix_runalert", 
                         paste(input$nmix_year[1],
								"-",
								input$nmix_year[2],
                               input$nmix_dau, 
                               "N-mixture model",
                               input$nmix_fitgo,
                               "failed!"),
                         styleclass = "danger")
        }
      })
    })

    #  Data Check
    output$nmix_vertout <- renderPrint({
    if(input$nmix_getdata == 0)
      return("No Data Loaded")
    
      nmix_err(nmix$data, input)
    
    })
      
    output$nmix_raw <- renderDataTable({
      if(is.null(nmix_subdata()))
        return()
      
      nmix_subdata()}, options = list(searching = T,
                                      pageLength = 15,
                                      lengthMenu = list(c(15, 25, -1),
                                                        c("15", "25", "All")))
    )

    observe({
      if(is.null(nmix$data))
        return()
      
      input$nmix_year
      
      tmp <- nmix$data %>% 
              filter(Bio_Year == input$nmix_year)
        
      updateSelectInput(session, "nmix_surveytype", 
                        choices = sort(unique(tmp$Survey_Type))) 
      
    })

    observe({
      if(is.null(nmix$data))
        return()
      
      input$nmix_getdata
      
      isolate({
        updateSelectInput(session, "nmix_aircraft", 
                          choices = switch(input$nmix_critter,
                                           "Mule Deer" = "Mule Deer",
                                           "Elk" = list("Hiller 12E",
                                                        "Hiller 12E no snow",
                                                        "Bell 47G"))) 
      })    
    })

    output$nmix_plot_dem <- renderPlot({
      if(is.null(nmix_subdata()) | input$nmix_fitgo == 0)
        return()
      
      mark_nmix_plot_comp(nmix$fit, nmix_subdata())
      
    })
    output$nmix_plot_total <- renderPlot({
      if(is.null(nmix_subdata()) | input$nmix_fitgo == 0)
        return()
      
      mark_nmix_plot_total(nmix$fit, nmix_subdata())
      
    })    
    output$nmix_table_cov1 <- renderDataTable({
      if(is.null(nmix$summ))
        return()
      
      mark_nmixp_tabs(nmix$summ, input)$act_tab}, 
        options = list(searching = F,
                       paging = F,
                       searchable = F)
    )
    output$nmix_table_cov2 <- renderDataTable({
      if(is.null(nmix$summ))
        return()
      
      mark_nmixp_tabs(nmix$summ, input)$veg_tab}, 
      options = list(searching = F,
                     paging = F,
                     searchable = F)
    )
    output$nmix_table_cov3 <- renderDataTable({
      if(is.null(nmix$summ))
        return()
      
      mark_nmixp_tabs(nmix$summ, input)$snow_tab}, 
      options = list(searching = F,
                     paging = F,
                     searchable = F)
    )
    output$nmix_table_cov4 <- renderDataTable({
      if(is.null(nmix$summ))
        return()
      
      mark_nmixp_tabs(nmix$summ, input)$grp_tab}, 
      options = list(searching = F,
                     paging = F,
                     searchable = F)
    )
    output$nmix_psummary <- renderDataTable({
      if(is.null(nmix$summ))
        return()
      
      mark_nmixp_tabs(nmix$summ, input)$p_out}, 
      options = list(searching = F,
                     paging = F,
                     searchable = F)
    )
    output$nmix_est <- renderDataTable({
      if(is.null(input$nmix_fitgo))
        return()
      
      mark_nmix_tab(nmix$fit, nmix_subdata())}, 
      options = list(searching = F,
                     paging = F,
                     searchable = F)
    )
    
    #  Generate sightability report
    output$nmix_report <- renderUI({
      # Don't do anything if the button has never been pressed
      if (input$nmix_fitgo == 0)
        return()
      
      input$nmix_fitgo
      
      isolate({
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        reportSight <- nmix_html(nmix, nmix_subdata(), input, 
                                  html_document(), session)
        includeHTML(reportSight)
      })
    })

    output$nmix_downloadReport <- downloadHandler(
      filename = function(){
        paste(name_nmix(nmix$fit),
              switch(input$nmix_reportformat,
                     #PDF = "pdf",
                     HTML = "html",
                     Word = "docx"), 
              sep = ".")
      },
      content = function(file){
        reportSight <- nmix_html(nmix, 
                            nmix_subdata(),
                            input,
                            switch(input$nmix_reportformat,
                                   #"PDF" = pdf_document(),
                                   "HTML" = html_document(),
                                   "Word" = word_document()),
                            session)
        file.rename(reportSight, file)
      }
    )