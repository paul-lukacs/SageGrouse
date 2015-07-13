    #  Server code for running sightability models
    #  Josh Nowak
    #  01/2015
	#  Paul Lukacs
	#  07/2015
###############################################################################
    #  Header subtitle
    output$sight_sp <- renderText({    
      #  Update daus when species changes
      updateSelectInput(session, "nmix_dau", 
                        choices = as.list(spp_area(daus, 
                                                   input$sight_critter)$Area))
      paste(input$sight_critter, "N-Mixture - In beta!")
      
    })

    #  Header model description
    output$nmix_sub <- renderText({
      
      paste(input$sight_dau,
            " / Year: ", input$sight_year,
            " / Model Type: ", input$sight_surveytype,
            " / Aircraft: ", input$sight_aircraft, sep = "")

    })

    #  Create a container to hold the sightability data
    sight <- reactiveValues()
    
    #  Get data from database when requested by user
    observe({
      # Don't do anything if the button has never been pressed
      if(input$sight_getdata == 0)
        return()
      
      isolate({
        hold <- try(api_connect("sightability", 
                                input$sight_critter, 
                                input$sight_dau, T), silent = T)
        if(class(hold) != "try-error"){
          sight$data <- hold
          updateSelectInput(session, "nmix_year", 
                            choices = sort(unique(hold$Bio_Year),
                                           decreasing = T))
          showshinyalert(session, "nmix_runalert", 
                         paste(input$sight_dau, 
                               "sightability data successfully downloaded!"),
                         styleclass = "success")                                          

        }else{
          showshinyalert(session, "sight_runalert", 
                         paste("Download failed!"),
                         styleclass = "danger")
        }
      })
    })

    #  Subset data to year and survey type
    sight_subdata <- reactive({
      if(input$sight_getdata == 0)
        return()
      
      tmp <- get_sight(sight$data, input)
      
    return(tmp)
    })

    #  call model when requested by user
    observe({
      # Don't do anything if the button has never been pressed
      if(input$sight_fitgo == 0)
        return()
      
      isolate({
        hold_sightmodel <- try(sight_wrapper(sight_subdata(), input), 
                               silent = T)
        
        if(class(hold_sightmodel) != "try-error"){
          
          showshinyalert(session, "sight_runalert", 
                         paste(input$sight_year,
                               input$sight_dau, 
                               "sightability model",
                               input$sight_fitgo,
                               "successfully ran!"),
                         styleclass = "success")

          sight$fit <- hold_sightmodel
          sight$summ <- try(sight.muleDeer(input, sight_subdata()))
          
        }else{

          showshinyalert(session, "sight_runalert", 
                         paste(input$sight_year,
                               input$sight_dau, 
                               "sightability model",
                               input$sight_fitgo,
                               "failed!"),
                         styleclass = "danger")
        }
      })
    })

    #  Data Check
    output$sight_vertout <- renderPrint({
    if(input$sight_getdata == 0)
      return("No Data Loaded")
    
      sight_err(sight$data, input)
    
    })
      
    output$sight_raw <- renderDataTable({
      if(is.null(sight_subdata()))
        return()
      
      sight_subdata()}, options = list(searching = T,
                                      pageLength = 15,
                                      lengthMenu = list(c(15, 25, -1),
                                                        c("15", "25", "All")))
    )

    observe({
      if(is.null(sight$data))
        return()
      
      input$sight_year
      
      tmp <- sight$data %>% 
              filter(Bio_Year == input$sight_year)
        
      updateSelectInput(session, "sight_surveytype", 
                        choices = sort(unique(tmp$Survey_Type))) 
      
    })

    observe({
      if(is.null(sight$data))
        return()
      
      input$sight_getdata
      
      isolate({
        updateSelectInput(session, "sight_aircraft", 
                          choices = switch(input$sight_critter,
                                           "Mule Deer" = "Mule Deer",
                                           "Elk" = list("Hiller 12E",
                                                        "Hiller 12E no snow",
                                                        "Bell 47G"))) 
      })    
    })

    output$sight_plot_dem <- renderPlot({
      if(is.null(sight_subdata()) | input$sight_fitgo == 0)
        return()
      
      mark_sight_plot_comp(sight$fit, sight_subdata())
      
    })
    output$sight_plot_total <- renderPlot({
      if(is.null(sight_subdata()) | input$sight_fitgo == 0)
        return()
      
      mark_sight_plot_total(sight$fit, sight_subdata())
      
    })    
    output$sight_table_cov1 <- renderDataTable({
      if(is.null(sight$summ))
        return()
      
      mark_sightp_tabs(sight$summ, input)$act_tab}, 
        options = list(searching = F,
                       paging = F,
                       searchable = F)
    )
    output$sight_table_cov2 <- renderDataTable({
      if(is.null(sight$summ))
        return()
      
      mark_sightp_tabs(sight$summ, input)$veg_tab}, 
      options = list(searching = F,
                     paging = F,
                     searchable = F)
    )
    output$sight_table_cov3 <- renderDataTable({
      if(is.null(sight$summ))
        return()
      
      mark_sightp_tabs(sight$summ, input)$snow_tab}, 
      options = list(searching = F,
                     paging = F,
                     searchable = F)
    )
    output$sight_table_cov4 <- renderDataTable({
      if(is.null(sight$summ))
        return()
      
      mark_sightp_tabs(sight$summ, input)$grp_tab}, 
      options = list(searching = F,
                     paging = F,
                     searchable = F)
    )
    output$sight_psummary <- renderDataTable({
      if(is.null(sight$summ))
        return()
      
      mark_sightp_tabs(sight$summ, input)$p_out}, 
      options = list(searching = F,
                     paging = F,
                     searchable = F)
    )
    output$sight_est <- renderDataTable({
      if(is.null(input$sight_fitgo))
        return()
      
      mark_sight_tab(sight$fit, sight_subdata())}, 
      options = list(searching = F,
                     paging = F,
                     searchable = F)
    )
    
    #  Generate sightability report
    output$sight_report <- renderUI({
      # Don't do anything if the button has never been pressed
      if (input$sight_fitgo == 0)
        return()
      
      input$sight_fitgo
      
      isolate({
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        reportSight <- sight_html(sight, sight_subdata(), input, 
                                  html_document(), session)
        includeHTML(reportSight)
      })
    })

    output$sight_downloadReport <- downloadHandler(
      filename = function(){
        paste(name_sight(sight$fit),
              switch(input$sight_reportformat,
                     #PDF = "pdf",
                     HTML = "html",
                     Word = "docx"), 
              sep = ".")
      },
      content = function(file){
        reportSight <- sight_html(sight, 
                            sight_subdata(),
                            input,
                            switch(input$sight_reportformat,
                                   #"PDF" = pdf_document(),
                                   "HTML" = html_document(),
                                   "Word" = word_document()),
                            session)
        file.rename(reportSight, file)
      }
    )