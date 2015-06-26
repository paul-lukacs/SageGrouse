    #  Server code for running IPM's under the analysis heading
    #  Josh Nowak
    #  01/2015
###############################################################################
    #  Populate header with large text holding the species followed by IPM
    output$ipmsp <- renderText({
      if(input$dbname == "" | is.null(input$dbname))
        return()
  #    load("grouseStates.RData")
      updateSelectInput(session, "state", 
		    choices = levels(grouseStates$StateNames),
		    selected = "Montana")
        paste(input$critter, "IPM")
    })
    
    #  Header subtitle, display database name
    output$ipmsub1 <- renderText({
      if(input$dbname == "" | is.null(input$dbname))
        return()
      
      paste("Database:", input$dbname)
    })

    #  Header subtitle of IPM settings
    output$ipmsub2 <- renderText({
      if(input$state == "" | is.null(input$state) | is.null(input$year))
        return()      
      
      paste("State ", input$dau,
        " / Years ", input$year[1], ":", input$year[2], " / ",
        input$juvSmod, " Juvenile Survival / ",
        input$recruitMod, " Recruitment / ", 
        input$adultSmod, " Adult Survival / ", sep = "")
    })

    #  Need to make the data available to the input widgets
    modeldata <- reactive({
      if(input$critter == "" | input$dbname == "")
        return()
      
      read_db(input)
    })

    #  Dynamic slider for harvest input
    #  Male
    output$mharv <- renderUI({
      if (is.null(modeldata()))
        return()
      
      x <- modeldata()
      mh <- round(x$MaleHarvest[which.max(x$Year[!is.na(x$MaleHarvest)])])
      tags$div(title = "Use slider to set future level of harvest",
        sliderInput("mharv", "Male Proposed Harvest",
                    min = round(mh * 0.5), 
                    max = round(mh * 2), 
                    value = mh,
                    step = 1, 
                    round = T,
                    ticks = T,
                    width = "100%"))
      
    })
    
    #  Female
    output$fharv <- renderUI({
      if (input$state == "" | is.null(input$state) | is.null(modeldata()))
        return()
      
      x <- modeldata()
      fh <- round(x$FemaleHarvest[which.max(x$Year[!is.na(x$FemaleHarvest)])])
      
      tags$div(title = "Use slider to set future level of harvest",
        sliderInput("fharv", "Female Proposed Harvest",
                    min = round(fh * 0.5), 
                    max = round(fh * 2), 
                    value = fh,
                    step = 1, 
                    round = T,
                    ticks = T,
                    width = "100%"))
    })
    
    #  Plot of estimated harvest, male
    output$oldhm <- renderPlot({
      if(is.null(input$fharv) | input$state == "")
        return()
      
      x <- modeldata()
      harv_gg(x, sex = "male", input)
      
    })
    
    #  Plot of estimated harvest, female
    output$oldhf <- renderPlot({
      if(is.null(input$fharv))
        return()
      
      x <- modeldata()
      harv_gg(x, sex = "female", input)
      
    })
	
    #  Need a reactive container to hold output of model run
    ipmrun <- reactiveValues()
  
    #  Call model when needed
    observe({
      # Don't do anything if the button has never been pressed
      if (input$fitgo == 0)
        return()
      
      isolate({
        hold <- try(call_ipm(input), silent = T)
        if(class(hold) != "try-error"){
          showshinyalert(session, "ipmrunalert", 
            paste("Model run", input$fitgo, "successful!"),
            styleclass = "success")
          ipmrun$ipm <- hold
        }else{
          showshinyalert(session, "ipmrunalert", 
            paste("Model run", input$fitgo, "failed!"),
            styleclass = "danger")
        }
      })
    })
    #  Raw Data Display
    output$ipmraw <- renderDataTable({
      
      read_db(input)}, options = list(searching = FALSE,
                                      pageLength = 15,
                                      lengthMenu = list(c(5, 15, -1),
                                                        c("5", "15", "All")))
    )     
  
  	#  Tabular output
  	output$ipmtable1 <- renderDataTable({
  	  if (input$fitgo == 0)
  	    return()
      
      mark_ipm_pop_tbl(ipmrun$ipm)}, options = list(searching = FALSE,
                                                pageLength = 5,
                                                lengthMenu = list(c(5, 15, -1),
                                                          c("5", "15", "All")))
    )     
    output$ipmtable2 <- renderDataTable({
      if (input$fitgo == 0)
        return()
      
      mark_ipm_rat_tbl(ipmrun$ipm)}, options = list(searching = FALSE,
                                                pageLength = 5,
                                                lengthMenu = list(c(5, 15, -1),
                                                          c("5", "15", "All")))
    )     
    output$ipmtable3 <- renderDataTable({
      if (input$fitgo == 0)
        return()
      
      mark_ipm_surv_tbl(ipmrun$ipm)}, options = list(searching = FALSE,
                                                 pageLength = 5,
                                                 lengthMenu = list(c(5, 15, -1),
                                                          c("5", "15", "All")))
    )     


    #  Plot outputs
    ipm_pin <- reactive({ 
      # Don't do anything if the button has never been pressed
      if (input$fitgo == 0)
        return()
      
      summ_ipm(ipmrun$ipm)
    })
    
    #  Download tabular output
#    output$downloadipm <- downloadHandler(
#      filename = name_ipm(input),
#      content = function(file){
#        write.csv(table_ipm(ipmrun$ipm), file)
#      },
#      contentType = "text/csv"
#    )

    #  Plot outputs
    #  Text portions
    output$geomean <- renderText({ 
      # Don't do anything if the button has never been pressed
      if (input$fitgo == 0)
        return()
      
      paste("Mean Growth Rate: ", round(ipmrun$ipm$BUGS$mean$geoLambda, 2), 
            " (SD = ", round(ipmrun$ipm$BUGS$sd$geoLambda, 3), ")", sep = "") 
    })
    #  N's
    output$ipmplotN <- renderPlot({
      # Don't do anything if the button has never been pressed
      if (input$fitgo == 0)
        return()
      
      dat <- get_ipm_dat(input)
      ipm_plot_N(ipm_pin(), dat, input)
  	})
    #  Lambda
    output$ipmplotL <- renderPlot({
      # Don't do anything if the button has never been pressed
      if (input$fitgo == 0)
        return()
      
      ipm_plot_L(ipm_pin())
    })
    #  Survival
    output$ipmplotS <- renderPlot({
      # Don't do anything if the button has never been pressed
      if (input$fitgo == 0)
        return()
      
      dat <- get_ipm_dat(input)
      ipm_plot_S(ipm_pin(), dat, input)
    })
    #  Recruitment
    output$ipmplotR <- renderPlot({
      # Don't do anything if the button has never been pressed
      if (input$fitgo == 0)
        return()
      dat <- get_ipm_dat(input)
      ipm_plot_rat(ipm_pin(), dat, input)
    })

    output$downloadReport <- downloadHandler(
      filename = function(){
        paste(name_ipm(ipmrun),
                       switch(input$ipm_reportformat,
                              #PDF = "pdf",
                              HTML = "html",
                              Word = "docx"), 
                       sep = ".")
      },
      content = function(file){
        report <- mark_html(input, 
                            ipmrun$ipm, 
                            switch(input$ipm_reportformat,
                                    #"PDF" = pdf_document(),
                                    "HTML" = html_document(),
                                    "Word" = word_document()),
                            session)
        file.rename(report, file)
      }
    )

    output$ipm_report <- renderUI({
      # Don't do anything if the button has never been pressed
      if (input$fitgo == 0)
        return()
      
      input$fitgo
      
      isolate({
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        report <- mark_html(input, ipmrun$ipm, html_document(), session)
        includeHTML(report)
      })
    })



    