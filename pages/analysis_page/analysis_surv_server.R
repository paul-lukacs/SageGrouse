      #  Server code for running survival models
      #  Josh Nowak
      #  03/2015
###############################################################################
      #  Header subtitle
      output$surv_sp <- renderText({    
        #  Update daus when species changes
        updateSelectInput(session, "surv_dau", 
                          choices = as.list(spp_area(daus, 
                                                     input$surv_critter)$Area))
        
        paste(input$surv_critter, "Survival - In Testing!")
        
      })

      output$surv_sub <- renderText({
        
        paste(input$surv_dau,
              " / Year: ", input$surv_year,
              " / Age: ", input$surv_age,
              " / Sex: ", ifelse(is.null(input$surv_exclude_sex), "Both",
                                 switch(input$surv_exclude_sex,
                                        "Female" = "Male",
                                        "Male" = "Female")),
              " / Season: ", input$surv_season, sep = "")
        
      })

      #  Create a container to hold the survival data
      surv <- reactiveValues()

      #  Get data from database when requested by user
      observe({
        # Don't do anything if the button has never been pressed
        if(input$surv_getdata == 0)
          return()
        
        isolate({
          hold <- try(api_connect("survival", 
                                  input$surv_critter, 
                                  input$surv_dau, T), silent = T)
          if(class(hold) != "try-error"){
            showshinyalert(session, "surv_runalert", 
                           paste(input$surv_dau, 
                                 "survival data download successful!"),
                           styleclass = "success")
            surv$data <- hold
            updateSelectInput(session, "surv_year", 
                              choices = sort(unique(hold$Bio_Year), 
                                             decreasing = T))
          }else{
            showshinyalert(session, "surv_runalert", 
                           paste("API Download failed!",
                                 "Please include the following in a bug report",
                                 input$surv_critter,
                                 input$surv_dau),
                           styleclass = "danger")
          }
        })
      })

      #  Update age inputs
      observe({
        if(is.null(surv$data))
          return()
        
        tmp_choice <- surv$data %>% 
          filter(Bio_Year == input$surv_year)
        sa_names <- sort(unique(tmp_choice$Age_Class))
        updateSelectInput(session, "surv_age", 
                          choices = sa_names[sa_names %in% c("Fawn", "Adult")])
        
      })

      #  Subset data given inputs
      surv_subdata <- reactive({
        if(input$surv_getdata == 0)
          return()
        
        tmp <- try(get_survd(surv$data, input), silent = T)
        
        if(class(tmp) == "try-error"){
          showshinyalert(session, "surv_runalert", 
                         paste("Survival data subset failed!",
                               "Please include the following in a bug report:",
                               input$surv_critter, "/",
                               input$surv_dau,"/",
                               input$surv_year,"",
                               input$surv_age,"/",
                               input$surv_season, "/",
                               "Survival data subset failure", "/",
                               attributes(tmp)$condition),
                         styleclass = "danger")
        }
        
        return(tmp)
      })

      #  call model when requested by user
      observe({
        # Don't do anything if the button has never been pressed
        if(input$surv_fitgo == 0)
          return()
        
        isolate({
          hold_survmodel <- try(surv_wrapper(surv_subdata(), input, T), 
                                silent = T)

          if(class(hold_survmodel) != "try-error"){
            showshinyalert(session, "surv_runalert", 
                           paste(input$surv_year,
                                 input$surv_dau, 
                                 "survival model",
                                 input$surv_fitgo,
                                 "successfully ran!"),
                           styleclass = "success")
            surv$fit <- hold_survmodel
            surv$fit_summ <- summ_surv(hold_survmodel)
          }else{
            showshinyalert(session, "surv_runalert", 
                           paste("Survival model failed to run!",
                                 "Please include the following in a bug report:",
                                 input$surv_critter, "/",
                                 input$surv_dau,"/",
                                 input$surv_year,"/",
                                 input$surv_age,"/",
                                 input$surv_season, "/",
                                 "survival model call failed with error",
                                 attributes(hold_survmodel)$condition),
                           styleclass = "danger")
            surv$fit <- NA
          }
        })
      })

      #  Raw survival data table
      output$surv_raw <- renderDataTable({
        if(is.null(surv_subdata()) | length(surv_subdata()) < 2)
          return()
        
        surv_subdata()$out}, 
        options = list(searching = T,
                       pageLength = 15,
                       lengthMenu = list(c(15, 25, -1),
                                         c("15", "25", "All")))
      )

      #  Sample size gauges
      output$surv_ss_gauge <- renderGvis({
        if(is.null(surv_subdata()$out))
          return()
        uni <- length(unique(surv_subdata()$out$Animal_ID))
        df <- data.frame(Label = "Marked", Value = uni)
        gvisGauge(df,
                  options = list(min = 0, max = 200, 
                                 greenColor = "#449d44",
                                 greenFrom = 100, greenTo = 200, 
                                 yellowColor = "#f0ad4e",
                                 yellowFrom = 50, yellowTo = 100,
                                 redColor = "#d9534f",
                                 redFrom = 0, redTo = 50, 
                                 width = 175, height = 175))
        
      })
      output$surv_dead_gauge <- renderGvis({
        if(is.null(surv_subdata()$out))
          return()
        
        meh <- make_eh(surv_subdata(), input)
        dead <- sum(meh$dead == 1)
        df <- data.frame(Label = "Dead", Value = dead)
        gvisGauge(df,
                  options = list(min = 0, max = length(meh$dead),
                                 greenColor = "#449d44",
                                 greenFrom = length(meh$dead)/2, 
                                 greenTo = length(meh$dead), 
                                 yellowColor = "#f0ad4e",
                                 yellowFrom = length(meh$dead)/4, 
                                 yellowTo = length(meh$dead)/2,
                                 redColor = "#d9534f",
                                 redFrom = 0, 
                                 redTo = length(meh$dead)/4, 
                                 width = 175, height = 175))
        
      })
      output$surv_cens_gauge <- renderGvis({
        if(is.null(surv_subdata()$out))
          return()
        
        meh <- make_eh(surv_subdata(), input)
        cens <- sum(meh$dead == 0)
        df <- data.frame(Label = "Censored", Value = cens)
        gvisGauge(df,
                  options = list(min = 0, max = length(meh$dead), 
                                 greenColor = "#449d44",
                                 greenFrom = 0, 
                                 greenTo = length(meh$dead)/4, 
                                 yellowColor = "#f0ad4e",
                                 yellowFrom = length(meh$dead)/4, 
                                 yellowTo = length(meh$dead)/2,
                                 redColor = "#d9534f",
                                 redFrom = length(meh$dead)/2, 
                                 redTo = length(meh$dead), 
                                 width = 175, height = 175))
        
      })
      output$surv_sst_gauge <- renderGvis({
        if(is.null(surv_subdata()$out))
          return()
        uni <- length(unique(surv_subdata()$out$Animal_ID))
        df <- data.frame(Label = "Marked", Value = uni)
        gvisGauge(df,
                  options = list(min = 0, max = 200, 
                                 greenColor = "#449d44",
                                 greenFrom = 100, greenTo = 200, 
                                 yellowColor = "#f0ad4e",
                                 yellowFrom = 50, yellowTo = 100,
                                 redColor = "#d9534f",
                                 redFrom = 0, redTo = 50, 
                                 width = 175, height = 175))
        
      })
      output$surv_deadt_gauge <- renderGvis({
        if(is.null(surv_subdata()$out))
          return()
        
        meh <- make_eh(surv_subdata(), input)
        dead <- sum(meh$dead == 1)
        df <- data.frame(Label = "Dead", Value = dead)
        gvisGauge(df,
                  options = list(min = 0, max = length(meh$dead),
                                 greenColor = "#449d44",
                                 greenFrom = length(meh$dead)/2, 
                                 greenTo = length(meh$dead), 
                                 yellowColor = "#f0ad4e",
                                 yellowFrom = length(meh$dead)/4, 
                                 yellowTo = length(meh$dead)/2,
                                 redColor = "#d9534f",
                                 redFrom = 0, 
                                 redTo = length(meh$dead)/4, 
                                 width = 175, height = 175))
        
      })
      output$surv_censt_gauge <- renderGvis({
        if(is.null(surv_subdata()$out))
          return()
        
        meh <- make_eh(surv_subdata(), input)
        cens <- sum(meh$dead == 0)
        df <- data.frame(Label = "Censored", Value = cens)
        gvisGauge(df,
                  options = list(min = 0, max = length(meh$dead), 
                                 greenColor = "#449d44",
                                 greenFrom = 0, 
                                 greenTo = length(meh$dead)/4, 
                                 yellowColor = "#f0ad4e",
                                 yellowFrom = length(meh$dead)/4, 
                                 yellowTo = length(meh$dead)/2,
                                 redColor = "#d9534f",
                                 redFrom = length(meh$dead)/2, 
                                 redTo = length(meh$dead), 
                                 width = 175, height = 175))
        
      })
      #  Survival model fit tabular output
      output$surv_table <- renderDataTable({
        if(input$surv_fitgo == 0)
          return()
        
        mark_surv_tbl(surv$fit_summ)}, 
        options = list(searching = F,
                       pageLength = 15,
                       lengthMenu = list(c(15, -1),
                                         c("15", "All")))
      )

      #  Survival model fit plots
      output$surv_plot_ts <- renderPlot({
        if(input$surv_fitgo == 0)
          return()
        
        mark_surv_ts(surv$fit_summ)
      })
      output$surv_plot_mm <- renderPlot({
        if(input$surv_fitgo == 0)
          return()
        
        mark_surv_post(surv$fit, "Smon", "darkred")
      })
      output$surv_mm_txt <- renderText({
        if(is.null(surv$fit))
          return()
        
        paste("Mean monthly survival:", round(surv$fit$BUGS$mean$Smon, 2))
      })
      output$surv_plot_sp <- renderPlot({
        if(input$surv_fitgo == 0)
          return()
        
        mark_surv_post(surv$fit, "Sann", "darkgreen")
      })
      output$surv_sp_txt <- renderText({
        if(input$surv_fitgo == 0)
          return()
        
        paste("Mean", 
              switch(surv$fit$age, 
                     "Fawn" = "Dec 15-June 01", 
                     "Adult" = tolower(surv$fit$season)), 
              "survival of",
              paste(tolower(surv$fit$age), "s", sep = ""),
              paste("(", surv$fit$year, "):", sep = ""),
              round(surv$fit$BUGS$mean$Sann, 2))
      })

      output$surv_prop_cens <- renderGvis({
        if(input$surv_fitgo == 0)
          return()
        
        mark_surv_pmort(surv_subdata(), T)
        
        
      })

      output$surv_prop_mort <- renderGvis({
        if(input$surv_fitgo == 0)
          return()
        
        mark_surv_pmort(surv_subdata(), F)
        
        
      })

      output$surv_downloadReport <- downloadHandler(
        filename = function(){
          paste(name_surv(surv),
                switch(input$surv_reportformat,
                       #PDF = "pdf",
                       HTML = "html",
                       Word = "docx"), 
                sep = ".")
        },
        content = function(file){
          report <- surv_html(surv, 
                              surv_subdata(),
                              input,
                              switch(input$surv_reportformat,
                                     #"PDF" = pdf_document(),
                                     "HTML" = html_document(),
                                     "Word" = word_document()),
                              session)
          file.rename(report, file)
        }
      )
      
      output$surv_report <- renderUI({
        # Don't do anything if the button has never been pressed
        if (input$surv_fitgo == 0)
          return()
        
        input$surv_fitgo
        
        isolate({
          
          # temporarily switch to the temp dir, in case you do not have write
          # permission to the current working directory
          reportS <- surv_html(surv, surv_subdata(), input, html_document(), 
                               session)
          includeHTML(reportS)
        })
      })
