    #  Server code for running simulations under the data heading
    #  Josh Nowak
    #  01/2015
################################################################################
    #  Plots of input values
    output$siminplot <- renderPlot({
      
      #  Plots
      par(mfrow = c(3, 1))
      
      #######################  Initial Abundance
      #  Collect inputs
      indens <- list(c(input$muN1, input$sdN1),
                     c(input$muN2, input$sdN2),
                     c(input$muN3, input$sdN3))
      
      #  Generate plot data
      pd <- lapply(indens, function(x){
        if(x[2] == 0){
          out <- matrix(c(x[1], x[2]), ncol = 2, nrow = 1)
        }
        if(x[2] > 0){
          out <- do.call(cbind, 
                         density(
                           rnorm(10000, x[1], x[2]))[1:2])
        }
        out
      })
      
      #  Limits for axes
      lo.hi <- apply(
        do.call(
          rbind, 
          lapply(pd, function(x) apply(x, 2, range))), 2, range)
      
      #  Add plots of inputs
      plot(1, 1, 
           main = "Initial Abundances",
           xlab = "N",
           ylab = "Density",
           xlim = lo.hi[,1],
           ylim = lo.hi[,2],
           bty = "n",
           cex.lab = 1.5,
           cex.main = 1.7,
           cex.axis = 1.2)
      
      lapply(1:length(pd), function(i){
        if(nrow(pd[[i]]) == 1){
          abline(v = pd[[i]][1], 
                 col = c("pink", "red", "blue")[i],
                 lwd = 2)
        }
        if(nrow(pd[[i]]) != 1){
          lines(pd[[i]][,1], pd[[i]][,2], 
                col = c("pink", "red", "blue")[i],
                lwd = 2)
        }
      })
  
      #############################  Demographic Rates
      #  Collect inputs
      indens <- list(c(input$phif, input$sdphif),
                     c(input$phiadf, input$sdphiadf),
                     c(input$phiadm, input$sdphiadm))
      
      #  Generate plot data
      pd <- lapply(indens, function(x){
        if(x[2] == 0){
          out <- matrix(c(x[1], x[2]), ncol = 2, nrow = 1)
        }
        if(x[2] > 0){
          out <- do.call(cbind, 
                         density(
                           plogis(rnorm(10000, qlogis(x[1]), x[2])))[1:2])
        }
        out
      })
      
      #  Limits for axes
      lo.hi <- apply(
        do.call(
          rbind, 
          lapply(pd, function(x) apply(x, 2, range))), 2, range)
      
      #  Add plots of inputs
      plot(1, 1, 
           main = "Input Survival Probabilities",
           xlab = "Survival",
           ylab = "Density",
           xlim = lo.hi[,1],
           ylim = lo.hi[,2],
           bty = "n",
           cex.lab = 1.5,
           cex.main = 1.7,
           cex.axis = 1.2)
      
      lapply(1:length(pd), function(i){
        if(nrow(pd[[i]]) == 1){
          abline(v = pd[[i]][1], 
                 col = c("pink", "red", "blue")[i],
                 lwd = 2)
        }
        if(nrow(pd[[i]]) != 1){
          lines(pd[[i]][,1], pd[[i]][,2], 
                col = c("pink", "red", "blue")[i],
                lwd = 2)
        }
      })
      
      #############################  Pregnancy Rate
      pregger <- rnorm(10000, input$mupreg, input$sdpreg)
      if(input$sdpreg == 0){
        plot(input$mupreg, 
             type = "n", 
             ylab = "Density", ,
             cex.lab = 1.5,
             cex.main = 1.7,
             cex.axis = 1.2, 
             bty = "n", 
             main = "Pregnancy Rate",
             xlab = "Pregnancy")
        abline(v = input$mupreg, lwd = 2, col = "green")
      }else{
        plot(density(pregger), 
             lwd = 2, 
             col = "green", 
             cex.lab = 1.5,
             cex.main = 1.7,
             cex.axis = 1.2, 
             bty = "n", 
             main = "Pregnancy Rate",
             xlab = "Pregnancy")
      }
      
    })

    #  Need to collect simulation inputs
    rsim <- function(simParams){
      #  Create dependency on Run Simulation button
      input$simgo
      
      #  Gather parameters and format for sim fun
      params <- lapply(simParams, function(i){
        input[[i]]
      })
      
      #  Collect initial abudance estimates
      muinit <- unlist(params[c(2, 4, 6)])
      sdinit <- unlist(params[c(3, 5, 7)])
      
      #  relist
      out <- c(params[1], list(muinit, sdinit), params[8:15])
      names(out) <- c("n.years", 
                      "mu.N.init", "sd.N.init",
                      "mu.preg", "sd.preg",
                      "mu.phi.fawn", "sd.phi.fawn",
                      "mu.phi.adf", "sd.phi.adf",
                      "mu.phi.adm", "sd.phi.adm")
      out
    }

    #  Call simulation
    simmer <- reactive({
      input$simgo
      do.call(sim.deer, isolate(rsim(simParams)))
    })

    #  Remind user of key inputs
    output$regsumm2 <- renderText({
      input$simgo
      paste("Simulations performed for", isolate(input$nyr), 
            "years and repeated", isolate(input$niters), "times.")
    })
    
    #  Plot outputs of simulation runs
    output$simoutplot <- renderPlot({
      input$simgo
      isolate({
        #
        par(mfrow = c(3, 1))
        #
        lo.hiN <- c(range(simmer()[,c(1:3)]))
        #  Abundance
        plot(1:input$nyr, simmer()[,1],
             main = "Simulated Abundance",
             xlab = "Year",
             ylab = "N",
             ylim = lo.hiN,
             type = "l",
             cex.lab = 1.5,
             cex.main = 1.7,
             cex.axis = 1.2,
             bty = "n",
             col = "pink"
        )
        lines(1:input$nyr, simmer()[,2], col = "red")
        lines(1:input$nyr, simmer()[,3], col = "blue")
        #  Survival
        lo.hiS <- c(range(simmer()[,c(5:7)]))
        #
        plot(1:input$nyr, simmer()[,5],
             main = "Simulated Survival",
             xlab = "Year",
             ylab = "Survival",
             ylim = lo.hiS,
             type = "l",
             cex.lab = 1.5,
             cex.main = 1.7,
             cex.axis = 1.2,
             bty = "n",
             col = "pink"
        )
        lines(1:input$nyr, simmer()[,6], col = "red")
        lines(1:input$nyr, simmer()[,7], col = "blue")
        #  Pregnancy and Ratios
        lo.hiP <- c(range(simmer()[,c(4,8:9)]))      
        #
        plot(1:input$nyr, simmer()[,4],
             main = "Simulated Survival",
             xlab = "Year",
             ylab = "Survival",
             ylim = lo.hiP,
             type = "l",
             cex.lab = 1.5,
             cex.main = 1.7,
             cex.axis = 1.2,
             bty = "n",
             col = "green"
        )
        lines(1:input$nyr, simmer()[,6], col = "purple")
        lines(1:input$nyr, simmer()[,7], col = "orange")
        
      })
    })

    #  Create tabular output of simulations
    output$simtable <- renderDataTable(simmer(), options = list(
      pageLength = 5,
      lengthMenu = list(c(5, 10, 20, 40, -1), 
                        c('5', '10', '20', '40', 'All'))))