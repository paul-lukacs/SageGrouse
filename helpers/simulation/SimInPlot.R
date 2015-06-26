      #  Simulation Input Script
      #  Oct 2014
      #  Josh Nowak
################################################################################
      #  Text then plots
      SimInPlot <- function(){
        #  Text
        yrtxt <- paste("Number of Years =", input$nyr)  
        ittxt <- paste("Number of Iterations =", input$niters)
        #  Plots
        layout(matrix(rep(1:8, each = 2), ncol = 2, byrow = T))
        #  Empty plot
        plot(seq(0, 1, by = 0.1), seq(0, 1, by = 0.1), type = "n", xaxt = "n", 
             yaxt = "n", xlab = "", ylab = "", main = "", bty = "n")
        #  Add text
        text(c(0.5, .5), c(0.1, 0.7), c(yrtxt, ittxt))
        #  Add histograms of inputs
        hist(rnorm(100000, input$muN1))
        hist(rnorm(100000, input$muN2))
        hist(rnorm(100000, input$muN3))
        
      }