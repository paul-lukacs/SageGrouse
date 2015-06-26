    #  Server code for the data explorer page under the data heading
    #  Josh Nowak
    #  01/2015
################################################################################
    #  Data loader
    vdata <- reactive({
      if(is.null(input$critter)){
        NULL
      }else if(input$critter == "Mule Deer"){
        modelCon <- odbcConnectAccess2007(DbName)                    
        queryTxt <- "SELECT * FROM tblPopModelData WHERE species='MD'"
        tmp <- sqlQuery( modelCon, queryTxt )
        tmp2 <- tmp[order(tmp$DAU, tmp$Year),]
        odbcClose(modelCon)
        return(tmp2)
      }
    })
    
    #  Tabular data
    output$viewdat <- renderDataTable({
      vd <- vdata()
      if(!is.data.frame(vd)){ 
        return(NULL)
      }else{
        apply(vd[,input$disp], 2, as.character)
      }
    },
    options = list(
      pageLength = 10,
      lengthMenu = list(
        c(10, 25, 50, 100, -1), 
        c("10", "25", "50", "100", "All")
      )
    )
    )

    #  Reactive panel because columns change with data type
    output$showcols <- renderUI({
      xx <- vdata()
      updateTypeAhead(session, "xcol", 
                      label = "X-Column",
                      choices = colnames(xx))
      updateTypeAhead(session, "ycol", 
                      label = "Y-Column",
                      choices = colnames(xx))            
      checkboxGroupInput(inputId = "disp", 
                         label = "Columns to show:", 
                         choices = colnames(xx), 
                         selected = colnames(xx),
                         inline = F)
      
    }
    )

    #  Data explorer plotting code
    output$viewplot <- renderPlot({
      raw <- vdata()
      if(is.null(input$ycol) | input$ycol == "")
        return()        
      y <- raw[,input$ycol]
      if(!is.null(input$xcol) & input$xcol != ""){
        x <- raw[,input$xcol]
      }
      if(input$pdrad == "Histogram"){
        hist(y)  
      }
      if(input$pdrad == "Box Plot" & input$xcol != ""){
        boxplot(y ~ x)
      }
      if(input$pdrad == "XY Plot" & input$xcol != ""){
        xy <- cbind(x, y)
        xy <- data.frame(xy[complete.cases(xy),])
        xy <- xy[order(xy[,1]),]
        plot(xy[,1], xy[,2], xlab = "x", ylab = "y", bty = "n")
        if(input$regress & input$xcol != "" & input$ycol != ""){
          lm.out <- lm(y ~ x, data = xy)
          pred <- predict(lm.out, interval = "confidence")
          lines(xy[,1], pred[,1], lwd = 2)
          lines(xy[,1], pred[,2], lty = 2)
          lines(xy[,1], pred[,3], lty = 2)
          output$regsumm <- renderPrint({ summary(lm.out) })
        }
      }
    })