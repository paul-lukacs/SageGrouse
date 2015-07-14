
tabPanel("IPM",
fixedPage(
  fluidRow(
    headerPanel(list(
      h4(textOutput("ipmsp", inline = T)),
      h6(textOutput("ipmsub1", inline = T), style = "color:darkgray"),
      h6(textOutput("ipmsub2", inline = T), style = "color:darkgray")))
  ),
  shinyalert("ipmrunalert", click.hide = FALSE, auto.close.after = NULL),
  tabsetPanel(
    tabPanel("Setup",
      hr(),
      h4("Define Species & IPM Database"),
      wellPanel(
        fluidRow(
          column(6,
            tags$div(title = "Select a species for analysis",
              select2Input("critter", "Species", 
                           choices = list("Sage Grouse"),
                           type = "select")
            )
          ),
          column(6,
            tags$div(title = "Select a database to use in the analysis",
              select2Input("dbname", "Database",
                           choices = list(DbName),
                           type = "select")
            )
          )
        )
      ),
      h4("Define Space & Time"),
      wellPanel(
        fluidRow(
          column(5,
            tags$div(title = "Select a state to analyze", 
              selectizeInput("state", "Analysis State", 
                choices = "",
                selected = "Montana",
                options = list(dropdownParent = "body",
                  maxItems = 1))
            )
          ),
          column(7,
            tags$div(title = "Choose years to analyze",
              sliderInput("year", "Analysis Years", 
                          min = 1980, 
                          max = 1900 + as.POSIXlt(Sys.time())$year + 5,
                          value = c(2002, 1900 + 
                                      as.POSIXlt(Sys.time())$year + 2), 
                          step = 1,
                          ticks = TRUE,
                          sep = "",
                          width = "100%")
            )
          )
        )
      ),
      
      h4("Demographic Variation"),
      wellPanel(
        fluidRow(
          column(4, 
            tags$div(title = "Choose how recruitment varies or hold it constant",
              select2Input("recruitMod", "Recruitment",
                              choices = c("Constant", 
                                          "Time Varying"),
                           type = "select")
            )
          ),
          column(4,
            tags$div(title = "Choose how juvenile survival varies or hold it constant",
              select2Input("juvSmod", "Juvenile Survival",
                           choices = list("Constant", "Time Varying"),
                           type = "select")
            )
          ),
          column(4,
            tags$div(title = "Choose how adult survival varies or hold it constant",
              select2Input("adultSmod", "Adult Survival",
                           choices = list("Constant", "Time Varying"),
                           type = "select")
            )
          )
        )
      ),
 #     
 #    h4("Future Harvest"),
 #     wellPanel(
 #       fluidRow(
 #         column(6, 
 #                plotOutput("oldhm", height = "200px"),
 #                uiOutput("mharv")),
 #         column(6, 
 #                plotOutput("oldhf", height = "200px"),
 #                uiOutput("fharv"))
 #       )
 #     ),
      
      h4("MCMC Controls"),
      wellPanel(
        fluidRow(
          column(4,
            tags$div(title = "Change at will if you know what you are doing",
              sliderInput("ipmburn", "Burnin Length", 
                min = 1000, 
                max = 50000, 
                value = 15000,
                step = 5000, 
                round = T,
                width = "100%")
            )
          ),                
          column(4,
            tags$div(title = "Change at will if you know what you are doing",
              sliderInput("ipmiter", "MCMC Iterations", 
                min = 10000, 
                max = 55000, 
                value = 25000,
                step = 5000, 
                round = T,
                width = "100%")
            )
          ),
          column(4,
            tags$div(title = "Change at will if you know what you are doing",
              sliderInput("ipmthin", "Thinning Rate", 
                min = 1, 
                max = 100, 
                value = 2,
                step = 1, 
                round = T,
                width = "100%")
            )
          )
        ),
        fluidRow(
          tags$div(title = "If checked the model will run until convergence or some maximum number of iterations is reached...this could take a while!",
                   checkboxInput("autoup", label = "Automate MCMC Convergence", 
                                 value = FALSE)) 
        )
      ),
      fluidRow(
          tags$div(title = "Click here to run the model", 
                   actionButton("fitgo", "Fit Model", 
                                 styleclass = "primary",
                                 size = "large",
                                 icon = "gears",
                                 icon.library = "font awesome"),
                   style = "padding-bottom: 200px;
                            padding-right:15px;
                            float:right"
          )
    )
    , icon = icon("tasks"), id = "ipmsetup"),  #  tabPanel Setup
    tabPanel("Raw Data",
             hr(),
             tags$div(dataTableOutput("ipmraw"), style = "padding-bottom:200px")       
    , icon = icon("folder-open"), id = "ipmraw"),  #  tabPanel Raw Data
    tabPanel("Plots",
      hr(),
      h4("Population Size"),
      plotOutput("ipmplotN", height = "350px"),
      checkboxInput("ipm_nfd", label = "Show field data", value = FALSE),
      checkboxInput("ipm_nserr", label = "Add CI", value = FALSE),      
      hr(style = "background:gray;
         border:0;
         height:2px;
         width:100%"),
      h4("Growth Rate"),
      plotOutput("ipmplotL", height = "350px"),
      textOutput("geomean"),
      hr(style = "background:gray;
         border:0;
         height:2px;
         width:100%"),
      h4("Survival"),
      plotOutput("ipmplotS", height = "350px"),
      tags$div(
        h6(HTML(paste(tags$span(style = "color:red", "- Adult Females"), 
                   tags$span(style = "color:blue", "- Adult Males"),
                   tags$span(style = "color:green", "- Juveniles"), sep = " ")))
      ),
      checkboxInput("ipm_sfd", label = "Show field data", value = FALSE),
      checkboxInput("ipm_sserr", label = "Add CI", value = FALSE),   
      hr(style = "background:gray;
         border:0;
         height:2px;
         width:100%"),
      h4("Sex & Age Ratios"),
      plotOutput("ipmplotR", height = "350px"),
      tags$div(
        h6(HTML(paste(tags$span(style = "color:red", "- Male/100 Females"), 
                      tags$span(style = "color:blue", "- Juveniles/100 Females"), 
                      sep = " ")))
      ),
      checkboxInput("ipm_rfd", label = "Show field data", value = FALSE),
      tags$div(checkboxInput("ipm_rserr", label = "Add CI", value = FALSE), 
                style = "padding-bottom:200px")   
      
    , icon = icon("bar-chart-o"), id = "ipm_plottab"
    ),  #  tabPanel, IPM plot output
     
    tabPanel("Table",
      hr(),
      h4("Population Size"),
      dataTableOutput("ipmtable1"),
      h4("Sex & Age Ratios"),
      dataTableOutput("ipmtable2"),
      h4("Survival"),
      tags$div(dataTableOutput("ipmtable3"), 
               style = "padding-bottom:200px"),      
      
      icon = icon("table"),
      id = "ipmtable"
    ),  #  tabPanel, IPM tabular output
    tabPanel("Report",
      hr(),
      wellPanel(
        radioButtons("ipm_reportformat", "Download Format", 
                     c("HTML", "Word")),
        downloadButton("downloadReport", label = "Download")
      ),
      uiOutput("ipm_report"),
      icon = icon("file-o"),
      id = "ipmreport"
    )
  , type= "pills")  #  tabsetPanel
  )  
)  #  tabPanel IPM
