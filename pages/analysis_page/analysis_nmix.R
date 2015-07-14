#
# N mixture analysis interface for sage-grouse
# Paul Lukacs 7/2015
#
tabPanel("N-Mixture",
fixedPage(
  fluidRow(
    headerPanel(list(
      h4(textOutput("nmix_sp", inline = T)),
      h6(textOutput("nmix_sub", inline = T), style = "color:darkgray")))

  ),
  shinyalert("nmix", click.hide = FALSE, auto.close.after = NULL),
  tabsetPanel(
    tabPanel("Setup",
      hr(),
	  
########################################################################	  
 #     h4("Define Species & Space"),
 #     wellPanel(
 #       fluidRow(
#          column(4,
#            tags$div(title = "Select a species for analysis",
#              select2Input("nmix_critter", "Species", 
#                          choices = list("Sage Grouse"),
#                          type = "select")
#            )
#          ),
#          column(4,
#            tags$div(title = "Select a unit to analyze", 
#              select2Input("nmix_dau", "Analysis Unit", 
#                          choices = "",
#                          type = "select")
#            )
#          ),
#          column(4,
#                 tags$div(title = "Click to download data from the database", 
#                          actionButton("nmix_getdata", "Get Data", 
#                                       styleclass = "",
#                                       size = "large",
#                                       icon = "download",
#                                      icon.library = "font awesome",
#                                       block = T)
#                 ), style = "margin-top: 20px"
#          )
#        )
#      ),
	h4("Define Species & IPM Database"),
      wellPanel(
	  fluidRow(
          column(6,
            tags$div(title = "Select a species for analysis",
              select2Input("nmix_critter", "Species", 
                           choices = list("Sage Grouse"),
                           type = "select")
            )
          ),
          column(6,
            tags$div(title = "Select a database to use in the analysis",
              select2Input("nmix_dbname", "Database",
                           choices = list(DbNameLek),
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
              selectizeInput("nmix_dau", "Analysis State", 
                choices = "",
                selected = "Montana",
                options = list(dropdownParent = "body",
                  maxItems = 1))
            )
          ),
          column(7,
            tags$div(title = "Choose years to analyze",
              sliderInput("nmix_year", "Analysis Years", 
                          min = 1980, 
                          max = 1900 + as.POSIXlt(Sys.time())$year + 5,
                          value = c(2002, 1900 + 
                                      as.POSIXlt(Sys.time())$year ), 
                          step = 1,
                          ticks = TRUE,
                          sep = "",
                          width = "100%")
            )
          )
        )
      ),
      
####################################
#      h4("Model Controls"),
#      wellPanel(
#        fluidRow(
#          column(4,
#                 tags$div(title = "Choose year to analyze",
#                          select2Input("nmix_year", "Year",
#                                      choices = "No Data",
#                                      type = "select")
#                 )  
#          ),
#          column(4,
#                 tags$div(title = "Select survey type, abundance or composition", 
#                          select2Input("nmix_surveytype", "Survey Type", 
#                                      choices = "No Data",
#                                      type = "select")
#                 )
#          ),          
#          column(4,
#                 tags$div(title = "Select a model, defined by aircraft", 
#                          select2Input("nmix_aircraft", "Aircraft/Model", 
#                                       choices = "No Data",
#                                       type = "select")
#                 )
#          )
#        )
#      ),
#      h4("Data Check"),
      tags$div(verbatimTextOutput("nmix_vertout")),
      fluidRow(
          tags$div(title = "Click here to run the model",
                   actionButton("nmix_fitgo", "Fit Model", 
                                styleclass = "primary",
                                size = "large",
                                icon = "gears",
                                icon.library = "font awesome"),
                   style = "margin-right:15px;
                            float:right"
          )
    )
    , icon = icon("tasks"), id = "nmix_setup"),  #  tabPanel Setup
    tabPanel("Raw Data",
             hr(),
             tags$div(dataTableOutput("nmix_raw"), 
                      style = "padding-bottom:200px")       
    , icon = icon("folder-open"), id = "nmix_raw"),  #  tabPanel Raw Data
    
    tabPanel("Plots",
      hr(),
      h4("Sightability"),
      plotOutput("nmix_plot_dem", height = "350px"), 
      plotOutput("nmix_plot_total", height = "350px"),       
      tags$div(h6("Black dots represent field observations while blue dots are 
                  the estimated abundances with 95% confidence intervals."),
               style = "padding-bottom:200px")
    ,  icon = icon("bar-chart-o"), id = "nmix_plots"
    ),  #  tabPanel, sight plot output
     
    tabPanel("Table",
      hr(),
      h4("Activity Observations"),
      tags$div(dataTableOutput("nmix_table_cov1"),
               style = "width:200px;
                        margin: 0 auto"),
      hr(),
      h4("Vegetation Observations"),
      tags$div(dataTableOutput("nmix_table_cov2"),
               style = "width:200px;
                        margin: 0 auto"),
      hr(),
      h4("Snow Observations"),
      tags$div(dataTableOutput("nmix_table_cov3"),
               style = "width:200px;
                        margin: 0 auto"),
      hr(),
      h4("Group Size Observations"),
      tags$div(dataTableOutput("nmix_table_cov4"),
               style = "width:200px;
                        margin: 0 auto;
                        padding-right:15px"),
      hr(),
      h4("Calculated Detection Probabilities"),
      tags$div(dataTableOutput("nmix_psummary"),
               style = "width:200px;
                        margin: 0 auto"),
      hr(),
      h4("Sightability Estimates"),
      fluidRow(tags$div(dataTableOutput("nmix_est"), 
                        style = "padding-bottom:200px;
                                 padding-left:15px;
                                 width:500px;
                                 margin: 0 auto"))
      , icon = icon("table"), id = "nmix_table"
    ),  #  tabPanel, sightival tabular output
    tabPanel("Report",
      hr(),
      wellPanel(
        radioButtons("nmix_reportformat", "Download Format", 
                     c("HTML", "Word")),
        downloadButton("nmix_downloadReport", label = "Download")
      ),
      uiOutput("nmix_report"),
      icon = icon("file-o"),
      id = "nmix_report"
    )
  , type= "pills"
  )  #  tabsetPanel
 )  #  tabPanel sightability
)
