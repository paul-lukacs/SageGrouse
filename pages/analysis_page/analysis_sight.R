tabPanel("Sightability",
fixedPage(
  fluidRow(
    headerPanel(list(
      h4(textOutput("sight_sp", inline = T)),
      h6(textOutput("sight_sub", inline = T), style = "color:darkgray")))

  ),
  shinyalert("sight_runalert", click.hide = FALSE, auto.close.after = NULL),
  tabsetPanel(
    tabPanel("Setup",
      hr(),
      h4("Define Species & Space"),
      wellPanel(
        fluidRow(
          column(4,
            tags$div(title = "Select a species for analysis",
              select2Input("sight_critter", "Species", 
                          choices = list("Mule Deer"),
                          type = "select")
            )
          ),
          column(4,
            tags$div(title = "Select a DAU to analyze", 
              select2Input("sight_dau", "Analysis DAU", 
                          choices = "",
                          type = "select")
            )
          ),
          column(4,
                 tags$div(title = "Click to download data from the database", 
                          actionButton("sight_getdata", "Get Data", 
                                       styleclass = "",
                                       size = "large",
                                       icon = "download",
                                       icon.library = "font awesome",
                                       block = T)
                 ), style = "margin-top: 20px"
          )
        )
      ),

      h4("Model Controls"),
      wellPanel(
        fluidRow(
          column(4,
                 tags$div(title = "Choose year to analyze",
                          select2Input("sight_year", "Year",
                                      choices = "No Data",
                                      type = "select")
                 )  
          ),
          column(4,
                 tags$div(title = "Select survey type, abundance or composition", 
                          select2Input("sight_surveytype", "Survey Type", 
                                      choices = "No Data",
                                      type = "select")
                 )
          ),          
          column(4,
                 tags$div(title = "Select a model, defined by aircraft", 
                          select2Input("sight_aircraft", "Aircraft/Model", 
                                       choices = "No Data",
                                       type = "select")
                 )
          )
        )
      ),
      h4("Data Check"),
      tags$div(verbatimTextOutput("sight_vertout")),
      fluidRow(
          tags$div(title = "Click here to run the model",
                   actionButton("sight_fitgo", "Fit Model", 
                                styleclass = "primary",
                                size = "large",
                                icon = "gears",
                                icon.library = "font awesome"),
                   style = "margin-right:15px;
                            float:right"
          )
    )
    , icon = icon("tasks"), id = "sight_setup"),  #  tabPanel Setup
    tabPanel("Raw Data",
             hr(),
             tags$div(dataTableOutput("sight_raw"), 
                      style = "padding-bottom:200px")       
    , icon = icon("folder-open"), id = "sight_raw"),  #  tabPanel Raw Data
    
    tabPanel("Plots",
      hr(),
      h4("Sightability"),
      plotOutput("sight_plot_dem", height = "350px"), 
      plotOutput("sight_plot_total", height = "350px"),       
      tags$div(h6("Black dots represent field observations while blue dots are 
                  the estimated abundances with 95% confidence intervals."),
               style = "padding-bottom:200px")
    ,  icon = icon("bar-chart-o"), id = "sight_plots"
    ),  #  tabPanel, sight plot output
     
    tabPanel("Table",
      hr(),
      h4("Activity Observations"),
      tags$div(dataTableOutput("sight_table_cov1"),
               style = "width:200px;
                        margin: 0 auto"),
      hr(),
      h4("Vegetation Observations"),
      tags$div(dataTableOutput("sight_table_cov2"),
               style = "width:200px;
                        margin: 0 auto"),
      hr(),
      h4("Snow Observations"),
      tags$div(dataTableOutput("sight_table_cov3"),
               style = "width:200px;
                        margin: 0 auto"),
      hr(),
      h4("Group Size Observations"),
      tags$div(dataTableOutput("sight_table_cov4"),
               style = "width:200px;
                        margin: 0 auto;
                        padding-right:15px"),
      hr(),
      h4("Calculated Detection Probabilities"),
      tags$div(dataTableOutput("sight_psummary"),
               style = "width:200px;
                        margin: 0 auto"),
      hr(),
      h4("Sightability Estimates"),
      fluidRow(tags$div(dataTableOutput("sight_est"), 
                        style = "padding-bottom:200px;
                                 padding-left:15px;
                                 width:500px;
                                 margin: 0 auto"))
      , icon = icon("table"), id = "sight_table"
    ),  #  tabPanel, sightival tabular output
    tabPanel("Report",
      hr(),
      wellPanel(
        radioButtons("sight_reportformat", "Download Format", 
                     c("HTML", "Word")),
        downloadButton("sight_downloadReport", label = "Download")
      ),
      uiOutput("sight_report"),
      icon = icon("file-o"),
      id = "sight_report"
    )
  , type= "pills"
  )  #  tabsetPanel
 )  #  tabPanel sightability
)
