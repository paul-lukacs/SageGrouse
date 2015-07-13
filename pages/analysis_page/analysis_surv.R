tabPanel("Survival",
  fixedPage(
  fluidRow(
    headerPanel(list(
      h4(textOutput("surv_sp", inline = T)),
      h6(textOutput("surv_sub", inline = T), style = "color:darkgray")))
  ),
  shinyalert("surv_runalert", click.hide = FALSE, auto.close.after = NULL),
  tabsetPanel(
    tabPanel("Setup",
      hr(),
      h4("Define Species & Space"),
      wellPanel(
        fluidRow(
          column(4,
            tags$div(title = "Select a species for analysis",
              select2Input("surv_critter", "Species", 
                           choices = list("Mule Deer"),
                        type = "select")
            )
          ),
          column(4,
            tags$div(title = "Select a DAU to analyze", 
              select2Input("surv_dau", "Analysis DAU", 
                          choices = "",
                          type = "select")
            )
          ),
          column(4,
            tags$div(title = "Click to download data from the database", 
              actionButton("surv_getdata", "Get Data", 
                            styleclass = "",
                            size = "large",
                            icon = "download",
                            icon.library = "font awesome",
                           block = T)
            ), style = "margin-top: 20px"
          )
        )  #  fluidRow
      ),  #  WellPanel
    
      h4("Model Setup"),
      wellPanel(
        fluidRow(
          column(4,
            tags$div(title = "Choose year to analyze",
              select2Input("surv_year", "Year",
                          choices = "No Data",
                          type = "select")
            )  
          ),
          column(4,
            tags$div(title = "Select animal age", 
              select2Input("surv_age", "Age", 
                          choices = "No Data",
                          type = "select")
            )
          ),          
          column(4,
            tags$div(title = "Choice determines if and how the data is subset seasonally", 
              select2Input("surv_season", "Season", 
                          choices = list("Annual",
                                         "Summer",
                                         "Fall",
                                         "Winter"),
                          type = "select")
            )
          )
        )  #  fluidRow
      ),  #  wellPanel
      h4("Exclude"),
      wellPanel(
        fluidRow(
          column(4,
            tags$div(title = "Choose whether to exclude certain individuals from the analysis according to censor type",
              checkboxGroupInput("surv_exclude_cens", "Censor Type",
                                 choices = list("One Week Mortality",
                                                "Not Retrieved",
                                                "Failed Collar",
                                                "Shed",
                                                "Unknown"))
            )
          ),
          column(4,
            tags$div(title = "Choose whether to exclude certain individuals from the analysis according to mortality type",
                checkboxGroupInput("surv_exclude_mort", "Mortality Type",
                                   choices = list("Harvested",
                                                  "Coyote Predation",
                                                  "Lion Predation",
                                                  "Wolf Predation",
                                                  "Unknown Predation",
                                                  "Unknown Nonpredation",
                                                  "Unknown"))
            ) 
          ),
          column(4,
            tags$div(title = "Choose whether to exclude individuals of the selected sex from analysis",
              checkboxGroupInput("surv_exclude_sex", "Sex",
                                 choices = list("Male", "Female"))
            ) 
          )
        )
      ),      
      h4("MCMC Controls"),
      wellPanel(
        fluidRow(
          column(4,
            tags$div(title = "Change at will if you know what you are doing",
              sliderInput("surv_burnin", "Burnin Length", 
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
              sliderInput("surv_niter", "MCMC Iterations", 
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
              sliderInput("surv_thin", "Thinning Rate", 
                          min = 1, 
                          max = 100, 
                          value = 2,
                          step = 1, 
                          round = T,
                          width = "100%")
            )
          )
        )  #  fluidRow
      ),  #  wellPanel
      fluidRow(
          tags$div(title = "Click here to run the model",
            actionButton("surv_fitgo", "Fit Model", 
                         styleclass = "primary",
                         size = "large",
                         icon = "gears",
                         icon.library = "font awesome"),
            style = "padding-bottom:200px;
                     padding-right:15px;
                     float:right"
          )
      )  #  fluidRow
   , icon = icon("tasks"), id = "surv_setup"),  #  tabPanel Setup
   tabPanel("Raw Data",
    hr(),
    tags$div(dataTableOutput("surv_raw"),
             style = "padding-bottom: 200px"
    )
   , icon = icon("folder-open"), id = "surv_raw"),  #  tabPanel Raw Data
   
   #####################  Plots
   tabPanel("Plots",
    hr(),
    h4("Sample Size Summary"),
    fluidRow(
      column(4,
        tags$div(title = "Number of animals marked in the survival dataset.",
                 htmlOutput("surv_ss_gauge"),
                 style = "width: 200px;
                          margin: auto")
      ),  
      column(4,
        tags$div(title = "Number of animals that died during the study period.",
                 htmlOutput("surv_dead_gauge"),
                 style = "width: 200px;
                          margin: auto")
      ), 
      column(4,
        tags$div(title = "Number of animals that were censored during the study 
                 period.",
                 htmlOutput("surv_cens_gauge"),
                 style = "width: 200px;
                          margin: auto")
      ) 
    ),

    fluidRow(
      column(6,
             tags$div(h4("Causes of Censoring"), style = "padding-left:15px"),
             tags$div(htmlOutput("surv_prop_cens"),
                      style = "width:250px;
                                margin: 0 auto")
      ),
      column(6, 
             h4("Causes of Mortality"),
              tags$div(htmlOutput("surv_prop_mort"),
                       style = "width:250px;
                                margin: 0 auto") 
      )
    ),
    hr(),
    h4("Variation in Monthly Survival"), 
    plotOutput("surv_plot_ts", height = "350px"),
    h4("Mean Monthly Survival"),
    plotOutput("surv_plot_mm", height = "350px"),
    textOutput("surv_mm_txt"),
    h4("Study Period Survival"),
    plotOutput("surv_plot_sp", height = "350px"),
    tags$div(textOutput("surv_sp_txt"), style = "padding-bottom:200px")
   , icon = icon("bar-chart-o"), id = "surv_plots"
   ),  #  tabPanel, surv plot output
   
   #####################  Table
   tabPanel("Table",
   hr(),
   h4("Sample Size Summary"),
   fluidRow(
    column(4,
           tags$div(title = "Number of animals marked in the survival dataset.",
                    htmlOutput("surv_sst_gauge"),
                    style = "width: 200px;
                    margin: auto")
           ),  
    column(4,
           tags$div(title = "Number of animals that died during the study period.",
                    htmlOutput("surv_deadt_gauge"),
                    style = "width: 200px;
                    margin: auto")
           ), 
    column(4,
           tags$div(title = "Number of animals that were censored during the study period.",
                    htmlOutput("surv_censt_gauge"),
                    style = "width: 200px;
                    margin: auto")
           ) 
    ),
    hr(),
    h4("Survival"),
    tags$div(dataTableOutput("surv_table"),
             style = "padding-bottom: 200px"
    )      
   , icon = icon("table"), id = "surv_table"
   ),  #  tabPanel, survival tabular output
   
   #####################  Report
   tabPanel("Report",
     hr(),
     wellPanel(
      radioButtons("surv_reportformat", "Download Format", 
                   c("HTML", "Word")),
      downloadButton("surv_downloadReport", label = "Download")
     ),
     tags$div(uiOutput("surv_report"), style = "padding-bottom: 200px")
   , icon = icon("file-o"), id = "surv_report"
   )
   , type= "pills"
  )  #  tabsetPanel
  )
)  #  tabPanel Survival
