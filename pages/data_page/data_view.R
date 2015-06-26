tabPanel("View",
         sidebarLayout(
           sidebarPanel(
             bsCollapse(multiple = TRUE, 
                        open = "pd1", 
                        id = "collapseView",
                        bsCollapsePanel(h5("Table Columns"),
                                        uiOutput("showcols"),
                                        id="col1", value="1"),
                        bsCollapsePanel(h5("Plot Controls"),
                                        radioButtons("pdrad", 
                                                     label = h4("Plot Type"),
                                                     choices = c(
                                                       "Histogram",
                                                       "Box Plot",
                                                       "XY Plot"),
                                                     selected = "Histogram"),
                                        bsTypeAhead("ycol", 
                                                    "Y-Column",
                                                    choices = "No Data"),
                                        bsTypeAhead("xcol", 
                                                    "X-Column",
                                                    choices = "No Data"),
                                        h4("Regression"),
                                        p(),
                                        h5("Y ~ B0 + B1*X"),
                                        p(),
                                        bsToggleButton("regress", 
                                                       label = "Add Regression to XY",
                                                       value = "FALSE"),
                                        id="pd1", value = "1")
                        
             )
             
             
           ),  #  sidebarPanel
           mainPanel(
             tabsetPanel(
               tabPanel("Table", dataTableOutput("viewdat")),
               tabPanel("Plot", plotOutput("viewplot"),
                        verbatimTextOutput("regsumm")))
           )
         )  #  sidebarLayout
)  #  tabPanel