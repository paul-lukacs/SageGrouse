tabPanel("Simulate",      
         sidebarLayout(
           sidebarPanel(
             h3("Simulate Data"),
             bsCollapse(multiple = T, open = 0, id = "simcollapse",
                        bsCollapsePanel(
                          h4("Controls"),
                          sliderInput("nyr", 
                                      label = h5("Years to Simulate:"), 
                                      min = 1, 
                                      max = 20,
                                      value = 5, 
                                      step = 1, 
                                      round = T),
                          
                          sliderInput("niters", label = h5("Iterations:"), 
                                      min = 0, 
                                      max = 10000, 
                                      value = 1, 
                                      step = 1000, 
                                      round = T)
                          
                        ),  #  bsCollapsePanel
                        
                        bsCollapsePanel(
                          h4("Initial Population", style = "data-icon:gears;
                             color:gray"),
                          sliderInput("muN1", 
                                      label = h5("Initial Fawn Abundance:"), 
                                      min = 0, 
                                      max = 10000, 
                                      value = 700, 
                                      step = 50),
                          sliderInput("sdN1", 
                                      label = "Observation Error:", 
                                      min = 0, 
                                      max = 100, 
                                      value = 0, 
                                      step = 5),          
                          sliderInput("muN2", 
                                      label = h5("Initial Doe Abundance:"), 
                                      min = 0, 
                                      max = 10000, 
                                      value = 800, 
                                      step = 50),
                          sliderInput("sdN2", 
                                      label = "Observation Error:", 
                                      min = 0, 
                                      max = 100, 
                                      value = 0, 
                                      step = 5),    
                          sliderInput("muN3", 
                                      label = h5("Initial Buck Abundance:"), 
                                      min = 0, 
                                      max = 10000, 
                                      value = 500, 
                                      step = 50),
                          sliderInput("sdN3", 
                                      label = "Observation Error:", 
                                      min = 0, 
                                      max = 100, 
                                      value = 0, 
                                      step = 5)
                          ),  #  CollapsePanel
                        
                        bsCollapsePanel(
                          h4("Demographic Rates"),
                          sliderInput("mupreg", 
                                      label = h5("Pregnancy Rate:"), 
                                      min = 0, 
                                      max = 1, 
                                      value = 0.88, 
                                      step = 0.01),       
                          sliderInput("sdpreg", 
                                      label = "Temporal Variability:", 
                                      min = 0, 
                                      max = 1, 
                                      value = 0, 
                                      step = 0.01),
                          sliderInput("phif", 
                                      label = h5("Fawn Survival:"), 
                                      min = 0, 
                                      max = 1, 
                                      value = 0.6, 
                                      step = 0.01),
                          sliderInput("sdphif", 
                                      label = "Temporal Variability:", 
                                      min = 0, 
                                      max = 1, 
                                      value = 0, 
                                      step = 0.01),
                          sliderInput("phiadf", 
                                      label = h5("Doe Survival:"), 
                                      min = 0, 
                                      max = 1, 
                                      value = 0.75, 
                                      step = 0.01),
                          sliderInput("sdphiadf", 
                                      label = "Temporal Variability:", 
                                      min = 0, 
                                      max = 1, 
                                      value = 0, 
                                      step = 0.01),
                          sliderInput("phiadm", 
                                      label = h5("Buck Survival:"), 
                                      min = 0, 
                                      max = 1, 
                                      value = 0.55, 
                                      step = 0.01),
                          sliderInput("sdphiadm", 
                                      label = "Temporal Variability:", 
                                      min = 0, 
                                      max = 1, 
                                      value = 0, 
                                      step = 0.01)
                        )  #  bsCollapsePanel
                        
           ),  #  bsCollapse
           
           p(actionButton("simgo", "Run Simulation", icon("gears")))
           
           ),  #  sidebarPanel
           
           mainPanel(
             tabsetPanel(
               tabPanel("Simulation Inputs", 
                        plotOutput("siminplot", height = 600)),
               tabPanel("Simulation Outputs", 
                        plotOutput("simoutplot", height = 600),
                        verbatimTextOutput("regsumm2"),
                        dataTableOutput("simtable"))
             )  #  tabsetPanel
           )  #  mainPanel
         )  #  sidebarLayout
)  #  tabPanel