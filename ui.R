#  GUI layout
#  Josh Nowak
#  October, 2014
#  modified by Paul Lukacs 06/2015
source("custom/db_connections.R")
source("custom/dau_lookup.R")
source("misc/packages.R")

#
shinyUI(
  navbarPage(
    tags$head(
      tags$style(HTML(".shiny-progress .progress{ height:20px }")),
      tags$link(rel="stylesheet", type="text/css", href="style.css"),
      tags$script(type="text/javascript", src = "md5.js"),
      tags$script(type="text/javascript", src = "passwdInputBinding.js")
    ),
    
    #  Home Page         
    tabPanel("Home", 
      #mainPanel(
        source("pages/home_page/home.R", local = T)$value
      #, width = 10)
    ),  #  tabPanel
    
    #  Analysis Page
    source("pages/analysis_page/analysis_ipm.R", local = T)$value,
    source("pages/analysis_page/analysis_sight.R", local = T)$value,
    source("pages/analysis_page/analysis_surv.R", local = T)$value,

    tabPanel("About",
        source("pages/contact_page/contact.R", local = T)$value    
    )  #  tabPanel
    
  , id = "popr_page", windowTitle = "PopR", collapsible = T
  )  # NavbarPage
)  #  ShinyUI
