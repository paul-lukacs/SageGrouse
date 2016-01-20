    #  Client side home page script
    #  Josh Nowak
    #  01/2015
	# Modified by Paul Lukacs for sage grouse model
	# 06/2015
################################################################################
    fixedPage(
      headerPanel(a(a(href = "http://www.wafwa.org/", 
                    img(src = "logo.png", 
                        style = "height:165px;
                                 width:104px")),
                    a(href = "http://www.cfc.umt.edu/research/lukacslab/",
                    img(src = "umt.png", 
                        style = "height:165px;
                                 width:351px;
                                 float:right"))), 
                  "PopR"),
      hr(style = "background:#6A7332;
         border:0;
         height:2px;
         width:100%"),
      
      h2("Welcome to PopR!", style = "color:#6A7332;
                                      text-align:center"),
      br(),      
      includeText("www/home.txt"),
      br(),
      div(class = "login", uiOutput("uiLogin")),
      hr(style = "background:#6A7332;
         border:0;
         height:2px;
         width:100%"),
      HTML('<footer>
            <p>PopR Version 1.0</br>
            Maintained by Josh Nowak & Paul Lukacs</br>
            Wildlife Biology Program</br>
            University of Montana</p>
            </footer>'),

      width = 10
      
      )     