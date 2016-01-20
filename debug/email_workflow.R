sender <- "paul.lukacs@gmail.com"  # Replace with a valid address
recipients <- c("paul.lukacs@umontana.edu")  # Replace with one or more valid addresses
email <- send.mail(from = sender,
                   to = recipients,
                   subject="Subject of the email",
                   body = "Body of the email",
                   smtp = list(host.name = "aspmx.l.google.com", port = 25),
                   authenticate = FALSE,
                   send = FALSE)
				   
	
				   
sender <- "paul.lukacs@umontana.edu"  # Replace with a valid address
recipients <- c("paul.lukacs@umontana.edu")  # Replace with one or more valid addresses
recipients <- c("paul.lukacs@gmail.com")  # Replace with one or more valid addresses
email <- send.mail(from = sender,
                   to = recipients,
                   subject="Subject of the email",
                   body = "Body of the email",
                   smtp = list(host.name = "messaging.umt.edu", port = 25, user.name="paul.lukacs", passwd=""),
                   authenticate = TRUE,
                   send = FALSE)
				   
sender <- "PopR.results@cfc.umt.edu"
recipients <- c("paul.lukacs@gmail.com")
email <- send.mail(from = sender,
                   to = recipients,
                   subject="Subject of the email",
                   body = "c:/temp/nmixReport.html",
				   html= TRUE,
                   smtp = list(host.name = "smtp.umt.edu", port = 25),
                   authenticate = FALSE, 
                   send = TRUE)
				   
				   
shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
    ),
    mainPanel(
      tableOutput('contents')
    )
  )
))