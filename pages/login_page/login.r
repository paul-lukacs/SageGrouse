	#### Log in module ###
	USER <- reactiveValues(Logged = Logged)
	  	passwdInput <- function(inputId, label) {
    	  tagList(
    		  tags$label(label),
    		  br(),
    		  tags$input(id = inputId, type="password", value="")
  	  )
	}
	 
	output$uiLogin <- renderUI({
# edit out after debugging	
		USER$Logged <- TRUE
##############
	  if(USER$Logged == FALSE){
		wellPanel(
		  textInput("userName", "User Name:"),
		  passwdInput("passwd", "Password:"),
		  br(),
		  actionButton("Login", "Login"),
		  textOutput("pass")
		)
	  }
	})

	output$pass <- renderText({  
	  if (USER$Logged == FALSE) {
		if (!is.null(input$Login)) {
	   if (input$Login > 0) {
		  Username <- isolate(input$userName)
		  Password <- isolate(input$passwd)
		  Id.username <- which(PASSWORD$Brukernavn == Username)
		  Id.password <- which(PASSWORD$Passord    == Password)
		  if (length(Id.username) > 0 & length(Id.password) > 0) {
			if (Id.username == Id.password) {
			  USER$Logged <- TRUE
			  "You are logged in to PopR!"
			} 
		  } else  {
			  "User name or password failed!"
		  }
		} 
		}
	  }
	})