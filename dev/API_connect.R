	input <- list()
	input$userid <- "Joshnwk"
	input$password <- "QuincyGrange1971"
	input$datatype <- "harvest"
	input$critternum <- 4
	input$gmu <- 12
	
	#  Create URL for login page
	loginpg <- "https://fishandgame.idaho.gov/ifwis/accounts/user/login?returnUrl=https%3A%2F%2Ffishandgame.idaho.gov%2Fifwis%2Fportal%2Fwildlife%3Fcachebuster%3D25937"
	
	xx <- POST(loginpg, authenticate = c("Joshnwk", "QuincyGrange1971"))
	
	#  Create URL
	datapg <- paste("https://fishandgame.idaho.gov/ifwis/rest/services/wildlife/popmodel/view/",
		input$datatype, "/", input$critternum, "?gmu=", input$gmu,
		sep = "")
	
	#  Set user information
	pars <- list(
		RURL = loginpg,
		username = "Joshnwk",
		password = "QuincyGrange1971")
	agent <- "Mozilla/5.0"	
	#  Set RCurl pars
	curl <- getCurlHandle()
	curlSetOpt(cookiejar = "", useragent = agent, followlocation = T, curl=curl, 
		ssl.verifypeer = F)
	
	#  POST login form
	html <- postForm(loginpg, .params = pars, curl = curl)
	
	#  Go to data
	html <- getURL(datapg, curl = curl)
		
	#  Connect to website
	x <- getURL(site, 
			userpwd = "Joshnwk:QuincyGrange1971",
			maxredirs = as.integer(20),
			followlocation = T,
			cookiefile = "C:/tmp/Rcookies",
			ssl.verifyhost = F,
			ssl.verifypeer = F)			
	
	