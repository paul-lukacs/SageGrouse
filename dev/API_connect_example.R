	#  Install required package if necessary
#	install.packages("RCurl")
	#  Load package
	require("RCurl")

	#  Create URL for login page
	loginpg <- "https://fishandgame.idaho.gov/ifwis/accounts/user/login?returnUrl=https%3A%2F%2Ffishandgame.idaho.gov%2Fifwis%2Fportal%2Fwildlife%3Fcachebuster%3D25937"
	
	#  Create URL for direct access to data
	datapg <- "https://fishandgame.idaho.gov/ifwis/rest/services/wildlife/popmodel/view/harvest/4?gmu=12"
	
	#  Set user information
	pars <- list(
		RURL = loginpg,
		username = "Joshnwk",
		password = "tempPopR15!")

	#  Set RCurl pars
	curl <- getCurlHandle()
	curlSetOpt(cookiejar = "", followlocation = T, curl=curl, 
		ssl.verifypeer = F)
	
	#  try POST login form
	html <- postForm(loginpg, .params = pars, curl = curl)
	#  response is Access Denied
	
	#  try GET request from the webpage
	html <- getURL(datapg, curl = curl)
	#  response is the same Access Denied
	
	#  Same as get request above, but using slightly different syntax that is 
	#  easier to read
	x <- getURL(datapg, 
			userpwd = "Joshnwk:QuincyGrange1971",
			maxredirs = as.integer(20),
			followlocation = T,
			cookiefile = "C:/tmp/Rcookies",
			ssl.verifyhost = F,
			ssl.verifypeer = F)	

	#  For documentation on the getURL function type ?getURL
	#  package documentation is at:
	#  http://cran.r-project.org/web/packages/RCurl/index.html
	#  An article detailing the use of the functions can be found at:
	#  http://www.omegahat.org/RCurl/RCurlJSS.pdf
	
	