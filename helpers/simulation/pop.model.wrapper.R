pop.model.wrapper <- function(md = modeldata(), b = b, method = method){
#
# pop.model.wrapper controls the flow of data process and model fitting
# 
	
	deerDataIn <- as.popData( input ) # instantiate population model data object

	deerDataIn <- coerceImpPopData( md, deerDataIn )   # fill data in popData object
	
	M <- input$mcmc
	
	modelname <- paste( b[[2]], b[[3]], sep="" ) # concatenate model name from components
	
	rec.model <- b[[4]] 
	
	deerDataIn$fBH[ (deerDataIn$nYears-2):deerDataIn$nYears ] <- rep( b[[5]], 3 ) 
	deerDataIn$fDH[ (deerDataIn$nYears-2):deerDataIn$nYears ] <- rep( b[[6]], 3 ) 
	
	data.check <- list(b[[1]], deerDataIn, modelname, rec.model)
	save(data.check, file = "C:/IDFG/data.check.RData")
	
#	cat( "b = ", b, "\n" )
	# run the model fitting
	model.res <- switch( method,
					bayes = mcmc(b, deerDataIn, model = modelname, rec.model = rec.model, b=0, M = M, adaptive = TRUE ),
					ml = optim( b, popModLikelihood, control=list(fnscale=-1),
								hessian = TRUE, data=deerDataIn, model=modelname, rec.model=rec.model )
						)
	
#	queryTxt <- paste( "SELECT * FROM tblObjectiveData WHERE species='", input$spp, "' AND DAU=", input$dau, sep="" )
#	obj.data <- sqlQuery( modelCon, queryTxt )
#	if( input$method == "bayes" ){
#		dirFile <- popModelReport( model.res, deerDataIn, choice, M, modelCon, upload=FALSE, samplesize=NULL )
#		plots are now called from popModelReport
#		mcmc.plots( model.res, deerDataIn, DAUplan=c(obj.data$PopObjMin, obj.data$PopObjective, obj.data$PopObjMax), input, dirFile )
#	}
#	odbcClose(modelCon)
	return( model.res )
}