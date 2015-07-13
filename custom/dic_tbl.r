		#  DIC function
		dic_tbl <- function(input, x){
			if(input$fitgo > 0 & !is.null(x$ipm)){
			if(input$fitgo == 1){
				ttbl <- data.frame(
					Run = input$fitgo,
					DAU = input$dau,
					Start_Year = x$ipm$year[1],
					End_Year = x$ipm$year[2],
					Recruitment = x$ipm$recruitMod,
					Juv_Surv = x$ipm$juvSmod,
					Adult_Surv = x$ipm$adultSmod,
					Female_Harv = x$ipm$fharv,
					Male_Harv = x$ipm$mharv,
					DIC = round(x$ipm$BUGS$DIC, 2),
					deltaDIC = 0
					)
			return(ttbl)
			}
			if(input$fitgo > 1){
				ttbl <- as.data.frame(rbind(ttbl, 
					data.frame(
						Run = input$fitgo,
						DAU = input$dau,
						Start_Year = x$ipm$year[1],
						End_Year = x$ipm$year[2],
						Recruitment = x$ipm$recruitMod,
						Juv_Surv = x$ipm$juvSmod,
						Adult_Surv = x$ipm$adultSmod,
						Female_Harv = x$ipm$fharv,
						Male_Harv = x$ipm$mharv,
						DIC = x$ipm$BUGS$DIC,
						deltaDIC = 0
						) 
				))
			return(ttbl)
			}
			
			}		
			
		}