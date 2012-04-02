perfilesColumna <-
function(contingencia){
	atr_promedio <- (margin.table(contingencia, margin = 2)/nrow(contingencia))
	perfiles_col <- (scale(contingencia,center = FALSE, 
		scale = atr_promedio))
	#write.table(round(100*perfiles_col), sep = ",")
	round(100*perfiles_col)
}

