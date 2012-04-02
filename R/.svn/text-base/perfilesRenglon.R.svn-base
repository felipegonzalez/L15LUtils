perfilesRenglon <-
function(contingencia){
	atr_promedio <- (margin.table(contingencia, margin = 1)/ncol(contingencia))
	perfiles_reng <- (scale(t(contingencia),center = FALSE, 
		scale = atr_promedio))
	#write.table(round(100*perfiles_col), sep = ",")
	t(round(100*perfiles_reng))
}

