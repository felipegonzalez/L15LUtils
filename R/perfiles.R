perfiles <-
function(contingencia, tipo='d'){ #d perfiles dobles, r perfiles renglon, c perfiles columna
	
	if(tipo %in% c('c','d','r'))
	{
	
		if (tipo=='d')
		{
			atr_promedio <- prop.table(margin.table(contingencia, margin = 1))
			tabla_prop <- prop.table(contingencia, margin = 2)
			perfiles <- t(scale(t(tabla_prop), center = FALSE, 
			scale = atr_promedio))
			#write.table(round(100*perfiles_col), sep = ",")
			res <- round(100*perfiles)
		}
  		else
  		{
  		
  				if(tipo=='r')
  				{
  					n <- ncol(contingencia)
					cont.2 <- t(contingencia)
					margen=1
  				}
  				else
  				{
  					n <- nrow(contingencia)
					cont.2 <- contingencia
					margen=2
  				}
  				atr_promedio <- (margin.table(contingencia, margin = margen)/n)
				
  				perfiles <- (scale(cont.2,center = FALSE, 
  					scale = atr_promedio))
  				#write.table(round(100*perfiles_col), sep = ",")
  				res <- round(100*perfiles)
				if(tipo=='r')
				{
					res=t(res)
				}
  		}

	}
	else
	{
		print("ERROR. El tipo debe ser c,d ó r.")
		res="ERROR"
	}
	res
}

