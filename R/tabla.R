# hace tabla de contigencia
tabla <-
function(x = NULL, y, datos, respuesta = "cruda", tipo = "resp.unica", corte = "no"){
	if (tipo == "resp.unica")
	{
		if (corte == "no")
		{
			if (is.null(x))
			{
				salida <- resp(table(datos[,y]), respuesta)
			}
			else
			{
				tab <- table(datos[,x],datos[,y])
				salida <- resp(tab,respuesta)	
			}
		}
		else
		{
			if (is.null(x))
			{
				salida <- "No tiene sentido x = NULL y corte != no"
			}
			else
			{
				tab <- table(datos[,x],datos[,y],datos[,corte])
				salida <- lapply(1:(dim(tab)[3]),function(k){resp(tab[,,k],respuesta)})
			}
		}
	}
	else
	{
		if (tipo == "mult.resp")
		{
			tab.y <- function(datos,respuesta){
				# datos[,y[1]] <- factor(datos[,y[1]],levels=c(0,1))
				aux <- lapply(y,function(r)
				{
					table(datos[,r])
				}) 
				tab<-Reduce("+",aux)
			if(sum(tab)!=0){
				    if(respuesta=="proporcion"){
				        tab<-round(100*tab/numeroResp(datos,y))}}
			tab
			} 
			datos[,x]<-factor(as.character(datos[,x])) 
			tab <- t(daply(datos,x,tab.y,respuesta))
			salida <- tab
		}
		else
		{
			if (tipo == "varios.atr")
			{
				nombres <- names(table(datos[,y[1]]))
				t1 <- apply(datos[,y], 2, table)
				if (class(t1) == "matrix")
				{
					tab <- t1
				}
				else
				{
                    # t2 <- lapply(t1,function(i){i[names(i) %in% nombres]})
                    t2<-lapply(t1,function(i){
                        names(i)
                        aux<-rep(0,length(nombres))
                        aux[nombres %in% names(i)]<-i
                        names(aux)<-nombres
                        aux})
                        # i[names(i) %in% nombres]})
					tab <- t(do.call(rbind, t2))
				}
				salida <- resp(tab,respuesta)
			}
			else
			{
				salida <- "Tipo de respuesta no v<U+00E1>lido"
			}
		}
	}
	salida
}


resp <-
function(tabla, respuesta,base=NA){
        switch(respuesta, 
    		cruda = tabla,
    		proporcion.reng = round(100*prop.table(tabla,1)),
    		proporcion.col = round(100*prop.table(tabla,2)),
    		perfiles.reng = perfilesRenglon(tabla),
    		perfiles.col = perfilesColumna(tabla),
    		perfiles = perfiles(tabla),
    		perfiles.aditivos = perfilesAditivos(tabla)
    	)
    }



