tmrUnic<-
function(datos, ind, base = -1, vector.faltantes = NULL){
	vec <- apply(datos[, ind], 1, function(x){
        if(length(unique(c(x, vector.faltantes))) == length(vector.faltantes)){
			vector.faltantes
		} 
        else{
			unique(x)[unique(x) != vector.faltantes]
		}
    })
	vec2 <- factor(do.call(c, vec), levels = levels(datos[, ind[1]]))
	cruda <- table(vec2)
	round(100*(cruda/numeroResp(datos,ind)))
}

