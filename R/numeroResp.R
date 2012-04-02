numeroResp <-
function(datos, ind){
	datos_ind<-as.data.frame(datos[,ind])
	no_respuestas<-apply(datos_ind,1,function(j) {
		sum(!is.na(j))>0
		})
	sum(no_respuestas)
}

