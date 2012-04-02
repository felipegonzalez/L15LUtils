tmr <-
function(datos,ind, base=-1){
	tablas_ind<-sapply(datos[,ind],table)
	if(base==-1){
	    base=numeroResp(datos,ind)
	}
	agregada<-margin.table(tablas_ind,1)/base
	#print(NumeroResp(datos,ind))
	round(100*(agregada))
}

