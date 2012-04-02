tmrRaw <-
function(datos,ind,base=1){
	tablas_ind<-sapply(datos[,ind],table)
	agregada<-margin.table(tablas_ind,1)
	agregada
}

