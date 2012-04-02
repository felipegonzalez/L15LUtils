tmrNSNC <-
function(datos,ind, base=-1){
	tablas_ind<-sapply(datos[,ind],table)
	if(base==-1){
	    base=numeroResp(datos,ind)
	}
	agregada<-margin.table(tablas_ind,1)/base
	#print(NumeroResp(datos,ind))
	ordenada<-sort(round(100*(agregada)),dec=TRUE)[1:10]
	logico<-sapply(1:length(names(ordenada)),function(x){
	    (sum(names(ordenada[x])==c("NO SABE","NO CONTESTO","NO SABE/NO CONTESTO",
	    "NO SABE / NO CONTESTO","Ns/Nc","No sabe/no contesto","No contestó","No Contestó/No Sabe  "))!=0)})
	if(sum(logico)!=0){
	CaracterNoSabe<-names(ordenada)[logico]
    niveles<-rownames(ordenada)[rownames(ordenada)!=CaracterNoSabe]
    ordenada<-ordenada[c(niveles,CaracterNoSabe)]}
    ordenada
}

