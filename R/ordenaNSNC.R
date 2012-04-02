ordenaNSNC <-
function(tabla,numero=0){
    if(numero==0){
        ordenada<-sort(round(100*prop.table(tabla)),dec=TRUE)
    }
    else{
        ordenada<-sort(round(100*prop.table(tabla)),dec=TRUE)[1:numero]}
    logico<-sapply(1:length(names(ordenada)),function(x){
    	    (sum(names(ordenada[x])==c("NO SABE","NO CONTESTO","NO SABE/NO CONTESTO","NO CONTESTÓ",
    	    "NO SABE / NO CONTESTO","Ns/Nc","No sabe/no contesto","No contestó","No Contestó/No Sabe  ","No Contestó/no Sabe"))!=0)})
    if(sum(logico)!=0){
	CaracterNoSabe<-names(ordenada)[logico]
    niveles<-rownames(ordenada)[rownames(ordenada)!=CaracterNoSabe]
    ordenada<-ordenada[c(niveles,CaracterNoSabe)]}
    ordenada
}

