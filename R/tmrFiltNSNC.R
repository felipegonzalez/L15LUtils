tmrFiltNSNC <-
function(datos,ind, base=-1){
    for(k in ind){
        levels(datos[, k])<-c(levels(datos[, k]), "Ns/Nc")
    }
    codigosNSNC<-c("NO SABE","NO CONTESTO","NO CONTESTÓ","NO SABE/NO CONTESTO","NO SABE / NO CONTESTO",
        "Ns/Nc","No sabe/no contesto","No Contestó/no Sabe")
    unico <- apply(datos[, ind], 1, unique)
    unico2<-lapply(unico,function(x){
        x[x %in% codigosNSNC]<- "Ns/Nc"
        x
    })
    vec<-lapply(unico2,function(x){
        if(length(unique(c(x, NA, "NINGUNO"))) == 2){"Ninguno"}
        else{
            if(length(unique(c(x, NA, "Ns/Nc"))) == 2){"Ns/Nc"}
            else{unique(x)[unique(x)!="Ns/Nc" & unique(x)!="NINGUNO"]}
                }} 
    )
    vec2 <- factor(do.call(c, vec), levels = levels(datos[, ind[1]]))
	cruda <- table(vec2)
	round(100*(cruda/numeroResp(datos,ind)))
}

