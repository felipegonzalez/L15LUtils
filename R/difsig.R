difsig <-
function(base,tamano,nde=2){
	aux<-function(mat,n,reng){
		m<-matrix(rep("",n*reng),ncol=n)
		for(j in 1:n){
			if(j<n){	
				for(k in 1:(n-j)){
					m[ ,j]<-paste(m[ ,j], sapply( mat[[j]][ ,k], function(x){ if(is.na(x)){""}else{if( x == 1 )
						{letters[k+j]} else{""}}}), sep="" )
					}
				}
				if(j>1){
					for(i in 1:(j-1)){
						k = j-i
						m[ ,j]<-paste(m[ ,j], sapply( mat[[k]][ ,i], function(x){ if(is.na(x)){""}else{if(x == -1)
							{letters[k]} else {""}}}), sep="")
						} 
					}
				}
			m
			}

		colum<-ncol(base)
		matriz<-list()
		for(j in 1:(colum-1)){
			indicadora <- list()
			for(i in (j+1):colum){
				indicadora[[i-j]] <- EeDif(base[,j],base[,i],tamano[j],tamano[i],nde)
			}
			matriz[[j]] <- do.call(cbind,indicadora)
		}

		m <- aux(matriz,colum,nrow(base))
		f <- matrix(paste(as.vector(round(base,1)),as.vector(m),sep=""),ncol=colum)
		rownames(f) <- rownames(base)
		colnames(f) <- paste(colnames(base),letters[1:colum])
		f
}

EeDif <-
function(datos1,datos2,tamano1,tamano2,nde){
 var1 <- sapply(datos1, function(j){j/100*(1-j/100)/tamano1})
 var2 <- sapply(datos2, function(j){j/100*(1-j/100)/tamano2})
 ee <- sqrt(var1 + var2)
 sign(datos1 - datos2)*(abs(datos1 - datos2) > 100*nde*ee)
}

