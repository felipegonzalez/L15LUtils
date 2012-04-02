tmrNinguna <-
function(datos,ind){
  tablas_ind<-sapply(datos[,ind],table)
  #print(class(tablas_ind))
  #class(tablas_ind)
  #print(tablas_ind)
  #tablas_ind_2 <- tablas_ind
  tablas_ind_2 <- sapply(2:ncol(tablas_ind), function(x){ 
        tab <- tablas_ind[,x]
        tab["NINGUNA"] <- 0
        tab
        #print(tab)
         })
    #print(tablas_ind_2)     
  tablas_ind_2<-cbind(tablas_ind[,1],tablas_ind_2)   
    #print(tablas_ind_2)
  agregada<-margin.table(tablas_ind_2,1)/numeroResp(datos,ind)
  round(100*(agregada))
}

