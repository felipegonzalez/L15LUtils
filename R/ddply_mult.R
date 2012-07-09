
ddply.mult <- function(.data, .variables, .grupo = NULL, .patron = NULL, 
      .progress = 'none', .drop = FALSE, filtro.var = NULL, filtro.vals = NULL){     
   
   
   if(is.null(.grupo)){
      if(is.null(.patron)){
         stop('Ningun grupo de respuesta multiple. Especificar .grupo o .patron')
      } 
      else{
         .grupo <- names(.data)[grep(.patron, names(.data), perl=TRUE)]
         if(length(.grupo)==0){
            stop('Ninguna coincidencia con patron.')
         }
      }
   }

   	if(!is.null(filtro.var)){
		filtro.1 <- .data[ , filtro.var] %in% filtro.vals
		.data <- .data[filtro.1, ]
	}

	datos <- .data[ , c(.variables, .grupo)]
	datos$id.tmp <- 1:nrow(datos)
	datos.m <- melt(datos, id.vars = c('id.tmp', .variables), .drop = .drop)
	
	out <- ddply(datos.m, .variables, function(df){
		### Cambiar esta línea para hacer el cálculo ponderado
		data.frame(table(df$value))
		# falta modificar para mostrar faltantes.
		#data.frame(table(df$value, useNA = 'always'))
		}, .drop = .drop)
	

	out.1 <- ddply(.data, .variables, nrow,.drop = .drop)
	names(out.1)[length(out.1)] <- 'Base'
	out.2 <- join(out, out.1, by = .variables, type = 'left')
	out.2$prop <- round(out.2$Freq/out.2$Base,2)
	out.2$sd <- sqrt(out.2$prop*(1-out.2$prop)/out.2$Base)
	# como lidiar con valores faltantes?
	#out.2$Var1 <- as.character(out.2$Var1)
	#out.2$Var1[is.na(out.2$Var1)] <- 'No disponible'
	if(!is.null(.patron)){
		### mejorar esto para expresiones regulares
		var.reng.1 <- str_replace_all(.patron, '[\\$\\^]','')
	   names(out.2)[names(out.2)=='Var1'] <- var.reng.1
	   #######
 	}
	if(!is.null(filtro.var)){
		attr(out.2, 'filtro.activo') <- paste(filtro.var, '=', toString(filtro.vals))
	} else{
		attr(out.2, 'filtro.activo') <- 'Total'
	}
	attr(out.2, 'columnas.nom') <- .grupo
	out.2
}
