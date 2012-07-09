
tabla.salida <- function(tab.1, var.col, var.reng, nombre.preg){
   ## esto hay que mejorarlo para expresiones regulares
   var.reng <- str_replace_all(var.reng, '[\\$\\^]','')
   ####
   tab.1$prop <- 100*tab.1$prop
   tab.1$sd <- 100*tab.1$sd
   var.otras <- setdiff(names(tab.1), 
      c(var.col, var.reng, 'prop', 'Freq','sd','Base'))

   tab.ag <- ddply(tab.1, c(var.otras,var.reng), summarise, 
   		Freq.total=sum(Freq), .drop=FALSE)
   tab.ag.1 <- ddply(tab.1, c(var.otras, var.col), summarise, 
      Base.total = max(Base), .drop=FALSE)
   tab.ag.2 <- ddply(tab.ag.1, var.otras, summarise, 
      Base.total = sum(Base.total),.drop=FALSE)
   
   tab.ag.3 <- join(tab.ag, tab.ag.2, by=var.otras)
   
   tab.ag.3$Total <- round(100*tab.ag.3$Freq.total/tab.ag.3$Base.total)
   tab.ag.3$sd.Total <- 
      sqrt(tab.ag.3$Total*(100-tab.ag.3$Total)/tab.ag.3$Base.total)
   form.str.1 <- paste( paste(var.otras, var.reng, sep='+'),
      '~', var.col, collapse='')
   form.1 <- as.formula(form.str.1)
   tab.x <- dcast(tab.1, form.1, value.var='prop', drop=FALSE)
   tab.out <- join(tab.x, tab.ag.3[,c(var.otras, var.reng, 'Total')],
       type='left', by = c(var.otras, var.reng))
   # agregar total
   #vars.1 <- setdiff(names(tab.1), c('prop','Freq','sd') )
   #tab.total <- ddply(tab.1, )
   #dcast(tab.base, )
   var.cols <- setdiff(names(tab.out), c(var.otras, var.reng,'Total'))
   tabla.porcentajes <- tab.out[, c(var.otras, var.reng, 'Total', var.cols)]

   ### Agregar bases
   form.str.2 <- paste( var.otras,
      '~', var.col, collapse='')
   form.2 <- as.formula(form.str.2)
   tab.1.base <- unique(tab.1[ , c(var.col,var.otras,'Base')])
   tab.1.tot <- ddply(tab.1.base, var.otras, summarise, 
      Base = sum(Base),.drop=FALSE)
   #tab.1.tot[, var.reng] <- 'Base'
   tab.1.tot[, var.col] <- 'Total'
   tab.2.base <- merge(tab.1.tot, tab.1.base, all = TRUE, sort = FALSE)
   tab.3.base <- dcast(tab.2.base, form.2, value.var='Base', drop=FALSE)
   tab.out.1 <- merge(tabla.porcentajes, tab.3.base, all=TRUE, sort = FALSE)
   tab.out.1[,var.reng] <- as.character(tab.out.1[,var.reng])
   tab.out.1[,var.reng][is.na(tab.out.1[,var.reng])] <- 'Base'
   tab.out.2 <- ddply(tab.out.1, var.otras, function(df){
   		df.sb <- df[df[,var.reng]!='Base' , ]
   		df.b <- df[df[,var.reng]=='Base' , ]
   		rbind(df.b, df.sb)
   	})
   var.tabla.otras <- setdiff(names(tab.out.2), c(var.otras, var.reng))
   tab.out.3 <- 
   	data.frame(tab.out.2[, var.otras],tab.out.2[,var.reng],tab.out.2[,var.tabla.otras])
   names(tab.out.3) <- c(var.otras, var.reng, var.tabla.otras)
   attr(tab.out.3, 'variables.eje') <- var.otras
   attr(tab.out.3, 'nombre.preg') <- nombre.preg
   attr(tab.out.3, 'filtro.activo') <- attr(tab.1, 'filtro.activo')
   attr(tab.out.3, 'var.reng') <- var.reng
   attr(tab.out.3, 'columnas.nom') <- attr(tab.1, 'columnas.nom')
   tab.out.3
}
