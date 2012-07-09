tablas.banner <- function(.data, var.otras, vars.col, .patron, nombre.preg,filtro.var=NULL,
	filtro.vals=NULL){
	salida.list <- list()
	out.list <- list()
	for(i in 1:length(vars.col)){
		salida.list[[i]] <- ddply.mult(datos, c(var.otras,vars.col[i]), .patron = .patron,
			filtro.var=filtro.var, filtro.vals=filtro.vals)
		out.list[[i]] <- tabla.salida(salida.list[[i]],
				vars.col[[i]], .patron, nombre.preg=nombre.preg)
	}
	#join.x <- function(x){
	#	join(x, by=c(var.otras, attr(out.list[[1]],'var.reng')))
	#}
	salida <- Reduce(join, out.list)
	if(!is.null(filtro.var)){
		attr(salida, 'filtro.activo') <- paste(filtro.var, '=', toString(filtro.vals))
	} else{
		attr(salida, 'filtro.activo') <- 'Total'
	}
	attr(salida, 'variables.eje') <- var.otras
    attr(salida, 'nombre.preg') <- nombre.preg
	salida

}

