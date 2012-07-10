
excel.tablas <- function(nombre.archivo='tablas.xlsx', lista.tablas, 
		proyecto='Un ejemplo',nombre.hoja='Tablas (%)'){
	template.file <- system.file('data/template_tablas.xlsx', package = 'L15LUtils')
	wb <- loadWorkbook ( template.file , create = FALSE )
	formato.gris <- getCellStyle(wb,'Renglonbase')
	formato.encabezado <- getCellStyle(wb,'Encabez. 1')
	formato.negrita <- getCellStyle(wb,'formatoPreg')

	createSheet ( wb, name = nombre.hoja )
	estudio <- paste( 'Estudio: ', proyecto, sep='' )
	fecha <- date()
	separador <- ''
	encabezado <- data.frame( nom = c( estudio, fecha, separador ) )

    writeWorksheet(wb, encabezado, sheet = nombre.hoja, startRow = 2, header = FALSE)
    setCellStyle(wb, sheet=nombre.hoja, row = 2, col = 1:10, cellstyle = formato.encabezado)
   
    renglon <- 8
	for(i in 1:length(lista.tablas)){
		tab.out <- lista.tablas[[i]]
		var.otras <- attr(tab.out, 'variables.eje')
		nombre.preg <- attr(tab.out, 'nombre.preg')
		filtro <- attr(tab.out, 'filtro.activo')
		writeWorksheet(wb, data.frame(Pregunta = nombre.preg), sheet = nombre.hoja, 
			startRow = renglon, header = FALSE)
		setCellStyle(wb, sheet = nombre.hoja, row = renglon, col = 1, 
			cellstyle = formato.negrita)
		writeWorksheet(wb, data.frame(Filtro = paste('Filtro:',filtro)), sheet = nombre.hoja, 
			startRow = renglon + 1, header = FALSE)
		renglon <- renglon + 2
		#writeWorksheet(wb, tab.out, sheet=nombre.hoja, startRow=renglon )
		xx <- ddply(tab.out, var.otras, function(df){
			xyz <- writeWorksheet(wb, df, sheet = nombre.hoja, startRow = renglon )
			setCellStyle (wb , sheet = nombre.hoja , 
				row = (renglon+1), col = 1:(ncol(df)+4), cellstyle = formato.gris)
			renglon <<- renglon + nrow(df) + 2
			} )
		renglon <- renglon  + 7
	}
	saveWorkbook(wb, file = nombre.archivo)
}
