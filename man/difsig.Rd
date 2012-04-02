\name{difsig}
\alias{difsig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Calcular Diferencias Significativas}
\description{
	Esta funcion calcula diferencias significativas entre columnas para una tabla de proporciones}
\usage{
difsig(base, tamano, nde = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base}{
	Tabla con proporciones}
  \item{tamano}{
	Vector con el numero de respondentes en cada columna}
  \item{nde}{
	Numero entero para definir diferencia significativa (en terminos de desviaciones estandar)
	}
}
\details{
	Esta funcion cicla sobre pares de columnas y llama en cada paso a la funcion EeDif como subrutina para calcular las diferencias significativas para estas.}
\value{
	Tabla con las columnas originales y letras indicando significancias entre columnas
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
	Teresa Ortiz}
	
\note{
	Esta medio chafa porque la hizo Tere cuando todavia no sabia bien R}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{


## The function is currently defined as
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

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
