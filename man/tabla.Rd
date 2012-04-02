\name{tabla}
\alias{tabla}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tablas generalizadas.}
\description{
Crea tablas crudas, de porcentajes por fila, por columna,.....}
\usage{
tabla(x = NULL, y, datos, respuesta = "cruda", tipo = "resp.unica", corte = "no")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	Variable}
  \item{y}{
 Una (o posiblemente varias) variables}
  \item{datos}{
Base de datos}
  \item{respuesta}{
Tipo de respuesta, a elegir entre cruda, porcentajes..... }
  \item{tipo}{
Tipo de tabla}
  \item{corte}{
Variable opcional de corte.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
La TABLA calculada.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Michelle Anzarut (parchado y arreglado por David Alvarez muajajaja)}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Ver también \code{\link{tmr}}, ~~~
}
\examples{



## The function is currently defined as
function(x = NULL, y, datos, respuesta = "cruda", tipo = "resp.unica", corte = "no"){
	if (tipo == "resp.unica")
	{
		if (corte == "no")
		{
			if (is.null(x))
			{
				salida <- resp(table(datos[,y]), respuesta)
			}
			else
			{
				tab <- table(datos[,x],datos[,y])
				salida <- resp(tab,respuesta)	
			}
		}
		else
		{
			if (is.null(x))
			{
				salida <- "No tiene sentido x = NULL y corte != no"
			}
			else
			{
				tab <- table(datos[,x],datos[,y],datos[,corte])
				salida <- lapply(1:(dim(tab)[3]),function(k){resp(tab[,,k],respuesta)})
			}
		}
	}
	else
	{
		if (tipo == "mult.resp")
		{
			tab.y <- function(datos,respuesta){
				# datos[,y[1]] <- factor(datos[,y[1]],levels=c(0,1))
				aux <- lapply(y,function(r)
				{
					table(datos[,r])
				}) 
				tab<-Reduce("+",aux)
			if(sum(tab)!=0){
				    if(respuesta=="proporcion"){
				        tab<-round(100*tab/numeroResp(datos,y))}}
			tab
			} 
			datos[,x]<-factor(as.character(datos[,x])) 
			tab <- t(daply(datos,x,tab.y,respuesta))
			salida <- tab
		}
		else
		{
			if (tipo == "varios.atr")
			{
				nombres <- names(table(datos[,y[1]]))
				t1 <- apply(datos[,y], 2, table)
				if (class(t1) == "matrix")
				{
					tab <- t1
				}else{
                    # t2 <- lapply(t1,function(i){i[names(i) \%in\% nombres]})
                    t2<-lapply(t1,function(i){
                        names(i)
                        aux<-rep(0,length(nombres))
                        aux[nombres \%in\% names(i)]<-i
                        names(aux)<-nombres
                        aux})
					tab <- t(do.call(rbind, t2))}
				salida <- resp(tab,respuesta)
			}
			else
			{
				salida <- "Tipo de respuesta no v<U+00E1>lido"
			}
		}
	}
	salida
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
