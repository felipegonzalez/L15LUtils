\name{tmrUnic}
\alias{tmrUnic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tablas de contingencia para preguntas de respuesta multiple, tomando Unique}

\description{
	Misma funcion que \code{\link{tmr}}, pero toma solo una respuesta por fila, utilizando la función \emph{unique} por fila (individuo).}
\usage{
tmrUnic(datos, ind, base = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{
	Base de datos que contiene las preguntas sobre las cual realizar la tabla.}

  \item{ind}{
	Códigos de las columnas de la base correspondientes a la tabla de interés.}

  \item{base}{
	Argumento opcional que indica tamano de la base. Defaults to -1, lo cual indica que el tamaño se calculara dentro del método.}
	
  \item{vector.faltantes}{
	Vector de caracteres con observaciones que no se quieren contar.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
TABLA. La tabla de contingenciasc con porcentajes.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Alguno de los siguientes: Felipe Gonzalez, David Alvarez o Teresa Ortiz}
\note{
OBSERVACION importante: Esta funcion parece hacer lo mismo que \code{\link{tmrRep}}, queda pendiente investigar si son iguales y en ese caso borrar alguna de las dos.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{tmr}},\code{\link{tmrNSNC}},\code{\link{tmrRaw}},\code{\link{tmrNinguna}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
tmrUnic<-
function(datos, ind, base = -1, vector.faltantes = NULL){
	vec <- apply(datos[, ind], 1, function(x){
        if(length(unique(c(x, vector.faltantes))) == length(vector.faltantes)){
			vector.faltantes
		} 
        else{
			unique(x)[unique(x) != vector.faltantes]
		}
    })
	vec2 <- factor(do.call(c, vec), levels = levels(datos[, ind[1]]))
	cruda <- table(vec2)
	round(100*(cruda/numeroResp(datos,ind)))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ respuesta }
\keyword{ multiple }% __ONLY ONE__ keyword per line
