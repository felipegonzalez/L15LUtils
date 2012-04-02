\name{tmrRaw}
\alias{tmrRaw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tablas de contingencia para preguntas de respuesta multiple, CRUDAS}
\description{
Similar  a \code{\link{tmr}}, pero en vez de calcular porcentajes, entrega valores crudos }
\usage{
tmrRaw(datos, ind, base = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{
	Base de datos que contiene las preguntas sobre las cual realizar la tabla}
  \item{ind}{
	Codigos de las columnas de la base correspondientes a la tabla de interes}
  \item{base}{
	Argumento opcional que indica tamano de la base. Defaults to -1}
}
\details{
}
\value{
	TABLA. La tabla de contingencia, en formato crudo.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Teresa Ortiz}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Ver también \code{\link{tmr}},\code{\link{tmrNSNC}},\code{\link{tmrNinguna}} }

\examples{

data(BaseEjemplo)
preguntas<-c("P12R1","P12R2","P12R3")
tabla<-tmrRaw(BaseEjemplo,preguntas,base=200)

## The function is currently defined as
function(datos,ind,base=1){
	tablas_ind<-sapply(datos[,ind],table)
	agregada<-margin.table(tablas_ind,1)
	agregada
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Tablas }
\keyword{ Respuestas }% __ONLY ONE__ keyword per line
