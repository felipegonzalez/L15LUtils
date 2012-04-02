\name{tmr}
\alias{tmr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Tablas de contingencia para preguntas de respuesta multiple
}
\description{
	Esta funcion toma como entrada una base de datos, indices de preguntas, y un tamano de base (opcional). Realiza cruzes de tablas a traves de sapply, y entrega los valores como porcentajes por columna.
}
\usage{
tmr(datos, ind, base = -1)
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
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	TABLA. La tabla de contingencia, con porcentajes por columna.
}
\references{
	Una referencia interesante es ...}
\author{
	Teresa Ortiz}
\note{
	Esta función era antes conocida como TablasMultResp. Para la versión 1.0 del paquete, dicho nombre de función para ésta y para derivados cambió a tmr.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Ver también \code{\link{tmrNSNC}},\code{\link{tmrRaw}} }
\examples{

data(BaseEjemplo)
preguntas<-c("P12R1","P12R2","P12R3")
tabla<-tmr(BaseEjemplo,preguntas,base=200)

## The function is currently defined as
function(datos,ind, base=-1){
	tablas_ind<-sapply(datos[,ind],table)
	if(base==-1){
	    base=numeroResp(datos,ind)
	}
	agregada<-margin.table(tablas_ind,1)/base
	#print(NumeroResp(datos,ind))
	round(100*(agregada))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
