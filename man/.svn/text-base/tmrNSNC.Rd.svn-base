\name{tmrNSNC}
\alias{tmrNSNC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tablas de contingencia para preguntas de respuesta multiple, reacomodando NS/NC}
\description{
	Misma funcion que \code{\link{tmr}}, pero ordenado descendentemente, tomando sólo 10 filas y colocando ``No Sabe/ No Contestó'' (o variantes de ésta) en la última posición.}
\usage{
tmrNSNC(datos, ind, base = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{
	Base de datos que contiene las preguntas sobre las cual realizar la tabla.}

  \item{ind}{
	Códigos de las columnas de la base correspondientes a la tabla de interés.}

  \item{base}{
	Argumento opcional que indica tamano de la base. Defaults to -1, lo cual indica que el tamaño se calculara dentro del método.}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
TABLA. La tabla de contingencias con porcentajes.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
David Alvarez}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Ver también \code{\link{tmr}}, \code{\link{ordenaNSNC}},\code{\link{tmrFiltNSNC}} }
\examples{


## The function is currently defined as
function(datos,ind, base=-1){
	tablas_ind<-sapply(datos[,ind],table)
	if(base==-1){
	    base=numeroResp(datos,ind)
	}
	agregada<-margin.table(tablas_ind,1)/base
	#print(NumeroResp(datos,ind))
	ordenada<-sort(round(100*(agregada)),dec=TRUE)[1:10]
	logico<-sapply(1:length(names(ordenada)),function(x){
	    (sum(names(ordenada[x])==c("NO SABE","NO CONTESTO","NO SABE/NO CONTESTO",
	    "NO SABE / NO CONTESTO","Ns/Nc","No sabe/no contesto","No contestó","No Contestó/No Sabe  "))!=0)})
	if(sum(logico)!=0){
	CaracterNoSabe<-names(ordenada)[logico]
    niveles<-rownames(ordenada)[rownames(ordenada)!=CaracterNoSabe]
    ordenada<-ordenada[c(niveles,CaracterNoSabe)]}
    ordenada
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ respuesta }
\keyword{ multiple }% __ONLY ONE__ keyword per line
