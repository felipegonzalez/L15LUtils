\name{ordenaNSNC}
\alias{ordenaNSNC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Colocar Respuesta Ns/Nc al Final}
\description{
	Esta función ordena de manera descendente, mueve (en caso de existir) la respuesta ``No Sabe/No Contesto'' (o variantes de ésta) a la ultima posición. Opcionalmente puede recortar la tabla al numero de filas deseado.}
\usage{
ordenaNSNC(tabla, numero = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tabla}{
	Tabla a la cual aplicar el ordenamiento}
  \item{numero}{
	Argumento opcional para recortar la tabla y tomar solo las primeras n filas. El default es 0, es decir, no recortar la tabla}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Una TABLA ordenada.
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
Ver también \code{\link{tmrNSNC}}}
\examples{


## The function is currently defined as
function(tabla,numero=0){
    if(numero==0){
        ordenada<-sort(round(100*prop.table(tabla)),dec=TRUE)
    }
    else{
        ordenada<-sort(round(100*prop.table(tabla)),dec=TRUE)[1:numero]}
    logico<-sapply(1:length(names(ordenada)),function(x){
    	    (sum(names(ordenada[x])==c("NO SABE","NO CONTESTO","NO SABE/NO CONTESTO","NO CONTESTÓ",
    	    "NO SABE / NO CONTESTO","Ns/Nc","No sabe/no contesto","No contestó","No Contestó/No Sabe  ","No Contestó/no Sabe"))!=0)})
    if(sum(logico)!=0){
	CaracterNoSabe<-names(ordenada)[logico]
    niveles<-rownames(ordenada)[rownames(ordenada)!=CaracterNoSabe]
    ordenada<-ordenada[c(niveles,CaracterNoSabe)]}
    ordenada
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Tabla }
\keyword{ Ordenar }% __ONLY ONE__ keyword per line
