\name{tmrNinguna}
\alias{tmrNinguna}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tablas de contingencia para preguntas de respuesta multiple, quitando ``ninguna''}
\description{
Similar  a \code{\link{tmr}}, pero elimina la respuesta ``ninguna''}
\usage{
tmrNinguna(datos, ind)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{
	Base de datos que contiene las preguntas sobre las cual realizar la tabla}

  \item{ind}{
	Codigos de las columnas de la base correspondientes a la tabla de interes}

}
\details{
}
\value{
	TABLA. La tabla de porcentajes, sin renglones con la respuesta ``ninguna''.
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
Ver también \code{\link{tmr}},\code{\link{tmrNSNC}},\code{\link{tmrRaw}} }


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(datos,ind){
  tablas_ind<-sapply(datos[,ind],table)
  #print(class(tablas_ind))
  #class(tablas_ind)
  #print(tablas_ind)
  #tablas_ind_2 <- tablas_ind
  tablas_ind_2 <- sapply(2:ncol(tablas_ind), function(x){ 
        tab <- tablas_ind[,x]
        tab["NINGUNA"] <- 0
        tab
        #print(tab)
         })
    #print(tablas_ind_2)     
  tablas_ind_2<-cbind(tablas_ind[,1],tablas_ind_2)   
    #print(tablas_ind_2)
  agregada<-margin.table(tablas_ind_2,1)/numeroResp(datos,ind)
  round(100*(agregada))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Tablas }
\keyword{ Respuesta }% __ONLY ONE__ keyword per line
