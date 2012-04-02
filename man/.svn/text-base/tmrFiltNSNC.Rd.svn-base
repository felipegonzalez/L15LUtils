\name{tmrFiltNSNC}
\alias{tmrFiltNSNC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tablas de contingencia para preguntas de respuesta multiple, filtrando NS/NC}

\description{
	Misma funcion que \code{\link{tmr}}, pero filtra ``No Sabe/ No Contestó'' (o variantes de ésta).}
\usage{
tmrFiltNSNC(datos, ind, base = -1)
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
Las siguientes respuestas se toma como sinónimos de ``No Sabe'':
% \begin{itemize}
% \item{"NO SABE"}
% \item{"NO CONTESTO"}
% \item{"NO CONTESTÓ"}
% \item{"NO SABE/NO CONTESTO"}
% \item{"NO SABE / NO CONTESTO"}
% \item{"Ns/Nc"}
% \item{"No sabe/no contesto"}
% \item{"No Contestó/no Sabe"}
% \end{itemize}

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
Esta funcion es similar a \code{\link{tmrNSNC}}, pero a diferencia de aquella, esta filtra NSNC en vez de mandarlo al final. Además, no hace sort ni escoge los más altos, como sí hace aquella.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Ver también \code{\link{tmrNSNC}} además de \code{\link{tmr}},\code{\link{tmrNinguna}},\code{\link{tmrRaw}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(datos,ind, base=-1){
    for(k in ind){
        levels(datos[, k])<-c(levels(datos[, k]), "Ns/Nc")
    }
    codigosNSNC<-c("NO SABE","NO CONTESTO","NO CONTESTÓ","NO SABE/NO CONTESTO","NO SABE / NO CONTESTO",
        "Ns/Nc","No sabe/no contesto","No Contestó/no Sabe")
    unico <- apply(datos[, ind], 1, unique)
    unico2<-lapply(unico,function(x){
        x[x \%in\% codigosNSNC]<- "Ns/Nc"
        x
    })
    vec<-lapply(unico2,function(x){
        if(length(unique(c(x, NA, "NINGUNO"))) == 2){"Ninguno"}
        else{
            if(length(unique(c(x, NA, "Ns/Nc"))) == 2){"Ns/Nc"}
            else{unique(x)[unique(x)!="Ns/Nc" & unique(x)!="NINGUNO"]}
                }} 
    )
    vec2 <- factor(do.call(c, vec), levels = levels(datos[, ind[1]]))
	cruda <- table(vec2)
	round(100*(cruda/numeroResp(datos,ind)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multiple }
\keyword{ respuesta }% __ONLY ONE__ keyword per line
