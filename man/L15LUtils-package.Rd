\name{L15LUtils-package}
\alias{L15LUtils-package}
\alias{L15LUtils}
\docType{package}
\title{
Funciones auxiliares para el analisis de datos
}
\description{
Compendio de funciones y metodos surgidos durante el analisis de datos dentro de LasQuinceLetras, utiles para lidiar con bases de datos derivadas de encuestas, producir analisis y crear modelos.

}
\details{
\tabular{ll}{
Package: \tab L15LUtils\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-05-21\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
% ~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Autor: LasQuinceLetras Staff

Maintainer: Felipe Gonzalez, Teresa Ortiz, David Alvarez, Michelle Anzarut


Quejas a  <felipexgonzalez@lasquinceletras.com.mx>,<teresa.ortiz@lasquinceletras.com.mx>,
<david.alvarez@lasquinceletras.com.mx>,<michelle.anzarut@lasquinceletras.com.mx>
}
\references{
HASTIE, TIBSHIRANI. \emph{The elements of statistical learning}
}
% ~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
% ~~ documentation directory ~~
\keyword{ package }
\keyword{ investigacion }
\keyword{ mercados }
\keyword{ analisis }
\keyword{ datos }



\seealso{
% Optional links to other man pages, e.g.
% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}}
}
\examples{
#Utilizar tmr (TablasMultResp)

data(BaseEjemplo)
preguntas<-c("P12R1","P12R2","P12R3")
tabla<-tmr(BaseEjemplo,preguntas,base=200)

}
