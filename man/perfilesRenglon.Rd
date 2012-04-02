\name{perfilesRenglon}
\alias{perfilesRenglon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Perfiles Por Renglon}
\description{
Esta funcion calcula perfiles por renglon para una tabla de contingencias}

\usage{
perfilesRenglon(contingencia)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{contingencia}{
	Una tabla de contingencias
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	Una TABLA con los perfiles por columna correspondientes.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
	Michelle Anzarut}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Otras funciones de perfiles: \code{\link{perfiles}}, \code{\link{perfilesColumna}}, \code{\link{perfilesAditivos}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(contingencia){
	atr_promedio <- (margin.table(contingencia, margin = 1)/ncol(contingencia))
	perfiles_reng <- (scale(t(contingencia),center = FALSE, 
		scale = atr_promedio))
	#write.table(round(100*perfiles_col), sep = ",")
	t(round(100*perfiles_reng))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
