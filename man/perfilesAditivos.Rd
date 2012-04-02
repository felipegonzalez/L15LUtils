\name{perfilesAditivos}
\alias{perfilesAditivos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Perfiles Aditivos}
\description{
	Esta funcion calcula perfiles aditivos para una tabla de contingencias}
\usage{
perfilesAditivos(contingencia)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{contingencia}{
	Tabla de contingencia}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	Una TABLA con los perfiles aditivos correspondientes.
}
\references{
	}
\author{
	Michelle Anzarut}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{perfiles}}, \code{\link{perfilesColumna}}, \code{\link{perfilesRenglon}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(contingencia){
	contingencia.1<-scale(contingencia,center = T, scale = F)
	contingencia.2<-scale(t(contingencia.1),center = T, scale = F)
	round(100*t(contingencia.2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
