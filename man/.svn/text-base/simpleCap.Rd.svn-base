\name{simpleCap}
\alias{simpleCap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Start Case}
\description{
	Transforma una variable de tipo string a start case (Mayusuculas solo al inicio de palabras)}
\usage{
simpleCap(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	Variable tipo string}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	Un string con Start Case aplicado
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
	Felipe Gonzalez}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

frase<-"Soy una Palabra con MAYUSCULAS"
simpleCap(frase)

## The function is currently defined as
function(x) {
    #print(x)
    if(is.character(x) ){
        s <- strsplit(x, " ")[[1]]
        #print(s)
        t <- s[s!=""]
        #print(t)
        #}
        paste(toupper(substring(t, 1,1)), tolower(substring(t, 2)),
          sep="", collapse=" ")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
