\name{biplotCustom2}
\alias{biplotCustom2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Biplot Personalizado
}
\description{
Hace gráficas biplot, para usarse con \code{\link{prcomp}}}
\usage{
biplotCustom2(x, choices = 1L:2L, scale = 1, pc.biplot = FALSE, ind = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{choices}{
%%     ~~Describe \code{choices} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
}
  \item{pc.biplot}{
%%     ~~Describe \code{pc.biplot} here~~
}
  \item{ind}{
lógico, por default ind = TRUE, si FALSE no se grafican los individuos
}
  \item{\dots}{
ejes, lógico, por default ejes = FALSE, si TRUE se dibujan los ejes x = 0, y = 0
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, choices = 1L:2L, scale = 1, pc.biplot = FALSE, 
    ind = TRUE,...) 
{
    if (length(choices) != 2) 
        stop("length of choices must be 2")
    if (!length(scores <- x$x)) 
        stop(gettextf("object '\%s' has no scores", deparse(substitute(x))), 
            domain = NA)
    if (is.complex(scores)) 
        stop("biplots are not defined for complex PCA")
    lam <- x$sdev[choices]
    n <- NROW(scores)
    lam <- lam * sqrt(n)
    if (scale < 0 || scale > 1) 
        warning("'scale' is outside [0, 1]")
    if (scale != 0) 
        lam <- lam^scale
    else lam <- 1
    if (pc.biplot) 
        lam <- lam/sqrt(n)
    a <- biplotCoords2(t(t(scores[, choices])/lam), t(t(x$rotation[, 
        choices]) * lam), ind = ind, ...)
    invisible()
    a
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
