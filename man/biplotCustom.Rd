\name{biplotCustom}
\alias{biplotCustom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Biplot Personalizado}
\description{
Hace gráficas biplot, para usarse con \code{\link{princomp}}}
\usage{
biplotCustom(x, choices = 1L:2L, scale = 1, pc.biplot = FALSE, ind = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{choices}{
Length 2 vector specifying the components to plot. Only the default is a biplot in the strict sense.}
  \item{scale}{
The variables are scaled by lambda ^ scale and the observations are scaled by lambda ^ (1-scale) where lambda are the singular values as computed by princomp. Normally 0 <= scale <= 1, and a warning will be issued if the specified scale is outside this range.}
  \item{pc.biplot}{
If true, use what Gabriel (1971) refers to as a ``principal component biplot'', with \eqn{\lambda = 1} and observations scaled up by \eqn{\sqrt(n)} and variables scaled down by \eqn{\sqrt(n)}. Then inner products between variables approximate covariances and distances between observations approximate Mahalanobis distance.}
  \item{ind}{
%%     ~~Describe \code{ind} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
This is a method for the generic function biplot. There is considerable confusion over the precise definitions: those of the original paper, Gabriel (1971), are followed here. Gabriel and Odoroff (1990) use the same definitions, but their plots actually correspond to pc.biplot = TRUE}
\value{
(a plot is produced on the current graphics device.)
}
\references{
Gabriel, K. R. (1971). The biplot graphical display of matrices with applications to principal component analysis. Biometrika, 58, 453–467.

Gabriel, K. R. and Odoroff, C. L. (1990). Biplots in biomedical research. Statistics in Medicine, 9, 469–485.}
\author{
Teresa Ortiz}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Ver también \code{\link{plotCaRota}}
}
\examples{


## The function is currently defined as
function (x, choices = 1L:2L, scale = 1, pc.biplot = FALSE, 
    ind = TRUE, ...) 
{
    if (length(choices) != 2) 
        stop("length of choices must be 2")
    if (!length(scores <- x$scores)) 
        stop(gettextf("object '\%s' has no scores", deparse(substitute(x))), 
            domain = NA)
    lam <- x$sdev[choices]
    if (is.null(n <- x$n.obs)) 
        n <- 1
    lam <- lam * sqrt(n)
    if (scale < 0 || scale > 1) 
        warning("'scale' is outside [0, 1]")
    if (scale != 0) 
        lam <- lam^scale
    else lam <- 1
    if (pc.biplot) 
        lam <- lam/sqrt(n)
    # print(lam)
    a <- biplotCoords(t(t(scores[, choices])/lam), t(t(x$loadings[, 
        choices]) * lam), ind = ind,...)
    invisible()
    a
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
