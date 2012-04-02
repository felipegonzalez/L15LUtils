\name{plotCaRota}
\alias{plotCaRota}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot de Analisis Canonico con Etiquetas Acomodadas}
\description{
		Esta función hace el grafico de analisis canonico, con rotacion de etiquetas y recocido simulado para no encimarlas}
\usage{
plotCaRota(x, theta = 0, dim = c(1, 2), map = "symmetric", what = c("all", "all"), mass = c(FALSE, FALSE), contrib = c("none", "none"), col = c("#000000", "#FF0000"), pch = c(16, 1, 17, 24), labels = c(2, 2), arrows = c(FALSE, FALSE), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Simple correspondence analysis object returned by ca
	}
  \item{theta}{
	Angulo con el cual hacer la rotacion}
  \item{dim}{
Numerical vector of length 2 indicating the dimensions to plot on horizontal and vertical axes respectively; default is first dimension horizontal and second dimension vertical.}
  \item{map}{
Character string specifying the map type. Allowed options include symmetric (default) ,rowprincipal,colprincipal,symbiplot,rowgab,colgab, rowgreen,colgreen}
  \item{what}{
Vector of two character strings specifying the contents of the plot. First entry sets the rows and the second entry the columns. Allowed values are 
all (all available points, default) 
active (only active points are displayed) 
passive (only supplementary points are displayed) 
none (no points are displayed) 
The status (active or supplementary) of rows and columns is set in ca using the options suprow and supcol.}
  \item{mass}{
Vector of two logicals specifying if the mass should be represented by the area of the point symbols (first entry for rows, second one for columns)}
  \item{contrib}{
Vector of two character strings specifying if contributions (relative or absolute) should be represented by different colour intensities. Available options are
none (contributions are not indicated in the plot).
absolute (absolute contributions are indicated by colour intensities).
relative (relative contributions are indicated by colour intensities).
If set to absolute or relative, points with zero contribution are displayed in white. The higher the contribution of a point, the closer the corresponding colour to the one specified by the col option.}
  \item{col}{
Vector of length 2 specifying the colours of row and column point symbols, by default blue for rows and red for columns. Colours can be entered in hexadecimal (e.g. \#FF0000), rgb (e.g. rgb(1,0,0)) values or by R-name (e.g. red).}
  \item{pch}{
Vector of length 4 giving the type of points to be used for row active and supplementary, column active and supplementary points. See pchlist for a list of symbols.}
  \item{labels}{
Vector of length two specifying if the plot should contain symbols only (0), labels only (1) or both symbols and labels (2). Setting labels to 2 results in the symbols being plotted at the coordinates and the labels with an offset.}
  \item{arrows}{
Vector of two logicals specifying if the plot should contain points (FALSE, default) or arrows (TRUE). First value sets the rows and the second value sets the columns.}
  \item{\dots}{
Further arguments passed to plot and points.}
}
\details{
The function plot.ca makes a two-dimensional map of the object created by ca with respect to two selected dimensions. By default the scaling option of the map is ``symmetric'', that is the so-called symmetric map. In this map both the row and column points are scaled to have inertias (weighted variances) equal to the principal inertia (eigenvalue or squared singular value) along the principal axes, that is both rows and columns are in pricipal coordinates. Other options are as follows:

-``rowprincipal'' or ``colprincipal'' - these are the so-called asymmetric maps, with either rows in principal coordinates and columns in standard coordinates, or vice versa (also known as row-metric-preserving or column-metric-preserving respectively). These maps are biplots;
-``symbiplot'' - this scales both rows and columns to have variances equal to the singular values (square roots of eigenvalues), which gives a symmetric biplot but does not preserve row or column metrics;
-``rowgab'' or ``colgab'' - these are asymmetric maps (see above) with rows (respectively, columns) in principal coordinates and columns (respectively, rows) in standard coordinates multiplied by the mass of the corresponding point. These are also biplots and were proposed by Gabriel & Odoroff (1990);
-``rowgreen'' or ``colgreen'' - these are similar to ``rowgab'' and ``colgab'' except that the points in standard coordinates are multiplied by the square root of the corresponding masses, giving reconstructions of the standardized residuals.
This function has options for sizing and shading the points. If the option mass is TRUE for a set of points, the size of the point symbol is proportional to the relative frequency (mass) of each point. If the option contrib is ``absolute'' or ``relative'' for a set of points, the colour intensity of the point symbol is proportional to the absolute contribution of the points to the planar display or, respectively, the quality of representation of the points in the display.}
\value{
La GRAFICA de analisis canonico correspondiente.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
	Teresa Ortiz}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 Ver también \code{\link{biplotCustom}}}
\examples{


## The function is currently defined as
function (x, theta=0,dim = c(1, 2), map = "symmetric", what = c("all", 
    "all"), mass = c(FALSE, FALSE), contrib = c("none", "none"), 
    col = c("#000000", "#FF0000"), pch = c(16, 1, 17, 24), labels = c(2, 
        2), arrows = c(FALSE, FALSE), ...) 
{
    obj <- x
    if (length(what) != 2) 
        what <- rep(what, length = 2)
    if (length(mass) != 2) 
        mass <- rep(mass, length = 2)
    if (length(contrib) != 2) 
        contrib <- rep(contrib, length = 2)
    if (length(col) != 2) 
        col <- rep(col, length = 2)
    if (length(labels) != 2) 
        labels <- rep(labels, length = 2)
    if (length(pch) != 4) 
        pch <- rep(pch, length = 4)
    if (!is.numeric(x$suprow)) {
        if (map == "colgab" | map == "colgreen") {
            if (what[1] != "none") 
                what[1] <- "active"
        }
    }
    if (!is.numeric(x$supcol)) {
        if (map == "rowgab" | map == "rowgreen") {
            if (what[2] != "none") 
                what[2] <- "active"
        }
    }
    K <- dim(obj$rowcoord)[2]
    I <- dim(obj$rowcoord)[1]
    J <- dim(obj$colcoord)[1]
    svF <- matrix(rep(obj$sv[1:K], I), I, K, byrow = TRUE)
    svG <- matrix(rep(obj$sv[1:K], J), J, K, byrow = TRUE)
    rpc <- obj$rowcoord * svF
    cpc <- obj$colcoord * svG
    symrpc <- obj$rowcoord * sqrt(svF)
    symcpc <- obj$colcoord * sqrt(svG)
    mt <- c("symmetric", "rowprincipal", "colprincipal", "symbiplot", 
        "rowgab", "colgab", "rowgreen", "colgreen")
    mti <- 1:length(mt)
    mtlut <- list(symmetric = list(x = rpc, y = cpc), rowprincipal = list(x = rpc, 
        y = obj$colcoord), colprincipal = list(x = obj$rowcoord, 
        y = cpc), symbiplot = list(x = symrpc, y = symcpc), rowgab = list(x = rpc, 
        y = obj$colcoord * obj$colmass), colgab = list(x = obj$rowcoord * 
        obj$rowmass, y = cpc), rowgreen = list(x = rpc, y = obj$colcoord * 
        sqrt(obj$colmass)), rowgreen = list(x = obj$rowcoord * 
        sqrt(obj$rowmass), y = cpc))
    x <- mtlut[[mti[mt == map]]][[1]]
    y <- mtlut[[mti[mt == map]]][[2]]
    x.names <- obj$rownames
    y.names <- obj$colnames
    rm(mt, mti, mtlut)
    indx <- dim(x)[1]
    indy <- dim(y)[1]
    pch.x <- rep(pch[1], dim(x)[1])
    pch.y <- rep(pch[3], dim(y)[1])
    pr <- c("none", "active", "passive", "all")
    pri <- 1:4
    if (is.na(obj$rowsup[1])) {
        sup.x <- NA
        act.x <- x
        xn.sup <- NA
        xn.act <- x.names
    }
    else {
        sup.x <- x[obj$rowsup, ]
        act.x <- x[-obj$rowsup, ]
        pch.x[obj$rowsup] <- pch[2]
        xn.sup <- x.names[obj$rowsup]
        xn.act <- x.names[-obj$rowsup]
    }
    if (is.na(obj$colsup[1])) {
        sup.y <- NA
        act.y <- y
        yn.sup <- NA
        yn.act <- y.names
    }
    else {
        sup.y <- y[obj$colsup, ]
        act.y <- y[-obj$colsup, ]
        pch.y[obj$colsup] <- pch[4]
        yn.sup <- y.names[obj$colsup]
        yn.act <- y.names[-obj$colsup]
    }
    prlut <- list(none = list(x = NA, y = NA), active = list(x = act.x, 
        y = act.y), supplementary = list(x = sup.x, y = sup.y), 
        all = list(x = x, y = y))
    nameslut <- list(none = list(x.names = NA, y.names = NA), 
        active = list(x.names = xn.act, y.names = yn.act), supplementary = list(x.names = xn.sup, 
            y.names = yn.sup), all = list(x.names = x.names, 
            y.names = y.names))
    pchlut <- list(none = list(x.pch = NA, y.pch = NA), active = list(x.pch = rep(pch[1], 
        dim(x)[1]), y.pch = rep(pch[3], dim(y)[1])), supplementary = list(x.pch = rep(pch[2], 
        dim(x)[1]), y.pch = rep(pch[4], dim(y)[1])), all = list(x.pch = pch.x, 
        y.pch = pch.y))
    x <- prlut[[pri[pr == what[1]]]][[1]]
    y <- prlut[[pri[pr == what[2]]]][[2]]
    x.names <- nameslut[[pri[pr == what[1]]]][[1]]
    y.names <- nameslut[[pri[pr == what[2]]]][[2]]
    x.pch <- pchlut[[pri[pr == what[1]]]][[1]]
    y.pch <- pchlut[[pri[pr == what[2]]]][[2]]
    if (is.matrix(x)) {
        x <- x[, dim]
    }
    else {
        x <- matrix(x[dim], ncol = length(dim), nrow = 1)
    }
    if (is.matrix(y)) {
        y <- y[, dim]
    }
    else {
        y <- matrix(y[dim], ncol = length(dim), nrow = 1)
    }
    if (mass[1]) 
        cex.x <- 0.5 + obj$rowmass^(1/3)/max(obj$rowmass^(1/3))
    else cex.x <- 1
    if (mass[2]) 
        cex.y <- 0.5 + obj$colmass^(1/3)/max(obj$colmass^(1/3))
    else cex.y <- 1
    nc0 <- 50
    cst <- 230
    col.x <- col[1]
    col.y <- col[2]
    if (contrib[1] == "relative") {
        cind <- obj$rowmass * (rpc[, dim[1]]^2 + rpc[, dim[2]]^2)/obj$rowinertia
        cb.x <- col2rgb(col[1])
        collut.x <- rgb(seq(cst, cb.x[1, 1], length = nc0), seq(cst, 
            cb.x[2, 1], length = nc0), seq(cst, cb.x[3, 1], length = nc0), 
            maxColorValue = 255)
        xtemp <- nc0 * (cind)
        col.x <- collut.x[xtemp]
    }
    else if (contrib[1] == "absolute") {
        cind <- obj$rowmass * (rpc[, dim[1]]^2 + rpc[, dim[2]]^2)/(obj$sv[dim[1]]^2 + 
            obj$sv[dim[2]]^2)
        cb.x <- col2rgb(col[1])
        p.x <- cb.x[, 1] + (cst - cb.x[, 1])/indx
        collut.x1 <- rgb(seq(cst, p.x[1], length = nc0/2), seq(cst, 
            p.x[2], length = nc0/2), seq(cst, p.x[3], length = nc0/2), 
            maxColorValue = 255)
        collut.x2 <- rgb(seq(p.x[1], cb.x[1, 1], length = nc0/2), 
            seq(p.x[2], cb.x[2, 1], length = nc0/2), seq(p.x[3], 
                cb.x[3, 1], length = nc0/2), maxColorValue = 255)
        collut.x <- c(collut.x1, collut.x2)
        xtemp <- nc0 * (cind)
        col.x <- collut.x[xtemp]
    }
    if (contrib[2] == "relative") {
        cind <- obj$colmass * (cpc[, dim[1]]^2 + cpc[, dim[2]]^2)/obj$colinertia
        cb.y <- col2rgb(col[2])
        collut.y <- rgb(seq(cst, cb.y[1, 1], length = nc0), seq(cst, 
            cb.y[2, 1], length = nc0), seq(cst, cb.y[3, 1], length = nc0), 
            maxColorValue = 255)
        ytemp <- nc0 * cind
        col.y <- collut.y[ytemp]
    }
    if (contrib[2] == "absolute") {
        cind <- obj$colmass * (cpc[, dim[1]]^2 + cpc[, dim[2]]^2)/(obj$sv[dim[1]]^2 + 
            obj$sv[dim[2]]^2)
        cb.y <- col2rgb(col[2])
        p.y <- cb.y[, 1] + (cst - cb.y[, 1])/indy
        collut.y1 <- rgb(seq(cst, p.y[1], length = nc0/2), seq(cst, 
            p.y[2], length = nc0/2), seq(cst, p.y[3], length = nc0/2), 
            maxColorValue = 255)
        collut.y2 <- rgb(seq(p.y[1], cb.y[1, 1], length = nc0/2), 
            seq(p.y[2], cb.y[2, 1], length = nc0/2), seq(p.y[3], 
                cb.y[3, 1], length = nc0/2), maxColorValue = 255)
        collut.y <- c(collut.y1, collut.y2)
        ytemp <- nc0 * cind
        col.y <- collut.y[ytemp]
    }
    q1 <- (1:dim(x)[1])
    q2 <- (1:dim(y)[1])
    l1 <- c(x[q1, 1], y[q2, 1])
    l1 <- l1[!is.na(l1)]
    l2 <- c(x[q1, 2], y[q2, 2])
    l2 <- l2[!is.na(l2)]
    if (length(l1) == 0) 
        l1 <- c(-0.1, 0.1)
    if (length(l2) == 0) 
        l2 <- c(-0.1, 0.1)
    lim1 <- range(l1) + c(-0.05, 0.05) * diff(range(l1))
    lim2 <- range(l2) + c(-0.05, 0.05) * diff(range(l2))
    pty.backup <- par()$pty
    
    # ============
    # = Rotaci<U+00F3>n =
    # ============
    A<-matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2)
    
    x.1 <- t(A\%*\%t(x[,1:2]))
    y.1 <- t(A\%*\%t(y[,1:2]))
    x[,1:2]<-x.1
    y[,1:2]<-y.1
    plot(c(x[, 1], y[, 1]), c(x[, 2], y[, 2]), xlab = "", ylab = "", 
        type = "n", axes = FALSE, asp = 1, ...)
    box()
    abline(h = 0, v = 0, lty = 3)
    axis(1, col = col[1])
    axis(2, col = col[1])
    if (!is.na(x[1]) & labels[1] != 1) {
        if (arrows[1]) {
            arrows(rep(0, length(x[, 1])), rep(0, length(x[, 
                1])), x[, 1], x[, 2], col = col.x, length = 0.1)
            x.t <- cbind(x[, 1], x[, 2])
	  }
        else {
            points(x[, 1], x[, 2], cex = cex.x, col = col.x, 
                pch = x.pch)
		# coord 
		x.t <- cbind(x[, 1], x[, 2])
        }
    }

    if (!is.na(y[1]) & labels[2] != 1) {
        if (arrows[2]) {
            arrows(rep(0, length(y[, 1])), rep(0, length(y[, 
                1])), y[, 1], y[, 2], col = col.y, length = 0.1)
            y.t <- cbind(y[, 1], y[, 2])
        }
        else {
            points(y[, 1], y[, 2], cex = cex.y, col = col.y, 
                pch = y.pch)
            y.t <- cbind(y[, 1], y[, 2])
        }
    }

    par(pty = pty.backup)
    d <- rbind(x.t, y.t)
    rownames(d) <- c(x.names, y.names)
    pointLabel(d[, 1], d[, 2], labels = rownames(d), cex = .75  )
    
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
