\name{ddply.mult}
\alias{ddply.mult}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ddply para respuesta multiple}
\description{
Produce tablas resumen de preguntas de respuesta multiple, cortando por
distintas dimensiones y filtrando segun variables seleccionadas.}
\usage{
ddply.mult(.data, .variables, .grupo = NULL, .patron = NULL, 
      .progress = 'none', .drop = FALSE, filtro.var = NULL, filtro.vals = NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.data}{
  Tabla de datos en forma ancha (por individuo)
}
  \item{.variables}{
  Dimensiones de corte (ver ddply)
  }
  \item{.grupo}{
  vector con identificadores de las columnas que componen la respuesta multiple
}
  \item{.patron}{
  expresion regular para determinar las columnas que componen la respuesta multiple.
}
  \item{filtro.var}{
  nombre de columna de .data con la que se filtra
}
  \item{filtro.vals}{
  valores incluidos en el filtro.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
Hay que tener cuidado con valores faltantes: este codigo, como esta ahora,
excluye todos los faltantes del calculo, aunque se contabilizan el base para
calcular porcentajes.}
\value{ Data frame con porcentajes y sus errores estandar calculados bajo MASSR.
Incluye tres atributos adicionales: filtro.activo, y columnas.nom (nombres de columnas
de respuesta multiple).}
\references{
Refs
}
\author{
Felipe Gonzalez}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Ver tambien paquete plyr}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


