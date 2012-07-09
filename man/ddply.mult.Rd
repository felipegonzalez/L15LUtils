\name{ddply.mult}
\alias{ddply.mult}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	ddply para respuesta múltiple}
\description{
Produce tablas resumen de preguntas de respuesta m\'ultiple, cortando por
distintas dimensiones y filtrando seg\'un variables seleccionadas.}
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
  vector con identificadores de las columnas que componen la respuesta m\'ultiple
}
  \item{.patron}{
  expresi\'on regular para determinar las columnas que componen la respuesta m\'ultiple.
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
Hay que tener cuidado con valores faltantes: este c\'odigo, como est\'a ahora,
excluye todos los faltantes del c\'alculo, aunque se contabilizan el base para
calcular porcentajes.}
\value{ Data frame con porcentajes y sus errores est\'andar calculados bajo MASSR.
Incluye tres atributos adicionales: filtro.activo, y columnas.nom (nombres de columnas
de respuesta m\'ultiple).}
\references{
Refs
}
\author{
Felipe Gonz\'alez}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Ver tambi\'en paquete plyr}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

