\name{ddply.mult}
\alias{ddply.mult}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	ddply para respuesta múltiple}
\description{
Produce tablas resumen de preguntas de respuesta múltiple, cortando por
distintas dimensiones y filtrando según variables seleccionadas.}
\usage{
ddply.mult(.data, .variables, .grupo = NULL, .patron = NULL, 
      .progress = 'none', .drop = FALSE, filtro.var = NULL, filtro.vals = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.data}{
  Tabla de datos en forma ancha (por individuo)
}
  \item{.variables}{
  Dimensiones de corte (ver ddply)
  }
  \item{.grupo}{
  vector con identificadores de las columnas que componen la respuesta múltiple
}
  \item{.patron}{
  expresión regular para determinar las columnas que componen la respuesta múltiple.
}
  \item{filtro.var}{
  nombre de columna de .data con la que se filtra
}
  \item{filtro.vals}{
  valores incluídos en el filtro.
%%     ~~Describe \code{ind} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}

\details{
Hay que tener cuidado con valores faltantes: este código, como está ahora,
excluye todos los faltantes del cálculo, aunque se contabilizan el base para
calcular porcentajes.}
\value{ Data frame con porcentajes y sus errores estándar calculados bajo MASSR.
Incluye tres atributos adicionales: filtro.activo, y columnas.nom (nombres de columnas
de respuesta múltiple).}
\references{
}
\author{
Felipe González}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Ver también paquete plyr}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

