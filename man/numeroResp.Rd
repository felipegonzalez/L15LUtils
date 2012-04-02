\name{numeroResp}
\alias{numeroResp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Numero de respondentes de pregunta de respuesta multiple}
\description{
	Esta funcion calcula el numero de respondentes distintos para una pregunta de respuesta multiple, contando solo una vez a aquellos que (posiblemente) contestan varias cosas, y contando a todos los que contestaron por lo menos una columna.}
\usage{
numeroResp(datos, ind)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datos}{
	Base de datos para con las preguntas relevantes}
  \item{ind}{
	Codigos de las columnas de la base correspondientes a la pregunta de interés}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	Un entero correspondiente al número de respondentes 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
	Felipe Gonzalez, Teresa Ortiz, David Alvarez, Michelle Anzarut}
	\note{
		Si se quieren mas notas}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(datos, ind){
	datos_ind<-as.data.frame(datos[,ind])
	no_respuestas<-apply(datos_ind,1,function(j) {
		sum(!is.na(j))>0
		})
	sum(no_respuestas)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Respondentes }
\keyword{ Respuesta}
\keyword{ Multiple }% __ONLY ONE__ keyword per line
