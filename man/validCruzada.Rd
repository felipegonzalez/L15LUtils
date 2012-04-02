\name{validCruzada}
\alias{validCruzada}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Realizar Validación Cruzada}
\description{
	Esta funcion calcula el error de validacion cruzada de diferentes ajustes con el metodo glmnet}
\usage{
validCruzada(y, X, ajuste.1, familia = "gaussian", alfa, lambda, salto = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
	Vector de respuesta del modelo}
  \item{X}{
	Matriz con los datos de las variables independientes}
  \item{ajuste.1}{
	Ajuste realizado por glmnet}
  \item{familia}{
	Mismo argumento que en glmnet. Defaults to gaussian}
  \item{alfa}{
	Parametro \eqn{\alpha}{\alpha} de mezclado del elastic net utilizado por glmnet. }
  \item{lambda}{ 
	Parametro \eqn{\lambda}{\lambda} inicial utilizado por glmnet}

  \item{salto}{
	Numero que indica la cantidad de observaciones a separar en cada doblez de validacion cruzada}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Regresa el valor de \eqn{\lambda}{\lambda} óptimo, además de que produce gráficas en el proceso.
}
\references{
Ver HASTIE, TIBSHIRANI. \emph{The elements of statistical learning} }
\author{
	Michelle Anzarut}
\note{
	}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	Veer también \code{\link{glmnet}}}
\examples{


## The function is currently defined as
function(y, X, ajuste.1, familia = "gaussian",alfa,lambda , salto = 2){
    N <- nrow(X)
	set.seed(17)
    orden <- sample(1:N,N)
    X1 <- X[orden ,]
    y1 <- y[orden]
	predecir <- function(i) {apply(i,1,which.max)}
	print ("Numero de regresiones:")
	print(length(seq(1,N-1-salto,salto)))
	errores <- sapply(seq(1,N-1-salto,salto), function(i) {
		ajuste <- glmnet(x = X1[-(i:(i+salto-1)),], y = y1[-(i:(i+salto-1))], family = familia , 												   		alpha = alfa,
			lambda = ajuste.1$lambda)
        pred <- predict(ajuste, newx = X1[i:(i+salto-1),])
        if (familia == "multinomial"){
			prediccion <- apply(pred, 3, predecir)
			apply(y1[i:(i+salto-1)] != prediccion, 2,mean)
		}
		else {
			apply((pred-y1[i:(i+salto-1)])^2,2,mean)	
		}
    })
    medias <- apply(sqrt(errores),1,mean)
    lambda.opt<-which.min(medias)
	sd((medias))
    plot(1:length(ajuste.1$lambda),medias,type="b")
    par(ask=TRUE)
    errorest<-sd(medias)/sqrt(length(medias))
   
    errbar(1:length(ajuste.1$lambda),medias,medias+errorest,medias-errorest,add=T)

    errbar(1:length(ajuste.1$lambda),medias,medias+errorest,medias-errorest)
	
	if (familia == "gaussian"){
		plot(predict(ajuste.1,newx=X)[,lambda],y)
	}
	else {
		pred.2 <- predict(ajuste.1, newx = X, type = "class")
		print("Porcentaje de mal predichos:")
		print(round(100*mean(y1 != pred.2[,lambda])))
	}
    par(ask=F)
	lambda.opt
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ validacion }
\keyword{ cruzada }
\keyword{ lambda }

