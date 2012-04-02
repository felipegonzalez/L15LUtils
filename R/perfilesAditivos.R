perfilesAditivos <-
function(contingencia){
	contingencia.1<-scale(contingencia,center = T, scale = F)
	contingencia.2<-scale(t(contingencia.1),center = T, scale = F)
	round(100*t(contingencia.2))
}

