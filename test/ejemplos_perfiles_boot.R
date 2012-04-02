datos$ciudad.edad <- interaction(datos$EDADR, datos$CIUDAD)
out <- perfiles_boot(datos, 'ciudad.edad', 'P30M1R01', n.boot=100, marcar.tol=15)
out$P30M1R01<-as.character(out$P30M1R01)
ggplot(out, aes(x=Prop, y=Perfil, ymax=Perfil.10, ymin=Perfil.90, colour=EDADR)) + geom_point()+
geom_linerange() + facet_wrap(~P30M1R01) 

out$Perfil.2 <- out$Perfil
out$Perfil.2[is.na(out$Perfil)] <- 100
out$marcar.dif[is.na(out$marcar.dif)] <- FALSE

out.x <- out[out$Freq>0,]
out.x$P30M1R01 <- str_trim(out.x$P30M1R01)
out.x$P30M1R01 <- reorder(out.x$P30M1R01, out.x$Freq, sum)
ggplot(out.x, aes(x=ciudad.edad, y=P30M1R01, fill=(Perfil.2-100)*marcar*(Prop>1), label=sprintf("%1.0f",Prop))) +
	geom_tile() + geom_text(size=3, colour="gray20") +
	scale_fill_gradient2(low = "red", high = "lightgreen")+
	opts(panel.grid.minor=theme_blank()) +
  opts(axis.ticks = theme_blank()) +
  opts(panel.grid.major=theme_blank()) +
  opts(axis.title.y = theme_blank()) +
  opts(axis.title.x = theme_blank()) +
  opts(panel.background = theme_rect(colour=NA))+
  opts(legend.position = "none")


out <- perfiles_boot(datos, 'EDADR', 'P30M1R01', 'SEXO', n.boot=100, marcar.tol=15)

out$Perfil.2 <- out$Perfil
out$Perfil.2[is.na(out$Perfil)] <- 100
out$marcar.dif[is.na(out$marcar.dif)] <- FALSE

out.x <- out[out$Freq>0,]
out.x$P30M1R01 <- str_trim(out.x$P30M1R01)
out.x$P30M1R01 <- reorder(out.x$P30M1R01, out.x$Freq, sum)
ggplot(out.x, aes(x=EDADR, y=P30M1R01, fill=(Perfil.2-100)*marcar*(Prop>1), label=sprintf("%1.0f%%",Prop))) +
	geom_tile() + geom_text(size=3, colour="gray20") +
	scale_fill_gradient2(low = "red", high = "lightgreen")+
	 facet_wrap(~SEXO, nrow=1)+
	opts(panel.grid.minor=theme_blank()) +
  opts(axis.ticks = theme_blank()) +
  opts(panel.grid.major=theme_blank()) +
  opts(axis.title.y = theme_blank()) +
  opts(axis.title.x = theme_blank()) +
  opts(panel.background = theme_rect(colour=NA))+
  opts(legend.position = "none")
