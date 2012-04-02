perfiles_boot <- function(datos, header, var, n.boot = 1, marcar.tol = 20){
    # Calcular primero puntuales
    dat <- datos[, c(header, var)]
    tablas <- function(datos, header, var){
        tab.raw <- ddply(datos, header, function(df){
            salida <- data.frame(table(df[, var]))
            names(salida) <- c( var, 'Freq')
            salida
        })
        tab.prop <- ddply(tab.raw, header, transform, Prop = 100*Freq/sum(Freq))
        tab.momios <- ddply(tab.prop, var, transform, Perfil = 100*Prop/mean(Prop))
        tab.momios
    }

    muestra.boot <- function(datos){
         muestra.reng <- sample(1:nrow(datos), nrow(datos), replace = TRUE)
         datos[muestra.reng, ]
    }
    reps <- rdply(n.boot, function(){
        tablas(muestra.boot(dat), header, var)
    })
    out <- ddply(reps, c(header,var), summarise, 
        Perfil = mean(Perfil),
        Perfil.sd = sd(Perfil),
        Perfil.10 = quantile(Perfil, 0.10),
        Perfil.90 = quantile(Perfil, 0.90)
    )
    out$marcar <- (out$Perfil.90-100)*(out$Perfil.10-100) > 0
    out$marcar.dif <- out$marcar & (out$Perfil > 100+marcar.tol || out$Perfil < 100-marcar.tol)
    out
}