perfiles_boot <- function(datos, header, var, n.boot = 100, marcar.tol = 20){
    # Calcular primero puntuales
    dat <- datos[, c(header, var)]
    tablas <- function(datos, header, var){
        tab.raw <- ddply(datos, header, function(df){
            salida <- data.frame(table(df[, var]))
            names(salida) <- c( var, 'Freq')
            salida$Base <- sum(salida$Freq)
            salida
        })
        tab.prop <- ddply(tab.raw, header, transform, Prop = 100*Freq/sum(Freq))
        tab.momios <- ddply(tab.prop, var, transform, Perfil = 100*Prop/mean(Prop))
        tab.momios
    }

    muestra.boot <- function(datos, header){
        datos.boot <- ddply(datos, header, function(df){
            muestra.reng <- sample(1:nrow(df), nrow(df), replace = TRUE)
            df[muestra.reng, ]   
        })
        datos.boot
    }
    reps <- rdply(n.boot, function(){
        tablas(muestra.boot(dat, header), header, var)
    })
    out <- ddply(reps, c(header,var), summarise, 
        Base = mean(Base),
        Freq = mean(Freq),
        Prop = mean(Prop),
        Perfil = mean(Perfil),
        Perfil.sd = sd(Perfil),
        Perfil.10 = quantile(Perfil, 0.10),
        Perfil.90 = quantile(Perfil, 0.90)
    )
    out$marcar <- (out$Perfil.90-100)*(out$Perfil.10-100) > 0
    out$marcar.dif <- out$marcar & (out$Perfil > 100+marcar.tol || out$Perfil < 100-marcar.tol)
    out
}