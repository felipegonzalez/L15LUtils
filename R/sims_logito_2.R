sims.logito.2 <- function(m, sims.num = 500, vars.agreg){
    mod <- m
    
    sims.c <- sim(mod, n.sims = sims.num)@coef
    mm.a <- expand.grid(mod$xlevels)
    
        # rearmar fórmula
    aux <- attr(mod$terms, 'term.labels')
    form <- formula(paste("~ ", paste(aux, "", sep = "", collapse = " + "), sep = ""))


    mm.b <- model.matrix(form, data = mm.a)
    mm.b2 <- mm.b[,colnames(sims.c)]

    mm.c <- data.frame(mm.a, mm.b2)
    
    preds <- mm.b2 %*% t(sims.c)
    colnames(preds) <- 1:sims.num
    
    mm.d <- data.frame(mm.c, preds)
    
    vars.x <- paste("X", 1:sims.num, sep = "")

    mm.f <- ddply(mm.d, vars.agreg, function(sub){
        media = mean(invlogit(apply(sub[,vars.x], 1, mean)))

        # hacer chorizo
        lista <- lapply(vars.x, function(c){sub[,c]})
        chorizo <- Reduce('c', lista)

        ymin = invlogit(quantile(chorizo, 0.1))
        ymax = invlogit(quantile(chorizo, 0.9))
        media = invlogit(mean(chorizo))

        data.frame(media, ymin, ymax)       
    })

    gg <- mm.f
}
