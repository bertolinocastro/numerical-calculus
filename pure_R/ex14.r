intersecao <- function(vec1,vec2){
    s <- 0
    for (i in vec1)
        if (i %in% vec2)
            s <- s + i
    s
}

l <- round(runif(100,-100,100))
m <- round(runif(100,-100,100))
cat('l=',l,'\n')
cat('\nm=',m,'\n')
cat('intersecao(l,m) = ',intersecao(l,m),'\n')
