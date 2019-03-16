maior <- function(vec){
    max <- vec[[1]]
    for (i in vec)
        if (i > max)
            max <- i
    max
}


l <- runif(100,-100,100)
cat('l=',l,'\n')
cat('maior(l) = ',maior(l),'\n')
