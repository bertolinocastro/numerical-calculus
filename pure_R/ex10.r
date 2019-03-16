media <- function(vec){
    s <- 0
    for (i in vec)
        s <- s + i
    s/length(vec)
}


l <- runif(100,-100,100)
cat('l=',l,'\n')
cat('media(l) = ',media(l),'\n')
