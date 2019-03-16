somadiagonal <- function(mat){
    s <- 0
    for (i in 1:nrow(mat))
        s <- s <- mat[i,i]
    s
}

l <- rexp(100)
dim(l) <- c(10,10)

cat('l=',l,'\n')
cat('somadiagonal(l) = ',somadiagonal(l),'\n')
