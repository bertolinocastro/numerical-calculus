somacoluna <- function(mat){
    tmat <- matrix(, nrow=ncol(mat), ncol=nrow(mat))
    for (i in 1:nrow(mat))
        for (j in 1:ncol(mat))
            tmat[j,i] <- mat[i,j]
    s <- 0
    for (i in 1:nrow(tmat))
        s <- s + tmat[i,1]
    s
}

l <- rexp(100)
dim(l) <- c(10,10)

cat('l=',l,'\n')
cat('somacoluna(l) = ',somacoluna(l),'\n')
