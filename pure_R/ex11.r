somamatriz <- function(mat){
    s <- 0
    for (i in 1:nrow(mat)){
        for (j in 1:ncol(mat))
            s <- s + mat[i,j]
    }
    s
}



l <- rexp(100)
dim(l) <- c(10,10)

cat('l=',l,'\n')
cat('somamatriz(l) = ',somamatriz(l),'\n')
