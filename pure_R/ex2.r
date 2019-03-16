divide <- function(a,b){
	c <- a
	x <- 0
	while(c >= b){
		c <- c - b
		x <- x + 1
	}
	c(x,c)
}

cat('Resultado: ',divide(1,2),'\n')
cat('Resultado: ',divide(6,2),'\n')
cat('Resultado: ',divide(7,2),'\n')
cat('Resultado: ',divide(2,4),'\n')
cat('Resultado: ',divide(0,4),'\n')
cat('Resultado: ',divide(10,0),'\n')
