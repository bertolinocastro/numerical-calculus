potencia <- function(a,b){
	c <- 1
	for (i in 1:b)
		c <- c * a
	c
}

cat('Resultado: ',potencia(1,2),'\n')
cat('Resultado: ',potencia(3,2),'\n')
cat('Resultado: ',potencia(4,2),'\n')
cat('Resultado: ',potencia(5,5),'\n')
