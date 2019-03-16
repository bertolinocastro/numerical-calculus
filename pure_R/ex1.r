multiplica <- function(a,b){
	c <- 0
	for (i in 1:b)
		c <- c + a
	c
}

x <- multiplica(5,3)
cat('Resultado: ',x,'\n')

x <- multiplica(5,5)
cat('Resultado: ',x,'\n')

x <- multiplica(5,7)
cat('Resultado: ',x,'\n')

x <- multiplica(1,3)
cat('Resultado: ',x,'\n')

