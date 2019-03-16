multiplos <- function(y){
	s <- 0
	for (j in 1:(y-1))
		if(j%%3==0 || j%%5==0)
			s <- s+ j
	s
}

for (i in 1:100)
	cat('i=',i,' resultado: ',multiplos(i),'\n')
