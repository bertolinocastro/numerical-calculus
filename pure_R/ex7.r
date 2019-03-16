fibonacci <- function(n){
	if(n==0)
		return(0)	
	if(n==1)
		return(1)
	return(fibonacci(n-1)+fibonacci(n-2))
}

for (i in 0:25)
	cat('i=',i,' resultado: ',fibonacci(i),'\n')
