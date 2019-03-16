fatorial <- function(n){
	if (n<=1)
		return(1)
	n*fatorial(n-1)
}
for (i in 1:15)
	cat('i=',i,' resultado: ',fatorial(i),'\n')
