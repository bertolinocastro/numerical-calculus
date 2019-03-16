inteiros <- function(x){
	sum(1:x)
}

for (i in 1:100)
	cat('i=',i,' resultado: ',inteiros(i),'\n')
