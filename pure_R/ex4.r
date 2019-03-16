primo <- function(n){
	for(i in 2:n){
		if(n%%i==0 && n!=i){
			return(0)
		}
	}
	ifelse(n==1,0,1)
}

for (j in 1:36)
	cat('j=',j,' Resultado: ',primo(j),'\n')
