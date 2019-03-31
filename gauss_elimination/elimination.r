# elimination
elimination <- function(A){
	i <- 1
	j <- 1
	m <- nrow(A)
	n <- ncol(A)
	while(i <= m && j <= n){
		i_max <- argmax(A[i:m,j])
		if(A[[i_max,j]] == 0)
			j <- j + 1
		else{
			swap(A[i_max],A[i])
			for(h in i:m){
				f <- A[[h,j]]/A[[i,j]]
				A[[h,j]] <- 0
				for(k in j+1:n)
					A[[h,k]] <- A[[h,k]] - A[[i,k]]*f
			}
			i <- i + 1
			j <- j + 1
		}
	}

}

# result
result <- function(A){
	i <- 1
	j <- 1
	m <- nrow(A)
	n <- ncol(A)
	X <- matrix(n)
	for(i in m:1){
		X[[i]] <- A[[i,n]]
	}


	
	for(i in 1:m){
		X[n-i]=B[n-i];
		for(j in 1:i)
			X[n-1-i] = X[n-1-i] - A[n-1-i][m-1-j]*X[n-1-j];
		X[n-1-i] = X[n-1-i]/A[n-1-i][n-1-i];
	}
}
