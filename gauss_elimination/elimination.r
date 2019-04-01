argmax <- function(s){
	m <- max(s)
	i <- 1
	while(s[[i]] != m)
		i <- i + 1
	i
}

swap <- function(A, x, y){
	tmp <- A[[x]]
	A[[x]] <- A[[y]]
	A[[y]] <- tmp
}

# elimination
elimination <- function(A){
	i <- 1
	j <- 1
	m <- nrow(A)
	n <- ncol(A)
	while(i < m && j < n){
		i_max <- argmax(A[i:m,j])
		if(A[[i_max,j]] == 0)
			j <- j + 1
		else{
			swap(A,i_max,i) # TODO: check this swap (isn't working)
			for(h in seq(i+1,m,1)){
				f <- A[[h,j]]/A[[i,j]]
				A[[h,j]] <- 0
				for(k in seq(j+1,n,1)){
					A[[h,k]] <- A[[h,k]] - A[[i,k]]*f
				}
			}
			print(A)
			i <- i + 1
			j <- j + 1
		}
	}
	A
}

# result
result <- function(A){
	m <- nrow(A)
	n <- ncol(A)
	X <- matrix()
	print(X)
	cat('m:',m,'n:',n,'\n')
	for(i in 1:m){
		cat('i:',i,'\n')
		X[[n-i]] <- A[[m-i+1,n]] # getting values from last column
		print(A[[m-i+1,n]])
		print(X[[n-i]])
		for(j in 1:i){
			cat('j:',j,'\n')
			X[[n-i]] <- X[[n-i]] - A[[m-i+1,n-j]]*X[[n-j]]
			print(A[[m-i+1,n-j]])
			print(X[[n-i]])
		}
		X[[n-i]] <- X[[n-i]]/A[[m-i+1,n-i+1]]
		print(A[[m-i+1,n-j]])
		print(X[[n-i]])
		print(X)
		q()
	}
	X
}

# creating a augmented matrix (4x4 angular coeficients and 4x1 linear coeficients)
mat <- matrix(
	c(
		1, 2, 3, 1,
		4, 5, 6, 2,
		0, 8, 9, 3
	),nrow=3,ncol=4,byrow=TRUE
)

cat('Initial matrix:\n');print(mat);cat('\n\n')
mat <- elimination(mat)
cat('Eliminated matrix:\n');print(mat);cat('\n\n')
result(mat)
cat('Result matrix:\n');print(mat);cat('\n\n')
