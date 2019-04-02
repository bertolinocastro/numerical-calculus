argmax <- function(s){
	m <- max(s)
	i <- 1
	while(s[[i]] != m)
		i <- i + 1
	i
}

swap <- function(A, x, y){
	cat('swap: x:',x,'y:',y,'\n')
	print(A)
	tmp <- A[x,]
	A[x,] <- A[y,]
	A[y,] <- tmp
	print(A)
	A
}

# seq wrapper
# creating a sequence generator similar to Python's range
# as R's "seq" isn't working properly.
sq <- function(from,to,by){
	ab <- NULL
	if((to-from)*by>0){ # creating sequence only if the step allows it
		ab <- seq(from, to, by)
		length(ab) <- length(ab) - 1 # discarding last index
	}
	ab
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
			A <- swap(A,i-1+i_max,i) # TODO: check this swap (isn't working)
			for(h in seq(i+1,m,1)){
				f <- A[[h,j]]/A[[i,j]]
				A[[h,j]] <- 0
				for(k in seq(j+1,n,1)){
					A[[h,k]] <- A[[h,k]] - A[[i,k]]*f
				}
			}
			cat('\n')
			print(A)
			i <- i + 1
			j <- j + 1
		}
	}
	A
}

result <- function(A){
	m <- nrow(A)
	n <- ncol(A)
	X <- matrix()

	for (i in m:1){
		X[[i]] <- (A[[i,n]] - sum(A[i,sq(m,i,-1)]*X[sq(m,i,-1)]))/A[[i,i]]
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

cat('\nInitial matrix:\n');print(mat);cat('\n\n')
mat <- elimination(mat)
cat('\nEliminated matrix:\n');print(mat);cat('\n\n')
xs <- result(mat)
cat('\nResulting Xs:\n');
for(i in 1:length(xs))
	cat(sprintf('X%d = %+f\n',i,xs[[i]]))
cat('\n\n')
