part_argmax <- function(s){
	m <- max(abs(s))
	i <- 1
	while(abs(s[[i]]) != m)
		i <- i + 1
	i
}

comp_argmax <- function(s){
	m <- max(abs(s))
	for (i in 1:nrow(s))
		for (j in 1:ncol(s))
			if(abs(s[i,j]) == m)
				return(c(i,j))
}

swap_rows <- function(A, x, y){
	if(x!=y){
		cat(sprintf('L%d <-> L%d\n',x,y))
		tmp <- A[x,]
		A[x,] <- A[y,]
		A[y,] <- tmp
		print(A);cat('\n')
	}
	A
}

swap_cols <- function(A, x, y){
	if(x!=y){
		cat(sprintf('C%d <-> C%d\n',x,y))
		tmp <- A[,x]
		A[,x] <- A[,y]
		A[,y] <- tmp
		print(A);cat('\n')
	}
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
partial_elimination <- function(A){
	i <- 1
	j <- 1
	m <- nrow(A)
	n <- ncol(A)
	while(i < m && j < n){
		i_max <- part_argmax(A[i:m,j])
		if(A[[i_max,j]] == 0)
			j <- j + 1
		else{
			A <- swap_rows(A,i-1+i_max,i) # TODO: check this swap (isn't working)
			for(h in seq(i+1,m,1)){
				f <- A[[h,j]]/A[[i,j]]
				A[[h,j]] <- 0
				for(k in seq(j+1,n,1)){
					A[[h,k]] <- A[[h,k]] - A[[i,k]]*f
				}
				cat(sprintf('L%d <- L%d - L%d*(A[%d,%d]/A[%d,%d])\n',h,h,i,h,j,i,j))
			}
			print(A); cat('\n')
			i <- i + 1
			j <- j + 1
		}
	}
	A
}

complete_elimination <- function(A){
	i <- 1
	j <- 1
	m <- nrow(A)
	n <- ncol(A)
	while(i < m && j < n){
		i_max <- comp_argmax(A[sq(i,m+1,1),sq(j,n,1)])
		A <- swap_rows(A,i_max[[1]]-1+i,i)
		A <- swap_cols(A,i_max[[2]]-1+j,j)

		# if(A[[i_max,j]] == 0)
		# 	j <- j + 1
		# else{
			# A <- swap(A,i-1+i_max,i) # TODO: check this swap (isn't working)
			for(h in seq(i+1,m,1)){
				f <- A[[h,j]]/A[[i,j]]
				A[[h,j]] <- 0
				for(k in seq(j+1,n,1)){
					A[[h,k]] <- A[[h,k]] - A[[i,k]]*f
				}
				cat(sprintf('L%d <- L%d - L%d*(A[%d,%d]/A[%d,%d])\n',h,h,i,h,j,i,j))
			}
			print(A); cat('\n')
			i <- i + 1
			j <- j + 1
		# }
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
mat0 <- matrix(
	c(
		1, 2, 3, 1,
		4, 5, 6, 2,
		-8, 8, 9, 3
	),nrow=3,ncol=4,byrow=TRUE
)

cat('\nInitial matrix:\n');print(mat0);cat('\n')
mat <- partial_elimination(mat0)
cat('\nPartially eliminated matrix:\n');print(mat);cat('\n')
xs <- result(mat)
cat('\nResulting Xs:\n');
for(i in 1:length(xs))
	cat(sprintf('X%d = %+f\n',i,xs[[i]]))
cat('\n\n')

# -------------
cat('# -------------\n\n')


cat('\nInitial matrix:\n');print(mat0);cat('\n\n')
mat <- complete_elimination(mat0)
cat('\nCompletely eliminated matrix:\n');print(mat);cat('\n\n')
xs <- result(mat)
cat('\nResulting Xs:\n');
for(i in 1:length(xs))
	cat(sprintf('X%d = %+f\n',i,xs[[i]]))
cat('\n\n')
