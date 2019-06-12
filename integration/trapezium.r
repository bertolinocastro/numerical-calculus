trapezio <- function(f,p){
    (p[[2]]-p[[1]])*(f(p[[2]])+f(p[[1]]))/2.
}

# f <- function(x){return(sqrt(x-1))}
# trapezio(f,seq(1,6,by=1))

trapezio(cos,c(0,pi/2.))
