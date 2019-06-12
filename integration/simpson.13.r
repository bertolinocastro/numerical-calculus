simpson.13 <- function(f,p){
    h <- (p[[2]]-p[[1]])/2
    h/3*(f(p[[1]])+4*f(p[[1]]+h)+f(p[[2]]))
}

simpson.13(cos,c(0,pi/2.))
