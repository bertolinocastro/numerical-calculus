newton <- function(x0,f,fp,e){
    while(abs(f(x0))>e){
        x0 <- x0 - f(x0)/fp(x0)
    }
    x0
}

func <- function(x){
    x**3-5*x**2+x+3
}

fprime <- function(x){
    3*x**2-10*x+1
}

xm <- newton(-2.44,func,fprime,0.001)

x <- seq(-3,0,0.02)

png('newton.png')
plot(x,func(x),type='l')
points(xm,func(xm),col='red',pch=20)
