secant <- function(x0,x1,f,e){
    while(abs(f(x1))>e){
        x2 <- x1 - f(x1)/(f(x1)-f(x0))*(x1-x0)
        x0 <- x1
        x1 <- x2
    }
    x1
}

func <- function(x){
    x**3-5*x**2+x+3
}

xm <- secant(-2.44,-2.43,func,0.001)

x <- seq(-3,0,0.02)

png('secant.png')
plot(x,func(x),type='l')
points(xm,func(xm),col='red',pch=20)
