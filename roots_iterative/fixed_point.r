fixed_point <- function(x0,f,fp,e){
    while(abs(f(x0))>e){
        x0 <- fp(x0)
    }
    x0
}

func <- function(x){
    x**2 + x - 6
}

phi <- function(x){
    sqrt(6-x)
}

xm <- fixed_point(1.5,func,phi,0.001)

x <- seq(1,3,0.02)

png('fixed.png')
plot(x,func(x),type='l')
lines(x,phi(x),type='l',col='blue')
points(xm,phi(xm),col='red',pch=20)
