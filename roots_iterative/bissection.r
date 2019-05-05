bissec <- function(a,b,f,e){
    x0 <- (b+a)/2
    while(abs(f(x0)) > e){
        if(f(a)*f(x0)>0){
            a <- x0
        }else{
            b <- x0
        }
        x0 <- (b+a)/2
        cat(x0,'\n')
    }
    x0
}

func <- function(x){
   y <- x**3 -9*x +3
}

xm <- bissec(0,1,func,0.001)

x <- seq(0,1,0.01)

png('bissec.png')
plot(x,func(x),type='l')
points(xm,func(xm),col='red',pch=20)
