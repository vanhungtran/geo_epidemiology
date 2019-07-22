# Modeling of infectious disease

## SIR model
(according Ottar N. Bjørnstad's program)

- Import packages
``` r
require(shiny)
require(deSolve)
require(phaseR)
```

## Define SIR model
``` r
 SIR=function(t, x, parms){
    S=x[1]
    I=x[2]
    R=x[3]
    
    beta=parms["beta"]
    mu=parms["mu"]
    gamma=parms["gamma"]
    N=parms["N"]
    dS = mu * (N - S) - beta * S * I / N
    dI = beta * S * I / N - (mu + gamma) * I
    dR = gamma * I - mu * R
    res=c(dS, dI, dR)
    list(res)
  }
```
 

## A example of SIR model

- Set parameters
``` r
times = seq(0, 10, by = 1/10)
parms = c(mu = 0, N = 1, beta = 2, gamma = 1/2)
start = c(S = 0.9, I = 0.09, R = 0.01)
```


``` r
out=ode(y=start, times=times, func=sirmod, parms=
          parms)
out=as.data.frame(out)
R0=parms["beta"]/(parms["gamma"]+parms["mu"])
```


-Plot
``` r
#Adjust margins to accommodate a second right axis
par(mar = c(5,5,2,5))
#Plot state variables
plot(x=out$time, y=out$S, ylab="Fraction", xlab="Time",type="l", col="green")
lines(x=out$time, y=out$I, col="red")
lines(x=out$time, y=out$R, col="blue")
#Add vertical line at turnover point
xx=out$time[which.max(out$I)]
lines(c(xx,xx), c(1/R0,max(out$I)), lty=3)
#prepare to superimpose 2nd plot
par(new=TRUE)
#plot effective reproductive ratio (w/o axes)
plot(x=out$time, y=R0*out$S, type="l", lty=2, lwd=2,col="black", axes=FALSE, xlab=NA, ylab=NA, ylim=c(-.5, 4.5))
lines(c(xx, 10), c(1,1), lty=3)
#Add right-hand axis for RE
axis(side = 4)
mtext(side = 4, line = 4, expression(R[E]))
#Add legend
legend("right", legend=c("S", "I", "R",expression(R[E])), lty=c(1,1,1, 2),col=c("green", "red", "blue", "black"))




```


![alt text](https://github.com/vanhungtran/modeling-of-infectious/blob/master/SIR.png)




