#install.packages("phaseR")
install.packages("pracma")
#library(phaseR)
library(pracma)

#'PUNTO 1
metodoEuler <- function(f, h, xi, yi, xf)
{
  N = (xf - xi) / h
  x = y = numeric(N+1)
  x[1] = xi; 
  y[1] = yi;
  i = 1
  while (i <= N)
  {
    x[i+1] = x[i]+h
    y[i+1] = y[i]+(h*f(x[i],y[i]))
    i = i+1
  }
  
  return (data.frame(X = x, Y = y))
}

f <- function(x, y) {
  
  return((-5.6e-8*0.5*6*((y^4)-(200^4)))/100)
  }



e1 = metodoEuler(f, 0.5, 0, 180, 200)

e1[nrow(e1),]

xx <- c(0, 200); yy <- c(0, 200)
vectorfield(f, xx, yy, scale = 10)
for (xs in seq(0, 200, by = 10)) 
{
  sol <- rk4(f, 0, 200, xs, 100)
  lines(sol$x, sol$y, col="purple")
}




