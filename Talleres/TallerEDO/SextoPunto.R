#Elaborado por: Laura Donado y Jhonny Parra
#2018

list.of.packages <- c("phaseR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(phaseR)

f<-function(fcn,x,y){
  return(eval(fcn))
}

# Solo para prueba con dy=x+y, y(0)=1
obtenerErrorAbsoluto<-function(x,y){
  solucion=exp(x)*(((x^2)*exp(-x))+(x*exp(-x))+1)
  return(abs(y-solucion))
}

graficarCampoPendiente<-function(x0, xn, y0, yn, fcn, numpendientes, metodo){
  apma1 <- function(t, y, parameters){
    a <- parameters[1] 
    dy <- a*(f(fcn, t, y))
    list(dy)
  } 
  apma1.flowField <- flowField(apma1, x = c(x0, xn), 
                               y   = c(y0, yn), parameters = c(1), 
                               points = numpendientes, system = "one.dim", 
                               add = FALSE, xlab = "x", ylab = "y", 
                               main = metodo)
  grid()
}

graficarSolucionNumerica<-function (x, y){
  points (x, y, pch=20, col="blue")
  for (i in 2:length(x)){
    segments(x[i-1], y[i-1], x[i], y[i], col="red")
  }
}

rk4<-function(dy, ti, tf, y0, h, graficar=TRUE, numpendientes=10){
  t<-seq(ti, tf, h)
  y<-c(y0)
  cat("x    |y         |k1        |k2        |k3        |k4       |error absoluto\n")
  for(i in 2:length(t)){
    k1=h*f(dy, t[i-1], y[i-1])
    k2=h*f(dy, t[i-1]+h/2, y[i-1]+k1*(0.5))
    k3=h*f(dy, t[i-1]+h/2, y[i-1]+k2*(0.5))
    k4=h*f(dy, t[i-1]+h, y[i-1]+k3)
    y<-c(y, y[i-1]+1/6*(k1+2*k2+2*k3+k4))
    cat(t[i-1]," | ", y[i-1]," | ",k1," | ",k2," | ",k3," | ",k4," | ",obtenerErrorAbsoluto(t[i-1],y[i-1]),"\n")
  }
  if (graficar){
    graficarCampoPendiente(min(t), max(t), min(y), max(y), dy, numpendientes, "RK4")
    graficarSolucionNumerica(t, y)
  }
  rta<-list(w=y, t=t)
}

rk3<-function(dy, ti, tf, y0, h, graficar=TRUE, numpendientes=10){
  t<-seq(ti, tf, h)
  y<-c(y0)
  cat("x    |y         |k1         |k2        |k3       |error absoluto\n")
  for(i in 2:length(t)){
    k1=h*f(dy, t[i-1], y[i-1])
    k2=h*f(dy, t[i-1]+h/2, y[i-1]+k1*(0.5))
    k3=h*f(dy, t[i-1]+h, y[i-1]-k1+2*k2)
    y<-c(y, y[i-1]+1/6*(k1+4*k2+k3))
    cat(t[i-1]," | ", y[i-1]," | ",k1," | ",k2," | ",k3," | ",obtenerErrorAbsoluto(t[i-1],y[i-1]),"\n")
  }
  if (graficar){
    graficarCampoPendiente(min(t), max(t), min(y), max(y), dy, numpendientes, "RK3")
    graficarSolucionNumerica(t, y)
  }
  rta<-list(w=y, t=t)
}

r<-rk4(expression(x+y+1-(x^2)), 0, 1, 1, 0.1)

r2<-rk3(expression(x+y+1-(x^2)), 0, 1, 1, 0.1)
