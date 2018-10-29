list.of.packages <- c("phaseR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(phaseR) 
#PUNTO 4

expresion <-expression (1-x^2+(x+y))
xi=0
yi=1
h=0.1
m=20
aux=0
t<-c()
ye<-c()
while(aux<m)
{
  x=xi
  y=yi
  k1=h*eval(expresion)
  x=xi+h
  y=yi+k1
  k2=h*eval(expresion)
  t<-c(t,xi)
  ye<-c(ye,yi)
  cat("Resultado del punto con xi= ",xi, " y yi= ",yi, " es: ")
  yi=yi+(1/2)*(k1+k2)
  cat(yi,"\n")
  xi=xi+h
  
  
  aux=aux+1
}
print(length(ye))
print(length(t))
print(ye)
print(t)
graficarCampoPendiente(min(t), max(t), min(ye), max(ye), expresion, 10, "EULER")
graficarSolucionNumerica(t, ye)

