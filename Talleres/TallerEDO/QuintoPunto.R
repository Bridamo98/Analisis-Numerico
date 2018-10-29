list.of.packages <- c("phaseR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(phaseR) 
##euler PUNTO 5

evaluarDosTerminos<-function(expresion,x,y)
{
  return( eval(expresion))
}


euler<- function(expresion, h, xi, yi,cantIteraciones)
{
  t<-c()
  y<-c()
  cat("Metodo de euler normal: \n")
  aux=0
  while(aux<cantIteraciones-1)
  {
  funcionevaluada=evaluarDosTerminos(expresion,xi,yi)
  yDeiMasUno=yi+h*funcionevaluada
  cat("Resultado punto x= ", xi," y yi=",yi," :",yDeiMasUno,"\n")
  yi=yDeiMasUno
  xi=xi+h
  t<-c(t,xi)
  y<-c(y,yi)
  aux=aux+1
  }
  
  graficarCampoPendiente(min(t), max(t), min(y), max(y), expresion, 10, "EULER")
  graficarSolucionNumerica(t, y)
  
  
}

promedioPendientes<- function(expresion, h, xi, yi,cantIteraciones)
{
  aux=0
  t<-c()
  y<-c()
  cat("\nMetodo del promedio de pendientes: \n")
  while(aux<cantIteraciones-1)
  {
    funcionevaluada=evaluarDosTerminos(expresion,xi,yi)
    yDeiMasUno=yi+h*funcionevaluada
    funcionevaluada2=evaluarDosTerminos(expresion,xi+h,yDeiMasUno)
    yDeiMasUno=yi+(h/2)*(funcionevaluada2+funcionevaluada)
    cat("Resultado punto x= ", xi," y yi=",yi," :",yDeiMasUno,"\n")
    yi=yDeiMasUno
    xi=xi+h
    t<-c(t,xi)
    y<-c(y,yi)
  
    aux=aux+1
  }
  graficarCampoPendiente(min(t), max(t), min(y), max(y), expresion, 10, "PROMEDIO DE PENDIENTES")
  graficarSolucionNumerica(t, y)
  
}


expresion<- expression(x^2+y)
h<-0.1
xi<-1
yi<-0
cantIteraciones=10
#euler(expresion,h,xi,yi,cantIteraciones)

promedioPendientes(expresion, h, xi, yi,cantIteraciones)