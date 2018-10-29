list.of.packages <- c("phaseR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(phaseR)
#PUNTO 3
evaluarDosTerminos<-function(expresion,x,y)
{
  return( eval(expresion))
}
sacarTerminoDeLaEcuacionDeTaylor<-function(expresion, n,x,y,h)
{
  derivadaAux=expresion
  derivadaAuy=expresion
  aux=0
  sumatoria=0
  while(aux<=n)
  {
    if(aux>1)
    {
      derivadaAux=D(derivadaAux,"x")
      derivadaAuy=D(derivadaAuy,"y")
      sumatoria=sumatoria+((h^(aux))/factorial(aux))*(eval(derivadaAux)+eval(derivadaAuy)*eval(expresion))
    }
    if(aux==1)
    {
      sumatoria=sumatoria+((h^(aux))/factorial(aux))*(eval(derivadaAux))  
    }
    if(aux==0)
    {
      sumatoria=sumatoria+y
    }
    #cat("Ene actual:", aux,"\n")
    # print(derivadaAux)
    #print(derivadaAuy)
    #if(aux==0)
    # cat("Iniciamos con: ", y,"\n") 
    #if(aux!=0)
    #cat("Funcion evaluada con un x de ",x," y un y de ",y," :",eval(derivadaAux),"\n") 
    # cat("T= ",sumatoria ,"\n")
    aux=aux+1
  }
  return (sumatoria)
}


errorDeTruncamiento<-function(expresion,n,xi,yi,h)
{
  derivadaAux=expresion
  derivadaAuy=expresion
  aux=n
  n=n+5
  sumatoria=0
  while(aux<=n)
  {
    if(aux>1)
    {
      derivadaAux=D(derivadaAux,"x")
      derivadaAuy=D(derivadaAuy,"y")
      sumatoria=sumatoria+((h^(aux))/factorial(aux))*(eval(derivadaAux)+eval(derivadaAuy)*eval(expresion))
    }
    if(aux==1)
    {
      sumatoria=sumatoria+((h^(aux))/factorial(aux))*(eval(derivadaAux))  
    }
    if(aux==0)
    {
      sumatoria=sumatoria+y
    }
    aux=aux+1
  }
  return (sumatoria)
}


euler<- function(expresion ,xi,yi,h,cantIteraciones)##n es el grado de la serie taylor
{
  t<-c()
  y<-c() 
  error=0
  aux=0
  while(aux<cantIteraciones)
  {
    funcionevaluada=sacarTerminoDeLaEcuacionDeTaylor(expresion,1,xi,yi,h)
    yDeiMasUno=funcionevaluada
    aux2=0
    cat("Punto numero: ",aux+1,"\n")
      yi=yDeiMasUno
      xi=xi+h
      t<-c(t,xi)
      y<-c(y,yi)
      cat("xi= ",xi,"yi= ",yi)
      error=errorDeTruncamiento(expresion,1,xi,yi,h)
      cat(" Error: ", error)
      cat("\n")
      aux2=aux2+1
    aux=aux+1
  }
  graficarCampoPendiente(min(t), max(t), min(y), max(y), expresion, 10, "EULER")
  graficarSolucionNumerica(t, y)
  return (yi)
}
###Con n se puede definir el grado de la serie de taylor, y con cantIteraciones la cantidad de iteraciones
###que imprime cada tupla de la salida en consola
expresion=expression(1-x^2+(x+y))##Aqui va la expresion en terminos de x y y
y=1
h=0.1
x=0
cantIteraciones=20
euler(expresion ,x,y,h,cantIteraciones)##llamada a la funcion de taylor