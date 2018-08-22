f<-function(x){return(log(x+2))}

g<-function(x){return(sin(x))}

funcionResultante<-function(x){return(f(x)-g(x))}#SE CONSTRUYE ESTA ECUACIÓN PARA PODER HALLAR EL PUNTO DE INTERSECCIÓN

algoritmo<-function(x0,x1,tolerancia){
  x2<-(x1-(funcionResultante(x1)/(funcionResultante(x1)-funcionResultante(x0)))*(x1-x0))
  if(abs(x2-x1)<=tolerancia){
    return (x2)
  }else if(funcionResultante(x2)*funcionResultante(x1)<0){
    return(algoritmo(x1,x2))
  }else{
    return (algoritmo(x0,x2))
  }
}

erorr<-0.0000001
#PASO 1 para f

x0<-(-2)
x1<-(-1)

if(funcionResultante(x0)*funcionResultante(x1)<0){
  print("El intervalo si cumple la condición")
  algoritmo(x0,x1,erorr)
  
}else if(funcionResultante(x0)*funcionResultante(x1)>0){
  print("El intervalo no cumple la condición")
  
}else{
  print("La raiz es uno de los intervaloSs")
}

