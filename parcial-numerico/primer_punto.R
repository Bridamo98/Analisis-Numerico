matriz<-matrix(c(2,5,1,7,4,6,7,8,9),3)
length(matriz)
matriz[1,2]
contFilas<-1
suma<-0
while(contFilas<=length(matriz)/3){
  contColumnas<-1
  while(contColumnas<=length(matriz)/3){
    
    suma<-suma+matriz[contColumnas,contFilas]
    contColumnas<-contColumnas+1
  }
  contFilas<-contFilas+1
}
print("para la matriz: ")
matriz
print("se tiene la siguiente suma")
print(suma)
#######################################RESPUESTA A PREGUNTAS TEXTUALES################################################################################
#PARA ESTE ALGORITMO SE TIENE QUE LA COMPLEJIDAD ES DE O(n^2), ya que esta es la complejidad del recorrido de una matriz n*n, 
