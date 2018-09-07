jacobi<-function(t,x0, b, aDiagonal, iteraciones){
  cont<-0
  diagonalInversa<-solve(aDiagonal)
  diagonalInversa
  while(cont<iteraciones){

    r<-t%*%x0
    c<-diagonalInversa%*%b
    x<-r+c
    print(x)
    x0<-x
    
    cont<-cont+1
  }
  return (x0)
}



x0<-c(1,1)
b<-c(11,13)


a<-matrix(c(2,5,1,7),2)
a
as.vector(a)
diagonal<-diag(a)
diagonal
aDiagonal<-diag(diagonal , sqrt(length(a)) , sqrt(length(a)) )
aDiagonal
solve(aDiagonal)

t<-matrix(solve(aDiagonal)%*%(aDiagonal-a),2,2)
t
info<-(eigen(t))
info
valoresPropios<-c(info['values'])

valoresPropios<-unlist(valoresPropios,use.names = FALSE)

valoresPropios
if(max(valoresPropios)<1 && max(valoresPropios*(-1))<1){
  iteraciones<-25
  x<-jacobi(t,x0, b,aDiagonal, iteraciones)
  print("EL MÉTODO CONVERGE AL VECTOR SOLUCIÓN...")
  print(x)
}else{
  print("EL MÉTODO NO CONVERGE PARA ESTE SISTEMA DE ECUACIONES")
}





