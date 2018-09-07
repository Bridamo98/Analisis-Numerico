jacobi<-function(t,x0, b, aDiagonal, i,error){
  cont<-0
  diagonalInversa<-solve(aDiagonal)
  diagonalInversa
  
  while(abs(x0[1]-x[1])<=error || abs(x0[2]-x[2])<=error || abs(x0[3]-x[3])<=error){
    
    r<-t%*%x0
    c<-diagonalInversa%*%b
    x<-r+c
    x0<-x
    cont<-cont+1
    if(i<cont){
      break
    }
  }
  print("Despues de ")
  print(cont)
  print("iteraciones")
  return (x0)
}



x0<-c(1,1,1)
b<-c(69,47,68)


a<-matrix(c(8,2,2,9,7,8,2,2,6),3)
a
as.vector(a)
diagonal<-diag(a)
diagonal
aDiagonal<-diag(diagonal , sqrt(length(a)) , sqrt(length(a)) )
i<-20
aDiagonal
solve(aDiagonal)

t<-matrix(solve(aDiagonal)%*%(aDiagonal-a),3,3)
t
info<-(eigen(t))
info
valoresPropios<-c(info['values'])

valoresPropios<-unlist(valoresPropios,use.names = FALSE)

valoresPropios

error<-0.0001
if(max(valoresPropios)<1 && max(valoresPropios*(-1))<1){
  
  x<-jacobi(t,x0, b,aDiagonal, i,error)
  print("EL MÉTODO CONVERGE AL VECTOR SOLUCIÓN...")
  print(x)
}else{
  print("EL MÉTODO NO CONVERGE PARA ESTE SISTEMA DE ECUACIONES")
}