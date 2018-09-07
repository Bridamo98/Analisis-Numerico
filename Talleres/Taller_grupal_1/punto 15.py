import math
def evaluarfuncionF(x):
	return round(5*x-math.exp(x)-1,8)
def evaluarfuncionG(x):
	print(round((((math.exp(x)/5)+1/5)),8))
	return(round((((math.exp(x)/5)+1/5)),8))
#end def
def puntoFijo(xInicial,numIteraciones,contIteraciones):
	if(numIteraciones>=contIteraciones):
		xInicial=evaluarfuncionG(xInicial)
		contIteraciones=contIteraciones+1
		return(puntoFijo(xInicial,numIteraciones,contIteraciones))
	else:
		return xInicial
				
#end def

#def main
numIteraciones=5

contIteraciones=1
xInicial=(0+1)/2
valor=0
resultado=puntoFijo(xInicial,numIteraciones,contIteraciones)
print("El resultado es",resultado)
print("La funcion evaluada en el punto es:",evaluarfuncionF(resultado))
					




#end main