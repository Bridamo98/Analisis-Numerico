import math
def evalularFuncion(x):
	return round(math.exp(-x)-x,8)
#end def
def muller(x0,x1,x2,error,iteraciones):
	iteraciones=iteraciones+1
	h0=x1-x0
	h1=x2-x1
	g0=(evalularFuncion(x1)-evalularFuncion(x0))/(x1-x0)
	g1=(evalularFuncion(x2)-evalularFuncion(x1))/(x2-x1)
	a=(g1-g0)/(h1-h0)
	b=(a*h1)+g1
	c=evalularFuncion(x2)
	if(b<0):
		x3=round(x2+((-1)*(2*c)/(b-((b**2-(4*a*c))**0.5))),8)
	else:
		x3=round(x2+((-1)*(2*c)/(b+((b**2-(4*a*c))**0.5))),8)
	#end if
	errorPorcentual=round(abs((x2-x3)/x2)*100,8)
	if(errorPorcentual<=error):
		print(iteraciones,"iteraciones")
		return x3
	else:
		return muller(x1,x2,x3,error,iteraciones)
	#end if
#end def

#main
x0=0.000000
x1=1.000000
x2=round((x0+x1)/2,6)
error=0.00000001
iteraciones=0
print("La solucion de f(x)=0 mediante el metodo de Muller es",str(muller(x0,x1,x2,error,iteraciones)))
#end main

