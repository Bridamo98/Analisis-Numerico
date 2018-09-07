def funcionDerivada(vector,ekis):
	resultado=0
	grado=len(vector)-1
	for n in range(0,len(vector)-1):
		if (vector[n]!=0):
			if(n==0):
				resultado=(resultado+vector[n])*grado
					
			else:
				resultado=grado*vector[n]+ekis*resultado
				
			#end if
			grado=grado-1
		#end if
	#end for
	return resultado
#end def

def funcion(vector,ekis):
	resultado=0
	for n in range(0,len(vector)):
		if (vector[n]!=0):
			if(n==0):
				resultado=resultado+vector[n]
				
			else:
				resultado=vector[n]+ekis*resultado
				
			#end if
		#end if
	#end for
	return resultado
#end def
def newton_raphson(vector,error,x1):
	x2=round(x1-(funcion(vector, x1)/funcionDerivada(vector,x1)),8)
	if(round(abs(x2-x1),8)<=error):
		return x2
	else:
		return newton_raphson(vector, error, x2)


def evaluarVolumen(x,y,l):
	return round(( (y-2*l)*(x-2*l)*l),8)
#end def
def secionar(intervalo, particiones):
	fin = []
	seccion = round((intervalo[1]-intervalo[0])/particiones, 8)
	fin.append(round(intervalo[0],8))
	for i in range(0, particiones-1):
		fin.append(fin[i]+seccion)
	#end for
	fin.append(intervalo[1])
	#print(fin)
	return fin
#end def
def n_seccionar(intervalo, particiones, vol,error,x,y):
	separaciones = secionar(intervalo, particiones)
	#caso Base----------------------------------------------------
	for i in range(0, len(separaciones)):
		if(round(abs((evaluarVolumen(x,y,separaciones[i])-vol) ), 8)<= error and evaluarVolumen(x,y,separaciones[i])<vol):
			return separaciones[i]
		#end if			
	#end for
	valor = evaluarVolumen(x,y,separaciones[0])
	valor = round(valor, 8)
	for i in range(1, len(separaciones)):
		
		valorAux = evaluarVolumen(x,y,separaciones[i])
		valorAux = round(valorAux, 8)
		if((valor < vol and valorAux < vol) or (valor > vol and valorAux > vol)):
			valor = valorAux
		else:
			nuevoIntervalo = []
			nuevoIntervalo.append(separaciones[i-1])
			nuevoIntervalo.append(separaciones[i])
			return n_seccionar(nuevoIntervalo, particiones, vol, error, x, y)
#end def
x=32.00000
y=24.00000
vol=1000.00000
error=0.0001
intervalo = [-10, 20]

print("La respuesta mediante el método de n-sección es:",n_seccionar(intervalo, 2,vol, error, x, y))
vector=[4,(-1)*(2*y+2),y*x,-1000]
print("La respuesta mediante el método de Newton-raphson es:",newton_raphson(vector, error, 5))

