import math
def evaluarFuncion(t):
	t=math.radians(t)
	print("t:",str(t/math.pi))
	#print(round(math.cos(3*t)+math.exp(t),8))
	print("r1",str(math.cos(3*t)+2))
	print("r2",str(2-math.exp(t)))
	print("suma",round((math.cos(3*t)+2)+(2-math.exp(t)),8))
	return round((math.cos(3*t)+2)+(2-math.exp(t)),8)
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
def n_seccionar(intervalo, particiones,error):
	separaciones = secionar(intervalo, particiones)
	print(separaciones)
	#caso Base----------------------------------------------------
	for i in range(0, len(separaciones)):
		if(round(abs(evaluarFuncion(separaciones[i])),8)<=error):

			return separaciones[i]
		#end if			
	#end for
	valor = evaluarFuncion(separaciones[0])
	valor = round(valor, 8)
	for i in range(1, len(separaciones)):
		
		valorAux = evaluarFuncion(separaciones[i])
		valorAux = round(valorAux, 8)
		if((valor < 0 and valorAux < 0) or (valor > 0 and valorAux > 0)):
			valor = valorAux
		else:
			nuevoIntervalo = []
			nuevoIntervalo.append(separaciones[i-1])
			nuevoIntervalo.append(separaciones[i])
			return n_seccionar(nuevoIntervalo, particiones,error)
#end def

error=0.0001
intervalo = [0, 360]
respuesta=n_seccionar(intervalo, 2, error)
respuesta=math.radians(respuesta)
print("La respuesta mediante el método de n-sección es:",respuesta)
print("1",str(math.cos(3*respuesta)+2))
print("2",str(2-math.exp(respuesta)))

