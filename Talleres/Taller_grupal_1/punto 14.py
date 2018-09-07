
def evaluarFuncion (x):
  return x+1;
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
	#print(separaciones)
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


#def main

intervalo=[]
intervalo.append(-10)
intervalo.append(10)
particiones=10
error=0.00001
respuesta=n_seccionar(intervalo, particiones,error)
print("Respuesta: "+str(respuesta))
print("Respuesta evaluada en f(x): "+str(evaluarFuncion (respuesta)))
#end def