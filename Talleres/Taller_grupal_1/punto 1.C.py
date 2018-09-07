class operaciones(object):
	def __init__(self):
		self.contador=0
	#end def
	def incrementar(self,numero):
		self.contador=self.contador+numero
	#end def
	def reiniciar(self):
		self.contador=0
	#end def
	def getContador(self):
		return self.contador
	#end def
#end class

def funcionDerivada(vector,ekis,cantOperaciones):
	resultado=0
	grado=len(vector)-1
	for n in range(0,len(vector)-1):
		if (vector[n]!=0):
			if(n==0):
				resultado=(resultado+vector[n])*grado
				cantOperaciones.incrementar(2)	
			else:
				resultado=grado*vector[n]+ekis*resultado
				cantOperaciones.incrementar(3)	
			#end if
			grado=grado-1
		#end if
	#end for
	return resultado
#end def

def funcion(vector,ekis,cantOperaciones):
	resultado=0
	for n in range(0,len(vector)):
		if (vector[n]!=0):
			if(n==0):
				resultado=resultado+vector[n]
				cantOperaciones.incrementar(1)
			else:
				resultado=vector[n]+ekis*resultado
				cantOperaciones.incrementar(2)
			#end if
		#end if
	#end for
	return resultado
#end def

#def main
cantOperaciones=operaciones()
vector =[-5, 0, 3, 0, 2, -4, 0]
ekis= -1
print ("Para los coeficientes: ")
print(vector)

print("la funcion evauluada cuando x="+str(ekis)+" es igual a: "+str(funcion(vector,ekis,cantOperaciones))+"\n con un numero de operaciones de: "+ str(cantOperaciones.getContador()))
cantOperaciones.reiniciar()
print("la derivada de la funcion evauluada cuando x="+str(ekis)+" es igual a: "+str(funcionDerivada(vector,ekis,cantOperaciones))+"\n con un numero de operaciones de: "+str(cantOperaciones.getContador()))
#end main