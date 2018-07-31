

def funcion(vector,ekis):
	resultado=0
	for n in range(0,len(vector)):
		if(n==0):
			resultado=resultado+vector[n]
		else:
			resultado=vector[n]+ekis*resultado

		#end if
	#end for
	return resultado
#end def

#def main
vector =[10, 20, 30, 5,0]
ekis= 8
print ("Para los coeficientes: ")
print(vector)
print("la funcion evauluada cuando x="+str(ekis)+" es igual a: "+str(funcion(vector,ekis))+"\n con un numero de operaciones de: "+ str(ekis*2))

#end main