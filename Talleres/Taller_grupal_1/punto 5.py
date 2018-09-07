
import math
#def main
v=4
Ev=0.1
t=5
Et=0.1

distanciaMayor=(v+Ev)*(t+Et)
distanciaMenor=(v-Ev)*(t-Et)

realIzquierda= v*t
realIncertidumbre=t*Ev+v*Et
print(realIzquierda,"+-",realIncertidumbre)

errorRelativoSuperior= realIzquierda-distanciaMayor
errorRelativoInferior= realIzquierda-distanciaMenor 

print("Error relativo superior:", errorRelativoSuperior,"+-",realIncertidumbre)
print ("Error relativo inferior:", errorRelativoInferior,"+-",realIncertidumbre)

errorPorcentualEnteraSuperior= errorRelativoSuperior*(1/realIzquierda)
errorPorcentualIncertidumbreSuperior= distanciaMayor*realIncertidumbre


errorPorcentualEnteraInferior=errorRelativoInferior*(1/realIzquierda)
errorPorcentualIncertidumbreInferior=distanciaMenor*realIncertidumbre
print ("\nError porcentual superior:",errorPorcentualEnteraSuperior,"+-",errorPorcentualIncertidumbreSuperior)
print ("Error porcentual inferior:", errorPorcentualEnteraInferior,"+-",errorPorcentualIncertidumbreInferior)
#end def









