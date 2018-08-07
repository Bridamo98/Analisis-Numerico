#include <iostream>
#include <math.h>
#include <stdlib.h>
#include <vector>
#include <iomanip>
using namespace std;
double evaluarFuncion(double x)//FUNCION PARA EVALUAR LA FUNCION DESCRITA EN EL PUNTO X
{
    return (((9.8*68.1)/x)*(1-exp(-(x/68.1)*10))-40);
}
double taller(vector<double> limites, double particiones, double *error,double * recursiones, double xrAnterior, double* errorMasUno)
{
    *recursiones=(*recursiones)+1;//SUMAR EN 1 EL NUMERO DE ITERACIONES(REECURSIONES)
    vector<double> Particiones ;//CONTENEDOR DE LAS NUEVAS PARTICIONES
    double seccion= (fabs((limites[0])-(limites[1]) )/particiones);//TAMAÑO DE CADA NUEVA PARTICION
    cout<<"seccion "<<seccion<<endl;
    double errorPorcentual ;//VARIABLE QUE TENDRÁ EL ERROR PORCENTUAL DE LA ITERACIÓN ACTUAL
    double xr=(limites[0] + limites[1])/2.000000;
    Particiones.push_back(limites[0]);//EL PRIMER LIMITE DEL CONTENEDOR DE PARTICIONES ES IGUAL AL LIMITE INFERIOR ESPECIFICADO PARA ESTA ITERACION
    for (int i=0; i<particiones; i++)//CALCULAR LOS NUEVOS PUNTOS DEL CONTENEDOR DE PARTICIONES
    {
        Particiones .push_back(Particiones[i]+seccion);//--------------------------------------------
    }
    vector <double>:: iterator iterador = Particiones.begin();//IMPRIMIR POR CONSOLA LAS NUEVAS PARTICIONES PARA UN SEGUIMIENTO DEL PROCESO
    for(; Particiones.end()!=iterador; iterador++)//-------------------------------------------------
    {
        cout<<*iterador<<" - ";//------------------------------------------------------------------
    }
    cout<<endl;
    //--------------------esto lo puso briam------------------------------------------
    if(xrAnterior!=0.00000)
    {
        errorPorcentual = fabs((xr-xrAnterior/xr)*100.00000);
        cout<<"ERROR PORCENTUAL ---"<<setprecision(5)<<errorPorcentual<<endl; 
    }
    /*

    CASO BASE DE LA FUNCIÓN RECURSIVA

    */
    for(int i=0; i<Particiones.size(); i++)//-----------------------------------------------------
    {
        double lol=evaluarFuncion(Particiones[i]);//EVALULAR LA FUNCIÓN EN CADA PUNTO DE LAS PARTICIONES        
        if(lol<=0.000001 && lol>=-0.000001)//LO QUE VA DESPUES DE && LO AGREGÓ BRIAM, VALIDANDO SI EL VALOR DE LA FUNCIÓN YA SE ACERCÓ A CERO LO SUFICIENTE
        {
            *errorMasUno=errorPorcentual;//AL SER LA ULTIMA ITERACION(IT) EL ERROR PORCENTUAL DE LA SIGUIENTE IT ES EL ERROR DE LA ACTUAL IT(NO EXISTE IT SIGUIENTE)
             *error= lol;//SE LE DA VALOR AL RESULTADO FINAL DE f(x) QUE SE ACERCA MÁS A CERO    
            return Particiones[i];//se retorna el x del f(x) ANTERIOR
        }
    }
    /*

    PASO RECURSIVO

    */
    double aux2=evaluarFuncion(Particiones[0]);//AUXILIAR PARA PODER REALIZAR LA COMPARACIÓN ENTRE LAS PARTICIONES CONSECUTIVAS
    for(int i=1; i<Particiones.size(); i++)
    {
        double aux1= evaluarFuncion(Particiones[i]);//EVALUAR LA FUNCION EN LOS PUNTOS DE LAS PARTICIONES MENOS EL PRIMERO
        if((aux2 < 0 && aux1 < 0) || (aux2 > 0 && aux1 > 0))//f(x) y f(x-1) tienen el mismo signo, EN EL INTERVALO NO SE ENCUENTRA LA RAÍZ
        {
            aux2= aux1;//PARA PODER REVISAR EL SIGUIENTE INTERVALO EL LIMITE SUPERIOR DE ESTA ITERACION PASA A SER EL LIMITE INFERIOR LA SIGUIENTE ITERACIÓN
        }
        else
        {
            vector<double> aux3;//SE CREA UN CONTENEDOR QUE GUARDARA LOS NUEVOS LIMITES DE LA SIGUIENTE PARTICIÓN
            aux3.push_back(Particiones[i-1]);//SE LES DA LOS VALORES A LOS LIMITES INFERIOR Y SUPERIOR OBTENIDOS ANTERIORMENTE
            aux3.push_back(Particiones[i]);//------------------------------------------------------------------------------
            float recActual=*recursiones;//SE CREA UNA COPIA DEL NUMERO DE LAS RECURSIONES LLEVADAS HASTA EL MOMENTO
            xrAnterior=xr;//ESTO LO AGREGO BRIAM
            double aux4 =taller(aux3,particiones, error, recursiones,xrAnterior,errorMasUno);//SE REALIZA EL LLAMADO RECURSIVO
            if(*recursiones!=1)
            {
                double k=(*errorMasUno)/errorPorcentual;
                cout<<" La convergencia para la iteracion "<<recActual<<" es "<<setprecision(6)<<k<<" con error Ei="<<errorPorcentual<<" y "<<" E(i+1) "<<*errorMasUno<<endl;
                *errorMasUno= errorPorcentual;
            }
            return aux4;
        }
    }
}
int main()
{
    vector <double> limites;//GUARDA LOS PUNTOS DE CORTE DE LAS SECCIONES
    limites.push_back(10);//SE ESTABLECE EL PUNTO DE CORTE INFERIOR
    limites.push_back(20);//SE ESTABLECE EL PUNTO DE CORTE SUPERIOR
    double cantParticiones= 40.00000;//SE ESTABLECE LA CANTIDAD DE PARTICIONES
    double * recursiones=new double;//VARIABLE PARA LLEVAR EL CONTEO DE LAS RECURSIONES Y PODER CONTARLAS AL FINAL
    *recursiones=0;// INICIALIZACIÓN DEL CONTADOR DE RECURSIONES EN 0
    double * error=new double;//TENDRÁ EL VALOR DEL ERROR ABSOLUTO DEL PROCESO(EL VALOR DE f(x) MÁS CERCANO A 0 QUE SE LOGRÓ ENCONTRAR)
    *error=0;//SE INICIALIZA PARA EVITAR INCONSISTENCIAS
    double* errorMasUno= new double;//PARA GUARDAR EL ERROR
    *errorMasUno=0.0000000;
    cout<<"---3 secciones---"<<endl<<"Respuesta:" <<taller(limites, cantParticiones,error,recursiones, 0.00000,errorMasUno)<<endl;
    cout<<"Error: "<<*error<<endl<<"# de recursiones: "<< *recursiones<<endl;
    cantParticiones=4.000000;
    *recursiones=0;
    *error=0;
    *errorMasUno=0.0000000;
    cout<<"---4 secciones---"<< endl<<"Respuesta :"<<taller(limites, cantParticiones,error, recursiones,0.00000,errorMasUno)<<endl;
    cout<<"Error: "<<*error<<endl<<"# de recursiones: "<< *recursiones<<endl;
}
