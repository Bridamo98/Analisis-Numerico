#include <iostream>
#include <math.h>
#include <stdlib.h>
#include <vector>
using namespace std;
double evaluarFuncion(double x)
{
    return (((9.8*68.1)/x)*(1-exp(-(x/68.1)*10))-40);
}
double taller(vector<double> limites, int particiones, double *error,double * recursiones)
{
    *recursiones=(*recursiones)+1;
    vector<double> Particiones ;
    double seccion= fabs((limites[0])-(limites[1]) )/particiones;
    Particiones.push_back(limites[0]);
    for (int i=0; i<particiones; i++)
    {
        Particiones .push_back(Particiones[i]+seccion);
    }

    vector <double>:: iterator iterador = Particiones.begin();
    for(; Particiones.end()!=iterador; iterador++)
    {
        cout<<*iterador<<" - ";
    }
    cout<<endl;
    for(int i=0; i<Particiones.size(); i++)
    {
        double lol=evaluarFuncion(Particiones[i]);

        if(lol<0)
        {
            lol*=-1;
        }
        if(lol<=0.000001)
        {
            *error= lol;
            return Particiones[i];
        }
    }
    double aux2=evaluarFuncion(Particiones[0]);
    for(int i=1; i<Particiones.size(); i++)
    {
        double aux1= evaluarFuncion(Particiones[i]);
        if((aux2 < 0 && aux1 < 0) || (aux2 > 0 && aux1 > 0))
        {
            aux2= aux1;
        }
        else
        {
            vector<double> aux3;
            aux3.push_back(Particiones[i-1]);
            aux3.push_back(Particiones[i]);
            return taller(aux3,particiones, error, recursiones);
        }
    }
}
int main()
{
    vector <double> limites;
    limites.push_back(10);
    limites.push_back(20);
    int cantParticiones= 3;
    double * recursiones=new double;
    *recursiones=0;
    double * error=new double;
    *error=0;
    cout<<"---3 secciones---"<<endl<<"Respuesta:" <<taller(limites, cantParticiones,error,recursiones)<<endl;
    cout<<"Error: "<<*error<<endl<<"# de recursiones: "<< *recursiones<<endl;
    cantParticiones=4;
    *recursiones=0;
    *error=0;
    cout<<"---4 secciones---"<< endl<<"Respuesta :"<<taller(limites, cantParticiones,error, recursiones)<<endl;
     cout<<"Error: "<<*error<<endl<<"# de recursiones: "<< *recursiones<<endl;
}
