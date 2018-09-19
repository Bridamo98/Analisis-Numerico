#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MAXCHAR 1024 /* read 1024 bytes at a time */
size_t strlen(const char * str)
{
    const char *s;
    for (s = str; *s; ++s);
    return(s - str);
}
struct punto
{
	int x;
	int y;
};
struct punto* minimo (struct punto* punto[1000], int contador)
{
	struct punto aux;
    float xaux,yaux;
    int i=0;
    xaux=punto[i]->x;
    yaux=punto[i]->y;
    i++;
    for( ;i<contador; i++)
    {
        if(xaux>punto[i]->x)
        {
            xaux=punto[i]->x;
        }
        if(yaux>punto[i]->y)
        {
            yaux=punto[i]->y;
        }
	}
	struct  punto* retorno= (struct punto*)malloc(sizeof(struct punto));
	retorno->x=xaux;
	retorno->y= yaux;
	return  retorno;
}
struct punto* maximo(struct punto* punto[1000], int contador)
{
	struct punto aux;
    float xaux,yaux;
    int i=0;
    xaux=punto[i]->x;
    yaux=punto[i]->y;
    i++;
    for( ;i<contador; i++)
    {
        if(xaux<punto[i]->x)
        {
            xaux=punto[i]->x;
        }
        if(yaux<punto[i]->y)
        {
            yaux=punto[i]->y;
        }
	}
	struct  punto* retorno= (struct punto*)malloc(sizeof(struct punto));
	retorno->x=xaux;
	retorno->y= yaux;
	return  retorno;
}
char verSiHayPunto(struct punto* puntos[1000], int fila, int columna, int contador,FILE* fa)
{
	for(int i=0; i<contador;i++)
	{
		if(puntos[i]->x==columna && puntos[i]->y==fila)
		{
			fprintf (fa,"(%d,",columna);
			fprintf (fa,"%d),",fila);
		}
	}
	fprintf (fa,".");
}

void organizarPuntos (struct punto* puntos[1000], int contador)
{
	struct punto* puntoMaximo= maximo (puntos,contador);
	struct punto* puntoMinimo = minimo (puntos,contador);
	puntoMaximo->x= puntoMaximo->x - puntoMinimo->x;
	puntoMaximo->y= puntoMaximo->y - puntoMinimo->y;
	char matriz[puntoMaximo->y][puntoMaximo->x];
	FILE* fa= fopen("chotas.txt", "w");
	for (int i=130; i>0; i--)
	{
		for (int ii=1;ii<201;ii++ )
		{
			verSiHayPunto( puntos,  i,ii, contador, fa);
		}
		fprintf(fa,"\n");
	}
	fclose(fa);
}

int verSiEsCercano( struct punto* Punto,struct  punto* puntos[1000] , int contador)/// 0 false 1 true
{
	if(contador==0)
	{
		return 1;
	}
 	for (int i=0; i<contador; i++)
 	{
 		float aux =sqrt( (Punto->x-puntos[i]->x)*(Punto->x-puntos[i]->x) +   (Punto->y-puntos[i]->y)*(Punto->y-puntos[i]->y));
 		printf("%9.6f \n",aux);
 		if(aux<0)
 		{
 			return 0;
 		}
 	}
 	return 1;
}

void hallarCoordenadas(int* contador2,struct punto* puntos[1000], char* linea, int contador);
int main(int argc, char const *argv[])
{ 
	FILE *fp;
    char* str=(char*)malloc(sizeof(char));
    char* filename = "pato.txt";
 	int contador=130;
 	int contador2=0;
 	struct punto* puntos[1000];
    fp = fopen(filename, "r");
    if (fp == NULL){
        printf("Could not open file %s",filename);
        return 1;
    }
    while (fgets(str, MAXCHAR, fp) != NULL){
        hallarCoordenadas(&contador2,puntos,str,contador);       
        contador=contador-2;
    }
    printf("%d\n", contador2);
    fclose(fp);
    organizarPuntos (puntos,contador2);

    return 0;
}
void hallarCoordenadas( int* contador2,struct punto* puntos[1000],char* linea, int contador){

	FILE *xx = fopen("x.txt", "a");
	FILE *yy = fopen("y.txt", "a");
	struct punto* aux=NULL;
	for(int i=0;i< strlen(linea);i++){
		if(i!=strlen(linea)-1){
			if(i>0){
				if(linea[i]!='M' && linea[i-1]=='M'){

					aux=(struct punto*)malloc(sizeof( struct punto));
					aux->x=i;
					aux->y=contador;

					if(verSiEsCercano( aux,puntos ,*contador2)==1)
					{
						puntos[*contador2]=	aux;
						(*contador2)=(*contador2)+1;
						fprintf(xx, "%d, ",i); 
						fprintf(yy, "%d, ",contador);
					}
				}else if(linea[i]=='M' && linea[i-1]!='M'){
						aux=(struct punto*)malloc(sizeof( struct punto));
						aux->x=i;
						aux->y=contador;	
						puntos[*contador2]=	aux;
						if(verSiEsCercano( aux,puntos ,*contador2)==1)
						{
							puntos[*contador2]=	aux;
							(*contador2)=(*contador2)+1;
							fprintf(xx, "%d, ",i-1); 
							fprintf(yy, "%d, ",contador);
						}
						
					}
			}else{
				if(linea[i]!='M'){
					aux=(struct punto*)malloc(sizeof( struct punto));
					aux->x=i;
					aux->y=contador;	
						if(verSiEsCercano( aux,puntos ,*contador2)==1)
						{
							puntos[*contador2]=	aux;
							(*contador2)=(*contador2)+1;
							fprintf(xx, "%d, ",i-1); 
							fprintf(yy, "%d, ",contador);
						}
				}
			}
		}	
	}
	fclose(xx);
	fclose(yy);
}