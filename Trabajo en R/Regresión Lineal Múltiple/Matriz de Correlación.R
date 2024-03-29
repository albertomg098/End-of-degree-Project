#Introducimos nuestra base de datos en R#
file.choose()
ruta= "C:\\Users\\alber\\OneDrive\\Documentos\\TFG\\Buena Base de datos\\M�ximos+Variables Explicativas.xlsx"
excel_sheets(ruta)
names(base)
base=read_excel(ruta)
casos=read_excel(ruta,sheet = "Hoja2")
attach(base)
library(tidyverse)
BASE=filter(base,Continente!='Ocean�a')
attach(BASE)

#REALIZAMOS LA MATRIZ DE CORRELACI�N#
install.packages("corrplot")
library(corrplot)
install.packages("ggplot2")
BASE2=BASE

BASE2$Pa�s=NULL
BASE2$Regi�n=NULL
BASE2$Continente=NULL
BASE2$`Pa�ses TE`=NULL
BASE2$CasosTotales=log(BASE$CasosTotales)
BASE2$PIB=log(BASE$PIB)
BASE2$Poblaci�n=log(BASE$Poblaci�n)
BASE2$Camas=log(BASE$Camas)
BASE2$Natalidad=log(BASE$Natalidad)
BASE2$DeudaP�blica=log(BASE$DeudaP�blica+500)
BASE2$GastoSalud=log(BASE$GastoSalud)

names(BASE2)
names(BASE2)=c("log(CasosTotales)","Contagios D�a","Muertes","MuertesD�a","Recuperados","Activos","log(PIB)","VarPIB","log(Poblaci�n)","sqrt(Camas)","log(Natalidad)","log(DeudaP�blica)","log(GastoSalud)")

correlacion= cor(BASE2,method = "pearson")
round(correlacion,digits = 2)
corrplot(correlacion)
BASE
