#Introducimos nuestra base de datos en R#
file.choose()
ruta= "C:\\Users\\alber\\OneDrive\\Documentos\\TFG\\Buena Base de datos\\Máximos+Variables Explicativas.xlsx"
excel_sheets(ruta)
names(base)
base=read_excel(ruta)
casos=read_excel(ruta,sheet = "Hoja2")
attach(base)
library(tidyverse)
BASE=filter(base,Continente!='Oceanía')
attach(BASE)

#REALIZAMOS LA MATRIZ DE CORRELACIÓN#
install.packages("corrplot")
library(corrplot)
install.packages("ggplot2")
BASE2=BASE

BASE2$País=NULL
BASE2$Región=NULL
BASE2$Continente=NULL
BASE2$`Países TE`=NULL
BASE2$CasosTotales=log(BASE$CasosTotales)
BASE2$PIB=log(BASE$PIB)
BASE2$Población=log(BASE$Población)
BASE2$Camas=log(BASE$Camas)
BASE2$Natalidad=log(BASE$Natalidad)
BASE2$DeudaPública=log(BASE$DeudaPública+500)
BASE2$GastoSalud=log(BASE$GastoSalud)

names(BASE2)
names(BASE2)=c("log(CasosTotales)","Contagios Día","Muertes","MuertesDía","Recuperados","Activos","log(PIB)","VarPIB","log(Población)","sqrt(Camas)","log(Natalidad)","log(DeudaPública)","log(GastoSalud)")

correlacion= cor(BASE2,method = "pearson")
round(correlacion,digits = 2)
corrplot(correlacion)
BASE
