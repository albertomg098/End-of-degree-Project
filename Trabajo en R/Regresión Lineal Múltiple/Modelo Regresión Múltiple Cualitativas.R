#Introducimos nuestra base de datos en R#
file.choose()
ruta= "C:\\Users\\alber\\OneDrive\\Documentos\\TFG\\Buena Base de datos\\Máximos+Variables Explicativas.xlsx"
excel_sheets(ruta)
names(base)
base=read_excel(ruta)
casos=read_excel(ruta,sheet = "Hoja2")
library(tidyverse)
BASE=filter(base,Continente!='Oceanía')
attach(BASE)
DeudaPública2=DeudaPública+500


#REGRESIÓN MÚLTIPLE CON CUALITATIVAS
names(BASE)
mrmultiple2=lm(log(CasosTotales)~ Continente+log(PIB)+log(Población)+sqrt(Camas)+log(Natalidad)+log(GastoSalud))

summary(mrmultiple2)
Continente

par(mfrow=c(2,2))

plot(residuals(mrmultiple2)~predict(mrmultiple2),ylim=c(-5,5),pch=19)
abline(c(0,0),col="red",lwd=2)
abline(c(3.094,0),col="red",lwd=2,lty=2)
abline(c(-3.094,0),col="red",lwd=2,lty=2)

par(mfrow=c(1,2))
hist(residuals(mrmultiple2),nclass = 20)
qqnorm(residuals(mrmultiple2),pch=19)
qqline(residuals(mrmultiple2),col="red",lwd=2,lty=2)