#Introducimos nuestra base de datos en R#
file.choose()
ruta= "C:\\Users\\alber\\OneDrive\\Documentos\\TFG\\Buena Base de datos\\Máximos+Variables Explicativas.xlsx"
excel_sheets(ruta)
names(base)
base=read_excel(ruta)
casos=read_excel(ruta,sheet = "Hoja2")
library(tidyverse)
BASE=filter(base,base$Continente!='Oceanía')
attach(BASE)
DeudaPública2=DeudaPública+500


#REGRESIÓN MÚLTIPLE SIN CUALITATIVAS
mrmultiple=lm(log(CasosTotales)~log(PIB)+log(Población)+log(GastoSalud))
mrmultiple2=lm(log(CasosTotales)~Continente+log(GastoSalud)+log(Natalidad)+log(Población)+log(PIB))

summary(mrmultiple)
par(mfrow=c(2,2))
plot(mrmultiple)

log(2.718281828)

summary(mrmultiple2)

#Diagnosis Residuos Vs Regresores
par(mfrow=c(2,2))

plot(residuals(mrmultiple)~log(PIB), ylab = "Residuos",pch=19,ylim=c(-4.75,4.75))
abline(c(0,0),col="red",lwd=2)
abline(c(3.1,0),col="red",lwd=2,lty=2)
abline(c(-3.1,0),col="red",lwd=2,lty=2)

plot(residuals(mrmultiple)~log(Población), ylab = "Residuos",pch=19,ylim=c(-4.75,4.75))
abline(c(0,0),col="red",lwd=2)
abline(c(3.1,0),col="red",lwd=2,lty=2)
abline(c(-3.1,0),col="red",lwd=2,lty=2)

plot(residuals(mrmultiple)~log(GastoSalud), ylab = "Residuos",pch=19, ylim=c(-4.75,4.75))
abline(c(0,0),col="red",lwd=2)
abline(c(3.1,0),col="red",lwd=2,lty=2)
abline(c(-3.1,0),col="red",lwd=2,lty=2)


#Diagnosis
par(mfrow=c(1,1))

plot(residuals(mrmultiple)~predict(mrmultiple), ylab = "Residuos",pch=19, ylim=c(-4.75,4.75))
abline(c(0,0),col="red",lwd=2)
abline(c(3.1,0),col="red",lwd=2,lty=2)
abline(c(-3.1,0),col="red",lwd=2,lty=2)

par(mfrow=c(1,2))
hist(residuals(mrmultiple),nclass = 20)
qqnorm(residuals(mrmultiple),pch=19)
qqline(residuals(mrmultiple),col="red",lwd=2,lty=2)
