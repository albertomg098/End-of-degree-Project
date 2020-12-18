#Introducimos nuestra base de datos en R#

library(ggplot2)
library(readxl)
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

#GRAFICOS XY
#PIB#
p= ggplot(data=BASE, mapping = aes(x=PIB,y=CasosTotales,col=Continente))
p= p+geom_point()
p=p+ggtitle("XY PIB")
p=p+geom_smooth(method = "lm",col="Orange")
p

p= ggplot(data=BASE, mapping = aes(x=log(PIB),y=log(CasosTotales),col=Continente))
p= p+geom_point()
p=p+ggtitle("XY PIB")
p=p+geom_smooth(method = "lm",col="Orange")
p

pib=lm(data=BASE, log(CasosTotales)~log(PIB))
par(mfrow=c(2,2))

plot(log(PIB),log(CasosTotales),pch=19,xlab ="log(PIB)",ylab = "log(Casos Totales)")
abline(pib,col="red",lwd=2)
plot(log(PIB),residuals(pib),pch=19,ylim=c(-6,6), xlab = "log(PIB)",ylab = "Residuos(PIB)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(3.222,0),col="red",lwd=2,lty=2)
abline(c(-3.222,0),col="red",lwd=2,lty=2)
hist(residuals(pib),nclass = 20)
qqnorm(residuals(pib),pch=19)
qqline(residuals(pib),col="red",lwd=2,lty=2)

outPIB=abs(residuals(pib))>755600
pib2=lm(CasosTotales[!outPIB]~log(PIB[!outPIB]))
summary(pib2)
par(mfrow=c(2,2))
plot(pib2)
plot(log(PIB[!outPIB]),CasosTotales[!outPIB],pch=19,xlab ="log(PIB)",ylab = "Casos Totales",ylim = c(-1e+06,4.5e+06))
abline(pib,col="red",lwd=2)
plot(log(PIB[!outPIB]),residuals(pib2),pch=19, ylim=c(-200000,8e+05),xlab = "log(PIB)",ylab = "Residuos(PIB)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(185960,0),col="red",lwd=2,lty=2)
abline(c(-185960,0),col="red",lwd=2,lty=2)

summary(pib)

#Comprobación homocedasticidad
fligner.test(log(CasosTotales)~log(PIB))
