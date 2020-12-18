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

#Gráficos XY
#Población#
p= ggplot(data=BASE, mapping = aes(x=Población,y=CasosTotales,color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Población")
p=p+geom_smooth(method = "lm",col="Orange")
p
p=p+facet_grid(Continente~.)
p

p= ggplot(data=BASE, mapping = aes(x=log(Población),y=log(CasosTotales),color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Población")
p=p+geom_smooth(method = "lm",col="Orange")
p
p=p+facet_grid(Continente~.)
p

pob=lm(data=BASE, log(CasosTotales)~log(Población))
par(mfrow=c(2,2))
plot(log(Población),log(CasosTotales),pch=19,xlab ="log(Población)",ylab = "log(Casos Totales)")
abline(pob,col="red",lwd=2)
plot(log(Población),residuals(pob),pch=19,xlab = "log(Población)",ylim= c(-6.5,6.5), ylab = "Residuos(Población)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(3.74,0),col="red",lwd=2,lty=2)
abline(c(-3.74,0),col="red",lwd=2,lty=2)
hist(residuals(pob),nclass = 20)
qqnorm(residuals(pob),pch=19)
qqline(residuals(pob),col="red",lwd=2,lty=2)

summary(pob)
