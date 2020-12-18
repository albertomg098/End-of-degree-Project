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

#Gráficos XY
#Camas#
p= ggplot(data=BASE, mapping = aes(x=Camas,y=CasosTotales,color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Camas")
p=p+geom_smooth(method = "lm",col="Orange")
p
p=p+facet_grid(Continente~.)
p

p= ggplot(data=BASE, mapping = aes(x=sqrt(Camas),y=log(CasosTotales),color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Camas")
p=p+geom_smooth(method = "lm",col="Orange")
p
p=p+facet_grid(Continente~.)
p

cam=lm(data=BASE, log(CasosTotales)~log(Camas))
par(mfrow=c(2,2))
plot(log(Camas),log(CasosTotales),pch=19,xlab ="log(Camas)",ylab = "log(Casos Totales)")
abline(cam,col="red",lwd=2)
plot(log(base$Camas),residuals(cam),pch=19, ylim=c(-7,7),xlab = "log(Camas)",ylab = "Residuos(Camas)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(2.376,0),col="red",lwd=2,lty=2)
abline(c(-2.376,0),col="red",lwd=2,lty=2)
hist(residuals(cam),nclass = 20)
qqnorm(residuals(cam),pch=19)
qqline(residuals(cam),col="red",lwd=2,lty=2)

cam=lm(data=BASE, log(CasosTotales)~sqrt(Camas))
par(mfrow=c(2,2))
plot(sqrt(Camas),log(CasosTotales),pch=19,xlab ="sqrt(Camas)",ylab = "log(Casos Totales)")
abline(cam,col="red",lwd=2)
plot(sqrt(Camas),residuals(cam),pch=19,xlab = "sqrt(Camas)",ylab = "Residuos(Camas)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(4.768,0),col="red",lwd=2,lty=2)
abline(c(-4.768,0),col="red",lwd=2,lty=2)
hist(residuals(cam),nclass = 20)
qqnorm(residuals(cam),pch=19)
qqline(residuals(cam),col="red",lwd=2,lty=2)

summary(cam)
