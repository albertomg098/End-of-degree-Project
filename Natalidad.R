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

#Gráficos Xy
#Natalidad#
p= ggplot(data=BASE, mapping = aes(x=Natalidad,y=CasosTotales,color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Natalidad")
p=p+geom_smooth(method = "lm",color="Orange")
p
p=p+facet_grid(Continente~.)
p

p= ggplot(data=BASE, mapping = aes(x=log(Natalidad),y=log(CasosTotales),color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Natalidad")
p=p+geom_smooth(method = "lm",color="Orange")
p
p=p+facet_grid(Continente~.)
p

nat=lm(data=BASE, log(CasosTotales)~log(Natalidad))
par(mfrow=c(2,2))
plot(log(Natalidad),log(CasosTotales),pch=19,xlab ="log(Natalidad)",ylab = "log(Casos Totales)")
abline(nat,col="red",lwd=2)
plot(log(Natalidad),residuals(nat),pch=19, ylim=c(-7,7),xlab = "log(Natalidad)",ylab = "Residuos(Natalidad)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(4.634,0),col="red",lwd=2,lty=2)
abline(c(-4.634,0),col="red",lwd=2,lty=2)
hist(residuals(nat),nclass = 20)
qqnorm(residuals(nat),pch=19)
qqline(residuals(nat),col="red",lwd=2,lty=2)

summary(nat)
