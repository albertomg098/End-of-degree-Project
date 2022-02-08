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
#GastoSalud#
p= ggplot(data=BASE, mapping = aes(x=GastoSalud,y=CasosTotales,color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Gasto en Salud")
p=p+geom_smooth(method = "lm",col="Orange")
p

p= ggplot(data=BASE, mapping = aes(x=log(GastoSalud),y=log(CasosTotales),color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Gasto en Salud")
p=p+geom_smooth(method = "lm",col="Orange")
p
p=p+facet_grid(Continente~.)
p

gast=lm(data=BASE, log(CasosTotales)~log(GastoSalud))
par(mfrow=c(2,2))
plot(log(GastoSalud),log(CasosTotales),pch=19,xlab ="log(Gasto en Salud)",ylab = "log(Casos Totales)")
abline(gast,col="red",lwd=2)
plot(log(GastoSalud),residuals(gast),pch=19, ylim=c(-7,7),xlab = "log(Gasto en Salud)",ylab = "Residuos(GastoSalud)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(4.736,0),col="red",lwd=2,lty=2)
abline(c(-4.736,0),col="red",lwd=2,lty=2)
hist(residuals(gast),nclass = 20)
qqnorm(residuals(gast),pch=19)
qqline(residuals(gast),col="red",lwd=2,lty=2)


summary(gast)
