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
#Deuda Pública#
DeudaPública2=DeudaPública+500

p= ggplot(data=BASE, mapping = aes(x=DeudaPública2,y=CasosTotales,color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Deuda Pública")
p=p+geom_smooth(method = "lm", col="Orange")
p

p= ggplot(data=BASE, mapping = aes(x=log(DeudaPública2),y=log(CasosTotales),color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Deuda Pública")
p=p+geom_smooth(method = "lm", col="Orange")
p
p=p+facet_grid(Continente~.)
p


deu=lm(data=BASE, log(CasosTotales)~log(DeudaPública2))
par(mfrow=c(2,2))
plot(log(DeudaPública2),log(CasosTotales),pch=19,xlab ="log(Deuda Pública)",ylab = "log(Casos Totales)")
abline(deu,col="red",lwd=2)
plot(log(DeudaPública2),residuals(deu),pch=19, ylim=c(-8.5,8.5),xlab = "log(DeudaPública)",ylab = "Residuos(DeudaPública)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(4.806,0),col="red",lwd=2,lty=2)
abline(c(-4.806,0),col="red",lwd=2,lty=2)
hist(residuals(deu),nclass = 20)
qqnorm(residuals(deu),pch=19)
qqline(residuals(deu),col="red",lwd=2,lty=2)

summary(deu)


Deudaública2
DeudaPública
