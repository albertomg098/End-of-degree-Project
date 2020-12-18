#Introducimos nuestra base de datos en R#
file.choose()
ruta= "C:\\Users\\alber\\OneDrive\\Documentos\\TFG\\Buena Base de datos\\Máximos+Variables Explicativas.xlsx"
excel_sheets(ruta)
names(base)
base=read_excel(ruta)
casos=read_excel(ruta,sheet = "Hoja2")
attach(base)


#REALIZAMOS LOS GRÁFICOS XY#

#PIB#
p= ggplot(data=base, mapping = aes(x=log(PIB),y=CasosTotales,col=Continente))
p= p+geom_point()
p=p+ggtitle("XY PIB")
p=p+geom_smooth(method = "lm",col="Orange")
p

pib=lm(data=base, CasosTotales~log(PIB))
par(mfrow=c(2,2))
plot(log(PIB),CasosTotales,pch=19,xlab ="log(PIB)",ylab = "Casos Totales",ylim = c(-1e+06,4.5e+06))
abline(pib,col="red",lwd=2)
plot(log(base$PIB),residuals(pib),pch=19, ylim=c(-800000,4e+06),xlab = "log(PIB)",ylab = "Residuos(PIB)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(755600,0),col="red",lwd=2,lty=2)
abline(c(-755600,0),col="red",lwd=2,lty=2)
hist(residuals(pib),nclass = 20)
qqnorm(residuals(pib),pch=19)
qqline(residuals(pib),col="red",lwd=2,lty=2)

outPIB=abs(residuals(pib))>755600
pib2=lm(CasosTotales[!outPIB]~log(PIB[!outPIB]))

summary(PIB)


#VarPIB#
p= ggplot(data=base, mapping = aes(x=VarPIB,y=CasosTotales,color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Var PIB")
p=p+geom_smooth(method = "lm",col="Orange")
p
p=p+facet_grid(Continente~.)
p

var=lm(data=base, CasosTotales~VarPIB)
par(mfrow=c(2,2))
plot(VarPIB,CasosTotales,pch=19,xlab ="Variación PIB",ylab = "Casos Totales",ylim=c(-1e+06,5e+06))
abline(var,col="red",lwd=2)
plot(base$VarPIB,residuals(var),pch=19, ylim=c(-850000,5e+06),xlab = "Variación PIB",ylab = "Residuos(VarPIB)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(822600,0),col="red",lwd=2,lty=2)
abline(c(-822600,0),col="red",lwd=2,lty=2)
hist(residuals(var),nclass = 20)
qqnorm(residuals(var),pch=19)
qqline(residuals(var),col="red",lwd=2,lty=2)

summary(var)

#Población#
p= ggplot(data=base, mapping = aes(x=log(Población),y=CasosTotales,color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Población")
p=p+geom_smooth(method = "lm",col="Orange")
p
p=p+facet_grid(Continente~.)
p

pob=lm(data=base, CasosTotales~log(Población))
par(mfrow=c(2,2))
plot(log(Población),CasosTotales,pch=19,xlab ="log(Población)",ylab = "Casos Totales",ylim = c(-1e+06,4.5e+06))
abline(pob,col="red",lwd=2)
plot(log(base$Población),residuals(pob),pch=19, ylim=c(-800000,4.5e+06),xlab = "log(Población)",ylab = "Residuos(Población)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(780200,0),col="red",lwd=2,lty=2)
abline(c(-780200,0),col="red",lwd=2,lty=2)
hist(residuals(pob),nclass = 20)
qqnorm(residuals(pob),pch=19)
qqline(residuals(pob),col="red",lwd=2,lty=2)

summary(pob)


#Camas#
p= ggplot(data=base, mapping = aes(x=log(Camas),y=CasosTotales,color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Camas")
p=p+geom_smooth(method = "lm",col="Orange")
p
p=p+facet_grid(Continente~.)
p

cam=lm(data=base, CasosTotales~log(Camas))
par(mfrow=c(2,2))
plot(log(Camas),CasosTotales,pch=19,xlab ="log(Camas)",ylab = "Casos Totales",ylim = c(-1e+06,4.5e+06))
abline(cam,col="red",lwd=2)
plot(log(base$Camas),residuals(cam),pch=19, ylim=c(-850000,4.5e+06),xlab = "log(Camas)",ylab = "Residuos(Camas)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(822200,0),col="red",lwd=2,lty=2)
abline(c(-822200,0),col="red",lwd=2,lty=2)
hist(residuals(cam),nclass = 20)
qqnorm(residuals(cam),pch=19)
qqline(residuals(cam),col="red",lwd=2,lty=2)

summary(cam)

#Natalidad#
p= ggplot(data=base, mapping = aes(x=log(Natalidad),y=CasosTotales,color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Natalidad")
p=p+geom_smooth(method = "lm",color="Orange")
p
p=p+facet_grid(Continente~.)
p

nat=lm(data=base, CasosTotales~log(Natalidad))
par(mfrow=c(2,2))
plot(log(base$Natalidad),base$CasosTotales,pch=19,xlab ="log(Natalidad)",ylab = "Casos Totales",ylim = c(-1e+06,4.5e+06))
abline(nat,col="red",lwd=2)
plot(log(base$Natalidad),residuals(nat),pch=19, ylim=c(-850000,4.5e+06),xlab = "log(Natalidad)",ylab = "Residuos(Natalidad)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(819800,0),col="red",lwd=2,lty=2)
abline(c(-819900,0),col="red",lwd=2,lty=2)
hist(residuals(nat),nclass = 20)
qqnorm(residuals(nat),pch=19)
qqline(residuals(nat),col="red",lwd=2,lty=2)

summary(nat)

#Deuda Pública#
p= ggplot(data=base, mapping = aes(x=DeudaPública,y=CasosTotales,color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Deuda Pública")
p
p=p+geom_smooth(method = "lm")
p=p+facet_grid(Continente~.)
p

deu=lm(data=base, CasosTotales~DeudaPública)
par(mfrow=c(2,2))
plot(base$DeudaPública,base$CasosTotales,pch=19,xlab ="Deuda Pública",ylab = "Casos Totales",ylim = c(-1e+06,4.5e+06))
abline(deu,col="red",lwd=2)
plot(base$DeudaPública,residuals(deu),pch=19, ylim=c(-850000,4.5e+06),xlab = "DeudaPública",ylab = "Residuos(DeudaPública)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(822200,0),col="red",lwd=2,lty=2)
abline(c(-822200,0),col="red",lwd=2,lty=2)
hist(residuals(deu),nclass = 20)
qqnorm(residuals(deu),pch=19)
qqline(residuals(deu),col="red",lwd=2,lty=2)

summary(deu)

#GastoSalud#
p= ggplot(data=base, mapping = aes(x=1/GastoSalud,y=CasosTotales,color=Continente))
p= p+geom_point()
p=p+ggtitle("XY Gasto en Salud")
p
p=p+geom_smooth(method = "lm")
p=p+facet_grid(Continente~.)
p

gast=lm(data=base, CasosTotales~log(GastoSalud))
par(mfrow=c(2,2))
plot(log(base$GastoSalud),base$CasosTotales,pch=19,xlab ="Gasto en Salud",ylab = "Casos Totales",ylim = c(-1e+06,4.5e+06))
abline(gast,col="red",lwd=2)
plot(log(base$GastoSalud),residuals(gast),pch=19, ylim=c(-850000,4.5e+06),xlab = "Gasto en Salud",ylab = "Residuos(GastoSalud)")
abline(c(0,0),col="red",lwd=2,lty=2)
abline(c(808200,0),col="red",lwd=2,lty=2)
abline(c(-808200,0),col="red",lwd=2,lty=2)
hist(residuals(gast),nclass = 20)
qqnorm(residuals(gast),pch=19)
qqline(residuals(gast),col="red",lwd=2,lty=2)


summary(gast)