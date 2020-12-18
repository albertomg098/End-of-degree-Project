#Introducimos nuestra base de datos en R#
file.choose()
ruta= "C:\\Users\\alber\\OneDrive\\Documentos\\TFG\\Buena Base de datos\\Máximos+Variables Explicativas.xlsx"
excel_sheets(ruta)
names(base)
base=read_excel(ruta)
casos=read_excel(ruta,sheet = "Hoja2")
attach(base)

#Gráficos XY
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
plot(var)
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

outVar=abs(residuals(var))>822600
var2=lm(CasosTotales[!outVar]~VarPIB[!outVar])
summary(var2)
par(mfrow=c(2,2))
plot(var2)


