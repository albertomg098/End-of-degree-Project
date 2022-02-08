
#Librerías
library(devtools)
library(readxl)
library(tidyverse)
library(lubridate)
library(tseries)
library(astsa)
library(forecast)
library(foreign)
library(timsac)
library(vars)
library(mFilter)
library(dynlm)
library(nlme)
library(quantmod)
library(xts)
library(urca)
library(car)
library(tsoutliers)
library(fracdiff)
library(autoarima)

##INTRODUCCIÓN DE DATOS##
file=file.choose()
ruta="C:\\Users\\alber\\OneDrive\\Documentos\\TFG\\Buena Base de datos\\Base Series Temporales.xlsx"
base=read_excel(ruta)
base_españa_entera=filter(base,base$País=='España')


##CORRECCIÓN Y LIMPIEZA DE DATOS##
base_sp=filter(base_españa_entera,base_españa_entera$Fecha>"2020-02-27")

base_sp_cmedia=base_sp
k=0
posicion=c(0,0)
for (i in 1:nrow(base_sp_cmedia)) {
  if(base_sp_cmedia[i,5]<0){
    base_sp_cmedia[i,5]=(base_sp_cmedia[i-3,5]+base_sp_cmedia[i-2,5]+base_sp_cmedia[i-1,5]+base_sp_cmedia[i+1,5]+base_sp_cmedia[i+2,5]+base_sp_cmedia[i+3,5])/6
    k=k+1
    posicion[k]=i
  }
}




##CREACIÓN DE SERIES DE TIEMPO##
serie_sin_correc=zoo(base_sp$`Nº de Contagios en el día`,base_sp$Fecha)
infect_spain_cmdia=zoo(base_sp_cmedia$`Nº de Contagios en el día`,base_sp_cmedia$Fecha)
plot(infect_spain_cmdia,ylab="Nº Infectados",main="Serie con Corrección por Media")
plot(serie_sin_correc)

##COMPROBACIÓN de si es estacionaria (Prueba de Dickey-Fuller)##
adf.test(infect_spain_cmdia,alternative = "stationary")





##TRANSFORMACIÓN A ESTACIONARIA##

#Método del logaritmo para estacionalidad (NO FUNCIONA)#
serielog=log(infect_spain_cmdia+500)
par(mfrow=c(1,1))
plot(serielog)
adf.test(serielog,alternative = "stationary")

#Aplicando solo diferenciación#
ndiffs(infect_spain_cmdia)
infect_cmedia_difer=diff(infect_spain_cmdia)
plot(infect_cmedia_difer)
adf.test(infect_cmedia_difer,alternative = "stationary")

#Aplicando la diferenciación al logaritmo#
ndiffs(serielog)
serielog_difer=diff(serielog)
serielog_difer_7=diff(serielog_difer,lag=7)

par(mfrow=c(1,3))
plot(serielog)
plot(serielog_difer,main="1 Diferencia")
plot(serielog_difer_7,main="1 Diferencia + Diferencia a 7 Días")
adf.test(serielog_difer_7,alternative = "stationary")


#Representación de los difeerentes métodos#
par(mfrow=c(2,2))
plot(serielog,main="Logaritmo")
plot(infect_cmedia_difer,main="Diferencias")
plot(serielog_difer,main="Logaritmo + Diferencias")
plot(infect_spain_cmdia)






###MODELO ARIMA###

##Estimación ordenes del modelo##

#Usando la serie logaritmo-diferenciación (L-D)#


par(mfrow=c(2,3))
plot(serielog_difer,main = "Serie LD")
acf(serielog_difer,main="Función de Autocorrelación Simple SLD")
pacf(serielog_difer, main="Función de Autocorrelación Parcial SLD ")
n=c(4,1,3)


par(mfrow=c(1,3))
plot(serielog_difer_7,main = "Serie LD a 7 Días")
acf(serielog_difer_7,lag.max=42,main="Función de Autocorrelación Simple SLD")
pacf(serielog_difer_7,lag.max=42, main="Función de Autocorrelación Parcial SLD ")



#Usando la serie con solo diferenciación(D)#
par(mfrow=c(1,3))
plot(infect_cmedia_difer,main = "Serie D")
acf(infect_cmedia_difer,frequency=1,main="Función de Autocorrelación Simple SD")
pacf(infect_cmedia_difer,frequency=1, main="Función de Autocorrelación Parcial SD")
n=c(6,1,3)


##Creación del modelo##

#Modelo para Serie L-D# OJO, no cuadra si ponemos que el orden de las medias móviles es 7, como realmente es#
modelo_serie_LD=arima(serielog,order = c(4,1,3))
summary(modelo_serie_LD)
modelo_serie_LD

#Modelo para Serie D#
modelo_serie_D=arima(infect_spain_cmdia,order = c(6,1,3))
summary(modelo_serie_D)

###DIAGNOSIS DEL MODELO###

##Modelo Serie L-D##
#Ruido Blanco#
tsdiag(modelo_serie_LD)
Box.test(residuals(modelo_serie_LD),type = "Ljung-Box")

#Homocedasticidad
error_LD=residuals(modelo_serie_LD) 
par(mfrow=c(1,1))
plot(error_LD, main="Residuos Serie LD") 
abline(c(0,0),col="red",lwd=2,lty=2)

#Independencia#
par(mfrow=c(1,2))
acf(error_LD,main="Autocorrelación Simple de Residuos LD")
pacf(error_LD,main="Autocorrelación Parcial de Residuos LD")

#Normalidad
qqnorm(error_LD, main= "Normalidad Residuos Serie LD")
qqline(error_LD)
ks.test(error_LD, pnorm, mean(error_LD), sd(error_LD))


##Modelo Serie D##
#Ruido Blanco#
tsdiag(modelo_serie_D)
Box.test(residuals(modelo_serie_D),type = "Ljung-Box")

#Homocedasticidad#
error_D=residuals(modelo_serie_D)
plot(error_D,main="Residuos Serie D") #Homocedasticidad#
abline(c(0,0),col="red",lwd=2,lty=2)

#Normalidad
qqnorm(error_D,main="Normalidad Residuos Serie D")
qqline(error_D)
ks.test(error_D, pnorm, mean(error_D), sd(error_D))



##PRONÓSTICO##
#Modelo de Serie  LD#
pronostico_LD=forecast::forecast(modelo_serie_LD,h=30)
plot(pronostico_LD)

predict(modelo_serie_LD,10)
futurVal_LD = forecast(modelo_serie_LD, h=10, level=c(95.0))
plot(futurVal_LD)
print(futurVal_LD)


#Modelo de Serie D#
pronostico_D=forecast::forecast(modelo_serie_D,h=30)
plot(pronostico_D)
pronostico_D

predict(modelo_serie_D,10)
futurVal_D = forecast(modelo_serie_D, h=10, level=c(95.0))
plot(futurVal_D)
print(futurVal_D)


  
  