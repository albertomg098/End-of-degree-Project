#Librer�as
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

##INTRODUCCI�N DE DATOS##
file=file.choose()
ruta="C:\\Users\\alber\\OneDrive\\Documentos\\TFG\\Buena Base de datos\\Base Series Temporales.xlsx"
base=read_excel(ruta)
base_espa�a_entera=filter(base,base$Pa�s=='Espa�a')


##CORRECCI�N Y LIMPIEZA DE DATOS##
base_sp=filter(base_espa�a_entera,base_espa�a_entera$Fecha>"2020-02-27")


base_sp_cmedia=base_sp
k=0
posicion=c(0,0)
for (i in 1:nrow(base_sp_cmedia)) 
  if(base_sp_cmedia[i,5]<0){
    base_sp_cmedia[i,5]=(base_sp_cmedia[i-3,5]+base_sp_cmedia[i-2,5]+base_sp_cmedia[i-1,5]+base_sp_cmedia[i+1,5]+base_sp_cmedia[i+2,5]+base_sp_cmedia[i+3,5])/6
    k=k+1
    posicion[k]=i
  }
}




##CREACI�N DE SERIES DE TIEMPO##

infect_spain_cmdia=zoo(base_sp_cmedia$`N� de Contagios en el d�a`,base_sp_cmedia$Fecha)
inf_cmedia_limpiados=tsclean(infect_spain_cmdia)


#Representaci�n de las diferentes Series con los Datos Corregidos#
par(mfrow=c(2,2))
plot(infect_spain,ylab="Infectados diarios", main="Serie Sin Correcci�n")
plot(tsclean(infect_spain),main="Serie Limpiados con Funci�n")
plot(infect_spain_cmdia,ylab="N� Infectados",main="Serie con Correcci�n por Media")
plot(inf_cmedia_limpiados,ylab="N� Infectados",main="Serie Limpiada")

ndiffs(inf_cmedia_limpiados)
nsdiffs(inf_cmedia_limpiados)

##COMPROBACI�N de si es estacionaria (Prueba de Dickey-Fuller)##
adf.test(infect_spain_cmdia,alternative = "stationary")


##TRANSFORMACI�N A ESTACIONARIA##

#M�todo del logaritmo para estacionalidad (NO FUNCIONA)#
serielog=log(inf_cmedia_limpiados+500)
plot(serielog)
adf.test(serielog,alternative = "stationary")

#Aplicando solo diferenciaci�n#
ndiffs(inf_cmedia_limpiados)
infect_cmedia_difer=diff(inf_cmedia_limpiados)
plot(infect_cmedia_difer)
adf.test(infect_cmedia_difer,alternative = "stationary")

#Aplicando la diferenciaci�n al logaritmo#
ndiffs(serielog)
serielog_difer=diff(serielog)
serielog_difer_7=diff(serielog_difer,lag=7)

par(mfrow=c(1,2))
plot(serielog_difer, main="Serie Transfromada")
plot(serielog_difer_7)
adf.test(serielog_difer,alternative = "stationary")



auto.arima(serielog)

#Representaci�n de los difeerentes m�todos#
par(mfrow=c(2,2))
plot(serielog,main="Logaritmo")
plot(infect_cmedia_difer,main="Diferencias")
plot(serielog_difer,main="Logaritmo + Diferencias")
plot(infect_spain_cmdia)


#Comparaci�n series no estacionaria y estacionaria#
par(mfrow=c(2,2))
plot(infect_spain_cmdia,ylab="N� Contagios", main="Serie no estacionaria")
acf(infect_spain_cmdia)
plot(serielog_difer,ylab="N� Contagios",main="Serie estacionaria")
acf(serielog_difer)



###MODELO ARIMA###

##Estimaci�n ordenes del modelo##

#Usando la serie logaritmo-diferenciaci�n (L-D)#


par(mfrow=c(1,2))
plot(serielog_difer,main = "Serie LD")
acf(serielog_difer,main="Funci�n de Autocorrelaci�n Simple SLD")
pacf(serielog_difer, main="Funci�n de Autocorrelaci�n Parcial SLD ")
n=c(4,1,3)



par(mfrow=c(1,3))
plot(serielog_difer_7,main = "Serie LD a 7 D�as")
acf(serielog_difer_7,main="Funci�n de Autocorrelaci�n Simple SLD")
pacf(serielog_difer_7, main="Funci�n de Autocorrelaci�n Parcial SLD ")



par(mfrow=c(1,3))
plot(diff(serielog_difer_7,lag = 6),main = "Serie LD a 7*6 D�as")
acf(diff(serielog_difer_7,lag = 6),frequency=1,lag.max=56,main="Funci�n de Autocorrelaci�n Simple SLD")
pacf(diff(serielog_difer_7,lag = 6),frequency=1,lag.max=42, main="Funci�n de Autocorrelaci�n Parcial SLD ")

#Usando la serie con solo diferenciaci�n(D)#
par(mfrow=c(1,3))
plot(infect_cmedia_difer,main = "Serie D")
acf(infect_cmedia_difer,frequency=1,main="Funci�n de Autocorrelaci�n Simple SD")
pacf(infect_cmedia_difer,frequency=1, main="Funci�n de Autocorrelaci�n Parcial SD")
n=c(6,1,3)


##Creaci�n del modelo##

#Modelo para Serie L-D# OJO, no cuadra si ponemos que el orden de las medias m�viles es 7, como realmente es#
modelo_serie_LD=arima(serielog,order = c(4,1,3))
summary(modelo_serie_LD)
modelo_serie_LD




mod0=arima(serielog,order = c(0,1,1),seasonal = list(order=c(0,1,2),period=7))
summary(mod0)
mod0

par(mfrow=c(1,2))
error=residuals(mod0) 
acf(error)
pacf(error)
plot(error, main="Residuos del Modelo")
abline(c(0,0),col="red")
abline(c(0.27243,0),col="red",lwd=2,lty=2)
abline(c(-0.27243,0),col="red",lwd=2,lty=2)

#Modelo para Serie D#
modelo_serie_D=arima(infect_spain_cmdia,order = c(6,1,3))
summary(modelo_serie_D)

###DIAGNOSIS DEL MODELO###

##Modelo Serie L-D##
#Ruido Blanco#
tsdiag(mod0)
Box.test(residuals(mod0),type = "Ljung-Box")

#Homocedasticidad
error_LD=residuals(modelo_serie_LD) 
par(mfrow=c(1,1))
plot(error, main="Residuos Serie ") 
abline(c(0,0),col="red",lwd=2,lty=2)

#Independencia#
par(mfrow=c(1,2))
acf(error_LD,main="Autocorrelaci�n Simple de Residuos LD")
pacf(error_LD,main="Autocorrelaci�n Parcial de Residuos LD")

#Normalidad
qqnorm(error, main= "Normalidad Residuos")
qqline(error)
ks.test(error, pnorm, mean(error), sd(error))


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



##PRON�STICO##
#Modelo de Serie  LD#
pronostico_LD=forecast::forecast(mod0,h=30)
plot(pronostico_LD)

predict(mod0,7)
futurVal_LD = forecast(mod0, h=7, level=c(95.0))
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







auto.arima(serielog, seasonal = FALSE)
auto.arima(serielog, stepwise = TRUE, approximation = TRUE)
mod_prueba=arima(serielog,order = c(0,1,1))
coeftest(mod_prueba)

error_mod_prueba=residuals(mod_prueba)
par(mfrow=c(1,2))
acf(error_mod_prueba)
pacf(error_mod_prueba)
