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

for (i in 1:nrow(base_sp_cmedia)) { 
  if(base_sp_cmedia[i,5]<0){
    base_sp_cmedia[i,5]=(base_sp_cmedia[i-3,5]+base_sp_cmedia[i-2,5]+base_sp_cmedia[i-1,5]+base_sp_cmedia[i+1,5]+base_sp_cmedia[i+2,5]+base_sp_cmedia[i+3,5])/6
  }
}




##CREACIÓN DE SERIES DE TIEMPO##

infect_spain_cmdia=zoo(base_sp_cmedia$`Nº de Contagios en el día`,base_sp_cmedia$Fecha)
inf_cmedia_limpiados=tsclean(infect_spain_cmdia)


#Representación de las diferentes Series con los Datos Corregidos#
par(mfrow=c(2,2))
plot(infect_spain,ylab="Infectados diarios", main="Serie Sin Corrección")
plot(infect_spain_cmdia,ylab="Nº Infectados",main="Serie con Corrección por Media")
plot(inf_cmedia_limpiados,ylab="Nº Infectados",main="Serie Limpiada")


adf.test(infect_spain_cmdia,alternative = "stationary")

ndiffs(inf_cmedia_limpiados)
nsdiffs(inf_cmedia_limpiados)

##TRANSFORMACIÓN A ESTACIONARIA##


serielog=log(inf_cmedia_limpiados+500)
plot(serielog, xlab="Tiempo" ,main="Serie Con Logaritmo")


serielog_difer=diff(serielog)
plot(serielog_difer, main="Serie Transfromada")


serie_dif_estacional=diff(serielog_difer,lag=7)
plot(serie_dif_estacional, main="Serie Con Diferencia Estacional")


adf.test(serie_dif_estacional,alternative = "stationary")

par(mfrow=c(1,2))
acf(serie_dif_estacional)
pacf(serie_dif_estacional)
plot(serie_dif_estacional)
plot(serielog_difer)


#Funciones de Autocorrelación
par(mfrow=c(1,2))
acf(serielog_difer,main="Función de Autocorrelación Simple")
pacf(serielog_difer, main="Función de Autocorrelación Parcial")


par(mfrow=c(1,2))
acf(serie_dif_estacional,main="Función de Autocorrelación Simple")
pacf(serie_dif_estacional,main="Función de Autocorrelación Parcial")


##MODELO1##
mod1=arima(serielog,order = c(0,1,1),seasonal = list(order=c(0,1,2),period=7))
summary(mod1)
mod1
coeftest(mod1)


#Diagnosis del modelo
par(mfrow=c(1,2))
error=residuals(mod1) 
acf(error)
pacf(error)

tsdiag(mod0)
Box.test(residuals(mod0),type = "Ljung-Box")


varianza_res= var(error)
media_res= mean(error)
desv_res=sqrt(varianza_res)

error_st=(error-media_res)/desv_res
plot(error_st, main="Residuos Estandarizados")
abline(c(0,0),col="red")
abline(c(3*desv_res,0),col="red",lwd=2,lty=2)
abline(c(-3*desv_res,0),col="red",lwd=2,lty=2)


qqnorm(error, main= "Normalidad Residuos")
qqline(error)
ks.test(error, pnorm, mean(error), sd(error))



##MODELO2##
mod2=arima(serielog,order = c(0,1,2),seasonal = list(order=c(0,1,2),period=7))
mod2
coeftest(mod2)

##MODELO3##
mod3=arima(serielog,order = c(0,1,3),seasonal = list(order=c(0,1,2),period=7))
mod3
coeftest(mod3)



##PRONÓSTICO##
#Modelo de Serie  LD#
pronostico_LD=forecast::forecast(mod1,h=7)
plot(pronostico_LD)

predict(mod1,7)
futurVal_LD = forecast(mod1, h=7, level=c(95.0))
plot(futurVal_LD)
print(futurVal_LD)

pred=data.frame(futurVal_LD)

datos_reales=pred
datos_reales$Point.Forecast=exp(datos_reales$Point.Forecast)
datos_reales$Lo.95=exp(datos_reales$Lo.95)
datos_reales$Hi.95=exp(datos_reales$Hi.95)


write.table(datos_reales,file = "predicción_real.csv",sep = ",")


