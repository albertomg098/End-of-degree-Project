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
for (i in 1:nrow(base_sp_cmedia)) {
  if(base_sp_cmedia[i,5]<0){
    base_sp_cmedia[i,5]=(base_sp_cmedia[i-3,5]+base_sp_cmedia[i-2,5]+base_sp_cmedia[i-1,5]+base_sp_cmedia[i+1,5]+base_sp_cmedia[i+2,5]+base_sp_cmedia[i+3,5])/6
    k=k+1
    posicion[k]=i
  }
}





##CREACI�N DE SERIES DE TIEMPO##
serie_sin_correc=ts(base_sp$`N� de Contagios en el d�a`,start = c(2020,58),frequency = 366)
infect_spain_cmdia=ts(base_sp_cmedia$`N� de Contagios en el d�a`,start = c(2020,58),frequency = 366)

par(mfrow=c(1,2))
plot(infect_spain_cmdia,ylab="N� Infectados",main="Serie con Correcci�n por Media")
plot(serie_sin_correc)



##COMPROBACI�N de si es estacionaria (Prueba de Dickey-Fuller)##
adf.test(infect_spain_cmdia,alternative = "stationary")





##TRANSFORMACI�N A ESTACIONARIA##

#M�todo del logaritmo para estacionalidad (NO FUNCIONA)#
serielog=log(infect_spain_cmdia+500)

serielog_diferencia=diff(serielog)
serielog_diferecia_7=diff(serielog_diferencia,lag = 7)

adf.test(serielog_diferecia_7,alternative = "stationary")


#Creaci�n del modelo

par(mfrow=c(1,2))
acf(serielog_diferecia_7)
pacf(serielog_diferecia_7)

auto.arima(serielog_diferecia_7, seasonal = TRUE)


mod2












