#Introducimos nuestra base de datos en R#
file.choose()
ruta= "C:\\Users\\alber\\OneDrive\\Documentos\\TFG\\Buena Base de datos\\Máximos+Variables Explicativas.xlsx"
excel_sheets(ruta)
names(base)
base=read_excel(ruta)
casos=read_excel(ruta,sheet = "Hoja2")
library(tidyverse)
BASE=filter(base,Continente!='Oceanía')
attach(BASE)


#Análisis de la varianza
mod=aov(CasosTotales~Continente)
anova(mod)

tapply(CasosTotales,Continente, mean)

ICplot(mod,'Continente')
pairwise.t.test(CasosTotales, Continente, p.adj='none')

par(mfrow=c(2,2))
plot(mod)

#Eliminación de Oceanía
install.packages("tidyverse")


#Análisis de la Varianza Transformaciones
mod2=aov(log(CasosTotales)~Continente)
anova(mod2)

tapply(log(CasosTotales),Continente, mean)

ICplot(mod2,'Continente')
pairwise.t.test(log(CasosTotales), Continente, p.adj='none')

par(mfrow=c(2,2))
plot(mod2)

#Comprabaciones homocedasticidad

bartlett.test(log(CasosTotales)~Continente)
fligner.test(log(CasosTotales)~Continente)
