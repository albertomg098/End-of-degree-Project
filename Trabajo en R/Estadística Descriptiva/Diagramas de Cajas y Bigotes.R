#Introducimos nuestra base de datos en R#
file.choose()
ruta= "C:\\Users\\alber\\OneDrive\\Documentos\\TFG\\Buena Base de datos\\Máximos+Variables Explicativas.xlsx"
excel_sheets(ruta)
names(base)
base=read_excel(ruta)
casos=read_excel(ruta,sheet = "Hoja2")
attach(base)

#DIAGRAMAS DE CAJAS Y BIGOTES#
boxplot(CasosTotales~Continente,data=base,horizontal = T, col=c("purple","blue","green","green4","red"))
boxplot(PIB~Continente,data=base,horizontal = T,col=c("purple","blue","green","green4","red"))
boxplot(VarPIB~Continente,data=base,horizontal = T,col=c("purple","blue","green","green4","red"))
boxplot(Población~Continente,data=base,horizontal = T,col=c("purple","blue","green","green4","red"))
boxplot(CamasRea~Continente,data=base,horizontal = T,col=c("purple","blue","green","green4","red"))
boxplot(NatalidadReal~Continente,data=base,horizontal = T,col=c("purple","blue","green","green4","red"))
boxplot(DeudaPública~Continente,data=base,horizontal = T,col=c("purple","blue","green","green4","red"))
boxplot(GastoSalud~Continente,data=base,horizontal = T,col=c("purple","blue","green","green4","red"))
boxplot(TasaDesempleo~Continente, horizontal=T,col=c("purple","blue","green","green4","red"))