##### GRAFICA DE DISPERSIÓN ####

LungCapData <- read.table(file.choose(), header = T, sep = ",")
attach(LungCapData)
names(LungCapData)



# Se hace el estudio sobre las variables LungCap y Age
# Tipo/clase de las variables $Age y $Height
class(Age)
class(Height)



# Podemos ver que hay una asociación lineal bastante fuerte

# Se presenta la grafica de dispersión
plot(Age, Height, main="Dispersión Altura vs. Edad", las=1,
     xlim = c(0,25), cex = 0.5, col = "red")

# abline: linea de regresión que se ajusta a la grafica
abline(lm(Height~Age))


abline(lm(Height~Age), col = "blue")


###### Calculo de la correlación y covarianza ######
# Exploremos la relación entre la edad y la capacidad pulmonar 

# Correlación Rango de Spearman: medida no paramétrica de la asociación
#                                monotona entre dos variables numéricas
# Correlación Rango de Kendall: medida no paramétrica de la asociación,
# 				basada en concordancia o discordancia del
# 				par x-y.


# Ayuda o documentación
help(cor.test)
?cor.test

# Se calcula la correlación entre las dos variables
cor(Age, Height)

# Correlación de Pearson: medida paramétrica de la asociación
#                         lineal entre 2 variables numéricas
# El metodo de Pearson esta por defecto
cor(Age, LungCap)


#El orden de las variables no incide en el valor de la correlación
cor(LungCap, Age)


#Correlación de spearman
cor(Age, LungCap, method="spearman")


#Correlación de kendall 
cor(Age, LungCap, method="kendall")


# Coeficiente de correlación de Pearson: es una medida de la fuerza 
# de una asociación lineal entre dos variables y se denota con r. 
# Básicamente, una correlación de Pearson entre producto y momento 
# intenta trazar una línea de mejor ajuste a través de los datos de dos 
# variables, y el coeficiente de correlación de Pearson, r, indica 
# cuán lejos están todos estos puntos de datos de esta línea de mejor ajuste 
# (es decir, cuán bien se ajustan los puntos de datos a este nuevo modelo/línea de mejor ajuste).
cor.test(Age, LungCap, method="pearson")




# Observemos la covarianza
cov(LungCap, Age)



# Observemos los gráficos para cada variable
pairs(LungCapData)



# Necesitamos observar las primeras graficas
pairs(LungCapData[,1:3])



#Revisemos la correlación entre las variables
cor(LungCapData)


#Produce error porque esta calculando la correlación sobre variables categoricas
#Calculemos sobre las variables numericas
cor(LungCapData[,1:3])



