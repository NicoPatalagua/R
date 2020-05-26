#### Regresion Lineal simple R ######

# Es útil para examinar o modelar la relación entre dos variables numericas

#  Se importa el dataframe
LungCapData <- read.table(file.choose(), header = T, sep = ",")
attach(LungCapData)
names(LungCapData)

str(LungCap)
str(Smoke)
class(Age)
class(LungCap)

# Modelar la relación entre la edad (Age) y la capacidad pulmonar (LungCap)
# LungCap variable dependiente (Y)
# Age variable independiente

#Graficamos la dispersión entre ambas variables
plot(Age, LungCap, main = "Grafico de Dispersión LungCap vs Age",
     col = "red")

# Se calcula la correlación ( o grado de asociación)
cor(Age, LungCap)
# Hay una asociación signicativa, positiva, lineal entre las dos variables

help(lm)

# Se crea el modelo de Regresión Lineal
# Variable (Y) ----- Variable (X)
rlmodelo <- lm(LungCap~Age)

# observemos el resumen estadistico (Summary)
summary(rlmodelo)

###### Anotación
# -Residuals: resumen de los residuos
# -Coefficients:
# -        Intercep: 1.19... Estimado de intercepción
# -        Age: 0.54.....    Estimado de la Pendiente
# -        Error Std. ... 


# - Residual standard error: Medida de variación de las observaciones
#                            alrededor de la regresión lineal (MSE: Mean Squeared Error).
# - R~cuadrado: Medida estadistica de cuan cerca estan los datos de la linea de regresión.
#               Conocida tambien coef. de determinación. Porcentaje (%) de la variación
#               de la variable respuesta (Y/outcome) que se explica por un modelo lineal.
#               R-cuadr = variación explicada / variación total.... (0...100%)
# F~Statistic: test de hipotesis y pvalue: ["test que todos los coef... en el modelo sean cero"]

####### Observemos los atributos del modelo #######
attributes(rlmodelo)
rlmodelo$coefficients
rlmodelo$residuals

##### Presentar la linea del modelo
plot(Age, LungCap, main = "Grafico de Dispersión LungCap vs Age",
     col = "red")
### La linea del model
abline(rlmodelo)

#### otra forma de extraer los coeficientes
coef(rlmodelo)


#### confint: Intervalos de Confianza
### Bridgestone .---- promedio = 40.000; km std = 3000km
## En promedio la llanta durará entre 37.000km 43.000km 
confint(rlmodelo)

confint(rlmodelo, level = 0.99)


### La tabla de analisis de varianza (ANOVA)
summary(rlmodelo)
# ANOVA: corresponde al f-test (test de fisher)
anova(rlmodelo)

############# Revisión de Supuestos (assumptions) de Regresión Lineal ##############
### Capacidad Pulmonar (LungCap) = Y, Resultado (outcome), variable dependiente

# Linea de Regresión: puede ser pensada como el valor "PREDICHO" o "AJUSTADO" 
# se etiqueta como Ysombrero (ŷ )
# Es considerada (ŷ ) la media (promedio) de X;
# ŷ  = µ y|x = b + mx 



############ Los Errores o Residuos: E 
# Son la diferencia entre el valor y (observado) y el valor ŷ (predicho/ajustado)

# Se asume (SUPUESTOS):
# 1.- Los valores de y (o el error residual) son independientes
# 2.- Los valores de y "pueden" ser expresados como una función lineal
# 3.- La variación de las observaciones alrededor de la linea LM (regr. L.) es constante
###### HOMOSCEDASTICITY: descripción estadistica "un modelo predictivo presenta 
# homos.... cuando la varianza del error condicional a la variables explicativas es 
# constante a lo largo de las observaciones
# 4.- Para el par (X,Y) los valores (o el error) se distribuye normalmente.

# supuestos (2,3,4) se comprueban examinando los residuos o errores del modelo.

# R-studio tiene diferentes graficas para hacer un diagnóstico de regresión (4 en total)
plot(rlmodelo)
### Primer grafico: "Si se cumple el supuesto de linealidad (se observa una linea)
# hay una nube de puntos sin ningun patron...."

### Segundo grafico: qqplot "CuartilCuartil grafico"
# Eje Y: error residuales ordenados, estandarizado
# Eje X: error residuales ordenados teoricos.
# " Para el supuesto se espera visualizar una linea diagonal "

#### Tercer y Cuarto Grafico: 
# Identifican la No-Linealidad, no varianza constante
# al igual que observaciones problematicas del modelo.


##### Si queremos presentar las 4 graficas al mismo tiempo

par(mfrow=c(2,2))
plot(rlmodelo)













