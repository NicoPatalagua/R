###### Graficas de Barras y Torta ########

# Graficas de barras y torta, son apropiadas para resumir la
# distribución de una variable categorica  

# Se carga el dataframe
LungCapData <- read.table(file.choose(), header = T, sep = ",")
attach(LungCapData)
View(LungCapData)

#Se observan los nombres
names(LungCapData)
class(Gender)

?barplot
# Grafica de barras es una muestra visual de la frecuencia para cada
# variable categorica o la frecuencia relativa (%) para cada categoria

# Se genera una tabla con la variable "Gender"
# y se obtiene el promedio de cada genero
cuenta <- table(Gender)
porcentaje <- table(Gender)/length(Gender)

# Se crea la grafica de barras
barplot(porcentaje)


# Se agrega información a la grafica
barplot(porcentaje, main="Porcentaje por Genero LungCapData", xlab = "Genero",
        ylab = "Porcentaje (%)", las = 1)


# Para cambiar las etiquetas
barplot(porcentaje, main="Porcentaje por Genero LungCapData", xlab = "Genero",
        ylab = "Porcentaje (%)", las = 1, names.arg = c("Femenino", "Masculino"))

# Si se quiere presentar horizontal
barplot(porcentaje, main="Porcentaje por Genero LungCapData", ylab = "Genero",
        xlab = "Porcentaje (%)", las = 1, names.arg = c("Femenino", "Masculino"), 
        horiz = T)


# Examinemos la relación entre Gender y Smoking
relaGen_Smo = table(Smoke, Gender)
relaGen_Smo

#Graficamos
barplot(relaGen_Smo)

#Mejoramos
barplot(relaGen_Smo, beside = T)

barplot(relaGen_Smo, beside = T, legend.text = T)

barplot(relaGen_Smo, beside = T, 
        legend.text = c("No-Fumador", "Fumador"),
        main = "Grafica de Genero y Fumadores",
        xlab = "Genero", las = 1,
        col = c("blue", "red"))

box()

#### Graficas de Pastel o Torta ##########
pie(cuenta)

#Se añade una mejora con la información
pie(cuenta, main = "Cantidad por Genero LungCapData")
box()

#### Diagrama de caja. También conocido como diagrama de caja y bigote, 
#### box plot, box-plot o boxplot. 
#### Es un método estandarizado para representar gráficamente una serie  
#### de datos numéricos a través de sus cuartiles.

?boxplot

# se reproduce el boxplo para la variable LungCap
boxplot(LungCap)

#representacion de quartiles
quantile(LungCap, probs = c(0, 0.25, 0.5, 0.75, 1))

# Se mejora el resultado
boxplot(LungCap, main="Box Plot de Capacidad Pulmonar",
        ylab = "Capacidad Pulmonar", ylim = c(0,16), las=1)


# Boxplot para dos variables
boxplot(LungCap ~ Gender, main = "BoxPlot Capacidad Pulmonar por Genero")


########################
# BoxPlot Estratificado son utiles para examinar la relación entre una  
# variable categorica y una numerica, dentro de estratos o grupos definidos 
# por una tercera variable categórica
## Ejemplo: relación entre Smoker y LungCap, dentro de grupos de Age
# Se crea la variable AgeGroups
AgeGroups <- cut(Age, breaks = c(0,13,15,17,25),
                 labels=c("<13", "14-15","16-17","18+"))

# Revisamos los primeros 5 de Age, y Agegroups
Age[1:5]
levels(AgeGroups)

# Graficamos 
boxplot(LungCap, ylab = "Capacidad Pulmonar", main = "BoxPlot de Capacidad Pulmonar",
        las =1)

# Grafiquemos para Smoker y no Smokers
boxplot(LungCap~Smoke, ylab = "Capacidad Pulmonar", main = "Capacidad Pulmonar vs. Fumadores",
        las =1)
# Se observa que en promedio los fumadores tienen mejor capacidad pulmonar

# Consideraciones:
# 1.- El efecto de fumar se confunde con el efecto de la edad
# 2.- En promedio, los fumadores son mayores que los que no son fumadores
# 3.- Son niños de 3 hasta 19 años
# 4.- Los niños mayores tienen grandes cuerpos y por lo tanto mayor capacidad pulmonar

# Examinemos la relación entre Smoker-LungCap, con los grupos de edad
# Por ejemplo: relación para los mayores de 18 años
boxplot(LungCap[Age>=18]~Smoke[Age>=18], ylab = "Capacidad Pulmonar", 
        main = "Capacidad Pulmonar vs. Fumadores, para 18años+",
        las =1)
# Se observa que se mejora la intuición de mejor capacidad pulmonar para los no fumadores


# Observemos con los grupos de edad
Age[1:5]
AgeGroups[1:5]


# Visualicemos la relación entre LungCap y Smoke, dentro de cada Grupo de Edad
boxplot(LungCap~Smoke*AgeGroups, 
        ylab = "Capacidad Pulmonar", 
        main = "Capacidad Pulmonar vs. Fumadores, por Grupos de Edad",
        las = 1)


# Mejoramos la legenda
boxplot(LungCap~Smoke*AgeGroups, 
        ylab = "Capacidad Pulmonar", 
        main = "Capacidad Pulmonar vs. Fumadores, por Grupos de Edad",
        las = 2)

# Agreguemos color
boxplot(LungCap~Smoke*AgeGroups, 
        ylab = "Capacidad Pulmonar", 
        main = "Capacidad Pulmonar vs. Fumadores, por Grupos de Edad",
        las = 2, col = c("blue","red"), axes=F, xlab = "Estratos de Edad")

# Mejoremos la presentacion
box()
axis(2, at=seq(0,20,2), seq(0,20,2), las = 1)
axis(1, at=c(1.5,3.5,5.5,7.5), labels = c("<13", "14-15","16-17","18+"))
legend(x=6.5, y = 4.5, 
       legend = c("No Fumadores", "Fumadores"),
       col = c(4,2), pch=15, cex=0.8)



## DIAGRAMAS DE HISTROGRAMA
# Representación gráfica de una variable en forma de barras, 
# donde la superficie de cada barra es proporcional a la frecuencia 
# de los valores representados.
?hist

# Histograma de capacidad pulmonar
hist(LungCap)

hist(LungCap, freq = F)


hist(LungCap, prob = T)


hist(LungCap, prob = T, ylim = c(0,0.2))

hist(LungCap, prob = T, ylim = c(0,0.2), breaks = 7)

hist(LungCap, prob = T, ylim = c(0,0.2), breaks = 14)

hist(LungCap, prob = T, ylim = c(0,0.2), breaks = c(0,2,4,6,8,10,12,14,16))


hist(LungCap, prob = T, ylim = c(0,0.2), 
     breaks = seq(from=0, to=16, by=2),
     main = "Histograma de Capacidad Pulmonar",
     xlab = "Capacidad Pulmonar", las = 1)

lines(density(LungCap))

lines(density(LungCap), col = 4, lwd = 2)



