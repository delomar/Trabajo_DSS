##########################
## TRABAJO FINAL DE DSS ##
##########################

## ANN para predecir el precio de un vehículo ##

# Establecer el directorio de trabajo

setwd("C:/Users/romap/Documents/R")

# Leer los datos
data = read.csv ("autos.csv", header=T) 

# Muestreo aleatorio 
samplesize = sample(nrow(data), nrow(data) / 3)

# Generar la misma muestra aleatoria cada vez y mantener la coherencia
set.seed (80) 
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

# Crear conjunto de entrenamiento y prueba
datatrain = data[ index, ]
datatest = data[ -index, ]

# Escalado de datos
max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

## ENTRENAMIENTO DE LA RED NEURONAL ##

# Cargar la librería
library(neuralnet)

# Crear el conjunto de entrenamiento y test
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# Entrenar la red neuronal
set.seed(2)

NN = neuralnet(price ~ dateCrawled + name + seller + offerType + abtest + 
                 vehicleType + yearOfRegistration, trainNN, hidden = 4 , linear.output = T )

# Ploteo
plot(NN)

## PROBAR LA RED ##

# Predicción del precio de un vehículo usando el modelo de red neuronal
predict_testNN = compute(NN, testNN[,c(1:7)])
predict_testNN = (predict_testNN$net.result * (max(data$price) - min(data$price))) + min(data$price)

plot(datatest$price, predict_testNN, col='orange', pch=16, ylab = "Precio predicho NN", xlab = "Precio Real")

abline(0,1)


