#carga de librerías
library(forecast)
library(lubridate)
library(ggplot2)
library(fpp2)
library(knitr)
library(readxl)
library(keras)
library(tensorflow)

#Carga de los datos
CovidItaliaMaster <- read_excel("CovidItaliaMaster.xlsx", 
                                col_types = c("date", "numeric", "numeric", "numeric"))

#Visualización de datos de carga
CovidItaliaMaster
View(CovidItaliaMaster)
summary(CovidItaliaMaster)
str(CovidItaliaMaster)

#Cambio de formato fecha a día/mes/año
as.Date(CovidItaliaMaster$Fecha, format ="%Y-%m-%d")

#Plot de datos maestros
plot(CovidItaliaMaster$Fecha, CovidItaliaMaster$Casos, xlab = "Meses", ylab = "Casos")
plot(CovidItaliaMaster$Fecha, CovidItaliaMaster$Muertes, xlab = "Meses", ylab = "Muertes")
plot(CovidItaliaMaster$Fecha, CovidItaliaMaster$Test, xlab = "Meses", ylab = "Test")

#Plot combinado de datos maestros
par(mfrow = c(2,2))
plot(CovidItaliaMaster$Fecha, CovidItaliaMaster$Casos) 
plot(CovidItaliaMaster$Fecha, CovidItaliaMaster$Muertes) 
plot(CovidItaliaMaster$Fecha, CovidItaliaMaster$Test)

#Dividir los datos por casos, muertes y test
Casos_Covid <- subset(CovidItaliaMaster, select = 2)
Muertes_Covid <- subset(CovidItaliaMaster, select = 3)
Test_Covid <- subset(CovidItaliaMaster, select = 4)

#Convertir los datos a vectores que se mostraran en x
Casos_Covid_Lista <- unlist(Casos_Covid)
Muertes_Covid_Lista <- unlist(Muertes_Covid)
Test_Covid_Lista <- unlist(Test_Covid)

#Calcular el día exacto del inicio y fin de la muestra
yday(ymd("2019-12-31")) #Fecha de inicio
yday(ymd("2020-01-05")) #Fecha de fin
yday(ymd("2020-09-15")) #Fecha del 70% de los datos para entrenamiento

#Convertir los datos a Time-Series con frecuencia diaria (frecuencia = 365 dias)
Casos_Covid_TS<- ts(data = Casos_Covid, start = c(2019,365), end = c(2021,5),frequency = 365)
Muertes_Covid_TS <- ts(data = Muertes_Covid, start = c(2019,365), end = c(2021,5),frequency = 365)
Test_Covid_TS <- ts(data = Test_Covid, start = c(2019,365), end = c(2021,5),frequency = 365)

#Seleccionar los datos de entrenamiento
Casos_Covid_Entrenamiento <- window(Casos_Covid_TS, start=c(2019,365), end= c(2020,259))
Muertes_Covid_Entrenamiento <- window(Muertes_Covid_TS, start=c(2019,365), end= c(2020,259))
Test_Covid_Entrenamiento <- window(Test_Covid_TS, start=c(2019,365), end= c(2020,259))

#FORECASTING==================================================================================
############################################  Métodos simples de forecasting, Pronóstico a 30 días (h)
  #Casos
Casos_Covid_Meanf_Method <- meanf(Casos_Covid_Entrenamiento, h=30)
Casos_Covid_Naive_Method <- naive(Casos_Covid_Entrenamiento, h=30)
Casos_Covid_Snaive_Method <- snaive(Casos_Covid_Entrenamiento, h=30)
Casos_Covid_RWF_Method <- rwf(Casos_Covid_Entrenamiento, h=30)
  #Plot de metodos para casos
par(mfrow = c(2,2))
plot(Casos_Covid_Meanf_Method)
plot(Casos_Covid_Naive_Method)
plot(Casos_Covid_Snaive_Method)
plot(Casos_Covid_RWF_Method)
  #Muertes
Muertes_Covid_Meanf_Method <- meanf(Muertes_Covid_Entrenamiento, h=30)
Muertes_Covid_Naive_Method <- naive(Muertes_Covid_Entrenamiento, h=30)
Muertes_Covid_Snaive_Method <- snaive(Muertes_Covid_Entrenamiento, h=30)
Muertes_Covid_RWF_Method <- rwf(Muertes_Covid_Entrenamiento, h=30)
  #Plot de métodos para muertes
par(mfrow = c(2,2))
plot(Muertes_Covid_Meanf_Method)
plot(Muertes_Covid_Naive_Method)
plot(Muertes_Covid_Snaive_Method)
plot(Muertes_Covid_RWF_Method)
  #Test
Test_Covid_Meanf_Method <- meanf(Test_Covid_Entrenamiento, h=30)
Test_Covid_Naive_Method <- naive(Test_Covid_Entrenamiento, h=30)
Test_Covid_Snaive_Method <- snaive(Test_Covid_Entrenamiento, h=30)
Test_Covid_RWF_Method <- rwf(Test_Covid_Entrenamiento, h=30, drift=TRUE)
  #Plot de métodos para test
par(mfrow = c(2,2))
plot(Test_Covid_Meanf_Method)
plot(Test_Covid_Naive_Method)
plot(Test_Covid_Snaive_Method)
plot(Test_Covid_RWF_Method)

#Plot combinado de forecast
  #Casos
autoplot(Casos_Covid_Entrenamiento) +
autolayer(Casos_Covid_Meanf_Method, series="Mean", PI=FALSE) +
autolayer(Casos_Covid_Naive_Method, series="Naive", PI=FALSE) +
autolayer(Casos_Covid_Snaive_Method, series="Seasonal naive", PI=FALSE) +
autolayer(Casos_Covid_RWF_Method, series="RWF", PI=FALSE) +
ggtitle("Forecasts Casos de Covid") +
xlab("Dias") + ylab("Casos") +
guides(colour=guide_legend(title="Forecast"))
 #Muertes
autoplot(Muertes_Covid_Entrenamiento) +
autolayer(Muertes_Covid_Meanf_Method, series="Mean", PI=FALSE) +
autolayer(Muertes_Covid_Naive_Method, series="Naive", PI=FALSE) +
autolayer(Muertes_Covid_Snaive_Method, series="Seasonal naive", PI=FALSE) +
autolayer(Muertes_Covid_RWF_Method, series="RWF", PI=FALSE) +
ggtitle("Forecasts Muertes de Covid") +
xlab("Dias") + ylab("Muertes") +
guides(colour=guide_legend(title="Forecast"))
  #Test
autoplot(Test_Covid_Entrenamiento) +
autolayer(Test_Covid_Meanf_Method, series="Mean", PI=FALSE) +
autolayer(Test_Covid_Naive_Method, series="Naive", PI=FALSE) +
autolayer(Test_Covid_Snaive_Method, series="Seasonal naive", PI=FALSE) +
autolayer(Test_Covid_RWF_Method, series="RWF", PI=FALSE) +
ggtitle("Forecasts Test de Covid") +
xlab("Dias") + ylab("Test") +
guides(colour=guide_legend(title="Forecast"))

#Diagnóstico residual
  #Casos
checkresiduals(Casos_Covid_Meanf_Method)
checkresiduals(Casos_Covid_Naive_Method)
checkresiduals(Casos_Covid_Snaive_Method)
checkresiduals(Casos_Covid_RWF_Method)
  #Muertes
checkresiduals(Muertes_Covid_Meanf_Method)
checkresiduals(Muertes_Covid_Naive_Method)
checkresiduals(Muertes_Covid_Snaive_Method)
checkresiduals(Muertes_Covid_RWF_Method)
  #Test
checkresiduals(Test_Covid_Meanf_Method)
checkresiduals(Test_Covid_Naive_Method)
checkresiduals(Test_Covid_Snaive_Method)
checkresiduals(Test_Covid_RWF_Method)

#Evaluación del pronóstico
#RMSE - Para seleccionar el mejor método.
#MAPE - Para la calidad del modelo.
  #Casos
Casos_Covid_Prueba <- window(Casos_Covid_TS, start=c(2020,260), end= c(2021,5))
accuracy(Casos_Covid_Meanf_Method,Casos_Covid_Prueba)
accuracy(Casos_Covid_Naive_Method,Casos_Covid_Prueba)
accuracy(Casos_Covid_Snaive_Method,Casos_Covid_Prueba)
accuracy(Casos_Covid_RWF_Method,Casos_Covid_Prueba)
  #Muertes
Muertes_Covid_Prueba <- window(Muertes_Covid_TS, start=c(2020,260), end= c(2021,5))
accuracy(Muertes_Covid_Meanf_Method,Muertes_Covid_Prueba)
accuracy(Muertes_Covid_Naive_Method,Muertes_Covid_Prueba)
accuracy(Muertes_Covid_Snaive_Method,Muertes_Covid_Prueba)
accuracy(Muertes_Covid_RWF_Method,Muertes_Covid_Prueba)
  #Test
Test_Covid_Prueba <- window(Test_Covid_TS, start=c(2020,260), end= c(2021,5))
accuracy(Test_Covid_Meanf_Method,Test_Covid_Prueba)
accuracy(Test_Covid_Naive_Method,Test_Covid_Prueba)
accuracy(Test_Covid_Snaive_Method,Test_Covid_Prueba)
accuracy(Test_Covid_RWF_Method,Test_Covid_Prueba)

#ARIMA========================================================================================
############################################  Métodos avanzados Arima, Pronóstico a 30 días (h)
  #Casos
Casos_Covid_Arima_Method <- forecast(auto.arima(Casos_Covid_Entrenamiento),h=30)
Casos_Covid_Arima_Method
autoplot(Casos_Covid_Arima_Method)
summary(Casos_Covid_Arima_Method)
accuracy(Casos_Covid_Arima_Method,Casos_Covid_Prueba)

  #Muertes
Muertes_Covid_Arima_Method <- forecast(auto.arima(Muertes_Covid_Entrenamiento),h=30)
Muertes_Covid_Arima_Method
autoplot(Muertes_Covid_Arima_Method)
summary(Muertes_Covid_Arima_Method)
accuracy(Muertes_Covid_Arima_Method,Muertes_Covid_Prueba)

  #Test
Test_Covid_Arima_Method <- forecast(auto.arima(Test_Covid_Entrenamiento),h=30)
Test_Covid_Arima_Method
autoplot(Test_Covid_Arima_Method)
summary(Test_Covid_Arima_Method)
accuracy(Test_Covid_Arima_Method,Test_Covid_Prueba)

#LSTM=========================================================================================
############################################  LSTM para time series 

#Datos // Usar un dato a la vez para cada tipo de modelo
data <- CovidItaliaMaster$Casos[1:nrow(CovidItaliaMaster)]
data <- CovidItaliaMaster$Muertes[1:nrow(CovidItaliaMaster)]
data <- CovidItaliaMaster$Test[1:nrow(CovidItaliaMaster)]
summary(data)

#Transformar datos a estacionalidad calculando una diferencia
diffed = diff(data, differences = 1)
length(diffed)

#k es el número de retrasos
lag_transform <- function(x, k= 1){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}
supervised = lag_transform(diffed, 1)
View(supervised)

#Separación de conjuntos de pruebas y entrenamientos
N = nrow(supervised)
n = round(N *0.7, digits = 0)
train = supervised[1:n, ]
test  = supervised[(n+1):N,  ]
test
train

#Normalización de los datos
scale_data = function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  
}

Scaled = scale_data(train, test, c(-1, 1))

y_train = Scaled$scaled_train[, 2]
x_train = Scaled$scaled_train[, 1]

y_test = Scaled$scaled_test[, 2]
x_test = Scaled$scaled_test[, 1]

##Transformación inversa
invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for( i in 1:t){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}


#Cambiar la forma de la entrada a 3-dim
dim(x_train) <- c(length(x_train), 1, 1)

#Especificar los argumentos requeridos
X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 1                #Debe ser un factor común tanto en el entrenamiento como en las muestras de la prueba
units = 3                     #Puede ajustar esto, en la fase de ajuste del modelo

#=========================================================================================

model <- keras_model_sequential() 
model%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
  layer_dense(units = 1)


model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics =  c("accuracy", "mse", "mae", "mape")
)

summary(model)

Epochs = 300  
for(i in 1:Epochs ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}

L = length(x_test)
scaler = Scaled$scaler

predictions = numeric(L)
test

#Prueba de los tiempos de predicción L dado el primer paso
tem <- data[n+1]
X <- x_test[1]
for(i in 1:L){
  
  dim(X) = c(1,1,1)
  yhat <- model %>% predict(X, batch_size=batch_size)
  #Almacenar para la próxima predicción
  X <- yhat
  #Invertir escala
  yhat <- invert_scaling(yhat, scaler,  c(-1, 1))
  #Invertir diferenciación
  yhat  <- yhat + tem
  #Almacenado
  predictions[i] <- yhat
  
  #Actualizar temp
  tem <- yhat
  
}

#Plotting de las predicciones
plot(1:length(predictions), predictions,col="red", pch=19)
points(1:length(predictions), data[n+1:length(predictions)+1],col="green", pch=19)


#Validación del modelo en plot dinámico.
history <- model %>% fit(
  x_train, y_train,
  epochs = 300, batch_size = 1,
  validation_split = 0.2
)

plot(history)
