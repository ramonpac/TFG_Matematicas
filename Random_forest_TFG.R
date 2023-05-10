library(caret)
library(dplyr)
library(readxl)
library(lubridate)
library(randomForest)
library(ggplot2)


#CÓDIGO: RANDOM FOREST PARA ESTIMAR EL CONSUMO FINAL. 
#TFG - Grado en Matemáticas - Universidad Rey Juan Carlos.
#Ramón Pacheco. 

# Preparación de los datos -----------------------------------------------
#Importamos los datos de la base de datos trimestral de la economía española: REMSDB.
datos<-read_excel("C:/Users/rramon/Downloads/BDREMS_arreglado.xlsx",sheet = "Hoja1")
df_datos<-as.data.frame(datos)
#En REMSDB aparece el año y el trimestre separados.
#Creamos un data frame con archivos de tipo fecha para unirlos.
fecha<-as.Date("1980/01/01")
fecha_cambiada<-matrix(ncol = 1, nrow = 164)
fecha_cambiada<-as.Date(fecha_cambiada)
for (i in 1:164){
  fecha_cambiada[i]<-fecha
  fecha<-fecha%m+%months(3)
}
fecha_cambiada<-as.data.frame(fecha_cambiada)
#Unimos los datos con los archivos de tipo fecha.
df_datos<-cbind(fecha_cambiada,df_datos)
#Es necesario ordenar los datos por fecha para la posterior división en datos de entrenamiento y test.
df_datos<-arrange(df_datos,fecha_cambiada)
#Eliminamos las columnas año y TR (porque ya tenemos el valor "fecha_cambiada")
df_datos<-select(df_datos,1,4:65)


# Estimaciones estáticas  -------------------------------------------

#Dividimos la muestra en parte de entrenamiento
datos_training<-df_datos[1:120,]
#Y parte de test
datos_test<-df_datos[121:164,]
#En los datos de test, eliminamos la variable consumo hogares (pues es la variable resultado)
datos_test_copia<-select(datos_test,1,2,4:63)
#Versión por defecto: 500 árboles
rf_500=randomForest(Consumo_hogares~., data=datos_training,type="regression")
print(rf_500)
plot(rf_500, main = "MSE según el número de árboles del Random Forest",col="red",lwd=1.5)
#Representamos el ajuste en los datos de entrenamiento
datos_training$Resultados_500<-rf_500[["predicted"]]
ggplot(data=datos_training)+
  geom_line(aes(x=fecha_cambiada,y=Consumo_hogares,color="Datos reales"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_500,color="Predicciones"))+
  labs(title='Random Forest - 500 árboles - Datos de entrenamiento', 
        subtitle='Comparación datos reales y estimados del consumo de los hogares.',
        y="Consumo hogares (millones de euros)", x=NULL,
        caption='Elaboración propia: Ramón Pacheco Murillo')+
  scale_color_manual(values = c("Datos reales" = "blue", "Predicciones" = "red"),name=NULL)
#Realizamos predicciones en base a este modelo
predicciones<- predict(rf_500,datos_test_copia)
#Las añadimos para la representación gráfica
datos_test$Resultados_500<-predicciones
ggplot(data=datos_test)+
  geom_line(aes(x=fecha_cambiada,y=Consumo_hogares,color="Datos reales"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_500,color="Predicciones"))+
  labs(title='Random Forest - 500 árboles - Datos de test', 
          subtitle='Comparación datos reales y estimados del consumo de los hogares.',
          y="Consumo hogares (millones de euros)", x=NULL,
          caption='Elaboración propia: Ramón Pacheco Murillo')+
  scale_color_manual(values = c("Datos reales" = "blue", "Predicciones" = "red"),name=NULL)
#Calculamos las medidas de error habituales del modelo para los datos de entrenamiento
RMSE_500_train<-sqrt(mean((datos_training$Consumo_hogares-datos_training$Resultados_500)^2))
MAE_500_train<-MAE(datos_training$Resultados_500,datos_training$Consumo_hogares)
MAPE_500_train<-mean(abs((datos_training$Consumo_hogares-datos_training$Resultados_500)/datos_training$Consumo_hogares)) * 100
#Calculamos las medidas de error habituales del modelo para los datos de test
RMSE_500_test<-sqrt(mean((datos_test$Consumo_hogares-datos_test$Resultados_500)^2))
MAE_500_test<-MAE(datos_test$Resultados_500,datos_test$Consumo_hogares)
MAPE_500_test<-mean(abs((datos_test$Consumo_hogares-datos_test$Resultados_500)/datos_test$Consumo_hogares)) * 100
#Añadimos los resultados a un dataframe que contendrá todos los resultados de todos los modelos
Resultados_test<-as.data.frame(datos_test$Resultados_500)
Resultados_train<-as.data.frame(datos_training$Resultados_500)

#Consideremos ahora el RF que genere menor MSE en los datos de entrenamiento

#Volvemos a recuperar los datos originales
#(pues se añadieron los valores del modelo anterior para la representación gráfica)
datos_training<-df_datos[1:120,]
#Y parte de test
datos_test<-df_datos[121:164,]
#En los datos de test, eliminamos la variable consumo hogares (pues es la variable resultado)
datos_test_copia<-select(datos_test,1,2,4:63)
#Número de árboles que minimizan el MSE - para los datos de entrenamiento
which.min(rf_500$mse)
modelo_rf=randomForest(Consumo_hogares~.,data=datos_training,type="regression", ntree=(499))
print(modelo_rf)
varImpPlot(modelo_rf, main = "Gráfico importancia - Random Forest")
#Representamos el ajuste en los datos de entrenamiento
datos_training$Resultados_min<-modelo_rf[["predicted"]]
ggplot(data=datos_training)+
  geom_line(aes(x=fecha_cambiada,y=Consumo_hogares,color="Datos reales"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_min,color="Predicciones"))+
  labs(title='Random Forest - mínimo M.S.E - Datos de entrenamiento', 
  subtitle='Comparación datos reales y estimados del consumo de los hogares.',
  y="Consumo hogares (millones de euros)", x=NULL,
 caption='Elaboración propia: Ramón Pacheco Murillo')+
  scale_color_manual(values = c("Datos reales" = "blue", "Predicciones" = "turquoise3"),name=NULL)
#Realizamos predicciones en base a este modelo
predicciones<- predict(modelo_rf,datos_test_copia)
#Añadimos para la representación gráfica
datos_test$Resultados_min<-predicciones
ggplot(data=datos_test)+
  geom_line(aes(x=fecha_cambiada,y=Consumo_hogares,color="Datos reales"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_min,color="Predicciones"))+
  labs(title='Random Forest - mínimo M.S.E - Datos de test', 
  subtitle='Comparación datos reales y estimados del consumo de los hogares.', 
  y="Consumo hogares (millones de euros)", x=NULL,
  caption='Elaboración propia: Ramón Pacheco Murillo')+
  scale_color_manual(values = c("Datos reales" = "blue", "Predicciones" = "turquoise3"),name=NULL)
#Calculamos las medidas de error habituales de este modelo para los datos de entrenamiento
RMSE_min_train<-sqrt(mean((datos_training$Consumo_hogares-datos_training$Resultados_min)^2))
MAE_min_train<-MAE(datos_training$Resultados_min,datos_training$Consumo_hogares)
MAPE_min_train<-mean(abs((datos_training$Consumo_hogares-datos_training$Resultados_min)/datos_training$Consumo_hogares)) * 100
#Calculamos las medidas de error habituales de este modelo para los datos de test
RMSE_min_test<-sqrt(mean((datos_test$Consumo_hogares-datos_test$Resultados_min)^2))
MAE_min_test<-MAE(datos_test$Resultados_min,datos_test$Consumo_hogares)
MAPE_min_test<-mean(abs((datos_test$Consumo_hogares-datos_test$Resultados_min)/datos_test$Consumo_hogares)) * 100
#Añadimos los resultados a un dataframe que contendrá todos los resultados de todos los modelos
Resultados_test<-cbind(Resultados_test,datos_test$Resultados_min)
Resultados_train<-cbind(Resultados_train,datos_training$Resultados_min)
colnames(Resultados_test)<-c("Estandar","minimo_mse")
colnames(Resultados_train)<-c("Estandar","minimo_mse")

datos_test$estandar<-Resultados_test$Estandar

ggplot(data=datos_test)+geom_line(aes(x=fecha_cambiada,y=Consumo_hogares,color="Datos reales"))+
  geom_line(aes(x=fecha_cambiada,y=estandar,color="Predicciones RF 500"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_min,color="Predicciones RF min MSE"))+
  labs(title='Comparativa modelo estándar vs ajustado nº de árboles',subtitle='Comparación datos reales y estimados del consumo para los datos de test.', 
       y= "Consumo hogares (millones de euros)", x = NULL,
       caption='Elaboración propia: Ramón Pacheco Murillo')+
  scale_color_manual(values = c("Datos reales" = "blue", "Predicciones RF 500" = "red","Predicciones RF min MSE" = "turquoise3"),name=NULL)
# Estimaciones dinámicas---------------------------------------------------------

#Creamos un vector que almacena las predicciones 
vector_resultados<-rep(0,44)

for (i in 0:43) {
  #Hacemos los datos de train más grandes cada vez que se ejecuta el bucle
  datos_training<-df_datos[1:(120+i),]
  #Ajustamos los datos de test para que corresponda con el trimestre inmediatamente siguiente
  datos_test<-df_datos[(121+i),]
  #Quitamos la variable Consumo_hogares de los datos de test
  datos_test<-select(datos_test,1:2,4:63)
  #Estimamos el modelo
  modelo_rf_iterativo=randomForest(Consumo_hogares~., data=datos_training,type="regression", ntree=(499))
  #Hacemos la predicción
  predicciones4<- predict(modelo_rf_iterativo,datos_test)
  #Guardamos la predicción en orden
  vector_resultados[i+1]<-predicciones4
}

#Recuperamos el set de datos originales para la gráfica
datos_test<-df_datos[121:164,]
datos_test$Resultados_wf<-vector_resultados
datos_test$Resultados_estandar<-Resultados_test$"minimo_mse"

ggplot(data=datos_test)+geom_line(aes(x=fecha_cambiada,y=Consumo_hogares,color="Datos reales"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_estandar,color="Pred_RF_min_MSE"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_wf,color="Pred_RF_dinámico"))+
  labs(title='Comparativa modelo dinámico vs estático.',subtitle='Comparación datos reales y estimados del consumo para los datos de test.', 
       y= "Consumo hogares (millones de euros)", x = "Fecha",
      caption='Elaboración propia: Ramón Pacheco Murillo')+scale_color_manual(values = c("Datos reales" = "blue", "Pred_RF_min_MSE" = "red","Pred_RF_dinámico" = "turquoise3"),name=NULL)

RMSE_wf_test<-sqrt(mean((datos_test$Consumo_hogares-datos_test$Resultados_wf)^2))
MAE_wf_test<-MAE(datos_test$Resultados_wf,datos_test$Consumo_hogares)
MAPE_wf_test<-mean(abs((datos_test$Consumo_hogares-datos_test$Resultados_wf)/datos_test$Consumo_hogares)) * 100

#Implementación variación del número de parámetros
vector_resultados_5<-rep(0,44)
for (i in 0:43) {
  datos_training<-df_datos[1:(120+i),]
  datos_test<-df_datos[(121+i),]
  datos_test<-select(datos_test,1:2,4:63)
  modelo_rf_iterativo=randomForest(Consumo_hogares~., data=datos_training,type="regression", ntree=(50))
  predicciones4<- predict(modelo_rf_iterativo,datos_test)
  vector_resultados_5[i+1]<-predicciones4
}

vector_resultados_2<-rep(0,44)
for (i in 0:43) {
  datos_training<-df_datos[1:(120+i),]
  datos_test<-df_datos[(121+i),]
  datos_test<-select(datos_test,1:2,4:63)
  modelo_rf_iterativo=randomForest(Consumo_hogares~., data=datos_training,type="regression", ntree=(100))
  predicciones4<- predict(modelo_rf_iterativo,datos_test)
  vector_resultados_2[i+1]<-predicciones4
}
vector_resultados_3<-rep(0,44)
for (i in 0:43) {
  datos_training<-df_datos[1:(120+i),]
  datos_test<-df_datos[(121+i),]
  datos_test<-select(datos_test,1:2,4:63)
  modelo_rf_iterativo=randomForest(Consumo_hogares~., data=datos_training,type="regression", ntree=(150))
  predicciones4<- predict(modelo_rf_iterativo,datos_test)
  vector_resultados_3[i+1]<-predicciones4
}
vector_resultados_4<-rep(0,44)
for (i in 0:43) {
  datos_training<-df_datos[1:(120+i),]
  datos_test<-df_datos[(121+i),]
  datos_test<-select(datos_test,1:2,4:63)
  modelo_rf_iterativo=randomForest(Consumo_hogares~., data=datos_training,type="regression", ntree=(200))
  predicciones4<- predict(modelo_rf_iterativo,datos_test)
  vector_resultados_4[i+1]<-predicciones4
}

datos_test<-df_datos[121:164,]
datos_test$Resultados_cincuenta<-vector_resultados_5
datos_test$Resultados_cien<-vector_resultados_2
datos_test$Resultados_ciento50<-vector_resultados_3
datos_test$Resultados_doscientos<-vector_resultados_4

#Guardamos las métricas de error.

RMSE_50_test<-sqrt(mean((datos_test$Consumo_hogares-datos_test$Resultados_cincuenta)^2))
MAE_50_test<-MAE(datos_test$Resultados_cincuenta,datos_test$Consumo_hogares)
MAPE_50_test<-mean(abs((datos_test$Consumo_hogares-datos_test$Resultados_cincuenta)/datos_test$Consumo_hogares)) * 100

RMSE_100_test<-sqrt(mean((datos_test$Consumo_hogares-datos_test$Resultados_cien)^2))
MAE_100_test<-MAE(datos_test$Resultados_cien,datos_test$Consumo_hogares)
MAPE_100_test<-mean(abs((datos_test$Consumo_hogares-datos_test$Resultados_cien)/datos_test$Consumo_hogares)) * 100

RMSE_150_test<-sqrt(mean((datos_test$Consumo_hogares-datos_test$Resultados_ciento50)^2))
MAE_150_test<-MAE(datos_test$Resultados_ciento50,datos_test$Consumo_hogares)
MAPE_150_test<-mean(abs((datos_test$Consumo_hogares-datos_test$Resultados_ciento50)/datos_test$Consumo_hogares)) * 100


RMSE_200_test<-sqrt(mean((datos_test$Consumo_hogares-datos_test$Resultados_doscientos)^2))
MAE_200_test<-MAE(datos_test$Resultados_doscientos,datos_test$Consumo_hogares)
MAPE_200_test<-mean(abs((datos_test$Consumo_hogares-datos_test$Resultados_doscientos)/datos_test$Consumo_hogares)) * 100

#Representación gráfica de todos los datos
ggplot(data=datos_test)+geom_line(aes(x=fecha_cambiada,y=Consumo_hogares,color="Datos reales"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_cincuenta,color="Predicciones RF 50"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_cien,color="Predicciones RF 100"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_ciento50,color="Predicciones RF 150"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_doscientos,color="Predicciones RF 200"))+
  labs(title='Comparativa entre modelos dinámicos',subtitle='Comparación datos reales y estimados del consumo para los datos de test.', 
       y= "Consumo hogares (millones de euros)", x = "Fecha",
       caption='Elaboración propia: Ramón Pacheco Murillo')+
  scale_color_manual(values = c("Datos reales" = "blue", "Predicciones RF 50" = "red","Predicciones RF 100" = "turquoise3","Predicciones RF 150" = "violetred","Predicciones RF 200" = "chocolate4"),name=NULL)
#Misma representación quitando la pandemia 
# Para poder visualizar mejor los datos
datos_test_pandemia<-datos_test[1:39,]
ggplot(data=datos_test_pandemia)+geom_line(aes(x=fecha_cambiada,y=Consumo_hogares,color="Datos reales"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_cincuenta,color="Predicciones RF 50"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_cien,color="Predicciones RF 100"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_ciento50,color="Predicciones RF 150"))+
  geom_line(aes(x=fecha_cambiada,y=Resultados_doscientos,color="Predicciones RF 200"))+
  labs(title='Comparativa entre modelos dinámicos - no pandemia',subtitle='Comparación datos reales y estimados del consumo para los datos de test.', 
       y= "Consumo hogares (millones de euros)", x = "Fecha",
       caption='Elaboración propia: Ramón Pacheco Murillo')+
  scale_color_manual(values = c("Datos reales" = "blue", "Predicciones RF 50" = "red","Predicciones RF 100" = "turquoise3","Predicciones RF 150" = "violetred","Predicciones RF 200" = "chocolate4"),name=NULL)

#Gráfico comparativo - modelo univariante vs random forest

#Hacemos los cálculos del modelo ARIMA univariante
serieTraining<-ts(df_datos$Consumo_hogares,start=1980,end=c(2018,4),frequency = 4)
df_test<-df_datos[157:164,]
modelo_uno<-auto.arima(serieTraining)
forecast_test<-forecast(modelo_uno,h=8)
df_test$arima<-as.numeric(forecast_test$mean)
df_test$datosRandom<-datos_test$Resultados_doscientos[37:44]
df_test$datosRandom2<-datos_test$Resultados_cincuenta[37:44]
#Representamos gráficamente la comparativa
ggplot(data=df_test) +
  geom_line(aes(x=fecha_cambiada,y=arima,col="ARIMA(2,1,0)"),lwd=1)+
  geom_line(aes(x=fecha_cambiada, y = Consumo_hogares,col="Datos reales"),lwd=1)+
  geom_line(aes(x=fecha_cambiada, y = datosRandom,col="Random Forest-200"),lwd=1)+
  geom_line(aes(x=fecha_cambiada, y = datosRandom2,col="Random Forest-50"),lwd=1)+
  geom_vline(xintercept=as.numeric(df_datos$fecha_cambiada[162]), linetype=2,col="orange",lwd=1.1)+
  labs(title='Comparativa ARIMA vs Random Forest',subtitle='A partir de los datos de la base de datos BDREMS', 
       y= "Consumo hogares (millones de euros)", x = "Fecha",
       caption='Elaboración propia: Ramón Pacheco Murillo')+scale_color_manual(values = c("Datos reales" = "blue", "ARIMA(2,1,0)" = "red", "Random Forest-200"="turquoise3","Random Forest-50"="chocolate4"),name=NULL)
#Calculamos la medida de error
RMSE_ARIMA_test<-sqrt(mean((df_test$Consumo_hogares-df_test$arima)^2))
MAE_ARIMA_test<-MAE(df_test$Consumo_hogares,df_test$arima)
MAPE_ARIMA_test<-mean(abs((df_test$Consumo_hogares-df_test$arima)/df_test$Consumo_hogares)) * 100
