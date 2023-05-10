library(stats)
library(tseries)
library(astsa)
library(dplyr)
library(caret)
library(readxl)
library(ggplot2)
library(lubridate)
library(forecast)
#CÓDIGO: ARIMA CONSUMO FINAL. 
#TFG - Grado en Matemáticas.
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
#Como el analisis es univariante, tomamos unicamente la fecha y el valor del Consumo final.
df_datos<-select(df_datos,1,5)
#Representamos gráficamente la serie
ggplot(data=df_datos)+geom_line(aes(x=fecha_cambiada,y=Consumo_hogares),col="blue")+
  geom_vline(xintercept=as.numeric(df_datos$fecha_cambiada[161]), linetype=2,col="red")+
  labs(title='Consumo final de los hogares.',subtitle='A partir de los datos de la base de datos BDREMS', 
       y= "Consumo hogares (millones de euros)", x = "Fecha",
       caption='Elaboración propia: Ramón Pacheco Murillo')
#Convertimos los datos de entrenamiento y test en datos de serie temporal
serieTraining<-ts(df_datos$Consumo_hogares,start=1980,end=c(2018,4),frequency = 4)
df_test<-df_datos[157:164,]
serieTest<-ts(df_test$Consumo_hogares,start=2019,end = c(2020,4), frequency = 4)
#Ajustamos el modelo ARIMA para los datos 
modelo_uno<-auto.arima(serieTraining)
summary(modelo_uno)
#Veamos la calidad del ajuste graficamente
df_training<-df_datos[1:156,]
df_training$arima<-as.numeric(modelo_uno$fitted)
ggplot(data=df_training) +
  geom_line(aes(x=fecha_cambiada,y=arima,color="Predicciones"))+
  geom_line(aes(x=fecha_cambiada, y = Consumo_hogares,color="Datos reales"))+
  labs(title='Ajuste ARIMA(2,1,0) a los datos de entrenamiento',subtitle='A partir de los datos de la base de datos BDREMS', 
       y= "Consumo hogares (millones de euros)", x = "Fecha",
       caption='Elaboración propia: Ramón Pacheco Murillo')+scale_color_manual(values = c("Datos reales" = "blue", "Predicciones" = "red"),name=NULL)
#Hacemos la prediccion con vista a 8 trimestres
forecast_test<-forecast(modelo_uno,h=8)
df_test<-df_datos[157:164,]
df_test$arima<-as.numeric(forecast_test$mean)
#Representamos graficamente
ggplot(data=df_test) +
  geom_line(aes(x=fecha_cambiada,y=arima,col="Predicciones"),lwd=1)+
  geom_line(aes(x=fecha_cambiada, y = Consumo_hogares,col="Datos reales"),lwd=1)+
  geom_vline(xintercept=as.numeric(df_datos$fecha_cambiada[162]), linetype=2,col="orange",lwd=1.1)+
  labs(title='Ajuste ARIMA(2,1,0) a los datos de test',subtitle='A partir de los datos de la base de datos BDREMS', 
       y= "Consumo hogares (millones de euros)", x = "Fecha",
       caption='Elaboración propia: Ramón Pacheco Murillo')+scale_color_manual(values = c("Datos reales" = "blue", "Predicciones" = "red"),name=NULL)
RMSE_modelo<-sqrt(mean((df_test$Consumo_hogares-df_test$arima)^2))
MAE_modelo<-MAE(df_test$arima,df_test$Consumo_hogares)
MAPE_modelo<-mean(abs((df_test$Consumo_hogares-df_test$arima)/df_test$Consumo_hogares)) * 100
#Valoramos la calidad del ajuste realizado
modelo_grafica<-sarima(serieTraining,2,1,0)
Box.test(residuals(modelo_grafica$fit),type = "Ljung-Box")
#Analisis de estacionariedad

#Funci'on autocorrelaci'o  sinmmple
acf(serieTraining,main="Función de Autocorrelación Simple",lag.max = 30,col="firebrick2",)
#Funci'on autocorrelaci'o  parcial
pacf(serieTraining,main="Función de Autocorrelación Parcial",lag.max = 30,col="firebrick2",xlab="Retados",ylab="Autocorrelaciones")
#Test de dickey-fuller
adf.test(serieTraining)

serieDif<-diff(serieTraining, differences = 1)
plot(serieDif, xlab="Fecha", main="Primera diferencia del gasto en consumo final",col="blue",lwd=1.1)
#Funci'on autocorrelaci'o  sinmmple
acf(serieDif,main="Función de Autocorrelación Simple",lag.max = 30,col="firebrick2",xlab="Retados",ylab="Autocorrelaciones")
#Funci'on autocorrelaci'o  parcial
pacf(serieDif,main="Función de Autocorrelación Parcial",lag.max = 30,col="firebrick2",xlab="Retados",ylab="Autocorrelaciones")
#Test de dickey-fuller
adf.test(serieDif)
#Test de Phillips/Peeron

serieDif2<-diff(serieTraining, differences = 2)
plot(serieDif2, xlab="Fecha", main="Segunda diferencia del gasto en consumo final",col="blue",lwd=1.1)
#Funci'on autocorrelaci'o  sinmmple
acf(serieDif2,main="Función de Autocorrelación Simple",lag.max = 30,col="firebrick2",xlab="Retados",ylab="Autocorrelaciones")
#Funci'on autocorrelaci'o  parcial
pacf(serieDif2,main="Función de Autocorrelación Parcial",lag.max = 30,col="firebrick2",xlab="Retados",ylab="Autocorrelaciones")
#Test de dickey-fuller
adf.test(serieDif2)



#Observemos que realizando la tranformacion logarítmica de las variables
#Los resultados no difieren tanto
modelo_log<-auto.arima(log(serieTraining))
summary(modelo_log)
df_training$rlog<-as.numeric(modelo_log$fitted)
df_training$clog<-log(df_training$Consumo_hogares)
ggplot(data=df_training)+
  geom_line(aes(x=fecha_cambiada,y=rlog,color="Predicciones"))+
  geom_line(aes(x=fecha_cambiada,y=clog,color="Datos reales"))+
  labs(title='Ajuste ARIMA(2,1,1) a los datos de entrenamiento',subtitle='A partir de los datos de la base de datos BDREMS', 
                                                                    y= "ln(Consumo hogares)", x = "Fecha",
                                                                  caption='Elaboración propia: Ramón Pacheco Murillo')+
  scale_color_manual(values = c("Datos reales" = "blue", "Predicciones" = "red"),name=NULL)
forecast_test<-forecast(modelo_log,h=8)
df_test$rlog<-as.numeric(forecast_test$mean)
df_test$clog<-log(df_test$Consumo_hogares)
ggplot(data=df_test)+
  geom_line(aes(x=fecha_cambiada,y=rlog,color="Predicciones"))+
  geom_line(aes(x=fecha_cambiada,y=clog,color="Datos reales"))+
  labs(title='Ajuste ARIMA(2,1,1) a los datos de test',subtitle='A partir de los datos de la base de datos BDREMS', 
                y= "ln(Consumo hogares)", x = "Fecha",
                caption='Elaboración propia: Ramón Pacheco Murillo')+
  geom_vline(xintercept=as.numeric(df_datos$fecha_cambiada[162]), linetype=2,col="orange",lwd=1.1)+
  scale_color_manual(values = c("Datos reales" = "blue", "Predicciones" = "red"),name=NULL)

