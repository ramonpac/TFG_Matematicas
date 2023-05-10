library(lubridate)
library(readxl)
library(stats)
library(dplyr)
library(tseries)
library(Kendall)
library(forecast)
library(astsa)
library(ggplot2)
datos_brutos<-read_excel("C:/Users/rramon/Downloads/datos_consumo_final.xls",sheet = "Hoja1",col_names = FALSE)
consumo<-as.data.frame(matrix(0, nrow = 112, ncol = 1))
for (i in 1:112){
  consumo[i,1]<-datos_brutos[nrow(datos_brutos)-i+1,2]
}

serieConsumo<-ts(consumo,start = 1995, end = c(2022,4), frequency = 4)

tsplot(serieConsumo, xlab="Fecha", ylab="Consumo final (millones de ???)", main="Gasto en consumo final",col="blue",lwd = 2)
abline(v=2020.25,lwd = 2,lty=2,col="red")
