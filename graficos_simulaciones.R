library(stats)
library(xlsx)
set.seed(100)
mod_arma<-arima.sim(n=100,model=list(ar=0.7,ma=0.35),rand.gen=rnorm,sd=1)
plot.ts(mod_arma,col="blue",lwd=2,ylab="Valor Simulado", xlab="Tiempo", main="Serie Temporal (Simulación modelo ARMA(1,1))")
#Funci'on autocorrelaci'o  sinmmple
acf(mod_arma,main="Función de Autocorrelación Simple",lag.max = 20,col="firebrick2",xlab="Retados",ylab="Autocorrelaciones",lwd=2)
#Funci'on autocorrelaci'o  parcial
pacf(mod_arma,main="Función de Autocorrelación Parcial",lag.max = 20,col="firebrick2",xlab="Retados",ylab="Autocorrelaciones",lwd=2)
datos<-as.data.frame(mod_arma)
write.xlsx(datos,"C:/Users/rramon/Documents/datos_arma_11.xlsx")

mod_ar<-arima.sim(n=100,model=list(ar=c(0.5,0.45)),rand.gen=rnorm,sd=1)
plot.ts(mod_ar,col="blue",lwd=2,ylab="Valor Simulado", xlab="Tiempo", main="Serie Temporal (simulación modelo AR(2))")
#Funci'on autocorrelaci'o  sinmmple
acf(mod_ar,main="Función de Autocorrelación Simple",lag.max = 15,col="firebrick2",xlab="Retados",ylab="Autocorrelaciones",lwd=2)
#Funci'on autocorrelaci'o  parcial
pacf(mod_ar,main="Función de Autocorrelación Parcial",lag.max = 15,col="firebrick2",xlab="Retados",ylab="Autocorrelaciones",lwd=2)
datos<-as.data.frame(mod_ar)
write.xlsx(datos,"C:/Users/rramon/Documents/datos_ar_2.xlsx")

mod_ma<-arima.sim(n=100,model=list(ma=c(-0.7,0.2)),rand.gen=rnorm,sd=1)
plot.ts(mod_ma,col="blue",lwd=2,ylab="Valor Simulado", xlab="Tiempo", main="Serie Temporal (simulación modelo MA(2))")
acf(mod_ma,main="Función de Autocorrelación Simple",lag.max = 15,col="firebrick2",xlab="Retados",ylab="Autocorrelaciones",lwd=2)
#Funci'on autocorrelaci'o  parcial
pacf(mod_ma,main="Función de Autocorrelación Parcial",lag.max = 15,col="firebrick2",xlab="Retados",ylab="Autocorrelaciones",lwd=2)
datos<-as.data.frame(mod_ma)
write.xlsx(datos,"C:/Users/rramon/Documents/datos_ma.xlsx")

