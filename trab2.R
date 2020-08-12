#carregando os dados
ny = read.table("NewYork-Menor65.txt")


#substituindo dados faltantes
ny[is.na(ny$V2),]
ny[4571,2] = mean(ny[c(4570,4572),2])


#Vendo os dados
ts.plot(ny$V2)
acf(ny$V2, lag.max = 1500)
pacf(ny$V2, lag.max = 1500)

#Dados semanais
ny$V1 = as.character(ny$V1)
ny_sem = NULL
datas = NULL
for( i in seq(1,length(ny$V2), 7)){
  ny_sem[i] = sum(ny$V2[i:(i+7)])
  datas[i] = ny$V1[i]
}
ny_sem = ny_sem[!is.na(ny_sem)]
datas = datas[!is.na(datas)]

#Vendo dados semanais
length(ny_sem) #730
ts.plot(ny_sem)
acf(ny_sem, lag.max = 200)#decaimento
abline(v = 52, col = "#1F77B4")
abline(v = 104, col = "#1F77B4")
pacf(ny_sem, lag.max = 100) #decaimento

#Decomposição
ts_ny_sem = ts(ny_sem, frequency = 52)
plot(decompose(ts_ny_sem))

#Retirando 26 observações
ny_past = ny_sem[1:(length(ny_sem) - 26)]
ny_future = ny_sem[(length(ny_sem) - 25):length(ny_sem)]

#ARMA(1,1)
t = seq(1,length(ny_past))
arma11 = arima(ny_past, c(1,0,1), xreg = c(t))

#Diagnósticos
acf(arma11$residuals)
pacf(arma11$residuals)
plot((arma11$residuals - mean(arma11$residuals))/sd(arma11$residuals))

#Visualizando ARMA11
arma11fitted <- ny_past - arma11$residuals
ts.plot(ny_past, col = "lightgrey")
lines(arma11fitted, col = "#1F77B4")
lines(arma11fitted + 2* sqrt(arma11$sigma2), col ="red")
lines(arma11fitted - 2* sqrt(arma11$sigma2), col ="red")

#Predição ARMA11
t_pred = (length(ny_sem) - 25):length(ny_sem)
arma11pred = predict(arma11, newxreg = t_pred)
ts.plot(ny_past, col = "lightgrey", xlim = c(600,730),
        ylim = c(200,500))
lines(arma11pred$pred, col = "#1F77B4")
lines(x = t_pred, ny_future, col = "black")
lines(x = t_pred, ny_future + 2*28.84, col = "red")
lines(x = t_pred, ny_future - 2*28.84, col = "red")

#AR1: Muito ruim
ar1 = arima(ny_past, c(1,0,0,), xreg = c(t))
acf(ar1$residuals)
pacf(ar1$residuals)
plot((ar1$residuals - mean(ar1$residuals))/sd(ar1$residuals))

#MA1: Muito ruim
ma1 = arima(ny_past, c(0,0,1), xreg = c(t))
acf(ma1$residuals)
pacf(ma1$residuals)
plot((ma1$residuals - mean(ma1$residuals))/sd(ma1$residuals))
