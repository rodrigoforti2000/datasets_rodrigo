#carregando os dados
ny = read.table("NewYork-Menor65.txt")

#substituindo dados faltantes
ny[is.na(ny$V2),]
ny[4571,2] = mean(ny[c(4570,4572),2])


#Vendo os dados
ts.plot(ny$V2)
acf(ny$V2)
pacf(ny$V2)

#Dados semanais
ny$V1 = as.character(ny$V1)
ny_sem = NULL
datas = NULL
for( i in seq(1,length(ny$V2), 7)){
  ny_sem[i] = mean(ny$V2[i:i+7])
  datas[i] = ny$V1[i]
}
ny_sem = ny_sem[!is.na(ny_sem)]
datas = datas[!is.na(datas)]

#Vendo dados semanais
length(ny_sem) #730
ts.plot(ny_sem)
acf(ny_sem, lag.max = 200)#decaimento
abline(v = 1, col = "#1F77B4")
abline(v = 2, col = "#1F77B4")
pacf(ny_sem, lag.max = 100) #decaimento

#Decomposição
ny_sem = ts(ny_sem, frequency = 52)
plot(decompose(ny_sem))
