#carregando os dados
ny = read.table("NewYork-Menor65.txt")

#substituindo dados faltantes
ny[is.na(ny$V2),]
ny[4571,2] = mean(ny[c(4570,4572),2])

#Vendo os dados
ts.plot(ny$V2)

#Dados semanais
ny_sem = NULL
for( i in seq(1,length(ny$V2), 7)){
  ny_sem[i] = mean(ny_dt$V2[i:i+7])
}
ny_sem = ny_sem[!is.na(ny_sem)]


#Vendo dados semanais
length(ny_sem) #730
ts.plot(ny_sem)
