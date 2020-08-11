library(data.table)

#carregando os dados
ny = read.table("NewYork-Menor65.txt")
ny[is.na(ny$V2),]
ny[4571,2] = mean(ny[c(4570,4572),2])

#Convertendo para data.table
ny_dt = as.data.table(ny)
