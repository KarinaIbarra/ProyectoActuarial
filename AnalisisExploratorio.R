qplot(data.month.Num.claims$Year,data.month.Num.claims$Num.claims)
#graficamos el numero de reclamaciones por a??o
anos<-c(1980:1990)#asignamos un vector con los correspondientes a??os por facilidad
montos.tot.anos<-c()#en esta parte creamos un vector del monto total de reclamaciones para los distintos a??os
for(i in 1:11){
  montos.tot.anos[i]<-sum(subset(data.claims$LossinDKM, data.claims$Year== (1979+i)))
}
qplot(anos,montos.tot.anos)#graficamos el monto total de reclamaciones por a??o (no es muy ??til)

meses<-c(1:12)#creamos un vector de monto total de reclamaciones por mes para 1980
montos.tot.mes1980<-c()
  for(i in 1:12){
    montos.tot.mes1980[i]<-sum(subset(data.claims$LossinDKM,data.claims$Year==(1980) | 
    data.claims$Month==i))
  }

mat<-matrix(, nrow = 12, ncol = 11) #lo anterior se puede compactar en una matriz
for(i in 1:12){ #donde las columnas son los a??os y los renglones los meses
for(j in 1:11){
  mat[i,j]<-sum(subset(data.claims$LossinDKM, data.claims$Year==(1979+j) | 
              data.claims$Month==i))

}
}

montos.ano.mes<-as.data.frame(mat,row.names = meses) #creamos un dataframe a partir de la matriz
colnames(montos.ano.mes)<-anos
print(montos.ano.mes)

qplot(meses,montos.ano.mes$`1980`)#graficamos algunos a??os y sus montos por mes a ver qu?? sucede
qplot(meses,montos.ano.mes$`1981`)
qplot(meses,montos.ano.mes$`1982`)
qplot(meses,montos.ano.mes$`1983`)
qplot(meses,montos.ano.mes$`1984`)
qplot(meses,montos.ano.mes$`1985`)
#sospecho que no es ??til 

#se procede a ordenar los montos de las reclamaciones de mayor a menor (sin importar el mes), por a??o

mat1<-matrix(, nrow = 12, ncol = 11) #lo anterior se puede compactar en una matriz
#donde las columnas son los a??os y los renglones los meses
for(j in 1:11){
  mat1[,j]<-sort(montos.ano.mes[[j]],decreasing = TRUE)
}
montos.ano.mes.ord<-as.data.frame(mat1)
colnames(montos.ano.mes.ord)<-anos

qplot(meses,montos.ano.mes.ord$`1980`)#creo este es un buen punto de partida para inferir alguna distribucion
qplot(meses,montos.ano.mes.ord$`1981`)
qplot(meses,montos.ano.mes.ord$`1982`)
qplot(meses,montos.ano.mes.ord$`1983`)
qplot(meses,montos.ano.mes.ord$`1984`)
qplot(meses,montos.ano.mes.ord$`1985`)
qplot(meses,montos.ano.mes.ord$`1986`)
qplot(meses,montos.ano.mes.ord$`1987`)
qplot(meses,montos.ano.mes.ord$`1988`)
qplot(meses,montos.ano.mes.ord$`1989`)
qplot(meses,montos.ano.mes.ord$`1990`)
