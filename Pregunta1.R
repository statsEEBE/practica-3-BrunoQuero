#ensayo de bernoulli

x<-c(0,1)
f<-c(0.68,0.32)
plot(x, f, type="h", ylim=c(0,1), col="red")
points(x,f,pch=16,col="red")
n<-43 ##saco de la encuesta
muestra<- sample(x,n,f, replace=TRUE)
table(muestra)
pie(table(muestra))
mean(muestra)
muestra<- sample(x,n,f, replace=TRUE)
sum(muestra)
bar<-barplot(table(muestra)/n)
lines(bar, f, type="h", ylim=c(0,1), col="red")
points(bar,f,pch=16,col="red")
##cada vez que run el programa, como es aleatorio el experimento, 
##el resultado varia tambien, por lo tanto, nos preguntamos:
##cual es la prob de que el experimento de x
Y<-function(i){sum(sample(x,n,f, replace=TRUE))}
Y(1)
m<-400000 ##repito 40 veces el experimento i miro hacia donde tiende
barplot(table(sapply(1:m,Y)))
set.seed(123)
encuestas<-sapply(1:m,Y)

frec.rel<-table(encuestas)/m
frec.rel["13"]
##lo anterior hecho es numericamente, ahora lo haremos exacto
dbinom(13,43,0.32) #probablilidad de que pase 13 
#en una encuesta de 43 con ña probabilidad de que de sí sea de 0.32 
dbinom(17,44,0.32) #p(x=17)
pbinom(16,44,0.32) #P(x<17)
