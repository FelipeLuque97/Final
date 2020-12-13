## Se elige una semilla
set.seed(12345)

## Se definen los datos de una regresión lineal simple
datos = data.frame(y=runif(100, min=-30, max=30), x=rnorm(100, mean=0, sd=2))

lm(formula = datos$y ~ datos$x)

## Para un lamda = 0
l=0

sum.errores.cuadrados = function(data, par) {
  with(data, sum((y-(par[1] + par[2] * x) )^2)-l*(par[2]))
}

resultado = optim(par=c(0,0), fn=sum.errores.cuadrados, data=datos)


beta<-runif(100, min=-3, max=3)

sse<-1:100

for (i in 1:100){
  sse[i]<-sum((datos$y-(resultado$par[1] + beta[i]* datos$x) )^2)} 
 

##Para un lamda de 5

l=1000

sum.errores.cuadrados = function(data, par) {
  with(data, sum((y-(par[1] + par[2] * x) )^2)-l*(par[2]))
}

resultado = optim(par=c(0,0), fn=sum.errores.cuadrados, data=datos)



sse2<-1:100

for (i in 1:100){
  sse2[i]<-sum((datos$y-(resultado$par[1] + beta[i]* datos$x) )^2)-l*(beta[i])} 


plot(beta,sse2)



##Para un lamda de 10

l=5000

sum.errores.cuadrados = function(data, par) {
  with(data, sum((y-(par[1] + par[2] * x) )^2)-l*(par[2]))
}

resultado = optim(par=c(0,0), fn=sum.errores.cuadrados, data=datos)



sse3<-1:100

for (i in 1:100){
  sse3[i]<-sum((datos$y-(resultado$par[1] + beta[i]* datos$x) )^2)-l*(beta[i])} 


plot(beta,sse3)



##Para un lamda de 30

l=10000

sum.errores.cuadrados = function(data, par) {
  with(data, sum((y-(par[1] + par[2] * x) )^2)-l*(par[2]))
}

resultado = optim(par=c(0,0), fn=sum.errores.cuadrados, data=datos)



sse4<-1:100

for (i in 1:100){
  sse4[i]<-sum((datos$y-(resultado$par[1] + beta[i]* datos$x) )^2)-l*(beta[i])} 


plot(beta,sse4)




##Para un lamda de 100

l=25000

sum.errores.cuadrados = function(data, par) {
  with(data, sum((y-(par[1] + par[2] * x) )^2)-l*(par[2]))
}

resultado = optim(par=c(0,0), fn=sum.errores.cuadrados, data=datos)



sse5<-1:100

for (i in 1:100){
  sse5[i]<-sum((datos$y-(resultado$par[1] + beta[i]* datos$x) )^2)-l*(beta[i])} 


plot(beta,sse5)


##Para un lamda de 250

l=35000

sum.errores.cuadrados = function(data, par) {
  with(data, sum((y-(par[1] + par[2] * x) )^2)-l*(par[2]))
}

resultado = optim(par=c(0,0), fn=sum.errores.cuadrados, data=datos)



sse6<-1:100

for (i in 1:100){
  sse6[i]<-sum((datos$y-(resultado$par[1] + beta[i]* datos$x) )^2)-l*(beta[i])} 


plot(beta,sse6)


ss<- cbind(beta,sse,sse2,sse3,sse4,sse5,sse6)


write.csv(ss, file="C:/Users/carlos  felipe/Desktop/Proyecto SITP/lasso2.csv")

