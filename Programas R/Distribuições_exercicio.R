set.seed(0) #garantir que se tem sempre a mesma solução

# 1- Distribuição de Poisson

nvals <- 10000
lambda <-1.5

vals <- rpois(nvals,lambda)

x11(title="Distribuição de Poisson Po(1.5)")

frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição de Poisson" , sub="Amostra 10.000", col="skyblue")
x <- seq(0,max(vals),1)
lines(x,dpois(x,lambda), col="darkblue", type="p", pch=8)



# 2- Distribuição Uniforme

nvals <- 10000
vals<-sample(0:2,nvals,replace=T)

x11(title="Distribuição Uniforme p=1/3")

frel <- table(vals) / length(vals)
fmax <- max(frel) + 0.05
plot(frel, ylim=c(0,fmax), xlab="y", ylab="g(y)", main="Distribuição Uniforme", sub="Amostra 10.000", col="brown4")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="darkblue", type="p", pch=1)



# 3 - Z = X + Y,X~Po(1.5),Y~Uniforme(p=1/3)

# 3.1 - Distribuição de Poisson

nvals <- 10000
lambda <-1.5

x11(title="Z = X + Y")
layout(matrix(c(1,2,3,3),2,2))

vals <- rpois(nvals,lambda)
frel <- table(vals) / length(vals)
plot(frel, ylim=c(0,0.5), xlab="x", ylab="f(x)", main="X=Distribuição de Poisson" , sub="Amostra 10.000", col="skyblue")
x <- seq(0,max(vals),1)
lines(x,dpois(x,lambda), col="darkblue", type="p", pch=8)

# 3.2 - Distribuição Uniforme

nvals <- 10000
vals<-sample(0:2,nvals,replace=T)

frel <- table(vals) / length(vals)
plot(frel, ylim=c(0,0.5), xlab="y", ylab="g(y)", main="Y=Distribuição Uniforme", sub="Amostra 10.000", col="brown4")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="black", type="p", pch=8)

# 3.3 Distribuição Z=X+Y

nvals <- 10000
lambda <-1.5

poisson<- rpois(nvals,lambda)
uniform<-sample(0:2,nvals,replace=T)

vals<-poisson+uniform

frel <- table(vals) / length(vals)
plot(frel, ylim=c(0,0.4), xlab="z", ylab="h(z)", main="Z = X + Y " , sub="Amostra 10.000",col="green")
x <- seq(0,max(vals),1)

# função de distribuição Z

fpz <- NULL
fpz[1] <- dpois(0,lambda)*1/3
fpz[2] <- dpois(1,lambda)*1/3+dpois(0,lambda)*1/3
for(i in 2:max(vals)) {
    fpz[i+1] <- dpois(i,lambda)*1/3+dpois(i-1,lambda)*1/3+dpois(i-2,lambda)*1/3
}
lines(x,fpz, col="darkblue", type="p", pch=8)
