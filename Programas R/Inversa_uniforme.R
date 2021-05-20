# Distribuição Uniforme Discreta (método:Inversa)

set.seed(0) #garantir que se tem sempre a mesma solução 

#Resolução 1: Simulação de uma distribuição uniforme discreta (n=3)

sim.discreta <- function(nvals)

{
	X.vals <- NULL
	for(j in 1:nvals) {
			u <- runif(1)
			if(u <= 1/3) {x <- 0}
			else if(u <= 2/3){x <- 1}
			else {x <- 2}
		X.vals[j] <- x
	}
	X.vals
}



x11(title="Resolução 1: Distribuição Uniforme p=1/3")
par(mfrow=c(2,2))

#-------------------------------------------------------------------------------------------
#  Amostra 100

nvals <- 100
vals <- sim.discreta(nvals)

frel <- table(vals) / length(vals)
fmax <- max(frel) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição Uniforme", sub="Amostra 100", col="brown4")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="darkblue", type="p", pch=1)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0: X segue uma uma distribuição Uniforme Discreta
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           				# valores observados
pj <- c(1/3,1/3,1/3)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals						# frequência sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()

p_value<-c(Amostra_100=aux)				#guardar valores

vals_100<-vals						# informação QQ Plot

#---------------------------------------------------------------------------------------
#  Amostra 1.000

nvals <- 1000
vals <- sim.discreta(nvals)
frel <- table(vals) / length(vals)
fmax <- max(frel) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição Uniforme", sub="Amostra 1.000", col="orange")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="darkblue", type="p", pch=1)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0: X segue uma uma distribuição Uniforme Discreta
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           				# valores observados
pj <- c(1/3,1/3,1/3)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals						# frequência sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()

p_value<-c(p_value,Amostra_1.000=aux)		#guardar valores

vals_1000<-vals						# informação QQ Plot

#---------------------------------------------------------------------------------------
#  Amostra 10.000

nvals <- 10000
vals <- sim.discreta(nvals)
frel <- table(vals) / length(vals)
fmax <- max(frel) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição Uniforme", sub="Amostra 10.000", col="green")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="darkblue", type="p", pch=1)

# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0:X segue uma uma distribuição Uniforme Discreta
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           				# valores observados
pj <- c(1/3,1/3,1/3)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals						# frequência sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()

p_value<-c(p_value,Amostra_10.000=aux)		#guardar valores

vals_10000<-vals						# informação QQ Plot


#-----------------------------------------------------------------------------
#  Amostra 100.000

nvals <- 100000
vals <- sim.discreta(nvals)
frel <- table(vals) / length(vals)
fmax <- max(frel) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição Uniforme", sub="Amostra 100.000", col="red")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="darkblue", type="p", pch=1)

# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0:X segue uma uma distribuição Uniforme Discreta
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           				# valores observados
pj <- c(1/3,1/3,1/3)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals						# frequência sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()

p_value<-c(p_value,Amostra_100.000=aux)		#guardar valores

p_value_r1<-p_value

vals_100000<-vals						# informação QQ Plot

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#Resolução 2: Simulação de uma distribuição uniforme discreta (n=3)


sim.discreta <- function(nvals)

{
	X.vals <- NULL
	for(j in 1:nvals) {
			u <- runif(1)
			X.vals[j] <- trunc(3u)+1
	}
	X.vals
}


x11(title="Resolução 2: Distribuição Uniforme p=1/3")
par(mfrow=c(2,2))


#-------------------------------------------------------------------------------------------
#  Amostra 100

nvals <- 100
vals <- sim.discreta(nvals)
frel <- table(vals) / length(vals)
fmax <- max(frel) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição Uniforme", sub="Amostra 100", col="brown4")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="darkblue", type="p", pch=1)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0:X segue uma uma distribuição Uniforme Discreta
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           				# valores observados
pj <- c(1/3,1/3,1/3)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals						# frequência sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()

p_value<-c(Amostra_100=aux)				#guardar valores


#-------------------------------------------------------------------------------------------
#  Amostra 1.000


nvals <- 1000
vals <- sim.discreta(nvals)
frel <- table(vals) / length(vals)
fmax <- max(frel) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição Uniforme", sub="Amostra 1.000", col="orange")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="darkblue", type="p", pch=1)



# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0: X segue uma uma distribuição Uniforme Discreta
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           				# valores observados
pj <- c(1/3,1/3,1/3)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals						# frequência sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()

p_value<-c(p_value,Amostra_1.000=aux)		#guardar valores

#---------------------------------------------------------------------------------------
#  Amostra 10.000

nvals <- 10000
vals <- sim.discreta(nvals)
frel <- table(vals) / length(vals)
fmax <- max(frel) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição Uniforme", sub="Amostra 10.000", col="green")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="darkblue", type="p", pch=1)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0:X segue uma uma distribuição Uniforme Discreta
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           				# valores observados
pj <- c(1/3,1/3,1/3)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals						# frequência sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()

p_value<-c(p_value,Amostra_10.000=aux)		#guardar valores

#-----------------------------------------------------------------------------
#  Amostra 100.000


nvals <- 100000
vals <- sim.discreta(nvals)
frel <- table(vals) / length(vals)
fmax <- max(frel) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)",  main="Distribuição Uniforme", sub="Amostra 100.000", col="red")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="darkblue", type="p", pch=1)



# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0: X segue uma uma distribuição Uniforme Discreta
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           				# valores observados
pj <- c(1/3,1/3,1/3)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals						# frequência sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()

p_value<-c(p_value,Amostra_100.000=aux)		#guardar valores

p_value_r2<-p_value

#-----------------------------------------------------------------------------
#QQ plot - resolução 1

x11(title="Inversa r1 - QQ Plot Uniforme")
 par(mfrow=c(2,2))

vals<-vals_100
nvals<-(length(vals))

qqplot(sample(0:2,nvals,replace=T), vals, main="QQ Plot Uniforme", sub="Amostra 100",xlab="Quartis Teóricos",
  ylab="Quartis Amostra")

abline(a=0, b=1, lty=2, col="red")


vals<-vals_1000
nvals<-(length(vals))

qqplot(sample(0:2,nvals,replace=T), vals, main="QQ Plot Uniforme", sub="Amostra 1.000",xlab="Quartis Teóricos",
  ylab="Quartis Amostra")

abline(a=0, b=1, lty=2, col="red")


vals<-vals_10000
nvals<-(length(vals))

qqplot(sample(0:2,nvals,replace=T), vals, main="QQ Plot Uniforme", sub="Amostra 10.000",,xlab="Quartis Teóricos",
  ylab="Quartis Amostra")

abline(a=0, b=1, lty=2, col="red")


vals<-vals_100000
nvals<-(length(vals))

qqplot(sample(0:2,nvals,replace=T), vals, main="QQ Plot Uniforme", sub="Amostra 100.000",xlab="Quartis Teóricos",
  ylab="Quartis Amostra")

abline(a=0, b=1, lty=2, col="red")


#------------------------------------------------------------------------------
p_value_r1							# mostra p-value
p_value_r1>0.05

p_value_r2							
p_value_r2>0.05

#
