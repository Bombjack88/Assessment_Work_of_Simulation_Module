# Distribuição de Poisson (método:Inversa)

set.seed(0) #garantir que se tem sempre a mesma solução
 
 sim.poisson <- function(lambda,nvals)
{
	X.vals <- NULL
	for(j in 1:nvals) {
		u <- runif(1)
		i <- 0
		p <- exp(-lambda)
		F <- p
		while (F <= u) {
			i <- i + 1
			p = p * lambda/i
			F <- F + p
		}
		X.vals[j] <- i
	}
	X.vals
}

 x11(title="Inversa - Distribuição de Poisson Po(1.5)")
 par(mfrow=c(2,2))
 
#-------------------------------------------------------------------------------------------
#  Amostra 100

 lambda <- 1.5
 nvals<-100
 vals <- sim.poisson(lambda, nvals)
 frel <- table(vals) / length(vals)
 fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
 plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição de Poisson" , sub="Amostra 100", col="skyblue")
 x <- seq(0,max(vals),1)
 lines(x,dpois(x,lambda), col="darkblue", type="p", pch=8)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0: X segue uma uma distribuição de Poisson (1.5)
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           			# valores observados
pj <- dpois(seq(0,max(vals),1),1.5)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals

# correções para Ej<5

	while(Ej[which.min(Ej)]<5){
	
	posição<-which.min(Ej)
	
	Ej[posição-1]<-Ej[posição-1]+Ej[posição]
	Ej<-Ej[-posição]

	Nj[posição-1]<-Nj[posição-1]+Nj[posição]
	Nj<-Nj[-posição]

	pj[posição-1]<-pj[posição-1]+pj[posição]
	pj<-pj[-posição]

	}
	Ej
	Nj
	pj

								# frequência sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()

p_value<-c(Amostra_100=aux)				#guardar valores

vals_100<-vals						# informação QQ Plot

#---------------------------------------------------------------------------------------
#  Amostra 1.000

 lambda <- 1.5
 nvals<-1000
 vals <- sim.poisson(lambda, nvals)

 frel <- table(vals) / length(vals)
 fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
 plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição de Poisson" , sub="Amostra 1.000", col="orange")
 x <- seq(0,max(vals),1)
 lines(x,dpois(x,lambda), col="darkblue", type="p", pch=8)

# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0: X segue uma uma distribuição de Poisson (1.5)
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))						# valores observados
pj <- dpois(seq(0,max(vals),1),1.5)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals

# correções para Ej<5

	while(Ej[which.min(Ej)]<5){
	
	posição<-which.min(Ej)
	
	Ej[posição-1]<-Ej[posição-1]+Ej[posição]
	Ej<-Ej[-posição]

	Nj[posição-1]<-Nj[posição-1]+Nj[posição]
	Nj<-Nj[-posição]

	pj[posição-1]<-pj[posição-1]+pj[posição]
	pj<-pj[-posição]

	}
	Ej
	Nj
	pj

						# frequência sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

pchisq(chi, df=gl, lower.tail=FALSE)
aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()								

p_value<-c(p_value,Amostra_1.000=aux)		#guardar valores

vals_1000<-vals						# informação QQ Plot

#---------------------------------------------------------------------------------------
#  Amostra 10.000
 lambda <- 1.5
 nvals<-10000
 vals <- sim.poisson(lambda, nvals)

 frel <- table(vals) / length(vals)
 fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
 plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição de Poisson" , sub="Amostra 10.000", col="green")
 x <- seq(0,max(vals),1)
 lines(x,dpois(x,lambda), col="darkblue", type="p", pch=8)
 

# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0: X segue uma uma distribuição de Poisson (1.5)
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           			# valores observados
pj <- dpois(seq(0,max(vals),1),1.5)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals


# correções para Ej<5

	while(Ej[which.min(Ej)]<5){
	
	posição<-which.min(Ej)
	
	Ej[posição-1]<-Ej[posição-1]+Ej[posição]
	Ej<-Ej[-posição]

	Nj[posição-1]<-Nj[posição-1]+Nj[posição]
	Nj<-Nj[-posição]

	pj[posição-1]<-pj[posição-1]+pj[posição]
	pj<-pj[-posição]

	}
	Ej
	Nj
	pj
						# frequência sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1     

pchisq(chi, df=gl, lower.tail=FALSE)
aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()								

p_value<-c(p_value,Amostra_10.000=aux)		#guardar valores

vals_10000<-vals					# informação QQ Plot

#---------------------------------------------------------------------------------------
# Amostra 100.000

  lambda <- 1.5
  nvals<-100000
  vals <- sim.poisson(lambda, nvals)


 frel <- table(vals) / length(vals)
 fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
 plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição de Poisson" , sub="Amostra 100.000", col="red")
 x <- seq(0,max(vals),1)
 lines(x,dpois(x,lambda), col="darkblue", type="p", pch=8)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0: X segue uma uma distribuição de Poisson (1.5)
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	             			# valores observados
pj <- dpois(seq(0,max(vals),1),1.5)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals

# correções para Ej<5

	while(Ej[which.min(Ej)]<5){
	
	posição<-which.min(Ej)
	
	Ej[posição-1]<-Ej[posição-1]+Ej[posição]
	Ej<-Ej[-posição]

	Nj[posição-1]<-Nj[posição-1]+Nj[posição]
	Nj<-Nj[-posição]

	pj[posição-1]<-pj[posição-1]+pj[posição]
	pj<-pj[-posição]

	}
	Ej
	Nj
	pj
						# frequência sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

pchisq(chi, df=gl, lower.tail=FALSE)
aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()								

p_value<-c(p_value,Amostra_100.000=aux)		# guardar valores

vals_100000<-vals	
#-----------------------------------------------------------------------------
#QQ plot

x11(title="Inversa - QQ Plot Po(1.5)")
 par(mfrow=c(2,2))


vals<-vals_100
nvals<-(length(vals))
qqplot(qpois(seq(0.5/nvals,1,1/nvals),lambda),sort(vals), main="QQ Plot Poisson", sub="Amostra 100", xlab="Quartis Teóricos", ylab="Quartis Amostra")
abline(a=0, b=1, lty=2, col="red")


vals<-vals_1000
nvals<-(length(vals))
qqplot(qpois(seq(0.5/nvals,1,1/nvals),lambda),sort(vals), main="QQ Plot Poisson", sub="Amostra 1.000", xlab="Quartis Teóricos", ylab="Quartis Amostra")
abline(a=0, b=1, lty=2, col="red")


vals<-vals_10000
nvals<-(length(vals))
qqplot(qpois(seq(0.5/nvals,1,1/nvals),lambda),sort(vals), main="QQ Plot Poisson", sub="Amostra 10.000", xlab="Quartis Teóricos", ylab="Quartis Amostra")
abline(a=0, b=1, lty=2, col="red")


vals<-vals_100000
nvals<-(length(vals))
qqplot(qpois(seq(0.5/nvals,1,1/nvals),lambda),sort(vals), main="QQ Plot Poisson", sub="Amostra 100.000", xlab="Quartis Teóricos", ylab="Quartis Amostra")
abline(a=0, b=1, lty=2, col="red")

#-----------------------------------------------------------------------------
p_value							# mostra p-value
p_value>0.05

#-----------------------------------------------------------------------------


