# Distribui��o de Poisson (m�todo:Inversa)

set.seed(0) #garantir que se tem sempre a mesma solu��o
 
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

 x11(title="Inversa - Distribui��o de Poisson Po(1.5)")
 par(mfrow=c(2,2))
 
#-------------------------------------------------------------------------------------------
#  Amostra 100

 lambda <- 1.5
 nvals<-100
 vals <- sim.poisson(lambda, nvals)
 frel <- table(vals) / length(vals)
 fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
 plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribui��o de Poisson" , sub="Amostra 100", col="skyblue")
 x <- seq(0,max(vals),1)
 lines(x,dpois(x,lambda), col="darkblue", type="p", pch=8)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hip�tese H_0: X segue uma uma distribui��o de Poisson (1.5)
# Hip�tese H_1: H_0 � falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           			# valores observados
pj <- dpois(seq(0,max(vals),1),1.5)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals

# corre��es para Ej<5

	while(Ej[which.min(Ej)]<5){
	
	posi��o<-which.min(Ej)
	
	Ej[posi��o-1]<-Ej[posi��o-1]+Ej[posi��o]
	Ej<-Ej[-posi��o]

	Nj[posi��o-1]<-Nj[posi��o-1]+Nj[posi��o]
	Nj<-Nj[-posi��o]

	pj[posi��o-1]<-pj[posi��o-1]+pj[posi��o]
	pj<-pj[-posi��o]

	}
	Ej
	Nj
	pj

								# frequ�ncia sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estat�stica de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a fun��o chis.test()

p_value<-c(Amostra_100=aux)				#guardar valores

vals_100<-vals						# informa��o QQ Plot

#---------------------------------------------------------------------------------------
#  Amostra 1.000

 lambda <- 1.5
 nvals<-1000
 vals <- sim.poisson(lambda, nvals)

 frel <- table(vals) / length(vals)
 fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
 plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribui��o de Poisson" , sub="Amostra 1.000", col="orange")
 x <- seq(0,max(vals),1)
 lines(x,dpois(x,lambda), col="darkblue", type="p", pch=8)

# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hip�tese H_0: X segue uma uma distribui��o de Poisson (1.5)
# Hip�tese H_1: H_0 � falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))						# valores observados
pj <- dpois(seq(0,max(vals),1),1.5)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals

# corre��es para Ej<5

	while(Ej[which.min(Ej)]<5){
	
	posi��o<-which.min(Ej)
	
	Ej[posi��o-1]<-Ej[posi��o-1]+Ej[posi��o]
	Ej<-Ej[-posi��o]

	Nj[posi��o-1]<-Nj[posi��o-1]+Nj[posi��o]
	Nj<-Nj[-posi��o]

	pj[posi��o-1]<-pj[posi��o-1]+pj[posi��o]
	pj<-pj[-posi��o]

	}
	Ej
	Nj
	pj

						# frequ�ncia sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estat�stica de teste
gl <- length(Ej)-1                              # graus de liberdade

pchisq(chi, df=gl, lower.tail=FALSE)
aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a fun��o chis.test()								

p_value<-c(p_value,Amostra_1.000=aux)		#guardar valores

vals_1000<-vals						# informa��o QQ Plot

#---------------------------------------------------------------------------------------
#  Amostra 10.000
 lambda <- 1.5
 nvals<-10000
 vals <- sim.poisson(lambda, nvals)

 frel <- table(vals) / length(vals)
 fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
 plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribui��o de Poisson" , sub="Amostra 10.000", col="green")
 x <- seq(0,max(vals),1)
 lines(x,dpois(x,lambda), col="darkblue", type="p", pch=8)
 

# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hip�tese H_0: X segue uma uma distribui��o de Poisson (1.5)
# Hip�tese H_1: H_0 � falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           			# valores observados
pj <- dpois(seq(0,max(vals),1),1.5)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals


# corre��es para Ej<5

	while(Ej[which.min(Ej)]<5){
	
	posi��o<-which.min(Ej)
	
	Ej[posi��o-1]<-Ej[posi��o-1]+Ej[posi��o]
	Ej<-Ej[-posi��o]

	Nj[posi��o-1]<-Nj[posi��o-1]+Nj[posi��o]
	Nj<-Nj[-posi��o]

	pj[posi��o-1]<-pj[posi��o-1]+pj[posi��o]
	pj<-pj[-posi��o]

	}
	Ej
	Nj
	pj
						# frequ�ncia sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estat�stica de teste
gl <- length(Ej)-1     

pchisq(chi, df=gl, lower.tail=FALSE)
aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a fun��o chis.test()								

p_value<-c(p_value,Amostra_10.000=aux)		#guardar valores

vals_10000<-vals					# informa��o QQ Plot

#---------------------------------------------------------------------------------------
# Amostra 100.000

  lambda <- 1.5
  nvals<-100000
  vals <- sim.poisson(lambda, nvals)


 frel <- table(vals) / length(vals)
 fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
 plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribui��o de Poisson" , sub="Amostra 100.000", col="red")
 x <- seq(0,max(vals),1)
 lines(x,dpois(x,lambda), col="darkblue", type="p", pch=8)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hip�tese H_0: X segue uma uma distribui��o de Poisson (1.5)
# Hip�tese H_1: H_0 � falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	             			# valores observados
pj <- dpois(seq(0,max(vals),1),1.5)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals

# corre��es para Ej<5

	while(Ej[which.min(Ej)]<5){
	
	posi��o<-which.min(Ej)
	
	Ej[posi��o-1]<-Ej[posi��o-1]+Ej[posi��o]
	Ej<-Ej[-posi��o]

	Nj[posi��o-1]<-Nj[posi��o-1]+Nj[posi��o]
	Nj<-Nj[-posi��o]

	pj[posi��o-1]<-pj[posi��o-1]+pj[posi��o]
	pj<-pj[-posi��o]

	}
	Ej
	Nj
	pj
						# frequ�ncia sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estat�stica de teste
gl <- length(Ej)-1                              # graus de liberdade

pchisq(chi, df=gl, lower.tail=FALSE)
aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a fun��o chis.test()								

p_value<-c(p_value,Amostra_100.000=aux)		# guardar valores

vals_100000<-vals	
#-----------------------------------------------------------------------------
#QQ plot

x11(title="Inversa - QQ Plot Po(1.5)")
 par(mfrow=c(2,2))


vals<-vals_100
nvals<-(length(vals))
qqplot(qpois(seq(0.5/nvals,1,1/nvals),lambda),sort(vals), main="QQ Plot Poisson", sub="Amostra 100", xlab="Quartis Te�ricos", ylab="Quartis Amostra")
abline(a=0, b=1, lty=2, col="red")


vals<-vals_1000
nvals<-(length(vals))
qqplot(qpois(seq(0.5/nvals,1,1/nvals),lambda),sort(vals), main="QQ Plot Poisson", sub="Amostra 1.000", xlab="Quartis Te�ricos", ylab="Quartis Amostra")
abline(a=0, b=1, lty=2, col="red")


vals<-vals_10000
nvals<-(length(vals))
qqplot(qpois(seq(0.5/nvals,1,1/nvals),lambda),sort(vals), main="QQ Plot Poisson", sub="Amostra 10.000", xlab="Quartis Te�ricos", ylab="Quartis Amostra")
abline(a=0, b=1, lty=2, col="red")


vals<-vals_100000
nvals<-(length(vals))
qqplot(qpois(seq(0.5/nvals,1,1/nvals),lambda),sort(vals), main="QQ Plot Poisson", sub="Amostra 100.000", xlab="Quartis Te�ricos", ylab="Quartis Amostra")
abline(a=0, b=1, lty=2, col="red")

#-----------------------------------------------------------------------------
p_value							# mostra p-value
p_value>0.05

#-----------------------------------------------------------------------------


