# Distribuição de Poisson (método:Rejeição)

set.seed(1) #garantir que se tem sempre a mesma solução

reject.poisson <- function(nvals,lambda,n)
{
	x.aceite <- NULL
	u.aceite <- NULL
	nx <- 0
	x.rejected <- NULL
	u.rejected <- NULL
	nrejected <- 0
	while(nx < nvals) {
		Y <- trunc(n*runif(1))
 		U <- runif(1)
		
		if(U <= dpois(Y,lambda)) {
 		#if(U <= (exp(-lambda)*lambda^Y)/factorial(Y)) {
		
			nx <- nx + 1
			x.aceite[nx] <- Y
			u.aceite[nx] <- U
		}
		else {
			nrejected <- nrejected + 1
			x.rejected[nrejected] <- Y
			u.rejected[nrejected] <- U
		}
	}
	plot(x.aceite,u.aceite,pch=20,col="green",xlab="x",ylab="f(x)",main="Método da Rejeição")
	lines(x.rejected,u.rejected,pch=20,col="red",type="p")
	x.aceite
}



 x11(title="Rejeição - Distribuição de Poisson Po(1.5)")
 par(mfrow=c(4,2))
 lambda<-1.5
#-------------------------------------------------------------------------------------------
#  Amostra 10

nvals <- 10
lambda<-1.5
n<-7
vals <- reject.poisson(nvals,lambda,n)
frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição de Poisson" , sub="Amostra 10", col="skyblue")
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
Ej<- pj*nvals						# frequência sob H_0

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

chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()

p_value<-c(Amostra_10=aux)				#guardar valores

#-----------------------------------------------------------------------------
#  Amostra 100

nvals <- 100
vals <- reject.poisson(nvals,lambda,n)
frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição de Poisson" , sub="Amostra 100", col="orange")
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
Ej<- pj*nvals						# frequência sob H_0

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

chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

pchisq(chi, df=gl, lower.tail=FALSE)
aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()								

p_value<-c(p_value,Amostra_100=aux)		#guardar valores

#---------------------------------------------------------------------------------------
#  Amostra 1.000

nvals <- 1000
vals <- reject.poisson(nvals,lambda,n)
frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição de Poisson" , sub="Amostra 1.000", col="green")
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
Ej<- pj*nvals						# frequência sob H_0

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

chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1     

pchisq(chi, df=gl, lower.tail=FALSE)
aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()								

p_value<-c(p_value,Amostra_1.000=aux)		#guardar valores

#---------------------------------------------------------------------------------------
# Amostra 10.0000 

nvals <- 10000
vals <- reject.poisson(nvals,lambda,n)
frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribuição de Poisson" , sub="Amostra 10.000", col="red")
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
Ej<- pj*nvals						# frequência sob H_0

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

chi <- sum((Nj-Ej)^2/Ej)                        # estatística de teste
gl <- length(Ej)-1                              # graus de liberdade

pchisq(chi, df=gl, lower.tail=FALSE)
aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()								

p_value<-c(p_value,Amostra_10.000=aux)		# guardar valores
#-----------------------------------------------------------------------------
p_value							# mostra p-value
p_value>0.05



