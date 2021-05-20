# Distribuição Z=X+Y (método:Rejeição)

set.seed(0) #garantir que se tem sempre a mesma solução

reject.Z <- function(nvals,lambda,n)
{
	
	# função probabilidade Z
	
	pontos<-n
	fpz <- NULL
	fpz[1] <- dpois(0,lambda)*1/3
	fpz[2] <- dpois(1,lambda)*1/3+dpois(0,lambda)*1/3
	for(i in 2:max(pontos)) {
    	fpz[i+1] <- dpois(i,lambda)*1/3+dpois(i-1,lambda)*1/3+dpois(i-2,lambda)*1/3
	}


	z.aceite <- NULL
	u.aceite <- NULL
	nz <- 0
	z.rejected <- NULL
	u.rejected <- NULL
	nrejected <- 0
	while(nz < nvals) {
		Y <- trunc(n*runif(1))
 		U <- runif(1)
		
		if(U <= fpz[Y+1]) { #Y+1 porque os vetores não têm indice 0
			nz <- nz + 1
			z.aceite[nz] <- Y
			u.aceite[nz] <- U
		}
		else {
			nrejected <- nrejected + 1
			z.rejected[nrejected] <- Y
			u.rejected[nrejected] <- U
		}
	}
	plot(z.aceite,u.aceite,pch=20,col="green",xlab="z",ylab="h(z)",main="Método da Rejeição")
	lines(z.rejected,u.rejected,pch=20,col="red",type="p")
	z.aceite
}

x11(title="Rejeição - Distribuição Z = X + Y")
par(mfrow=c(4,2))

#Inputs
lambda<-1.5
n<-10

#-------------------------------------------------------------------------------------------
#  Amostra 10

nvals<-10
vals <- reject.Z(nvals,lambda,n)
frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="z", ylab="h(z)", main="Distribuição Z = X + Y" , sub="Amostra 10", col="skyblue")
x <- seq(0,max(vals),1)

# função probabilidade Z

fpz <- NULL
fpz[1] <- dpois(0,lambda)*1/3
fpz[2] <- dpois(1,lambda)*1/3+dpois(0,lambda)*1/3
for(i in 2:max(vals)) {
    fpz[i+1] <- dpois(i,lambda)*1/3+dpois(i-1,lambda)*1/3+dpois(i-2,lambda)*1/3
}
lines(x,fpz, col="darkblue", type="p", pch=8)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0: X segue uma distribuição Z = X + Y
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           			# valores observados
pj <- fpz		
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

#---------------------------------------------------------------------------------------
#  Amostra 100

nvals <- 100
vals <- reject.Z(nvals,lambda,n)
frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="z", ylab="h(z)", main="Distribuição Z = X + Y"  , sub="Amostra 100", col="orange")
x <- seq(0,max(vals),1)

# função probabilidade Z

fpz <- NULL
fpz[1] <- dpois(0,lambda)*1/3
fpz[2] <- dpois(1,lambda)*1/3+dpois(0,lambda)*1/3
for(i in 2:max(vals)) {
    fpz[i+1] <- dpois(i,lambda)*1/3+dpois(i-1,lambda)*1/3+dpois(i-2,lambda)*1/3
}
lines(x,fpz, col="darkblue", type="p", pch=8)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0: X segue uma distribuição Z = X + Y
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))						# valores observados
pj <- fpz	
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
vals <- reject.Z(nvals,lambda,n)
frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="z", ylab="h(z)", main="Distribuição Z = X + Y" , sub="Amostra 1.000", col="green")
x <- seq(0,max(vals),1)

# função probabilidade Z

fpz <- NULL
fpz[1] <- dpois(0,lambda)*1/3
fpz[2] <- dpois(1,lambda)*1/3+dpois(0,lambda)*1/3
for(i in 2:max(vals)) {
    fpz[i+1] <- dpois(i,lambda)*1/3+dpois(i-1,lambda)*1/3+dpois(i-2,lambda)*1/3
}
lines(x,fpz, col="darkblue", type="p", pch=8)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0: X segue uma distribuição Z = X + Y
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))						# valores observados
pj <- fpz	
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

p_value<-c(p_value,Amostra_1.000=aux)		#guardar valores

#---------------------------------------------------------------------------------------
#  Amostra 10.000
 
nvals <- 10000
vals <- reject.Z(nvals,lambda,n)
frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="z", ylab="h(z)", main="Distribuição Z = X + Y" , sub="Amostra 10.000", col="green")
x <- seq(0,max(vals),1)

# função probabilidade Z

fpz <- NULL
fpz[1] <- dpois(0,lambda)*1/3
fpz[2] <- dpois(1,lambda)*1/3+dpois(0,lambda)*1/3
for(i in 2:max(vals)) {
    fpz[i+1] <- dpois(i,lambda)*1/3+dpois(i-1,lambda)*1/3+dpois(i-2,lambda)*1/3
}
lines(x,fpz, col="darkblue", type="p", pch=8)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hipótese H_0: X segue uma distribuição Z = X + Y
# Hipótese H_1: H_0 é falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))						# valores observados
pj <- fpz	
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

p_value<-c(p_value,Amostra_10.000=aux)		#guardar valores

#----------------------------------------------------------------------------------	
#------------------------------------------------------------------------------
p_value							# mostra p-value
p_value>0.05



 
