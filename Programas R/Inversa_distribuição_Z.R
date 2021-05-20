# Distribuição Z= X + Y(método:Inversa)

set.seed(0) #garantir que se tem sempre a mesma solução

#Resolução 1: Simulação da distribuição Z = X + Y
 
 sim.Z <- function(lambda,nvals)
{
	X.vals <- NULL
	Y.vals <- NULL
	

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
		
		u <- runif(1)
		if(u <= 1/3) {y <- 0}
			else if(u <= 2/3){y <- 1}
			else {y <- 2}
		
		X.vals[j] <- i
		Y.vals[j] <- y
		
	}
	
	Z<-X.vals+Y.vals
}

x11(title="Resolução 1: Distribuição Z = X + Y")
par(mfrow=c(2,2))


#-------------------------------------------------------------------------------------------
#  Amostra 100

lambda <- 1.5
nvals<-100

vals <- sim.Z(lambda,nvals)
frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="z", ylab="h(z)", main="Distribuição Z = X + Y" , sub="Amostra 100", col="skyblue")
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

p_value<-c(Amostra_100=aux)				#guardar valores

vals_100<-vals						# informação QQ Plot

#---------------------------------------------------------------------------------------
#  Amostra 1.000

lambda <- 1.5
nvals<-1000
vals <- sim.Z(lambda, nvals)
nvals<-1000

frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="z", ylab="h(z)", main="Distribuição Z = X + Y" , sub="Amostra 1.000", col="orange")
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

vals_1000<-vals						# informação QQ Plot
#---------------------------------------------------------------------------------------
#  Amostra 10.000

lambda <- 1.5
nvals<-10000
vals <- sim.Z(lambda, nvals)

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
gl <- length(Ej)-1     

pchisq(chi, df=gl, lower.tail=FALSE)
aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()								

p_value<-c(p_value,Amostra_10.000=aux)		#guardar valores

vals_10000<-vals						# informação QQ Plot
#---------------------------------------------------------------------------------------
# Amostra 100.000

lambda <- 1.5
nvals<-100000
vals <- sim.Z(lambda,nvals)

frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="z", ylab="h(z)", main="Distribuição Z = X + Y" , sub="Amostra 100.000", col="red")
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
Nj<-(c(N0,Nj))	             			# valores observados
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

p_value<-c(p_value,Amostra_100.000=aux)		# guardar valores

p_value_r1<-p_value		

vals_100000<-vals						# informação QQ Plot					
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Resolução 2: Simulação da distribuição Z

sim.Z2 <- function(nvals,lambda)

{
	
	# função probabilidade Z
	
	pontos<-11
	fpz <- NULL
	fpz[1] <- dpois(0,lambda)*1/3
	fpz[2] <- dpois(1,lambda)*1/3+dpois(0,lambda)*1/3
	for(i in 2:max(pontos)) {
    	fpz[i+1] <- dpois(i,lambda)*1/3+dpois(i-1,lambda)*1/3+dpois(i-2,lambda)*1/3
	}
	
	#cum.probs<-cumsum(fpz)
	
	X.vals <- NULL
	for(j in 1:nvals) {
		u <- runif(1)
		i <- 0
		p <- fpz[1]
		F <- p
		while (F <= u) {
			i <- i + 1
			p = fpz[i+1]
			F <- F + p
		}
		X.vals[j] <- i
	}
	X.vals
}



x11(title="Resolução 2: Distribuição Z= X + Y")
par(mfrow=c(2,2))


#-------------------------------------------------------------------------------------------
#  Amostra 100

nvals <- 100
lambda<-1.5

vals <- sim.Z2(nvals,lambda)

frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="z", ylab="h(z)", main="Distribuição Z = X + Y" , sub="Amostra 100", col="skyblue")
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

p_value<-c(Amostra_100=aux)				#guardar valores

#---------------------------------------------------------------------------------------
#  Amostra 1.000

nvals <- 1000
lambda<-1.5

vals <- sim.Z2(nvals,lambda)


frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="z", ylab="h(z)", main="Distribuição Z = X + Y" , sub="Amostra 1.000", col="orange")
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
lambda<-1.5

vals <- sim.Z2(nvals,lambda)

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
gl <- length(Ej)-1     

pchisq(chi, df=gl, lower.tail=FALSE)
aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()								

p_value<-c(p_value,Amostra_10.000=aux)		#guardar valores


#---------------------------------------------------------------------------------------
#  Amostra 100.000

lambda <- 1.5
nvals<-100000

vals <- sim.Z2(nvals,lambda)

frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="z", ylab="h(z)", main="Distribuição Z = X + Y" , sub="Amostra 100.000", col="red")
x <- seq(0,max(vals),1)

# função de distribuição Z

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
Nj<-(c(N0,Nj))	             			# valores observados
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

p_value<-c(p_value,Amostra_100.000=aux)		# guardar valores

p_value_r2<-p_value							

#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
set.seed(0) #garantir que se tem sempre a mesma solução

#Resolução 3: Simulação da distribuição Z

sim.Z3 <- function(nvals,lambda)

{
	X.vals <- NULL
	
	# função probabilidade Z

	pontos<-10
	fpz <- NULL
	fpz[1] <- dpois(0,lambda)*1/3
	fpz[2] <- dpois(1,lambda)*1/3+dpois(0,lambda)*1/3
	for(i in 2:max(pontos)) {
    	fpz[i+1] <- dpois(i,lambda)*1/3+dpois(i-1,lambda)*1/3+dpois(i-2,lambda)*1/3
	}
	
	cum.probs<-cumsum(fpz)

	for(j in 1:nvals) {
			u <- runif(1)
			x <- sum(u>=cum.probs) #retorna o n.º de intervalos em que a condição é verdadeira
			X.vals[j] <- x
	}
	X.vals
}



x11(title="Resolução 3: Distribuição Z = X + Y")
par(mfrow=c(2,2))


#-------------------------------------------------------------------------------------------
#  Amostra 100

lambda <- 1.5
nvals<-100

vals <- sim.Z3(nvals,lambda)
frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="z", ylab="h(z)", main="Distribuição Z = X + Y" , sub="Amostra 100", col="skyblue")
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

p_value<-c(Amostra_100=aux)				#guardar valores

#---------------------------------------------------------------------------------------
#  Amostra 1.000

lambda <- 1.5
nvals<-1000
vals <- sim.Z3(nvals,lambda)
nvals<-1000

frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="z", ylab="h(z)", main="Distribuição Z = X + Y" , sub="Amostra 1.000", col="orange")
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

lambda <- 1.5
nvals<-10000
vals <- sim.Z3(nvals,lambda)


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
gl <- length(Ej)-1     

pchisq(chi, df=gl, lower.tail=FALSE)
aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a função chis.test()								

p_value<-c(p_value,Amostra_10.000=aux)		#guardar valores


#---------------------------------------------------------------------------------------
#  Amostra 100.000

lambda <- 1.5
nvals<-100000
vals <- sim.Z3(nvals,lambda)

frel <- table(vals) / length(vals)
fmax <- max(dpois(trunc(lambda)+1,lambda),max(frel)) + 0.05
plot(frel, ylim=c(0,fmax), xlab="z", ylab="h(z)", main="Distribuição Z = X + Y" , sub="Amostra 100.000", col="red")
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
Nj<-(c(N0,Nj))	             			# valores observados
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

p_value<-c(p_value,Amostra_100.000=aux)		# guardar valores

p_value_r3<-p_value

#----------------------------------------------------------------------------------	
#-----------------------------------------------------------------------------
#QQ plot - resolução 1

x11(title="Inversa r1 - QQ Plot Z = X +Y")
 par(mfrow=c(2,2))

vals<-vals_100
nvals<-(length(vals))
poisson<- rpois(nvals,lambda)
uniform<-sample(0:2,nvals,replace=T)
f.vals<-poisson+uniform
qqplot(f.vals, vals, main="QQ Plot Z = X +Y", sub="Amostra 100",xlab="Quartis Teóricos",
  ylab="Quartis Amostra")
abline(a=0, b=1, lty=2, col="red")

vals<-vals_1000
nvals<-(length(vals))
poisson<- rpois(nvals,lambda)
uniform<-sample(0:2,nvals,replace=T)
f.vals<-poisson+uniform
qqplot(f.vals, vals, main="QQ Plot Z = X +Y", sub="Amostra 1.000",xlab="Quartis Teóricos",
  ylab="Quartis Amostra")
abline(a=0, b=1, lty=2, col="red")


vals<-vals_10000
nvals<-(length(vals))
poisson<- rpois(nvals,lambda)
uniform<-sample(0:2,nvals,replace=T)
f.vals<-poisson+uniform
qqplot(f.vals, vals, main="QQ Plot Z = X +Y", sub="Amostra 10.000",,xlab="Quartis Teóricos",
  ylab="Quartis Amostra")
abline(a=0, b=1, lty=2, col="red")


vals<-vals_100000
nvals<-(length(vals))
poisson<- rpois(nvals,lambda)
uniform<-sample(0:2,nvals,replace=T)
f.vals<-poisson+uniform
qqplot(f.vals, vals, main="QQ Plot Z = X +Y", sub="Amostra 100.000",xlab="Quartis Teóricos",
  ylab="Quartis Amostra")
abline(a=0, b=1, lty=2, col="red")


#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
p_value_r1							# mostra p-value
p_value_r1>0.05

p_value_r2							
p_value_r2>0.05

p_value_r3							
p_value_r3>0.05
						


