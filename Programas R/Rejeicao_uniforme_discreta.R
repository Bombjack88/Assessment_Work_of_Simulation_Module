# Distribui��o Uniforme Discreta (m�todo:Rejei��o)

set.seed(1) #garantir que se tem sempre a mesma solu��o

reject.uniform <- function(nvals)
{
	x.aceite <- NULL
	u.aceite <- NULL
	nx <- 0
	x.rejected <- NULL
	u.rejected <- NULL
	nrejected <- 0
	while(nx < nvals) {
		Y <- trunc(3*runif(1))
 		U <- runif(1)
		
 		if(U <= 1/3) {
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
	plot(x.aceite,u.aceite,pch=20,col="green",xlab="x",ylab="f(x)",main="M�todo da Rejei��o")
	lines(x.rejected,u.rejected,pch=20,col="red",type="p")
	x.aceite
}

 x11(title="Rejei��o - Distribui��o Uniforme Discreta")
 par(mfrow=c(4,2))


#-------------------------------------------------------------------------------------------
#  Amostra 10
nvals <- 10
vals <- reject.uniform(nvals)
frel <- table(vals) / length(vals)
fmax <- max(frel) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribui��o Uniforme", sub="Amostra 10", col="brown4")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="darkblue", type="p", pch=1)

# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hip�tese H_0: X segue uma uma distribui��o uniforme discreta
# Hip�tese H_1: H_0 � falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           				# valores observados
pj <- c(1/3,1/3,1/3)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals						# frequ�ncia sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estat�stica de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a fun��o chis.test()

p_value<-c(Amostra_10=aux)				#guardar valores


#-------------------------------------------------------------------------------------------
#  Amostra 100

nvals <- 100
vals <- reject.uniform(nvals)
frel <- table(vals) / length(vals)
fmax <- max(frel) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribui��o Uniforme", sub="Amostra 100", col="orange")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="darkblue", type="p", pch=1)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hip�tese H_0: X segue uma uma distribui��o Uniforme discreta
# Hip�tese H_1: H_0 � falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           				# valores observados
pj <- c(1/3,1/3,1/3)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals						# frequ�ncia sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estat�stica de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a fun��o chis.test()

p_value<-c(p_value,Amostra_100=aux)				#guardar valores
#---------------------------------------------------------------------------------------
#  Amostra 1.000


nvals <- 1000
vals <- reject.uniform(nvals)
frel <- table(vals) / length(vals)
fmax <- max(frel) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)", main="Distribui��o Uniforme", sub="Amostra 1.000", col="green")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="darkblue", type="p", pch=1)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hip�tese H_0: X segue uma uma distribui��o Uniforme Discreta
# Hip�tese H_1: H_0 � falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           				# valores observados
pj <- c(1/3,1/3,1/3)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals						# frequ�ncia sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estat�stica de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a fun��o chis.test()

p_value<-c(p_value,Amostra_1.000=aux)		#guardar valores


#---------------------------------------------------------------------------------------
#  Amostra 10.000

nvals <- 10000
vals <- reject.uniform(nvals)
frel <- table(vals) / length(vals)
fmax <- max(frel) + 0.05
plot(frel, ylim=c(0,fmax), xlab="x", ylab="f(x)",  main="Distribui��o Uniforme", sub="Amostra 10.000", col="red")
x <- seq(0,max(vals),1)
lines(x,c(1/3,1/3,1/3), col="darkblue", type="p", pch=1)


# Teste do Qui-Quadrado (teste da qualidade de ajustamento)

# Hip�tese H_0:X segue uma uma distribui��o Uniforme Discreta
# Hip�tese H_1: H_0 � falsa
 
Nj<-table(vals)  
N0<-Nj[1]
Nj<-tabulate(vals)
Nj<-(c(N0,Nj))	           				# valores observados
pj <- c(1/3,1/3,1/3)		
pj<- pj[-length(Nj)]
pj<- c(pj,1-sum(pj))
Ej<- pj*nvals						# frequ�ncia sob H_0
chi <- sum((Nj-Ej)^2/Ej)                        # estat�stica de teste
gl <- length(Ej)-1                              # graus de liberdade

aux<-pchisq(chi, df=gl, lower.tail=FALSE)	
aux								# manual
chisq.test(Nj, p=pj)					# usando a fun��o chis.test()

p_value<-c(p_value,Amostra_10.000=aux)		#guardar valores


#------------------------------------------------------------------------------
p_value							# mostra p-value
p_value>0.05
