jpeg("figures/fig_3_kobe_phi_0.8.Rho_0.6.logFE_0.27.OU_0.jpeg",width=7, height=3.5, units="in",res=800)
#dev.new(width=7, height=3.25,new=FALSE)
par(mfrow=c(1,2),bty="o", mar=c(2,2,1,2),oma=c(2,2,1,1))#set dimensions to plots

alpha<- inputs$alphas[10,]
beta<- inputs$betas[10,]
Ro <- log(alpha)/beta; sum(Ro)

Ro <- log(alpha)/beta; sum(Ro)
phi <- 0.8
rho <- 0.6
#Preturn <- c(0.25,0.25,0.25,0.25)
Preturn <- c(0,0,1,0)
for.error <- 0.27 # from ben
OU <- 0 #0.2 #(collie and peterman high end YK chum)
control <- 0 #1 is perfect and 0 is none
MSY.add <- 1

source("MSY_hcr_function.R")		

out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)

smsy <- (1-lambert_W0(exp(1-log(alpha))))/beta
Uopt <- round(0.5* log(alpha)-0.07* log(alpha)^2,digits=2)
b_bmsy <- array(NA,dim=c(1,pops))
u_umsy <- array(NA,dim=c(1,pops))

for (w in 1:pops){
	b_bmsy[w] <- mean(out$S[48:51,w])/smsy[w]
	u_umsy[w] <- mean(out$H[48:51,w]/out$N[48:51,w])/Uopt[w]
}

plot(b_bmsy, u_umsy,xlim=c(-0.205,4), ylim=c(-0.25,4),col="white",yaxt="n",xaxt="n")
axis(1)
axis(2,las=2)

control <-0
for (l in 1: 10){
	out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
	for (w in 1:pops){
		b_bmsy[w] <- mean(out$S[48:51,w])/smsy[w]
		u_umsy[w] <- mean(out$H[48:51,w]/out$N[48:51,w])/Uopt[w]
	}
points(b_bmsy, u_umsy,pch=16,col="#00000048",cex=1.25)
}
abline(v=1,lty=2)
abline(h=1,lty=2)
mtext("(a) Mgmt. control = 0.125",line=0.5)

plot(b_bmsy, u_umsy,xlim=c(-0.25,4), ylim=c(-0.25,4),col="white",yaxt="n",xaxt="n")
axis(1)
axis(2,las=2)

control <-0.75
for (l in 1: 10){
	out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
	for (w in 1:pops){
		b_bmsy[w] <- mean(out$S[48:51,w])/smsy[w]
		u_umsy[w] <- mean(out$H[48:51,w]/out$N[48:51,w])/Uopt[w]
	}
points(b_bmsy, u_umsy,pch=16,col="#00000048",cex=1.25)
}
abline(v=1,lty=2)
abline(h=1,lty=2)
mtext("(b) Mgmt. control = 0.75",line=0.5)

mtext("U/Umsy",2,outer=T,line=0.5,cex=1.25)
mtext("S/Smsy",1,outer=T,line=0.5,cex=1.25)

dev.off()