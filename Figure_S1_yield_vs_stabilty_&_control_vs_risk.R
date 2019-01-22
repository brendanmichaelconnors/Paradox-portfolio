indexx<- 90 # set index (or "slice"); columns are synchrony and rows are heterogenity in productivity
colors <- viridis(5)
risk_sims <-readRDS(file = "output/mgmt_risk_sims.100.phi_0.8.Rho_0.6.logFE_0.1.OU_0.Dec282018")

jpeg("figures/fig_S1_phi_0.8.Rho_0.6.logFE_0.1.OU_0.Jan212019.jpeg",width=6.5, height=3.25, units="in",res=800)
#dev.new(width=8, height=4,new=FALSE)
par(mfrow=c(1,2),bty="o", mar=c(2,2,2,2),oma=c(3,2,0,0))#set dimensions to plots

long_sims <- round(risk_sims, digits = 2)
long_sims$max.yld <- long_sims$harvest/max(long_sims$harvest)

sims_out <- subset(long_sims, control == 0 & MSY == 1)
x <- as.numeric(paste(sims_out[,1]))
y <- as.numeric(paste(sims_out[,2]))
z <-as.numeric(sims_out[,8]*100)
data.loess= loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
ext<-as.matrix(predict(data.loess, newdata = grid))

x <- as.numeric(paste(sims_out[,1]))+0.00000001
y <- as.numeric(paste(sims_out[,2]))+0.00000001
z <-as.numeric(1/sims_out[,4])
data.loess = loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
stability<-as.matrix(predict(data.loess, newdata = grid))

plot(stability[indexx,], ext[indexx,],col=colors[5],type="l",lwd=3,ylim=c(0,100),xlim=c(0.8,3),yaxt="n",ylab="",xlab="Stability (1/CV)")
axis(2,las=2)
mtext("Yield (% of maximum)",2,line=2.5)

sims_out <- subset(long_sims, control == 0.25 & MSY == 1)
x <- as.numeric(paste(sims_out[,1]))
y <- as.numeric(paste(sims_out[,2]))
z <-as.numeric(sims_out[,8]*100)
data.loess= loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
ext<-as.matrix(predict(data.loess, newdata = grid))

x <- as.numeric(paste(sims_out[,1]))+0.00000001
y <- as.numeric(paste(sims_out[,2]))+0.00000001
z <-as.numeric(1/sims_out[,4])
data.loess = loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
stability<-as.matrix(predict(data.loess, newdata = grid))

points(stability[indexx,], ext[indexx,],col=colors[3],type="l",lwd=3)

sims_out <- subset(long_sims, control == 0.5 & MSY == 1)
x <- as.numeric(paste(sims_out[,1]))
y <- as.numeric(paste(sims_out[,2]))
z <-as.numeric(sims_out[,8]*100)
data.loess= loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
ext<-as.matrix(predict(data.loess, newdata = grid))
x <- as.numeric(paste(sims_out[,1]))+0.00000001
y <- as.numeric(paste(sims_out[,2]))+0.00000001
z <-as.numeric(1/sims_out[,4])
data.loess = loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
stability<-as.matrix(predict(data.loess, newdata = grid))
ext[ext<0]<-0
points(stability[indexx,], ext[indexx,],col=colors[2],type="l",lwd=3)

sims_out <- subset(long_sims, control == 0.75 & MSY == 1)
x <- as.numeric(paste(sims_out[,1]))
y <- as.numeric(paste(sims_out[,2]))
z <-as.numeric(sims_out[,8]*100)
data.loess= loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
ext<-as.matrix(predict(data.loess, newdata = grid))

x <- as.numeric(paste(sims_out[,1]))+0.00000001
y <- as.numeric(paste(sims_out[,2]))+0.00000001
z <-as.numeric(1/sims_out[,4])
data.loess = loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
stability<-as.matrix(predict(data.loess, newdata = grid))

points(stability[indexx,], ext[indexx,],col=colors[1],type="l",lwd=3)

legend(1.9,50,c("0","0.25","0.5","0.75"),lwd=3,lty=1,col=c(colors[5],colors[3],colors[2],colors[1]),cex=0.8, bty="n")
text(2.4,54,"Mgmt. control (C*)",cex=0.8)
mtext("A",3,line=0.25,adj=-0.1,font=2,cex=1.3)

####


sims_out <- subset(long_sims, control == 0 & MSY == 1)
x <- as.numeric(paste(sims_out[,1]))
y <- as.numeric(paste(sims_out[,2]))
z <-as.numeric(sims_out[,8]*100) 
data.loess= loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
ext<-as.matrix(predict(data.loess, newdata = grid))

x <- as.numeric(paste(sims_out[,1]))+0.00000001
y <- as.numeric(paste(sims_out[,2]))+0.00000001
z <-as.numeric(1/sims_out[,4])
data.loess = loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
stability<-as.matrix(predict(data.loess, newdata = grid))

plot(stability[indexx,], ext[indexx,],col=colors[5],type="l",lwd=3,ylim=c(0,70),xlim=c(0.8,3),yaxt="n",ylab="",xlab="")
axis(2,las=2)
mtext("Stability (1/CV)",1,line=1,outer=T)
mtext("B",3,line=0.25,adj=-0.1,font=2,cex=1.3)

sims_out <- subset(long_sims, control == 0 & MSY == 0.75)
x <- as.numeric(paste(sims_out[,1]))
y <- as.numeric(paste(sims_out[,2]))
z <-as.numeric(sims_out[,8]*100)
data.loess= loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
ext<-as.matrix(predict(data.loess, newdata = grid))

x <- as.numeric(paste(sims_out[,1]))+0.00000001
y <- as.numeric(paste(sims_out[,2]))+0.00000001
z <-as.numeric(1/sims_out[,4])
data.loess = loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
stability<-as.matrix(predict(data.loess, newdata = grid))

points(stability[indexx,], ext[indexx,],col=colors[3],type="l",lwd=3)

sims_out <- subset(long_sims, control == 0 & MSY == 0.5)
x <- as.numeric(paste(sims_out[,1]))
y <- as.numeric(paste(sims_out[,2]))
z <-as.numeric(sims_out[,8]*100)
data.loess= loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
ext<-as.matrix(predict(data.loess, newdata = grid))
x <- as.numeric(paste(sims_out[,1]))+0.00000001
y <- as.numeric(paste(sims_out[,2]))+0.00000001
z <-as.numeric(1/sims_out[,4])
data.loess = loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
stability<-as.matrix(predict(data.loess, newdata = grid))
ext[ext<0]<-0
points(stability[indexx,], ext[indexx,],col=colors[2],type="l",lwd=3)

sims_out <- subset(long_sims, control == 0 & MSY == 0.25)
x <- as.numeric(paste(sims_out[,1]))
y <- as.numeric(paste(sims_out[,2]))
z <-as.numeric(sims_out[,8]*100)
data.loess= loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
ext<-as.matrix(predict(data.loess, newdata = grid))

x <- as.numeric(paste(sims_out[,1]))+0.00000001
y <- as.numeric(paste(sims_out[,2]))+0.00000001
z <-as.numeric(1/sims_out[,4])
data.loess = loess(z~x*y)
grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
stability<-as.matrix(predict(data.loess, newdata = grid))

points(stability[indexx,], ext[indexx,],col=colors[1],type="l",lwd=3)

legend(1.9,50,c("1","0.75","0.5","0.25"),lwd=3,lty=1,col=c(colors[5],colors[3],colors[2],colors[1]),cex=0.8, bty="n")
text(2.4,54,"Risk tolerance (P*)",cex=0.8)

dev.off()