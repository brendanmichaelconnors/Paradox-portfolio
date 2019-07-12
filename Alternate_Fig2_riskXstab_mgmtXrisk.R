########################################################################################
# Alternate_Fig2_riskXstab_mgmtXrisk.R
#
# Figure 2: Fishery and conservation consequences of stock diversity
# Last updated: July 5, 2019
# Author: B. Connors (DFO)
#        
########################################################################################

# --- Load simulation output -----------------------------------------------------------
risk_sims <-readRDS(file = "output/mgmt_risk_sims.100.phi_0.8.Rho_0.6.logFE_0.1.OU_0.12July2019")
long_sims <- round(risk_sims, digits = 2)
long_sims$max.yld <- long_sims$harvest/max(long_sims$harvest)
indexx<- 50 # set index (or "slice"); columns are synchrony and rows are heterogenity in productivity
colors <- viridis(4)
min_cor = 0

jpeg("figures/ALT_fig_2_phi_0.8.Rho_0.6.logFE_0.1.OU_0.11July2019.jpeg",width=5.5, height=5.5, units="in",res=800)
#dev.new(width=8, height=4,new=FALSE)
par(mfrow=c(2,2),bty="o", mar=c(2,2,2,2),oma=c(3,2,0,0))#set dimensions to plots

  # Panel A
  sims_out <- subset(long_sims, control == 0 & MSY == 1)
    x <- as.numeric(paste(sims_out[,1]))
    y <- as.numeric(paste(sims_out[,2]))
    z <-as.numeric(sims_out[,3]*100)
    data.loess= loess(z~x*y)
    grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
    ext<-as.matrix(predict(data.loess, newdata = grid))
    ext[ext<0]<-0
    x <- as.numeric(paste(sims_out[,1]))+0.00000001
    y <- as.numeric(paste(sims_out[,2]))+0.00000001
    z <-as.numeric(1/sims_out[,4])
    data.loess = loess(z~x*y)
    grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
    stability<-as.matrix(predict(data.loess, newdata = grid))
    plot(stability[indexx,], ext[indexx,],col=colors[4],type="l",lwd=3,ylim=c(0,21),xlim=c(0.8,2.4),yaxt="n",ylab="Risk (% extirpated)",xlab="Stability (1/CV)")
 
  axis(2,las=2)
  mtext("Risk (% extirpated)",2,line=2.5)
  
  sims_out <- subset(long_sims, control == 0.12 & MSY == 1)
    x <- as.numeric(paste(sims_out[,1]))
    y <- as.numeric(paste(sims_out[,2]))
    z <-as.numeric(sims_out[,3]*100)
    data.loess= loess(z~x*y)
    grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
    ext<-as.matrix(predict(data.loess, newdata = grid))
    ext[ext<0]<-0
    x <- as.numeric(paste(sims_out[,1]))+0.00000001
    y <- as.numeric(paste(sims_out[,2]))+0.00000001
    z <-as.numeric(1/sims_out[,4])
    data.loess = loess(z~x*y)
    grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
    stability<-as.matrix(predict(data.loess, newdata = grid))
    points(stability[indexx,], ext[indexx,],col=colors[3],type="l",lwd=3)
  
  sims_out <- subset(long_sims, control == 0.25 & MSY == 1)
    x <- as.numeric(paste(sims_out[,1]))
    y <- as.numeric(paste(sims_out[,2]))
    z <-as.numeric(sims_out[,3]*100)
    data.loess= loess(z~x*y)
    grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
    ext<-as.matrix(predict(data.loess, newdata = grid))
    ext[ext<0]<-0
    x <- as.numeric(paste(sims_out[,1]))+0.00000001
    y <- as.numeric(paste(sims_out[,2]))+0.00000001
    z <-as.numeric(1/sims_out[,4])
    data.loess = loess(z~x*y)
    grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
    stability<-as.matrix(predict(data.loess, newdata = grid))
    points(stability[indexx,], ext[indexx,],col=colors[2],type="l",lwd=3)
  
  sims_out <- subset(long_sims, control == 0.5 & MSY == 1)
    x <- as.numeric(paste(sims_out[,1]))
    y <- as.numeric(paste(sims_out[,2]))
    z <-as.numeric(sims_out[,3]*100)
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
    points(stability[indexx,], ext[indexx,],col=colors[1],type="l",lwd=3)
  
  legend(1.8,20,c("0","0.125","0.25","0.5"),lwd=3,lty=1,col=c(colors[4],colors[3],colors[2],colors[1]),cex=0.8, bty="n")
  text(2,20,"Mgmt. control (C*)",cex=0.8)
  mtext("A",3,line=0.2,adj=-0.1,font=2,cex=1.3)

  # Panel B
  sims_out <- subset(long_sims, control == 0 & MSY == 1)
    x <- as.numeric(paste(sims_out[,1]))
    y <- as.numeric(paste(sims_out[,2]))
    z <-as.numeric(sims_out[,3]*100)
    data.loess= loess(z~x*y)
    grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
    ext<-as.matrix(predict(data.loess, newdata = grid))
    ext[ext<0]<-0
    x <- as.numeric(paste(sims_out[,1]))+0.00000001
    y <- as.numeric(paste(sims_out[,2]))+0.00000001
    z <-as.numeric(1/sims_out[,4])
    data.loess = loess(z~x*y)
    grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
    stability<-as.matrix(predict(data.loess, newdata = grid))
    plot(stability[indexx,], ext[indexx,],col=colors[4],type="l",lwd=3,ylim=c(0,21),xlim=c(0.8,2.4),yaxt="n",ylab="",xlab="")

  axis(2,las=2)
  mtext("B",3,line=0.2,adj=-0.1,font=2,cex=1.3)
  
  sims_out <- subset(long_sims, control == 0 & MSY == 0.88)
    x <- as.numeric(paste(sims_out[,1]))
    y <- as.numeric(paste(sims_out[,2]))
    z <-as.numeric(sims_out[,3]*100)
    data.loess= loess(z~x*y)
    grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
    ext<-as.matrix(predict(data.loess, newdata = grid))
    ext[ext<0]<-0
    x <- as.numeric(paste(sims_out[,1]))+0.00000001
    y <- as.numeric(paste(sims_out[,2]))+0.00000001
    z <-as.numeric(1/sims_out[,4])
    data.loess = loess(z~x*y)
    grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
    stability<-as.matrix(predict(data.loess, newdata = grid))
    points(stability[indexx,], ext[indexx,],col=colors[3],type="l",lwd=3)
  
  sims_out <- subset(long_sims, control == 0 & MSY == 0.75)
    x <- as.numeric(paste(sims_out[,1]))
    y <- as.numeric(paste(sims_out[,2]))
    z <-as.numeric(sims_out[,3]*100)
    data.loess= loess(z~x*y)
    grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
    ext<-as.matrix(predict(data.loess, newdata = grid))
    ext[ext<0]<-0
    x <- as.numeric(paste(sims_out[,1]))+0.00000001
    y <- as.numeric(paste(sims_out[,2]))+0.00000001
    z <-as.numeric(1/sims_out[,4])
    data.loess = loess(z~x*y)
    grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
    stability<-as.matrix(predict(data.loess, newdata = grid))
    points(stability[indexx,], ext[indexx,],col=colors[2],type="l",lwd=3)
  
  sims_out <- subset(long_sims, control == 0 & MSY == 0.5)
    x <- as.numeric(paste(sims_out[,1]))
    y <- as.numeric(paste(sims_out[,2]))
    z <-as.numeric(sims_out[,3]*100)
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
    points(stability[indexx,], ext[indexx,],col=colors[1],type="l",lwd=3)

  legend(1.8, 20,c("1","0.88","0.75","0.5"),lwd=3,lty=1,col=c(colors[4],colors[3],colors[2],colors[1]),cex=0.8, bty="n")
  text(2, 20,"OF tolerance (P*)",cex=0.8)

# Panel C
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
    plot(stability[indexx,], ext[indexx,],col=colors[4],type="l",lwd=3,ylim=c(0,100),xlim=c(0.8,2.4),yaxt="n",ylab="",xlab="Stability (1/CV)")

  axis(2,las=2)
  mtext("Yield (% of maximum)",2,line=2.5)
  
  sims_out <- subset(long_sims, control == 0.12 & MSY == 1)
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
    points(stability[indexx,], ext[indexx,],col=colors[2],type="l",lwd=3)
  
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
    points(stability[indexx,], ext[indexx,],col=colors[1],type="l",lwd=3)
  
  mtext("C",3,line=0.25,adj=-0.1,font=2,cex=1.3)
  
  # Panel D
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
    plot(stability[indexx,], ext[indexx,],col=colors[4],type="l",lwd=3,ylim=c(0,100),xlim=c(0.8,2.4),yaxt="n",ylab="",xlab="")

  axis(2,las=2)
  mtext("Stability (1/CV)",1,line=1,outer=T)
  mtext("D",3,line=0.25,adj=-0.1,font=2,cex=1.3)
  
  sims_out <- subset(long_sims, control == 0 & MSY == 0.88)
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
    points(stability[indexx,], ext[indexx,],col=colors[2],type="l",lwd=3)
  
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
    points(stability[indexx,], ext[indexx,],col=colors[1],type="l",lwd=3)
  
dev.off()