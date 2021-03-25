########################################################################################
# figures.R
#
# Create all simulation based figures 
#        
########################################################################################

# --- Figure 1 synchrony X richness X evenness -----------------------------------------

# load simulation output
mean_out <-readRDS(file = "output/richness_sync_300_0.8_0.6_0010_0.1_0_heteroP8.08March2021")
mean_out_even <-readRDS(file = "output/even_sync_no_covary_300_0.8_0.6_0010_0.1_0_heteroP8.03March2021")

# calculate catch relative to maximum
max.catch_hetP_2 <- c(max(mean_out[1,,2]),max(mean_out[2,,2]),max(mean_out[3,,2]),max(mean_out[4,,2]),max(mean_out[5,,2]),max(mean_out[6,,2]),max(mean_out[7,,2]),max(mean_out[8,,2]),max(mean_out[9,,2]),max(mean_out[10,,2]))
max.catch <- cbind(max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2)

max.catch_hetP_2_even <- c(max(mean_out_even[1,,2]),max(mean_out_even[2,,2]),max(mean_out_even[3,,2]),max(mean_out_even[4,,2]),max(mean_out_even[5,,2]),max(mean_out_even[6,,2]),max(mean_out_even[7,,2]),max(mean_out_even[8,,2]),max(mean_out_even[9,,2]),max(mean_out_even[10,,2]))
max.catch_even <- cbind(max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even)

min_cor=0

# create figure
jpeg("figures/Figure_1.jpeg", width=5.5, height=7.5, units="in",res=800)
  #dev.new(width=6, height=5,new=FALSE)
  par(mfrow=c(3,2),bty="o", mar=c(2,1,2.5,2.8),oma=c(3,3,1,3))#set dimensions to plots
  
  # Panel A 
  long.data <- as.data.frame(as.table(1/mean_out[,,6]))
  x <- as.numeric(paste(long.data[,1]))+0.00000001
  y <- as.numeric(paste(long.data[,2]))+0.00000001
  z <-as.numeric(long.data[,3])
  data.loess.decline.10 = loess(z~x*y)
  grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
  zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
  xx<-seq(6,31,length.out=100)
  yy<-seq(min_cor,1,length.out=100)
  
  image(xx,yy,zz,axes=F,ylab="",xlab="",col = rev(viridis(100)), zlim=c(0.8,2.2))
    axis(2,las=2,cex.axis=0.9)
    axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9, labels = FALSE)
  mtext("Stability (1/CV)",3,cex=0.9,line=1,adj=1.9)
  mtext("A",3,cex=1.2,line=-1,adj=-0.25,font=2)
  box(col="grey")
  
  # Panel B
  long.data <- as.data.frame(as.table(1/mean_out_even[,,6]))
  x <- as.numeric(paste(long.data[,1]))+0.00000001
  y <- as.numeric(paste(long.data[,2]))+0.00000001
  z <-as.numeric(long.data[,3])
  data.loess.decline.10 = loess(z~x*y)
  grid = expand.grid(list(x = seq(1,10,length.out=100), y = seq(min_cor,1,length.out=100)))
  zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
  xx<-seq(1,10,length.out=100)
  yy<-seq(min_cor,1,length.out=100)
  
  image(xx,yy,zz,axes=F,ylab="",xlab="")
    axis(2,las=2,cex.axis=0.9,labels = FALSE)
    axis(1,at=c(1,2.8,4.6,6.4,8.2,10),cex.axis=0.9,labels = FALSE)
  mtext("B",3,cex=1.2,line=-1,adj=-0.15,font=2)
  
  image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = rev(viridis(100)), zlim=c(0.8,2.2))
  box(col="grey")
  
  # Panel C    
  long.data <- as.data.frame(as.table(mean_out[,,5]*100))
  x <- as.numeric(paste(long.data[,1]))+0.00000001
  y <- as.numeric(paste(long.data[,2]))+0.00000001
  z <-as.numeric(long.data[,3])
  data.loess.decline.10 = loess(z~x*y)
  grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
  zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
  zz[zz<0] <- 0
  xx<-seq(6,31,length.out=100)
  yy<-seq(min_cor,1,length.out=100)
  
  image(xx,yy,zz,axes=F,ylab="",xlab="",col = viridis(100), zlim=c(0,45))
    axis(2,las=2,cex.axis=0.9)
    axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9, labels = FALSE)
  mtext("Risk (% stocks extirpated)",3,cex=0.9,line=1,adj=10.1)
  mtext("C",3,cex=1.2,line=-1,adj=-0.25,font=2)
  mtext("Synchrony",2,line=2.5)
  box(col="grey")
  
  # Panel D
  long.data <- as.data.frame(as.table(mean_out_even[,,5]*100))
  x <- as.numeric(paste(long.data[,1]))+0.00000001
  y <- as.numeric(paste(long.data[,2]))+0.00000001
  z <-as.numeric(long.data[,3])
  data.loess.decline.10 = loess(z~x*y)
  grid = expand.grid(list(x = seq(1,10,length.out=100), y = seq(min_cor,1,length.out=100)))
  zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
  zz[zz<0] <- 0
  xx<-seq(1,10,length.out=100)
  yy<-seq(min_cor,1,length.out=100)
  
  image(xx,yy,zz,axes=F,ylab="",xlab="",col = viridis(100), zlim=c(0,45))
    axis(2,las=2,cex.axis=0.9, labels = FALSE)
    axis(1,at=c(1,2.8,4.6,6.4,8.2,10),cex.axis=0.9,labels = FALSE)
  mtext("D",3,cex=1.2,line=-1,adj=-0.15,font=2)

  image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = viridis(100), zlim=c(0,45))
  box(col="grey")
  
  # Panel E
  long.data <- as.data.frame(as.table((mean_out[,,2]/max.catch)*100))
  x <- as.numeric(paste(long.data[,1]))+0.00000001
  y <- as.numeric(paste(long.data[,2]))+0.00000001
  z <-as.numeric(long.data[,3])
  data.loess.decline.10 = loess(z~x*y)
  grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
  zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
  xx<-seq(6,31,length.out=100)
  yy<-seq(min_cor,1,length.out=100)
  
  image(xx,yy,zz,axes=F,ylab="",xlab="",col = rev(viridis(100)), zlim=c(75,100))
    axis(2,las=2,cex.axis=0.9)
    axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9)
  mtext("Yield (% max. harvest)",3,cex=0.9,line=1,adj=3.9)
  mtext("E",3,cex=1.2,line=-1,adj=-0.25,font=2)
  mtext("Synchrony",2,line=2.5)
  mtext("Richness (no. stocks)",1,line=2.5)
  box(col="grey")
  
  # Panel F
  long.data <- as.data.frame(as.table((mean_out_even[,,2]/max.catch_even)*100))
  x <- as.numeric(paste(long.data[,1]))+0.00000001
  y <- as.numeric(paste(long.data[,2]))+0.00000001
  z <-as.numeric(long.data[,3])
  data.loess.decline.10 = loess(z~x*y)
  grid = expand.grid(list(x = seq(1,10,length.out=100), y = seq(min_cor,1,length.out=100)))
  zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
  xx<-seq(1,10,length.out=100)
  yy<-seq(min_cor,1,length.out=100)
  
  image(xx,yy,zz,axes=F,ylab="",xlab="",col = rev(viridis(100)), zlim=c(75,100))
    axis(2,las=2,cex.axis=0.9, labels = FALSE)
    axis(1,at=c(1,2.8,4.6,6.4,8.2,10),cex.axis=0.9,labels = c(0.03,0.3,0.47,0.65,0.82,1))
  mtext("F",3,cex=1.2,line=-1,adj=-0.15,font=2)
  mtext("Evenness (Pielou)",1,line=2.5)
  
  image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = rev(viridis(100)), zlim=c(75,100))
  box(col="grey")
  
dev.off()

# --- Figure 2 risk and yield X stability------------------------------------------------------------------

# load simulation output
risk_sims <-readRDS(file = "output/mgmt_risk_sims.200.phi_0.8.Rho_0.6.logFE_0.1.OU_0.08March2021")
long_sims <- round(risk_sims, digits = 2)
long_sims$max.yld <- long_sims$harvest/max(long_sims$harvest)
indexx<- 50 # set index (or "slice"); columns are synchrony and rows are heterogenity in productivity
colors <- viridis(4)
min_cor = 0

# create figure
jpeg("figures/Figure_2.jpeg",width=5, height=5.15, units="in",res=800)
  #dev.new(width=8, height=4,new=FALSE)
  par(mfrow=c(2,2),bty="o", mar=c(1,1,1,1),oma=c(4,3,2,0))#set dimensions to plots
  
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
  
  plot(stability[indexx,], ext[indexx,],col=colors[4],type="l",lwd=3,ylim=c(0,21),xlim=c(0.8,2.4),yaxt="n",xaxt="n",ylab="Risk (% extirpated)",xlab="Stability (1/CV)")
  axis(1,labels=F)
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
  
  box(col="grey")
  
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
  
  plot(stability[indexx,], ext[indexx,],col=colors[4],type="l",lwd=3,ylim=c(0,21),xlim=c(0.8,2.4),yaxt="n",xaxt="n",ylab="",xlab="")
  
  axis(1,labels = F)
  axis(2,las=2,labels = F)
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
  
  box(col="grey")
  
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
  
  box(col="grey")
  
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
  
  plot(stability[indexx,], ext[indexx,],col=colors[4],type="l",lwd=3,ylim=c(0,100),xlim=c(0.8,2.4),yaxt="n",xaxt="n",ylab="",xlab="")
  axis(1)
  axis(2,las=2,labels = F)
  mtext("Stability (1/CV)",1,line=1.5,outer=T)
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
  
  box(col="grey")

dev.off()

# --- Figure 3 Kobe plots --------------------------------------------------------------

# run simulations for plot
source("kobe_simulations.R")

#Define management control from simulations
HC <- "0.85"
LC <- "0"

# create figure
mycolors <- viridis(4)

p1 <- ggplot(data = full.sims.top[which(full.sims.top$control==LC),] ,aes(S_Smsy, F_Fmsy))+
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "top")+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,3))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"))+
  geom_point(color=mycolors[1],fill='blue',size=2,alpha=0.5,shape=16)+
  annotate("text", x=2.5, y=3,   label= "Mgmt. control = Low",size=2.5) + 
  annotate("text", x=2.5, y=2.65, label= "OF tolerance = High",size=2.5)  


p2 <- ggplot(data = full.sims.top[which(full.sims.top$control==HC),],aes(S_Smsy, F_Fmsy))+
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "top")+
  scale_y_continuous("",limits = c(0,3),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"))+
  geom_point(color=mycolors[3],fill=mycolors[3],size=2,alpha=0.5,shape=16)+
  annotate("text", x=2.5, y=3,   label= "Mgmt. control = High",size=2.5) + 
  annotate("text", x=2.5, y=2.65, label= "OF tolerance = High",size=2.5)  

p3 <- ggplot() + 
  geom_density(data=full.sims.top, aes(x=F_Fmsy, group=control,fill=control),alpha=0.5, adjust=4)+ 
  scale_fill_manual(values=c(mycolors[1], mycolors[3]))+
  coord_flip()+ 
  scale_x_continuous("",limits = c(0,3),position = "top",labels=NULL)+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+ 
  theme(legend.position="none",plot.margin = unit(c(1,4,-0.5,0), "lines"))+
  geom_vline(xintercept=1)

p4 <- ggplot(data = full.sims.bottom[which(full.sims.bottom$control==LC),],aes(S_Smsy, F_Fmsy))+
  scale_x_continuous("",limits = c(0,4),labels = NULL)+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,3))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(1,0.2,0,0), "lines"))+
  geom_point(color=mycolors[4],fill=mycolors[4],size=2,alpha=0.5,shape=16) +
  annotate("text", x=2.5, y=3,   label= "Mgmt. control = Low",size=2.5) + 
  annotate("text", x=2.5, y=2.65, label= "OF tolerance = Low",size=2.5) 

p5 <- ggplot( data = full.sims.bottom[which(full.sims.bottom$control==HC),],aes(S_Smsy, F_Fmsy))+
  scale_x_continuous("",limits = c(0,4),labels = NULL)+
  scale_y_continuous("",limits = c(0,3),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(1,0.2,0,0), "lines"))+
  geom_point(color=mycolors[2],fill=mycolors[2],size=2,alpha=0.5,shape=16) +
  annotate("text", x=2.5, y=3,   label= "Mgmt. control = High",size=2.5) + 
  annotate("text", x=2.5, y=2.65, label= "OF tolerance = Low",size=2.5) 

p6 <- ggplot() + 
  geom_density(data=full.sims.bottom, aes(x=F_Fmsy, group=control,fill=control),alpha=0.5, adjust=4)+ 
  scale_fill_manual(values=c(mycolors[4], mycolors[2]))+
  coord_flip()+ 
  scale_x_continuous(limits = c(0,3))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+ 
  theme(legend.position="none",plot.margin = unit(c(1,4,1,0), "lines"))+
  geom_vline(xintercept=1)

p7 <- ggplot() + 
  geom_density(data=btml, aes(x=S_Smsy, group=risk,fill=risk),alpha=0.5, adjust=2.5)+ 
  scale_fill_manual(values=c(mycolors[1], mycolors[4]))+
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+ 
  theme(legend.position="none",plot.margin = unit(c(-0.75,-0.5,4,1.35), "lines"))+
  geom_vline(xintercept=1)+ 
  scale_y_reverse()

p8 <- ggplot() + 
  geom_density(data=btmr, aes(x=S_Smsy, group=risk,fill=risk),alpha=0.5, adjust=4)+ 
  scale_fill_manual(values=c(mycolors[3], mycolors[2]))+
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+ 
  theme(legend.position="none",plot.margin = unit(c(-0.75,-0.5,4,0.75), "lines"))+
  geom_vline(xintercept=1)+ 
  scale_y_reverse()

jpeg("figures/Figure_3.jpeg",width=6, height=6, units="in",res=800)
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8, labels = c("A", "B", "C", "D", "E", "F", "G", "H"))
dev.off()

# --- Figure S1  --------------------------------------------------------------

# load simulation output
sim_hetP_2 <-readRDS(file = "output/richness_sync_300_0.8_0.6_0010_0.1_0_heteroP2.08March2021")
  max.catch_hetP_2 <- c(max(sim_hetP_2[1,,2]),max(sim_hetP_2[2,,2]),max(sim_hetP_2[3,,2]),max(sim_hetP_2[4,,2]),max(sim_hetP_2[5,,2]),max(sim_hetP_2[6,,2]),max(sim_hetP_2[7,,2]),max(sim_hetP_2[8,,2]),max(sim_hetP_2[9,,2]),max(sim_hetP_2[10,,2]))
  max.catch_hetP_2 <- cbind(max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2)

sim_hetP_8 <-readRDS(file = "output/richness_sync_300_0.8_0.6_0010_0.1_0_heteroP8.08March2021")
  max.catch_hetP_8 <- c(max(sim_hetP_8[1,,2]),max(sim_hetP_8[2,,2]),max(sim_hetP_8[3,,2]),max(sim_hetP_8[4,,2]),max(sim_hetP_8[5,,2]),max(sim_hetP_8[6,,2]),max(sim_hetP_8[7,,2]),max(sim_hetP_8[8,,2]),max(sim_hetP_8[9,,2]),max(sim_hetP_8[10,,2]))
  max.catch_hetP_8 <- cbind(max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8)

sim_hetP_6 <-readRDS(file = "output/richness_sync_300_0.8_0.6_0010_0.1_0_heteroP5.08March2021")
  max.catch_hetP_6 <- c(max(sim_hetP_6[1,,2]),max(sim_hetP_6[2,,2]),max(sim_hetP_6[3,,2]),max(sim_hetP_6[4,,2]),max(sim_hetP_6[5,,2]),max(sim_hetP_6[6,,2]),max(sim_hetP_6[7,,2]),max(sim_hetP_6[8,,2]),max(sim_hetP_6[9,,2]),max(sim_hetP_6[10,,2]))
  max.catch_hetP_6 <- cbind(max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6)

min_cor<-0

# generate figure
jpeg("figures/Figures_S1.jpeg",width=8, height=7, units="in",res=800)
#dev.new(width=8, height=10,new=FALSE)

  # Set up plotting region
  m <- rbind(
    cbind(0.05,0.33,0.7,1),
    cbind(0.33,0.61,0.7,1),
    cbind(0.61,0.95,0.7,1),
    
    cbind(0.05,0.33,0.4,0.7),
    cbind(0.33,0.61,0.4,0.7),
    cbind(0.61,0.95,0.4,0.7),
    
    cbind(0.05,0.33,0.1,0.4),
    cbind(0.33,0.61,0.1,0.4),
    cbind(0.61,0.95,0.1,0.4),
    
    cbind(0,1,0,1)
  )
  
  split.screen(m)
  
  # ROW 1: Yield
  
  screen(1)
    par(mar = c(1,1.5,1,1))
    long.data <- as.data.frame(as.table((sim_hetP_2[,,2]/max.catch_hetP_2)*100))
    x <- as.numeric(paste(long.data[,1]))+0.00000001
    y <- as.numeric(paste(long.data[,2]))+0.00000001
    z <-as.numeric(long.data[,3])
    data.loess.decline.10 = loess(z~x*y)
    grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
    zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
    xx<-seq(6,31,length.out=100)
    yy<-seq(min_cor,1,length.out=100)
    
    image(xx,yy,zz,axes=F,ylab="",xlab="",col = rev(viridis(100)), zlim=c(60,100))
    axis(2,las=2,cex.axis=0.9)
    axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9,labels = F)
    mtext("Var. in productivity = low",3,line=0, cex=0.9)
  
  screen(2)
    par(mar = c(1,1.5,1,1))
    long.data <- as.data.frame(as.table((sim_hetP_6[,,2]/max.catch_hetP_6*100)))
    x <- as.numeric(paste(long.data[,1]))+0.00000001
    y <- as.numeric(paste(long.data[,2]))+0.00000001
    z <-as.numeric(long.data[,3])
    data.loess.decline.10 = loess(z~x*y)
    grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
    zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
    xx<-seq(6,31,length.out=100)
    yy<-seq(min_cor,1,length.out=100)
    
    image(xx,yy,zz,axes=F,ylab="",xlab="",col = rev(viridis(100)), zlim=c(60,100))
    axis(2,las=2,cex.axis=0.9,labels = F)
    axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9,labels = F)
    mtext("Var. in productivity = medium",3,line=0, cex=0.9)
  
  screen(3)           
    par (mar=c(1,1.5,1,4))
    long.data <- as.data.frame(as.table((sim_hetP_8[,,2]/max.catch_hetP_8*100)))
    x <- as.numeric(paste(long.data[,1]))+0.00000001
    y <- as.numeric(paste(long.data[,2]))+0.00000001
    z <-as.numeric(long.data[,3])
    data.loess.decline.10 = loess(z~x*y)
    grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
    zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
    xx<-seq(6,31,length.out=100)
    yy<-seq(min_cor,1,length.out=100)
    
    image(xx,yy,zz,axes=F,ylab="",xlab="")
    mtext("Yield (% of maximum)",4,line=4)
    mtext("Var. in productivity = high",3,line=0, cex=0.9)
    axis(2,las=2,cex.axis=0.9,labels = F)
    axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9,labels = F)
    image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = rev(viridis(100)), zlim=c(60,100))
  
  # ROW 2: Stability
  
  screen(4)
    par(mar = c(1,1.5,1,1))
    long.data <- as.data.frame(as.table(1/sim_hetP_2[,,6]))
    x <- as.numeric(paste(long.data[,1]))+0.00000001
    y <- as.numeric(paste(long.data[,2]))+0.00000001
    z <-as.numeric(long.data[,3])
    data.loess.decline.10 = loess(z~x*y)
    grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
    zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
    xx<-seq(6,31,length.out=100)
    yy<-seq(min_cor,1,length.out=100)
    
    image(xx,yy,zz,axes=F,ylab="",xlab="",col = rev(viridis(100)),zlim=c(0.75,2.6))
    mtext("Synchrony",2,line=2.5)
    axis(2,las=2,cex.axis=0.9)
    axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9,labels = F)
  
  screen(5)
    par(mar = c(1,1.5,1,1))
    long.data <- as.data.frame(as.table(1/sim_hetP_6[,,6]))
    x <- as.numeric(paste(long.data[,1]))+0.00000001
    y <- as.numeric(paste(long.data[,2]))+0.00000001
    z <-as.numeric(long.data[,3])
    data.loess.decline.10 = loess(z~x*y)
    grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
    zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
    xx<-seq(6,31,length.out=100)
    yy<-seq(min_cor,1,length.out=100)
    
    image(xx,yy,zz,axes=F,ylab="",xlab="",col = rev(viridis(100)),zlim=c(0.75,2.6))
    axis(2,las=2,cex.axis=0.9,labels = F)
    axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9,labels = F)
  
  screen(6)
    par (mar=c(1,1.5,1,4))
    long.data <- as.data.frame(as.table(1/sim_hetP_8[,,6]))
    x <- as.numeric(paste(long.data[,1]))+0.00000001
    y <- as.numeric(paste(long.data[,2]))+0.00000001
    z <-as.numeric(long.data[,3])
    data.loess.decline.10 = loess(z~x*y)
    grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
    zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
    xx<-seq(6,31,length.out=100)
    yy<-seq(min_cor,1,length.out=100)
    
    image(xx,yy,zz,axes=F,ylab="",xlab="",col = viridis(100))
    mtext("Catch stability (1/CV)",4,line=4)
    axis(2,las=2,cex.axis=0.9,labels = F)
    axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9,labels = F)
    image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = rev(viridis(100)),zlim=c(0.75,2.6))
  
  # ROW 3: Risk
  screen(7)
    par(mar = c(1,1.5,1,1))
    long.data <- as.data.frame(as.table(sim_hetP_2[,,5]*100))
    x <- as.numeric(paste(long.data[,1]))+0.00000001
    y <- as.numeric(paste(long.data[,2]))+0.00000001
    z <-as.numeric(long.data[,3])
    data.loess.decline.10 = loess(z~x*y)
    grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
    zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
    zz[zz<0] <- 0
    xx<-seq(6,31,length.out=100)
    yy<-seq(min_cor,1,length.out=100)
    
    image(xx,yy,zz,axes=F,ylab="",xlab="",col = viridis(100), zlim=c(0,25))
    axis(2,las=2,cex.axis=0.9)
    axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9)
  
  screen(8)
    par(mar = c(1,1.5,1,1))
    long.data <- as.data.frame(as.table(sim_hetP_6[,,5]*100))
    x <- as.numeric(paste(long.data[,1]))+0.00000001
    y <- as.numeric(paste(long.data[,2]))+0.00000001
    z <-as.numeric(long.data[,3])
    data.loess.decline.10 = loess(z~x*y)
    grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
    zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
    zz[zz<0] <- 0
    xx<-seq(6,31,length.out=100)
    yy<-seq(min_cor,1,length.out=100)
    
    image(xx,yy,zz,axes=F,ylab="",xlab="",col = viridis(100), zlim=c(0,25))
    axis(2,las=2,cex.axis=0.9,labels = F)
    axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9)
    mtext("Richness (no. of stocks)",1,line=2.5)
  
  screen(9)
    par (mar=c(1,1.5,1,4))
    long.data <- as.data.frame(as.table(sim_hetP_8[,,5]*100))
    x <- as.numeric(paste(long.data[,1]))+0.00000001
    y <- as.numeric(paste(long.data[,2]))+0.00000001
    z <-as.numeric(long.data[,3])
    data.loess.decline.10 = loess(z~x*y)
    grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
    zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
    zz[zz<0] <- 0
    xx<-seq(6,31,length.out=100)
    yy<-seq(min_cor,1,length.out=100)
    
    image(xx,yy,zz,axes=F,ylab="",xlab="")
    mtext("Risk (% stocks ext.)",4,line=4)
    axis(2,las=2,cex.axis=0.9,labels = F)
    axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9)
    
    image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = viridis(100), zlim=c(0,25))
  
  close.screen(all.screens = TRUE)
dev.off()

# --- Figure S2----------------------------------------------------------------------------------------------
# load simulation output 
risk_sims <-readRDS(file = "output/mgmt_risk_sims.200.phi_0.8.Rho_0.6.logFE_0.1.OU_0.08March2021")
  long_sims <- round(risk_sims , digits = 2)
  min_cor = 0
  colors <- viridis(4)

# generate figure

jpeg("figures/Figure_S2.jpeg",width=8, height=7, units="in",res=800)
#dev.new(width=8, height=10,new=FALSE)

  # Set up plotting region
  m <- rbind(
    cbind(0.05,0.35,0.7,1),
    cbind(0.35,0.65,0.7,1),
    cbind(0.65,0.95,0.7,1),
    
    cbind(0.05,0.35,0.4,0.7),
    cbind(0.35,0.65,0.4,0.7),
    cbind(0.65,0.95,0.4,0.7),
    
    cbind(0.05,0.35,0.1,0.4),
    cbind(0.35,0.65,0.1,0.4),
    cbind(0.65,0.95,0.1,0.4),
    
    cbind(0,1,0,1)
  )
  
  split.screen(m)
  
  screen(1)
    par(mar = c(1,1.5,1,1))
    risk_stab_plot(long_sims, "FALSE", "TRUE", "FALSE", 1, 20, 1)
    mtext("Var. in productivity = low",3,line=0, cex=0.9)
  
  screen(2)
    par(mar = c(1,1.5,1,1))
    risk_stab_plot(long_sims, "FALSE", "FALSE", "FALSE", 1, 50, 1)
    mtext("Var. in productivity = medium",3,line=0, cex=0.9)
  
  screen(3)
    par(mar = c(1,1.5,1,1))
    risk_stab_plot(long_sims, "FALSE", "FALSE", "FALSE", 1, 90, 1)
    mtext("Var. in productivity = high",3,line=0, cex=0.9)
    mtext("OF tolerance = high",4,line=0.5)
  
  screen(4)
    par(mar = c(1,1.5,1,1))
    risk_stab_plot(long_sims, "TRUE", "TRUE", "FALSE", 0.75, 20, 2)
    mtext("Risk (% stocks extirpated)",2,line=2)
  
  screen(5)
    par(mar = c(1,1.5,1,1))
    risk_stab_plot(long_sims, "FALSE", "FALSE", "FALSE", 0.75, 50, 2)
  
  screen(6)
    par(mar = c(1,1.5,1,1))
    risk_stab_plot(long_sims, "FALSE", "FALSE", "FALSE", 0.75, 90, 2)
    mtext("OF tolerance = medium",4,line=0.5)
  
  screen(7)
    par(mar = c(1,1.5,1,1))
    risk_stab_plot(long_sims, "FALSE", "TRUE", "TRUE", 0.5, 20, 3)
  
  screen(8)
    par(mar = c(1,1.5,1,1))
    risk_stab_plot(long_sims, "FALSE", "FALSE", "TRUE", 0.5, 50, 3)
    mtext("Catch stability (1/CV)",1,line=2.5)
  
  screen(9)
    par(mar = c(1,1.5,1,1))
    risk_stab_plot(long_sims, "FALSE", "FALSE", "TRUE", 0.5, 90, 3)
    mtext("OF tolerance = low",4,line=0.5)
  
  close.screen(all.screens = TRUE)
  
dev.off()


# --- Figure S3 ------------------------------------------------------------------------------

# load simulation output
risk_sims <-readRDS(file = "output/mgmt_risk_sims.200.phi_0.8.Rho_0.6.logFE_0.1.OU_0.08March2021")
  long_sims <- round(risk_sims , digits = 2)
  min_cor = 0
  colors <- viridis(4)

# generate figure
jpeg("figures/Figure_S3.jpeg",width=8, height=7, units="in",res=800)
  #dev.new(width=8, height=10,new=FALSE)
  
  # Set up plotting region
  m <- rbind(
    cbind(0.05,0.35,0.7,1),
    cbind(0.35,0.65,0.7,1),
    cbind(0.65,0.95,0.7,1),
    
    cbind(0.05,0.35,0.4,0.7),
    cbind(0.35,0.65,0.4,0.7),
    cbind(0.65,0.95,0.4,0.7),
    
    cbind(0.05,0.35,0.1,0.4),
    cbind(0.35,0.65,0.1,0.4),
    cbind(0.65,0.95,0.1,0.4),
    
    cbind(0,1,0,1)
  )
  
  split.screen(m)
  
  # ROW 1: Yield
  
  screen(1)
    par(mar = c(1,1.5,1,1))
    yield_stab_plot(long_sims, "FALSE", "TRUE", "FALSE", 1, 20, 1)
    mtext("Var. in productivity = low",3,line=0, cex=0.9)
  
  screen(2)
    par(mar = c(1,1.5,1,1))
    yield_stab_plot(long_sims, "FALSE", "FALSE", "FALSE", 1, 50, 1)
    mtext("Var. in productivity = medium",3,line=0, cex=0.9)
  
  screen(3)
    par(mar = c(1,1.5,1,1))
    yield_stab_plot(long_sims, "FALSE", "FALSE", "FALSE", 1, 90, 1)
    mtext("Var. in productivity = high",3,line=0, cex=0.9)
    mtext("OF tolerance = high",4,line=0.5)
  
  screen(4)
    par(mar = c(1,1.5,1,1))
    yield_stab_plot(long_sims, "FALSE", "TRUE", "FALSE", 0.75, 20, 2)
    mtext("Yield (% of maximum)",2,line=2.5)
  
  screen(5)
    par(mar = c(1,1.5,1,1))
    yield_stab_plot(long_sims, "FALSE", "FALSE", "FALSE", 0.75, 50, 2)
  
  screen(6)
    par(mar = c(1,1.5,1,1))
    yield_stab_plot(long_sims, "FALSE", "FALSE", "FALSE", 0.75, 90, 2)
    mtext("OF tolerance = medium",4,line=0.5)
  
  screen(7)
    par(mar = c(1,1.5,1,1))
    yield_stab_plot(long_sims, "TRUE", "TRUE", "TRUE", 0.5, 20, 3)
  
  screen(8)
    par(mar = c(1,1.5,1,1))
    yield_stab_plot(long_sims, "FALSE", "FALSE", "TRUE", 0.5, 50, 3)
    mtext("Catch stability (1/CV)",1,line=2.5)
  
  screen(9)
    par(mar = c(1,1.5,1,1))
    yield_stab_plot(long_sims, "FALSE", "FALSE", "TRUE", 0.5, 90, 3)
    mtext("OF tolerance = low",4,line=0.5)
  
  close.screen(all.screens = TRUE)
  
  dev.off()
  



