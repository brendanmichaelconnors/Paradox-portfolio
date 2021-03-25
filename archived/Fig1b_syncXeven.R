########################################################################################
# Fig1_syncXrich.R
#
# Figure 1a: Fishery and conservation consequences of stock diversity
# Last updated: July 5, 2019
# Author: B. Connors (DFO)
#        
########################################################################################

# --- Load simulation output -----------------------------------------------------------
mean_out <-readRDS(file = "output/even_sync_100_0.8_0.6_0010_0.1_0_heteroP8.16Feb2021")
mean_out <-readRDS(file = "output/even_sync_100_0.8_0.6_0010_0.1_0_heteroP8.23Feb2021")

  max.catch_hetP_2 <- c(max(mean_out[1,,2]),max(mean_out[2,,2]),max(mean_out[3,,2]),max(mean_out[4,,2]),max(mean_out[5,,2]),max(mean_out[6,,2]),max(mean_out[7,,2]),max(mean_out[8,,2]),max(mean_out[9,,2]),max(mean_out[10,,2]))
  max.catch <- cbind(max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2)
  min_cor=0

jpeg("figures/fig_1b_phi_0.8.Rho_0.6.logFE_0.10.OU_0.hetP8.23Feb2021.jpeg",width=6, height=5.15, units="in",res=800)
#dev.new(width=6, height=5,new=FALSE)
  par(mfrow=c(2,2),bty="o", mar=c(3,3,2.5,5),oma=c(2,2,1,1))#set dimensions to plots

  long.data <- as.data.frame(as.table((mean_out[,,2]/max.catch)*100))
  x <- as.numeric(paste(long.data[,1]))+0.00000001
  y <- as.numeric(paste(long.data[,2]))+0.00000001
  z <-as.numeric(long.data[,3])
  data.loess.decline.10 = loess(z~x*y)
  grid = expand.grid(list(x = seq(1,10,length.out=100), y = seq(min_cor,1,length.out=100)))
  zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
  xx<-seq(1,10,length.out=100)
  yy<-seq(min_cor,1,length.out=100)
  
  image(xx,yy,zz,axes=F,ylab="",xlab="")
    axis(2,las=2,cex.axis=0.72)
    axis(1,at=c(1,2.8,4.6,6.4,8.2,10),cex.axis=0.72,labels = c(0.03,0.30,0.47,0.65,0.82,1))
    mtext("Yield (% max. harvest)",3,cex=0.9,line=0.5)
    mtext("A",3,cex=1.2,line=0.5,adj=-0.25,font=2)
    mtext("Synchrony",2,line=2.5)
    image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = rev(viridis(100)))
  
  long.data <- as.data.frame(as.table(1/mean_out[,,6]))
  x <- as.numeric(paste(long.data[,1]))+0.00000001
  y <- as.numeric(paste(long.data[,2]))+0.00000001
  z <-as.numeric(long.data[,3])
  data.loess.decline.10 = loess(z~x*y)
  grid = expand.grid(list(x = seq(1,10,length.out=100), y = seq(min_cor,1,length.out=100)))
  zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
  xx<-seq(1,10,length.out=100)
  yy<-seq(min_cor,1,length.out=100)
  
  image(xx,yy,zz,axes=F,ylab="",xlab="")
    axis(2,las=2,cex.axis=0.72)
    axis(1,at=c(1,2.8,4.6,6.4,8.2,10),cex.axis=0.72,labels = c(0.03,0.3,0.47,0.65,0.82,1))
    mtext("Stability (1/CV)",3,cex=0.9,line=0.5)
    mtext("B",3,cex=1.2,line=0.5,adj=-0.25,font=2)
  image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = rev(viridis(100)))
  
  long.data <- as.data.frame(as.table(mean_out[,,5]*100))
  x <- as.numeric(paste(long.data[,1]))+0.00000001
  y <- as.numeric(paste(long.data[,2]))+0.00000001
  z <-as.numeric(long.data[,3])
  data.loess.decline.10 = loess(z~x*y)
  grid = expand.grid(list(x = seq(1,10,length.out=100), y = seq(min_cor,1,length.out=100)))
  zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
  zz[zz<0] <- 0
  xx<-seq(1,10,length.out=100)
  yy<-seq(min_cor,1,length.out=100)
  
  image(xx,yy,zz,axes=F,ylab="",xlab="")
    axis(2,las=2,cex.axis=0.72)
    axis(1,at=c(1,2.8,4.6,6.4,8.2,10),cex.axis=0.72,labels = c(0.03,0.3,0.47,0.65,0.82,1))
    mtext("  Risk (% stocks extirpated)",3,cex=0.9,line=0.5)
    mtext("C",3,cex=1.2,line=0.5,adj=-0.25,font=2)
    mtext("Evenness index",1,line=-0.8,outer=T)
    mtext("Synchrony",2,line=2.5)
    image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = viridis(100))

dev.off()



mean_out <-readRDS(file = "output/even_sync_no_covary_300_0.8_0.6_0010_0.1_0_heteroP8.03March2021")

max.catch_hetP_2 <- c(max(mean_out[1,,2]),max(mean_out[2,,2]),max(mean_out[3,,2]),max(mean_out[4,,2]),max(mean_out[5,,2]),max(mean_out[6,,2]),max(mean_out[7,,2]),max(mean_out[8,,2]),max(mean_out[9,,2]),max(mean_out[10,,2]))
max.catch <- cbind(max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2)
min_cor=0

jpeg("figures/fig_1b_no_cov_phi_0.8.Rho_0.6.logFE_0.10.OU_0.hetP8.03March.jpeg",width=6, height=5.15, units="in",res=800)
#dev.new(width=6, height=5,new=FALSE)
par(mfrow=c(2,2),bty="o", mar=c(3,3,2.5,5),oma=c(2,2,1,1))#set dimensions to plots

long.data <- as.data.frame(as.table((mean_out[,,2]/max.catch)*100))
x <- as.numeric(paste(long.data[,1]))+0.00000001
y <- as.numeric(paste(long.data[,2]))+0.00000001
z <-as.numeric(long.data[,3])
data.loess.decline.10 = loess(z~x*y)
grid = expand.grid(list(x = seq(1,10,length.out=100), y = seq(min_cor,1,length.out=100)))
zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
xx<-seq(1,10,length.out=100)
yy<-seq(min_cor,1,length.out=100)

image(xx,yy,zz,axes=F,ylab="",xlab="")
axis(2,las=2,cex.axis=0.72)
axis(1,at=c(1,2.8,4.6,6.4,8.2,10),cex.axis=0.72,labels = c(0.03,0.30,0.47,0.65,0.82,1))
mtext("Yield (% max. harvest)",3,cex=0.9,line=0.5)
mtext("A",3,cex=1.2,line=0.5,adj=-0.25,font=2)
mtext("Synchrony",2,line=2.5)
image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = rev(viridis(100)))

long.data <- as.data.frame(as.table(1/mean_out[,,6]))
x <- as.numeric(paste(long.data[,1]))+0.00000001
y <- as.numeric(paste(long.data[,2]))+0.00000001
z <-as.numeric(long.data[,3])
data.loess.decline.10 = loess(z~x*y)
grid = expand.grid(list(x = seq(1,10,length.out=100), y = seq(min_cor,1,length.out=100)))
zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
xx<-seq(1,10,length.out=100)
yy<-seq(min_cor,1,length.out=100)

image(xx,yy,zz,axes=F,ylab="",xlab="")
axis(2,las=2,cex.axis=0.72)
axis(1,at=c(1,2.8,4.6,6.4,8.2,10),cex.axis=0.72,labels = c(0.03,0.3,0.47,0.65,0.82,1))
mtext("Stability (1/CV)",3,cex=0.9,line=0.5)
mtext("B",3,cex=1.2,line=0.5,adj=-0.25,font=2)
image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = rev(viridis(100)))

long.data <- as.data.frame(as.table(mean_out[,,5]*100))
x <- as.numeric(paste(long.data[,1]))+0.00000001
y <- as.numeric(paste(long.data[,2]))+0.00000001
z <-as.numeric(long.data[,3])
data.loess.decline.10 = loess(z~x*y)
grid = expand.grid(list(x = seq(1,10,length.out=100), y = seq(min_cor,1,length.out=100)))
zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
zz[zz<0] <- 0
xx<-seq(1,10,length.out=100)
yy<-seq(min_cor,1,length.out=100)

image(xx,yy,zz,axes=F,ylab="",xlab="")
axis(2,las=2,cex.axis=0.72)
axis(1,at=c(1,2.8,4.6,6.4,8.2,10),cex.axis=0.72,labels = c(0.03,0.3,0.47,0.65,0.82,1))
mtext("  Risk (% stocks extirpated)",3,cex=0.9,line=0.5)
mtext("C",3,cex=1.2,line=0.5,adj=-0.25,font=2)
mtext("Evenness index",1,line=-0.8,outer=T)
mtext("Synchrony",2,line=2.5)
image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = viridis(100))

dev.off()

