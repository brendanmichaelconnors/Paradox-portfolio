########################################################################################
# Fig1_syncXrichXeven.R
#
# Figure 1: Fishery and conservation consequences of stock diversity
# Last updated: July 5, 2019
# Author: B. Connors (DFO)
#        
########################################################################################

# --- Load simulation output -----------------------------------------------------------

# richness simulation output
mean_out <-readRDS(file = "output/richness_sync_300_0.8_0.6_0010_0.1_0_heteroP8.08March2021")

# calculate catch relative to maximum
max.catch_hetP_2 <- c(max(mean_out[1,,2]),max(mean_out[2,,2]),max(mean_out[3,,2]),max(mean_out[4,,2]),max(mean_out[5,,2]),max(mean_out[6,,2]),max(mean_out[7,,2]),max(mean_out[8,,2]),max(mean_out[9,,2]),max(mean_out[10,,2]))
max.catch <- cbind(max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2)

# evenness simulation output
mean_out_even <-readRDS(file = "output/even_sync_no_covary_300_0.8_0.6_0010_0.1_0_heteroP8.03March2021")

# calculate catch relative to maximum
max.catch_hetP_2_even <- c(max(mean_out_even[1,,2]),max(mean_out_even[2,,2]),max(mean_out_even[3,,2]),max(mean_out_even[4,,2]),max(mean_out_even[5,,2]),max(mean_out_even[6,,2]),max(mean_out_even[7,,2]),max(mean_out_even[8,,2]),max(mean_out_even[9,,2]),max(mean_out_even[10,,2]))
max.catch_even <- cbind(max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even,max.catch_hetP_2_even)
min_cor=0

# Plot
jpeg("figures/fig_1_25Mar2021.jpeg", width=5.5, height=7.5, units="in",res=800)
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
#image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = rev(viridis(100)))
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
#mtext("Stability (1/CV)",3,cex=0.9,line=0.5)
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
#image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = viridis(100))   
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
#mtext("  Risk (% stocks extirpated)",3,cex=0.9,line=0.5)
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
#image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = rev(viridis(100)))
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
#mtext("Yield (% max. harvest)",3,cex=0.9,line=0.5)
mtext("F",3,cex=1.2,line=-1,adj=-0.15,font=2)
#mtext("Synchrony",2,line=2.5)
mtext("Evenness (Pielou)",1,line=2.5)

image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = rev(viridis(100)), zlim=c(75,100))
box(col="grey")
dev.off()