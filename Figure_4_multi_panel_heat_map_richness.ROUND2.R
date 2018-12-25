
mean_out <-readRDS(file = "richness_sync_200_0.8_0.6_0010_0_0_heteroP2.23Dec2018")
min_cor<-0
max.catch <- c(max(mean_out[1,,2]),max(mean_out[2,,2]),max(mean_out[3,,2]),max(mean_out[4,,2]),max(mean_out[5,,2]),max(mean_out[6,,2]),max(mean_out[7,,2]),max(mean_out[8,,2]),max(mean_out[9,,2]),max(mean_out[10,,2]))
max.catch <- cbind(max.catch,max.catch,max.catch,max.catch,max.catch,max.catch,max.catch,max.catch,max.catch,max.catch)

jpeg("figures/fig_4_heat_map_phi_0.8.Rho_0.6.logFE_0.OU_0.hetP2.jpeg",width=6, height=5.15, units="in",res=800)
#dev.new(width=6, height=5,new=FALSE)
par(mfrow=c(2,2),bty="o", mar=c(3,3,2.5,5),oma=c(2,2,1,1))#set dimensions to plots
long.data <- as.data.frame(as.table((mean_out[,,2]/max.catch)*100))
x <- as.numeric(paste(long.data[,1]))+0.00000001
y <- as.numeric(paste(long.data[,2]))+0.00000001
z <-as.numeric(long.data[,3])
data.loess.decline.10 = loess(z~x*y)
grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
xx<-seq(6,31,length.out=100)
yy<-seq(min_cor,1,length.out=100)

image(xx,yy,zz,axes=F,ylab="",xlab="")
axis(2,las=2,cex.axis=0.9)
axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9)
mtext("Yield (% max. harvest)",3,cex=0.9,line=0.5)
mtext("A",3,cex=1.2,line=0.5,adj=-0.25,font=2)
mtext("Synchrony",2,line=2.5)
image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3)

long.data <- as.data.frame(as.table(1/mean_out[,,6]))
x <- as.numeric(paste(long.data[,1]))+0.00000001
y <- as.numeric(paste(long.data[,2]))+0.00000001
z <-as.numeric(long.data[,3])
data.loess.decline.10 = loess(z~x*y)
grid = expand.grid(list(x = seq(6,31,length.out=100), y = seq(min_cor,1,length.out=100)))
zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
xx<-seq(6,31,length.out=100)
yy<-seq(min_cor,1,length.out=100)

image(xx,yy,zz,axes=F,ylab="",xlab="")
axis(2,las=2,cex.axis=0.9)
axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9)
mtext("Stability (1/CV)",3,cex=0.9,line=0.5)
mtext("B",3,cex=1.2,line=0.5,adj=-0.25,font=2)
image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3)

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

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

image(xx,yy,zz,axes=F,ylab="",xlab="")
axis(2,las=2,cex.axis=0.9)
axis(1,at=c(6,11,16,21,26,31),cex.axis=0.9)
mtext("  Risk (% stocks extirpated)",3,cex=0.9,line=0.5)
mtext("C",3,cex=1.2,line=0.5,adj=-0.25,font=2)
mtext("Number of populations",1,line=-0.8,outer=T)
mtext("Synchrony",2,line=2.5)
image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3)

dev.off()

