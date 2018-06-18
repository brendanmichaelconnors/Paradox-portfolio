
jpeg("figures/fig_4_heat_map_phi_0.8.Rho_0.6.logFE_0.27.OU_0.jpeg",width=7, height=3, units="in",res=800)
#dev.new(width=6, height=5,new=FALSE)
par(mfrow=c(1,2),bty="o", mar=c(3,3,2,5),oma=c(1,1,1,1))#set dimensions to plots

long.data <- as.data.frame(as.table(1/mean_out[,,6]))
x <- as.numeric(paste(long.data[,1]))+0.00000001
y <- as.numeric(paste(long.data[,2]))+0.00000001
z <-as.numeric(long.data[,3])
data.loess.decline.10 = loess(z~x*y)
grid = expand.grid(list(x = seq(4,22,length.out=100), y = seq(min_cor,1,length.out=100)))
zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
xx<-seq(4,22,length.out=100)
yy<-seq(min_cor,1,length.out=100)

image(xx,yy,zz,axes=F,ylab="",xlab="")
axis(2,las=2,cex.axis=0.9)
axis(1,cex.axis=0.9)
mtext("(a) Stability (1/CV)",3,cex=1,line=0.5)
mtext("Synchrony",2,line=2.5)
image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3)

long.data <- as.data.frame(as.table(mean_out[,,5]*100))
x <- as.numeric(paste(long.data[,1]))+0.00000001
y <- as.numeric(paste(long.data[,2]))+0.00000001
z <-as.numeric(long.data[,3])
data.loess.decline.10 = loess(z~x*y)
grid = expand.grid(list(x = seq(4,22,length.out=100), y = seq(min_cor,1,length.out=100)))
zz<-as.matrix(predict(data.loess.decline.10, newdata = grid))
zz[zz<0] <- 0
xx<-seq(4,22,length.out=100)
yy<-seq(min_cor,1,length.out=100)

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

image(xx,yy,zz,axes=F,ylab="",xlab="")
axis(2,las=2,cex.axis=0.9)
axis(1,cex.axis=0.9)
mtext("(b) Risk (% stocks extirpated)",3,cex=1,line=0.5)
mtext("Number of populations",1,line=-1,outer=T)
image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3)

dev.off()

