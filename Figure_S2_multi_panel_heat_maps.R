# Read in simulations, process
sim_hetP_2 <-readRDS(file = "output/richness_sync_200_0.8_0.6_0010_0_0_heteroP2.15Feb2019")
max.catch_hetP_2 <- c(max(sim_hetP_2[1,,2]),max(sim_hetP_2[2,,2]),max(sim_hetP_2[3,,2]),max(sim_hetP_2[4,,2]),max(sim_hetP_2[5,,2]),max(sim_hetP_2[6,,2]),max(sim_hetP_2[7,,2]),max(sim_hetP_2[8,,2]),max(sim_hetP_2[9,,2]),max(sim_hetP_2[10,,2]))
max.catch_hetP_2 <- cbind(max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2,max.catch_hetP_2)

sim_hetP_8 <-readRDS(file = "output/richness_sync_200_0.8_0.6_0010_0_0_heteroP8.15Feb2019")
max.catch_hetP_8 <- c(max(sim_hetP_8[1,,2]),max(sim_hetP_8[2,,2]),max(sim_hetP_8[3,,2]),max(sim_hetP_8[4,,2]),max(sim_hetP_8[5,,2]),max(sim_hetP_8[6,,2]),max(sim_hetP_8[7,,2]),max(sim_hetP_8[8,,2]),max(sim_hetP_8[9,,2]),max(sim_hetP_8[10,,2]))
max.catch_hetP_8 <- cbind(max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8,max.catch_hetP_8)

sim_hetP_6 <-readRDS(file = "output/richness_sync_200_0.8_0.6_0010_0_0_heteroP5.15Feb2019")
max.catch_hetP_6 <- c(max(sim_hetP_6[1,,2]),max(sim_hetP_6[2,,2]),max(sim_hetP_6[3,,2]),max(sim_hetP_6[4,,2]),max(sim_hetP_6[5,,2]),max(sim_hetP_6[6,,2]),max(sim_hetP_6[7,,2]),max(sim_hetP_6[8,,2]),max(sim_hetP_6[9,,2]),max(sim_hetP_6[10,,2]))
max.catch_hetP_6 <- cbind(max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6,max.catch_hetP_6)

min_cor<-0




jpeg("figures/fig_S2_phi_0.8.Rho_0.6.logFE_0.1.OU_0.Feb162019.jpeg",width=8, height=7, units="in",res=800)
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
  
  image(xx,yy,zz,axes=F,ylab="",xlab="",col = rev(viridis(100)), zlim=c(70,100))
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
  
  image(xx,yy,zz,axes=F,ylab="",xlab="",col = rev(viridis(100)), zlim=c(70,100))
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
  image.plot(xx,yy,zz,axes=F,add=T,legend.mar=3,col = rev(viridis(100)), zlim=c(70,100))
  
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
  mtext("Richness (no. of populations)",1,line=2.5)
  
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



