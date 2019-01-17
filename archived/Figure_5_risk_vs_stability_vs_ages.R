indexx<- 90 # set index (or "slice"); columns are synchrony and rows are heterogenity in productivity
rownames(ext)[indexx]
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
colors <- jet.colors(5)

jpeg("figures/fig_5_age_slices_phi_0.8.Rho_0.6.logFE_0.27.OU_0.jpeg",width=4, height=4, units="in",res=800)
#dev.new(width=4, height=4,new=FALSE)
par(mfrow=c(1,1),bty="o", mar=c(2,2,2,2),oma=c(3,2,0,0))#set dimensions to plots

long_sims <- round(master.sims_out, digits = 2)

sims_out <- subset(long_sims, age == 0)
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

plot(stability[indexx,], ext[indexx,],col=colors[5],type="l",lwd=3,ylim=c(0,25),xlim=c(0.8,3.5),yaxt="n",ylab="Risk (% extirpated)",xlab="Stability (1/CV)")
axis(2,las=2)
mtext("Risk (% extirpated)",2,line=2.5)
mtext("Stability (1/CV)",1,line=2.5)

sims_out <- subset(long_sims, age == 0.5)
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

points(stability[indexx,], ext[indexx,],col=colors[4],type="l",lwd=3)

sims_out <- subset(long_sims, age == 0.33)
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

points(stability[indexx,], ext[indexx,],col=colors[3],type="l",lwd=3)

sims_out <- subset(long_sims, age == 0.25)
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
points(stability[indexx,], ext[indexx,],col=colors[2],type="l",lwd=3)

legend(1.8,16,c("[0,0,1,0]","[0,0.5,0.5,0]","[0,0.33,0.33,0.33]","[0.25,0.25,0.25,0.25]"),lwd=3,lty=1,col=c(colors[5],colors[4],colors[3],colors[2]),cex=0.8, bty="n")
text(2.6,17,"Age structure",cex=0.8)

dev.off()