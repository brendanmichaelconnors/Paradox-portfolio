##=============================================================================
## Multi panel plot with details within each region, for supp mat. 
## July 1, 2019 (Canada Day!)
##=============================================================================

setwd("~/Dropbox/SFU Postdoc/Skeena Work/Paradox of Portfolio/Panel Plotting")
library(MASS)
library(ggplot2)
library(gridExtra)
library(dplyr)

# -----------------------------------------------------------------------------
# Panel plot for sockey systems
# -----------------------------------------------------------------------------
setwd("~/Dropbox/SFU Postdoc/Skeena Work/Paradox of Portfolio/Panel Plotting")

systems <- c("Bristol Bay", "Fraser", "Skeena")
decades <- c("1970s", "1980s", "1990s", "2000s")
System.all <- list()

yr.range <- 10

# error if Cultus lake is in the csv for alpha / beta parameters or if Skeena
# has Azuklotz

for(j in 1:length(systems)) {
  temp.S <- read.csv(paste(systems[j], "_Smsy_", yr.range, "yrs.csv", sep=""), header=T)
  colnames(temp.S) <- c("lake", "meanSmsy", "SlowerCI", "SupperCI",
                        "S1970s", "S1980s", "S1990s", "S2000s", "SAll")
  temp.U <- read.csv(paste(systems[j], "_Umsy_", yr.range, "yrs.csv", sep=""), header=T)
  temp.params <- read.csv(paste("../Parameter Estimation/", systems[j], "/", 
                                systems[j], "_AlphaBeta_estimates.csv", sep=""))
  for(i in 1:length(decades)) {
    S.msy.temp <- cbind(temp.S[(4+i)]/temp.S$meanSmsy,
                        temp.S[(4+i)]/temp.S$SlowerCI,
                        temp.S[(4+i)]/temp.S$SupperCI)
    colnames(S.msy.temp) <- c(paste("S_Smsy_", decades[i], sep=""),
                              paste("SLCL_", decades[i], sep=""),
                              paste("SUCL_", decades[i], sep=""))
    temp.S <- cbind(temp.S, S.msy.temp)
    
    U.msy.temp <- cbind(temp.U[(4+i)]/temp.U$meanUmsy,
                        temp.U[(4+i)]/(temp.U$lowerCI),
                        temp.U[(4+i)]/(temp.U$upperCI))
    colnames(U.msy.temp) <- c(paste("U_Umsy_", decades[i], sep=""),
                              paste("ULCL_", decades[i], sep=""),
                              paste("UUCL_", decades[i], sep=""))
    temp.U <- cbind(temp.U, U.msy.temp)
    
    sys.name <- rep(systems[j], nrow(temp.U))
    
    
    
  }
  
  if(j==2) {temp.params <- subset(temp.params, stock!="Cultus")}
  if(j==3) {temp.params <- subset(temp.params, stock!="Azuklotz")}
  alpha.order <- rank(temp.params$alpha)
  System.all[[j]] <- cbind(system=sys.name, temp.S, temp.U, temp.params, 
                           alpha.order)
  
}

all.dat <- do.call(rbind.data.frame, System.all)


# -----------------------------------------------------------------------------
# Just detailed point information for each system - 6 panel
# -----------------------------------------------------------------------------

png("SuppMat_01.13.2020.png", width=1300, height=1300)
par(mfrow=c(3,2))
par(mar=c(5.1, 5.1, 2.1, 15.1))

for(i in c(1,3,2)) {
dat1 = all.dat[all.dat$system==systems[i],]

dat <- dat1 %>% 
  arrange(desc(SAll))

col.range <- colorRampPalette(c("red", "blue"))(nrow(dat))
# col.range <- paste(col.range, "99", sep="")


plot(x=dat$S_Smsy_1980s, y=dat$U_Umsy_1980s, bg=col.range[dat$alpha.order], 
     cex=sqrt(dat$SAll/10000), pch=21, xlim=c(0,4), ylim=c(0,2.5), col="black",
     xlab="S/Smsy", ylab="U/Umsy", main=systems[i], cex.axis=2, cex.lab=2, las=1,
     cex.main=2)
abline(h=1)
abline(v=1)
text(x=rep(4.2,nrow(dat)), y=seq(0,2.5,length.out=nrow(dat)),
     labels = dat$lake, pos=4, xpd=NA, cex=2)

for(j in 1:nrow(dat)) {
  lines(x=c(dat$S_Smsy_1980s[j], 4.22),
        y=c(dat$U_Umsy_1980s[j], seq(0,2.5,length.out=nrow(dat))[j]),
        xpd=NA)
  
  
}
  
plot(x=dat$S_Smsy_2000s, y=dat$U_Umsy_2000s, bg=col.range[dat$alpha.order], 
     cex=sqrt(dat$SAll/10000), pch=21, xlim=c(0,4), ylim=c(0,2.5), col="black",
     xlab="S/Smsy", ylab="", main=systems[i], cex.axis=2, cex.lab=2, las=1,
     cex.main=2)
abline(h=1)
abline(v=1)
text(x=rep(4.2,nrow(dat)), y=seq(0,2.5,length.out=nrow(dat)),
     labels = dat$lake, pos=4, xpd=NA, cex=2)

for(j in 1:nrow(dat)) {
  lines(x=c(dat$S_Smsy_2000s[j], 4.22),
        y=c(dat$U_Umsy_2000s[j], seq(0,2.5,length.out=nrow(dat))[j]),
        xpd=NA)
  
  
}  
}
dev.off()
