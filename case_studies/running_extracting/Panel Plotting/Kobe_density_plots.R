##=============================================================================
## Multi panel plot attempt 
## October 25 (C bday!), 2018
##=============================================================================

setwd("~/Dropbox/SFU Postdoc/Skeena Work/Paradox of Portfolio/Panel Plotting")
library(MASS)

# -----------------------------------------------------------------------------
# Panel plot for sockey systems
# -----------------------------------------------------------------------------
setwd("~/Dropbox/SFU Postdoc/Skeena Work/Paradox of Portfolio/Panel Plotting")

systems <- c("Bristol Bay", "Fraser", "Skeena")
decades <- c("1970s", "1980s", "1990s", "2000s")
S.all <- list()
U.all <- list()

for(j in 1:length(systems)) {
  S.all[[j]] <- read.csv(paste(systems[j], "_Smsy.csv", sep=""), header=T)
  U.all[[j]] <- read.csv(paste(systems[j], "_Umsy.csv", sep=""), header=T)
}

# -----------------------------------------------------------------------------
# Kernel density plots
# -----------------------------------------------------------------------------

# Four decades -- for supplementary materials
colpal <- colorRampPalette(colors=c("white","lightpink2", "mediumvioletred"), space="rgb")(10) 

pdf("SuppMat_salmonsystems_12.19.18.pdf", width=13, height=10)
par(mfrow=c(3,4))
par(mar=c(4.1, 4.1, 1.3, 1.2))
for(j in 1:length(systems)) {
  temp.S <- S.all[[j]]
  temp.U <- U.all[[j]]
  ptcol <- colorRampPalette(colors=c("red", "orange","yellowgreen", "steelblue4", "darkblue"), space="rgb")(nrow(temp.S))  
  for(i in 1:length(decades)) {
    x=(temp.S[(4+i)]/temp.S$meanSmsy)
    xlcl=(temp.S[(4+i)]/temp.S$lowerCI) 
    xucl=(temp.S[(4+i)]/temp.S$upperCI) 
    y=(temp.U[(4+i)]/temp.U$meanUmsy) 
    ylcl=(temp.U[(4+i)]/(temp.U$lowerCI)) 
    yucl=(temp.U[(4+i)]/(temp.U$upperCI)) 
    
    if(j==3 & i==3) {x1 <- x[(-8),]; y1=y[(-8),]
    kern.density <- kde2d(x=x1, y=y1,  h=1.25, n=50, lims=c(0,4.5,0,3))
    } else {
      kern.density <- kde2d(x=x[,1], y=y[,1],  h=1.25, n=50, lims=c(0,4.5,0,3))
    }
    
    image(kern.density, col=colpal,  xlab="S/Smsy", ylab="F/Fmsy", axes=F, 
          xaxs="i", yaxs="i", main=paste(systems[j], decades[i], sep="-"))
    #filled.contour(kern.test)
    #points(x=x[,1], y=y[,1], pch=16, col=ptcol, cex=3)
    points(x=x[,1], y=y[,1], pch=16)
    for(k in 1:nrow(temp.S)){
      lines(x=c(xlcl[k,1],xucl[k,1]),y=c(y[k,1],y[k,1]), col=gray(0.4))
      lines(x=c(x[k,1],x[k,1]),y=c(ylcl[k,1],yucl[k,1]), col=gray(0.4))
    }
    abline(h=1, lty=3)
    abline(v=1, lty=3)
    axis(1, at=c(0, 1, 2, 3, 4, 5))
    axis(2, at=c(0, 0.5, 1, 1.5, 2, 2.5))
    box()
  }
}
dev.off()

# For paper -- Figure 4

systems <- c("Bristol Bay", "Fraser", "Skeena")
decades <- c("1980s","2000s")

pdf("CaseStudy_salmonsystems_12.19.18.pdf", width=7, height=10)
par(mfrow=c(3,2))
par(mar=c(4.1, 4.1, 1.3, 1.2))
for(j in 1:length(systems)) {
  temp.S <- S.all[[j]]
  temp.U <- U.all[[j]]
  ptcol <- colorRampPalette(colors=c("red", "orange","yellowgreen", "steelblue4", "darkblue"), space="rgb")(nrow(temp.S))  
  for(i in c(2,4)) {
    x=(temp.S[(4+i)]/temp.S$meanSmsy)
    xlcl=(temp.S[(4+i)]/temp.S$lowerCI) 
    xucl=(temp.S[(4+i)]/temp.S$upperCI) 
    y=(temp.U[(4+i)]/temp.U$meanUmsy) 
    ylcl=(temp.U[(4+i)]/(temp.U$lowerCI)) 
    yucl=(temp.U[(4+i)]/(temp.U$upperCI)) 
    
    if(j==3 & i==3) {x1 <- x[(-8),]; y1=y[(-8),]
    kern.density <- kde2d(x=x1, y=y1,  h=1.25, n=50, lims=c(0,4.5,0,3))
    } else {
      kern.density <- kde2d(x=x[,1], y=y[,1],  h=1.25, n=50, lims=c(0,4.5,0,3))
    }
    
    image(kern.density, col=colpal,  xlab="S/Smsy", ylab="F/Fmsy", axes=F, 
          xaxs="i", yaxs="i", main=paste(systems[j], decades[i], sep="-"))
    #filled.contour(kern.test)
    #points(x=x[,1], y=y[,1], pch=16, col=ptcol, cex=3)
    points(x=x[,1], y=y[,1], pch=16)
    for(k in 1:nrow(temp.S)){
      lines(x=c(xlcl[k,1],xucl[k,1]),y=c(y[k,1],y[k,1]), col=gray(0.4))
      lines(x=c(x[k,1],x[k,1]),y=c(ylcl[k,1],yucl[k,1]), col=gray(0.4))
    }
    abline(h=1, lty=3)
    abline(v=1, lty=3)
    axis(1, at=c(0, 1, 2, 3, 4, 5))
    axis(2, at=c(0, 0.5, 1, 1.5, 2, 2.5))
    box()
  }
}
dev.off()