##=============================================================================
## Multi panel plot with details within each region, for supp mat. 
## July 1, 2019 (Canada Day!)
##=============================================================================

setwd("~/Dropbox/SFU Postdoc/Skeena Work/Paradox of Portfolio/Panel Plotting")
library(MASS)
library(ggplot2)
library(gridExtra)

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
  System.all[[j]] <- cbind(system=sys.name, temp.S, temp.U, temp.params)
  
}

all.dat <- do.call(rbind.data.frame, System.all)


# -----------------------------------------------------------------------------
# Just detailed point information for each system - 6 panel
# -----------------------------------------------------------------------------

# Bristol Bay first
dat = all.dat[all.dat$system==systems[1],]

p1 <- ggplot(data = dat) +
  # divide by number for size
  geom_point(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, colour=alpha),size=sqrt(dat$SAll/10000), alpha=0.90,
             shape=16) +
  # log for size
  # geom_point(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, colour=alpha),size=log(dat$SAll), alpha=0.90,
  #            shape=16) +
  scale_colour_gradient(low = "red", high = "blue")+
  scale_x_continuous("",limits = c(0,4),labels=rep("",5))+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]), limits = c(0,2.5)) +
  geom_vline(xintercept=1) +
  geom_hline(yintercept=1) +
  theme_bw() +
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=0.2, y=2.5, label= "1980s",size=5) +
  annotate("text", x=4, y=2.5, label="A", size=6) +
  annotate("text", label=dat$lake, x=rep(3,nrow(dat)), y=seq(0,2.5,length.out=nrow(dat)))
p1

p2 <- ggplot(data = dat) +
  # divide by number for size
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s, colour=alpha), shape=16, alpha=0.90,
             size=sqrt(dat$SAll/10000)) +
  # log for size
  # geom_point(aes(S_Smsy_2000s, U_Umsy_2000s, colour=alpha), shape=16, alpha=0.90,
  #            size=log(dat$SAll)) +
  scale_colour_gradient(low = "red", high = "blue")+
  scale_x_continuous("",limits = c(0,4),labels=rep("",5))+
  scale_y_continuous("",limits = c(0,2.5),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=0.2, y=2.5,   label= "2000s",size=5)+
  annotate("text", x=2.5, y=2.5,   label= "Brisol Bay",size=5)+
  annotate("text", x=4, y=2.5, label="B", size=6)+
  annotate("text", label=dat$lake, x=rep(3,nrow(dat)), y=seq(0,2.5,length.out=nrow(dat)))

# p2


# Skeena
dat = all.dat[all.dat$system==systems[3],]

p9 <- ggplot(data = dat) +
  geom_point(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, colour=alpha),size=sqrt(dat$SAll/1000),alpha=0.9,
             shape=16) +
  scale_colour_gradient(low = "red", high = "blue")+
  scale_x_continuous("",limits = c(0,4),labels=rep("",5))+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]), limits = c(0,2.5)) +
  geom_vline(xintercept=1) +
  geom_hline(yintercept=1) +
  theme_bw() +
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=4, y=2.5, label="C", size=6)+
  annotate("text", label=dat$lake, x=rep(3,nrow(dat)), y=seq(0,2.5,length.out=nrow(dat)))


# p9

p10 <- ggplot(data = dat) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s, colour=alpha), alpha=0.9,shape=16,
             size=sqrt(dat$SAll/1000)) +
  scale_colour_gradient(low = "red", high = "blue")+
  scale_x_continuous("",limits = c(0,4),labels=rep("",5))+
  scale_y_continuous("",limits = c(0,2.5),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=2.5, y=2.5,   label= "Skeena River",size=5)+
  annotate("text", x=4, y=2.5, label="D", size=6)+
  annotate("text", label=dat$lake, x=rep(3,nrow(dat)), y=seq(0,2.5,length.out=nrow(dat)))



# Fraser
dat = all.dat[all.dat$system==systems[2],]

p5 <- ggplot(data = dat) +
  geom_point(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, colour=alpha),size=sqrt(dat$SAll/1500),#alpha=0.45,
             shape=16) +
  scale_colour_gradient(low = "red", high = "blue")+
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4), position = "bottom")+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]), limits = c(0,2.5)) +
  geom_vline(xintercept=1) +
  geom_hline(yintercept=1) +
  theme_bw() +
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=4, y=2.5, label="E", size=6)+
  annotate("text", label=dat$lake, x=rep(3,nrow(dat)), y=seq(0,2.5,length.out=nrow(dat)))


# p5

p6 <- ggplot(data = dat) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s, colour=alpha), shape=16, alpha=0.9,
             size=sqrt(dat$SAll/1500)) +
  scale_colour_gradient(low = "red", high = "blue")+
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,6), position = "bottom")+
  scale_y_continuous("",limits = c(0,2.5),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=2.5, y=2.5,   label= "Fraser River",size=5)+
  annotate("text", x=4, y=2.5, label="F", size=6)+
  annotate("text", label=dat$lake, x=rep(3,nrow(dat)), y=seq(0,2.5,length.out=nrow(dat)))

# p6

# pdf("SuppMat_Demo_Fraser_divide1000+Sqrt.pdf", width=10, height=5)
# grid.arrange(p5, p6, layout_matrix=lay.out)
# dev.off()


# p10
# 
# pdf("SuppMat_Demo_Skeena_divide8000.pdf", width=10, height=5)
# grid.arrange(p9, p10, layout_matrix=lay.out)
# dev.off()

###############################################################################



layout.mat <- matrix(c(1,1,2,2,
                       1,1,2,2,
                       3,3,4,4,
                       3,3,4,4,
                       5,5,6,6,
                       5,5,6,6), nrow=6, byrow=TRUE)


pdf(paste("AllSystems_SuppMat_sqrt", yr.range, "yrs_07.02.2019.pdf", sep=""),
    width=10, height=8)
grid.arrange(p1,p2,p9,p10,p5,p6,
             layout_matrix=layout.mat)
dev.off()


