##=============================================================================
## Multi panel plot attempt 
## October 25 (C bday!), 2018
##=============================================================================

library(MASS)
library(ggplot2)
library(gridExtra)

# -----------------------------------------------------------------------------
# Panel plot for sockey systems
# -----------------------------------------------------------------------------

systems <- c("Bristol Bay", "Fraser", "Skeena")
decades <- c("1970s", "1980s", "1990s", "2000s")
System.all <- list()

yr.range <- 10


for(j in 1:length(systems)) {
  temp.S <- read.csv(paste(systems[j], "_Smsy_", yr.range, "yrs.csv", sep=""), header=T)
  colnames(temp.S) <- c("lake", "meanSmsy", "SlowerCI", "SupperCI",
                        "S1970s", "S1980s", "S1990s", "S2000s", "SAll")
  temp.U <- read.csv(paste(systems[j], "_Umsy_", yr.range, "yrs.csv", sep=""), header=T)
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

  System.all[[j]] <- cbind(system=sys.name, temp.S, temp.U)

}

all.dat <- do.call(rbind.data.frame, System.all)

# -----------------------------------------------------------------------------
# Kernel density plots -- 12 panel plot
# -----------------------------------------------------------------------------

bandwidth = c(0.8,.8)
# bandwidth feels super arbitrary -- where it basically sets how tight the 
# contours are around the points, the first one sets in the x direction and the
# second in the y direction (though they seem to interact)... as soon as the 
# first element jumps to 0.9, we start to get the weird splits in the Skeena
# 2000s plot... c(0.8, 1) seems to be the closest to what we had before, but
# without the overstretching in the Skeena plot...
# CHANGE: since we reduced the y axis from c(0,4) to c(0,2.5) this changed
# to c(0.8, 0.8)

############
### BBay ###
############
dat = all.dat[all.dat$system==systems[1],]
colors <- colorRampPalette(c("white", "#458B74", "#2F4F4F"))(10)
p1 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, fill = stat(level)), geom="polygon", h=bandwidth) +
  scale_fill_distiller(palette=12, direction=1) +
  scale_x_continuous("",limits = c(0,4),labels=rep("",5))+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]), limits = c(0,2.5)) +
  geom_vline(xintercept=1) +
  geom_hline(yintercept=1) +
  geom_point(aes(x=S_Smsy_1980s, y=U_Umsy_1980s), size=2.5,alpha=0.45,shape=16) +
  theme_bw() +
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=0.2, y=2.5, label= "1980s",size=5) +
  annotate("text", x=4, y=2.5, label="A", size=6)

p2 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_2000s, y=U_Umsy_2000s, fill = stat(level)), geom="polygon", h=bandwidth) +
  scale_fill_distiller(palette=12, direction=1) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous("",limits = c(0,4),labels=rep("",5))+
  scale_y_continuous("",limits = c(0,2.5),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=0.2, y=2.5,   label= "2000s",size=5)+
  annotate("text", x=2.5, y=2.5,   label= "Bristol Bay",size=5)+
  annotate("text", x=4, y=2.5, label="B", size=6)


p3 <- ggplot(data=dat) +
  geom_density(aes(x=S_Smsy_1980s, group=system,fill=system),alpha=0.65, adjust=3, linetype="dashed")+ 
  geom_density(aes(x=S_Smsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("#5D478B"))+
  scale_x_continuous(limits = c(0,4))+
  coord_cartesian(ylim=c(0,1.2))+
  theme(panel.background = element_blank(),axis.text = element_blank(), 
        axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(0,0.2,2,.2), "lines"))+
  geom_vline(xintercept=1) +
  annotate("text", x=3, y=0.7, label= "S/Smsy", size=4)+
  annotate("text", x=4, y=1.1, label="D", size=6)

p4 <- ggplot(data=dat) +
  geom_density(aes(x=U_Umsy_1980s, group=system,fill=system),alpha=0.65, adjust=3, linetype="dashed")+ 
  geom_density(aes(x=U_Umsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("#5D478B"))+
  coord_flip(ylim=c(0,1.5))+
  # coord_cartesian()+
  scale_x_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,2.5)) +
  theme(panel.background = element_blank(),axis.text = element_blank(), 
        axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(0,0.2,2,.2), "lines"))+
  geom_vline(xintercept=1) +
  annotate("text", x=2, y=1.2, label= "U/Umsy", size=4)+
  annotate("text", x=2.3, y=1.5, label="C", size=6)


##############
### Skeena ###
##############
dat = all.dat[all.dat$system==systems[3],]

p9 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, fill = stat(level)), geom = "polygon", h=bandwidth) +
  scale_fill_distiller(palette=2, direction=1) +
  geom_point(aes(S_Smsy_1980s, U_Umsy_1980s), size=2.5,alpha=0.45, shape=16) +
  scale_x_continuous("",limits = c(0,4),labels=rep("",5))+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,2.5))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=4, y=2.5, label="E", size=6)

p10 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_2000s, y=U_Umsy_2000s, fill = stat(level)), geom = "polygon", h=bandwidth) +
  scale_fill_distiller(palette=2, direction=1) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous("",limits = c(0,4),labels=rep("",5))+
  scale_y_continuous("",limits = c(0,2.5),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=2.5, y=2.5,   label= "Skeena River",size=5)+
  annotate("text", x=4, y=2.5, label="F", size=6)

p11 <- ggplot(data=dat) +
  geom_density(aes(x=S_Smsy_1980s, group=system,fill=system),alpha=0.65, adjust=3, linetype="dashed")+ 
  geom_density(aes(x=S_Smsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("#2E8B57"))+
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "bottom") +
  #scale_x_continuous(limits = c(0,4))+
  coord_cartesian(ylim=c(0,1.2))+
  theme(panel.background = element_blank(),axis.text = element_blank(), 
        axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(0,0.2,2,.2), "lines"))+
  geom_vline(xintercept=1)+
  # annotate("text", x=3, y=0.7, label= "S/Smsy", size=4)+
  annotate("text", x=4, y=1.1, label="H", size=6)

p12 <- ggplot(data=dat) +
  geom_density(aes(x=U_Umsy_1980s, group=system,fill=system),alpha=0.65, adjust=3, linetype="dashed")+ 
  geom_density(aes(x=U_Umsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("#2E8B57"))+
  coord_flip(ylim=c(0,1.5))+
  # coord_cartesian(ylim=c(0,1.2))+
  scale_x_continuous(limits = c(0,2.5))+
  theme(panel.background = element_blank(),axis.text = element_blank(), 
        axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(0,0.2,2,.2), "lines"))+
  geom_vline(xintercept=1) +
  # annotate("text", x=2, y=1.2, label= "U/Umsy", size=4)+
  annotate("text", x=2.3, y=1.5, label="G", size=6)


##############
### Fraser ###
##############
dat = all.dat[all.dat$system==systems[2],]

p5 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, fill = stat(level)), geom="polygon", h=bandwidth) +
  scale_fill_distiller(palette=1, direction=1) +
  geom_point(aes(S_Smsy_1980s, U_Umsy_1980s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4), position = "bottom")+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,2.5))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=4, y=2.5, label="I", size=6)

p6 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_2000s, y=U_Umsy_2000s, fill = stat(level)), geom="polygon", h=bandwidth) +
  scale_fill_distiller(palette=1, direction=1) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4), position = "bottom")+
  scale_y_continuous("",limits = c(0,2.5),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=2.5, y=2.5,   label= "Fraser River",size=5)+
  annotate("text", x=4, y=2.5, label="J", size=6)

p7 <- ggplot(data=dat) +
  geom_density(aes(x=S_Smsy_1980s, group=system,fill=system),alpha=0.65, adjust=3, linetype="dashed")+ 
  geom_density(aes(x=S_Smsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("#104E8B"))+
  scale_x_continuous(limits = c(0,4))+
  coord_cartesian(ylim=c(0,1.2))+
  theme(panel.background = element_blank(),axis.text = element_blank(), 
        axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(0,0.2,2,.2), "lines"))+
  geom_vline(xintercept=1) +
  # annotate("text", x=3, y=0.7, label= "S/Smsy", size=4)+
  annotate("text", x=4, y=1.1, label="L", size=6)
  
p8 <- ggplot(data=dat) +
  geom_density(aes(x=U_Umsy_1980s, group=system,fill=system),alpha=0.65, adjust=3, linetype="dashed")+ 
  geom_density(aes(x=U_Umsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("#104E8B"))+
  coord_flip(ylim=c(0,1.5))+
  # coord_cartesian(ylim=c(0,1.2))+
  scale_x_continuous(limits = c(0,2.5))+
  theme(panel.background = element_blank(),axis.text = element_blank(), 
        axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(0,0.2,2,.2), "lines"))+
  geom_vline(xintercept=1) +
  # annotate("text", x=2, y=1.2, label= "U/Umsy", size=4)+
  annotate("text", x=2.3, y=1.5, label="K", size=6)


# pdf("AllSystems_12panel.pdf", width=11, height=8)
# grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,
#              layout_matrix=rbind(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12)))
# dev.off()
# 
# layout.mat <- matrix(c(1,1,2,2,3,NA,
#                        1,1,2,2,NA,4,
#                        5,5,6,6,7,NA,
#                        5,5,6,6,NA,8,
#                        9,9,10,10,11,NA,
#                        9,9,10,10,NA,12), nrow=6, byrow=TRUE)

layout.mat <- matrix(c(1,1,2,2,3,
                       1,1,2,2,4,
                       5,5,6,6,7,
                       5,5,6,6,8,
                       9,9,10,10,11,
                       9,9,10,10,12), nrow=6, byrow=TRUE)


pdf(paste("AllSystems_12panel_", yr.range, "yrs_08.21.2019.pdf", sep=""),
    width=9, height=8.5) # REDUCE PDF WIDTH
grid.arrange(p1,p2,p4,p3,p9,p10,p12,p11,p5,p6,p8,p7,
             layout_matrix=layout.mat)
              # , labels = c("A", "B", "C", "D", "E", "F", "G", "H"))
             # labels=c("A", "")) # ADD THE LABELS HERE!
dev.off()

library(ggpubr)


###############################################################################
###############################################################################
###############################################################################

# Thick lines and darker points

###############################################################################
###############################################################################
###############################################################################

############
### BBay ###
############
dat = all.dat[all.dat$system==systems[1],]
colors <- colorRampPalette(c("white", "#458B74", "#2F4F4F"))(10)
p1 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, fill = stat(level)), geom="polygon", h=bandwidth) +
  scale_fill_distiller(palette=12, direction=1) +
  scale_x_continuous("",limits = c(0,4),labels=rep("",5))+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]), limits = c(0,2.5)) +
  geom_vline(xintercept=1, size=1) +
  geom_hline(yintercept=1, size=1) +
  geom_point(aes(x=S_Smsy_1980s, y=U_Umsy_1980s), size=3,alpha=0.55,shape=16) +
  theme_bw() +
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=0.2, y=2.5, label= "1980s",size=5) +
  annotate("text", x=4, y=2.5, label="A", size=6)

p2 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_2000s, y=U_Umsy_2000s, fill = stat(level)), geom="polygon", h=bandwidth) +
  scale_fill_distiller(palette=12, direction=1) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), size=3,alpha=0.55,shape=16) +
  scale_x_continuous("",limits = c(0,4),labels=rep("",5))+
  scale_y_continuous("",limits = c(0,2.5),labels = NULL)+
  geom_vline(xintercept=1, size=1) +
  geom_hline(yintercept=1, size=1) +
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=0.2, y=2.5,   label= "2000s",size=5)+
  annotate("text", x=2.5, y=2.5,   label= "Bristol Bay",size=5)+
  annotate("text", x=4, y=2.5, label="B", size=6)


p3 <- ggplot(data=dat) +
  geom_density(aes(x=S_Smsy_1980s, group=system,fill=system),alpha=0.65, adjust=3, linetype="dashed")+ 
  geom_density(aes(x=S_Smsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("#5D478B"))+
  scale_x_continuous(limits = c(0,4))+
  coord_cartesian(ylim=c(0,1.2))+
  theme(panel.background = element_blank(),axis.text = element_blank(), 
        axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(0,0.2,2,.2), "lines"))+
  geom_vline(xintercept=1) +
  annotate("text", x=3, y=0.7, label= "S/Smsy", size=4)+
  annotate("text", x=4, y=1.1, label="D", size=6)

p4 <- ggplot(data=dat) +
  geom_density(aes(x=U_Umsy_1980s, group=system,fill=system),alpha=0.65, adjust=3, linetype="dashed")+ 
  geom_density(aes(x=U_Umsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("#5D478B"))+
  coord_flip(ylim=c(0,1.5))+
  # coord_cartesian()+
  scale_x_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,2.5)) +
  theme(panel.background = element_blank(),axis.text = element_blank(), 
        axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(0,0.2,2,.2), "lines"))+
  geom_vline(xintercept=1) +
  annotate("text", x=2, y=1.2, label= "U/Umsy", size=4)+
  annotate("text", x=2.3, y=1.5, label="C", size=6)


##############
### Skeena ###
##############
dat = all.dat[all.dat$system==systems[3],]

p9 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, fill = stat(level)), geom = "polygon", h=bandwidth) +
  scale_fill_distiller(palette=2, direction=1) +
  geom_point(aes(S_Smsy_1980s, U_Umsy_1980s), size=3,alpha=0.55,shape=16) +
  scale_x_continuous("",limits = c(0,4),labels=rep("",5))+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,2.5))+
  geom_vline(xintercept=1, size=1) +
  geom_hline(yintercept=1, size=1) +
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=4, y=2.5, label="E", size=6)

p10 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_2000s, y=U_Umsy_2000s, fill = stat(level)), geom = "polygon", h=bandwidth) +
  scale_fill_distiller(palette=2, direction=1) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), size=3,alpha=0.55,shape=16) +
  scale_x_continuous("",limits = c(0,4),labels=rep("",5))+
  scale_y_continuous("",limits = c(0,2.5),labels = NULL)+
  geom_vline(xintercept=1, size=1) +
  geom_hline(yintercept=1, size=1) +
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=2.5, y=2.5,   label= "Skeena River",size=5)+
  annotate("text", x=4, y=2.5, label="F", size=6)

p11 <- ggplot(data=dat) +
  geom_density(aes(x=S_Smsy_1980s, group=system,fill=system),alpha=0.65, adjust=3, linetype="dashed")+ 
  geom_density(aes(x=S_Smsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("#2E8B57"))+
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "bottom") +
  #scale_x_continuous(limits = c(0,4))+
  coord_cartesian(ylim=c(0,1.2))+
  theme(panel.background = element_blank(),axis.text = element_blank(), 
        axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(0,0.2,2,.2), "lines"))+
  geom_vline(xintercept=1)+
  # annotate("text", x=3, y=0.7, label= "S/Smsy", size=4)+
  annotate("text", x=4, y=1.1, label="H", size=6)

p12 <- ggplot(data=dat) +
  geom_density(aes(x=U_Umsy_1980s, group=system,fill=system),alpha=0.65, adjust=3, linetype="dashed")+ 
  geom_density(aes(x=U_Umsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("#2E8B57"))+
  coord_flip(ylim=c(0,1.5))+
  # coord_cartesian(ylim=c(0,1.2))+
  scale_x_continuous(limits = c(0,2.5))+
  theme(panel.background = element_blank(),axis.text = element_blank(), 
        axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(0,0.2,2,.2), "lines"))+
  geom_vline(xintercept=1) +
  # annotate("text", x=2, y=1.2, label= "U/Umsy", size=4)+
  annotate("text", x=2.3, y=1.5, label="G", size=6)


##############
### Fraser ###
##############
dat = all.dat[all.dat$system==systems[2],]

p5 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, fill = stat(level)), geom="polygon", h=bandwidth) +
  scale_fill_distiller(palette=1, direction=1) +
  geom_point(aes(S_Smsy_1980s, U_Umsy_1980s), size=3,alpha=0.55,shape=16) +
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4), position = "bottom")+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,2.5))+
  geom_vline(xintercept=1, size=1) +
  geom_hline(yintercept=1, size=1) +
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=4, y=2.5, label="I", size=6)

p6 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_2000s, y=U_Umsy_2000s, fill = stat(level)), geom="polygon", h=bandwidth) +
  scale_fill_distiller(palette=1, direction=1) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), size=3,alpha=0.55,shape=16) +
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4), position = "bottom")+
  scale_y_continuous("",limits = c(0,2.5),labels = NULL)+
  geom_vline(xintercept=1, size=1) +
  geom_hline(yintercept=1, size=1) +
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(), 
        plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5)) +
  annotate("text", x=2.5, y=2.5,   label= "Fraser River",size=5)+
  annotate("text", x=4, y=2.5, label="J", size=6)

p7 <- ggplot(data=dat) +
  geom_density(aes(x=S_Smsy_1980s, group=system,fill=system),alpha=0.65, adjust=3, linetype="dashed")+ 
  geom_density(aes(x=S_Smsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("#104E8B"))+
  scale_x_continuous(limits = c(0,4))+
  coord_cartesian(ylim=c(0,1.2))+
  theme(panel.background = element_blank(),axis.text = element_blank(), 
        axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(0,0.2,2,.2), "lines"))+
  geom_vline(xintercept=1) +
  # annotate("text", x=3, y=0.7, label= "S/Smsy", size=4)+
  annotate("text", x=4, y=1.1, label="L", size=6)

p8 <- ggplot(data=dat) +
  geom_density(aes(x=U_Umsy_1980s, group=system,fill=system),alpha=0.65, adjust=3, linetype="dashed")+ 
  geom_density(aes(x=U_Umsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("#104E8B"))+
  coord_flip(ylim=c(0,1.5))+
  # coord_cartesian(ylim=c(0,1.2))+
  scale_x_continuous(limits = c(0,2.5))+
  theme(panel.background = element_blank(),axis.text = element_blank(), 
        axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(0,0.2,2,.2), "lines"))+
  geom_vline(xintercept=1) +
  # annotate("text", x=2, y=1.2, label= "U/Umsy", size=4)+
  annotate("text", x=2.3, y=1.5, label="K", size=6)


# pdf("AllSystems_12panel.pdf", width=11, height=8)
# grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,
#              layout_matrix=rbind(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12)))
# dev.off()
# 
# layout.mat <- matrix(c(1,1,2,2,3,NA,
#                        1,1,2,2,NA,4,
#                        5,5,6,6,7,NA,
#                        5,5,6,6,NA,8,
#                        9,9,10,10,11,NA,
#                        9,9,10,10,NA,12), nrow=6, byrow=TRUE)

layout.mat <- matrix(c(1,1,2,2,3,
                       1,1,2,2,4,
                       5,5,6,6,7,
                       5,5,6,6,8,
                       9,9,10,10,11,
                       9,9,10,10,12), nrow=6, byrow=TRUE)


pdf(paste("AllSystems_12panel_BOLD_", yr.range, "yrs_08.21.2019.pdf", sep=""),
    width=9, height=8.5) # REDUCE PDF WIDTH
grid.arrange(p1,p2,p4,p3,p9,p10,p12,p11,p5,p6,p8,p7,
             layout_matrix=layout.mat)
# , labels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# labels=c("A", "")) # ADD THE LABELS HERE!
dev.off()

library(ggpubr)

