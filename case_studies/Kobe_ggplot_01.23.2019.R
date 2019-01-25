##=============================================================================
## Multi panel plot using ggplot code 
## January 23, 2019
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


for(j in 1:length(systems)) {
  temp.S <- read.csv(paste(systems[j], "_Smsy.csv", sep=""), header=T)
  colnames(temp.S) <- c("lake", "meanSmsy", "SlowerCI", "SupperCI",
                        "S1970s", "S1980s", "S1990s", "S2000s")
  temp.U <- read.csv(paste(systems[j], "_Umsy.csv", sep=""), header=T)
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

# Bristol Bay first
dat = all.dat[all.dat$system==systems[1],]

p1 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, fill = stat(level)), geom = "polygon") +
  scale_fill_distiller(palette="Reds", direction=1) +
  geom_point(aes(x=S_Smsy_1980s, y=U_Umsy_1980s),size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous(limits = c(0,4))+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,4)) +
  geom_vline(xintercept=1) +
  geom_hline(yintercept=1) +
  theme_bw() +
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none") +
  annotate("text", x=2.5, y=3.8,   label= "1980s",size=4) 

p2 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_2000s, y=U_Umsy_2000s, fill = stat(level)), geom = "polygon") +
  scale_fill_distiller(palette="Reds", direction=1) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous(limits = c(0,4))+
  scale_y_continuous("",limits = c(0,4),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none")+
  annotate("text", x=2.5, y=3.8,   label= "2000s",size=4) 

p3 <- ggplot(data=dat) +
  geom_density(aes(x=S_Smsy_1980s, group=system,fill=system),alpha=0.65, adjust=3)+ 
  geom_density(aes(x=S_Smsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("red"))+
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(1,4,1,0), "lines"))+
  geom_vline(xintercept=1)

p4 <- ggplot(data=dat) +
  geom_density(aes(x=U_Umsy_1980s, group=system,fill=system),alpha=0.65, adjust=3)+ 
  geom_density(aes(x=U_Umsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("red"))+
  coord_flip()+ 
  scale_x_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,4)) +
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(1,4,1,0), "lines"))+
  geom_vline(xintercept=1) +
  annotate("text", x=3, y=0.5, label= "Bristol Bay", size=6)


# Fraser
dat = all.dat[all.dat$system==systems[2],]

p5 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, fill = stat(level)), geom = "polygon") +
  scale_fill_distiller(palette="Greens", direction=1) +
  geom_point(aes(S_Smsy_1980s, U_Umsy_1980s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous("",limits = c(0,4),labels = NULL)+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,4))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none")

p6 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_2000s, y=U_Umsy_2000s, fill = stat(level)), geom = "polygon") +
  scale_fill_distiller(palette="Greens", direction=1) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous("",limits = c(0,4),labels = NULL)+
  scale_y_continuous("",limits = c(0,4),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none")

p7 <- ggplot(data=dat) +
  geom_density(aes(x=S_Smsy_1980s, group=system,fill=system),alpha=0.65, adjust=3)+ 
  geom_density(aes(x=S_Smsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("green"))+
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(1,4,1,0), "lines"))+
  geom_vline(xintercept=1)
  
p8 <- ggplot(data=dat) +
  geom_density(aes(x=U_Umsy_1980s, group=system,fill=system),alpha=0.65, adjust=3)+ 
  geom_density(aes(x=U_Umsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("green"))+
  coord_flip()+ 
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(1,4,1,0), "lines"))+
  geom_vline(xintercept=1) +
  annotate("text", x=3, y=0.5, label= "Fraser", size=6)

# Skeena
dat = all.dat[all.dat$system==systems[3],]

p9 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, fill = stat(level)), geom = "polygon") +
  scale_fill_distiller(palette="Blues", direction=1) +
  geom_point(aes(S_Smsy_1980s, U_Umsy_1980s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "bottom")+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,4))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none")

p10 <- ggplot(data = dat) +
  stat_density_2d(aes(x=S_Smsy_2000s, y=U_Umsy_2000s, fill = stat(level)), geom = "polygon") +
  scale_fill_distiller(palette="Blues", direction=1) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "bottom")+
  scale_y_continuous("",limits = c(0,4),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none")

p11 <- ggplot(data=dat) +
  geom_density(aes(x=S_Smsy_1980s, group=system,fill=system),alpha=0.65, adjust=3)+ 
  geom_density(aes(x=S_Smsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("blue"))+
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "bottom") +
  #scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(1,4,1,0), "lines"))+
  geom_vline(xintercept=1)

p12 <- ggplot(data=dat) +
  geom_density(aes(x=U_Umsy_1980s, group=system,fill=system),alpha=0.65, adjust=3)+ 
  geom_density(aes(x=U_Umsy_2000s, group=system,fill=system),alpha=0.45, adjust=3)+ 
  scale_fill_manual(values=c("blue"))+
  coord_flip()+ 
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(1,4,1,0), "lines"))+
  geom_vline(xintercept=1) +
  annotate("text", x=3, y=0.5, label= "Skeena", size=6)

pdf("AllSystems_12panel.pdf", width=11, height=8)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,
             layout_matrix=rbind(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12)))
dev.off()

# -----------------------------------------------------------------------------
# Kernel density plots -- 6 panel plot
# -----------------------------------------------------------------------------

p1 <- ggplot(data = all.dat[all.dat$system==systems[1],]) +
  stat_density_2d(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, fill = stat(level)), geom = "polygon") +
  # scale_fill_distiller(palette=3, direction=1) +
  # geom_point(aes(x=S_Smsy_1980s, y=U_Umsy_1980s),color="red",fill='red',size=2,alpha=0.65,shape=16) +
  scale_fill_distiller(palette="Reds", direction=1) +
  geom_point(aes(x=S_Smsy_1980s, y=U_Umsy_1980s),size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "top") +
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,4)) +
  geom_vline(xintercept=1) +
  geom_hline(yintercept=1) +
  theme_bw() +
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none") +
  annotate("text", x=2.5, y=3.8,   label= "Bristol Bay, 1980s",size=4) 

p2 <- ggplot(data = all.dat[all.dat$system==systems[2],]) +
  stat_density_2d(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, fill = stat(level)), geom = "polygon") +
  # scale_fill_distiller(palette=3, direction=1)+
  # geom_point(aes(S_Smsy_1980s, U_Umsy_1980s), color='green',fill='green',size=2,alpha=0.45,shape=16) +
  scale_fill_distiller(palette="Greens", direction=1) +
  geom_point(aes(S_Smsy_1980s, U_Umsy_1980s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous("",limits = c(0,4),labels = NULL)+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,4))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none")+
  annotate("text", x=2.5, y=3.8,   label= "Fraser River, 1980s",size=4) 

p3 <- ggplot(data = all.dat[all.dat$system==systems[3],]) +
  stat_density_2d(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, fill = stat(level)), geom = "polygon") +
  # scale_fill_distiller(palette=3, direction=1)+
  # geom_point(aes(S_Smsy_1980s, U_Umsy_1980s), color='blue',fill='blue',size=2,alpha=0.45,shape=16)+
  scale_fill_distiller(palette="Blues", direction=1) +
  geom_point(aes(S_Smsy_1980s, U_Umsy_1980s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous("",limits = c(0,4),labels = NULL)+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,4))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none")+
  annotate("text", x=2.5, y=3.8,   label= "Skeena River, 1980s",size=4) 

p4 <- ggplot(data = all.dat[all.dat$system==systems[1],]) +
  stat_density_2d(aes(x=S_Smsy_2000s, y=U_Umsy_2000s, fill = stat(level)), geom = "polygon") +
  # scale_fill_distiller(palette=3, direction=1)+
  # geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), color="red",fill='red',size=2,alpha=0.65,shape=16)+
  scale_fill_distiller(palette="Reds", direction=1) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "top")+
  scale_y_continuous("",limits = c(0,4),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none")+
  annotate("text", x=2.5, y=3.8,   label= "Bristol Bay, 2000s",size=4) 

p5 <- ggplot(data = all.dat[all.dat$system==systems[2],]) +
  stat_density_2d(aes(x=S_Smsy_2000s, y=U_Umsy_2000s, fill = stat(level)), geom = "polygon") +
  # scale_fill_distiller(palette=3, direction=1)+
  # geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), color='green',fill='green',size=2,alpha=0.45,shape=16)+
  scale_fill_distiller(palette="Greens", direction=1) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous("",limits = c(0,4),labels = NULL)+
  scale_y_continuous("",limits = c(0,4),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none")+
  annotate("text", x=2.5, y=3.8,   label= "Fraser River, 2000s",size=4) 

p6 <- ggplot(data = all.dat[all.dat$system==systems[3],]) +
  stat_density_2d(aes(x=S_Smsy_2000s, y=U_Umsy_2000s, fill = stat(level)), geom = "polygon") +
  # scale_fill_distiller(palette=3, direction=1)+
  # geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), color='blue',fill='blue',size=2,alpha=0.45,shape=16)+
  scale_fill_distiller(palette="Blues", direction=1) +
  geom_point(aes(S_Smsy_2000s, U_Umsy_2000s), size=2.5,alpha=0.45,shape=16) +
  scale_x_continuous("",limits = c(0,4),labels = NULL)+
  scale_y_continuous("",limits = c(0,4),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"), legend.position="none")+
  annotate("text", x=2.5, y=3.8,   label= "Skeena River, 2000s",size=4) 


pdf("AllSystems_Kobe_colourbysystem_01.24.2019.pdf", width=6, height=9)
# ggarrange(p1, p4, p2, p5, p3, p6, labels = c("A", "B", "C", "D", "E", "F"),
#           ncol=2, nrow=3)
grid.arrange(p1, p4, p2, p5, p3, p6, layout_matrix=rbind(c(1,2),c(3,4),c(5,6)))
dev.off()

# -----------------------------------------------------------------------------
# Plot with densities on the side
# -----------------------------------------------------------------------------

p2.1 <- ggplot(data = all.dat) +
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "top")+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,4))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"))+
  geom_point(aes(x=S_Smsy_2000s, y=U_Umsy_2000s, colour=system), shape=16) + 
  theme(legend.position=c(.7,.8)) +
  geom_point(aes(x=S_Smsy_1980s, y=U_Umsy_1980s, colour=system), shape=1) +
  theme(legend.position=c(.9,.8))

p2.2 <- ggplot(data=all.dat) +
    geom_density(aes(x=U_Umsy_1980s, group=system,fill=system),alpha=0.65, adjust=3)+ 
    scale_fill_manual(values=c("red", "green", "blue"))+
    coord_flip()+ 
    scale_x_continuous(limits = c(0,4))+
    theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+
    theme(legend.position="none",plot.margin = unit(c(1,4,1,0), "lines"))+
    geom_vline(xintercept=1) +
    annotate("text", x=2.5, y=1, label= "1980s", size=5) 

p2.3 <- ggplot(data=all.dat) +
  geom_density(aes(x=U_Umsy_2000s, group=system,fill=system),alpha=0.65, adjust=3)+ 
  scale_fill_manual(values=c("red", "green", "blue"))+
  coord_flip()+ 
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(1,4,1,0), "lines"))+
  geom_vline(xintercept=1) +
  annotate("text", x=2.5, y=1, label= "2000s", size=5) 


p2.4 <- ggplot(data=all.dat) +
  geom_density(aes(x=S_Smsy_1980s, group=system,fill=system),alpha=0.65, adjust=3)+ 
  scale_fill_manual(values=c("red", "green", "blue"))+
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(1,4,1,0), "lines"))+
  geom_vline(xintercept=1)+
  scale_y_reverse() +
  annotate("text", x=2.5, y=1, label= "1980s", size=5) 

p2.5 <- ggplot(data=all.dat) +
  geom_density(aes(x=S_Smsy_2000s, group=system,fill=system),alpha=0.65, adjust=3)+ 
  scale_fill_manual(values=c("red", "green", "blue"))+
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+
  theme(legend.position="none",plot.margin = unit(c(1,4,1,0), "lines"))+
  geom_vline(xintercept=1)+
  scale_y_reverse() +
  annotate("text", x=2.5, y=1, label= "2000s", size=5) 

pdf("AllSystems_density_demo.pdf", 10, 10)
grid.arrange(p2.1, p2.2, p2.3, p2.4, p2.5, layout_matrix=rbind(c(1,1,2),c(1,1,3),c(4,5,6)))
dev.off()

