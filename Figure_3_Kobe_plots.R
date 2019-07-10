
source("Kobe_sims.R")

##########################################
#Summary stats

output <- rbind(btml,btmr)

XX <- "0" # mgmt. control
YY <- "1" # conservation risk tolerance

OFingT <- 0.1 # overfishing threshold
OFedT <- 0.2 # overfished threshold

kobe_data <- output[which(output$control==XX & output$risk  == YY),]

Overfished_overfishging <- nrow(subset(kobe_data,F_Fmsy >(1+OFingT) & S_Smsy < (1-OFedT)))/nrow(kobe_data)
Overfished_underfishging <- nrow(subset(kobe_data,F_Fmsy < (1-OFingT) & S_Smsy < (1-OFedT)))/nrow(kobe_data)
Underfished_overfishging <- nrow(subset(kobe_data,F_Fmsy > (1+OFingT) & S_Smsy > (1+OFedT)))/nrow(kobe_data)
Underfished_underfishging <- nrow(subset(kobe_data,F_Fmsy < (1-OFingT) & S_Smsy > (1+OFedT)))/nrow(kobe_data)
Overfishging <- nrow(subset(kobe_data,F_Fmsy >(1+OFingT) ))/nrow(kobe_data)
Overfished <-   nrow(subset(kobe_data,S_Smsy < (1-OFedT)))/nrow(kobe_data)

round(cbind(Overfished_overfishging,
      Overfished_underfishging,
      Underfished_overfishging,
      Underfished_underfishging,
      Overfishging,Overfished
            ),
      digits=2)


##########################################
#Plot

#Set colors
mycolors <- viridis(4)

#Define mgmt. control from sims
HC <- "0.85"
LC <- "0"



p1 <- ggplot(data = full.sims.top[which(full.sims.top$control==LC),] ,aes(S_Smsy, F_Fmsy))+
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "top")+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,3))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"))+
  geom_point(color=mycolors[1],fill='blue',size=2,alpha=0.65,shape=16)+
  annotate("text", x=2.5, y=3,   label= "Mgmt. control = Low",size=2.5) + 
  annotate("text", x=2.5, y=2.65, label= "Risk tolerance = High",size=2.5)  
  

p2 <- ggplot(data = full.sims.top[which(full.sims.top$control==HC),],aes(S_Smsy, F_Fmsy))+
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "top")+
  scale_y_continuous("",limits = c(0,3),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"))+
  geom_point(color=mycolors[3],fill=mycolors[3],size=2,alpha=0.45,shape=16)+
  annotate("text", x=2.5, y=3,   label= "Mgmt. control = High",size=2.5) + 
  annotate("text", x=2.5, y=2.65, label= "Risk tolerance = High",size=2.5)  

p3 <- ggplot() + 
  geom_density(data=full.sims.top, aes(x=F_Fmsy, group=control,fill=control),alpha=0.65, adjust=4)+ 
  scale_fill_manual(values=c(mycolors[1], mycolors[3]))+
  coord_flip()+ 
  scale_x_continuous("",limits = c(0,3),position = "top",labels=NULL)+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+ 
  theme(legend.position="none",plot.margin = unit(c(1,4,-0.5,0), "lines"))+
  geom_vline(xintercept=1)

p4 <- ggplot(data = full.sims.bottom[which(full.sims.bottom$control==LC),],aes(S_Smsy, F_Fmsy))+
  scale_x_continuous("",limits = c(0,4),labels = NULL)+
  scale_y_continuous(name=bquote(" U/ "* U[MSY]),limits = c(0,3))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(1,0.2,0,0), "lines"))+
  geom_point(color=mycolors[4],fill=mycolors[4],size=2,alpha=0.65,shape=16) +
  annotate("text", x=2.5, y=3,   label= "Mgmt. control = Low",size=2.5) + 
  annotate("text", x=2.5, y=2.65, label= "Risk tolerance = Low",size=2.5) 

p5 <- ggplot( data = full.sims.bottom[which(full.sims.bottom$control==HC),],aes(S_Smsy, F_Fmsy))+
  scale_x_continuous("",limits = c(0,4),labels = NULL)+
  scale_y_continuous("",limits = c(0,3),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(1,0.2,0,0), "lines"))+
  geom_point(color=mycolors[2],fill=mycolors[2],size=2,alpha=0.65,shape=16) +
  annotate("text", x=2.5, y=3,   label= "Mgmt. control = High",size=2.5) + 
  annotate("text", x=2.5, y=2.65, label= "Risk tolerance = Low",size=2.5) 

p6 <- ggplot() + 
  geom_density(data=full.sims.bottom, aes(x=F_Fmsy, group=control,fill=control),alpha=0.65, adjust=3)+ 
  scale_fill_manual(values=c(mycolors[4], mycolors[2]))+
  coord_flip()+ 
  scale_x_continuous(limits = c(0,3))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+ 
  theme(legend.position="none",plot.margin = unit(c(1,4,1,0), "lines"))+
  geom_vline(xintercept=1)

p7 <- ggplot() + 
  geom_density(data=btml, aes(x=S_Smsy, group=risk,fill=risk),alpha=0.65, adjust=2.5)+ 
  scale_fill_manual(values=c(mycolors[1], mycolors[4]))+
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+ 
  theme(legend.position="none",plot.margin = unit(c(-0.75,-0.5,4,1.35), "lines"))+
  geom_vline(xintercept=1)+ 
  scale_y_reverse()

p8 <- ggplot() + 
  geom_density(data=btmr, aes(x=S_Smsy, group=risk,fill=risk),alpha=0.65, adjust=4)+ 
  scale_fill_manual(values=c(mycolors[3], mycolors[2]))+
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+ 
  theme(legend.position="none",plot.margin = unit(c(-0.75,-0.5,4,0.75), "lines"))+
  geom_vline(xintercept=1)+ 
  scale_y_reverse()

jpeg("figures/fig_3_kobe_100sims_phi_0.8.Rho_0.6.logFE_1.OU_0.July0852019.jpeg",width=6, height=6, units="in",res=800)
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8, labels = c("A", "B", "C", "D", "E", "F", "G", "H"))

dev.off()
