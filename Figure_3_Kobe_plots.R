# define characteristics of population complex 

pops <- 12 # of populations (MUST BE EVEN)
max.a <- 7 # maximum productivity
min.a <- 2 # minimum productivity
steps <- 10 # of steps between high and low heterogenity
equ_spw <- 40000 # equilibrium abundance (assumes all pops are the same size)
min_cor <- -0.1 # minimum value of degree of correlation (with 10 pops cannot go lower than ... )

inputs <- hetero_prod_func(pops, max.a, min.a, steps, equ_spw,"No") # generate matrix of alphas and betas
alpha<- inputs$alphas[10,]
beta<- inputs$betas[10,]
Ro <- log(alpha)/beta; sum(Ro)

Smsy <- (1-lambert_W0(exp(1-log(alpha))))/beta
Uopt <- round(0.5* log(alpha)-0.07* log(alpha)^2,digits=2)

ny = 50
phi <- 0.4
rho <- 0.6
episd <- 0.6
Preturn <- c(0,0,1,0)
for.error <- 0.1
OU <- 0

num.sims <- 10

time_period <- c(48:51)

# high risk, low mgmt control

control <- 0 
MSY.add <- 1

sim_output <- matrix(NA,pops,4,dimnames=list(NULL,c("S_Smsy","F_Fmsy","control","risk")))
sim_output.2 <- matrix(NA,pops,4,dimnames=list(NULL,c("S_Smsy","F_Fmsy","control","risk")))

source("MSY_hcr_function.R")				
for (i in 1:num.sims){
  out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
  if(i==1){
    sim_output[,1] <- apply(out$S[48:51,],c(2),mean)/Smsy
    sim_output[,2] <- apply(out$H[48:51,]/out$N[48:51,],c(2),mean)/Uopt
    sim_output[,3] <- rep(control,pops)
    sim_output[,4] <- rep(MSY.add,pops)
  }else{
    sim_output.2[,1] <- apply(out$S[48:51,],c(2),mean)/Smsy
    sim_output.2[,2] <- apply(out$H[48:51,]/out$N[48:51,],c(2),mean)/Uopt
    sim_output.2[,3] <- rep(control,pops)
    sim_output.2[,4] <- rep(MSY.add,pops)

      sim_output <- rbind(sim_output,sim_output.2)
  }
}

full.sims.top <- sim_output

# high risk, high mgmt control

control <- 0.85
MSY.add <- 1

source("MSY_hcr_function.R")				
for (i in 1:num.sims){
  out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
  if(i==1){
    sim_output[,1] <- apply(out$S[48:51,],c(2),mean)/Smsy
    sim_output[,2] <- apply(out$H[48:51,]/out$N[48:51,],c(2),mean)/Uopt
    sim_output[,3] <- rep(control,pops)
    sim_output[,4] <- rep(MSY.add,pops)

  }else{
    sim_output.2[,1] <- apply(out$S[48:51,],c(2),mean)/Smsy
    sim_output.2[,2] <- apply(out$H[48:51,]/out$N[48:51,],c(2),mean)/Uopt
    sim_output.2[,3] <- rep(control,pops)
    sim_output.2[,4] <- rep(MSY.add,pops)
    sim_output <- rbind(sim_output,sim_output.2)
  }
}

full.sims.top <- rbind(full.sims.top,sim_output)

full.sims.top <- as.data.frame(full.sims.top)
full.sims.top[,3] <- as.factor(full.sims.top[,3])
full.sims.top[,4] <- as.factor(full.sims.top[,4])
full.sims.top <- full.sims.top[complete.cases(full.sims.top),]

# low risk, low mgmt control

control <- 0 
MSY.add <- 0.5

sim_output <- matrix(NA,pops,4,dimnames=list(NULL,c("S_Smsy","F_Fmsy","control","risk")))
sim_output.2 <- matrix(NA,pops,4,dimnames=list(NULL,c("S_Smsy","F_Fmsy","control","risk")))

source("MSY_hcr_function.R")				
for (i in 1:num.sims){
  out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
  if(i==1){
    sim_output[,1] <- apply(out$S[48:51,],c(2),mean)/Smsy
    sim_output[,2] <- apply(out$H[48:51,]/out$N[48:51,],c(2),mean)/Uopt
    sim_output[,3] <- rep(control,pops)
    sim_output[,4] <- rep(MSY.add,pops)
  }else{
    sim_output.2[,1] <- apply(out$S[48:51,],c(2),mean)/Smsy
    sim_output.2[,2] <- apply(out$H[48:51,]/out$N[48:51,],c(2),mean)/Uopt
    sim_output.2[,3] <- rep(control,pops)
    sim_output.2[,4] <- rep(MSY.add,pops)
    
    sim_output <- rbind(sim_output,sim_output.2)
  }
}

full.sims.bottom <- sim_output


# low risk, high mgmt control

control <- 0.85
MSY.add <- 0.5

source("MSY_hcr_function.R")				
for (i in 1:num.sims){
  out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
  if(i==1){
    sim_output[,1] <- apply(out$S[48:51,],c(2),mean)/Smsy
    sim_output[,2] <- apply(out$H[48:51,]/out$N[48:51,],c(2),mean)/Uopt
    sim_output[,3] <- rep(control,pops)
    sim_output[,4] <- rep(MSY.add,pops)
    
  }else{
    sim_output.2[,1] <- apply(out$S[48:51,],c(2),mean)/Smsy
    sim_output.2[,2] <- apply(out$H[48:51,]/out$N[48:51,],c(2),mean)/Uopt
    sim_output.2[,3] <- rep(control,pops)
    sim_output.2[,4] <- rep(MSY.add,pops)
    sim_output <- rbind(sim_output,sim_output.2)
  }
}

full.sims.bottom <- rbind(full.sims.bottom,sim_output)

full.sims.bottom <- as.data.frame(full.sims.bottom)
full.sims.bottom[,3] <- as.factor(full.sims.bottom[,3])
full.sims.bottom[,4] <- as.factor(full.sims.bottom[,4])
full.sims.bottom <- full.sims.bottom[complete.cases(full.sims.bottom),]


btml <- rbind(full.sims.top[which(full.sims.top$control=="0"),],full.sims.bottom[which(full.sims.bottom$control=="0"),])

btmr <- rbind(full.sims.top[which(full.sims.top$control=="0.85"),],full.sims.bottom[which(full.sims.bottom$control=="0.85"),])

##########################################
#Summary stats


Overfished <- subset(full.sims.top[which(full.sims.top$control=="0"),],S_Smsy >1 & F_Fmsy <0.90)
dim(Overfished)[1]/dim(HH)[1]  

Overfished <- subset(full.sims.top[which(full.sims.top$control=="0.85"),],S_Smsy >1 & F_Fmsy <0.90)
dim(Overfished)[1]/dim(HH)[1] 

##########################################
#Plot

p1 <- ggplot(data = full.sims.top[which(full.sims.top$control=="0"),] ,aes(S_Smsy, F_Fmsy))+
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "top")+
  scale_y_continuous(name=bquote(" F/ "* F[MSY]),limits = c(0,4))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"))+
  geom_point(color="blue",fill='blue',size=2,alpha=0.65,shape=16)+
  annotate("text", x=2.5, y=4,   label= "Mgmt. control = Low",size=2.5) + 
  annotate("text", x=2.5, y=3.65, label= "Risk tolerance = High",size=2.5)  
  

p2 <- ggplot(data = full.sims.top[which(full.sims.top$control=="0.85"),],aes(S_Smsy, F_Fmsy))+
  scale_x_continuous(name=bquote(" S/"* S[MSY]),limits = c(0,4),position = "top")+
  scale_y_continuous("",limits = c(0,4),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(0,0.2,0.2,0), "lines"))+
  geom_point(color='green',fill='green',size=2,alpha=0.45,shape=16)+
  annotate("text", x=2.5, y=4,   label= "Mgmt. control = High",size=2.5) + 
  annotate("text", x=2.5, y=3.65, label= "Risk tolerance = High",size=2.5)  

p3 <- ggplot() + 
  geom_density(data=full.sims.top, aes(x=F_Fmsy, group=control,fill=control),alpha=0.65, adjust=2)+ 
  scale_fill_manual(values=c("blue", "green"))+
  coord_flip()+ 
  scale_x_continuous("",limits = c(0,4),position = "top",labels=NULL)+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+ 
  theme(legend.position="none",plot.margin = unit(c(2,4,0,0), "lines"))+
  geom_vline(xintercept=1)

p4 <- ggplot(data = full.sims.bottom[which(full.sims.bottom$control=="0"),],aes(S_Smsy, F_Fmsy))+
  scale_x_continuous("",limits = c(0,4),labels = NULL)+
  scale_y_continuous(name=bquote(" F/ "* F[MSY]),limits = c(0,4))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(1,0.2,0,0), "lines"))+
  geom_point(color='red',fill='red',size=2,alpha=0.65,shape=16) +
  annotate("text", x=2.5, y=4,   label= "Mgmt. control = Low",size=2.5) + 
  annotate("text", x=2.5, y=3.65, label= "Risk tolerance = Low",size=2.5) 

p5 <- ggplot( data = full.sims.bottom[which(full.sims.bottom$control=="0.85"),],aes(S_Smsy, F_Fmsy))+
  scale_x_continuous("",limits = c(0,4),labels = NULL)+
  scale_y_continuous("",limits = c(0,4),labels = NULL)+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  theme_bw()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),plot.margin = unit(c(1,0.2,0,0), "lines"))+
  geom_point(color='black',fill='black',size=2,alpha=0.65,shape=16) +
  annotate("text", x=2.5, y=4,   label= "Mgmt. control = High",size=2.5) + 
  annotate("text", x=2.5, y=3.65, label= "Risk tolerance = Low",size=2.5) 


p6 <- ggplot() + 
  geom_density(data=full.sims.bottom, aes(x=F_Fmsy, group=control,fill=control),alpha=0.65, adjust=3)+ 
  scale_fill_manual(values=c("red", "black"))+
  coord_flip()+ 
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+ 
  theme(legend.position="none",plot.margin = unit(c(1,4,1,0), "lines"))+
  geom_vline(xintercept=1)

p7 <- ggplot() + 
  geom_density(data=btml, aes(x=S_Smsy, group=risk,fill=risk),alpha=0.65, adjust=2)+ 
  scale_fill_manual(values=c("blue", "red"))+
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+ 
  theme(legend.position="none",plot.margin = unit(c(-0.75,-0.5,4,1.35), "lines"))+
  geom_vline(xintercept=1)+ 
  scale_y_reverse()

p8 <- ggplot() + 
  geom_density(data=btmr, aes(x=S_Smsy, group=risk,fill=risk),alpha=0.65, adjust=2)+ 
  scale_fill_manual(values=c("green", "black"))+
  scale_x_continuous(limits = c(0,4))+
  theme(panel.background = element_blank(),axis.text = element_blank(), axis.ticks=element_blank(),axis.title=element_blank())+ 
  theme(legend.position="none",plot.margin = unit(c(-0.75,-0.5,4,0.75), "lines"))+
  geom_vline(xintercept=1)+ 
  scale_y_reverse()

#jpeg("figures/fig_2_kobe_10sims_0.6_0.8_0010.jpeg",width=6, height=6, units="in",res=800)
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8, labels = c("A", "B", "C", "D", "E", "F", "G", "H"))

dev.off()
