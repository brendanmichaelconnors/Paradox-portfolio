########################################################################################
# kobe_simulations.R
#
# Run simulations across 2 combinations of mgmt. control and risk tolerance based on 
#   characteristics of large mixed-stock sockeye fisheries for illustrative Kobe plots
#        
########################################################################################

# define characteristics of population complex 
pops <- 12 # of populations (MUST BE EVEN)
max.a <- 7 # maximum productivity
min.a <- 2 # minimum productivity
steps <- 10 # of steps between high and low heterogeneity
equ_spw <- 40000 # equilibrium abundance (assumes all pops are the same size)

inputs <- hetero_prod_func(pops, max.a, min.a, steps, equ_spw,"No","Yes") # generate matrix of alphas and betas
alpha<- inputs$alphas[10,]
beta<- inputs$betas[10,]
Ro <- log(alpha)/beta; sum(Ro)

Smsy <- (1-lambert_W0(exp(1-log(alpha))))/beta
Uopt <- round(0.5* log(alpha)-0.07* log(alpha)^2,digits=2)

ny = 50
phi <- 0.3
rho <- 0.25
episd <- 0.4
Preturn <- c(0,0.7,0.3,0)
for.error <- 0.05
OU <- 0
num.sims <- 100

# set management control and and risk tolerance
HC <- 0.85
LC <- 0
  
HR <- 1
LR <- 0.5  

# high risk, low mgmt control

control <- LC 
MSY.add <- HR

sim_output <- matrix(NA,pops,4,dimnames=list(NULL,c("S_Smsy","F_Fmsy","control","risk")))
sim_output.2 <- matrix(NA,pops,4,dimnames=list(NULL,c("S_Smsy","F_Fmsy","control","risk")))

source("MSY_hcr_function.R")				
for (i in 1:num.sims){
  out <- process(ny, Ro, rho, phi, Preturn, episd , alpha, beta, control, MSY.add, for.error)
  if(i==1){
    sim_output[,1] <- apply(out$S[41:51,],c(2),mean)/Smsy
    sim_output[,2] <- apply(out$H[41:51,]/out$N[41:51,],c(2),mean)/Uopt
    sim_output[,3] <- rep(control,pops)
    sim_output[,4] <- rep(MSY.add,pops)
  }else{
    sim_output.2[,1] <- apply(out$S[41:51,],c(2),mean)/Smsy
    sim_output.2[,2] <- apply(out$H[41:51,]/out$N[41:51,],c(2),mean)/Uopt
    sim_output.2[,3] <- rep(control,pops)
    sim_output.2[,4] <- rep(MSY.add,pops)
    
    sim_output <- rbind(sim_output,sim_output.2)
  }
}

full.sims.top <- sim_output

# high risk, high mgmt control

control <- HC
MSY.add <- HR

source("MSY_hcr_function.R")				
for (i in 1:num.sims){
  out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
  if(i==1){
    sim_output[,1] <- apply(out$S[41:51,],c(2),mean)/Smsy
    sim_output[,2] <- apply(out$H[41:51,]/out$N[41:51,],c(2),mean)/Uopt
    sim_output[,3] <- rep(control,pops)
    sim_output[,4] <- rep(MSY.add,pops)
    
  }else{
    sim_output.2[,1] <- apply(out$S[41:51,],c(2),mean)/Smsy
    sim_output.2[,2] <- apply(out$H[41:51,]/out$N[41:51,],c(2),mean)/Uopt
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

control <- LC 
MSY.add <- LR

sim_output <- matrix(NA,pops,4,dimnames=list(NULL,c("S_Smsy","F_Fmsy","control","risk")))
sim_output.2 <- matrix(NA,pops,4,dimnames=list(NULL,c("S_Smsy","F_Fmsy","control","risk")))

source("MSY_hcr_function.R")				
for (i in 1:num.sims){
  out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
  if(i==1){
    sim_output[,1] <- apply(out$S[41:51,],c(2),mean)/Smsy
    sim_output[,2] <- apply(out$H[41:51,]/out$N[41:51,],c(2),mean)/Uopt
    sim_output[,3] <- rep(control,pops)
    sim_output[,4] <- rep(MSY.add,pops)
  }else{
    sim_output.2[,1] <- apply(out$S[41:51,],c(2),mean)/Smsy
    sim_output.2[,2] <- apply(out$H[41:51,]/out$N[41:51,],c(2),mean)/Uopt
    sim_output.2[,3] <- rep(control,pops)
    sim_output.2[,4] <- rep(MSY.add,pops)
    
    sim_output <- rbind(sim_output,sim_output.2)
  }
}

full.sims.bottom <- sim_output


# low risk, high mgmt control

control <- HC
MSY.add <- LR

source("MSY_hcr_function.R")				
for (i in 1:num.sims){
  out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
  if(i==1){
    sim_output[,1] <- apply(out$S[41:51,],c(2),mean)/Smsy
    sim_output[,2] <- apply(out$H[41:51,]/out$N[41:51,],c(2),mean)/Uopt
    sim_output[,3] <- rep(control,pops)
    sim_output[,4] <- rep(MSY.add,pops)
    
  }else{
    sim_output.2[,1] <- apply(out$S[41:51,],c(2),mean)/Smsy
    sim_output.2[,2] <- apply(out$H[41:51,]/out$N[41:51,],c(2),mean)/Uopt
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


btml <- rbind(full.sims.top[which(full.sims.top$control==LC),],full.sims.bottom[which(full.sims.bottom$control==LC),])

btmr <- rbind(full.sims.top[which(full.sims.top$control==HC),],full.sims.bottom[which(full.sims.bottom$control==HC),])

# generate some summary statistics
output <- rbind(btml,btmr)

XX <- "0" # mgmt. control
YY <- "0.5" # conservation risk tolerance

OFingT <- 0.2 # over-fishing threshold
OFedT <- 0.2 # over-fished threshold

kobe_data <- output[which(output$control==XX & output$risk  == YY),]

Sweet_spot <- nrow(subset(kobe_data,F_Fmsy < (1+OFingT) & S_Smsy > (1-OFedT) & F_Fmsy > (1-OFingT) & S_Smsy < (1+OFedT)))/nrow(kobe_data)
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
            Overfishging,
            Overfished,
            Sweet_spot
  ),
  digits=2)
