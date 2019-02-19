# define characteristics of population complex 

pops <- 12 # of populations (MUST BE EVEN)
max.a <- 7 # maximum productivity
min.a <- 2 # minimum productivity
steps <- 10 # of steps between high and low heterogenity
equ_spw <- 80000 # equilibrium abundance (assumes all pops are the same size)
min_cor <- -0.1 # minimum value of degree of correlation (with 10 pops cannot go lower than ... )

inputs <- hetero_prod_func(pops, max.a, min.a, steps, equ_spw,"No","Yes") # generate matrix of alphas and betas
alpha<- inputs$alphas[10,]
beta<- inputs$betas[10,]
Ro <- log(alpha)/beta; sum(Ro)

Smsy <- (1-lambert_W0(exp(1-log(alpha))))/beta
Uopt <- round(0.5* log(alpha)-0.07* log(alpha)^2,digits=2)

ny = 50
phi <- 0.1
rho <- 0.8
episd <- 0.6
Preturn <- c(0,0,1,0)
for.error <- 0.1
OU <- 0

num.sims <- 4

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
