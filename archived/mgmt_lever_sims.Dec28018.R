#------------------------------------------------------------------------------#
# Run simulations to generate figure 2 by populating a long form data frame with 
#	outcome of simulations (across a range of harvest and management control)
#------------------------------------------------------------------------------#



##############
pops <- 12 # of populations (MUST BE EVEN)
max.a <- 7 # maximum productivity
min.a <- 2 # minimum productivity
steps <- 10 # of steps between high and low heterogenity
equ_spw <- 40000 # equilibrium abundance (assumes all pops are the same size)
min_cor <- 0 # minimum value of degree of correlation (with 10 pops cannot go lower than ... )

inputs <- hetero_prod_func(pops, max.a, min.a, steps, equ_spw,"No") # generate matrix of alphas and betas

rho.all <- array(NA,dim=c(steps,1))
rho.range <- seq(min_cor,0.99,length.out=steps)
for(ww in 1:length(rho.range)){ # range of covariation among populaitons
  rho.all[ww,] <- rho.range[ww]
}

ny = 50
num.sims <- 10
phi <- 0.8
episd <- 0.6
Preturn <- c(0,0,1,0)
for.error <- 0.1
OU <- 0.0

outcomes <- array(NA,dim=c(num.sims,7))
mean_out <- array(NA,dim=c(steps,steps,7),dimnames=list(seq(0,1,length.out=steps),seq(min_cor,1,length.out=steps),NULL))

#############
# no control, max harvest
control <- 0 
MSY.add <- 1

for(ii in 1:steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]		
    source("MSY_hcr_function.R")				
	for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add,for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- sims_out

# perfect control, max harvest
control <- 1 
MSY.add <- 1

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

# 0.125 control, max harvest
control <- 0.125 
MSY.add <- 1

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)


#### 0.25 control, max harvest
control <- 0.25 
MSY.add <- 1

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

#### 0.375 control, max harvest
control <- 0.375 
MSY.add <- 1

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

#### 0.5 control, max harvest
control <- 0.5 
MSY.add <- 1

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]		
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

#### 0.75 control, max harvest
control <- 0.75 
MSY.add <- 1

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]		
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

############################################

#############
# no control, max harvest
control <- 0 
MSY.add <- 0.75

for(ii in 1:steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]		
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

# perfect control, max harvest
control <- 1 
MSY.add <- 0.75

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

# 0.125 control, max harvest
control <- 0.125 
MSY.add <- 0.75

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)


#### 0.25 control, max harvest
control <- 0.25 
MSY.add <- 0.75

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

#### 0.375 control, max harvest
control <- 0.375 
MSY.add <- 0.75

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

#### 0.5 control, max harvest
control <- 0.5 
MSY.add <- 0.75

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]		
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

#### 0.75 control, max harvest
control <- 0.75 
MSY.add <- 0.75

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]		
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

############################################


#############
# no control, max harvest
control <- 0 
MSY.add <- 0.5

for(ii in 1:steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]		
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

# perfect control, max harvest
control <- 1 
MSY.add <- 0.5

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

# 0.125 control, max harvest
control <- 0.125 
MSY.add <- 0.5

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)


#### 0.25 control, max harvest
control <- 0.25 
MSY.add <- 0.5

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

#### 0.375 control, max harvest
control <- 0.375 
MSY.add <- 0.5

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

#### 0.5 control, max harvest
control <- 0.5 
MSY.add <- 0.5

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]		
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

#### 0.75 control, max harvest
control <- 0.75 
MSY.add <- 0.5

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]		
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

############################################


################################################



# 0 control, 0.25 harvest
control <- 0 
MSY.add <- 0.25

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

#### 0 control, 0.875 harvest
control <- 0
MSY.add <- 0.875

for(ii in 1: steps){
  for(jj in 1: steps){
    alpha<- inputs$alphas[ii,]
    beta<- inputs$betas[ii,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]				
    source("MSY_hcr_function.R")				
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")

master.sims_out <- rbind(master.sims_out,sims_out)

saveRDS(master.sims_out,"output/mgmt_risk_sims.phi_0.8.Rho_0.6.logFE_0.OU_0.Dec282018")
