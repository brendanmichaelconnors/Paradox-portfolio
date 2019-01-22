#------------------------------------------------------------------------------#
# Run simulation to generate figure 4: two performance measures across range 
#	of asynchrony and population richness
#------------------------------------------------------------------------------#

# define characteristics of simulation and create array to store outputs

pops <- c(6,9,12,15,18,21,24,27,30,33)
max.a <- 7 # maximum productivity
min.a <- 2 # minimum productivity
steps <- 10 # of steps between high and low heterogenity
equ_spw <- 40000 # equilibrium abundance (assumes all pops are the same size)
min_cor <- 0 # minimum value of degree of correlation (with 10 pops cannot go lower than ... )

rho.all <- array(NA,dim=c(steps,1))
rho.range <- seq(min_cor,0.99,length.out=steps)
for(ww in 1:length(rho.range)){
  rho.all[ww,] <- rho.range[ww]
}

ny = 50
num.sims <- 200

phi <- 0.8
episd <- 0.6
Preturn <- c(0,0,1,0)
for.error <- 0.15 
OU <- 0 

outcomes <- array(NA,dim=c(num.sims,7))
mean_out <- array(NA,dim=c(length(pops),steps,7),dimnames=list(pops,seq(min_cor,1,length.out=steps),NULL))

# run simulations

for(ii in 1:length(pops)){
  for(jj in 1:steps){
    # biological characteristics
    pop.num <- pops[ii]
    inputs <- hetero_prod_func(pop.num, max.a, min.a, steps, equ_spw,"Yes")
    alpha<- inputs$alphas[8,]
    beta<- inputs$betas[8,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]
    control <- 0 #1 is perfect and 0 is none
    MSY.add <- 1
    source("MSY_hcr_function.R")		
    
    # run simulations!
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
        mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

saveRDS(mean_out,"richness_sync_200_0.8_0.6_0010_0_0_heteroP8.25Dec2018")    
##############

# define characteristics of simulation and create array to store outputs

pops <- c(6,9,12,15,18,21,24,27,30,33)
max.a <- 7 # maximum productivity
min.a <- 2 # minimum productivity
steps <- 10 # of steps between high and low heterogenity
equ_spw <- 40000 # equilibrium abundance (assumes all pops are the same size)
min_cor <- 0 # minimum value of degree of correlation (with 10 pops cannot go lower than ... )

rho.all <- array(NA,dim=c(steps,1))
rho.range <- seq(min_cor,0.99,length.out=steps)
for(ww in 1:length(rho.range)){
  rho.all[ww,] <- rho.range[ww]
}

ny = 50
num.sims <- 200

phi <- 0.8
episd <- 0.6
Preturn <- c(0,0,1,0)
for.error <- 0 
OU <- 0 

outcomes <- array(NA,dim=c(num.sims,7))
mean_out <- array(NA,dim=c(length(pops),steps,7),dimnames=list(pops,seq(min_cor,1,length.out=steps),NULL))

# run simulations

for(ii in 1:length(pops)){
  for(jj in 1:steps){
    # biological characteristics
    pop.num <- pops[ii]
    inputs <- hetero_prod_func(pop.num, max.a, min.a, steps, equ_spw,"Yes")
    alpha<- inputs$alphas[2,]
    beta<- inputs$betas[2,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]
    control <- 0 #1 is perfect and 0 is none
    MSY.add <- 1
    source("MSY_hcr_function.R")		
    
    # run simulations!
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

saveRDS(mean_out,"richness_sync_200_0.8_0.6_0010_0_0_heteroP2.25Dec2018")    

##############

# define characteristics of simulation and create array to store outputs

pops <- c(6,9,12,15,18,21,24,27,30,33)
max.a <- 7 # maximum productivity
min.a <- 2 # minimum productivity
steps <- 10 # of steps between high and low heterogenity
equ_spw <- 40000 # equilibrium abundance (assumes all pops are the same size)
min_cor <- 0 # minimum value of degree of correlation (with 10 pops cannot go lower than ... )

rho.all <- array(NA,dim=c(steps,1))
rho.range <- seq(min_cor,0.99,length.out=steps)
for(ww in 1:length(rho.range)){
  rho.all[ww,] <- rho.range[ww]
}

ny = 50
num.sims <- 200

phi <- 0.8
episd <- 0.6
Preturn <- c(0,0,1,0)
for.error <- 0 
OU <- 0 

outcomes <- array(NA,dim=c(num.sims,7))
mean_out <- array(NA,dim=c(length(pops),steps,7),dimnames=list(pops,seq(min_cor,1,length.out=steps),NULL))

# run simulations

for(ii in 1:length(pops)){
  for(jj in 1:steps){
    # biological characteristics
    pop.num <- pops[ii]
    inputs <- hetero_prod_func(pop.num, max.a, min.a, steps, equ_spw,"Yes")
    alpha<- inputs$alphas[6,]
    beta<- inputs$betas[6,]
    Ro <- log(alpha)/beta; sum(Ro)
    rho <- rho.all[jj,]
    control <- 0 #1 is perfect and 0 is none
    MSY.add <- 1
    source("MSY_hcr_function.R")		
    
    # run simulations!
    for (l in 1: num.sims){
      out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)
      outcomes[l,] <- out$PMs
    }
    mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
  }
}

saveRDS(mean_out,"richness_sync_200_0.8_0.6_0010_0_0_heteroP6.25Dec2018")    