########################################################################################
# richness_sims.R
#
# Mixed-stock fishery simulations across range of richness, synchrony and stock 
#   productivities
# Last updated: July 5, 2019
# Author: B. Connors (DFO)
#        
########################################################################################

# --- Evenness simulations -------------------------------------------------------------
  
  # --- Set common conditions for simulations  -------------------------------------------
  pops <- 10
  max.a <- 7 # maximum productivity
  min.a <- 2 # minimum productivity
  steps <- 10 # of steps between high and low heterogeneity
  equ_spw_master <- 400000 # equilibrium abundance (assumes all pops are the same size)
  min_cor <- 0 # minimum value of degree of correlation (with 10 pops cannot go lower than ... )
  
  rho.all <- array(NA,dim=c(steps,1))
  rho.range <- seq(min_cor,0.99,length.out=steps)
  for(ww in 1:length(rho.range)){
    rho.all[ww,] <- rho.range[ww]
  }
  
  ny = 50
  num.sims <-300
  
  phi <- 0.8
  episd <- 0.6
  Preturn <- c(0,0,1,0)
  for.error <- 0.1
  OU <- 0 
  
  # matrix of specific pop proportions to generate desired evenness
  even <- matrix(NA,10,10)
  even[1,]<-c(0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.991)
  even[2,]<-c(0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.01,0.02,0.963)
  even[3,]<-c(0.001,0.001,0.001,0.001,0.001,0.001,0.014,0.03,0.03,0.92)
  even[4,]<-c(0.001,0.001,0.001,0.001,0.001,0.001,0.02,0.03,0.1,0.844)
  even[5,]<-c(0.001,0.001,0.001,0.015,0.02,0.02,0.03,0.05,0.05,0.812)
  even[6,]<-c(0.001,0.004,0.01,0.015,0.02,0.02,0.03,0.05,0.15,0.7)
  even[7,]<-c(0.001,0.01,0.015,0.02,0.03,0.04,0.05,0.15,0.2,0.484)
  even[8,]<-c(0.001,0.01,0.015,0.02,0.03,0.04,0.15,0.2,0.25,0.284)
  even[9,]<-c(0.01,0.01,0.01,0.05,0.1,0.1,0.12,0.2,0.2,0.2)
  even[10,]<-c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
  
  # --- High heterogeneity in stock productivity correlated with evenness  -----------------------------------------
  outcomes <- array(NA,dim=c(num.sims,7))
  mean_out <- array(NA,dim=c(steps,steps,7),dimnames=list(seq(1,10),seq(min_cor,1,length.out=steps),NULL))
  
  for(ii in 1:steps){
    for(jj in 1:steps){
      # biological characteristics
      pop.num <- pops
      inputs <- hetero_prod_func_even(pop.num, max.a, min.a, steps, equ_spw_master,"Yes","No",even)
      alpha<- inputs$alphas[ii,]
      beta<- inputs$betas[ii,]
      Ro <- log(alpha)/beta; sum(Ro)
      rho <- rho.all[jj,]
      control <- 0 #1 is perfect and 0 is none
      MSY.add <- 1
      source("MSY_hcr_function.R")		
      
      # run simulations!
      for (l in 1: num.sims){
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
        outcomes[l,] <- out$PMs
      }
          mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
    }
  }
  
  saveRDS(mean_out,"output/even_sync_100_0.8_0.6_0010_0.1_0_heteroP8.23Feb2021")    
  
  # --- High heterogeneity in stock productivity un-correlated with evenness  -----------------------------------------
  outcomes <- array(NA,dim=c(num.sims,7))
  mean_out <- array(NA,dim=c(steps,steps,7),dimnames=list(seq(1,10),seq(min_cor,1,length.out=steps),NULL))
  
  for(ii in 1:steps){
    for(jj in 1:steps){
      # biological characteristics
      # run simulations!
      for (l in 1: num.sims){
        
      for(i in 1:10){
        even[i,]<-sample(even[i,])
      }
      
      pop.num <- pops
      inputs <- hetero_prod_func_even(pop.num, max.a, min.a, steps, equ_spw_master,"Yes","No",even)
      alpha<- inputs$alphas[ii,]
      beta<- inputs$betas[ii,]
      Ro <- log(alpha)/beta; sum(Ro)
      rho <- rho.all[jj,]
      control <- 0 #1 is perfect and 0 is none
      MSY.add <- 1
      source("MSY_hcr_function.R")		

        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
        outcomes[l,] <- out$PMs
      }
      mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
    }
  }
  
  saveRDS(mean_out,"output/even_sync_no_covary_300_0.8_0.6_0010_0.1_0_heteroP8.03March2021")    
  