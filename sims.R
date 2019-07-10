########################################################################################
# richness_sims.R
#
# Mixed-stock fishery simulaitons across range of rishness, synchrony and stock 
#   productivies
# Last updated: July 5, 2019
# Author: B. Connors (DFO)
#        
########################################################################################

# --- Richness simulations -------------------------------------------------------------
  
  # --- Set common conditions for simulations  -------------------------------------------
  pops <- c(6,9,12,15,18,21,24,27,30,33)
  max.a <- 7 # maximum productivity
  min.a <- 2 # minimum productivity
  steps <- 10 # of steps between high and low heterogenity
  equ_spw <- 800000 # equilibrium abundance (assumes all pops are the same size)
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
  for.error <- 0.1
  OU <- 0 
  
  # --- High heterogenity in stock productivity  -----------------------------------------
  outcomes <- array(NA,dim=c(num.sims,7))
  mean_out <- array(NA,dim=c(length(pops),steps,7),dimnames=list(pops,seq(min_cor,1,length.out=steps),NULL))
  
  for(ii in 1:length(pops)){
    for(jj in 1:steps){
      # biological characteristics
      pop.num <- pops[ii]
      inputs <- hetero_prod_func(pop.num, max.a, min.a, steps, equ_spw,"Yes","Yes")
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
  
  saveRDS(mean_out,"output/richness_sync_200_0.8_0.6_0010_0.1_0_heteroP8.08July2019")    
  
  # --- Low heterogenity in stock productivity  -----------------------------------------
  outcomes <- array(NA,dim=c(num.sims,7))
  mean_out <- array(NA,dim=c(length(pops),steps,7),dimnames=list(pops,seq(min_cor,1,length.out=steps),NULL))
  
  for(ii in 1:length(pops)){
    for(jj in 1:steps){
      # biological characteristics
      pop.num <- pops[ii]
      inputs <- hetero_prod_func(pop.num, max.a, min.a, steps, equ_spw,"Yes","Yes")
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
  
  saveRDS(mean_out,"richness_sync_200_0.8_0.6_0010_0.1_0_heteroP2.08July2019")    
  
  # --- Moderate heterogenity in stock productivity  -----------------------------------------
  outcomes <- array(NA,dim=c(num.sims,7))
  mean_out <- array(NA,dim=c(length(pops),steps,7),dimnames=list(pops,seq(min_cor,1,length.out=steps),NULL))
  
  for(ii in 1:length(pops)){
    for(jj in 1:steps){
      # biological characteristics
      pop.num <- pops[ii]
      inputs <- hetero_prod_func(pop.num, max.a, min.a, steps, equ_spw,"Yes","Yes")
      alpha<- inputs$alphas[5,]
      beta<- inputs$betas[5,]
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
  
  saveRDS(mean_out,"richness_sync_200_0.8_0.6_0010_0.1_0_heteroP5.08July2019")    
  

# --- Mgmt. lever simulations ------------------------------------------------------------
  
  # --- Set common conditions for simulations  ------------------------------------------
  pops <- 12 # of populations (MUST BE EVEN)
  max.a <- 7 # maximum productivity
  min.a <- 2 # minimum productivity
  steps <- 10 # of steps between high and low heterogenity
  equ_spw <- 40000 # equilibrium abundance (assumes all pops are the same size)
  min_cor <- 0 # minimum value of degree of correlation (with 10 pops cannot go lower than ... )
  
  inputs <- hetero_prod_func(pops, max.a, min.a, steps, equ_spw,"No","Yes") # generate matrix of alphas and betas
  
  rho.all <- array(NA,dim=c(steps,1))
  rho.range <- seq(min_cor,0.99,length.out=steps)
  for(ww in 1:length(rho.range)){ # range of covariation among populaitons
    rho.all[ww,] <- rho.range[ww]
  }
  
  ny = 50
  num.sims <- 100
  phi <- 0.8
  episd <- 0.6
  Preturn <- c(0,0,1,0)
  for.error <- 0.1
  OU <- 0.0
  
  outcomes <- array(NA,dim=c(num.sims,7))
  mean_out <- array(NA,dim=c(steps,steps,7),dimnames=list(seq(0,1,length.out=steps),seq(min_cor,1,length.out=steps),NULL))
  
  # --- Simulations: mgmt. control = 0, risk tol. = 1 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 1, risk tol. = 1 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0.125, risk tol. = 1 ---------------------------------
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
  
  # --- Simulations: mgmt. control = 0.25, risk tol. = 1 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0.375, risk tol. = 1 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0.5, risk tol. = 1 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0.75, risk tol. = 1 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0, risk tol. = 0.751 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 1, risk tol. = 0.75 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0.125, risk tol. = 0.75 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0, risk tol. = 1 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0.375, risk tol. = 0.75 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0.5, risk tol. = 0.75 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0.75, risk tol. = 0.75 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0, risk tol. = 0.5 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 1, risk tol. = 0.5 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0.125, risk tol. = 0.5 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0.25, risk tol. = 0.5 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0.375, risk tol. = 0.5 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0.5, risk tol. = 0.5 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0.75, risk tol. = 0.5 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0, risk tol. = 0.25 ------------------------------------
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
  
  # --- Simulations: mgmt. control = 0, risk tol. = 0.875 ------------------------------------
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
 
  # --- Save all combined simulation output ------------------------------------
  saveRDS(master.sims_out,"output/mgmt_risk_sims.100.phi_0.8.Rho_0.6.logFE_0.1.OU_0.09July2019")
  
  