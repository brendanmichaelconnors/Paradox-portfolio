########################################################################################
# simulations.R
#
# Mixed-stock fishery simulations across range of richness, evenness, synchrony and stock 
#   productivities. Not pretty but get the job done.
#        
########################################################################################

# --- Richness simulations -------------------------------------------------------------
  
  # --- Set common conditions for simulations  -------------------------------------------
  pops <- c(6,9,12,15,18,21,24,27,30,33) # number of populations
  max.a <- 7 # maximum productivity
  min.a <- 2 # minimum productivity
  steps <- 10 # of steps between high and low heterogenity
  equ_spw <- 400000 # equilibrium abundance (assumes all pops are the same size)
  min_cor <- 0 # minimum value of degree of correlation (with 10 pops cannot go lower than ... )
  
  rho.all <- array(NA,dim=c(steps,1)) # range of correlation in recruitment
  rho.range <- seq(min_cor,0.99,length.out=steps)
  for(ww in 1:length(rho.range)){
    rho.all[ww,] <- rho.range[ww]
  }
  
  ny = 50 # number of years in simulation
  num.sims <-300 # number of Monte carlo trials
  
  phi <- 0.8 # auto-correlation in recruitment
  episd <- 0.6 # recruitment process variaiton
  Preturn <- c(0,0,1,0) # age at maturity
  for.error <- 0.1 # forecast error
  OU <- 0 # outcome uncertainty
  
  # --- High heterogeneity in stock productivity  -----------------------------------------
  outcomes <- array(NA,dim=c(num.sims,7))
  mean_out <- array(NA,dim=c(length(pops),steps,7),dimnames=list(pops,seq(min_cor,1,length.out=steps),NULL))
  
  for(ii in 1:length(pops)){
    for(jj in 1:steps){
      pop.num <- pops[ii]
      inputs <- hetero_prod_func(pop.num, max.a, min.a, steps, equ_spw,"Yes","Yes")
      alpha<- inputs$alphas[8,]
      beta<- inputs$betas[8,]
      Ro <- log(alpha)/beta; sum(Ro)
      rho <- rho.all[jj,]
      control <- 0 
      MSY.add <- 1
      source("MSY_hcr_function.R")		
      
      for (l in 1: num.sims){
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
        outcomes[l,] <- out$PMs
      }      
      mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
    }
  }
  
  saveRDS(mean_out,"output/richness_sync_300_0.8_0.6_0010_0.1_0_heteroP8.08March2021")    
  
  # --- Low heterogeneity in stock productivity  -----------------------------------------
  outcomes <- array(NA,dim=c(num.sims,7))
  mean_out <- array(NA,dim=c(length(pops),steps,7),dimnames=list(pops,seq(min_cor,1,length.out=steps),NULL))
  
  for(ii in 1:length(pops)){
    for(jj in 1:steps){
      pop.num <- pops[ii]
      inputs <- hetero_prod_func(pop.num, max.a, min.a, steps, equ_spw,"Yes","Yes")
      alpha<- inputs$alphas[2,]
      beta<- inputs$betas[2,]
      Ro <- log(alpha)/beta; sum(Ro)
      rho <- rho.all[jj,]
      control <- 0 
      MSY.add <- 1
      source("MSY_hcr_function.R")		
      
      for (l in 1: num.sims){
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
        outcomes[l,] <- out$PMs
      }
      mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
    }
  }
  
  saveRDS(mean_out,"output/richness_sync_300_0.8_0.6_0010_0.1_0_heteroP2.08March2021")    
  
  # --- Moderate heterogeneity in stock productivity  -----------------------------------------
  outcomes <- array(NA,dim=c(num.sims,7))
  mean_out <- array(NA,dim=c(length(pops),steps,7),dimnames=list(pops,seq(min_cor,1,length.out=steps),NULL))
  
  for(ii in 1:length(pops)){
    for(jj in 1:steps){
      pop.num <- pops[ii]
      inputs <- hetero_prod_func(pop.num, max.a, min.a, steps, equ_spw,"Yes","Yes")
      alpha<- inputs$alphas[5,]
      beta<- inputs$betas[5,]
      Ro <- log(alpha)/beta; sum(Ro)
      rho <- rho.all[jj,]
      control <- 0 
      MSY.add <- 1
      source("MSY_hcr_function.R")		
      
      for (l in 1: num.sims){
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
        outcomes[l,] <- out$PMs
      }
      mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
    }
  }
  
  saveRDS(mean_out,"output/richness_sync_300_0.8_0.6_0010_0.1_0_heteroP5.08March2021")    
  
# --- Evenness simulations -------------------------------------------------------------
  
  # --- Set common conditions for simulations  -------------------------------------------
  pops <- 10 # number of populations
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
  
  even <- matrix(NA,10,10) # matrix of specific population proportions to generate desired evenness
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
      pop.num <- pops
      inputs <- hetero_prod_func_even(pop.num, max.a, min.a, steps, equ_spw_master,"Yes","No",even)
      alpha<- inputs$alphas[ii,]
      beta<- inputs$betas[ii,]
      Ro <- log(alpha)/beta; sum(Ro)
      rho <- rho.all[jj,]
      control <- 0 
      MSY.add <- 1
      source("MSY_hcr_function.R")		
      
      for (l in 1: num.sims){
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
        outcomes[l,] <- out$PMs
      }
      mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
    }
  }
  
  saveRDS(mean_out,"output/even_sync_300_0.8_0.6_0010_0.1_0_heteroP8.23Feb2021")    
  
  # --- High heterogeneity in stock productivity un-correlated with evenness  -----------------------------------------
  outcomes <- array(NA,dim=c(num.sims,7))
  mean_out <- array(NA,dim=c(steps,steps,7),dimnames=list(seq(1,10),seq(min_cor,1,length.out=steps),NULL))
  
  for(ii in 1:steps){
    for(jj in 1:steps){
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
        control <- 0 
        MSY.add <- 1
        source("MSY_hcr_function.R")		
        
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
        outcomes[l,] <- out$PMs
      }
      mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
    }
  }
  
  saveRDS(mean_out,"output/even_sync_no_covary_300_0.8_0.6_0010_0.1_0_heteroP8.03March2021")    
  
# --- Mgmt. lever simulations ------------------------------------------------------------
  
  # --- Set common conditions for simulations  ------------------------------------------
  pops <- 12 # of populations (MUST BE EVEN)
  max.a <- 7 # maximum productivity
  min.a <- 2 # minimum productivity
  steps <- 10 # of steps between high and low heterogeneity
  equ_spw <- 40000 # equilibrium abundance (assumes all pops are the same size)
  min_cor <- 0 # minimum value of degree of correlation (with 10 pops cannot go lower than ... )
  
  inputs <- hetero_prod_func(pops, max.a, min.a, steps, equ_spw,"No","Yes") # generate matrix of alphas and betas
  
  rho.all <- array(NA,dim=c(steps,1))
  rho.range <- seq(min_cor,0.99,length.out=steps)
  for(ww in 1:length(rho.range)){ # range of covariation among populations
    rho.all[ww,] <- rho.range[ww]
  }
  
  ny = 50 # number of years in simulation
  num.sims <-200 # number of Monte carlo trials
  phi <- 0.8 # auto-correlation in recruitment
  episd <- 0.6 # recruitment process variaiton
  Preturn <- c(0,0,1,0) # age at maturity
  for.error <- 0.1 # forecast error

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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
        outcomes[l,] <- out$PMs
      }
      mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
    }
  }
  
  sims_out.1 <- melt(mean_out[,,5])
  sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
  colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")
  
  master.sims_out <- rbind(master.sims_out,sims_out)
  
  # --- Simulations: mgmt. control = 0, risk tol. = 0.75 ------------------------------------
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
        outcomes[l,] <- out$PMs
      }
      mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
    }
  }
  
  sims_out.1 <- melt(mean_out[,,5])
  sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
  colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")
  
  master.sims_out <- rbind(master.sims_out,sims_out)
  
  # --- Simulations: mgmt. control = 0.25, risk tol. = 0.75 ------------------------------------
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
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
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
        outcomes[l,] <- out$PMs
      }
      mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
    }
  }
  
  sims_out.1 <- melt(mean_out[,,5])
  sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
  colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")
  
  master.sims_out <- rbind(master.sims_out,sims_out)
 
  # --- Simulations: mgmt. control = 0, risk tol. = 0.35 ------------------------------------
  control <- 0
  MSY.add <- 0.35
  
  for(ii in 1: steps){
    for(jj in 1: steps){
      alpha<- inputs$alphas[ii,]
      beta<- inputs$betas[ii,]
      Ro <- log(alpha)/beta; sum(Ro)
      rho <- rho.all[jj,]				
      source("MSY_hcr_function.R")				
      for (l in 1: num.sims){
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
        outcomes[l,] <- out$PMs
      }
      mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
    }
  }
  
  sims_out.1 <- melt(mean_out[,,5])
  sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
  colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")
  
  master.sims_out <- rbind(master.sims_out,sims_out)
  
  # --- Simulations: mgmt. control = 0, risk tol. = 0.3 ------------------------------------
  control <- 0
  MSY.add <- 0.3
  
  for(ii in 1: steps){
    for(jj in 1: steps){
      alpha<- inputs$alphas[ii,]
      beta<- inputs$betas[ii,]
      Ro <- log(alpha)/beta; sum(Ro)
      rho <- rho.all[jj,]				
      source("MSY_hcr_function.R")				
      for (l in 1: num.sims){
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
        outcomes[l,] <- out$PMs
      }
      mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
    }
  }
  
  sims_out.1 <- melt(mean_out[,,5])
  sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
  colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")
  
  master.sims_out <- rbind(master.sims_out,sims_out)
  
  # --- Simulations: mgmt. control = 0, risk tol. = 0.1 ------------------------------------
  control <- 0
  MSY.add <- 0.1
  
  for(ii in 1: steps){
    for(jj in 1: steps){
      alpha<- inputs$alphas[ii,]
      beta<- inputs$betas[ii,]
      Ro <- log(alpha)/beta; sum(Ro)
      rho <- rho.all[jj,]				
      source("MSY_hcr_function.R")				
      for (l in 1: num.sims){
        out <- process(ny, Ro, rho, phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
        outcomes[l,] <- out$PMs
      }
      mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
    }
  }
  
  sims_out.1 <- melt(mean_out[,,5])
  sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)),melt(mean_out[,,2])[,3])
  colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY","harvest")
  
  master.sims_out <- rbind(master.sims_out,sims_out)
  
  # --- Save all combined simulation output 
  saveRDS(master.sims_out,"output/mgmt_risk_sims.200.phi_0.8.Rho_0.6.logFE_0.1.OU_0.08March2021")

  
  
  