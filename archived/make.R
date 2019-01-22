#------------------------------------------------------------------------------#
# Code to run simulations and generate Figures
# ** make.R last run on 2018-06-18 ** 
#------------------------------------------------------------------------------#
rm(list = ls())

source("load.R")

#------------------------------------------------------------------------------#
# Run simulation to generate figure 1: three performance measures across range 
#	of asynchrony and variation in productivity
#------------------------------------------------------------------------------#

# define characteristics of simulation and create array to store outputs

pops <- 12 # of populations (MUST BE EVEN)
max.a <- 11 # maximum productivity
min.a <- 2 # minimum productivity
steps <- 10 # of steps between high and low heterogenity
equ_spw <- 40000 # equilibrium abundance (assumes all pops are the same size)
min_cor <- -0.1 # minimum value of degree of correlation (with 10 pops cannot go lower than ... )

inputs <- hetero_prod_func(pops, max.a, min.a, steps, equ_spw,"No") # generate matrix of alphas and betas

rho.all <- array(NA,dim=c(steps,1))
rho.range <- seq(min_cor,0.99,length.out=steps)
for(ww in 1:length(rho.range)){ # range of covariation among populaitons
	rho.all[ww,] <- rho.range[ww]
}

ny = 50
num.sims <- 200
phi <- 0.8
episd <- 0.6
Preturn <- c(0,0,1,0)
for.error <- 0.27 
OU <- 0 

outcomes <- array(NA,dim=c(num.sims,7))
mean_out <- array(NA,dim=c(steps,steps,7),dimnames=list(seq(0,1,length.out=steps),seq(min_cor,1,length.out=steps),NULL))

# run simulations

for(ii in 1:steps){
	for(jj in 1:steps){
		# biological characteristics
		alpha<- inputs$alphas[ii,]
		beta<- inputs$betas[ii,]
		Ro <- log(alpha)/beta; sum(Ro)
		rho <- rho.all[jj,]
		control <- 0 #1 is perfect and 0 is none
		MSY.add <- 1
		source("MSY_hcr_function.R")				
		for (l in 1: num.sims){
			out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
			outcomes[l,] <- out$PMs
		}
	mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
	}
}
	
source("Figure_1_multi_panel_heat_map_het_prod.R")

#------------------------------------------------------------------------------#
# Run simulations to generate figure 2 by populating a long form data frame with 
#	outcome of simulations (across a range of harvest control)
#------------------------------------------------------------------------------#
	
# no control, max harvest
control <- 0 
MSY.add <- 1
	
for(ii in 1:steps){
	for(jj in 1: steps){
		alpha<- inputs$alphas[ii,]
		beta<- inputs$betas[ii,]
		Ro <- log(alpha)/beta; sum(Ro)
		rho <- rho.all[jj,]		
		for (l in 1: num.sims){
			out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
			outcomes[l,] <- out$PMs
		}
	mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
	}
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)))
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY")

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
		for (l in 1: num.sims){
			out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
			outcomes[l,] <- out$PMs
		}
	mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
	}
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)))
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY")

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
		for (l in 1: num.sims){
			out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
			outcomes[l,] <- out$PMs
		}
	mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
	}
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)))
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY")

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
		for (l in 1: num.sims){
			out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
			outcomes[l,] <- out$PMs
		}
	mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
	}
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)))
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY")

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
		for (l in 1: num.sims){
			out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
			outcomes[l,] <- out$PMs
		}
	mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
	}
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)))
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY")

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
		for (l in 1: num.sims){
			out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
			outcomes[l,] <- out$PMs
		}
	mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
	}
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)))
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY")

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
		for (l in 1: num.sims){
			out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
			outcomes[l,] <- out$PMs
		}
	mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
	}
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(MSY.add,(steps* steps)))
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","MSY")

master.sims_out <- rbind(master.sims_out,sims_out)

source("Figure_2_risk_vs_stabilty_vs_control.R")

#------------------------------------------------------------------------------#
# Run simulations to generate ilustrative kobe plots in figure 3 
#------------------------------------------------------------------------------#

source("Figure_3_kobe.R")

#------------------------------------------------------------------------------#
# Run simulation to generate figure 4: two performance measures across range 
#	of asynchrony and population richness
#------------------------------------------------------------------------------#

# define characteristics of simulation and create array to store outputs

pops <- c(3,6,9,12,15,18,21,24,27,30)
max.a <- 11 # maximum productivity
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
for.error <- 0.27 
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
			out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
			outcomes[l,] <- out$PMs
		}
	mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
	}
}
	
source("Figure_4_multi_panel_heat_map_richness.R")

#------------------------------------------------------------------------------#
# Run simulations to generate figure 5 by populating a long form data frame with 
#	outcome of simulations (across a range of age composition)
#------------------------------------------------------------------------------#
	
#### no control, max harvest, no age variation

control <- 0 
MSY.add <- 1
age <- 0
Preturn <- c(0,0,1,0)
		
for(ii in 1:steps){
	for(jj in 1: steps){
		alpha<- inputs$alphas[ii,]
		beta<- inputs$betas[ii,]
		Ro <- log(alpha)/beta; sum(Ro)
		rho <- rho.all[jj,]		
		for (l in 1: num.sims){
			out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
			outcomes[l,] <- out$PMs
		}
	mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
	}
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(age,(steps* steps)))
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","age")
master.sims_out <- sims_out

#### no control, max harvest, some age variation
age <- 0.5
Preturn <- c(0,0.5,0.5,0)
		
for(ii in 1: steps){
	for(jj in 1: steps){
		alpha<- inputs$alphas[ii,]
		beta<- inputs$betas[ii,]
		Ro <- log(alpha)/beta; sum(Ro)
		rho <- rho.all[jj,]				
		for (l in 1: num.sims){
			out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
			outcomes[l,] <- out$PMs
		}
	mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
	}
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(age,(steps* steps)))
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","age")
master.sims_out <- rbind(master.sims_out,sims_out)

#### no control, max harvest, a little more age variation

age <- 0.33
Preturn <- c(0,0.33,0.33,0.33)
		
for(ii in 1: steps){
	for(jj in 1: steps){
		alpha<- inputs$alphas[ii,]
		beta<- inputs$betas[ii,]
		Ro <- log(alpha)/beta; sum(Ro)
		rho <- rho.all[jj,]				
		for (l in 1: num.sims){
			out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
			outcomes[l,] <- out$PMs
		}
	mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
	}
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(age,(steps* steps)))
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","age")
master.sims_out <- rbind(master.sims_out,sims_out)

#### no control, max harvest, even more age variation

age <- 0.25
Preturn <- c(0.25,0.25,0.25,0.25)
		
for(ii in 1: steps){
	for(jj in 1: steps){
		alpha<- inputs$alphas[ii,]
		beta<- inputs$betas[ii,]
		Ro <- log(alpha)/beta; sum(Ro)
		rho <- rho.all[jj,]				
		# run simulations!
		for (l in 1: num.sims){
			out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add)
			outcomes[l,] <- out$PMs
		}
	mean_out[ii,jj,] <- apply(outcomes,c(2),quantile,probs=c(0.5),na.rm=T)
	}
}

sims_out.1 <- melt(mean_out[,,5])
sims_out <- cbind(sims_out.1, melt(mean_out[,,6])[,3],rep(control,(steps* steps)),rep(age,(steps* steps)))
colnames(sims_out)<- c("het_prod","sync","ext","CV_har","control","age")
master.sims_out <- rbind(master.sims_out,sims_out)

source("Figure_2_risk_vs_stabilty_vs_ages.R")

