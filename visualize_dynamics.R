pops <- 6 # of populations (MUST BE EVEN)
max.a <- 7 # maximum productivity
min.a <- 2 # minimum productivity
steps <- 10 # of steps between high and low heterogenity
equ_spw <- 40000 # equilibrium abundance (assumes all pops are the same size)

inputs <- hetero_prod_func(pops, max.a, min.a, steps, equ_spw,"No") # generate matrix of alphas and betas
alpha<- inputs$alphas[10,]
beta<- inputs$betas[10,]
Ro <- log(alpha)/beta; sum(Ro)

ny = 50
phi <- 0.6
rho <- 0.6
episd <- 0.8
Preturn <- c(0,0,1,0)
for.error <- 0.1
OU <- 0

control <- 0 
MSY.add <- 1

source("MSY_hcr_function.R")				
out <- process(ny, Ro, rho, phi, Preturn, U, alpha, beta, control, MSY.add, for.error)

plot(log(out$S[,1]),type="l",ylim=c(0,max(log(out$S))))
for(i in 1:pops){
  points(log(out$S[,i]),type="l")
}
