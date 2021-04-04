##=============================================================================
## Executing the JAGS model
##=============================================================================

# This script will run the JAGS model to estimate alpha and beta parameters 
# for the linear Ricker form, for the system specified at the start

library(R2jags)
# setwd() # set working directory to location of R script
dir.base <- getwd() 

# Choose region
region <- "Skeena" # choose region, three options are "Fraser", "Bristol Bay" or
# "Skeena"

# Bring in the data associated with the region in question - it is in the folder
# with the associated name, and will be sourced using a script in that folder
# specifying how to read in the data
if(region=="Skeena") {
# For Skeena
setwd(paste(dir.base, "/", region, sep=""))
source("Skeena_setup.R")
}

if(region=="Bristol Bay") {
# For Bristol Bay
setwd(paste(dir.base, "/", region, sep=""))
source("BBay_setup.R")
}

if(region=="Fraser") {
# For Fraser
setwd(paste(dir.base, "/", region, sep=""))
source("Fraser_setup.R")
}

#------------------------------------------------------------------------------
# Write out the JAGS model
#------------------------------------------------------------------------------

modelFilename = "Bayes_SR_informative.txt"
cat("
    model{
    
    for(i in 1:Nstocks) {	
    a[i]~dunif(0, 4) 
    
    b[i]~dlnorm(prmub[i],prtaub[i])	#prior on stock-independent b
    
    sd[i]~dunif(0.05,10)
    tau[i]<-pow(sd[i],-0.5)	
    
    }
    
    for(i in 1:Nstocks) {	
    for(j in 1:Nyrs[i]) { # remember that Nyrs is a matrix, just like LNRS
    LNRS[j,i]~dnorm(Pred[j,i],tau[i]) # LNRS = log(R/s)
    Pred[j,i]<-a[i]-b[i]*S[j,i]
    }
    }
    
    
    }
    ", fill=TRUE, file=modelFilename)
  
jags.data = list("Nstocks","Nyrs","LNRS","S","cov", "prmub","prtaub")
jags.parms = c("a","b","b2","sd", "mu_b2","sd_b2")


#-----------------------------------------------------------------------------
#   Run Model
#-----------------------------------------------------------------------------
print("Running Parallel")
jagsfit.p <- jags.parallel(data=jags.data,  parameters.to.save=jags.parms,
                           n.thin=100,
                           n.iter=500000, model.file=modelFilename,
                           n.burnin = 50000,n.chains=4)
post = as.mcmc(jagsfit.p)
mypost = as.matrix(post, chain=F)

##### INFERENCE #####
gelman.diag(post, multivariate = F)
pdf(paste("Diagnostics/ConvergenceTest_", format(Sys.time(), "%b_%d_%Y"), "_2",
          ".pdf", sep=""), 10, 10)
par(mfrow=c(3,3))
gelman.plot(post)
dev.off()

pdf(paste("Diagnostics/Trace&Density_", format(Sys.time(), "%b_%d_%Y"), "_2",
          ".pdf", sep=""), width=6, height=10)
par(mfrow=c(3,2))
plot(post)
dev.off()

pdf(paste("Diagnostics/Density_", format(Sys.time(), "%b_%d_%Y"), "_2", 
          ".pdf", sep=""), 10, 10)
par(mfrow=c(3,3))
plot(post, trace=F, density=T)
dev.off()
    
model.probs <- round(cbind(est = colMeans(mypost),sd = apply(mypost,2,sd),
                           ci = t(apply(mypost,2,quantile,c(.025,.975)))),
                     digits=8)
model.probs
write.table(mypost,file=paste("Outputs/", region, ".post.out.", 
                              format(Sys.time(), "%b_%d_%Y"), "_2", 
                              sep=""),col.names=T, row.names=F)


