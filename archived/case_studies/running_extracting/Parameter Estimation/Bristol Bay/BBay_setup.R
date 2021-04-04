##=============================================================================
## Bayesian Estimation Procedure for alpha, beta estimation
## November 16th, 2017
##=============================================================================

# This file brings in all the BBay data -- reads it in for both the Bayesian
# model run but also for subsequent plotting. So this brings everything in and
# puts it into the desired format
# This is all based on the code Brendan sent me -- original file named 
# "HBSRM.R" ... all I have done is cut out unnecessary pieces, and reorganized
# so that it is more understandable to me, and so that I can adapt it to run 
# for the three different systems we are looking at

# Setup Parameters
FBYr <- -99 # set first brood year, "-99" for no constraint
#MinSRpts <- 3 # set minimum # of SR data points required to be included in the 
# analysis
fndata <- "Bristol Bay Spawner-Recruit Data_wER.csv"


#------------------------------------------------------------------------------
# Read in the data and manipulate it as needed
#------------------------------------------------------------------------------
MaxStocks=8
Nstocks=8

d <- read.csv(fndata, header=T)

StNames = unique(d$CU)
Nstocks = length(StNames)
MaxYrs = length(unique(d$BY))
Nyrs = rep(MaxYrs, Nstocks)
NEscYrs = Nyrs


#Assign data to appropriate elements of S and R arrays
S=matrix(nrow=MaxYrs,ncol=Nstocks);R=S	
SR_BY=matrix(nrow=MaxYrs,ncol=Nstocks)
for (i in 1:Nstocks) {
  d1=subset(d,CU==StNames[i])
  S[1:Nyrs[i],i]=d1$Esc
  R[1:Nyrs[i],i]=d1$Rec
  SR_BY[1:Nyrs[i],i]=d1$BY
}

LNRS <- log(R/S)

#Also creat arrays holding all escapement data and years of escapement 
#(note some of these years don't have recruitment data) 
MaxEscYrs=max(NEscYrs)			
Esc=matrix(nrow=MaxEscYrs,ncol=Nstocks);BY=Esc;ER=Esc;
for (i in 1:Nstocks) {
  d1a=subset(d,CU==StNames[i])
  BY[1:NEscYrs[i],i]=d1a$BY
  Esc[1:NEscYrs[i],i]=d1a$Esc
  ER[1:NEscYrs[i],i]=d1a$ER
}

#------------------------------------------------------------------------------
# Read in data on priors
#------------------------------------------------------------------------------
#Set priors on b
dat.prior = read.table("BBay_beta_priors.csv", sep=",", header=T)
prSmax=dat.prior$Hist_Avg;
prCV=dat.prior$Hist_CV

prmub=log(1/prSmax)	#convert mean prior on Smax to log b for winbugs model
prtaub=1/prCV^2				#convert from cv to tau
prsdb=1/sqrt(prtaub)

prSmax.log <- log(prSmax)

#------------------------------------------------------------------------------
# Side calculation!! R/S correlation matrix
#------------------------------------------------------------------------------
yr.max <- max(Nyrs)
rps.dat <- matrix(nrow=yr.max, ncol=(Nstocks+1))
rps.dat[,1] <- seq(min(d$BY),max(d$BY))
for(i in 1:Nstocks) {
  d1=subset(d,CU==StNames[i])
  for(j in 1:yr.max) {
    yr.dat <- subset(d1,BY==rps.dat[j,1])
    ifelse(nrow(yr.dat)>0, rps.dat[j,i+1]<-yr.dat$Rec/yr.dat$Esc, rps.dat[j,2]<-NA)
  }
}

# take out the years with NAs, so only do correlation on matching years
rps.data <- na.omit(rps.dat)

correlations <- cor(rps.data[,-1])[lower.tri(cor(rps.data[,-1]))]
mean(correlations)
range(correlations)


