##=============================================================================
## Script to bring in data for Fraser River stocks
##=============================================================================

# This file brings in all the Fraser data -- reads it in for both the Bayesian
# model run but also for subsequent plotting. So this brings everything in and
# puts it into the desired format

# It has been made to largely match the previous R scripts for the Skeena and 
# BBay

# Setup Parameters
FBYr <- -99 # set first brood year, "-99" for no constraint
MinSRpts <- 20 # set minimum # of SR data points required to be included in the 
# analysis
fndata <- "FraserData_05.11.2018.csv"


#------------------------------------------------------------------------------
# Read in the data and manipulate it as needed
#------------------------------------------------------------------------------
MaxStocks=19

d0 <- read.csv(fndata, header=T) # all stocks have > 39 yrs of data, so use all
d <- subset(d0,is.na(Rec)==F & is.na(Esc)==F)

StNames = unique(d$CU)
Nstocks = length(StNames)
MaxYrs = length(unique(d$BY))
Nyrs = rep(NA,length=Nstocks)
MaxEsc_allYrs = Nyrs

for(i in 1: Nstocks) {
  temp <- d[d$CU==StNames[i],]
  Nyrs[i] <- nrow(temp)
  MaxEsc_allYrs[i] <- max(temp$Esc)
  # print(StNames[i])
  # print(sum(temp$Esc>0)) # this was just a check to make sure there are
  # the same number of recruitment estimates as there are escapements
  # print(sum(temp$Rec>0))
}
NEscYrs = Nyrs

# Assign data to appropriate elements of S and R arrays
S=matrix(nrow=MaxYrs,ncol=Nstocks);R=S	
SR_BY=matrix(nrow=MaxYrs,ncol=Nstocks)
for (i in 1:Nstocks) {
  d1=subset(d,CU==StNames[i])
  S[1:Nyrs[i],i]=d1$Esc
  R[1:Nyrs[i],i]=d1$Rec
  SR_BY[1:Nyrs[i],i]=d1$BY
}

LNRS <- log(R/S)

# Also creat arrays holding all escapement data and years of escapement 

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
dat.prior <- read.csv("Fraser_beta_priors_HistAv.csv", header=T)
# dat.pr2 <- dat.pr1[dat.pr1$Stock!="Fennell",]
# dat.prior <- dat.pr2[dat.pr2$Stock!="Harrison",]
prSmax=dat.prior$Hist_avg;
prCV=dat.prior$CV

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
