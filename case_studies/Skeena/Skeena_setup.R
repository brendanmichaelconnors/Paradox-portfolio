##=============================================================================
## Script to bring in data for Skeena River stocks
##=============================================================================

# This file brings in all the Skeena data -- reads it in for both the Bayesian
# model run but also for subsequent plotting. So this brings everything in and
# puts it into the desired format


# Setup Parameters
FBYr <- -99 # set first brood year, "-99" for no constraint
MinSRpts <- 20 # set minimum # of SR data points required to be included in the 
# analysis
fndata <- "SX_SRdata.txt"


#------------------------------------------------------------------------------
# Read in the data and manipulate it as needed
#------------------------------------------------------------------------------
MaxStocks=scan(file=fndata,nlines=1,skip=1)

# First pass through to get list of stocks and determine how many years of SR 
# points for each
d0 <- read.table(fndata, skip=3+MaxStocks, header=T) # skip lines as needed
d = subset(d0,is.na(Rec)==F & is.na(Esc)==F & BY>=FBYr) # remove NAs
StNames = unique(d$CU)
Nstocks = length(StNames)
Nyrs = vector(length=Nstocks)
NEscYrs=Nyrs
GdSt = StNames;GdSt[1:Nstocks]=rep(NA,Nstocks)
for (i in 1:Nstocks) {			#Get number of years of valid data for each stock
  d1=subset(d,CU==StNames[i])
  Nyrs[i] = dim(d1)[1]
  if (Nyrs[i]>=MinSRpts) GdSt[i] = StNames[i]
}

# Second pass through data to exclude stocks that don't have enought SR points
d0 <- read.table(file=fndata,header=T,skip=3+MaxStocks)
d = subset(d0,is.na(Rec)==F & is.na(Esc)==F & BY>=FBYr & is.na(match(CU,GdSt))==F)
# this line above is just repulling a subset of d0 but selecting based on whether
# the name is found in the list of names in GdSt -- so it is shorter than d above
StNames=unique(d$CU)
Nstocks=length(StNames)
Nyrs=vector(length=Nstocks)
for (i in 1:Nstocks) {			#Get number of years of valid data for each stock
  d1=subset(d,CU==StNames[i])
  Nyrs[i]=dim(d1)[1]
  
  d1a=subset(d0,CU==StNames[i])	#Need to create an escapement array for all 
  #years with escapement data (some of these years don't have recruit data)
  NEscYrs[i]=dim(d1a)[1]
}

MaxYrs=max(Nyrs)			#Assign data to appropriate elements of S and R arrays
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
  d1a=subset(d0,CU==StNames[i])
  BY[1:NEscYrs[i],i]=d1a$BY
  Esc[1:NEscYrs[i],i]=d1a$Esc
  ER[1:NEscYrs[i],i]=d1a$ER
}

#------------------------------------------------------------------------------
# Read in data on priors
#------------------------------------------------------------------------------
#Set priors on b
dat.prior = read.table(file=fndata,header=T,skip=2,nrows=MaxStocks)
dat.prior2 = subset(dat.prior,is.na(match(CU,StNames))==F)
prSmax=dat.prior2$prSmax;
prSmax.log <- log(prSmax)
prCV=dat.prior2$prCV

prmub=log(1/prSmax)	#convert mean prior on Smax to log b for winbugs model
prtaub=1/prCV^2				#convert from cv to tau
prsdb=1/sqrt(prtaub)

#------------------------------------------------------------------------------
# Side calculation!! R/S correlation matrix
#------------------------------------------------------------------------------
yr.max <- 53
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


