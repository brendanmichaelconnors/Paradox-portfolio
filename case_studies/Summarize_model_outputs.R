##=============================================================================
## Making the data for the panel plot
##=============================================================================

# Script to take in JAGS outputs and produce Smsy and Umsy estimates that can
# then be used in the Kobe plots or other summary metrics


#------------------------------------------------------------------------------
# Set up requirements
#------------------------------------------------------------------------------

require(gsl)
require(coda)

# setwd() # set working directory to location of R script
dir.base <- getwd() 

region <- "Skeena" # choose region, three options are "Fraser", "Bristol Bay" or
# "Skeena"

# set file names for each of the systems (these are named by date when Run_JAGS.R
# script is run
skeena.out <- paste("Skeena.post.out.Dec_19_2018", sep="")
fraser.out <- paste("Fraser.post.out.Dec_19_2018", sep="")
bbay.out <- "Bristol Bay.post.out.Dec_19_2018"

# Bring in the data assocaited with the region in question - it is in the folder
# with the associated name, and will be sourced using a script in that folder
# specifying how to read in the data
if(region=="Skeena") {
  # For Skeena
  is.uninform <- F
  fnpost =  skeena.out
  setwd(paste(dir.base, "/", region, sep=""))
  source("Skeena_setup.R")
  ngrows=6
  ngcols=3
}

if(region=="Bristol Bay") {
  # For Bristol Bay
  region <- "Bristol Bay"
  is.uninform <- F
  fnpost <- bbay.out
  dir <- dir.base
  setwd(paste(dir.base, "/", region, sep=""))
  source("BBay_setup.R")
  ngrows=3
  ngcols=3
}

if(region=="Fraser") {
  # For Fraser
  is.uninform <- F
  fnpost = fraser.out
  setwd(paste(dir.base, "/", region, sep=""))
  source("Fraser_setup.R")
  ngrows=6
  ngcols=3
}


# Read in Bayesian outputs for the region that was specified
d2<-read.table(file=paste("Outputs/", fnpost, sep=""), header=T)
nsims=dim(d2)[1]
d2.mcmc <- as.mcmc(d2)

#------------------------------------------------------------------------------
# Getting outputs for Smsy and S current / historical values
#------------------------------------------------------------------------------

lower.yr <- c(1969, 1979, 1989, 1999)
yr.range <- 12

temp.Smsy <- matrix(NA, nrow=Nstocks, ncol=8)
temp.Umsy <- matrix(NA, nrow=Nstocks, ncol=8)

for (i in 1:Nstocks) {
  (temp.stock=as.character(StNames[i]))
  
  # Get appropriate a and b values for current stock
  ii=which(names(d2)==paste("a.",i,".",sep=""))
  temp.a <- d2[,ii]
  jj=which(names(d2)==paste("b.",i,".",sep=""))
  temp.b <- d2[,jj]
  
  # Smsy.all <- temp.a/temp.b * (0.5-0.07*temp.a)
  # Uopt.all <- (0.5*temp.a - 0.07*temp.a^2)
  Smsy.all <- (1 - lambert_W0(exp(1-temp.a))) / temp.b
  Uopt.all <- (1 - lambert_W0(exp(1-temp.a)))
  
  # breaking the escapement dataset into two time periods
  sub.stock <- subset(d,CU==StNames[i])
  sub.stock2 <- sub.stock[sub.stock$BY > 1970,]
  temp.yrs <- dim(sub.stock2)[1]
  
  Scurrent <- mean(tail(sub.stock2$Esc, 10))
  Shistorical <- mean(head(sub.stock2$Esc, 10))
  S1970s <- mean(sub.stock[lower.yr[1]<sub.stock$BY & sub.stock$BY<=(lower.yr[1]+yr.range),]$Esc)
  S1980s <- mean(sub.stock[lower.yr[2]<sub.stock$BY & sub.stock$BY<=(lower.yr[2]+yr.range),]$Esc)
  S1990s <- mean(sub.stock[lower.yr[3]<sub.stock$BY & sub.stock$BY<=(lower.yr[3]+yr.range),]$Esc)
  S2000s <- mean(sub.stock[lower.yr[4]<sub.stock$BY & sub.stock$BY<=(lower.yr[4]+yr.range),]$Esc)
  Sall <- mean(na.omit(sub.stock$Esc))

  Ucurrent <- mean(tail(sub.stock2$ER, 10))
  Uhistorical <- mean(head(sub.stock2$ER, 10))
  U1970s <- mean(sub.stock[lower.yr[1]<sub.stock$BY & sub.stock$BY<=(lower.yr[1]+yr.range),]$ER)
  U1980s <- mean(sub.stock[lower.yr[2]<sub.stock$BY & sub.stock$BY<=(lower.yr[2]+yr.range),]$ER)
  U1990s <- mean(sub.stock[lower.yr[3]<sub.stock$BY & sub.stock$BY<=(lower.yr[3]+yr.range),]$ER)
  U2000s <- mean(sub.stock[lower.yr[4]<sub.stock$BY & sub.stock$BY<=(lower.yr[4]+yr.range),]$ER)
  Uall <- mean(na.omit(sub.stock$ER))
  
  # calculate the ratios, but we only want those that are with the 95% credible
  # interval of Smsy
  
  Smsy.CI <- quantile(Smsy.all, c(0.025, 0.975))
  Smsy <- Smsy.all
  Smsy[which(Smsy<Smsy.CI[1]| Smsy>Smsy.CI[2])] = NA
  temp.Smsy[i,] <- c(median(Smsy.all), Smsy.CI[1], Smsy.CI[2], 
                    S1970s, S1980s, S1990s, S2000s, Sall)
  ###### Change mean(Smsy.all) or median(Smsy.all)
  ###### set the last value in the vector to Scurrent or Savg
  
  
  Uopt.CI <- quantile(Uopt.all, c(0.025, 0.975))
  Uopt <- Uopt.all
  Uopt[which(Uopt<Uopt.CI[1] | Uopt>Uopt.CI[2])] = NA
  temp.Umsy[i,] <- c(median(Uopt.all), Uopt.CI[1], Uopt.CI[2], 
                    U1970s, U1980s, U1990s, U2000s, Uall)
  ###### Change mean(Umsy.all) or median(Umsy.all)
  ###### set the last value in the vector to Ucurrent or Uavg
  
}


colnames(temp.Smsy) <- c("meanSmsy", "lowerCI", "upperCI", "S1970s", "S1980s", "S1990s", "S2000s", "SAll")
row.names(temp.Smsy) <- StNames

colnames(temp.Umsy) <- c("meanUmsy", "lowerCI", "upperCI", "U1970s", "U1980s", "U1990s", "U2000s", "UAll")
row.names(temp.Umsy) <- StNames

# REMOVE ROWS WITH NA IN 2000S
# these are not included in the figures or analysis, so have to be taken out
temp.Smsy2 <- as.data.frame(temp.Smsy)
temp.Smsy2 <- temp.Smsy2[is.nan(temp.Smsy2$S2000s)==FALSE,]

temp.Umsy2 <- as.data.frame(temp.Umsy)
temp.Umsy2 <- temp.Umsy2[is.nan(temp.Umsy2$U2000s)==FALSE,]


write.csv(temp.Smsy2, paste("Outputs/", region, "_Smsy_", yr.range, "yrs.csv", sep=""))
write.csv(temp.Umsy2, paste("Outputs/", region, "_Umsy_", yr.range, "yrs.csv", sep=""))


#------------------------------------------------------------------------------
# Getting out Alpha and Beta parameters
#------------------------------------------------------------------------------
ab.mat <- data.frame(stock=StNames,
                      alpha=rep(NA, length(StNames)),
                      beta=rep(NA, length(StNames)))

for (i in 1:Nstocks) {
  (temp.stock=as.character(StNames[i]))
  
  # Get appropriate a and b values for current stock
  ii=which(names(d2)==paste("a.",i,".",sep=""))
  temp.a <- d2[,ii]
  jj=which(names(d2)==paste("b.",i,".",sep=""))
  temp.b <- d2[,jj]
  
  ab.mat[i,c(2:3)] <- c(mean(temp.a), mean(temp.b))
}

write.csv(ab.mat, paste("Outputs/", region, "_AlphaBeta_estimates.csv", sep=""), row.names=F)


#------------------------------------------------------------------------------
# Getting system mean and sd for s/smsy across time periods
#------------------------------------------------------------------------------

sys.means <- data.frame(period = rep(colnames(temp.Smsy2[4:7]),2),
                        measure = rep(c("Smsy", "Umsy"), each=4),
                        means = rep(NA, 8),
                        sd = rep(NA, 8))
                        
for(m in 4:7) {
 temp.meanS <- mean(temp.Smsy2[,m] / temp.Smsy2$meanSmsy)
 temp.sdS <- sd(temp.Smsy2[,m] / temp.Smsy2$meanSmsy)
 sys.means[m-3, c(3,4)] <- c(temp.meanS, temp.sdS)
 
 temp.meanU <- mean(temp.Umsy2[,m] / temp.Umsy2$meanUmsy)
 temp.sdU <- sd(temp.Umsy2[,m] / temp.Umsy2$meanUmsy)
 sys.means[m+1, c(3,4)] <- c(temp.meanU, temp.sdU)
}


write.csv(sys.means, paste("Outputs/", region, "_SmsyMeanSD_Table2.csv", sep=""), row.names=F)
