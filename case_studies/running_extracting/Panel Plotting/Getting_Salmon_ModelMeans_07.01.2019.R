##=============================================================================
## Making the data for the panel plot
## November 26th, 2018
##=============================================================================

## May 29 2019 modification: I was calculating the "S" for time periods from a
## ten-year period, but since that does not match with sockeye LH, I have 
## altered this code to allow for 8-year or 12-year periods.

# Trying to make the data.frame and then plots for the heatmap that Jon wants
# but only uses means and not all model outputs

## New funky function to create a matrix for a heatmap of our Kobe outputs
# Making my own plotting code and code to just look at JAGS output
# library(R2jags)

#------------------------------------------------------------------------------
# First just set up which system we are looking at, bring in the data and then
# the bayesian outputs
#------------------------------------------------------------------------------
# For Skeena
region <- "Skeena"
is.uninform <- F
fnpost = "Skeena.post.out.Nov_16_2017"
fnpost = paste("Skeena.post.out.Dec_19_2018", sep="")
dir <- "~/Dropbox/SFU Postdoc/Skeena Work/Paradox of Portfolio/Parameter Estimation"
setwd(paste(dir, "/", region, sep=""))
source("Skeena_setup.R")
ngrows=6
ngcols=3

# For Bristol Bay
# region <- "Bristol Bay"
# is.uninform <- F
# fnpost <- "Bristol Bay.post.out.Dec_19_2018_2"
# dir <- "~/Dropbox/SFU Postdoc/Skeena Work/Paradox of Portfolio/Parameter Estimation"
# setwd(paste(dir, "/", region, sep=""))
# source("BBay_setup.R")
# ngrows=3
# ngcols=3

# For Fraser
# region <- "Fraser"
# is.uninform <- F
# fnpost = paste("Fraser.post.out.Dec_19_2018", sep="") # note that this run used the mean historical average population size -- if want prior for two without to be the max of the historical range need to re-run the jags.
# dir <- "~/Dropbox/SFU Postdoc/Skeena Work/Paradox of Portfolio/Parameter Estimation"
# setwd(paste(dir, "/", region, sep=""))
# source("Fraser_setup.R")
# ngrows=6
# ngcols=3

# source("../plotting_functions.R")
# DoStats=T
# fnBench="Bench.out.unif"	
# fnStatus="Status.out.unif"	
# ifelse(is.uninform==T, pr.type <- "uninformative", pr.type <- "informative")

# Read in Bayesian outputs 
d2<-read.table(file=fnpost,header=T)
nsims=dim(d2)[1]
d2.mcmc <- as.mcmc(d2)

#------------------------------------------------------------------------------
# Getting outputs for Smsy and S current / historical values
#------------------------------------------------------------------------------

lower.yr <- c(1969, 1979, 1989, 1999)
yr.range <- 10

temp.Smsy <- matrix(NA, nrow=Nstocks, ncol=8)
temp.Umsy <- matrix(NA, nrow=Nstocks, ncol=8)

for (i in 1:Nstocks) {
  (temp.stock=as.character(StNames[i]))
  
  # Get appropriate a and b values for current stock
  ii=which(names(d2)==paste("a.",i,".",sep=""))
  temp.a <- d2[,ii]
  jj=which(names(d2)==paste("b.",i,".",sep=""))
  temp.b <- d2[,jj]
  
  Smsy.all <- temp.a/temp.b * (0.5-0.07*temp.a)
  Uopt.all <- (0.5*temp.a - 0.07*temp.a^2)
  
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
  print(nrow(sub.stock[lower.yr[4]<sub.stock$BY & sub.stock$BY<=(lower.yr[4]+yr.range),]))
  
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
temp.Smsy2 <- as.data.frame(temp.Smsy)
temp.Smsy2 <- temp.Smsy2[is.nan(temp.Smsy2$S2000s)==FALSE,]

temp.Umsy2 <- as.data.frame(temp.Umsy)
temp.Umsy2 <- temp.Umsy2[is.nan(temp.Umsy2$U2000s)==FALSE,]


write.csv(temp.Smsy2, paste("../../Panel Plotting/", region , "_Smsy_", yr.range, "yrs.csv", sep=""))
write.csv(temp.Umsy2, paste("../../Panel Plotting/", region , "_Umsy_", yr.range, "yrs.csv", sep=""))


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

write.csv(ab.mat, paste(region, "_AlphaBeta_estimates.csv", sep=""), row.names=F)
