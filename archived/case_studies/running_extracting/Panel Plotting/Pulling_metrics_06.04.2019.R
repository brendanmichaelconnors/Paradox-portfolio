##=============================================================================
## Pulling out metrics for Paradox manuscript
## June 4th, 2019
##=============================================================================

setwd("~/Dropbox/SFU Postdoc/Skeena Work/Paradox of Portfolio/Panel Plotting")
library(rstan)
library(Hmisc)

#------------------------------------------------------------------------------
# First just set up which system we are looking at, bring in the data and then
# the bayesian outputs
#------------------------------------------------------------------------------
# For Skeena
# region <- "Skeena"
# is.uninform <- F
# fnpost = "Skeena.post.out.Nov_16_2017"
# fnpost = paste("Skeena.post.out.Dec_19_2018", sep="")
# dir <- "~/Dropbox/SFU Postdoc/Skeena Work/Paradox of Portfolio/Parameter Estimation"
# setwd(paste(dir, "/", region, sep=""))
# source("Skeena_setup.R")
# ngrows=6
# ngcols=3

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
region <- "Fraser"
is.uninform <- F
fnpost = paste("Fraser.post.out.Dec_19_2018", sep="") # note that this run used the mean historical average population size -- if want prior for two without to be the max of the historical range need to re-run the jags.
dir <- "~/Dropbox/SFU Postdoc/Skeena Work/Paradox of Portfolio/Parameter Estimation"
setwd(paste(dir, "/", region, sep=""))
source("Fraser_setup.R")
ngrows=6
ngcols=3

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

min.yr <- min(BY, na.rm=TRUE)
max.yr <- max(BY, na.rm=TRUE)
yr.range <- max.yr-min.yr

# set up our empty data frames that will be filled with residual values and the
# lag1 values
residuals <- as.data.frame(matrix(ncol=(Nstocks+1), nrow=yr.range+1))
residuals[,1] <- seq(from=min.yr, to=max.yr, 1)
colnames(residuals) <- c("Year", as.character(StNames))

lag1.mat <- data.frame(stock.id = StNames,
                       lag1.val = rep(NA, Nstocks))

median.alpha <- vector()
cv.vals <- vector()

for (i in 1:Nstocks) {
  # pull out stock specific information to fill in residuals and lag values
  temp.stock=as.character(StNames[i])
  print(temp.stock)
  temp.dat <- d[d$CU==temp.stock,]
  
  temp.S <- temp.dat$Esc
  temp.R <- temp.dat$Rec
  temp.yr <- temp.dat$BY
  
  # Get appropriate a and b values for current stock
  ii=which(names(d2)==paste("a.",i,".",sep=""))
  temp.a <- median(d2[,ii])
  jj=which(names(d2)==paste("b.",i,".",sep=""))
  temp.b <- median(d2[,jj])
  
  median.alpha <- c(median.alpha, temp.a)
  cv.vals <- c(cv.vals, sd(temp.R)/mean(temp.R))
  
  # calculate the known lnrs and the predicted values and residuals
  lnrs.dat <- log(temp.R/temp.S)
  lnrs.pred <- temp.a + temp.b*temp.S
  resid.short <- lnrs.pred - lnrs.dat  
  
  # the next section deals with the fact that the residuals are often not 
  # continuous in time, so it adds NAs between the years that are not 
  # continuous
  resid.full <- vector()
  
  yr.diffs <- temp.yr[-1]-temp.yr[-(length(temp.yr))]
  
  if(sum(yr.diffs) > length(yr.diffs)) {
    resid.full[1] <- resid.short[1]
    for(j in 1:length(yr.diffs)) {
      if(yr.diffs[j] == 1) { resid.full <- c(resid.full, resid.short[j+1])
      } else { resid.full <- c(resid.full, rep(NA, (yr.diffs[j]-1)), resid.short[j+1])}
    }
  } else (resid.full = resid.short)
  
  # here, we only assign the years where there are values to the overall 
  # residuals data frame, so that we keep track of all stocks across the 
  # same time range
  residuals[(residuals$Year>=min(temp.yr) & residuals$Year<=max(temp.yr)), (i+1)] <- resid.full
  
  # calculate the lag1 value and save it to the matrix
  lag.val <- acf(resid.full, lag.max=1, na.action=na.pass, plot=FALSE)
  lag1.mat$lag1.val[i] <- lag.val[[1]][2]
  
}

# calculate the parwise correlations across stocks in the same system
# use the rcorr function from Hmisc package, because it does the pairwise
# correlations but rather than deleting all rows that contain NAs, it just 
# removes those for the pairs in question. (values match perfectly with cor()
# function when using Bristol Bay where there are no NAs)
cor.mat <- residuals %>%
  select(-Year) %>%
  as.matrix() %>%
  rcorr(type = "spearman")

cor.final <- cor.mat[[1]]
cor.final[upper.tri(cor.final, diag=TRUE)] <- NA
write.csv(lag1.mat, paste("../../Panel Plotting/lag1_mat_", region, ".csv", sep=""))
write.csv(cor.final, paste("../../Panel Plotting/corr_mat_", region, ".csv", sep=""))

# hand calculations for the table in the paper:
# SYNCHRONY
mean(cor.final, na.rm=TRUE)
min(cor.final, na.rm=TRUE)
max(cor.final, na.rm=TRUE)

# PRODUCTIVITY
exp(min(median.alpha))
exp(max(median.alpha))

# AUTOCORRELATION
mean(lag1.mat$lag1.val)
min(lag1.mat$lag1.val)
max(lag1.mat$lag1.val)

# CV RECRUITMENT
mean(cv.vals)
min(cv.vals)
max(cv.vals)






