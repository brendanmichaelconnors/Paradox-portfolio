#------------------------------------------------------------------------------#
# MSY based harvest control rule function and code to generate array of harvest 
#	control rules under varying degrees of control
#------------------------------------------------------------------------------#	

hcr = function(egfloor, run,for.error,OU){
	run.est <- run * (rlnorm(1,0,for.error)); if(is.na(run.est)==TRUE){run.est <- run}
	if(run.est - egfloor <= 0){hr = 0}
	if(run.est > egfloor){ hr = (run.est - egfloor)/run}
	if(hr < 0){hr=0}
	if(hr >1 ){hr=1}
	return(hr)
	}

smsy <- (1-lambert_W0(exp(1-log(alpha))))/beta

# populate matrix of realized harvest rates across a range of run-sizes
runs <- seq(1,1000000,length.out=10000)	

har.rate.all<- matrix(NA,length(runs),1)
	
	for(iii in 1:length(runs)){har.rate.all[iii] <- hcr(sum(smsy),runs[iii],0,0)}

realized_har <- cbind(runs, har.rate.all)

# add sub stock harvest rates under perfect control

har.rate <- array(NA,dim=c(length(runs),2, length(alpha)+1))

for(w in 1:length(alpha)){
	for(iii in 1:length(runs)){
		har.rate[iii,2,w] <- hcr(smsy[w],runs[iii],0,0)* MSY.add
		har.rate[iii,1,w] <- runs[iii]
		}
	}

har.rate[,2,length(alpha)+1] <- har.rate.all
har.rate[,1,length(alpha)+1] <- runs

master.har <- har.rate