######################################################################
# functions.R
# Functions for mixed-stock fishery simulations  
#
######################################################################

#------------------------------------------------------------------------------#
# Status function; estimates equilibrium spwn abundance and catch plus whether 
# over fished or extinct
#------------------------------------------------------------------------------#
#U <- harvest rate
#a <- productivity
#b <- density dependence 
SC.eq <- function(U,a,b){
  a <- log(a)
  S.eq <- max(0,(a-(-log(1-U)))/b)
  C.eq <- max(0,((a-(-log(1-U)))/b)*exp(a-b*((a-(-log(1-U)))/b))-((a-(-log(1-U)))/b))
  OF <- ifelse(U>0.5*a-0.07*a^2,1,0)
  EX <- ifelse(S.eq==0,1,0)
  return(c(S.eq,C.eq,OF,EX))
  }


#------------------------------------------------------------------------------#
# Multi-stock simulation function
#------------------------------------------------------------------------------#
# ny <- the number of years on top of a 20 year initialization
# Ro <- the sub-stock recruitment at time zero
# rho <- the expected correlation among stocks
# phi <- the expected correlation through time
# Preturn <- the expected proportion of spawners from age 4-7
# episd <- random recruitment variation
# alpha <- sub-stock productivity (not in log space)
# beta <- sub-stock density dependence 
# control <- degree of management control (1 is perfect stock specific Umsy, 0 is aggregate Umsy)
# MSY.add <- degree of downward adjustment of Umsy (e.g., 1 is none, 0.5 is 50% reduction in realized harvest rate) 

process = function(ny,Ro,rho,phi,Preturn,episd,alpha,beta,control,MSY.add,for.error){
	ny = ny+20
	ns = length(Ro) #number of sub-stocks

	#Create correlation among stocks in recruitment residuals
	R <- matrix(rho,ns,ns) #empty correlation matrix
	for(i in 1:ns)R[i,i] <- 1
	V <- diag(rep(episd,ns)) # diagonal matrix of sqrt(variances)
	Sig <- V %*% R %*% V #variance-covariance matrix
	epi = rmvnorm(n=ny, sigma=Sig)

	#Build time series of Spawners (S), abundance of returning spawners pre-harvest
	# (N), and the component of the residual that is correlated throught time (v)
	R = t(matrix(Ro,ns,ny))
	S = R * (1-0)
	v = R; v[,]=0
	R[1:7,]=t(replicate(7,Ro,simplify=T))*exp(epi[1:7,])
	N = array(0,dim=c(ny,4,ns))
	Ntot = R; Ntot[,]=0
	H = Ntot
	S = Ntot
	predR = Ntot
	N[5:7,1,]=R[5:7-(4),]*Preturn[1]
	N[6:7,2,]=R[6:7-(5),]*Preturn[2]
	N[7,3,]=R[7-(6),]*Preturn[3]
	
	# Loop through years of simulation	
	for(i in (7+1):ny){
		N[i,1,]=R[i-(4),]*Preturn[1]
		N[i,2,]=R[i-(5),]*Preturn[2]
		N[i,3,]=R[i-(6),]*Preturn[3]
		N[i,4,]=R[i-(7),]*Preturn[4]
		#N[N[,,]=='NaN'] <- 0
		Ntot[i,]=colSums(N[i,,])
		#Ntot[i,]=Ntot[i,]*rlnorm(1,0,for.error)
	
			for (yy in 1:length(alpha)){
			u <- master.har[which.min(abs(master.har[,1,yy]-(Ntot[i,yy]))),2,yy]*control + 
					master.har[which.min(abs(master.har[,1,length(alpha)+1]-sum(Ntot[i,]))),2,length(alpha)+1]*(1-control)
					
		outcome_error <- (1+rnorm(1,0,OU))
		H[i,yy] =  Ntot[i,yy] * u *ifelse(outcome_error<0, 0, outcome_error)*MSY.add 			
			}
		
		S_exp = Ntot[i,]-H[i,] ; S_exp[S_exp<0] = 0
		S[i,] = S_exp
				
		S[i,S[i,]<10] = 0 # drive pops below a threshold to extinction
		R[i,] = alpha[]*S[i,]*exp(-beta[]*S[i,]+phi*v[i-1,]+epi[i,])
		predR[i,] = alpha[]*S[i,]*exp(-beta[]*S[i,])
		v[i,] = log(R[i,])-log(predR[i,])
		v[i,is.nan(v[i,])]<-0
}
	 
	#Output
	S[S[,]=='NaN'] <- 0
	Ntot[Ntot[,]=='NaN'] <- 0
	pms <- matrix(NA,1,7) # performance measures: escapement, harvest, harvest rate, overfished, extinct, prop years failed to meet subsistence goal, CV in harvest
	over<- matrix(NA,length(alpha))
	ext<- matrix(NA,length(alpha))
	harvest_rate <- (H[20:ny,]/Ntot[20:ny,])[,1]
	
	for(j in 1:length(alpha)){
		over[j] <- SC.eq(mean(harvest_rate),alpha[j],beta[j])[3]
		ext[j] <- ifelse(median(S[(ny-20):ny,j]) < ((log(alpha)/beta)*0.05)[j],1,0) # less than 5% of unfished biomass/abundance
		}
	harvest <-rowSums(H[,])

	pms[,1] <- sum(S[20:ny,])/(ny-19)
	pms[,2] <- mean(harvest[(ny-20):ny]) 
	pms[,3] <- mean(harvest_rate)
	pms[,4] <- sum(over)/length(alpha)
	pms[,5] <- sum(ext)/length(alpha)
	pms[,6] <- sd(harvest[(ny-20):ny])/mean(harvest[(ny-20):ny]) 
	pms[,7] <- sd(harvest[(ny-20):ny])/mean(harvest[(ny-20):ny]) 
	
	list(S=S[20:ny,],N=Ntot[20:ny,],H=H[20:ny,],PMs=pms)
	}

#------------------------------------------------------------------------------#
# Function to generate matrix of alphas and betas for populations of equal size
#------------------------------------------------------------------------------#	
# pops <- # of populations (MUST BE EVEN)
# max.a <- maximum productivity
# min.a <- minimum productivity
# steps <- # of steps between high and low heterogeneity
# equ_spw <- equilibrium abundance (assumes all pops are the same size)
# var_pop <- maintain evenness in variation in productivity across populations? ("Yes" or "No")
# fixed_size <- Should total size remain the same regardless of richness? ("Yes" or "No") if yes then it is equal to equ_spw

hetero_prod_func = function(pops, max.a, min.a, steps, equ_spw,var_pop,fixed_size){
  if(fixed_size == "Yes"){equ_spw = equ_spw/pops}
  
  alpha.all <- array(NA,dim=c(steps,pops))
  alpha.all[1,] <- rep(mean(c(max.a, min.a),pops))
  alpha.all[steps,1] <- min.a
  alpha.all[steps, pops] <- max.a
  
  for(i in 2:pops){
    alpha.all[steps,i] <- alpha.all[steps,i-1] + (max.a-min.a)/(pops-2)
    for(ii in 2:(steps)){
      alpha.all[ii,i-1] <- alpha.all[ii-1,i-1] - (alpha.all[1,i-1] - alpha.all[steps,i-1])/(steps-2)
      alpha.all[ii, pops] <- alpha.all[ii-1, pops] - (alpha.all[1, pops] - alpha.all[steps, pops])/(steps-2)
    }
  }
  
  if(var_pop == "Yes"){
    alpha.all[,1:(pops/3)] <- alpha.all[,1]
    alpha.all[,((pops/3)+1):((pops/3)+pops/3)] <- alpha.all[,(pops/2)+0.5]
    alpha.all[,((pops)-(pops/3)+1):pops]  <- alpha.all[,pops]
  }	
  
  beta.all <- array(NA,dim=c(steps,pops))
  beta.all <- log(alpha.all)/equ_spw
  list(alphas = alpha.all,betas = beta.all)
}

#------------------------------------------------------------------------------#
# Function to generate matrix of alphas and betas for populations of unequal size
#------------------------------------------------------------------------------#	
# pops <- # of populations (MUST BE 10)
# max.a <- maximum productivity
# min.a <- minimum productivity
# steps <- # of steps between high and low heterogeneity
# equ_spw <- equilibrium abundance (assumes all pops are the same size)
# var_pop <- maintain evenness in variation in productivity across populations? ("Yes" or "No")
# fixed_size <- Should total size remain the same regardless of richness? ("Yes" or "No") if yes then it is equal to equ_spw
# evenness <- matrix of stock proportions (columns) by level of evenness (rows)
hetero_prod_func_even = function(pops, max.a, min.a, steps, equ_spw, var_pop, fixed_size, evenness){
  even <- matrix(NA,10,10)
  even[1,]<-c(0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.991)
  even[2,]<-c(0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.01,0.02,0.963)
  even[3,]<-c(0.001,0.001,0.001,0.001,0.001,0.001,0.014,0.03,0.03,0.92)
  even[4,]<-c(0.001,0.001,0.001,0.001,0.001,0.001,0.02,0.03,0.1,0.844)
  even[5,]<-c(0.001,0.001,0.001,0.015,0.02,0.02,0.03,0.05,0.05,0.812)
  even[6,]<-c(0.001,0.004,0.01,0.015,0.02,0.02,0.03,0.05,0.15,0.7)
  even[7,]<-c(0.001,0.01,0.015,0.02,0.03,0.04,0.05,0.15,0.2,0.484)
  even[8,]<-c(0.001,0.01,0.015,0.02,0.03,0.04,0.15,0.2,0.25,0.284)
  even[9,]<-c(0.01,0.01,0.01,0.05,0.1,0.1,0.12,0.2,0.2,0.2)
  even[10,]<-c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
  
  if(fixed_size == "Yes"){equ_spw = equ_spw/pops}
  if(fixed_size == "No"){equ_spw = equ_spw*even[,]}
  
  alpha.all <- array(NA,dim=c(steps,pops))
  alpha.all[1,] <- rep(mean(c(max.a, min.a),pops))
  alpha.all[steps,1] <- min.a
  alpha.all[steps, pops] <- max.a
  
  for(i in 2:pops){
    alpha.all[steps,i] <- alpha.all[steps,i-1] + (max.a-min.a)/(pops-2)
    for(ii in 2:(steps)){
      alpha.all[ii,i-1] <- alpha.all[ii-1,i-1] - (alpha.all[1,i-1] - alpha.all[steps,i-1])/(steps-2)
      alpha.all[ii, pops] <- alpha.all[ii-1, pops] - (alpha.all[1, pops] - alpha.all[steps, pops])/(steps-2)
    }
  }
  
  if(var_pop == "Yes"){
    alpha.all[,1:(pops/3)] <- alpha.all[,1]
    alpha.all[,((pops/3)+1):((pops/3)+pops/3)] <- alpha.all[,(pops/2)+0.5]
    alpha.all[,((pops)-(pops/3)+1):pops]  <- alpha.all[,pops]
  }	
  
  if(fixed_size == "No"){
    for(j in 1:(steps)){
      alpha.all[j,] <- alpha.all[8,]
    }
  }	
  
  beta.all <- array(NA,dim=c(steps,pops))
  beta.all <- log(alpha.all)/equ_spw
  list(alphas = alpha.all,betas = beta.all)
}

#------------------------------------------------------------------------------#
# Filled contour function by Ian Taylor, Carey McGilliard and Bridget Ferris
#------------------------------------------------------------------------------#
filled.contour3 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
{
  # modification by Ian Taylor of the filled.contour function
  # to remove the key and facilitate overplotting with contour()
  # further modified by Carey McGilliard and Bridget Ferris
  # to allow multiple plots on one page

  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
 # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
 # on.exit(par(par.orig))
 # w <- (3 + mar.orig[2]) * par("csi") * 2.54
 # par(las = las)
 # mar <- mar.orig
 plot.new()
 # par(mar=mar)
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
    stop("no proper 'z' matrix specified")
  if (!is.double(z)) 
    storage.mode(z) <- "double"
	.filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                            col = col)

  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  invisible()
}


filled.legend <-
  function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
    axes = TRUE, frame.plot = axes, ...) 
{
  # modification of filled.contour by Carey McGilliard and Bridget Ferris
  # designed to just plot the legend
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
  #  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  #  on.exit(par(par.orig))
  #  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    #layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
  #  par(las = las)
  #  mar <- mar.orig
  #  mar[4L] <- mar[2L]
  #  mar[2L] <- 1
  #  par(mar = mar)
   # plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
        yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    if (missing(key.axes)) {
        if (axes) 
            axis(4)
    }
    else key.axes
    box()
}

#------------------------------------------------------------------------------#
# Multi-panel risk vs. stability plot
#------------------------------------------------------------------------------#
risk_stab_plot <- function(long_sims, incl.legend, yaxis_plot, xaxis_plot, risk.tolerance, indexx, row){
  
  if(row == 1){ylim = 25}
  if(row == 2){ylim = 10}
  if(row == 3){ylim = 10}
  
  sims_out <- subset(long_sims, control == 0 & MSY == risk.tolerance)
  x <- as.numeric(paste(sims_out[,1]))
  y <- as.numeric(paste(sims_out[,2]))
  z <-as.numeric(sims_out[,3]*100)
  data.loess= loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  ext<-as.matrix(predict(data.loess, newdata = grid))
  x <- as.numeric(paste(sims_out[,1]))+0.00000001
  y <- as.numeric(paste(sims_out[,2]))+0.00000001
  z <-as.numeric(1/sims_out[,4])
  data.loess = loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  stability<-as.matrix(predict(data.loess, newdata = grid))
  ext[ext<0]<-0
  plot(stability[indexx,], ext[indexx,],col=colors[4],type="l",lwd=3,ylim=c(0, ylim),xlim=c(0.75,2.75),yaxt="n",xaxt="n",ylab="",xlab="")
  
  sims_out <- subset(long_sims, control == 0.12 & MSY == risk.tolerance)
  x <- as.numeric(paste(sims_out[,1]))
  y <- as.numeric(paste(sims_out[,2]))
  z <-as.numeric(sims_out[,3]*100)
  data.loess= loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  ext<-as.matrix(predict(data.loess, newdata = grid))
  x <- as.numeric(paste(sims_out[,1]))+0.00000001
  y <- as.numeric(paste(sims_out[,2]))+0.00000001
  z <-as.numeric(1/sims_out[,4])
  data.loess = loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  stability<-as.matrix(predict(data.loess, newdata = grid))
  ext[ext<0]<-0
  points(stability[indexx,], ext[indexx,],col=colors[3],type="l",lwd=3)
  
  sims_out <- subset(long_sims, control == 0.25 & MSY == risk.tolerance)
  x <- as.numeric(paste(sims_out[,1]))
  y <- as.numeric(paste(sims_out[,2]))
  z <-as.numeric(sims_out[,3]*100)
  data.loess= loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  ext<-as.matrix(predict(data.loess, newdata = grid))
  x <- as.numeric(paste(sims_out[,1]))+0.00000001
  y <- as.numeric(paste(sims_out[,2]))+0.00000001
  z <-as.numeric(1/sims_out[,4])
  data.loess = loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  stability<-as.matrix(predict(data.loess, newdata = grid))
  ext[ext<0]<-0
  points(stability[indexx,], ext[indexx,],col=colors[2],type="l",lwd=3)
  
  sims_out <- subset(long_sims, control == 0.5 & MSY == risk.tolerance)
  x <- as.numeric(paste(sims_out[,1]))
  y <- as.numeric(paste(sims_out[,2]))
  z <-as.numeric(sims_out[,3]*100)
  data.loess= loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  ext<-as.matrix(predict(data.loess, newdata = grid))
  x <- as.numeric(paste(sims_out[,1]))+0.00000001
  y <- as.numeric(paste(sims_out[,2]))+0.00000001
  z <-as.numeric(1/sims_out[,4])
  data.loess = loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  stability<-as.matrix(predict(data.loess, newdata = grid))
  ext[ext<0]<-0
  points(stability[indexx,], ext[indexx,],col=colors[1],type="l",lwd=3)
  
  if(yaxis_plot=="TRUE"){
    axis(2,las=2)  
  }else{
    axis(2,las=2,labels = F)  
  }  
  
  if(xaxis_plot=="TRUE"){
    axis(1)  
  }else{
    axis(1,labels = F)  
  }   
  
  if(incl.legend == "TRUE"){
    legend(1.5,9,c("0","0.125","0.25","0.5"),lwd=3,lty=1,col=c(colors[4],colors[3],colors[2],colors[1]),cex=0.9, bty="n")
    text(2,9,"Mgmt. control (C*)",cex=0.9)
  }
}

#------------------------------------------------------------------------------#
# Multi-panel yield vs. stability plot
#------------------------------------------------------------------------------#
yield_stab_plot <- function(long_sims, incl.legend, yaxis_plot, xaxis_plot, risk.tolerance, indexx, row){
  
  if(row == 1){ylim = 100}
  if(row == 2){ylim = 100}
  if(row == 3){ylim = 100}
  
  long_sims <- round(long_sims, digits = 2)
  long_sims$max.yld <- long_sims$harvest/max(long_sims$harvest)
  
  sims_out <- subset(long_sims, control == 0 & MSY == risk.tolerance)
  
  x <- as.numeric(paste(sims_out[,1]))
  y <- as.numeric(paste(sims_out[,2]))
  z <-as.numeric(sims_out[,8]*100)
  data.loess= loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  ext<-as.matrix(predict(data.loess, newdata = grid))
  x <- as.numeric(paste(sims_out[,1]))+0.00000001
  y <- as.numeric(paste(sims_out[,2]))+0.00000001
  z <-as.numeric(1/sims_out[,4])
  data.loess = loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  stability<-as.matrix(predict(data.loess, newdata = grid))
  ext[ext<0]<-0
  plot(stability[indexx,], ext[indexx,],col=colors[4],type="l",lwd=3,ylim=c(0, ylim),xlim=c(0.75,2.75),yaxt="n",xaxt="n",ylab="",xlab="")
  
  sims_out <- subset(long_sims, control == 0.12 & MSY == risk.tolerance)
  x <- as.numeric(paste(sims_out[,1]))
  y <- as.numeric(paste(sims_out[,2]))
  z <-as.numeric(sims_out[,8]*100)
  data.loess= loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  ext<-as.matrix(predict(data.loess, newdata = grid))
  x <- as.numeric(paste(sims_out[,1]))+0.00000001
  y <- as.numeric(paste(sims_out[,2]))+0.00000001
  z <-as.numeric(1/sims_out[,4])
  data.loess = loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  stability<-as.matrix(predict(data.loess, newdata = grid))
  ext[ext<0]<-0
  points(stability[indexx,], ext[indexx,],col=colors[3],type="l",lwd=3)
  
  sims_out <- subset(long_sims, control == 0.25 & MSY == risk.tolerance)
  x <- as.numeric(paste(sims_out[,1]))
  y <- as.numeric(paste(sims_out[,2]))
  z <-as.numeric(sims_out[,8]*100)
  data.loess= loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  ext<-as.matrix(predict(data.loess, newdata = grid))
  x <- as.numeric(paste(sims_out[,1]))+0.00000001
  y <- as.numeric(paste(sims_out[,2]))+0.00000001
  z <-as.numeric(1/sims_out[,4])
  data.loess = loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  stability<-as.matrix(predict(data.loess, newdata = grid))
  ext[ext<0]<-0
  points(stability[indexx,], ext[indexx,],col=colors[2],type="l",lwd=3)
  
  sims_out <- subset(long_sims, control == 0.5 & MSY == risk.tolerance)
  x <- as.numeric(paste(sims_out[,1]))
  y <- as.numeric(paste(sims_out[,2]))
  z <-as.numeric(sims_out[,8]*100)
  data.loess= loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  ext<-as.matrix(predict(data.loess, newdata = grid))
  x <- as.numeric(paste(sims_out[,1]))+0.00000001
  y <- as.numeric(paste(sims_out[,2]))+0.00000001
  z <-as.numeric(1/sims_out[,4])
  data.loess = loess(z~x*y)
  grid = expand.grid(list(x = seq(0,1,length.out=100), y = seq(min_cor,1,length.out=100)))
  stability<-as.matrix(predict(data.loess, newdata = grid))
  ext[ext<0]<-0
  points(stability[indexx,], ext[indexx,],col=colors[1],type="l",lwd=3)
  
  if(yaxis_plot=="TRUE"){
    axis(2,las=2)  
  }else{
    axis(2,las=2,labels = F)  
  }  
  
  if(xaxis_plot=="TRUE"){
    axis(1)  
  }else{
    axis(1,labels = F)  
  }   
  
  if(incl.legend == "TRUE"){
    legend(1.5,95,c("0","0.125","0.25","0.5"),lwd=3,lty=1,col=c(colors[4],colors[3],colors[2],colors[1]),cex=0.9, bty="n")
    text(2,95,"Mgmt. control (C*)",cex=0.9)
  }
}