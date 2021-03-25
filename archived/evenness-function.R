#------------------------------------------------------------------------------#
# New function to generate matrix of alphas and betas WITH EVENNESS
#------------------------------------------------------------------------------#	


hetero_prod_func_even = function(pops, max.a, min.a, steps, equ_spw, var_pop, fixed_size, evenness){
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


