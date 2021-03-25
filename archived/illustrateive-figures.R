#### misc ------------------------

library(DirichletReg)
library(vegan)
library(tidyverse)
library(viridis)

#### Evenness plot -----------------
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

even_plot <- t(even)
evenesss <- matrix(NA,1,10)
for(i in 1:10){
  evenesss[i]<-round(diversity(even_plot[,i])/log(10),digits=2)
}

colnames(even_plot) <- evenesss
#even_plot$population <- seq(1,10)
#even_plot <- as.data.frame(even_plot)
#even_long <- pivot_longer(even_plot,cols=!population,names_to="eveness", values_to="proportion")

p<-barplot(t(even),col=rev(viridis(10)),
        yaxt="n",
        ylab="Proportion of aggregate",
        xlab="Pioleu's evenness")
axis(2,las=2)
axis(1,at=p, labels=evenesss)
box(col="grey")

#### Synchrony plots ----

source("functions.R")
source("evenness-function.R")
source("load.R")

# --- Set common conditions for simulations  -------------------------------------------
pop.num <-10
max.a <- 6 # maximum productivity
min.a <- 3 # minimum productivity
steps <- 10 # of steps between high and low heterogenity
equ_spw <- 400000 # equilibrium abundance (assumes all pops are the same size)
min_cor <- 0 # minimum value of degree of correlation (with 10 pops cannot go lower than ... )

rho.all <- array(NA,dim=c(steps,1))
rho.range <- seq(min_cor,0.99,length.out=steps)
for(ww in 1:length(rho.range)){
  rho.all[ww,] <- rho.range[ww]
}

ny = 50
phi <- 0.6
episd <- 0.6
Preturn <- c(0,0,1,0)
for.error <- 0.1
OU <- 0 

inputs <- hetero_prod_func_even(pop.num, max.a, min.a, steps, equ_spw,"Yes","No",even)
alpha<- inputs$alphas[1,]
beta<- inputs$betas[1,]
Ro <- log(alpha)/beta; sum(Ro)
rho <- rho.all[1,]
control <- 1 #1 is perfect and 0 is none
MSY.add <- 1
source("MSY_hcr_function.R")		

low_synh  <- process(ny, Ro, rho.all[1,], phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
med_synh  <- process(ny, Ro, rho.all[4,], phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
high_synh  <- process(ny, Ro, rho.all[7,], phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)
extreme_synh  <- process(ny, Ro, rho.all[10,], phi, Preturn, episd, alpha, beta, control, MSY.add, for.error)

par(mfrow=c(4,1),bty="o", mar=c(0.5,1,0.5,1),oma=c(4,4,1,1))#set dimensions to plots

pop_cols=rev(viridis(10))
plot(seq(6:51),
     low_synh$N[6:51,1]/1000,
     type="l",
     lwd=2,
     col=pop_cols[1],
     ylim=c(0,500),
     yaxt="n",
     xaxt="n",
     ylab="Run-size",
     xlab="Year")
axis(2,las=2)
box(col="grey")

for(i in 2:10){
  lines(seq(6:51),
        low_synh$N[6:51,i]/1000,
        type="l",
        lwd=2,
        col=pop_cols[i])
}

plot(seq(6:51),
     med_synh$N[6:51,1]/1000,
     type="l",
     lwd=2,
     col=pop_cols[1],
     ylim=c(0,500),
     yaxt="n",
     xaxt="n",
     ylab="Run-size",
     xlab="Year")
axis(2,las=2)
box(col="grey")

for(i in 2:10){
  lines(med_synh$N[6:51,i]/1000,
        type="l",
        lwd=2,
        col=pop_cols[i])
}

plot(seq(6:51),
     high_synh$N[6:51,1]/1000,
     type="l",
     lwd=2,
     col=pop_cols[1],
     ylim=c(0,500),
     yaxt="n",
     xaxt="n",
     ylab="Run-size",
     xlab="Year")
axis(2,las=2)
box(col="grey")

for(i in 2:10){
  lines(high_synh$N[6:51,i]/1000,
        type="l",
        lwd=2,
        col=pop_cols[i])
}

plot(seq(6:51),
     extreme_synh$N[6:51,1]/1000,
     type="l",
     lwd=2,
     col=pop_cols[1],
     ylim=c(0,500),
     yaxt="n",
     ylab="Run-size",
     xlab="Year")
axis(2,las=2)
box(col="grey")

for(i in 2:10){
  lines(extreme_synh$N[6:51,i]/1000,
        type="l",
        lwd=2,
        col=pop_cols[i])
}

mtext("Year",1, line=2, outer=TRUE)
mtext("Run-size (1000s)",2, line=2, outer=TRUE)


