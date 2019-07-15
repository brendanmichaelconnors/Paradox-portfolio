########################################################################################
# FigS3_yieldXstabMultiPanel.R
#
# Figure S3: Influence of management control, biodiversity and risk tolerance 
#   on yield - stability tradeoffs 
#
# Last updated: July 5, 2019
# Author: B. Connors (DFO)
#        
########################################################################################

# --- Load simulation output -----------------------------------------------------------
risk_sims <-readRDS(file = "output/mgmt_risk_sims.200.phi_0.8.Rho_0.6.logFE_0.1.OU_0.12July2019")
  long_sims <- round(risk_sims , digits = 2)
  min_cor = 0
  colors <- viridis(4)

# --- Generate figure ----------------------------------------------------------------
  
jpeg("figures/fig_S3.phi_0.8.Rho_0.6.logFE_0.1.OU_0.12July2019.jpeg",width=8, height=7, units="in",res=800)
#dev.new(width=8, height=10,new=FALSE)

# Set up plotting region

m <- rbind(
  cbind(0.05,0.35,0.7,1),
  cbind(0.35,0.65,0.7,1),
  cbind(0.65,0.95,0.7,1),
  
  cbind(0.05,0.35,0.4,0.7),
  cbind(0.35,0.65,0.4,0.7),
  cbind(0.65,0.95,0.4,0.7),
  
  cbind(0.05,0.35,0.1,0.4),
  cbind(0.35,0.65,0.1,0.4),
  cbind(0.65,0.95,0.1,0.4),
  
  cbind(0,1,0,1)
)

split.screen(m)

# ROW 1: Yield

screen(1)
  par(mar = c(1,1.5,1,1))
  yield_stab_plot(long_sims, "FALSE", "TRUE", "FALSE", 1, 20, 1)
  mtext("Var. in productivity = low",3,line=0, cex=0.9)
  
screen(2)
  par(mar = c(1,1.5,1,1))
  yield_stab_plot(long_sims, "FALSE", "FALSE", "FALSE", 1, 50, 1)
  mtext("Var. in productivity = medium",3,line=0, cex=0.9)
  
screen(3)
  par(mar = c(1,1.5,1,1))
  yield_stab_plot(long_sims, "FALSE", "FALSE", "FALSE", 1, 90, 1)
  mtext("Var. in productivity = high",3,line=0, cex=0.9)
  mtext("OF tolerance = high",4,line=0.5)
  
screen(4)
  par(mar = c(1,1.5,1,1))
  yield_stab_plot(long_sims, "FALSE", "TRUE", "FALSE", 0.75, 20, 2)
  mtext("Yield (% of maximum)",2,line=2.5)
  
screen(5)
  par(mar = c(1,1.5,1,1))
  yield_stab_plot(long_sims, "FALSE", "FALSE", "FALSE", 0.75, 50, 2)
  
screen(6)
  par(mar = c(1,1.5,1,1))
  yield_stab_plot(long_sims, "FALSE", "FALSE", "FALSE", 0.75, 90, 2)
  mtext("OF tolerance = medium",4,line=0.5)
  
screen(7)
  par(mar = c(1,1.5,1,1))
  yield_stab_plot(long_sims, "TRUE", "TRUE", "TRUE", 0.5, 20, 3)
  
screen(8)
  par(mar = c(1,1.5,1,1))
  yield_stab_plot(long_sims, "FALSE", "FALSE", "TRUE", 0.5, 50, 3)
  mtext("Catch stability (1/CV)",1,line=2.5)
  
screen(9)
  par(mar = c(1,1.5,1,1))
  yield_stab_plot(long_sims, "FALSE", "FALSE", "TRUE", 0.5, 90, 3)
  mtext("OF tolerance = low",4,line=0.5)
  
close.screen(all.screens = TRUE)

dev.off()
