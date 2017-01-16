

# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Define directories
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"
preddir1 <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions"
preddir2 <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions/simstocks"

# Read prediction data
preds.ram <- read.csv(paste(preddir1, "dlm_status_predictions.csv", sep="/"), as.is=T)
preds.sim <- read.csv(paste(preddir2, "dlm_status_predictions_simstocks.csv", sep="/"), as.is=T)

# # Reduce to observations with cMSY predictions
# preds1 <- subset(preds1, !is.na(cmsy_rf))
# preds2 <- subset(preds2, !is.na(cmsy_bbmsy))


# HELPER FUNCTIONS
################################################################################

# Plot functions
# obs <- preds$true_bbmsy; ests <- preds$cmsy_bbmsy
plot_data <- function(obs, ests){
  
  # Plot point density
  smoothScatter(obs, ests,  
                nrpoint=0, nbin=50,
                las=0, cex.axis=0.7, tck=-0.02,
                xlim=c(0,5), ylim=c(0,5), xlab="", ylab="")
  
  # Calculate performance statistics
  # Proportional error = (predicted - observed) / (|observed|)
  # Bias: median proportional error
  # Accuracy: median absolute proportional error
  prop.error <- (ests-obs) / abs(obs)
  abs.prop.error <- abs(prop.error)
  bias <- median(prop.error, na.rm=T)
  accuracy <- median(abs.prop.error, na.rm=T)
  ranking <- cor(obs, ests, method="spearman", use="pairwise.complete.obs")
  
  # Print correlation
  rtext <- paste0("r=", format(round(ranking, 2), nsmall=2))
  mpe.text <- paste0("MPE=", format(round(bias, 2), nsmall=2))
  mape.text <- paste0("MAPE=", format(round(accuracy, 2), nsmall=2))
  stat.text <- paste(rtext, mpe.text, mape.text, sep="\n")
  text(labels=stat.text, pos=2, x=5.2, y=4.3, cex=0.75)
  
  # Add marker lines
  lines(x=c(1,1), y=c(0,5), lwd=mline.lwd, lty=3) # vertical
  lines(x=c(0,5), y=c(1,1), lwd=mline.lwd, lty=3) # horizontal
  lines(x=c(0,3.8), y=c(0,3.8), lwd=mline.lwd, lty=3) # diagonal
  
}

# PLOT DATA
################################################################################

# Params
mline.lwd <- 0.4

# Setup figure
figname <- "Fig3_cmsy_zhou_performance.png"
png(paste(plotdir, figname, sep="/"), width=5, height=3.5, units="in", res=600)
par(mfrow=c(2,3), mar=c(1, 1, 0.5, 0.5), mgp=c(2,0.2,0), oma=c(1.5,1.5,1,0))

# Loop through datasets and plot
datasets <- c("ram", "sim")
for(i in 1:length(datasets)){
  
  # Subset data
  dataset <- datasets[i]
  if(dataset=="ram"){
    preds <- subset(preds.ram, select=c(assessid, true_bbmsy, cmsy_rf_bbmsy, zbrt_bbmsy, ocom_bbmsy))
    colnames(preds) <- c("stockid", "true_bbmsy", "cmsy_bbmsy", "zbrt_bbmsy", "ocom_bbmsy")
  }else{
    preds <- subset(preds.sim, select=c(stockid, true_bbmsy, cmsy_bbmsy, zbrt_bbmsy, ocom_bbmsy))
  }

  # Plot cMSY performance
  plot_data(obs=preds$true_bbmsy, ests=preds$cmsy_bbmsy)
  stext <- ifelse(dataset=="ram", "RAMLDB stocks", "Simulated stocks")
  text(labels=stext, pos=4, x=-0.3, y=4.8, cex=0.9, font=2)
  
  # Plot Zhou-BRT performance
  plot_data(obs=preds$true_bbmsy, ests=preds$zbrt_bbmsy)
  
  # Plot Zhou-OCOM performance
  plot_data(obs=preds$true_bbmsy, ests=preds$ocom_bbmsy)
  
}

# Method labels
mtext("cMSY", outer=T, side=3, line=-0.3, adj=0.15, cex=0.7, font=2)
mtext("Zhou-BRT", outer=T, side=3, line=-0.3, adj=0.51, cex=0.7, font=2)
mtext("Zhou-OCOM", outer=T, side=3, line=-0.3, adj=0.90, cex=0.7, font=2)

# Axis labels
xlabel <- expression(paste("B/B"["MSY"], " observed"))
ylabel <- expression(paste("B/B"["MSY"], " predicted"))
mtext(xlabel, outer=T, side=1, line=0.5, adj=0.51, cex=0.7)
mtext(ylabel, outer=T, side=2, line=0.1, adj=0.5, cex=0.7)

# Off
dev.off()
graphics.off()







