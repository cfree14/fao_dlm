

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


# PLOT DATA
################################################################################

# Params
mline.lwd <- 0.6
datasets <- c("ram", "sim")

# Setup figure
figname <- "Fig4_zhou_performance_continuous.png"
png(paste(plotdir, figname, sep="/"), width=4, height=4, units="in", res=600)
par(mfrow=c(2,2), mar=c(1, 1, 0.5, 0.5), mgp=c(2,0.5,0), oma=c(1.8,1.8,0.5,0))

# Loop through datasets
for(i in 1:length(datasets)){
  
  # Setup data
  dataset <- datasets[i]
  if(dataset=="ram"){label <- "RAMLDB stocks"}else{label <- "Simulated stocks"}
  if(dataset=="ram"){preds <- preds.ram}else{preds <- preds.sim}
  
  # Plot Zhou BRT
  smoothScatter(preds$bbmsy, preds$zbrt_s,  
                nrpoint=0, nbin=50,
                las=0, cex.axis=0.6, xlim=c(0,5), ylim=c(0,1), xlab="", ylab="")
  mtext(label, side=3, adj=0.05, line=-1, cex=0.6, font=2)
  
  # Plot Zhou OCOM
  smoothScatter(preds$bbmsy, preds$ocom_s, 
                nrpoint=0, nbin=50,
                las=0, cex.axis=0.6, xlim=c(0,5), ylim=c(0,1), xlab="", ylab="")

}

# Titles
mtext("Zhou-BRT", outer=T, side=3, line=-0.5, adj=0.22, cex=0.7, font=2)
mtext("Zhou-OCOM", outer=T, side=3, line=-0.5, adj=0.82, cex=0.7, font=2)

# Axis labels
xlabel <- expression(paste("B/B"["MSY"], " observed"))
ylabel <- "Saturation"
mtext(xlabel, outer=T, side=1, line=0.55, adj=0.5, cex=0.7)
mtext(ylabel, outer=T, side=2, line=0.55, adj=0.5, cex=0.7)

# Off
dev.off()
graphics.off()







