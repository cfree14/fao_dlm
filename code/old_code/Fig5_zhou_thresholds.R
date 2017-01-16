

# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Packages
library(RColorBrewer)

# Define directories
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"
preddir1 <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions"
preddir2 <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions/simstocks"

# Read prediction data
preds1 <- read.csv(paste(preddir1, "dlm_status_predictions.csv", sep="/"), as.is=T)
preds2 <- read.csv(paste(preddir2, "dlm_status_predictions_simstocks.csv", sep="/"), as.is=T)

# RAMLDB (ram) or simulated (sim) stocks?
dataset <- "sim"

# READ DATA
################################################################################

# Colors
gnylrd5 <- rev(brewer.pal(5, "RdYlGn"))
gn <- gnylrd5[1]
yl <- gnylrd5[3]
rd <- gnylrd5[5]
colors <- c(gn, yl, rd)
  
# Params
breaks <- seq(0,1.5,0.05)
s_cols <- c("zbrt_s8", "zbrt_s38", "zbrt_s", "ocom_s")
titles <- c(expression("S"["8"]), expression("S"["38"]), expression("S"["avg"]),
            expression("S"["OCOM"]))

# Customize to dataset
if(dataset=="ram"){
  figname <- "Fig5_zhou_threshold_performance_ramldb.png"
  yaxis.bin <- 5
  preds <- preds1
}else{
  figname <- "Fig6_zhou_threshold_performance_simstocks.png"
  yaxis.bin <- 50
  preds <- preds2
  
}

# Setup figure
png(paste(plotdir, figname, sep="/"), width=6, height=3, units="in", res=600)
par(mfcol=c(3,4), mar=c(1,1.5,0.5,0.4), oma=c(2,2,1,0), mgp=c(3,0.7,0), xpd=T)

# Loop through saturation estimates
for(i in 1:length(s_cols)){
  
  # Subset data
  s.col <- s_cols[i]
  s.under <- preds[preds$bbmsy_status=="under", s.col]
  s.fully <- preds[preds$bbmsy_status=="fully", s.col]
  s.over <- preds[preds$bbmsy_status=="over", s.col]
  
  # Plot underexploited
  ylim <- ceiling(max(hist(s.under, breaks=breaks, plot=F)$count)/yaxis.bin)*yaxis.bin
  hist(s.under, breaks=breaks, col=gn, border=F, las=1, cex.axis=0.7,
       xlim=c(0,1), ylim=c(0, ylim), xaxt="n", xlab="", ylab="", main="", xpd=T)
  axis(1, at=seq(0,1,0.2), labels=F)
  sapply(c(0.25,0.75), function(x) lines(x=c(x,x), y=c(0,ylim), lty=3, lwd=1))
  
  # Plot fully exploited
  ylim <- ceiling(max(hist(s.fully, breaks=breaks, plot=F)$count)/yaxis.bin)*yaxis.bin
  hist(s.fully, breaks=breaks, col="orange", border=F, las=1, cex.axis=0.7,
       xlim=c(0,1), ylim=c(0, ylim), xaxt="n", xlab="", ylab="", main="")
  axis(1, at=seq(0,1,0.2), labels=F)
  sapply(c(0.25,0.75), function(x) lines(x=c(x,x), y=c(0,ylim), lty=3, lwd=1))
  
  # Plot overexploited
  ylim <- ceiling(max(hist(s.over, breaks=breaks, plot=F)$count)/yaxis.bin)*yaxis.bin
  hist(s.over, breaks=breaks, col=rd, border=F, las=1, cex.axis=0.7,
       xlim=c(0,1), ylim=c(0, ylim), xlab="", ylab="", main="")
  sapply(c(0.25,0.75), function(x) lines(x=c(x,x), y=c(0,ylim), lty=3, lwd=1))

}

# Add labels
mtext("Saturation", outer=T, side=1, adj=0.5, cex=0.7, line=0.8)
mtext("Frequency", outer=T, side=2, adj=0.52, cex=0.7, line=0.6)
mtext(titles[1], outer=T, side=3, adj=0.13, cex=0.7, line=-0.5, font=2)
mtext(titles[2], outer=T, side=3, adj=0.38, cex=0.7, line=-0.5, font=2)
mtext(titles[3], outer=T, side=3, adj=0.64, cex=0.7, line=-0.5, font=2)
mtext(titles[4], outer=T, side=3, adj=0.90, cex=0.7, line=-0.5, font=2)

# Off
dev.off()
graphics.off()








