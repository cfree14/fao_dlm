

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
preds1 <- read.csv(paste(preddir1, "dlm_status_predictions.csv", sep="/"), as.is=T)
preds2 <- read.csv(paste(preddir2, "dlm_status_predictions_simstocks.csv", sep="/"), as.is=T)

# Reduce to observations with cMSY predictions
preds1 <- subset(preds1, !is.na(cmsy_rf))
preds2 <- subset(preds2, !is.na(cmsy_bbmsy))


# PLOT DATA
################################################################################

# Params
mline.lwd <- 0.6

# Setup figure
figname <- "Fig3_cmsy_performance.png"
png(paste(plotdir, figname, sep="/"), width=4, height=2, units="in", res=600)
par(mfrow=c(1,2), mar=c(2.5, 1, 0.5, 0.5), mgp=c(2,0.6,0), oma=c(0,1,0,0))

# RAMLDB stocks
#########################################################

# Plot RAMLDB predictions
# plot(cmsy_rf ~ bbmsy, preds1, bty="n", las=1, cex.axis=0.6,
#      xlim=c(0,5), ylim=c(0,5), cex=0.6,
#      xlab="", ylab="", col="grey60")
smoothScatter(preds1$bbmsy, preds1$cmsy_rf,  
              nrpoint=0, nbin=50,
              las=1, cex.axis=0.6, xlim=c(0,5), ylim=c(0,5), xlab="", ylab="")

# Calculate performance statistics
# Proportional error = (predicted - observed) / (|observed|)
# Bias: median proportional error
# Accuracy: median absolute proportional error
obs <- preds1$bbmsy
preds <- preds1$cmsy_rf
prop.error <- (preds-obs) / abs(obs)
abs.prop.error <- abs(prop.error)
bias <- median(prop.error)
accuracy <- median(abs.prop.error)
ranking <- cor(obs, preds, method="spearman")

# Add marker lines
lines(x=c(1,1), y=c(0,5), lwd=mline.lwd, lty=3)
lines(x=c(0,5), y=c(1,1), lwd=mline.lwd, lty=3)
lines(x=c(0,5), y=c(0,5), lwd=mline.lwd, lty=3)

# Add labels
text(labels="RAMLDB stocks", pos=4, x=-0.4, y=4.8, cex=0.5, font=2)

# Simulated stocks
#########################################################

# Plot simulated stock predictions
# plot(cmsy_bbmsy ~ bbmsy, preds2, bty="n", las=1, cex.axis=0.6,
#      xlim=c(0,5), ylim=c(0,5), cex=0.6, 
#      xlab="", ylab="", col="grey60")
smoothScatter(preds2$bbmsy, preds2$cmsy_bbmsy, 
              nrpoint=0, nbin=50,
              las=1, cex.axis=0.6, xlim=c(0,5), ylim=c(0,5), xlab="", ylab="")

# Add marker lines
lines(x=c(1,1), y=c(0,5), lwd=mline.lwd, lty=3)
lines(x=c(0,5), y=c(1,1), lwd=mline.lwd, lty=3)
lines(x=c(0,5), y=c(0,5), lwd=mline.lwd, lty=3)

# Calculate performance statistics
# Proportional error = (predicted - observed) / (|observed|)
# Bias: median proportional error
# Accuracy: median absolute proportional error
obs <- preds2$bbmsy
preds <- preds2$cmsy_bbmsy
prop.error <- (preds-obs) / abs(obs)
abs.prop.error <- abs(prop.error)
bias <- median(prop.error)
accuracy <- median(abs.prop.error)
ranking <- cor(obs, preds, method="spearman")

# Add labels
text(labels="Simulated stocks", pos=4, x=-0.4, y=4.8, cex=0.5, font=2)


# Labels
xlabel <- expression(paste("B/B"["MSY"], " observed"))
ylabel <- expression(paste("B/B"["MSY"], " predicted"))
mtext(xlabel, outer=T, side=1, line=-1, adj=0.5, cex=0.7)
mtext(ylabel, outer=T, side=2, line=0, adj=0.67, cex=0.7)


# Off
dev.off()
graphics.off()







