

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

# # Reduce to observations with cMSY predictions
# preds1 <- subset(preds1, !is.na(cmsy_rf))
# preds2 <- subset(preds2, !is.na(cmsy_bbmsy))


# PLOT DATA
################################################################################

# Setup figure
figname <- "Fig6_zhou_ocom_bias.png"
png(paste(plotdir, figname, sep="/"), width=4, height=2, units="in", res=600)
par(mfcol=c(1,2), mar=c(2.7, 1, 0.5, 0.5), mgp=c(2,0.6,0), oma=c(0,1.5,0,0))

# Plot RAMLDB stock histogram
hist(preds1$ocom_bbmsy, breaks=seq(0,2,0.1), col="grey60", border=F, las=1, 
     xlab="", main="", xlim=c(0,2), ylim=c(0,25), cex.axis=0.6)
mtext("RAMLDB stocks", side=3, adj=0.05, line=-0.8, cex=0.6, font=2)

# Plot simulated stock histogram
hist(preds2$ocom_bbmsy, breaks=seq(0,2,0.1), col="grey60", border=F, las=1, 
     xlab="", main="", xlim=c(0,2), ylim=c(0,1000), cex.axis=0.6)
mtext("Simulated stocks", side=3, adj=0.05, line=-0.8, cex=0.6, font=2)

# Add x-axis label
mtext("Frequency", outer=T, side=2, line=0.5, adj=0.67, cex=0.75)
mtext(expression("Zhou-OCOM B/B"["MSY"]), outer=T, side=1, line=-1, adj=0.5, cex=0.75)


# Off
dev.off()
graphics.off()




