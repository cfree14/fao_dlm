

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
preds <- read.csv(paste(preddir2, "dlm_status_predictions_simstocks.csv", sep="/"), as.is=T)

# # Reduce to observations with cMSY predictions
# preds1 <- subset(preds1, !is.na(cmsy_rf))
# preds2 <- subset(preds2, !is.na(cmsy_bbmsy))


# HELPER FUNCTIONS
################################################################################

# Plot functions
# zhou <- preds$ocom_bbmsy; cmsy <- preds$cmsy_bbmsy
plot_data <- function(cmsy, zhou){
  
  # Plot point density
  smoothScatter(zhou, cmsy,  
                nrpoint=0, nbin=50,
                las=0, cex.axis=0.7, tck=-0.02,
                xlim=c(0,2), ylim=c(0,2), xlab="", ylab="")
  
  # Calculate correlation
  corr <- cor(zhou, cmsy, method="spearman", use="pairwise.complete.obs")
  
  # Print correlation
  ctext <- paste0("r=", format(round(corr,3),nsmall=3))
  text(labels=ctext, pos=4, x=1.2, y=1.95, cex=0.75)
  
  # Add marker lines
  lines(x=c(1,1), y=c(0,2), lwd=mline.lwd, lty=3) # vertical
  lines(x=c(0,2), y=c(1,1), lwd=mline.lwd, lty=3) # horizontal
  lines(x=c(0,1.8), y=c(0,1.8), lwd=mline.lwd, lty=3) # diagonal
  
}

# PLOT DATA
################################################################################

# Params
mline.lwd <- 0.4

# Setup figure
figname <- "Fig5_cmsy_zhou_correlation_ed_scenarios.png"
png(paste(plotdir, figname, sep="/"), width=5, height=3.5, units="in", res=600)
par(mfcol=c(2,4), mar=c(2.5, 1, 0.5, 0.5), mgp=c(2,0.2,0), oma=c(0,1.5,1,0))

# Loop through datasets and plot
eds <- c("ED0", "ED0.6", "OW", "RC")
for(i in 1:length(eds)){
  
  # Subset data
  sdata <- subset(preds, ed==eds[i])

  # Plot cMSY ~ Zhou-BRT correlation
  plot_data(zhou=sdata$zbrt_bbmsy, cmsy=sdata$cmsy_bbmsy)

  # Add regression line
  lmfit <- lm(cmsy_bbmsy ~ zbrt_bbmsy, sdata)
  curve(coef(lmfit)[1]+coef(lmfit)[2]*x, from=0, to=2, lwd=1, add=T)
  
  # Plot cMSY ~ Zhou-OCOM correlation
  plot_data(zhou=sdata$ocom_bbmsy, cmsy=sdata$cmsy_bbmsy)
  
  # Add regression line
  lmfit <- lm(cmsy_bbmsy ~ ocom_bbmsy, sdata)
  curve(coef(lmfit)[1]+coef(lmfit)[2]*x, from=0, to=2, lwd=1, add=T)
  
}

# Axis labels
xlabel1 <- expression("Zhou-BRT B/B"["MSY"])
xlabel2 <- expression("Zhou-OCOM B/B"["MSY"])
ylabel <- expression("cMSY B/B"["MSY"])
mtext(xlabel1, outer=T, side=1, line=-13.7, adj=0.51, cex=0.7)
mtext(xlabel2, outer=T, side=1, line=-1, adj=0.51, cex=0.7)
mtext(ylabel, outer=T, side=2, line=0.1, adj=0.5, cex=0.7)

# Add exploitation dynamics stuff
line1 <- -0.2
cextext <- 0.55
mtext("Constant", outer=T, side=3, line=line1, adj=0.095, cex=cextext, font=2)
mtext("Biomass-coupled", outer=T, side=3, line=line1, adj=0.36, cex=cextext, font=2)
mtext("Increasing", outer=T, side=3, line=line1, adj=0.64, cex=cextext, font=2)
mtext("Roller coaster", outer=T, side=3, line=line1, adj=0.94, cex=cextext, font=2)

# Off
dev.off()
graphics.off()







