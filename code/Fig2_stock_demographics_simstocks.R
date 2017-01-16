

# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Define directories
simdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/simulated_stocks"

# Export simulated stock data with code
load(paste(simdir, "anderson_etal_simstocks_output.Rdata", sep="/"))


# PLOT B/BMSY HISTOGRAM
################################################################################

# Setup figure
figname <- "Fig2_stock_demographics_simstocks.png"
png(paste(plotdir, figname, sep="/"), width=4, height=3, units="in", res=600)
par(mfrow=c(1, 1), mar=c(3, 3.5, 0.5, 0.5), mgp=c(1.9,0.6,0))

#  Plot histogram
colors <- c("grey20", rep("grey50", 2), rep("grey70", 9))
hist(key$bbmsy, breaks=seq(0,6,0.5), las=0,
     col=colors, border=F, cex.axis=0.8,
     xlab=expression("B/B"["MSY"]), main="")

# Add legend
nvals <- table(key$bbmsy_status)
legend.text <- c("Overexploited", "Fully exploited", "Underexploited")
legend.text <- paste(legend.text, " (n=", nvals, ")", sep="")
legend("topright", legend.text, fill=c("grey20", "grey50", "grey70"), bty="n", border=F, cex=0.75)

# Off
dev.off()
graphics.off()
