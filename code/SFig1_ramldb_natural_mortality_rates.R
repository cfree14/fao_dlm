

# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Define directories
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/zhou_input"
preddir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions"

# Read data
data <- read.csv(paste(datadir, "m_dataset_for_ramldb_stocks_for_zhou.csv", sep="/"), as.is=T)

# Read final prediction dataset
preds <- read.csv(paste(preddir, "dlm_status_predictions_finalset.csv", sep="/"), as.is=T)

# Reduce stock data to those used in final prediction comparison
data <- subset(data, assessid %in% preds$assessid)

# PLOT DATA
################################################################################

# Setup figure
figname <- "SFig1_ramldb_natural_mortality_rates.png"
png(paste(plotdir, figname, sep="/"), width=4, height=3, units="in", res=600)
par(mar=c(3,3,0.5,0.5),  mgp=c(1.8,0.6,0))

# Plot histogram
hist(data$m, breaks=seq(0,3,0.05), las=1, col="grey60", border=F, xlim=c(0,1.5),
     xlab="Natural mortality rate", main="", cex.axis=0.8, cex.lab=0.9)

# Add sample
table(data$m_source)
nfb <- 10+3
nram <- 150 + 5
ntext <- paste("From assessments: 155 values", "From FishBase and other refs: 13 values", sep="\n")
mtext(ntext, side=3, adj=0.95, line=-2, cex=0.7)

# Off
dev.off()
graphics.off()




