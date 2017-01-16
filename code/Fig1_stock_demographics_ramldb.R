

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# READ DATA
################################################################################

# Packages
library(dplyr)
library(RColorBrewer)

# Define directories
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"
preddir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions"

# Read stock data
data <- read.csv(paste(datadir, "ramldb_stocks_for_fao_analysis.csv", sep="/"), as.is=T)

# Read final prediction dataset
preds <- read.csv(paste(preddir, "dlm_status_predictions_finalset.csv", sep="/"), as.is=T)

# Reduce stock data to those used in final prediction comparison
data <- subset(data, assessid %in% preds$assessid)


# PLOT DATA
################################################################################

# Colors
display.brewer.pal(5, "RdYlBu")
gnylrd5 <- rev(brewer.pal(5, "RdYlBu"))
gn <- gnylrd5[1]
yl <- gnylrd5[3]
rd <- gnylrd5[5]

# Main color (grey70 or yl)
main.color <- "grey70"

# Setup figure
figname <- "Fig1_stock_demographics_ramldb.png"
png(paste(plotdir, figname, sep="/"), width=6, height=4, units="in", res=600)
par(mfrow=c(2, 3), mar=c(4.5, 3.5, 0.5, 0.5), mgp=c(2.5,0.7,0), oma=c(1,1,0,0))

# A. Fishery type
#########################################

# Count fishery types
nfisheries <- data.frame(table(data$fishery_type))
colnames(nfisheries) <- c("fishery", "count")
nfisheries <- arrange(nfisheries, desc(count))

# Plot barplot: yl or grey70
barplot(nfisheries$count, las=2, ylim=c(0,40), space=0, col=main.color, border=F,
        names=nfisheries$fishery, cex.names=0.7, cex.axis=0.8)
mtext("A", side=3, adj=-0.25, line=-1, font=2, cex=0.7)

# Add sample size text
nstocks <- nrow(data)
nspecies <- length(unique(data$species))
nfamilies <- length(unique(data$family))
mtext(paste(nstocks, "stocks"), side=3, adj=0.95, cex=0.5, line=-1)
mtext(paste(nspecies, "species"), side=3, adj=0.95, cex=0.5, line=-1.8)
mtext(paste(nfamilies, "families"), side=3, adj=0.95, cex=0.5, line=-2.6)

# B. Country
#########################################

# Count countries
ncountries <- data.frame(table(data$country))
ncountries <- arrange(rename(ncountries, country=Var1, count=Freq), desc(count))

# Propotion US stocsk
ncountries$count[ncountries$country=="United States"] / sum(ncountries$count)

# Plot barplot
barplot(ncountries$count, las=2, ylim=c(0,100), space=0, col=main.color, border=F,
        names=ncountries$country, cex.names=0.8, cex.axis=0.8)
mtext("B", side=3, adj=-0.25, line=-1, font=2, cex=0.7)

# C. U.S. agency
#########################################

# Count agencies
nagencies <- data.frame(table(data$agency2[data$country=="United States"]))
nagencies <- arrange(rename(nagencies, agency=Var1, count=Freq), desc(count))

# Plot barplot
barplot(nagencies$count, las=2, ylim=c(0,30), space=0, col=main.color, border=F, 
        names=nagencies$agency, cex.names=0.8, cex.axis=0.8)
mtext("C", side=3, adj=-0.25, line=-1, font=2, cex=0.7)

# D. Assessment year
#########################################

# Reset plotting parameters
par(mar=c(4, 3.5, 0.5, 0.5))

# Plot histogram
hist(data$year, breaks=seq(2000,2015,1), las=1, 
     ylim=c(0, 50), col=main.color, border=F, cex.axis=0.8,
     main="", xlab="Assessment year", ylab="")
mtext("D", side=3, adj=-0.25, line=-1, font=2, cex=0.7)

# E. Stock status
#########################################

# Plot histogram
# colors <- c(rev(brewer.pal(5, "RdYlGn"))[c(1,3,5)])
# gre <- colors[1]
# yel <- colors[2]
# red <- colors[3]
# colors1 <- c(red, "orange", "orange", rep(gre, 11))
colors1 <- c("grey20", rep("grey50", 2), rep("grey70", 30))
hist(data$bbmsy, breaks=seq(0,15,0.5), las=1, xlim=c(0,7),
     ylim=c(0, 50), col=colors1, border=F, cex.axis=0.8,
     main="", xlab=expression("B/B"["MSY"]), ylab="", freq=T)
mtext("E", side=3, adj=-0.25, line=-1, font=2, cex=0.7)

# Add legend
nvals <- rev(table(data$bbmsy_status))
legend.text <- c("Overexploited", "Fully exploited", "Underexploited")
legend.text <- paste(legend.text, " (n=", nvals, ")", sep="")
legend("topright", legend.text, fill=c("grey20", "grey50", "grey70"), bty="n",
       border=F, cex=0.75)

# F. Landings
#########################################

# Add catch (mt) category
breaks <- c(0,1,2,5,10,20,50,100,200,500,1000,2000)
xlabels <- c("<1", "1-2", "2-5", "5-10", "10-20", "20-50", "50-100", "100-200",
             "200-500", "500-1000", ">1000")
data$catch_catg <- cut(data$catch_tmt, breaks=breaks)
barplot(table(data$catch_catg), las=2, space=0, col="grey70", border=F, cex.axis=0.8,
        ylim=c(0,60), xlab="", names=xlabels, cex.names=0.8)
mtext("F", side=3, adj=-0.25, line=-1, font=2, cex=0.7)

# Add x-axis label
mtext("Catch (1000s mt)", outer=T, side=1, adj=0.93, line=-0.5, cex=0.65)

# Final touches
#########################################

# Add y-axis label
mtext("Frequency", outer=T, side=2, adj=0.56, line=-0.5, cex=0.75)


# Off
dev.off()
graphics.off()

