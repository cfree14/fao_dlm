
# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Packages
library(plyr)
library(dplyr)

# Define directories
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/simulated_stocks"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"

# Read data
load(paste(datadir, "anderson_etal_simstocks_output.Rdata", sep="/"))
rm(data)
data <- catch


# PLOT CATCH TIME SERIES
################################################################################

# Add scenario column to key
key$scenario <- paste0(key$lh, "_", key$id*100, "%_", key$ed, "_", 
                       key$sigmaR, "sr_", key$ar, "ar_", key$sigmaC, "sc_", key$ts, "yr")

# Add stockid and scenario columsn to data
data$scenario <- paste0(data$lh, "_", data$id*100, "%_", data$ed, "_",
                        data$sigmaR, "sr_", data$ar, "ar_", data$sigmaC, "sc_", data$ts, "yr")

# Refactor key for sorting purposes
key$lh <- factor(key$lh, levels=c("DE", "SP", "LP"))

# Replace key exploitation dynamics for sorting purposes
# Constant (ED0), biomass-coupled (ED0.6), increasing (OW), roller coaster (RC)
# key$ed <- as.character(revalue(key$ed, c("ED0"="ED1", "ED0.6"="ED2", "OW"="ED3", "RC"="ED4")))

# Sort key
key <- arrange(key, lh, ts, id, sigmaR, ar, sigmaC, ed, iter)

# Unique scenarios
scenarios <- unique(key$scenario)

# Setup figure
figname <- "AppendixB_catch_time_series_simstocks.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(8,4), oma=c(4,6,4,4), mar=c(2.5, 0.5, 0.5, 1.0), mgp=c(2.5,0.7,0))

# Loop through stocks: i <- 1; j <- 1
# for(i in 1:(32*1)){
for(i in 1:length(scenarios)){
  
  # Identify stocks in scenario
  stocks <- unique(key$stockid[key$scenario==scenarios[i]])
  
  # Subset scenario data
  sdata <- subset(data, scenario==scenarios[i])
  
  # Set axis limits
  # ymax = maximum catch, xmax = time series length
  tc_max <- max(sdata$catch)
  ts_length <- unique(sdata$ts)
  ymax <- tc_max*1.4
  
  # Simulation traits
  lh <- unique(sdata$lh)
  lh1 <- ifelse(lh=="DE", "Demersal", ifelse(lh=="SP", "Small pelagic", "Large pelagic"))
  ed <- unique(sdata$ed)
  id <- paste0(unique(sdata$id)*100, "% K")
  ar <- paste0(unique(sdata$ar), " ar")
  sr <- paste0(unique(sdata$sigmaR), " sigmaR")
  sc <- paste0(unique(sdata$sigmaC), " sigmaC")
  text1 <- paste(lh1, id, sep=", ")
  text2 <- paste(ar, sr, sc, sep=", ")
  stext <- paste0(text1, "\n", text2)
 
  # Life history and color
  line.col <- ifelse(lh=="DE", "forestgreen", ifelse(lh=="SP", "dodgerblue3", "darkblue"))
  
  # Setup plot
  plot(1:10, 1:10, type="n", bty="n", cex.axis=0.85,
       xlim=c(0, ts_length), ylim=c(0,ymax), xlab="", ylab="", xaxt="n", yaxt="n")
  axis(1, at=c(1, ts_length), labels=T, cex.axis=0.85)
  axis(2, at=c(0, ymax), labels=c(0, format(round(ymax,1), nsmall=1)), cex.axis=0.85)
  text(labels=stext, x=0.5, y=ymax*0.88, pos=4, cex=0.8)
  
  # Plot each iteration
  for(j in 1:length(stocks)){
    sdata1 <- subset(sdata, stockid==stocks[j])
    years <- 1:ts_length
    catch <- sdata1$catch
    lines(years, catch, col=line.col, lwd=0.6)
  }
  
  # Add labels
  if(i%in%seq(1,5760,32)){
    
    # Axis titles
    mtext("Year", outer=T, side=1, line=0, adj=0.5, cex=0.8, font=2)
    mtext("Catch", outer=T, side=2, line=1.7, adj=0.5, cex=0.8, font=2)
    
    # Exploitation dynamics
    mtext("Constant", outer=T, side=3, line=0.5, adj=0.1, cex=0.7, font=2)
    mtext("Biomass-coupled", outer=T, side=3, line=0.5, adj=0.36, cex=0.7, font=2)
    mtext("Increasing", outer=T, side=3, line=0.5, adj=0.63, cex=0.7, font=2)
    mtext("Roller coaster", outer=T, side=3, line=0.5, adj=0.91, cex=0.7, font=2)
  
  }
  
}

# Off
dev.off()
graphics.off()

