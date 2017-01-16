
# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Packages
library(dplyr)

# Define directories
ramdir <- "~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/ramldb/ramldb_20150202/formatted"
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"

# Read stock data
data <- read.csv(paste(datadir, "ramldb_stocks_for_fao_analysis.csv", sep="/"), as.is=T)

# Read RAMLDB assessment info
sainfo <- read.csv(paste(ramdir, "ramldb_assessment_data.csv", sep="/"), as.is=T)

# Read RAMLDB time series data
ts.orig <- read.csv(paste(ramdir, "ramldb_time_series_data.csv", sep="/"), as.is=T)


# PLOT CATCH TIME SERIES
################################################################################

# Rearrange data
data <- arrange(data, bbmsy)

# Stocks
stocks <- data$assessid

# Setup figure
figname <- "AppendixA_catch_time_series_ramldb.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(8,4), oma=c(4,6,4,4), mar=c(2.5, 0.5, 0.5, 1.0), mgp=c(2.5,0.7,0))

# Loop through stocks: i <- 1
for(i in 1:length(stocks)){
  
  # Subset data
  stock <- stocks[i]
  sdata <- subset(ts.orig, assessid==stock, 
                  select=c(assessid, year, tc, tl, b_bmsy_touse))
  print(paste(i, stock))
  
  # Format subsetted data
  # 1. Choose catch column and rename columns
  # 2. Eliminate years with no catch data (catch=NA)
  catch_type <- data$catch_ts[data$assessid==stock]
  sdata <- sdata[,c("year", catch_type, "b_bmsy_touse")]
  colnames(sdata) <- c("year", "catch", "bbmsy")
  sdata <- subset(sdata, !is.na(catch))
  
  # Extract B/BMSY
  bbmsy <- data$bbmsy[data$assessid==stock]
  if(bbmsy<0.5){color<-"red"}
  if(bbmsy>0.5 & bbmsy<1.5){color<-"orange"}
  if(bbmsy>1.5){color<-"darkgreen"}
  
  # Plot catch time series
  ymax <- max(sdata$catch/1000)*1.4
  plot(catch/1000 ~ year, sdata, type="l", bty="n", las=1, xaxt="n", yaxt="n",
       ylim=c(0, ymax), xlab="", ylab="", col=color, lwd=1.2)
  axis(1, at=c(min(sdata$year), max(sdata$year)), labels=T, cex.axis=0.85)
  axis(2, at=c(0, ymax), labels=c(0, round(ymax,1)), cex.axis=0.85)
  
  # Add stock ID
  stockid <- unique(ts.orig$stockid[ts.orig$assessid==stock])
  mtext(stockid, side=3, adj=0.05, line=-1, cex=0.5, font=2)
  
  # Add catch type
  ts_text <- paste(toupper(catch_type), "-", nrow(sdata), " yr", sep="")
  mtext(ts_text, side=3, adj=0.05, line=-1.8, cex=0.5)
  
  # Add axis labels
  # seq(1,nrow(data),32)
  if(i%in%c(1,33,65,97,129)){
    mtext("Year", outer=T, side=1, line=0, adj=0.5, cex=0.8, font=2)
    mtext("Catch (1000s mt)", outer=T, side=2, line=1.7, adj=0.5, cex=0.8, font=2)
  }
  if(i==161){
    mtext("Year", outer=T, side=1, line=-47, adj=0.5, cex=0.8, font=2)
    mtext("Catch (1000s mt)", outer=T, side=2, line=1.7, adj=0.87, cex=0.8, font=2)
  }
  
}



# Off
dev.off()
graphics.off()

