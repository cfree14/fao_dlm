

# READ DATA
################################################################################

# Clear workspace
rm(list = ls())

# Define directories
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures/cmsy"
ramdir <- "~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/ramldb/ramldb_20150202/formatted"
preddir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions"

# Read CMSY data
data1 <- read.csv(paste(preddir, "status_predictions_cmsy_froese1.csv", sep="/"), as.is=T)
data2 <- read.csv(paste(preddir, "status_predictions_cmsy_froese2.csv", sep="/"), as.is=T)
data3 <- read.csv(paste(preddir, "status_predictions_cmsy_froese3.csv", sep="/"), as.is=T)

# Read stock info
info <- read.csv(paste(datadir, "ramldb_stocks_for_fao_analysis.csv", sep="/"), as.is=T)

# Read RAMLDB time series data
ts.orig <- read.csv(paste(ramdir, "ramldb_time_series_data.csv", sep="/"), as.is=T)


# INSPECT DATA
################################################################################

# Merge CMSY predictions
data <- rbind(data1, data2, data3)

# Which stocks are missing?
stocks <- info$assessid
stocks.cmsy <- data$Stock
stocks.missing <- stocks[!(stocks%in%stocks.cmsy)]

# Inspect time series of missing stocks
par(mfrow=c(2,2))
for(i in 1:length(stocks.missing)){
  
  # Subset stock data
  stock <- stocks.missing[i]
  sdata <- subset(ts.orig, assessid==stock, 
                  select=c(assessid, year, tc, tl, b_bmsy_touse))

  # Format subsetted data
  # 1. Choose catch column and rename columns
  # 2. Eliminate years with no catch data (catch=NA)
  catch_type <- info$catch_ts[info$assessid==stock]
  sdata <- sdata[,c("year", catch_type, "b_bmsy_touse")]
  colnames(sdata) <- c("year", "catch", "bbmsy")
  sdata <- subset(sdata, !is.na(catch))
  
  # Extract B/BMSY
  bbmsy <- info$bbmsy[info$assessid==stock]
  if(bbmsy<0.5){color<-"red"}
  if(bbmsy>0.5 & bbmsy<1.5){color<-"orange"}
  if(bbmsy>1.5){color<-"darkgreen"}
  
  # Plot catch time series
  ymax <- max(sdata$catch/1000)*1.2
  plot(catch/1000 ~ year, sdata, type="l", bty="n", las=1, xaxt="n", yaxt="n",
       ylim=c(0, ymax), xlab="", ylab="Catch (1000s mt)", col=color, lwd=1.2)
  axis(1, at=c(min(sdata$year), max(sdata$year)), labels=T, cex.axis=0.85)
  axis(2, at=c(0, ymax), labels=c(0, round(ymax,1)), cex.axis=0.85)
  
  # Add stock ID
  stockid <- unique(ts.orig$stockid[ts.orig$assessid==stock])
  mtext(stockid, side=1, adj=0.05, line=-1, cex=0.5, font=2)
  
  # Add catch type
  ts_text <- paste(toupper(catch_type), "-", nrow(sdata), " yr", sep="")
  mtext(ts_text, side=3, adj=0.05, line=-1, cex=0.5)
  
}

# EXPORT DATA
################################################################################

# Export data
write.csv(data, paste(preddir, "status_predictions_cmsy_froese.csv", sep="/"), row.names=F)







