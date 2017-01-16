

# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Packages
library(dplyr)

# Define directories
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"
cmsydir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/cmsy_input"
preddir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions/simstocks"

# Read simulated stock key
data_orig <- read.csv(paste(datadir, "simulated_stocks_for_fao_analysis.csv", sep="/"), as.is=T)
data_orig <- subset(data_orig, select=-c(mprm, comsir, sscom))
stocks <- data_orig$stockid

# Read simulated stock catch time series
ts <- read.csv(paste(cmsydir, "simulated_stock_catch_time_series_for_cmy_froese.csv", sep="/"), as.is=T)
ts <- subset(ts, select=c(Stock, yr, ct))
colnames(ts) <- c("stockid", "year", "catch")                                   

# Read SSP-2002 and SSP-2012 prediction code
source("~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/other_dlm/calculate_fao_ssp_statuses.R")


# SSP 2002 & 2012 STATUS PREDICTIONS
################################################################################

# SSP = FAO stock status plots
# SSP-2002 = Froese & Kesner-Reyes (2002)
# SSP-2013 = Kleisner et al. 2013 (same as Kleisner & Pauly 2011)

# Create dataframe
data <- data_orig
data$ssp02 <- NA
data$ssp13 <- NA

# Loop through stocks: i <- 1
for(i in 1:length(stocks)){
  
  # Subset data
  stock <- stocks[i]
  sdata <- subset(ts, stockid==stock)
  print(paste(i, stock))
  
  # Estimate and record status
  statuses <- calc_ssp_statuses(tc=sdata$catch, yrs=sdata$year, yr=max(sdata$year))
  data$ssp02[i] <- statuses[1]
  data$ssp13[i] <- statuses[2]
  
}

# Export results
write.csv(data, paste(preddir, "simstocks_status_predictions_ssp.csv", sep="/"), row.names=F)






