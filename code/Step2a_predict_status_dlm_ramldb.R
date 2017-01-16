

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
preddir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions"

# Read stock data
data_orig <- read.csv(paste(datadir, "ramldb_stocks_for_fao_analysis.csv", sep="/"), as.is=T)
data_orig <- subset(data_orig, select=c(assessid, agency1,
                                        country, region, area, 
                                        species, species_common,
                                        category, resilience, catch_ts))
stocks <- data_orig$assessid

# Read RAMLDB assessment info
sainfo <- read.csv(paste(ramdir, "ramldb_assessment_data.csv", sep="/"), as.is=T)

# Read RAMLDB time series data
ts.orig <- read.csv(paste(ramdir, "ramldb_time_series_data.csv", sep="/"), as.is=T)

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

# Loop through stocks: i <- 2
# #2 throws an error
for(i in 1:length(stocks)){
  
  # Subset data
  stock <- stocks[i]
  sdata <- subset(ts.orig, assessid==stock, 
                  select=c(assessid, year, tc, tl, b_bmsy_touse))
  print(paste(i, stock))
  
  # Use catch or landings?
  catch_type <- data$catch_ts[data$assessid==stock]
  
  # Format subsetted data
  # 1. Choose catch column and rename columns
  # 2. Eliminate years with no catch data (catch=NA)
  sdata1 <- sdata[,c("year", catch_type, "b_bmsy_touse")]
  colnames(sdata1) <- c("year", "catch", "bbmsy")
  sdata1 <- subset(sdata1, !is.na(catch))
  
  # Trim leading 0s, if any
  nonzeros <- sdata1$catch!=0
  firstnonzero <- min(which(nonzeros==T))
  sdata1 <- sdata1[firstnonzero:nrow(sdata1),]
  
  # Estimate and record status
  statuses <- calc_ssp_statuses(tc=sdata1$catch, yrs=sdata1$year, yr=max(sdata1$year))
  data$ssp02[i] <- statuses[1]
  data$ssp13[i] <- statuses[2]
  
}

# Export results
write.csv(data, paste(preddir, "status_predictions_ssp.csv", sep="/"), row.names=F)


# MPRM STATUS PREDICTIONS
################################################################################

# Create dataframe
data <- data_orig
data$mprm <- NA

# Loop through stocks: i <- 1
for(i in 1:length(stocks)){
  
  # Subset data
  stock <- stocks[i]
  sdata <- subset(ts.orig, assessid==stock, 
                  select=c(assessid, year, tc, tl, b_bmsy_touse))
  print(paste(i, stock))
  
  # Use catch or landings?
  catch_type <- data$catch_ts[data$assessid==stock]
  
  # Format subsetted data
  # 1. Choose catch column and rename columns
  # 2. Eliminate years with no catch data (catch=NA)
  sdata1 <- sdata[,c("year", catch_type, "b_bmsy_touse")]
  colnames(sdata1) <- c("year", "catch", "bbmsy")
  sdata1 <- subset(sdata1, !is.na(catch))
  
  # Trim leading 0s, if any
  nonzeros <- sdata1$catch!=0
  firstnonzero <- min(which(nonzeros==T))
  sdata1 <- sdata1[firstnonzero:nrow(sdata1),]
  
  # Estimate status
  spp_catg <- as.character(data$category[i])
  prmdata <-   format_prm(year=sdata1$year, catch=sdata1$catch, bbmsy=sdata1$bbmsy, 
                          species_cat=spp_catg)
  mprm_stats <- predict_prm(prmdata, model=datalimited::ram_prm_model, ci=T)
  data$mprm[i] <- mprm_stats$bbmsy$bbmsy_q50[nrow(mprm_stats$bbmsy)]
  
}

# Export results
write.csv(data, paste(preddir, "status_predictions_mprm.csv", sep="/"), row.names=F)


# CMSY STATUS PREDICTIONS
################################################################################

# Parameters
reps <- 2000000

# Create dataframe
data <- data_orig
data$cmsy <- NA

# Loop through stocks: i <- 21
for(i in 1:length(stocks)){
  
  # Subset data
  stock <- stocks[i]
  sdata <- subset(ts.orig, assessid==stock, 
                  select=c(assessid, year, tc, tl, b_bmsy_touse))
  print(paste(i, stock))
  
  # Use catch or landings?
  catch_type <- data$catch_ts[data$assessid==stock]
  
  # Format subsetted data
  # 1. Choose catch column and rename columns
  # 2. Eliminate years with no catch data (catch=NA)
  sdata1 <- sdata[,c("year", catch_type, "b_bmsy_touse")]
  colnames(sdata1) <- c("year", "catch", "bbmsy")
  sdata1 <- subset(sdata1, !is.na(catch))
  
  # Trim leading 0s, if any
  nonzeros <- sdata1$catch!=0
  firstnonzero <- min(which(nonzeros==T))
  sdata1 <- sdata1[firstnonzero:nrow(sdata1),]
  
  # Look up resilience
  spp_res <- data$resilience[data$assessid==stock]
  if(stock=="NEFSC-BUTTERGOMCHATT-1965-2012-HIVELY"){spp_res <- "unknown"}
  
  # Estimate status
  try({
    cmsy_stats <- cmsy(yr=sdata1$year, ct=sdata1$catch, start_r=resilience(spp_res), reps=reps)
    data$cmsy[i] <- cmsy_stats$bbmsy$bbmsy_q50[nrow(cmsy_stats$bbmsy)]
  })
  
}

# Export results
write.csv(data, paste(preddir, "status_predictions_cmsy_anderson.csv", sep="/"), row.names=F)






