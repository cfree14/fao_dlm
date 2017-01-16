
# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Packages
# library(dplyr)

# Define directories
brtdir <- "~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/brt_models"
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"

# Load BRT model data
# (to identify stocks in the test dataset)
load(paste(brtdir, "brt_catg_model_numeric.Rdata", sep="/"))
rm(data.test, data.train, key, best.model, tuned.models.bf5, tuned.models.bf6, tuned.models.bf7,
   tuned.models.bf8, tuned.models.bf9)
data.test <- arrange(data.test, bbmsy)

# Read RAMLDB assessment info
sainfo <- read.csv(paste(ramdir, "ramldb_assessment_data.csv", sep="/"), as.is=T)

# Read RAMLDB time series data
ts.orig <- read.csv(paste(ramdir, "ramldb_time_series_data.csv", sep="/"), as.is=T)

# Read resilience categories
res <- read.csv(paste(datadir, "RAM_bmsy_Ctousev4.csv", sep="/"), as.is=T)


# BUILD DATA
################################################################################

# Setup data
results <- data.frame(assessid=data.test$assessid, species=NA, category=NA, resilience=NA,
                      bbmsy=data.test$bbmsy)

# Loop through results
for(i in 1:nrow(results)){
  
  # Stock
  print(i)
  stock <- results$assessid[i]
  
  # Record scientific name
  spp <- sainfo$sci_name[sainfo$assessid==stock]
  results$species[i] <- spp
  
  # Record species category
  spp_catg <- as.character(unique(ram_prm_dat$species_cat[ram_prm_dat$scientificname==spp]))
  if(length(spp_catg)==0){spp_catg <- NA}
  results$category[i] <- spp_catg
  
  # Record species reslience
  spp_res <- unique(res$res[res$scientificname==spp])
  if(length(spp_res)==0){spp_res <- NA}
  results$resilience[i] <- spp_res
  
  
}

# Fill in missing species categories
sort(unique(ram_prm_dat$species_cat))
results[is.na(results$category),]
# results$category[results$species=="Loligo pealeii"] <- "Miscellaneous coastal fishes"
results$category[results$species=="Sardinella spp"] <- "Herrings, sardines, anchovies"

# Fill in missing species resiliences
sort(unique(res$res)) # High, Medium, Low, Very low
results[is.na(results$resilience),]
results[results$resilience=="",]
sardinella <- res[,grep("Sardinella", res$scientificname)]
# results$resilience[results$species=="Loligo pealeii"] <- "High" # seems to be based on online refs
results$resilience[results$species=="Sardinella spp"] <- "High" # S. aurita = high, S. maderensis = medium
results$resilience[results$species=="Placopecten magellanicus"] <- "Medium" # Atl. deep-sea scallop = moderate vulnerability (45/100)
results$resilience[results$species=="Chionoecetes bairdi"] <- NA # tanner crab
results$resilience[results$species=="Haliotis iris"] <- NA # blackfoot paua (abalone)

# Save results
results_base <- results
stocks <- results$assessid

# PLOT CATCH TIME SERIES
################################################################################

# Setup figure
figname <- "SFig4_test_stock_catch_time_series.png"
png(paste(plotdir, figname, sep="/"), width=8, height=6, units="in", res=600)
par(mfrow=c(6,6), oma=c(1.5,3,0.5,0), mar=c(2.5, 0.5, 0.5, 1.0), mgp=c(2.5,0.7,0))

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
  catch_type <- ifelse(sum(!is.na(sdata$tc)) > sum(!is.na(sdata$tl)), "tc", "tl")
  sdata <- sdata[,c("year", catch_type, "b_bmsy_touse")]
  colnames(sdata) <- c("year", "catch", "bbmsy")
  sdata <- subset(sdata, !is.na(catch))
  
  # Extract B/BMSY
  bbmsy <- results$bbmsy[results$assessid==stock]
  if(bbmsy<0.5){color<-"red"}
  if(bbmsy>0.5 & bbmsy<1.5){color<-"orange"}
  if(bbmsy>1.5){color<-"darkgreen"}
  
  # Plot catch time series
  ymax <- max(sdata$catch/1000)*1.2
  plot(catch/1000 ~ year, sdata, type="l", bty="n", las=1, xaxt="n", yaxt="n",
       ylim=c(0, ymax), xlab="", ylab="", col=color, lwd=1.2)
  axis(1, at=c(min(sdata$year), max(sdata$year)), labels=T, cex.axis=0.85)
  axis(2, at=c(0, ymax), labels=c(0, round(ymax,1)), cex.axis=0.85)
  
  # Add stock ID
  stockid <- unique(ts.orig$stockid[ts.orig$assessid==stock])
  mtext(stockid, side=1, adj=0.05, line=-1, cex=0.5, font=2)
  
  # Add catch type
  ts_text <- paste(toupper(catch_type), "-", nrow(sdata), " yr", sep="")
  mtext(ts_text, side=3, adj=0.05, line=-1, cex=0.5)
  
  
}

# Add lengend
# plot(1:10, 1:10, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
# legend("topleft", bty="n", col=c("red", "orange", "green"), lwd=1.5, cex=0.9,
#        title=expression(bold("Stock status")), 
#        legend=c("overexploited", "fully exploited", "underexploited"))

# Add axis labels
mtext("Year", outer=T, side=1, line=0, adj=0.5, cex=0.8)
mtext("Catch (1000s mt)", outer=T, side=2, line=1.7, adj=0.5, cex=0.8)

# Off
dev.off()
graphics.off()


# MPRM STATUS PREDICTIONS
################################################################################

# Create dataframe
results <- results_base
results$mprm <- NA

# Loop through stocks: i <- 1
for(i in 1:length(stocks)){
  
  # Subset data
  stock <- stocks[i]
  sdata <- subset(ts.orig, assessid==stock, 
                  select=c(assessid, year, tc, tl, b_bmsy_touse))
  print(paste(i, stock))
  
  # Use catch or landings?
  catch_type <- ifelse(sum(!is.na(sdata$tc)) > sum(!is.na(sdata$tl)), "tc", "tl")
  
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
  spp_catg <- as.character(results$category[i])
  prmdata <-   format_prm(year=sdata1$year, catch=sdata1$catch, bbmsy=sdata1$bbmsy, 
                          species_cat=spp_catg)
  mprm_stats <- predict_prm(prmdata, model=datalimited::ram_prm_model, ci=T)
  results$mprm[i] <- mprm_stats$bbmsy$bbmsy_q50[nrow(mprm_stats$bbmsy)]
  
}

# Export results
write.csv(results, paste(datadir, "test_stock_status_predictions_mprm.csv", sep="/"), row.names=F)


# CMSY STATUS PREDICTIONS
################################################################################

# Parameters
# 21 NEFSC-BUTTERGOMCHATT-1965-2012-HIVELY doesn't fit -- use "unknown" resilience
reps <- 2000000

# Create dataframe
results <- results_base
results$cmsy <- NA

# Loop through stocks: i <- 21
for(i in 1:length(stocks)){
  
  # Subset data
  stock <- stocks[i]
  sdata <- subset(ts.orig, assessid==stock, 
                  select=c(assessid, year, tc, tl, b_bmsy_touse))
  print(paste(i, stock))
  
  # Use catch or landings?
  catch_type <- ifelse(sum(!is.na(sdata$tc)) > sum(!is.na(sdata$tl)), "tc", "tl")
  
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
  spp_res <- results$resilience[results$assessid==stock]
  if(stock=="NEFSC-BUTTERGOMCHATT-1965-2012-HIVELY"){spp_res <- "unknown"}
  
  # Estimate status
  try({
    cmsy_stats <- cmsy(yr=sdata1$year, ct=sdata1$catch, start_r=resilience(spp_res), reps=reps)
    results$cmsy[i] <- cmsy_stats$bbmsy$bbmsy_q50[nrow(cmsy_stats$bbmsy)]
  })
  
}

# Export results
write.csv(results, paste(datadir, "test_stock_status_predictions_cmsy.csv", sep="/"), row.names=F)


# COM-SIR STATUS PREDICTIONS
################################################################################

# N posterior
# 500,000 successfully fits 23 of 29
# 1,000,000 successfully fits 23 of 29
nposterior <- 2000000
nburnin <- nposterior*0.1

# Create dataframe
results <- results_base
results$comsir <- NA

# Loop through stocks: i <- 1
for(i in 1:length(stocks)){
  
  # Subset data
  stock <- stocks[i]
  sdata <- subset(ts.orig, assessid==stock, 
                  select=c(assessid, year, tc, tl, b_bmsy_touse))
  print(paste(i, stock))
  
  # Use catch or landings?
  catch_type <- ifelse(sum(!is.na(sdata$tc)) > sum(!is.na(sdata$tl)), "tc", "tl")
  
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
  spp_res <- results$resilience[results$assessid==stock]
  if(stock=="NEFSC-BUTTERGOMCHATT-1965-2012-HIVELY"){spp_res <- "unknown"}
  
  # Estimate status
  try({
    comsir_stats <- comsir(yr=sdata1$year, ct=sdata1$catch, start_r=resilience(spp_res),
                           nsim=nburnin, n_posterior=nposterior)
    results$comsir[i] <- comsir_stats$bbmsy$bbmsy_q50[nrow(comsir_stats$bbmsy)]
  })
  
}

# Check % fit:
sum(!is.na(results$comsir))

# Export results
write.csv(results, paste(datadir, "test_stock_status_predictions_comsir.csv", sep="/"), row.names=F)


# SSCOM STATUS PREDICTIONS
################################################################################

# Create dataframe
results <- results_base
results$sscom <- NA

# Loop through stocks: i <- 1
for(i in 1:length(stocks)){
  
  # Subset data
  stock <- stocks[i]
  sdata <- subset(ts.orig, assessid==stock, 
                  select=c(assessid, year, tc, tl, b_bmsy_touse))
  print(paste(i, stock))
  
  # Use catch or landings?
  catch_type <- ifelse(sum(!is.na(sdata$tc)) > sum(!is.na(sdata$tl)), "tc", "tl")
  
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
  try({
    sscom_stats <- sscom(yr=sdata1$year, ct=sdata1$catch)
    results$sscom[i] <- sscom_stats$bbmsy$bbmsy_q50[nrow(sscom_stats$bbmsy)]
  })
  
}

# Export results
write.csv(results, paste(datadir, "test_stock_status_predictions_sscom.csv", sep="/"), row.names=F)


# SSP 2002 & 2012 STATUS PREDICTIONS
################################################################################

# SSP = FAO stock status plots
# SSP-2002 = Froese & Kesner-Reyes (2002)
# SSP-2012 = Kleisner et al. 2012 (same as Kleisner & Pauly 2011)

# Create dataframe
results <- results_base
results$ssp02 <- NA
results$ssp12 <- NA

# Loop through stocks: i <- 1
for(i in 1:length(stocks)){
  
  # Subset data
  stock <- stocks[i]
  sdata <- subset(ts.orig, assessid==stock, 
                  select=c(assessid, year, tc, tl, b_bmsy_touse))
  print(paste(i, stock))
  
  # Use catch or landings?
  catch_type <- ifelse(sum(!is.na(sdata$tc)) > sum(!is.na(sdata$tl)), "tc", "tl")
  
  # Format subsetted data
  # 1. Choose catch column and rename columns
  # 2. Eliminate years with no catch data (catch=NA)
  sdata1 <- sdata[,c("year", catch_type, "b_bmsy_touse")]
  colnames(sdata1) <- c("year", "catch", "bbmsy")
  sdata1 <- subset(sdata1, !is.na(catch))
  
  # Estimate and record status
  statuses <- calc_ssp_statuses(tc=sdata1$catch, yrs=sdata1$year, yr=max(sdata1$year))
  results$ssp02[i] <- statuses[1]
  results$ssp12[i] <- statuses[2]
  
}

# Export results
write.csv(results, paste(datadir, "test_stock_status_predictions_ssp.csv", sep="/"), row.names=F)




