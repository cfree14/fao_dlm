
# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Sean Anderson's datalimited package
# https://github.com/datalimited/datalimited
# To run, I had to install XCode 8.1, Command Line Tools for XCode 8.1, and JAGS 4.2.0 for Mac
# You can upload the package from Github or from zip using the code below.
# devtools::install_github("hadley/devtools") # temporarily needed due to a bug
# devtools::install_github("datalimited/datalimited", force=T)
# install.packages(paste(datadir, "datalimited_0.0.2.tgz", sep="/"), repos=NULL)

# Packages
library(dplyr)
# library(fishmethods) # catchmsy
library(datalimited) # cmsy, comsir, sscom, mprm

# Define directories
brtdir <- "~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/brt_models"
ramdir <- "~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/ramldb/ramldb_20150202/formatted"
codedir <- "~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/other_dlm"
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"

# Source code for implementing SSP-2002 and SSP-2012 methods
source(paste(codedir, "calculate_fao_ssp_statuses.R", sep="/"))

# Load BRT model data
load(paste(brtdir, "brt_catg_model_numeric.Rdata", sep="/"))
rm(data.test, data.train, key, best.model, tuned.models.bf5, tuned.models.bf6, tuned.models.bf7,
   tuned.models.bf8, tuned.models.bf9)
data <- arrange(data, bbmsy)

# Read RAMLDB assessment info
sainfo <- read.csv(paste(ramdir, "ramldb_assessment_data.csv", sep="/"), as.is=T)

# Read RAMLDB time series data
ts.orig <- read.csv(paste(ramdir, "ramldb_time_series_data.csv", sep="/"), as.is=T)

# Read resilience categories
res <- read.csv("~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/other_dlm/data/RAM_bmsy_Ctousev4.csv", as.is=T)


# BUILD DATA
################################################################################

# Setup data
results <- data.frame(assessid=data$assessid, species=NA, category=NA, resilience=NA,
                      bbmsy=data$bbmsy, tcyrs=NA, tlyrs=NA)

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
  
  # Record catch/landings time series length
  sdata <- subset(ts.orig, assessid==stock)
  results$tcyrs[i] <- sum(!is.na(sdata$tc))
  results$tlyrs[i] <- sum(!is.na(sdata$tl))

}

# Fill in missing species categories
sort(unique(ram_prm_dat$species_cat))
results[is.na(results$category),]
results$category[results$species=="Lutjanus analis"] <- "Miscellaneous coastal fishes" # mutton snapper
results$category[results$species=="Epinephelus flavolimbatus"] <- "Miscellaneous coastal fishes" # yellowedge grouper
results$category[results$species=="Scorpaena guttata"] <- "Miscellaneous coastal fishes" # California scorpionfish
results$category[results$species=="Loligo pealeii"] <- "Miscellaneous coastal fishes" # longfin inshore squid
results$category[results$species=="Sardinella spp"] <- "Herrings, sardines, anchovies" # sardinella
results$category[results$species=="Sardinella aurita"] <- "Herrings, sardines, anchovies" # round sardinella
results[is.na(results$category),]

# Fill in missing species resiliences
sort(unique(res$res)) # High, Medium, Low, Very low
results[is.na(results$resilience),]
results[results$resilience=="",]
results[is.na(results$resilience),]
sardinella <- res[,grep("Sardinella", res$scientificname)]
# From FishBase
results$resilience[results$species=="Sebastes spp"] <- "Low" # Acadian redfish: S. fasciatus=low (4.5-14 yr), S. mentella=Very low (>14 yr)
results$resilience[results$species=="Lutjanus analis"] <- "Low" # mutton snapper: low (4.5-14 yr doubling time)
results$resilience[results$species=="Epinephelus flavolimbatus"] <- "Low" # yellowedge grouper: low (4.5-14 yr doubling time)
results$resilience[results$species=="Scorpaena guttata"] <- "Medium" # CA scorpionfish: medium (1.4-4.4 yr doubling time)
results$resilience[results$species=="Sardinella spp"] <- "High" # S. aurita = high (<15 month), S. maderensis = medium (1.4-4.4 yr)
results$resilience[results$species=="Sardinella aurita"]  <- "High" # S. aurita = high (<15 month doubling time)
# From SeaLifeBase
results$resilience[results$species=="Loligo pealeii"] <- "High" # longfin inshore squid: low-mod vulnerability (26/100) = high resilience
results$resilience[results$species=="Placopecten magellanicus"] <- "Medium" # Atl. deep-sea scallop: mod vulnerability (45/100) = mod resilience
results$resilience[results$species=="Homarus americanus"] <- "Medium" # American lobster: mod-high vulnerability (46/100) = mod resilience
results$resilience[results$species=="Paralithodes camtschaticus"] <- "High" # red king crab: low vulnerability (12/100) = high resilience
results$resilience[results$species=="Spisula solidissima"] <- "Medium" # Atlantic surf clam: mod vulnerability (37/100) = mod resilience
results$resilience[results$species=="Metanephrops challengeri"] <- NA # scampi
results$resilience[results$species=="Chionoecetes opilio"] <- NA # snow crab
results$resilience[results$species=="Chionoecetes bairdi"] <- NA # tanner crab
results$resilience[results$species=="Haliotis iris"] <- NA # blackfoot paua (abalone)
results[results$resilience=="",]
results[is.na(results$resilience),]

# Save results
results_base <- results
stocks <- results$assessid


# PLOT CATCH TIME SERIES
################################################################################

# Setup figure
figname <- "SFig1_catch_time_series.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8, height=6)
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




