

# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Define directories
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"
preddir1 <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions"
preddir2 <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions/simstocks"
tabledir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/tables/csvs"

# Read prediction data
preds.ram <- read.csv(paste(preddir1, "dlm_status_predictions.csv", sep="/"), as.is=T)
preds.sim <- read.csv(paste(preddir2, "dlm_status_predictions_simstocks.csv", sep="/"), as.is=T)

# Reduce to predictions with Zhou-OCOM and cMSY
preds.ram <- subset(preds.ram, !is.na(ocom_bbmsy) & !is.na(cmsy_rf_bbmsy))
preds.sim <- subset(preds.sim, !is.na(ocom_bbmsy) & !is.na(cmsy_bbmsy))

# BUILD DATA
################################################################################

# Setup dataframe
results <- data.frame(scenario=c("RAMLDB stocks", "Simulated stocks", 
                                 "Constant", "Biomass-coupled", "Increasing", "Roller coaster"),
                      n=NA, zbrt=NA, ocom=NA)

# RAMLDB analysis
results$n[1] <- nrow(preds.ram)
results$zbrt[1] <- cor(preds.ram$cmsy_rf_bbmsy, preds.ram$zbrt_bbmsy, method="spearman")
results$ocom[1] <- cor(preds.ram$cmsy_rf_bbmsy, preds.ram$ocom_bbmsy, method="spearman")

# All simulated stocks
results$n[2] <- nrow(preds.sim)
results$zbrt[2] <- cor(preds.sim$cmsy_bbmsy, preds.sim$zbrt_bbmsy, method="spearman")
results$ocom[2] <- cor(preds.sim$cmsy_bbmsy, preds.sim$ocom_bbmsy, method="spearman")

# Loop through exploitation dynamics
eds <- c("ED0", "ED0.6", "OW", "RC")
for(i in 1:length(eds)){
  
  # Subset data
  sdata <- subset(preds.sim, ed==eds[i])
  
  # Record data
  results$n[i+2] <- nrow(sdata)
  results$zbrt[i+2] <- cor(sdata$cmsy_bbmsy, sdata$zbrt_bbmsy, method="spearman")
  results$ocom[i+2] <- cor(sdata$cmsy_bbmsy, sdata$ocom_bbmsy, method="spearman")
  
}

# EXPORT DATA
################################################################################

# Export data
write.csv(results, paste(tabledir, "Table12_cmsy_zhou_correlations.csv", sep="/"), row.names=F)

