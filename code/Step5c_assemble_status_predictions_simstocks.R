
# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Packages
library(plyr)
library(dplyr)

# Define directories
brtdir <- "~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/brt_models"
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"
preddir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions/simstocks"

# Read status predictions
ssp <- read.csv(paste(preddir, "simstocks_status_predictions_ssp.csv", sep="/"), as.is=T)
cmsy <- read.csv(paste(preddir, "simstocks_status_predictions_cmsy.csv", sep="/"), as.is=T)
zhou_brt <- read.csv(paste(preddir, "simstocks_status_predictions_zhou_brt.csv", sep="/"), as.is=T)
zhou_ocom <- read.csv(paste(preddir, "simstocks_status_predictions_zhou_ocom.csv", sep="/"), as.is=T)
dlm <- read.csv(paste(preddir, "simstocks_status_predictions_anderson.csv", sep="/"), as.is=T)

# Build data using SSP as base
data <- ssp

# BUILD DATA
################################################################################

# Step 1. Merge SSP and cMSY predictions
###################################################

# Format cMSY predictions
cmsy.format <- subset(cmsy, select=c(Stock, B_Bmsy))
colnames(cmsy.format) <- c("stockid", "cmsy_bbmsy")

# Format SSP predictions
data$ssp02_status <- revalue(as.factor(data$ssp02), 
                             c("undeveloped"="under", "developing"="under",
                               "fully exploited"="fully", 
                               "collapsed/closed"="over", "overfished"="over"))
data$ssp13_status <- revalue(as.factor(data$ssp13),
                             c("developing"="under",
                               "exploited"="fully",
                               "overexploited"="over", "collapsed"="over", "rebuilding"="over"))

# Merge SSP and cMSY predictions
data <- merge(data, cmsy.format, by="stockid", all=T)

# Add cMSY status
data$cmsy_status <- cut(data$cmsy_bbmsy, breaks=c(0,0.5,1.5,999), labels=c("over", "fully", "under"))


# Step 2. Add DLM (mPRM, SSCOM, COM-SIR) predictions
###################################################

# Format DLM
dlm <- subset(dlm, select=c(stockid, mprm_bbmsy, sscom_bbmsy, comsir_bbmsy))
dlm$mprm_status <- cut(dlm$mprm_bbmsy, breaks=c(0,0.5,1.5,999), labels=c("over", "fully", "under"))
dlm$sscom_status <- cut(dlm$sscom_bbmsy, breaks=c(0,0.5,1.5,999), labels=c("over", "fully", "under"))
dlm$comsir_status <- cut(dlm$comsir_bbmsy, breaks=c(0,0.5,1.5,999), labels=c("over", "fully", "under"))

# Merge data
data <- merge(data, dlm, by="stockid", all=T)

# Step 3. Add Zhou-BRT predictions
###################################################

# Merge data
colnames(zhou_brt) <- c("stockid", "zbrt8_s", "zbrt38_s", "zbrt_s")
data <- merge(data, zhou_brt, by="stockid", all=T)

# Transform S to B/BMSY
# S=B/K & BMSY=K/2 therefore S=B/(2*BMSY) & B/BMSY=S*2
data$zbrt_bbmsy <- data$zbrt_s * 2

# Format status columns
# A1=under, A2=fully, A3=over
data$zbrt_status <- cut(data$zbrt_s, breaks=c(0,0.25,0.75,999), labels=c("over", "fully", "under"))


# Step 4. Add Zhou-OCOM predictions
###################################################

# Merge data
zhou_ocom1 <- subset(zhou_ocom, percent==50, select=c(stockID, S))
zhou_ocom1$stockID <- zhou_brt$stockid
colnames(zhou_ocom1) <- c("stockid", "ocom_s")
data <- merge(data, zhou_ocom1, by="stockid", all=T)

# Transform S to B/BMSY
# S=B/K & BMSY=K/2 therefore S=B/(2*BMSY) & B/BMSY=S*2
data$ocom_bbmsy <- data$ocom_s * 2

# Format status columns
# A1=under, A2=fully, A3=over
data$ocom_status <- cut(data$ocom_s, breaks=c(0,0.25,0.75,999), labels=c("over", "fully", "under"))


# Step 5. Rearrange columns
###################################################

# Rearrange columns
colnames(data)
data <- subset(data, select=c("stockid", "stockid_orig", 
                              "lh", "id", "ed", "sigmaR", "ar", "sigmaC", "ts", "iter", 
                              "bbmsy", "bbmsy_status",
                              "ssp02", "ssp02_status", "ssp13", "ssp13_status", 
                              "cmsy_bbmsy", "cmsy_status", "mprm_bbmsy", "mprm_status",
                              "sscom_bbmsy", "sscom_status", "comsir_bbmsy", "comsir_status",
                              "zbrt8_s", "zbrt38_s", "zbrt_s", "zbrt_bbmsy", "zbrt_status", 
                              "ocom_s", "ocom_bbmsy", "ocom_status"))

# Rename columns
colnames(data) <- c("stockid", "stockid_orig", 
                    "lh", "id", "ed", "sigmaR", "ar", "sigmaC", "ts", "iter", 
                    "true_bbmsy", "true_status",
                    "ssp02", "ssp02_status", "ssp13", "ssp13_status", 
                    "cmsy_bbmsy", "cmsy_status", "mprm_bbmsy", "mprm_status",
                    "sscom_bbmsy", "sscom_status", "comsir_bbmsy", "comsir_status",
                    "zbrt8_s", "zbrt38_s", "zbrt_s", "zbrt_bbmsy", "zbrt_status", 
                    "ocom_s", "ocom_bbmsy", "ocom_status")


# EXPORT DATA
################################################################################

# Export data
write.csv(data, paste(preddir, "dlm_status_predictions_simstocks.csv", sep="/"), row.names=F)



