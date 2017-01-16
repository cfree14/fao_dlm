
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
preddir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions"

# Read status predictions
orcs <- read.csv(paste(brtdir, "status_predictions_from_brt_model.csv", sep="/"), as.is=T)
mprm <- read.csv(paste(preddir, "status_predictions_mprm.csv", sep="/"), as.is=T)
cmsy_sa <- read.csv(paste(preddir, "status_predictions_cmsy_anderson.csv", sep="/"), as.is=T)
cmsy_rf <- read.csv(paste(preddir, "status_predictions_cmsy_froese.csv", sep="/"), as.is=T)
ssp <- read.csv(paste(preddir, "status_predictions_ssp.csv", sep="/"), as.is=T)
zhou_brt <- read.csv(paste(preddir, "status_predictions_zhou_brt.csv", sep="/"), as.is=T)
zhou_ocom <- read.csv(paste(preddir, "status_predictions_zhou_ocom.csv", sep="/"), as.is=T)


# BUILD DATA
################################################################################

# Step 1. Merge mPRM and rORCS predictions
###################################################

# Merge data
data <- merge(mprm, orcs, by="assessid", all.x=T)

# Reduce/rename data
colnames(data)
data <- subset(data, select=c(assessid, agency1, country, region, area,
                              species, species_common, category, resilience,
                              catch_ts, bbmsy, bbmsy_status, bbmsy_status_pred, mprm))
colnames(data) <- c("assessid", "agency", "country", "region", "area",
                    "species", "species_common", "category", "resilience",
                    "catch_type", "true_bbmsy", "true_status", "orcs_status", "mprm_bbmsy")

# Format status columns
# A1=under, A2=fully, A3=over
data$true_status <- revalue(as.factor(data$true_status), c("A1"="under", "A2"="fully", "A3"="over"))
data$orcs_status <- revalue(as.factor(data$orcs_status), c("A1"="under", "A2"="fully", "A3"="over"))
data$mprm_status <- cut(data$mprm_bbmsy, breaks=c(0,0.5,1.5,999), labels=c("over", "fully", "under"))


# Step 2. Add SSP predictions
###################################################

# Merge data
ssp1 <- subset(ssp, select=c(assessid, ssp02, ssp13))
data <- merge(data, ssp1, by="assessid", all=T)

# Format status columns
# A1=under, A2=fully, A3=over
data$ssp02_status <- revalue(as.factor(data$ssp02), 
                             c("undeveloped"="under", "developing"="under",
                               "fully exploited"="fully", 
                               "collapsed/closed"="over", "overfished"="over"))
data$ssp13_status <- revalue(as.factor(data$ssp13),
                             c("developing"="under",
                               "exploited"="fully",
                               "overexploited"="over", "collapsed"="over", "rebuilding"="over"))


# Step 3. Add CMSY (Anderson) predictions
###################################################

# Merge data
cmsy_sa1 <- subset(cmsy_sa, select=c(assessid, cmsy))
colnames(cmsy_sa1) <- c("assessid", "cmsy_sa_bbmsy")
data <- merge(data, cmsy_sa1, by="assessid", all=T)

# Format status columns
# A1=under, A2=fully, A3=over
data$cmsy_sa_status <- cut(data$cmsy_sa_bbmsy, breaks=c(0,0.5,1.5,999), labels=c("over", "fully", "under"))


# Step 4. Add CMSY (Froese) predictions
###################################################

# Merge data
cmsy_rf1 <- subset(cmsy_rf, select=c(Stock, B_Bmsy))
colnames(cmsy_rf1) <- c("assessid", "cmsy_rf_bbmsy")
data <- merge(data, cmsy_rf1, by="assessid", all=T)

# Format status columns
# A1=under, A2=fully, A3=over
data$cmsy_rf_status <- cut(data$cmsy_rf_bbmsy, breaks=c(0,0.5,1.5,999), labels=c("over", "fully", "under"))

# Step 5. Add Zhou BRT predictions
###################################################

# Merge data
colnames(zhou_brt) <- c("assessid", "zbrt8_s", "zbrt38_s", "zbrt_s")
data <- merge(data, zhou_brt, by="assessid", all=T)

# Transform S to B/BMSY
# S=B/K & BMSY=K/2 therefore S=B/(2*BMSY) & B/BMSY=S*2
data$zbrt_bbmsy <- data$zbrt_s * 2

# Format status columns
# A1=under, A2=fully, A3=over
data$zbrt_status <- cut(data$zbrt_s, breaks=c(0,0.25,0.75,999), labels=c("over", "fully", "under"))


# Step 6. Add Zhou OCOM predictions
###################################################

# Merge data
zhou_ocom1 <- subset(zhou_ocom, percent==50, select=c(stockID, S))
zhou_ocom1$stockID <- zhou_brt$assessid
colnames(zhou_ocom1) <- c("assessid", "ocom_s")
data <- merge(data, zhou_ocom1, by="assessid", all=T)

# Transform S to B/BMSY
# S=B/K & BMSY=K/2 therefore S=B/(2*BMSY) & B/BMSY=S*2
data$ocom_bbmsy <- data$ocom_s * 2

# Format status columns
# A1=under, A2=fully, A3=over
data$ocom_status <- cut(data$ocom_s, breaks=c(0,0.25,0.75,999), labels=c("over", "fully", "under"))


# EXPORT DATA
################################################################################

# Export data
write.csv(data, paste(preddir, "dlm_status_predictions.csv", sep="/"), row.names=F)




