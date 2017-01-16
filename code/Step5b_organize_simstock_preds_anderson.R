
# READ DATA
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(plyr)
library(reshape2)
library(dplyr)

# Define directories
simdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/simulated_stocks"
preddir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions/simstocks"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"

# Load data
load(paste(simdir, "anderson_etal_simstocks_output.Rdata", sep="/"))

# BUILD DATA
################################################################################

# Reduce data
preds1 <- subset(data, year==60, select=c(stockid, method_id, b_bmsy_true, b_bmsy_est))
colnames(preds1) <- c("stockid", "method", "bbmsy", "bbmsy_est")

# Change method names
preds1$method <- revalue(as.factor(preds1$method), 
                         c("Costello"="mprm", "COM.SIR"="comsir", "SSCOM"="sscom"))

# Reshape data (long-to-wide)
# When I try this it turns out there are duplicate runs for 320 stocks 
preds2 <- dcast(preds1, stockid+bbmsy ~ method, value.var="bbmsy_est")

# 320 stocks with duplicates (all SSCOM/COMSIR, no mprm)
preds2a <- subset(preds2, mprm>1 | comsir>1 | sscom>1)
sum(preds2a$mprm>1); sum(preds2a$comsir<2); sum(preds2a$sscom<2)
duplicate.stocks <- sort(unique(preds2a$stockid))

# Are the duplicates identical?
# Yes, they are identical. Just take the mean in the summary.
dup.check <- as.data.frame(
  preds1 %>%
    filter(stockid%in%duplicate.stocks & method%in%c("comsir", "sscom")) %>%
    group_by(stockid, method) %>%
    summarize(bbmsy_est_n=length(bbmsy_est),
              bbmsy_est_avg=mean(bbmsy_est),
              bbmsy_est_sd=sd(bbmsy_est))
)
sum(dup.check$sd>0)

# Reshape data (long-to-wide)
# Taking the mean because there are duplicates and the duplicates are identical
preds2 <- dcast(preds1, stockid+bbmsy ~ method, value.var="bbmsy_est", 
                fun.aggregate=mean, na.rm=T)

# Rename columns
colnames(preds2) <- c("stockid", "true_bbmsy", "comsir_bbmsy", "mprm_bbmsy", "sscom_bbmsy")

# EXPORT DATA
################################################################################

# Export data
write.csv(preds2, paste(preddir, "simstocks_status_predictions_anderson.csv", sep="/"), row.names=F)




