

# READ DATA
################################################################################

# Clear workspace
rm(list = ls())

# Define directories
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"
preddir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions/simstocks"

# Read key
key <- read.csv(paste(datadir, "simulated_stocks_for_fao_analysis.csv", sep="/"), as.is=T)

# Read CMSY data
# I attempted to run this in four chunks but there was a crash during the final chunk
# Chunk 1: SC=0.0, AR=0.0 (simstock_cmsy_pred_alien_0ar_0sc.csv)
# Chunk 2: SC=0.0, AR=0.8 (simstock_cmsy_pred_alien_0ar_0sc.csv)
# Chunk 3: SC=0.2, AR=0.0 (simstock_cmsy_pred_alien_0ar_0.2sc.csv)
# Chunk 4: SC=0.2, AR=0.8 (simstock_cmsy_pred_alien_0.8ar_0.2sc.csv)
chunk1 <- read.csv(paste(preddir, "simstock_cmsy_pred_alien_0ar_0sc.csv", sep="/"), as.is=T)
chunk2 <- read.csv(paste(preddir, "simstock_cmsy_pred_alien_0.8ar_0sc.csv", sep="/"), as.is=T)
chunk3 <- read.csv(paste(preddir, "simstock_cmsy_pred_alien_0ar_0.2sc.csv", sep="/"), as.is=T) # 1 stock missing
# chunk3b <- read.csv(paste(preddir, "simstock_cmsy_pred_alien_0ar_0.2sc_v2.csv", sep="/"), as.is=T) # the missing stock (if I get it)
chunk4a <- read.csv(paste(preddir, "simstock_cmsy_pred_alien_0.8ar_0.2sc.csv", sep="/"), as.is=T) # Alien before crash
chunk4b <- read.csv(paste(preddir, "simstock_cmsy_pred_alien_0.8ar_0.2sc_v2.csv", sep="/"), as.is=T) # Alien after crash
chunk4c <- read.csv(paste(preddir, "simstock_cmsy_pred_chris_0.8ar_0.2sc.csv", sep="/"), as.is=T) # Chris computer, going backwards


# MERGE DATA
################################################################################

# Merge Chunk 3 files
##############################################

# Merge Chunk 3 files
# (only needed if it gets fit)
# chunk3 <- rbind(chunk3a, chunk3b)
# chunk3 <- arrange(chunk3, Stock)
# length(unique(chunk3$Stock))

# Merge Chunk 4 files
##############################################

# Add column to record computer-attempt
chunk4a$try <- "alien1"
chunk4b$try <- "alien2"
chunk4c$try <- "chris"

# Check to make sure there is no overlap in Alien 1 and Alien 2
# Checked. There is no overlap. Go ahead and merge these two chunks.
sum(chunk4a$Stock %in% chunk4b$Stock)
sum(chunk4b$Stock %in% chunk4a$Stock)
chunk4alien <- rbind(chunk4a, chunk4b)

# However, there should be overlap in the Alien 2 and Chris chunks
# Use the Alien fits. Eliminate Chris fits occuring in Alien dataset.
sum(chunk4c$Stock %in% chunk4alien$Stock) # 13 overlap
chunk4chris.noverlap <- chunk4c[!(chunk4c$Stock%in%chunk4alien$Stock),]

# Merge Chunk 4 attempts, remove try column, sort, and check
chunk4 <- rbind(chunk4alien, chunk4chris.noverlap)
chunk4 <- arrange(subset(chunk4, select=-try), Stock)
length(unique(chunk4$Stock))

# Perform final merge
##############################################

# Final merge
data <- rbind(chunk1, chunk2, chunk3, chunk4)
data <- arrange(data, Stock)

# Any missing?
key$stockid[!(key$stockid%in%data$Stock)]


# EXPORT DATA
################################################################################

# Export data
write.csv(data, paste(preddir, "simstocks_status_predictions_cmsy.csv", sep="/"), row.names=F)

