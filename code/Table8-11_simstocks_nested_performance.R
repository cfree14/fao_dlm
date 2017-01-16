

# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Packages
library(caret)

# Define directories
preddir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions/simstocks"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"
tabledir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/tables/csvs"

# Read status predictions
data_orig <- read.csv(paste(preddir, "dlm_status_predictions_simstocks.csv", sep="/"), as.is=T)

# ANALYZE DATA
################################################################################

# Method statuses
status_cols <- c("ssp02_status", "ssp13_status", "cmsy_status",
                 # "mprm_status", "sscom_status", "comsir_status",
                 "zbrt_status", "ocom_status")

# Method abbreviations
methods <- c("SSP-2002", "SSP-2013", "cMSY", 
             # "mPRM", "SSCOM", "COM-SIR",
             "Zhou-BRT", "Zhou-OCOM")

# Reduce to stocks with all predictions
data <- data_orig[sapply(1:nrow(data_orig), function(x) sum(is.na(data_orig[x, status_cols]))==0),]

# Comparisons
# Life history (LH), time series lengths (TS)
# Intitial depletion (ID), exploitation dynamics (ED)
lhs <- c("DE", "SP", "LP") # sort(unique(data$lh))
lh.names <- c("Demersal fish", "Small pelagic fish", "Large pelagic fish")
ids <- sort(unique(data$id), decreasing=T)
id.names <- c("100% of carrying capacity", "70% of carrying capacity", "40% of carrying capacity")
eds <- sort(unique(data$ed))
ed.names <- c("Constant F", "Biomass-coupled F",
              "Increasing F", "Roller coaster F")
tls <- sort(unique(data$ts))
tl.names <- c("20 yr catch time series", "60 yr catch time series")
comp.names <- c("lh", "id", "ed", "ts")
comp.levels <- list(lhs, ids, eds, tls)
level.names <- list(lh.names, id.names, ed.names, tl.names)

# Table numbers
tables <- 8:11

# Loop through factors
# For testing: i <- 1; j <- 1; k <- 1
for(i in 1:length(comp.names)){
  
  # Factor info
  comp <- comp.names[i] 
  c.levels <- comp.levels[[i]]
  
  # Loop through levels
  for(j in 1:length(c.levels)){
    
    # Subset stocks to level
    if(comp=="lh"){sdata <- subset(data, lh==c.levels[j])}
    if(comp=="ts"){sdata <- subset(data, ts==c.levels[j])}
    if(comp=="id"){sdata <- subset(data, id==c.levels[j])}
    if(comp=="ed"){sdata <- subset(data, ed==c.levels[j])}
    
    # Setup data frame for recording results
    results <- data.frame(method=methods, accuracy=NA, kappa=NA)
    
    # Loop through status comparisons
    for(k in 1:length(status_cols)){
      
      # Compare status predictions with "true" status (B/BMSY status)
      status_col <- status_cols[k]
      pred <- factor(sdata[,status_col], levels=c("under", "fully", "over"))
      true <- factor(sdata$true_status, levels=c("under", "fully", "over"))
      perf.stats <- postResample(pred=pred, obs=true)
      
      # Record comparison
      results[k,2:3] <- perf.stats
      
    } # close status comparison loop
    
    # Rearrange results columns and values
    results <- subset(results, select=c(method, kappa, accuracy))
    results <- arrange(results, desc(kappa))
    
    # Save results for each level
    if(j==1){final <- results}else{final <- cbind(final, rep("", nrow(results)), results)}
  
  } # close level loop
  
  # Format combined results
  ##################################
  
  # Rename columns (allow for gap column)
  colnames(final) <- c(c("method", "kappa", "accuracy"),
                       rep(c("", "method", "kappa", "accuracy"), length(c.levels)-1))
  
  # Convert factors to strings
  fctr.cols <- sapply(final, is.factor)
  final[, fctr.cols] <- sapply(final[, fctr.cols], as.character)

  # Add subheader row
  newrow <- rep(NA, ncol(final))
  final <- rbind(newrow, final, stringsAsFactors=F)

  # Build and add subheader text
  sh.names <- level.names[[i]] 
  sh.counts <- sapply(c.levels, function(x) sum(data[,comp]==x))
  subheader.text <- paste0(sh.names, " (n=", sh.counts, ")")
  final[1,which(colnames(final)=="method")] <- subheader.text

  # Export table
  perf.file <- paste0("Table", tables[i], "_simstock_factor_comp_", comp, ".csv")
  write.csv(final, paste(tabledir, perf.file, sep="/"), na="", row.names=F)

} # close factor loop





