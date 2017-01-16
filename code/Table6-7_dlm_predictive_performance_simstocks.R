

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

# Statuses to compare
status_cols <- c("ssp02_status", "ssp13_status", "cmsy_status",
                 # "mprm_status", "sscom_status", "comsir_status",
                 "zbrt_status", "ocom_status")

# Methods
methods <- c("SSP-2002", "SSP-2013", "cMSY", 
             # "mPRM", "SSCOM", "COM-SIR",
             "Zhou-BRT", "Zhou-OCOM")

# How many didn't converge?
sapply(status_cols, function(x) sum(is.na(data_orig[,x])))

# Reduce to stocks with all predictions
data <- data_orig[sapply(1:nrow(data_orig), function(x) sum(is.na(data_orig[x, status_cols]))==0),]

# Setup data frame for recording fit
results <- data.frame(method=methods, accuracy=NA, kappa=NA)

# Loop through statuses
for(i in 1:length(status_cols)){
  
  # Compare status predictions with "true" status (B/BMSY status)
  status_col <- status_cols[i]
  pred <- factor(data[,status_col], levels=c("under", "fully", "over"))
  true <- factor(data$true_status, levels=c("under", "fully", "over"))
  perf.stats <- postResample(pred=pred, obs=true)
  
  # Record comparison
  results[i,2:3] <- perf.stats
  
  # Extract and transpose confusion matrix
  # transposing puts the predictions in columns
  confusion.matrix <- confusionMatrix(data=pred, reference=true)
  cmat <- as.matrix.data.frame(t(as.matrix(confusion.matrix[[2]])))
  cmat.df <- data.frame(method=methods[i], status=c("underexploited", "fully exploited", "overexploited"), n=c(table(true)), cmat, row.names=NULL)
  colnames(cmat.df) <- c("method", "status", "n", "under", "fully", "over")
  
  # Add success and error columns
  success <- sapply(1:3, function(x) cmat[x,x]/sum(cmat[x,])*100)
  re.error <- sapply(1:3, function(x) (sum(cmat[,x])-sum(cmat[x,]))/sum(cmat[x,])*100)
  success.format <- paste0(format(round(success, 1), nsmall=1), "%")
  re.error.format <- paste0(format(round(re.error, 1), nsmall=1), "%")
  cmat.df$success <- success.format
  cmat.df$error <- re.error.format
  
  # Add total row
  n <- sum(cmat.df$n)
  nunder <- sum(cmat.df$under)
  nfully <- sum(cmat.df$fully)
  nover <- sum(cmat.df$over)
  nsuccess <- sum(diag(cmat))
  psuccess <- paste0(format(round(nsuccess/n*100, 1), nsmall=1), "%")
  totsuccess <- paste0("(", nsuccess, ") ", psuccess)
  avgerror <- paste0(format(round(mean(abs(re.error)), 1), nsmall=1), "% (absolute)")
  tot.row <- data.frame(method=methods[i],
                        status="Total or mean:",
                        n=n,under=nunder, fully=nfully, over=nover,
                        success=totsuccess, error=avgerror)
  
  # Add total row to matrix
  cmat.df <- rbind(cmat.df, tot.row)
  
  # Save confusion matrices
  if(i==1){cmat.final <- cmat.df}else{cmat.final <- rbind(cmat.final, cmat.df)}
  
}

# Rearrange columns and values
results <- subset(results, select=c(method, kappa, accuracy))
results <- arrange(results, desc(kappa))

# EXPORT RESULTS
################################################################################

# Export results
perf.file <- "Table6_dlm_performance_simstocks.csv"
write.csv(results, paste(tabledir, perf.file, sep="/"), row.names=F)

# Export confusion matrices
confmat.file <- "Table7_dlm_confusion_matrix_simstocks.csv"
write.csv(cmat.final, paste(tabledir, confmat.file, sep="/"), row.names=F)





