

# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Packages
library(caret)

# Define directories
orcsdir <- "~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/brt_models"
preddir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"
tabledir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/tables/csvs"
  
# Read rORCS BRT model (just to get test dataset)
load(paste(orcsdir, "brt_catg_model_numeric.Rdata", sep="/"))
rm(key, data, data.train, best.model, tuned.models.bf5, tuned.models.bf6,
   tuned.models.bf7, tuned.models.bf8, tuned.models.bf9)

# Read status predictions
data_orig <- read.csv(paste(preddir, "dlm_status_predictions.csv", sep="/"), as.is=T)

# Perform analysis on "full" or "test" dataset
dataset <- "full"

# ANALYZE DATA
################################################################################

# Statuses to compare
status_cols <- c("orcs_status", 
                 # "mprm_status", 
                 "ssp02_status", "ssp13_status",
                 "cmsy_rf_status", "zbrt_status", "ocom_status")

# How many didn't converge?
sapply(status_cols, function(x) sum(is.na(data_orig[,x])))

# Reduce to stocks with all predictions
data <- data_orig[sapply(1:nrow(data_orig), function(x) sum(is.na(data_orig[x, status_cols]))==0),]
write.csv(data, paste(preddir, "dlm_status_predictions_finalset.csv", sep="/"), row.names=F)

# Optionally, reduce to just stocks in rOCRS test dataset
if(dataset=="test"){data <- subset(data, assessid%in%data.test$assessid)}

# Methods
methods <- c("rORCS", 
             # "mPRM", 
             "SSP-2002", "SSP-2013", 
             "cMSY", "Zhou-BRT", "Zhou-OCOM")

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

# Export file names
if(dataset=="full"){
  perf.file <- "Table3_dlm_performance_ramldb_full.csv"
  confmat.file <- "Table4_dlm_confusion_matrix_ramldb_full.csv"
}else{
  perf.file <- "Table3_dlm_performance_ramldb_test.csv"
  confmat.file <- "Table5_dlm_confusion_matrix_ramldb_test.csv"
}


# Export results
write.csv(results, paste(tabledir, perf.file, sep="/"), row.names=F)

# Export confusion matrices
write.csv(cmat.final, paste(tabledir, confmat.file, sep="/"), row.names=F)





