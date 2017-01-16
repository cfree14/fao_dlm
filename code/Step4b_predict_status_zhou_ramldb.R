

# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Packages
library(plyr)

# Define directories
ramdir <- "~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/ramldb/ramldb_20150202/formatted"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"
codedir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/code/zhou"
zinputdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/zhou_input"
zoutputdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/zhou_output"
cmsyinputdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/cmsy_input"
preddir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions"

# Read catch data
catch <- read.csv(paste(cmsyinputdir, "ramldb_stock_catch_time_series_for_cmsy_froese.csv", sep="/"), as.is=T) 

# Read natural mortality data
mdata <- read.csv(paste(zinputdir, "m_dataset_for_ramldb_stocks_for_zhou.csv", sep="/"), as.is=T) 
sessM <- subset(mdata, select=c(assessid, m))
colnames(sessM) <- c("stock", "M")

# Source Zhou OCOM models/functions
load(paste(codedir, "BRTmodelP8.RData", sep="/"))   
load(paste(codedir, "BRTmodelP38.RData", sep="/"))
source(paste(codedir, "OCOM functions_R code.R", sep="/"))


# ESTIMATE SATURATION (ZHOU ET AL 2016a)
################################################################################

# Format catch data for Zhou method
# catch data must have three columns: stock, yr, catch
colnames(catch) <- c("stock", "yr", "catch", "bt")
catchData <- subset(catch, select=c(stock, yr, catch))

# Construct predictors
###########################################

# Derive predictors from catch history
sessPar <- catchParam(catchData)

# Center the first 37 predictors   
sessParCent <- scale(sessPar[,2:38], center=BRTmodelP8$parMean, scale=F)

# Construct predition data  
stockName <- unique(as.character(catchData$stock))
nstk <- length(stockName)
predDat <- data.frame(stock=stockName, sessParCent, sessPar[,39:57])

# Estimate saturation
###########################################

# Estimate saturation
s8 <- predict.gbm(BRTmodelP8$model, predDat, n.trees=BRTmodelP8$model$gbm.call$best.trees, type="response")
s38 <- predict.gbm(BRTmodelP38$model, predDat, n.trees=BRTmodelP38$model$gbm.call$best.trees, type="response")  

# Bias correction for model 1
slr8.a <- BRTmodelP8$slr[[1]]; slr8.b = BRTmodelP8$slr[[2]]; 
sBC8 <- (s8-slr8.a)/slr8.b
sBC8[sBC8<=0] <- 0.01 

# Bias correction for model 2
slr38.a <- BRTmodelP38$slr[[1]]; slr38.b = BRTmodelP38$slr[[2]]; 
sBC38 <- (s38-slr38.a)/slr38.b
sBC38[sBC38<=0] <- 0.01

# Output saturation
sessS <- data.frame(stock=stockName, S8=sBC8, S38=sBC38, S = (sBC8+sBC38)/2)

# Export saturation estimates
write.csv(sessS, paste(preddir, "status_predictions_zhou_brt.csv", sep="/"), row.names=F)

# Export saturation estimates
write.csv(sessPar, paste(zoutputdir, "status_predictors_zhou_brt.csv", sep="/"), row.names=F)


# OCOM STOCK ASSESSMENT (ZHOU ET AL 2016B)
################################################################################

# Iterations?
n = 10000 

# Setup empty array
summ = array(NA, dim=c(5,4,nstk))

# Loop through stocks: i <- 1
for (i in 1:nstk) {
  
  # Subset stock data
  stk = stockName[i]
  dat = subset(catchData, stock==stk)
  C = dat$catch ; 
  yr = dat$yr
  print(paste(i, stk, sep=" - "))
  
  # Derive r prior with lognorm dist 
  M = sessM$M[sessM$stock==stk] 
  r_mean = M*1.72; r_var = 0.23            # for teleosts 
  r_sig = sqrt(log(r_var/(r_mean)^2+1))
  ## r_mean = 0.79*M ; r_var = 0.23           # for chondrichthyans
  r_mu = log(r_mean)-r_sig^2/2   
  ri = rlnorm(n, r_mu, r_sig)
  
  # Derive S prior btw 0 and 1
  s_mean = sessS[sessS$stock==stk, 4] # column 2 = BRTmodelP8
  si = Sdistrib(n, s_mean)
  
  rs = cbind(r=ri, s=si)
  k.low = max(C); k.up=max(C)*200
  opt = apply(rs, 1, function(x) { optimize(BDM, c(k.low, k.up), r=x[["r"]], S=x[["s"]], b=1, C=C) }) 
  
  ki = sapply(opt, '[[', 1)
  msy = ki*ri/4
  obji = sapply(opt,'[[',2)
  kr = data.frame(k=ki, r=ri, msy, s=si, obj=obji)
  
  # Save whatever this is
  write.csv(kr, paste0(zoutputdir, "/csvs/kr", i, ".csv", sep=""))
  
  ## summary 
  kr2 = kr
  kr2[kr2$k<1.01*k.low | kr2$k>0.99*k.up | kr2$obj>0.01,] = NA # bordering effect
  kr2 = na.omit(kr2)
  # plot(log(kr2$k), log(kr2$r), xlab="K", ylab='r')  
  # abline(h=mean(log(kr$r)), v=mean(log(kr$k)), lty=2, col=2)
  summ[,,i] = apply(kr2, 2, function(x) quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95)))[,1:4]
}

## summary result
summ2 = adply(summ, 3) 
colnames(summ2)=c('stockID', 'k', 'r', 'MSY', 'S')
sumOut = data.frame(percent=rep(c(5, 25, 50, 75, 95),nstk), summ2) 

# Save results
write.csv(sumOut, paste0(preddir, "/status_predictions_zhou_ocom.csv"), row.names=F)

#####################################
## distribution 
par(mfrow=c(2,2), mai=c(.8,.8,0.1,0.1))
hist(kr2$k, xlab='K', main='', las=1); abline(v=median(kr$k), lwd=2); 
hist(kr2$r, xlab='r', main='', las=1); abline(v=median(kr$r), lwd=2);
hist(kr2$msy, xlab='MSY', main='', las=1); abline(v=median(kr$msy), lwd=2);
hist(kr2$s, xlab='S', main='', las=1); abline(v=median(kr$s), lwd=2);

#####################################
## draw 100 biomass trajectories

par(mfrow=c(5,3), mai=c(.2,.25,0.1,0.1), omi=c(0.5,0.5,0,0))

for (i in 1:nstk) { # i=1
  cdat = subset(catchData, stock==stockName[i])
  oc0 = read.csv(paste0(zoutputdir,'/csvs/kr', i, '.csv', sep=''))
  drawBt(cdat, oc0)
}


