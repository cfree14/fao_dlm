
# READ DATA
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(plyr)
library(dplyr)

# Define directories
simdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/simulated_stocks"
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"
cmsyinputdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/cmsy_input"

# Load data
data <- readRDS(paste(simdir, "batch1result.rds", sep="/"))

# BUILD STOCK KEY
################################################################################

# 5760 scenarios from Rosenberg et al. (2014)
# ----------------------------------------------
# Life history (LH): large pelagic (LP), small pelagic (SP), demersal (DE)
# Biomass deplation (ID): 100% (1.0), 70% (0.7), 40% (0.4) of carrying capacity
# Exploitation dynamics (ED): constant (ED0), biomass-coupled (ED0.6), increasing (OW), roller-coaster (RC)
# Process error (sigmaR): 0.2 or 0.6 variance in normal distribution
# Autoregressive error (AR): uncorrelated (0.0) or 0.6 autoregressive correlation
# Observation error (sigmaC): No error (0.0) or 0.2 variance in normal distribution
# Time series length (TS): 20 or 60 years
# Iterations (iter): 10 stochastic iterations for each scenario

# Add and reduce columns
data$stockid <- paste(data$LH, "_", data$ID*100, "%_", data$ED, "_", 
                      data$sigmaR, "sr_", data$AR, "ar_", data$sigmaC, "sc_", data$TS, "yr_", data$iter, sep="")
key <- subset(data, select=c(stockid, stock_id, LH, ID, ED, sigmaR, AR, sigmaC, TS, iter))

# Reduce rows to unique parameter combinations
key <- unique(key)

# Rearrange column order
key <- arrange(key, LH, ID, ED, sigmaR, AR, sigmaC, TS, iter)

# Rename columns
colnames(key) <- c("stockid", "stockid_orig", "lh", "id", "ed", "sigmaR", "ar", "sigmaC", "ts", "iter")

# Record true final BMSY and whether each DLM worked
one.stock <- subset(data, stockid=="DE_100%_ED0_0.2sr_0.8ar_0.2sc_20yr_1")
stats <- as.data.frame(
  data %>%
    group_by(stockid) %>%
    summarize(bbmsy=unique(b_bmsy_true[year==max(year)]),
              bbmsy_status=cut(bbmsy, breaks=c(0,0.5,1.5,999),labels=c("over", "fully", "under")),
              mprm=ifelse("Costello"%in%unique(method_id), "yes", "no"),
              comsir=ifelse("COM.SIR"%in%unique(method_id), "yes", "no"),
              sscom=ifelse("SSCOM"%in%unique(method_id), "yes", "no")
              )
)

# How many times did method fail?
sum(stats$mprm=="no") # 25 missing
sum(stats$sscom=="no") # 0 missing
sum(stats$comsir=="no") # 5 missing

# Merge datasets
key <- merge(key, stats, by="stockid")

# Export key
write.csv(key, paste(datadir, "simulated_stocks_for_fao_analysis.csv", sep="/"), row.names=F)


# FORMAT TIME SERIES DATA
################################################################################

# Reduce and rename columns
catch <- subset(data, select=c(stockid, 
                               LH, ID, ED, sigmaR, AR, sigmaC, TS, iter,  
                               year, b_bmsy_true, catch))
colnames(catch) <- c("stockid", 
                     "lh", "id", "ed", "sigmaR", "ar", "sigmaC", "ts", "iter",
                     "year", "bbmsy", "catch")

# Reduce to unique entries
catch <- unique(catch)

# Rearrange order
catch <- arrange(catch, lh, id, ed, sigmaR, ar, sigmaC, ts, iter, year)

# Export simulated stock data with code
save(data, catch, key, file=paste(simdir, "anderson_etal_simstocks_output.Rdata", sep="/"))


# BUILD DATA FOR CMSY (FROESE)
################################################################################

# Build catch time series
##################################

# Build catch time series
ts <- as.data.frame(
  data %>%
    group_by(stockid, year) %>%
    summarize(catch=unique(catch))
)
colnames(ts) <- c("Stock", "yr", "ct")
ts$bt <- NA

# Export catch file
write.csv(ts, paste(cmsyinputdir, "simulated_stock_catch_time_series_for_cmsy_froese.csv", sep="/"), row.names=F)

# Build ID file
##################################

# Build ID file
id <- data.frame(Region="In",
                 Subregion="Silica",
                 Stock=key$stockid,
                 Name=paste("Computer fish", 1:nrow(key)),
                 EnglishName=paste("Computer fish", 1:nrow(key)),
                 ScientificName=paste("Fishus computerus", 1:nrow(key)),
                 SpecCode=NA, # optional, don't bother
                 Group=key$lh,
                 Source="Rosenburg et al. 2014",
                 # required, look up in catch time series
                 MinOfYear=61-key$ts, MaxOfYear=60,
                 StartYear=61-key$ts, EndYear=60,
                 # optional, but fill in if possible
                 Flim=NA, Fpa=NA, Blim=NA, Bpa=NA, Bmsy=NA, FMSY=NA,
                 MSYBtrigger=NA, B40=NA, M=NA, Fofl=NA, SSB=NA, 
                 Resilience=revalue(key$lh, c("SP"="Medium", "DE"="Low", "LP"="Low")),
                 # optional, just use defaults
                 r.low=NA, r.hi=NA, stb.low=NA, stb.hi=NA, int.yr=NA, 
                 intb.low=NA, intb.hi=NA, endb.low=NA, endb.hi=NA, q.start=NA, q.end=NA, 
                 btype="None", 
                 force.cmsy=F,
                 Comment=NA)

# Export id file
write.csv(id, paste(cmsyinputdir, "simulated_stock_info_key_for_cmsy_froese.csv", sep="/"), row.names=F)






