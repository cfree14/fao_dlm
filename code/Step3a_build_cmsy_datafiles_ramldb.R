

# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Packages
library(dplyr)

# Define directories
ramdir <- "~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/ramldb/ramldb_20150202/formatted"
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"
cmsyinputdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/cmsy_input"

# Read stock data
data_orig <- read.csv(paste(datadir, "ramldb_stocks_for_fao_analysis.csv", sep="/"), as.is=T)
data_orig <- subset(data_orig, select=c(assessid, agency1,
                                        country, region, area, 
                                        species, species_common,
                                        category, resilience, catch_ts))
stocks <- data_orig$assessid

# Read RAMLDB assessment info
sainfo <- read.csv(paste(ramdir, "ramldb_assessment_data.csv", sep="/"), as.is=T)

# Read RAMLDB time series data
ts.orig <- read.csv(paste(ramdir, "ramldb_time_series_data.csv", sep="/"), as.is=T)



# BUILD CATCH FILE
################################################################################

# Build catch file
# Stock, yr, ct, bt
for(i in 1:length(stocks)){
  
  # Stock
  stock <- stocks[i]
  catch_type <- data_orig$catch_ts[data_orig$assessid==stock]
  print(paste(i, stock, sep="-"))
  
  # Subset data
  sdata <- subset(ts.orig, assessid==stock, select=c("assessid", "year", catch_type))
  colnames(sdata) <- c("Stock", "yr", "ct")
  
  # Eliminate years with no catch data (catch=NA)
  sdata <- subset(sdata, !is.na(ct))
  
  # Combine data
  if(i==1){catch <- sdata}else{catch <- rbind(catch, sdata)}
  
}

# Add bt column
catch$bt <- NA

# Export catch file
write.csv(catch, paste(cmsyinputdir, "ramldb_stock_catch_time_series_for_cmsy_froese.csv", sep="/"), row.names=F)


# BUILD ID FILE
################################################################################

# Build ID file
id <- data.frame(Region=data_orig$region,
                 Subregion=data_orig$area,
                 Stock=data_orig$assessid,
                 Name=data_orig$species_common,
                 EnglishName=data_orig$species_common,
                 ScientificName=data_orig$species,
                 SpecCode=NA, # optional, don't bother
                 Group=data_orig$category,
                 Source="RAMLDB",
                 # required, look up in catch time series
                 MinOfYear=NA, MaxOfYear=NA,
                 StartYear=NA, EndYear=NA, 
                 # optional, but fill in if possible
                 Flim=NA, Fpa=NA, Blim=NA, Bpa=NA, Bmsy=NA, FMSY=NA,
                 MSYBtrigger=NA, B40=NA, M=NA, Fofl=NA, SSB=NA, 
                 Resilience=data_orig$resilience,
                 # optional, just use defaults
                 r.low=NA, r.hi=NA, stb.low=NA, stb.hi=NA, int.yr=NA, 
                 intb.low=NA, intb.hi=NA, endb.low=NA, endb.hi=NA, q.start=NA, q.end=NA, 
                 btype="None", 
                 force.cmsy=F,
                 Comment=NA)

# Loop through stocks and fill in the blanks
for(i in 1:nrow(id)){
  
  # Stock
  stock <- id$Stock[i]
  
  # Catch info
  scatch <- subset(catch, Stock==stock)
  id$MinOfYear[i] <- min(scatch$yr)
  id$MaxOfYear[i] <- max(scatch$yr)
  id$StartYear[i] <- scatch$yr[min(which(scatch$ct!=0))]
  id$EndYear[i] <- id$MaxOfYear[i]
  
  # Reference points
  id$Bmsy[i] <- sainfo$bmsy[sainfo$assessid==stock]
  id$FMSY[i] <- sainfo$fmsy[sainfo$assessid==stock]
  id$M[i] <- sainfo$m[sainfo$assessid==stock]
  id$SSB[i] <- sainfo$ssbmsy[sainfo$assessid==stock]
  
}
  
# Export id file
write.csv(id, paste(cmsyinputdir, "ramldb_stock_info_key_for_cmsy_froese.csv", sep="/"), row.names=F)


