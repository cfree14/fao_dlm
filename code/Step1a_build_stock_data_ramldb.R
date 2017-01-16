
# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Packages
library(dplyr)
library(datalimited)

# Define directories
ramdir <- "~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/ramldb/ramldb_20150202/formatted"
orcsdir <- "~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/orcs_scores"
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"

# Read stock data
data <- read.csv(paste(orcsdir, "orcs_score_data_stocks.csv", sep="/"), as.is=T)

# Read RAMLDB assessment info
sainfo <- read.csv(paste(ramdir, "ramldb_assessment_data.csv", sep="/"), as.is=T)

# Read RAMLDB time series data
ts.orig <- read.csv(paste(ramdir, "ramldb_time_series_data.csv", sep="/"), as.is=T)

# Read resilience categories from Sean Anderson (probably from FishBase)
res <- read.csv("~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/other_dlm/data/RAM_bmsy_Ctousev4.csv", as.is=T)


# BUILD DATA
################################################################################

# Add species category, resilience, tcyrs, tlyrs columns
data$category <- NA
data$resilience <- NA
data$tc_nyrs <- NA
data$tl_nyrs <- NA
data$catch_ts <- NA
data$catch_ts_nyrs <- NA

# Fill in blank rows
for(i in 1:nrow(data)){
  
  # Stock
  stock <- data$assessid[i]
  print(paste(i, stock, "-"))
  
  # Look up and record species category
  spp <- data$species[i]
  spp_catg <- as.character(unique(ram_prm_dat$species_cat[ram_prm_dat$scientificname==spp]))
  if(length(spp_catg)==0){spp_catg <- NA}
  data$category[i] <- spp_catg
  
  # Look up and record species resilience
  spp_res <- unique(res$res[res$scientificname==spp])
  if(length(spp_res)==0){spp_res <- NA}
  if(spp_res=="" & !is.na(spp_res)){spp_res <- NA}
  data$resilience[i] <- spp_res
  
  # Record catch/landings time series length
  sdata <- subset(ts.orig, assessid==stock)
  data$tc_nyrs[i] <- sum(!is.na(sdata$tc))
  data$tl_nyrs[i] <- sum(!is.na(sdata$tl))
  data$catch_ts[i] <- ifelse(sum(!is.na(sdata$tc)) >= sum(!is.na(sdata$tl)), "tc", "tl")
  data$catch_ts_nyrs[i] <- max(data$tc_nyrs[i], data$tl_nyrs[i], na.rm=T)

}

# Species category
##############################################

# Fill in missing species categories
sort(unique(ram_prm_dat$species_cat))
sort(data$species[is.na(data$category)])
data$category[data$species=="Epinephelus flavolimbatus"] <- "Miscellaneous coastal fishes" # yellowedge grouper
data$category[data$species=="Loligo pealeii"] <- "Miscellaneous coastal fishes" # longfin inshore squid
data$category[data$species=="Lutjanus analis"] <- "Miscellaneous coastal fishes" # mutton snapper
data$category[data$species=="Sardinella aurita"] <- "Herrings, sardines, anchovies" # round sardinella
data$category[data$species=="Sardinella spp"] <- "Herrings, sardines, anchovies" # sardinella
data$category[data$species=="Scorpaena guttata"] <- "Miscellaneous coastal fishes" # California scorpionfish
sort(data$species[is.na(data$category)])

# Species resilience
##############################################

# Fill in missing species resiliences
sort(unique(res$res)) # High, Medium, Low, Very low
sort(unique(data$species[is.na(data$resilience)]))

# From FishBase (n=6)
data$resilience[data$species=="Sebastes spp"] <- "Low" # Acadian redfish: S. fasciatus=low (4.5-14 yr), S. mentella=Very low (>14 yr)
data$resilience[data$species=="Lutjanus analis"] <- "Low" # mutton snapper: low (4.5-14 yr doubling time)
data$resilience[data$species=="Epinephelus flavolimbatus"] <- "Low" # yellowedge grouper: low (4.5-14 yr doubling time)
data$resilience[data$species=="Scorpaena guttata"] <- "Medium" # CA scorpionfish: medium (1.4-4.4 yr doubling time)
data$resilience[data$species=="Sardinella spp"] <- "High" # S. aurita = high (<15 month), S. maderensis = medium (1.4-4.4 yr)
data$resilience[data$species=="Sardinella aurita"]  <- "High" # S. aurita = high (<15 month doubling time)

# From SeaLifeBase (n=9)
data$resilience[data$species=="Loligo pealeii"] <- "High" # longfin inshore squid: low-mod vulnerability (26/100) = high resilience
data$resilience[data$species=="Placopecten magellanicus"] <- "Medium" # Atl. deep-sea scallop: mod vulnerability (45/100) = mod resilience
data$resilience[data$species=="Homarus americanus"] <- "Medium" # American lobster: mod-high vulnerability (46/100) = mod resilience
data$resilience[data$species=="Paralithodes camtschaticus"] <- "High" # red king crab: low vulnerability (12/100) = high resilience
data$resilience[data$species=="Spisula solidissima"] <- "Medium" # Atlantic surf clam: mod vulnerability (37/100) = mod resilience
data$resilience[data$species=="Metanephrops challengeri"] <- "Medium" # scampi: M=0.2-0.3 (r=0.4-0.6=Medium)
data$resilience[data$species=="Chionoecetes opilio"] <- "Medium" # snow crab: M=0.23 (r=0.46)
data$resilience[data$species=="Chionoecetes bairdi"] <- "Medium" # tanner crab: M=0.23,0.13-0.35 (r=0.26-0.70=)
data$resilience[data$species=="Haliotis iris"] <- "Low" # blackfoot paua (abalone): M<0.1 (r=0.2=Low)

# Check which species are still missing resiliences
sort(unique(data$species[is.na(data$resilience)]))


# Only use species with >20 years of data
##############################################

# Catch time series lengths
hist(data$catch_ts_nyrs, breaks=seq(0,140,5), las=1, col="grey60", 
     main="", xlab="# of years")

# Subset data
# 185 to 172 stocks
nyrs <- 20
data <- subset(data, catch_ts_nyrs>=nyrs)


# EXPORT DATA
################################################################################

# Export data
write.csv(data, paste(datadir, "ramldb_stocks_for_fao_analysis.csv", sep="/"), row.names=F)




