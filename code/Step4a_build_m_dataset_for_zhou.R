

# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Packages
library(dplyr)
library(reshape2)
library(rfishbase)

# Define directories
ramdir <- "~/Dropbox/Chris/Rutgers/projects/orcs_methods/data/ramldb/ramldb_20150202/formatted"
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"
codedir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/code/zhou"
zinputdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/zhou_input"
zoutputdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/zhou_output"

# Read ORCS score M dataset
orcsm <- read.csv(paste(zinputdir, "orcs_score_natural_mortalities.csv", sep="/"), as.is=T)

# Read RAMLDB life history data
ramlh <- read.csv(paste(ramdir, "ramldb_bioparams_formatted.csv", sep="/"), as.is=T)
ramlh$biovalue <- as.numeric(ramlh$biovalue)

# BUILD FISHBASE LIFE HISTORY DATA
################################################################################

# Species
species <- sort(unique(orcsm$species))

# Lookup tmax (popchar), M, and Von B parameters (popgrowth)
fb_tmax <- popchar(species)
fb_mvonb <- popgrowth(species)

# Format FishBase tmax data
fb_tmax1 <- as.data.frame(
  fb_tmax %>%
    group_by(sciname) %>%
    summarize(tmax_yr=max(tmax, na.rm=T),
              tmax_obs=sum(!is.na(tmax)),
              tmax_vals=paste(sort(tmax[!is.na(tmax)]), collapse=", "))
)
fb_tmax1 <- subset(fb_tmax1, tmax_obs>0)

# Format FishBase M and Von B data
fb_mvonb1 <- as.data.frame(
  fb_mvonb %>%
    group_by(sciname) %>%
    summarize(m=mean(M, na.rm=T),
              m_obs=sum(!is.na(M)),
              m_vals=paste(sort(M[!is.na(M)]), collapse=", "),
              linf_mm=mean(TLinfinity, na.rm=T)*10,
              linf_obs=sum(!is.na(TLinfinity)),
              linf_vals=paste(sort(TLinfinity[!is.na(TLinfinity)]*10), collapse=", "),
              k=mean(K, na.rm=T)*10,
              k_obs=sum(!is.na(K)),
              k_vals=paste(sort(K[!is.na(K)]), collapse=", "))
)

# Merge FishBase M, tmax, and Von B data
fb_data <- merge(fb_mvonb1, fb_tmax1, by="sciname", all=T)


# FORMAT RAMLDB LIFE HISTORY DATA
################################################################################

# Useful life history traits
# Natural mortality, maximum age, Von B growth parameters (Linf/k)
sort(unique(ramlh$bioid))
traits <- c("NATMORT-1/yr", "M-1/month", "M-1/T", "M-1/yr", "MAX-AGE-yr",
            "VB-k-1/yr", "VB-k-cm/T", "VB-k-mm/T", "Linf-cm", "Linf-mm")

# Reduce to stocks and traits of interest
ramlh1 <- subset(ramlh, assessid%in%orcsm$assessid & bioid%in%traits)

# Reshape data (long-to-wide); rename and rearrange columns
ramlh2 <- dcast(ramlh1, assessid~bioid, value.var="biovalue", fun.aggregate=mean, na.rm=T)
colnames(ramlh2) <- c("assessid", "linf_cm", "linf_mm", "m_1yr1", "tmax_yr", "m_1yr2",
                      "k_1yr", "k_cmt", "k_mmt")
ramlh2 <- subset(ramlh2, select=c(assessid, m_1yr1, m_1yr2, tmax_yr, linf_cm, linf_mm, k_1yr))

# Are the Ms ever different? No. When 2 Ms are provided, they are identical.
# Thus, to generate the final M column, add the mean of the two options with na.rm=T
ramlh2[!is.na(ramlh2$m_1yr1) & !is.na(ramlh2$m_1yr2) & ramlh2$m_1yr1!=ramlh2$m_1yr2,]
ramlh2$m_1yr <- apply(ramlh2[, c("m_1yr1", "m_1yr2")], 1, mean, na.rm=T)

# If a linf (mm) is provided, is a linf (cm) also provided?
# No, linf (mm) is only provided once with no corresponding linf (cm)
# So, I should convert all available linf (cm) to linf (mm)
ramlh2[!is.na(ramlh2$linf_mm),]
ramlh2$linf_mm[!is.na(ramlh2$linf_cm)] <- ramlh2$linf_cm[!is.na(ramlh2$linf_cm)] * 10


# BUILD DATA
################################################################################

# Estimate M from life history
# M = 4.899*tmax^-0.916
# M = 4.118*k^0.73*linf^-0.33# Tmax = max age

# Setup M dataset
data <- data.frame(assessid=orcsm$assessid, species=orcsm$species, m_orcs=orcsm$m_val, 
                   m_ramldb=NA, m_fb=NA, m_tmax=NA, m_vonb=NA)

# Populate M dataset
for(i in 1:nrow(data)){
  
  # Stock
  stock <- data$assessid[i]
  spp <- data$species[i]
  
  # RAMLDB natural mortality rate
  m_ramldb <- ramlh2$m_1yr[ramlh2$assessid==stock]
  data$m_ramldb[i] <- ifelse(length(m_ramldb)==0, NA, m_ramldb)
  
  # FishBase natural mortality rate
  m_fb <- fb_data$m[fb_data$sciname==spp]
  data$m_fb[i] <- ifelse(length(m_fb)==0, NA, m_fb)
  
  # FishBase tmax-based natural mortality rate
  tmax_yr <- fb_data$tmax_yr[fb_data$sciname==spp]
  data$m_tmax[i] <- ifelse(length(tmax_yr)==0, NA, 4.899*tmax_yr^-0.916)
  
  # FishBase growth-based natural mortality rate
  k <- fb_data$k[fb_data$sciname==spp]
  linf_mm <- fb_data$linf_mm[fb_data$sciname==spp]
  if(length(k)==1 & length(linf_mm)==1){
    data$m_vonb[i] <- 4.118*k^0.73*linf_mm^-0.33
  }

}

# Identify final m value
data$m <- NA
data$m_source <- NA
for(i in 1:nrow(data)){
  
  # ORCS first
  if(!is.na(data$m_orcs[i])){
    data$m[i] <- data$m_orcs[i]
    data$m_source[i] <- "ORCS scores"
  }
  
  # RAMLDB second
  if(is.na(data$m_orcs[i]) & !is.na(data$m_ramldb[i])){
    data$m[i] <- data$m_ramldb[i]
    data$m_source[i] <- "RAMLDB"
  }
  
  # FishBase third
  if(is.na(data$m_orcs[i]) & is.na(data$m_ramldb[i]) & !is.na(data$m_fb[i])){
    data$m[i] <- data$m_fb[i]
    data$m_source[i] <- "FishBase"
  }
  
}

# Inspect missing values
data[is.na(data$m),]

# Provide answers to missing values
data[data$species=="Sardinella aurita",]
data$m[data$species=="Sardinella spp"] <- 0.98
data$m[data$species=="Thunnus orientalis"] <- 0.15
data$m[data$species=="Sebastes spp"] <- 4.899*60^-0.916 # S. aurita = 75 yr, S. fasciatus = 30-50 yr
data$m_source[data$species=="Sardinella spp"] <- "S. aurita FishBase value"
data$m_source[data$species=="Thunnus orientalis"] <- "Whitlock et al. 2012"
data$m_source[data$species=="Sebastes spp"] <- "FishBase maximum ages"
  
# EXPORT DATA
################################################################################

# Export data
write.csv(data, paste(zinputdir, "m_dataset_for_ramldb_stocks_for_zhou.csv", sep="/"), row.names=F)








