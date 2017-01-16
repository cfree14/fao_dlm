
# Clear workspace
rm(list = ls())

# READ DATA
################################################################################

# Detach dplyr
detach("package:dplyr", unload=T)

# Define directories
datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data"
cmsydir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/code/CMSY"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/figures"

# Read ID and catch data
id_file <- "fao_dlm_stock_info.csv"
catch_file <- "fao_dlm_catch_time_series.csv"
id <- read.csv(paste(cmsydir, id_file, sep="/")) 
catch <- read.csv(paste(cmsydir, catch_file, sep="/"))

# Select stocks to run
stocks <- as.character(id$Stock)[1:3]

# Set working directory
setwd(cmsydir)

# Run CMSY code
source(paste(cmsydir, "CMSY_O_7q_modified_for_fao_dlm_analysis.R", sep="/"))
