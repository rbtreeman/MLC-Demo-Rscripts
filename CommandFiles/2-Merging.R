##############################################################################
# READ ME
# BEFORE EXECUTING THIS SCRIPT, BE SURE THAT:

###1) The folder MLC-Demo-RScripts/, and all of its subfolders and contents
###### (as described in ReadMe.pdf), are installed on your computer.

###2) The working directory for this R session is set to the MLC-Demo-RScripts/
###### folder.
##############################################################################


# ____________________________________________________
# Executing this file (2-Merging.R), 
# opens the files created by 1-Processing.R (
# WDIAnalysis.csv and PEWAnalysis.csv) which are located in the Temp/ folder, 
# and merges them together, before saving them as Analysis.csv in the 
# AnalysisData/ folder. 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++
# Basic Startup Procedures
# ++++++++++++++++++++++++++++++++++++++++++++++++++++


# ____________________________________________________
# Clear plots, the console, and the R workspace.

# Clear the plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clear workspace
rm(list=ls())


# ____________________________________________________
# Check to ensure all necessary packages are installed,
# and install any that are missing.

list.of.packages <- c("plyr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)


# ____________________________________________________
# Loads all of the packages needed.
library(plyr) # Used for merging


# ____________________________________________________  
# Opens up data frame PEWAnalysis.csv and WDIAnalysis.csv
pew_analysis <- read.csv('Temp/PewAnalysis.csv')
wdi_analysis <- read.csv('Temp/WDIAnalysis.csv')


# ____________________________________________________  
# Merges them together in the 1:M format, using pew_analysis dataframe as the lower level
merged_working <- join(pew_analysis, wdi_analysis, by='country', type='full')

# ____________________________________________________  
# Saves the merged data set into the Analysis-Data/
# folder as Analysis.csv
# This is the "analysis data" file that will be used by 3-Processing.R and 4-Analysis.R to generate the 
# figures that appears in report and the data appendix

merged_analysis <- merged_working
write.csv(merged_analysis, file= "AnalysisData/Analysis.csv", row.names = FALSE)


# End of File