##############################################################################
# READ ME
# BEFORE EXECUTING THIS SCRIPT, BE SURE THAT:

###1) The folder MLC-Demo-RScripts/, and all of its subfolders and contents
###### (as described in ReadMe.pdf), are installed on your computer.

###2) The working directory for this R session is set to the MLC-Demo-RScripts/
###### folder.
##############################################################################


# ____________________________________________________
# Executing this file (1-Processing.R)
# opens the original datasets downloaded from the PEW
# as well as World Bank, then processes them and saves them as separate analysis files
# in the Temp/ folder.



# ++++++++++++++++++++++++++++++++++++++++++++++++++++
# Basic Startup Procedures
# ++++++++++++++++++++++++++++++++++++++++++++++++++++


# ____________________________________________________
# Clear plots, the console, and the R workspace

# Clear the plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clear workspace
rm(list=ls())


# ____________________________________________________
# Check to ensure all necessary packages are installed,
# and install any that are missing.

list.of.packages <- c("dplyr", "tidyr", "foreign", "expss", "openxlsx", "dmm")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)




# ____________________________________________________
# Clear plots, the console, and the R workspace

# Clear the plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clear workspace
rm(list=ls())


# ____________________________________________________
# Loads all of the packages needed
library(foreign) # Used for reading STATA file
library(dplyr) # Used for changing variable
library(tidyr) # Used for changing variables
library(expss) # Used for assigning variable names and labels
library(openxlsx) # Allows us to open the XLSX files for the WDI data
library(dmm) # Used for the "unfactoring" command


# ____________________________________________________
# Read the STATA data file of the first original data file, which is
# downloaded from the PEw survey
# and located in the OriginalData/ subfolder.

pew_working <- read.spss('OriginalData/OriginalPew.sav', to.data.frame=TRUE)

# Only keep the variabels that we need
pew_working <- data.frame(pew_working$country, pew_working$q2, pew_working$q74)


# ____________________________________________________
# rename the variables to their new names, and give them labels
#Here, we give name and label the "satis" variable
pew_working$satis <- pew_working$pew_working.q2
var_lab(pew_working$satis) <- "Satisfaction (self report); scale of 0-10"


#Here, we give name and label the "country" variable
pew_working$country <- pew_working$pew_working.country
var_lab(pew_working$country) <- "Region/Country of Origin"


#Here, we give name and label the "country" variable
pew_working$age <- pew_working$pew_working.q74
var_lab(pew_working$age) <- "Age (in years)"


#Unfactor the "age" variable so we can do some operations on it later on
pew_working$age <- unfactor(pew_working$age)

# Remove refusals  for "satis"
pew_working <- pew_working[pew_working$satis!= "Don't know (DO NOT READ)", ]
pew_working <- pew_working[pew_working$satis!= "Refused (DO NOT READ)", ]

# Convert "satis" to a numerical so can calculate averages later on
pew_working$satis <- as.numeric(pew_working$satis)
pew_working$satis <- (pew_working$satis)-1

#Remove refusals and the young or elderly from 'age"
# Note that refusals are coded as "98" and "99", so this removes them as well
pew_working <- pew_working[pew_working$age <= 70, ]
pew_working <- pew_working[pew_working$age >= 21, ]


# Generate a variable called n that shows amount of people from each country to responses

pew_working <- pew_working %>% add_count(country)


# Then drop all individuals for whom n is less than 900.
# In other words, remove the countries which have less than 900 respondents
pew_working <- filter(pew_working, n >= 900)

# Now drop the country_n variable, since it is no longer used.
# Also drop the q2, q74, and working.country variables since they are no longer needed
pew_working$n <- NULL 
pew_working$pew_working.q2 <- NULL
pew_working$pew_working.q74 <- NULL
pew_working$pew_working.country <- NULL

# Generate a new variable called "age2" which is equal to square of age
pew_working$age2 <- (pew_working$age)^2

# ____________________________________________________
# Save the analysis data file as PewAnalysis.csv in the 
# Temp/ folder


pew_analysis <- pew_working
write.csv(pew_analysis, file= "Temp/PewAnalysis.csv", row.names = FALSE)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++
# Begin Processing the WDI World Bank Original Data
# ++++++++++++++++++++++++++++++++++++++++++++++++++++


# ____________________________________________________
# Clear plots, the console, and the R workspace

# Clear the plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clear workspace
rm(list=ls())

# ____________________________________________________
# Loads all of the packages needed
library(dplyr) # Used for changing variables
library(tidyr) # Used for changing variables
library(expss) # Used for assigning variable names and labels
library(openxlsx) # Used for opening .xlsx files
library(reshape2) # Used for reshaping data

# ____________________________________________________
# Read the STATA data file of the second original data file, which is
# downloaded from the WDI survey
# and located in the OriginalData/ subfolder.

wdi_working <- read.xlsx('OriginalData/OriginalWDI.xlsx')

#Selects the first 14 rows, removing the last two rows (which do not have data anyways)
wdi_working <- head(wdi_working,14)

# Remove the "Series.Name" variable from our observations.  
wdi_working$Series.Name <- NULL                   

# ____________________________________________________
# Reshape the spreadsheet so that unit of observation becomes "country",
# and the variable becomes "series"

wdi_working$var_no <- wdi_working$Series.Code
wdi_working$Series.Code <- NULL
wdi_working$YR <- wdi_working$`2002.[YR2002]`
wdi_working$`2002.[YR2002]` <- NULL


# Reshape it so that it shows the government consumption and per capita GDP as columns 
# alongside the country code. 

library(reshape2)
wdi_working <- dcast(wdi_working, Country.Code~var_no, value.var='YR')
names(wdi_working)[-1] <- paste('YR', names(wdi_working[-1]))
wdi_working

# ____________________________________________________
# Modification of variables, such as renaming and relabeling 

# Rename the variable "YR NE.CON.GOVT.ZS" to "gov_cons"
# Also give the "gov_cons" variable the label of "Gov. cons., % of GDP"

wdi_working$gov_cons  <- wdi_working$`YR NE.CON.GOVT.ZS`
wdi_working$`YR NE.CON.GOVT.ZS` <- NULL
var_lab(wdi_working$gov_cons) <- "Gov. cons., % of GDP"

# Rename the variable "YR NY.GDP.PCAP.CD" to "gdp_pc"
# Also give the "gdp_pc" variable the label of "GDP per capita (current [2002] $)"

wdi_working$gdp_pc <- wdi_working$`YR NY.GDP.PCAP.CD`
wdi_working$`YR NY.GDP.PCAP.CD` <- NULL
var_lab(wdi_working$gdp_pc ) <- "GDP per capita (current [2002] $)"


#Generate a variable called "country", which matches the PEW codes

wdi_working$country[wdi_working$Country.Code == "CHN"] <- "China"
wdi_working$country[wdi_working$Country.Code == "IND"] <- "India"
wdi_working$country[wdi_working$Country.Code == "IDN"] <- "Indonesia"
wdi_working$country[wdi_working$Country.Code == "JOR"] <- "Jordan"
wdi_working$country[wdi_working$Country.Code == "PAK"] <- "Pakistan"
wdi_working$country[wdi_working$Country.Code == "RUS"] <- "Russia"
wdi_working$country[wdi_working$Country.Code == "USA"] <- "US"


# Remove the"Series Code" variable from our observations.               
wdi_working$Series.Code <- NULL 

# ____________________________________________________  
# Saves the processed data into the working directory's Temp/
# folder as WDIAnalysis.csv

wdi_analysis <- wdi_working
write.csv(wdi_analysis, file= "Temp/WDIAnalysis.csv", row.names = FALSE)
