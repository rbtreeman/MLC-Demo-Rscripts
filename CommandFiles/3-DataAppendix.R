##############################################################################
# READ ME
# BEFORE EXECUTING THIS SCRIPT, BE SURE THAT:

###1) The folder MLC-Demo-RScripts/, and all of its subfolders and contents
###### (as described in ReadMe.pdf), are installed on your computer.

###2) The working directory for this R session is set to the MLC-Demo-RScripts/
###### folder.
##############################################################################


# ____________________________________________________
# Executing this file (3-DataAppendix.R), 
# opens the Analysis.csv dataset created by 2-Merging.R, 
# and creates the graphs and tables found in the DataAppendix/ subfolder of the 
# Output/ folder. 


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

list.of.packages <- c("tidyr", "dplyr", "ggplot2", "summarytools")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)


# ____________________________________________________
# Loads all of the packages needed.
library(dplyr) # Used for changing variables
library(tidyr) # Used for changing variables
library(ggplot2) # Used for graphing
library(foreign) # Used for importing .dta files
library(summarytools) # Used for creating frequency tables
library(lattice)

# ____________________________________________________
# Open up the analysis.csv file located in the Analysis-Data/ folder. 

analysis <- read.csv('AnalysisData/Analysis.csv')


# ++++++++++++++++++++++++++++++++++++++++++++++++++++
# Individual Level Variables
# ++++++++++++++++++++++++++++++++++++++++++++++++++++

# ____________________________________________________
# Now creating the frequency table for "country"
countrytable <- freq(analysis$country, plain.ascii = FALSE, style = "simple")
countrytable <- cbind(Row.Names = rownames(countrytable), countrytable)
countrytable <- countrytable[,c(1,2,3,4)]
colnames(countrytable)[colnames(countrytable)=="Row.Names"] <- "Country"
colnames(countrytable)[colnames(countrytable)=="Freq"] <- "Individuals"
colnames(countrytable)[colnames(countrytable)=="% Valid"] <- "Relative Frequency (Percent)"
colnames(countrytable)[colnames(countrytable)=="% Valid Cum."] <- "Cumulative Frequency(Percent)"

write.table(countrytable, file = "Output/DataAppendix/countrytable.csv", sep = ",", quote = FALSE, row.names = F)
write.table(countrytable, file = "Output/DataAppendix/countrytable.txt", sep = ",", quote = FALSE, row.names = F)


# ____________________________________________________
# Now creating the frequency table for "satis"
satistable <- freq(analysis$satis, plain.ascii = FALSE, style = "simple")
satistable <- cbind(Row.Names = rownames(satistable), satistable)
satistable <- satistable[,c(1,2,3,4)]
colnames(satistable)[colnames(satistable)=="Row.Names"] <- "Satisfaction Level"
colnames(countrytable)[colnames(countrytable)=="Freq"] <- "Individuals"
colnames(satistable)[colnames(satistable)=="% Valid"] <- "Relative Frequency (Percent)"
colnames(satistable)[colnames(satistable)=="% Valid Cum."] <- "Cumulative Frequency(Percent)"

write.table(satistable, file = "Output/DataAppendix/satistable.csv", sep = ",", quote = FALSE, row.names = F)
write.table(satistable, file = "Output/DataAppendix/satistable.txt", sep = ",", quote = FALSE, row.names = F)

# Now creating the bar plot distribution for "satis"
png("Output/DataAppendix/satisdistribution.png")
barplot(table(analysis$satis)/sum(!is.na(analysis$satis)), 
        main = "Barplot of Satis", 
        xlab = "Satisfaction Level", 
        ylab = "Relative Frequency", 
        col = "darkblue")
dev.off()


# ____________________________________________________
# Now creating the summary statistics table for "age"
agetable <- descr(analysis$age, stats = "fivenum", transpose = TRUE, headings = FALSE)
write.table(agetable, file = "Output/DataAppendix/agetable.csv", sep = ",", quote = FALSE, row.names = F)
write.table(agetable, file = "Output/DataAppendix/agetable.txt", sep = ",", quote = FALSE, row.names = F)

# Now creating the histogram distribution for "satis"
png("Output/DataAppendix/agedistribution.png")
barplot(table(analysis$age)/sum(!is.na(analysis$age)), 
        main = "Histogram of Age", 
        xlab = "Age in Years", 
        ylab = "Relative Frequency", 
        col = "darkblue")
dev.off()

# ____________________________________________________
# Now creating the summar statistics table for "age_squared"
age_sqtable <- descr(analysis$age2, stats = "fivenum", transpose = TRUE, headings = FALSE)
write.table(age_sqtable, file = "Output/DataAppendix/agesqtable.csv", sep = ",", quote = FALSE, row.names = F)
write.table(age_sqtable, file = "Output/DataAppendix/agesqtable.txt", sep = ",", quote = FALSE, row.names = F)

# Now creating the histogram distribution for "satis"
png("Output/DataAppendix/age_sqdistribution.png")
barplot(table(analysis$age2)/sum(!is.na(analysis$age2)), 
        main = "Histogram of Age Squared", 
        xlab = "Age in Years Squared", 
        ylab = "Relative Frequency", 
        col = "darkblue")
dev.off()


# ++++++++++++++++++++++++++++++++++++++++++++++++++++
# Country Level Variables
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
# Loads all of the packages needed.
library(dplyr) # Used for changing variables
library(tidyr) # Used for changing variables
library(ggplot2) # Used for graphing
library(foreign) # Used for importing .dta files
library(summarytools) # Used for creating frequency tables
library(lattice)


# ____________________________________________________
# Open up the analysis.csv file located in the Analysis-Data/ folder. 

analysis <- read.csv('AnalysisData/Analysis.csv')

# ____________________________________________________
# Create some dataframes required for the next few graphs

# Create country level mean satisfaction
mean_satis <- analysis %>%
        group_by(country) %>%
        dplyr::summarize(mean_satis = mean(satis, na.rm=TRUE))

# Create mean GDP per capita variable by country 
mean_satis2 <- analysis %>%
        group_by(country) %>%
        dplyr::summarize(mean_gdp_pc = mean(gdp_pc, na.rm=TRUE))

# Create government consumptiona as percent of GDP
mean_satis3 <- analysis %>%
        group_by(country) %>%
        dplyr::summarize(mean_gov_cons = mean(gov_cons, na.rm=TRUE)) 

# ____________________________________________________
# Now creating a table for the country level satisfaction
cm_satistable <- descr(analysis$gov_cons, stats = "fivenum", transpose = TRUE, headings = FALSE)
write.table(cm_satistable, file = "Output/DataAppendix/cm_satistable.csv", sep = ",", quote = FALSE, row.names = F)
write.table(cm_satistable, file = "Output/DataAppendix/cm_satistable.txt", sep = ",", quote = FALSE, row.names = F)

# Now creating the histogram distribution for "cm_satis"
png("Output/DataAppendix/cm_satisdistribution.png")
print(histogram(mean_satis$mean_satis, xlab = "Mean Satisfaction by Country Distribution", ylab = "Relative Frequency", 
          main = "Histogram of Country Level Satisfaction (cm_satis)", col="red"))
dev.off()

# ____________________________________________________
# Now creating a table for the gov_cons

gov_constable <- descr(analysis$gov_cons, stats = "fivenum", transpose = TRUE, headings = FALSE)
write.table(gov_constable, file = "Output/DataAppendix/gov_constable.csv", sep = ",", quote = FALSE, row.names = F)
write.table(gov_constable, file = "Output/DataAppendix/gov_constable.txt", sep = ",", quote = FALSE, row.names = F)


# Now creating the histogram distribution for "gov_cons"
png("Output/DataAppendix/gov_consdistribution.png")
print(histogram(mean_satis3$mean_gov_cons, xlab = "Government Consumption by Country Distribution", ylab = "Relative Frequency", 
          main = "Government Consumption as Percent of GDP (gov_cons)", col="red"))
dev.off()

# ____________________________________________________
# Now creating a table for the gdp_pc

gdp_pctable <- descr(analysis$gdp_pc, stats = "fivenum", transpose = TRUE, headings = FALSE)
write.table(gdp_pctable, file = "Output/DataAppendix/gdp_pctable.csv", sep = ",", quote = FALSE, row.names = F)
write.table(gdp_pctable, file = "Output/DataAppendix/gdp_pctable.txt", sep = ",", quote = FALSE, row.names = F)

# Now creating the histogram distribution for "gdp_pc"
png("Output/DataAppendix/gdp_pcsdistribution.png")
print(histogram(mean_satis2$mean_gdp_pc, xlab = "GDP Per Capita in USD, by country", ylab = "Relative Frequency", 
          main = "GDP per capita in USD (gdp_pc)", col="red", breaks=10))
dev.off()
# ____________________________________________________
# End of file