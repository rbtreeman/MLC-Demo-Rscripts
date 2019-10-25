##############################################################################
# READ ME
# BEFORE EXECUTING THIS SCRIPT, BE SURE THAT:

###1) The folder MLC-Demo-RScripts/, and all of its subfolders and contents
###### (as described in ReadMe.pdf), are installed on your computer.

###2) The working directory for this R session is set to the MLC-Demo-RScripts/
###### folder.
##############################################################################


# ____________________________________________________
# Executing this file (4-Analysis.R), 
# opens the analysis.csv dataset created by 2-Merging.R, 
# and creates the graphs and tables found in the various subfolders of the 
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

list.of.packages <- c("memisc", "tidyr", "dplyr", "ggplot2", "xtable", "AMR", "stargazer")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)


# ____________________________________________________
# Loads all of the packages needed.
library(dplyr) # Used for changing variables
library(tidyr) # Used for changing variables
library(ggplot2) # Used for graphing
library(memisc) #Used for regressions
library(xtable) #Used for TEX tables of regression
library(AMR) # Used for frequency tables
library(stargazer) #Used for TEX tables of regression

# ____________________________________________________
# Open up the analysis.csv file located in the Analysis-Data/ folder. 

analysis <- read.csv('AnalysisData/Analysis.csv')

# ____________________________________________________
#Generate Table 1

#Comment:....

analysis$satis<- as.numeric(analysis$satis)

mean_satis <- analysis %>%
  group_by(country) %>%
  dplyr::summarize(mean_satis = mean(satis, na.rm=TRUE))

gdp_pc <- analysis %>%
  group_by(country) %>%
  dplyr::summarize(gdp_pc = mean(gdp_pc, na.rm=TRUE)) 

gov_cons <- analysis %>%
  group_by(country) %>%
  dplyr::summarize(gov_cons = mean(gov_cons, na.rm=TRUE)) 

#Comment:...

means_table <- cbind(mean_satis, gdp_pc, gov_cons)
means_table <- means_table[,c(1,2,4,6)]

#Label the variables in means_tabl
#var_lab(means_table$mean_satis) <- "Country Mean Satisfaction (self report); scale of 0-10"
#var_lab(means_table$gdp_pc) <- "GDP per capita"
#var_lab(means_table$gov_cons) <- "Government Consumption as a Percent of GDP"

#Comment:...writing the tables to the Output/Tables/ folder...

write.table(means_table, file = "Output/Tables/Means_table.csv", sep = ",", quote = FALSE, row.names = F)
write.table(means_table, file = "Output/Tables/Means_table.txt", sep = ",", quote = FALSE, row.names = F)


# ____________________________________________________
#Generate Table 2

# Regression 1 (This regression only considers age)
reg1 <- lm(satis ~ age + age2 ,data = analysis)

# Regression 2 *controlling for country fixed effects
reg2 <- lm(satis ~ age + age2 + Country.Code ,data = analysis)

# Place results of these two regressions in a table

# Latex Version
stargazer(reg1, reg2, title="Results of the Two Regressions", 
          dep.var.labels=c("Satisfaction with Life"), 
          covariate.labels=c("Age","Age Squared", "Intercept"),
          align=TRUE, out="Output/Tables/regression_table.tex")

# Text Version
stargazer(reg1, reg2, type="text", title="Results of the Two Regressions, In Text Form", 
          dep.var.labels=c("Satisfaction with Life"),
          align=TRUE, out="Output/Tables/regression_table.txt")


# ____________________________________________________
#TEXT ON PAGE 4 OF THE PAPER, STATING THAT,
#WHEN COUNTRY FIXED EFFECTS ARE NOT INCLUDED IN THE REGRESSION,
#ESTIMATED AGE AT MINIMUM SWB IS ABOUT
#47 YEARS AND 9 MONTHS


# Generate the number in the text, which states the age at minimum satisfaction
reg1agesq <- reg1[["coefficients"]][["age2"]]
reg1age <- reg1[["coefficients"]][["age"]]

minsatis1=-1*reg1age/(2*reg1agesq)

#Comment: ....write result to output file....
write.table(minsatis1, file = "Output/InText/intext1.txt", sep = "\t", row.names = F)



# ____________________________________________________
#TEXT ON PAGE 4 OF THE PAPER, STATING THAT,
#WHEN COUNTRY FIXED EFFECTS ARE INCLUDED IN THE REGRESSION,
#ESTIMATED AGE AT MINIMUM SWB IS ABOUT
#53 YEARS AND 11 MONTHS


# Generate the number in the text, which states the age at minimum satisfaction, accounting for country level variables
reg2agesq <- reg2[["coefficients"]][["age2"]]
reg2age <- reg2[["coefficients"]][["age"]]

minsatis2=-1*reg2age/(2*reg2agesq)

#Comment: ..write to file...
write.table(minsatis2, file = "Output/InText/intext2.txt", sep = "\t", row.names = F)


# ____________________________________________________
# Generation of Figures

# Figure 1 (GDP Per Capita vs Satisfaction)
ggplot(data = means_table, aes(x = gdp_pc, y = mean_satis)) + 
  geom_point() + 
  geom_text(aes(label = country), vjust = "inward", hjust = "inward") + 
  xlab("Mean GDP Per Capita") + 
  ylab("Country mean satisfaction") +
  ggtitle("GDP Per Capita vs Mean Satisfaction by Country")

ggsave('Output/Figures/Figure1.png')


# Figure 2 (Expenditure vs Satisfaction)
ggplot(data = means_table, aes(x = gov_cons, y = mean_satis)) +
  geom_point() +
  geom_text(aes(label = country), vjust = "inward", hjust = "inward") + 
  xlab("Government Consumption as Percent of GDP") + 
  ylab("Country mean satisfaction") +
  ggtitle("Government Expenditure vs Mean Satisfaction by Country")

ggsave('Output/Figures/Figure2.png')



# ____________________________________________________
# End of File