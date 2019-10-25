##############################################################################
# READ ME
# BEFORE EXECUTING THIS SCRIPT, BE SURE THAT:

###1) The folder MLC-Demo-RScripts/, and all of its subfolders and contents
###### (as described in ReadMe.pdf), are installed on your computer.

###2) The working directory for this R session is set to the MLC-Demo-RScripts/
###### folder.
##############################################################################


# This R-script is the master script for the Midlift Crisis Demo C&P
# It executes the scripts 1-Processing.R, 2-Merging.R, 
# 3-DataAppendix.R, and 4-Analysis.R in sequential order.


# ____________________________________________________
# Clear plots, the console, and the R workspace

# Clear the plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clear workspace
rm(list=ls())


# ____________________________________________________
# Call 1-Processing.R.
# Executing 1-Processing.R opens the original datasets downloaded from the PEW,
# as well as World Bank, then processes them and saves them as separate analysis files
# in the Temp/ folder.

source('CommandFiles/1-Processing.R', local = FALSE)


# ____________________________________________________
# Call 2-Merging.R
# 2-Merging.R opens the analysis data pew_analysis.csv and wdi_analysis.csv from the
# /Temp subfolder which was created by 1-Processing.R, 
# merges them together to form the analysis date file Analysis.csv needed
# for the analysis and data appendix, and saves it in the Analysis-Data/ subfolder
source('CommandFiles/2-Merging.R', local = FALSE)


# ____________________________________________________
# Call 3-DataAppenedix.R.
# 3-DataAppenedix.R opens the analysis data analysis.csv, 
# generates the graphs required for the data appendix portion of the exercise
# and stores them in the DataAppendix/ subfolder of the Output/.
source('CommandFiles/3-DataAppendix.R', local = FALSE)


# ____________________________________________________
# Call 4-Analysis.R
# 4-Analysis.R opens the analysis data analysis.csv,
# generates the graphs required for the analysis portion of the exercise,
# and stores them in the Figures/ and Tables/, and InText/ 
# subfolders of the Output/ folder.
source('CommandFiles/4-Analysis.R', local = FALSE)


# End of file