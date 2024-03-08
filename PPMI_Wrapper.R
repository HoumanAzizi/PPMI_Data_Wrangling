# by Houman Azizi

# Libraries to load
#install.packages("devtools")
#devtools::install_github("davidsjoberg/hablar")
library(tidyr)
library(dplyr)
library(reshape2)
library(hablar)
library(data.table)
library(stringr)
library(readr)

rm(list = ls())
cat("\014")


####################### INPUT FOLDER PATH ####################### 
# Create a new folder -> put the following 11 or 12 files/scripts inside it
#     PPMI_Wide_to_Cleaned.R
#     PPMI_Wide_to_Cleaned_Imaging.R
#     PPMI_Cleaned_to_Processed.R
#     PPMI_Wide_to_Processed_Bio.R
#     PPMI_Cleaned_to_Processed_Imaging.R
#     PPMI_Wrapper.R
#     PPMI_Merge_Final.R
#     PPMI_Raw_to_Wide.R
#     UPSIT_to_percentile.R
#     UPSIT_Normative_Values_female.csv 
#     UPSIT_Normative_Values_male.csv
#     OPTIONAL -> PPMI_CNO.csv
#           if you want to add CNO, create a file with unique patient IDs as column 1 and CNO numbers as column 2
#           if CNO is not needed, comment the "Add CNOs" section out in the PPMI_Cleaned_to_Processed.R file

# Show the path to this new folder below. Use '/' at the end of the path. (e.g. /home/PPMI_Folder/)
folder_path <- "/PATH/TO/SCRIPTS_FOLDER/"
# Show the path to raw data file. Use '/' at the end of the path. (e.g. /home/PPMI_Raw_Data/)
raw_path <- "/PATH/TO/RAW_DATA_FOLDER/"
# Add the date of raw data download -> presented at the end of each downloaded file (ex. "01Jun2023")
download_date <- "RAW_DATA_DOWNLOAD_DATE"

setwd(folder_path)








# Loading Scripts
source("PPMI_Raw_to_Wide.R")
source("PPMI_Wide_to_Cleaned.R")
source("PPMI_Wide_to_Cleaned_Imaging.R")
source("PPMI_Wide_to_Processed_Bio.R")
source("PPMI_Cleaned_to_Processed.R")
source("PPMI_Cleaned_to_Processed_Imaging.R")
source("PPMI_Merge_Final.R")
source("PPMI_AllMeasure_Merge.R")










####################### Data Wrangling #######################
# Running scripts back to back - outputs a confirmation after each step

# Gets raw PPMI data -> turns into Wide versions using desired variables
result1 <- PPMI_Raw_to_Wide(folder_path, raw_path, download_date)
print(result1)

# Combines all Wide files -> makes sure each row is a unique visit of a participant
result2 <- PPMI_Wide_to_Cleaned(folder_path)
print(result2)

# Combines all Wide Imaging files
# Merges BL and SC visits -> marks all of them as BL
# Find Imaging date difference from its corresponding behavioral visit
result3 <- PPMI_Wide_to_Cleaned_Imaging(folder_path)
print(result3)

# Creates a final Bio file
# Merges BL and SC visits as well
result4 <- PPMI_Wide_to_Processed_Bio(folder_path)
print(result4)

# Creates a final processed file with behavioral data
# Makes extra calculations, removes extra columns, rename columns, adds CNO
result5 <- PPMI_Cleaned_to_Processed(folder_path)
print(result5)

# For Imaging data, matches scan dates with regular visit dates + change column names
result6 <- PPMI_Cleaned_to_Processed_Imaging(folder_path)
print(result6)

# Merges processed behavioral and imaging data
# Produces 3 final files to use: PPMI_Merged_FINAL_AllVisits.csv, PPMI_Merged_FINAL_RegularVisits.csv, PPMI_Merged_FINAL_ExtraVisits.csv
result7 <- PPMI_FinalMerge(folder_path)
print(result7)

# Merges ALL the files to be final
# Produces a final file to use: PPMI_Merged_AllMeasures_AllVisits_FINAL.csv
result8 <- PPMI_ALL_Merge_Final(folder_path)
print(result8)


