# PPMI_Data_Wrangling

## Overview
Gets raw PPMI data -> outputs processed/cleaned versions

## Required Libraries
library(tidyr)
library(dplyr)
library(reshape2)
library(hablar)
library(data.table)
library(stringr)


<br/><br/>
<br/><br/>
# Steps
## Step 1: Createa new folder & put the following 11 or 12 files/scripts inside it
1. PPMI_Wide_to_Cleaned.R
2. PPMI_Wide_to_Cleaned_Imaging.R
3. PPMI_Cleaned_to_Processed.R
4. PPMI_Wide_to_Processed_Bio.R 
5. PPMI_Cleaned_to_Processed_Imaging.R
6. PPMI_Wrapper.R
7. PPMI_Merge_Final.R
8. PPMI_Raw_to_Wide.R
9. UPSIT_to_percentile.R
10. UPSIT_Normative_Values_female.csv 
11. UPSIT_Normative_Values_male.csv
12. OPTIONAL: PPMI_CNO.csv
        if you want to add CNO, create a file with unique patient IDs as column 1 and CNO numbers as column 2
        if CNO is not needed, comment the "Add CNOs" section out in the PPMI_Cleaned_to_Processed.R file

## Step 2: Run PPMI_Wrapper.R


<br/><br/>
<br/><br/>
# Final Useful Files
## Behavioral
PPMI_allVisits_cleaned_processed.csv
PPMI_extraVisits_cleaned_processed.csv
PPMI_regularVisits_cleaned_processed.csv
NOTE: Regular Visit include BL and Uxx visits. Extra Visit included everything in All Visit except the regular Visit ones.

## Imaging
PPMI_Imaging_All_cleaned_processed.csv
PPMI_Imaging_Main_cleaned_processed.csv
NOTE: "PPMI_Imaging_Main_cleaned_processed.csv" has the imaging information that is used in "Merged_Final" files"

## Behavioral + Imaging
PPMI_Merged_FINAL_AllVisits.csv
PPMI_Merged_FINAL_ExtraVisits.csv
PPMI_Merged_FINAL_RegularVisits.csv
NOTE: Regular Visit include BL and Uxx visits. Extra Visit included everything in All Visit except the regular Visit ones.

## Bio
PPMI_Bio_cleaned_processed.csv
