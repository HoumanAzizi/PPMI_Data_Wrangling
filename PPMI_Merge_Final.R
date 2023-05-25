# by Houman Azizi
# Merging processed behavioral and imaging data

PPMI_FinalMerge <- function(folder_path) {
  
  setwd(folder_path)
  
  Imaging <- read.csv("PPMI_Imaging_Main_cleaned_processed.csv", sep=",", header = T)
  PPMI <- read.csv("PPMI_allVisits_cleaned_processed.csv", sep=",", header = T)
  PPMI_Regular <- read.csv("PPMI_regularVisits_cleaned_processed.csv", sep=",", header = T)
  PPMI_Extra <- read.csv("PPMI_extraVisits_cleaned_processed.csv", sep=",", header = T)
  
  
  Imaging <- Imaging %>% select(-Visit_Date)
  
  all_visit <- PPMI %>% left_join(Imaging, by = c("Patient_ID","Visit_ID")) %>% arrange(Patient_ID, Visit_ID)
  regular_visit <- PPMI_Regular %>% left_join(Imaging, by = c("Patient_ID","Visit_ID")) %>% arrange(Patient_ID, Visit_ID)
  extra_visit <- PPMI_Extra %>% left_join(Imaging, by = c("Patient_ID","Visit_ID")) %>% arrange(Patient_ID, Visit_ID)
  
  write.csv(all_visit,paste0(folder_path,"PPMI_Merged_FINAL_AllVisits.csv"), row.names=FALSE)
  write.csv(regular_visit,paste0(folder_path,"PPMI_Merged_FINAL_RegularVisits.csv"), row.names=FALSE)
  write.csv(extra_visit,paste0(folder_path,"PPMI_Merged_FINAL_ExtraVisits.csv"), row.names=FALSE)
  
  
  
  return("COMPLETED FINAL STEP 7: Merged Processed Imaging and Behavioral Data - Final filename: PPMI_Merged_FINAL.csv")
}
  


