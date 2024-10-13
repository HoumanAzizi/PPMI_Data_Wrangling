PPMI_ALL_Merge_Final <- function(folder_path) {
  
  setwd(folder_path)
  
  PPMI <- read.csv("PPMI_allVisits_cleaned_processed.csv", sep=",", header = T)
  imaging <- read.csv("PPMI_Imaging_All_cleaned_processed.csv", sep=",", header = T)
  bio <- read.csv("PPMI_Bio_cleaned_processed.csv", sep=",", header = T)
  MRI <- read.csv("PPMI_07Dec2023_MRI_List.csv", sep=",", header = F)
  
  
  # Fix MRI file
  MRI$MRI_date <- as.character(as.Date(as.character(MRI$V2), format = "%Y%m%d"))
  MRI <- MRI %>% select(-V2)
  colnames(MRI) <- c('Patient_ID', 'MRI_Date')
  
  
  ######## Matching MRI visits to regular visit dates ########
  MRI$Visit_ID_Match <- NA
  MRI$Visit_Date_Match <- NA
  
  # find the closet visit_id from the main PPMI
  for (i in 1:nrow(MRI)) {
    if (MRI$Patient_ID[i] %in% PPMI$Patient_ID) {
      closest_date <- PPMI %>% filter(Patient_ID == MRI$Patient_ID[i]) %>% 
        mutate(tempDayDiff = abs(as.integer(difftime(Visit_Date, MRI$MRI_Date[i])))) %>% 
        slice(which.min(tempDayDiff))
      MRI$Visit_ID_Match[i] <- closest_date$Visit_ID
      MRI$Visit_Date_Match[i] <- closest_date$Visit_Date
    }
  }
  MRI <- MRI[complete.cases(MRI),]
  MRI <- MRI %>% rowwise() %>%  mutate(DayDiff_MRI = as.integer(difftime(MRI_Date, Visit_Date_Match, units = "days")))
                                               
  
  
  ######## Add bio, MRI, PET ########
  # PET <- imaging %>% select("Patient_ID", "Visit_ID", "DayDiff_PET", "AV133_RCAUD_S", "AV133_RPUTANT_S", 
  #                           "AV133_RPUTPOST_S", "AV133_LCAUD_S", "AV133_LPUTANT_S", "AV133_LPUTPOST_S" ) %>% filter(!is.na(DayDiff_PET))
  MRI <- MRI %>% select('Patient_ID', 'Visit_ID_Match', 'MRI_Date', 'DayDiff_MRI')
  DAT <- imaging %>% select(Patient_ID, Visit_ID, DayDiff_DAT, starts_with('DAT')) %>% filter(!is.na(DayDiff_DAT))
  colnames(MRI) <- c('Patient_ID', 'Visit_ID', 'MRI_Scan_Date', 'DayDiff_MRI')
  
  
  PPMI <- PPMI %>% left_join(MRI, by = c("Patient_ID", "Visit_ID"))
  # PPMI <- PPMI %>% left_join(PET, by = c("Patient_ID", "Visit_ID"))
  PPMI <- PPMI %>% left_join(DAT, by = c("Patient_ID", "Visit_ID"))
  PPMI <- PPMI %>% left_join(bio, by = c("Patient_ID", "Visit_ID"))
  
  
  # Fix a column name problem
  colnames(PPMI)[which(colnames(PPMI) == 'Cognitive_Diagnosis_Confidence.level')] <- 'Cognitive_Diagnosis_Confidence_Level'
  
  # Save
  PPMI <- PPMI %>% mutate(MRI_Scan_Date_n = gsub("-", "", MRI_Scan_Date)) %>% relocate(MRI_Scan_Date_n, .after = MRI_Scan_Date)
  write.csv(PPMI,paste0(folder_path,"PPMI_Merged_AllMeasures_AllVisits_FINAL.csv"), row.names=FALSE)
  
  # Save a MATLAB friendly version
  PPMI_matlab <- PPMI
  PPMI_matlab[is.na(PPMI_matlab)] <- NaN
  write.csv(PPMI,paste0(folder_path,"PPMI_Merged_AllMeasures_AllVisits_FINAL_vMATLAB.csv"), row.names=FALSE)
  
  return("COMPLETED FINAL STEP: Merged ALL the data together - Final filename: PPMI_Merged_AllMeasures_AllVisits_FINAL.csv")
  
}





