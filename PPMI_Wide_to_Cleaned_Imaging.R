# Created by Houman Azizi
# Combines All Wide PPMI Files -> Imaging Data


PPMI_Wide_to_Cleaned_Imaging <- function(folder_path) {
  
  setwd(paste0(folder_path,"Data_Wide/"))
  
  
  
  ######## Magnetic_Resonance_Imaging__MRI_wide.csv as Initial File ########
  PPMI <- read.csv("Magnetic_Resonance_Imaging__MRI_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% filter(MRI_Completed==1)
  
  
  
  ######## MRI_Metadata_wide.csv ########
  newFile <- read.csv("MRI_Metadata_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% left_join(newFile, by = c("Patient_Number","MRI_Scan_Date"="MRI_Scan_Date_Metadata","MRI_Scan_Date_asDate"="MRI_Scan_Date_Metadata_asDate")) %>% arrange(Patient_Number, MRI_Scan_Date_asDate)
  
  
  
  
  ######## DaTScan_Analysis_wide.csv ########
  newFile <- read.csv("DaTScan_Analysis_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID")) %>% arrange(Patient_Number, Visit_ID)
  
  
  
  
  ######## DaTScan_Metadata_wide.csv ########
  newFile <- read.csv("DaTScan_Metadata_wide.csv", sep=",", header = T) %>% select(-c(DATSCAN_LIGAND,DAT_Scan_Date_Metadata,DAT_Scan_Date_Metadata_asDate)) #it's a duplicate column from previous file
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID")) %>% arrange(Patient_Number, Visit_ID)
  
  
  
  
  ######## AV-133_PET_Analysis_wide.csv ########
  newFile <- read.csv("AV-133_PET_Analysis_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID")) %>% arrange(Patient_Number, Visit_ID)
  
  
  
  rm(list=(ls()[ls()!="PPMI"]))
  cat("\014")
  
  
  
  
  
  
  
  
  ################ Merge all SCs into BLs for all files ################
  img_BLSC <- PPMI %>% filter(Visit_ID == "BL" | Visit_ID == "SC") %>%
    group_by(Patient_Number) %>% filter(n() > 1) %>% arrange(Patient_Number,Visit_ID)
  uniqueIDs <- unique(img_BLSC$Patient_Number)
  
  # merging & saving days difference between SC and BL
  img_BL <- data.frame(matrix(nrow = length(uniqueIDs), ncol = ncol(img_BLSC)))
  colnames(img_BL) <- colnames(img_BLSC)
  for (i in 1:length(uniqueIDs)) {
    temp <- rbind(img_BLSC[(i*2)-1,],img_BLSC[i*2,])
    img_BL[i,] <- setDT(temp)[, lapply(.SD,na.omit)][1]
  }
  # replacing BLs instead of BL/SCs
  PPMI <- PPMI %>% filter(!(paste0(Patient_Number,Visit_ID) %in% paste0(img_BLSC$Patient_Number,img_BLSC$Visit_ID)))
  PPMI <- as.data.frame(rbind(PPMI,img_BL)) %>% arrange(Patient_Number,Visit_ID)
  
  
  
  # changing SC to BL for participants who only have a single SC visit
  PPMI$Visit_ID[PPMI$Visit_ID == "SC"] <- "BL"
  
  
  
  
  
  
  
  ######## Find date_difference of each imaging from the date mentioned in behavioral file ########
  beh <- read.csv("../PPMI_allVisits_cleaned.csv", sep=",", header = T) %>% select(Patient_Number, Visit_ID, Visit_Date_asDate)
  PPMI <- PPMI %>% left_join(beh, by = c("Patient_Number","Visit_ID")) %>% arrange(Patient_Number, Visit_ID)
  colnames(PPMI)[length(PPMI)] <- "Behavioural_Visit_Date_asDate"
  PPMI <- PPMI %>% relocate(Behavioural_Visit_Date_asDate, .after = Visit_ID)
  
  
  PPMI <- PPMI %>% rowwise() %>%  mutate(MRI_Delay_Days = as.integer(difftime(MRI_Scan_Date_asDate, Behavioural_Visit_Date_asDate)),
                                         DAT_Delay_Days = as.integer(difftime(DAT_Scan_Date_asDate, Behavioural_Visit_Date_asDate)),
                                         PET_Delay_Days = as.integer(difftime(PET_Scan_Date_asDate, Behavioural_Visit_Date_asDate))) %>%
    relocate(MRI_Delay_Days, .after = MRI_Scan_Date_asDate) %>%
    relocate(DAT_Delay_Days, .after = DAT_Scan_Date_asDate) %>%
    relocate(PET_Delay_Days, .after = PET_Scan_Date_asDate)
  
  
  
  # change Patient_Number to Patient_ID
  colnames(PPMI)[1] <- "Patient_ID"
  # change Behavioural_Visit_Date_asDate to Visit_Date
  colnames(PPMI)[3] <- "Visit_Date"
  
  
  
  ######## Save the original imaging file without matching dates to main visits ########
  
  
  write.csv(PPMI,paste0(folder_path,"PPMI_Imaging_cleaned.csv"), row.names=FALSE)
  
  
  
 
  return("COMPLETED STEP 3: Wide Imaging PPMI data Cleaned")
}