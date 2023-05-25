# Created by Houman Azizi
# Combines All Wide PPMI Files

PPMI_Wide_to_Cleaned <- function(folder_path) {
  
  setwd(paste0(folder_path,"Data_Wide/"))
  
  
  
  ######## OLDER REMOVE DUPLICATE VERSION ########
  # # getting only the problematic rows -> fixing them
  # PPMI_cor <- PPMI %>% group_by(Patient_Number, Visit_ID) %>% 
  #   filter(n() > 1) %>% arrange(Patient_Number,Visit_Date_asDate) %>% 
  #   mutate(across(c(NUPDR3OF:NP1RTOT), sum_)) %>% 
  #   mutate(remove = case_when(n() > 1 & row_number()==2 ~ TRUE)) %>% 
  #   filter(is.na(remove))
  # PPMI_cor <- PPMI_cor[,-length(PPMI_cor)]
  # # removing problematic rows from main df -> adding corrected rows -> rearrange
  # PPMI <- PPMI %>% filter(!(paste0(Patient_Number,Visit_ID) %in% paste0(PPMI_cor$Patient_Number,PPMI_cor$Visit_ID)))
  # PPMI <- rbind(PPMI,PPMI_cor) %>% arrange(Patient_Number,Visit_Date_asDate)
  # rm(list=(ls()[ls()!="PPMI"]))
  # cat("\014")
  
  
  
  
  
  ######## LOAD THIS FUNCTION FIRST ########
  # Each row should be a unique single visit of a participant
  # This function removes duplicate rows -> saves a _Delay_Days variable showing the difference between a specific test administration date and actual visit's day
  removeDuplicates <- function(ppmi_func,filename) {
    
    PPMI_temp <- ppmi_func %>% group_by(Patient_Number, Visit_ID) %>% 
      filter(n() > 1) %>% arrange(Patient_Number, Visit_ID, Visit_Date_asDate) 
    PPMI_cor <- data.frame(matrix(nrow = 0, ncol = length(PPMI_temp)+1))
    colnames(PPMI_cor) <- c(colnames(PPMI_temp), paste0(filename,"_Delay_Days"))
    for (i in 1:(dim(PPMI_temp)[1]/2)) {
      temp <- rbind(PPMI_temp[(i*2)-1,1:length(PPMI_temp)],PPMI_temp[i*2,1:length(PPMI_temp)])
      PPMI_cor[i,1:length(PPMI_temp)] <- setDT(temp)[, lapply(.SD,na.omit)][1] #these 2 lines do the merge for the rows existing in temp
      
      PPMI_cor[i,length(PPMI_cor)] <- PPMI_temp$Visit_Date_asDate[(i*2)]
    }
    # removing problematic rows from main df -> adding corrected rows -> rearrange
    ppmi_func <- ppmi_func %>% filter(!(paste0(Patient_Number,Visit_ID) %in% paste0(PPMI_cor$Patient_Number,PPMI_cor$Visit_ID)))
    ppmi_func[,paste0(filename,"_Delay_Days")] <- NA
    ppmi_func <- rbind(ppmi_func,PPMI_cor) %>% arrange(Patient_Number,Visit_Date_asDate)
    
    return(ppmi_func)
  }
  
  
  
  
  
  
  
  ################ GOING THROUGH WIDE FILES ################
  
  ######## MDS_UPDRS_Part_III.csv as Initial File ########
  PPMI <- read.csv("MDS_UPDRS_Part_III_wide.csv", sep=",", header = T)
  PPMI$UPDRS3_Delay_Days <- PPMI$Visit_Date_asDate
  
  
  
  
  
  ######## MDS-UPDRS_Part_I.csv ########
  newFile <- read.csv("MDS-UPDRS_Part_I_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  ## CLEANING DUPLICATES ##
  PPMI <- removeDuplicates(PPMI,"UPDRS1")
  
  
  
  
  ######## MDS-UPDRS_Part_I_Patient_Questionnaire.csv ########
  # newFile <- read.csv("MDS-UPDRS_Part_I_Patient_Questionnaire_wide.csv", sep=",", header = T)
  # PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  
  
  
  
  ######## MDS-MDS_UPDRS_Part_II__Patient_Questionnaire.csv ########
  newFile <- read.csv("MDS_UPDRS_Part_II__Patient_Questionnaire_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"UPDRS2")
  
  
  
  
  ######## MDS-UPDRS_Part_IV__Motor_Complications.csv ########
  newFile <- read.csv("MDS-UPDRS_Part_IV__Motor_Complications_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"UPDRS4")
  
  
  
  
  ######## Montreal_Cognitive_Assessment__MoCA_.csv ########
  newFile <- read.csv("Montreal_Cognitive_Assessment__MoCA_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"MOCA")
  
  
  
  
  ######## Benton_Judgement_of_Line_Orientation.csv ########
  newFile <- read.csv("Benton_Judgement_of_Line_Orientation_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"JLO")
  
  
  
  
  ######## Cognitive_Categorization.csv ########
  newFile <- read.csv("Cognitive_Categorization_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"COG")
  
  
  
  
  ######## Epworth_Sleepiness_Scale.csv ########
  newFile <- read.csv("Epworth_Sleepiness_Scale_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"ESS")
  
  
  
  
  ######## Geriatric_Depression_Scale__Short_Version_.csv ########
  newFile <- read.csv("Geriatric_Depression_Scale__Short_Version_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"GDS")
  
  
  
  
  ######## Hopkins_Verbal_Learning_Test_-_Revised_wide.csv ########
  newFile <- read.csv("Hopkins_Verbal_Learning_Test_-_Revised_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"DVT")
  
  
  
  
  ######## Letter_-_Number_Sequencing_wide.csv ########
  newFile <- read.csv("Letter_-_Number_Sequencing_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"LNS")
  
  
  
  
  ######## Lumbar_Puncture_wide.csv ########
  newFile <- read.csv("Lumbar_Puncture_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"Lumbar")
  
  
  
  
  ######## Modified_Schwab___England_Activities_of_Daily_Living_wide.csv ########
  newFile <- read.csv("Modified_Schwab___England_Activities_of_Daily_Living_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"MSE")
  
  
  
  
  ######## QUIP-Current-Short_wide.csv ########
  newFile <- read.csv("QUIP-Current-Short_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"QUIP")
  
  
  
  
  ######## REM_Sleep_Behavior_Disorder_Questionnaire_wide.csv ########
  newFile <- read.csv("REM_Sleep_Behavior_Disorder_Questionnaire_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"REM")
  
  
  
  
  ######## SCOPA-AUT_wide.csv ########
  newFile <- read.csv("SCOPA-AUT_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"SCOPA")
  
  
  
  
  ######## Modified_Semantic_Fluency_wide.csv ########
  newFile <- read.csv("Modified_Semantic_Fluency_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"SFT")
  
  
  
  
  ######## State-Trait_Anxiety_Inventory_wide.csv ########
  newFile <- read.csv("State-Trait_Anxiety_Inventory_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"STAI")
  
  
  
  
  ######## Symbol_Digit_Modalities_Test_wide.csv ########
  newFile <- read.csv("Symbol_Digit_Modalities_Test_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"SDM")
  
  
  
  
  ######## University_of_Pennsylvania_Smell_Identification_Test__UPSIT_wide.csv ########
  newFile <- read.csv("University_of_Pennsylvania_Smell_Identification_Test__UPSIT_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"UPSIT")
  
  ######## Vital_Signs_wide.csv ########
  newFile <- read.csv("Vital_Signs_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"Vital")
  
  ######## Blood_Chemistry___Hematology_wide.csv ########
  newFile <- read.csv("Blood_Chemistry___Hematology_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date"="Blood_Collection_Date","Visit_Date_asDate"="Blood_Collection_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"Blood")
  
  
  
  ######## Skin_Biopsy_wide.csv ########
  newFile <- read.csv("Skin_Biopsy_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"Skin")
  
  
  
  
  ######## Determination_of_Freezing_and_Falls_wide.csv ########
  newFile <- read.csv("Determination_of_Freezing_and_Falls_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"FreezFall")
  
  
  
  
  ######## Clock_Drawing_wide.csv ########
  newFile <- read.csv("Clock_Drawing_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% full_join(newFile, by = c("Patient_Number","Visit_ID","Visit_Date","Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  PPMI <- removeDuplicates(PPMI,"CLC")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################  Changing _Delay_Days Columns from Dates to Number of Days ################################
  
  #colnames(PPMI)[str_sub(colnames(PPMI),-3,-1)=="ays"]
  
  PPMI <- PPMI %>% mutate(UPDRS1_Delay_Days = as.integer(difftime(UPDRS1_Delay_Days, Visit_Date_asDate, units = "days")),
                          UPDRS2_Delay_Days = as.integer(difftime(UPDRS2_Delay_Days, Visit_Date_asDate, units = "days")),
                          UPDRS3_Delay_Days = as.integer(difftime(UPDRS3_Delay_Days, Visit_Date_asDate, units = "days")),
                          UPDRS4_Delay_Days = as.integer(difftime(UPDRS4_Delay_Days, Visit_Date_asDate, units = "days")),
                          MOCA_Delay_Days = as.integer(difftime(MOCA_Delay_Days, Visit_Date_asDate, units = "days")),
                          JLO_Delay_Days = as.integer(difftime(JLO_Delay_Days, Visit_Date_asDate, units = "days")),
                          COG_Delay_Days = as.integer(difftime(COG_Delay_Days, Visit_Date_asDate, units = "days")),
                          ESS_Delay_Days = as.integer(difftime(ESS_Delay_Days, Visit_Date_asDate, units = "days")),
                          GDS_Delay_Days = as.integer(difftime(GDS_Delay_Days, Visit_Date_asDate, units = "days")),
                          DVT_Delay_Days = as.integer(difftime(DVT_Delay_Days, Visit_Date_asDate, units = "days")),
                          LNS_Delay_Days = as.integer(difftime(LNS_Delay_Days, Visit_Date_asDate, units = "days")),
                          Lumbar_Delay_Days = as.integer(difftime(Lumbar_Delay_Days, Visit_Date_asDate, units = "days")),
                          MSE_Delay_Days = as.integer(difftime(MSE_Delay_Days, Visit_Date_asDate, units = "days")),
                          QUIP_Delay_Days = as.integer(difftime(QUIP_Delay_Days, Visit_Date_asDate, units = "days")),
                          REM_Delay_Days = as.integer(difftime(REM_Delay_Days, Visit_Date_asDate, units = "days")),
                          SCOPA_Delay_Days = as.integer(difftime(SCOPA_Delay_Days, Visit_Date_asDate, units = "days")),
                          SFT_Delay_Days = as.integer(difftime(SFT_Delay_Days, Visit_Date_asDate, units = "days")),
                          STAI_Delay_Days = as.integer(difftime(STAI_Delay_Days, Visit_Date_asDate, units = "days")),
                          SDM_Delay_Days = as.integer(difftime(SDM_Delay_Days, Visit_Date_asDate, units = "days")),
                          UPSIT_Delay_Days = as.integer(difftime(UPSIT_Delay_Days, Visit_Date_asDate, units = "days")),
                          Vital_Delay_Days = as.integer(difftime(Vital_Delay_Days, Visit_Date_asDate, units = "days")),
                          Blood_Delay_Days = as.integer(difftime(Blood_Delay_Days, Visit_Date_asDate, units = "days")),
                          Skin_Delay_Days = as.integer(difftime(Skin_Delay_Days, Visit_Date_asDate, units = "days")),
                          FreezFall_Delay_Days = as.integer(difftime(FreezFall_Delay_Days, Visit_Date_asDate, units = "days")),
                          CLC_Delay_Days = as.integer(difftime(CLC_Delay_Days, Visit_Date_asDate, units = "days")))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################  Patient_Number BASED MERGES ################
  
  ######## Family_History.csv ########
  newFile <- read.csv("Family_History_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% left_join(newFile, by = c("Patient_Number")) %>% arrange(Patient_Number, Visit_Date_asDate)
  
  
  ######## PD_Diagnosis_History_wide.csv ########
  newFile <- read.csv("PD_Diagnosis_History_wide.csv", sep=",", header = T) %>% select(-c("Visit_ID", "Visit_Date", "Visit_Date_asDate"))
  PPMI <- PPMI %>% left_join(newFile, by = c("Patient_Number")) %>% arrange(Patient_Number, Visit_Date_asDate)
  
  
  ######## Socio-Economics_wide.csv ########
  newFile <- read.csv("Socio-Economics_wide.csv", sep=",", header = T) %>% filter(!is.na(EDUCYRS))
  PPMI <- PPMI %>% left_join(newFile, by = c("Patient_Number","Visit_ID", "Visit_Date", "Visit_Date_asDate")) %>% arrange(Patient_Number, Visit_Date_asDate)
  for (i in 2:nrow(PPMI)) {
    if (is.na(PPMI$EDUCYRS[i]) && PPMI$Patient_Number[i]==PPMI$Patient_Number[i-1]) {
      PPMI$EDUCYRS[i] <- PPMI$EDUCYRS[i-1]
    }
  }
  for (i in (nrow(PPMI)-1):1) {
    if (is.na(PPMI$EDUCYRS[i]) && PPMI$Patient_Number[i]==PPMI$Patient_Number[i+1]) {
      PPMI$EDUCYRS[i] <- PPMI$EDUCYRS[i+1]
    }
  }
  
  
  ######## Prodromal_History_wide.csv ########
  newFile <- read.csv("Prodromal_History_wide.csv", sep=",", header = T) %>% select(-c("Visit_ID", "Visit_Date", "Visit_Date_asDate"))
  PPMI <- PPMI %>% left_join(newFile, by = c("Patient_Number")) %>% arrange(Patient_Number, Visit_Date_asDate)
  
  ######## Demographics_wide.csv ########
  newFile <- read.csv("Demographics_wide.csv", sep=",", header = T) %>% select(-c("Visit_ID", "Visit_Date", "Visit_Date_asDate"))
  PPMI <- PPMI %>% left_join(newFile, by = c("Patient_Number")) %>% arrange(Patient_Number, Visit_Date_asDate)
  
  ######## Participant_Status_wide.csv ########
  newFile <- read.csv("Participant_Status_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% left_join(newFile, by = c("Patient_Number")) %>% arrange(Patient_Number, Visit_Date_asDate)
  
  
  rm(list=(ls()[ls()!="PPMI"]))
  cat("\014")
  
  
  
  
  
  
  
  
  
  
  
  
  ################  Adding Extra Columns ################
  
  # Add age_at_visit
  PPMI <- PPMI %>% mutate(Age_at_Visit = round(as.double(difftime(Visit_Date_asDate,BIRTHDT_asDate, units = "days")/365), digits = 1)) %>% 
    relocate(Age_at_Visit, .after = Visit_Date_asDate) %>% arrange(Patient_Number,Visit_Date_asDate)
  
  # Add baseline date
  onlyBL <- PPMI %>% filter(Visit_ID == "BL") %>% arrange(Patient_Number,Visit_Date_asDate) %>%
    select("Patient_Number","Visit_Date_asDate","Age_at_Visit") %>% mutate(trueBL = duplicated(Patient_Number)) %>%
    filter(trueBL == FALSE) %>% mutate(BL_Date = Visit_Date_asDate) %>% mutate(BL_Age = Age_at_Visit) %>%
    select("Patient_Number","BL_Date","BL_Age") #finding true BL dates for each subject
  PPMI <- PPMI %>% full_join(onlyBL, by = "Patient_Number") %>% relocate(BL_Date, .after = Age_at_Visit) %>%
    relocate(BL_Age, .after = BL_Date)
  
  # Add each visits date from BL in days
  PPMI <- PPMI %>% mutate(Days_from_BL = as.integer(difftime(Visit_Date_asDate,BL_Date, units = "days"))) %>%
    relocate(Days_from_BL, .after = BL_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  
  
  
  rm(list=(ls()[ls()!="PPMI"]))
  cat("\014")
  
  
  
  
  ################ Merge all SCs into BLs for all files ################
  PPMI$SC_Delay_Days <- NA
  beh_BLSC <- PPMI %>% filter(Visit_ID == "BL" | Visit_ID == "SC") %>% 
    group_by(Patient_Number) %>% filter(n() > 1) %>% arrange(Patient_Number,Visit_ID)
  uniqueIDs <- unique(beh_BLSC$Patient_Number)
  
  # merging & saving days difference between SC and BL
  beh_BL <- data.frame(matrix(nrow = length(uniqueIDs), ncol = ncol(beh_BLSC)))
  colnames(beh_BL) <- colnames(beh_BLSC)
  for (i in 1:length(uniqueIDs)) {
    beh_BLSC$SC_Delay_Days[(i*2)-1] <- beh_BLSC$Days_from_BL[i*2]
    
    temp <- rbind(beh_BLSC[(i*2)-1,],beh_BLSC[i*2,])
    beh_BL[i,] <- setDT(temp)[, lapply(.SD,na.omit)][1]
  }
  # replacing BLs instead of BL/SCs
  PPMI <- PPMI %>% filter(!(paste0(Patient_Number,Visit_ID) %in% paste0(beh_BLSC$Patient_Number,beh_BLSC$Visit_ID)))
  PPMI <- rbind(PPMI,beh_BL) %>% arrange(Patient_Number,Visit_Date_asDate)
  
  
  
  # changing SC to BL for participants who only have a single SC visit
  PPMI$Visit_ID[PPMI$Visit_ID == "SC"] <- "BL"
  
  
  # adding sex as a word as well
  PPMI <- PPMI %>% mutate(SEX_n = SEX) %>% mutate(SEX = case_when(SEX_n == 0 ~ "Female",
                                                                  SEX_n == 1 ~ "Male"))
  
  
  # Save the output
  PPMI_regular <- PPMI %>% filter(Visit_ID == "SC" | Visit_ID == "BL" | substr(Visit_ID,1,1) == "V")
  PPMI_other <- PPMI %>% filter(!(Visit_ID == "SC" | Visit_ID == "BL" | substr(Visit_ID,1,1) == "V"))
  
  
  write.csv(PPMI,paste0(folder_path,"PPMI_allVisits_cleaned.csv"), row.names=FALSE)
  write.csv(PPMI_regular,paste0(folder_path,"PPMI_regularVisits_cleaned.csv"), row.names=FALSE)
  write.csv(PPMI_other,paste0(folder_path,"PPMI_extraVisits_cleaned.csv"), row.names=FALSE)
  
  
  
  
  
  return("COMPLETED STEP 2: Wide Bheavioral PPMI data Cleaned")
}