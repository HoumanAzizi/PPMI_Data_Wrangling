# Created by Houman Azizi
# Gets raw PPMI data and turns them into the wide version using only the desired variables
# Also does some calculations for some measures such as UPDRS3
# Does calculation of LDOPA and LEDD based on Script 7 of PPMI

PPMI_Raw_to_Wide <- function(folder_path, raw_path, download_date) {
  
  setwd(folder_path)
  dir.create("Data_Wide", recursive = TRUE)
  setwd(paste0(folder_path,"Data_Wide/"))
  
  ######## MDS_UPDRS_Part_III.csv ########
  UPDRS3 <- read.csv(paste0(raw_path,"MDS-UPDRS_Part_III_",download_date,".csv"), sep=",", header = T)
  # UPDRS3[,11:48] <- sapply(UPDRS3[,11:48],as.numeric) # new versions of PPMI raw data has fixed this problem
  #creating values for CleanedPPMI_to_Processed.R file
  UPDRS_PartIII = rowSums(UPDRS3[,c("NP3SPCH","NP3FACXP","NP3RIGN","NP3RIGRU","NP3RIGLU","NP3RIGRL","NP3RIGLL","NP3FTAPR","NP3FTAPL","NP3HMOVR","NP3HMOVL","NP3PRSPR",
                                    "NP3PRSPL","NP3TTAPR","NP3TTAPL","NP3LGAGR","NP3LGAGL","NP3RISNG","NP3GAIT","NP3FRZGT","NP3PSTBL","NP3POSTR","NP3BRADY","NP3PTRMR",
                                    "NP3PTRML","NP3KTRMR","NP3KTRML","NP3RTARU","NP3RTALU","NP3RTARL","NP3RTALL","NP3RTALJ","NP3RTCON")])
  #UPDRS_PartIII[is.na(UPDRS3$NP3TOT)] <- NA
  Tremor_score_3 = rowMeans(UPDRS3[,c("NP3PTRMR","NP3PTRML","NP3KTRMR","NP3KTRML","NP3RTARU","NP3RTALU","NP3RTARL","NP3RTALL","NP3RTALJ","NP3RTCON")],na.rm = TRUE)
  PIGD_score_3 = rowMeans(UPDRS3[,c("NP3GAIT","NP3FRZGT","NP3PSTBL")])
  UPDRS3$NP3TOT_Calculated <- UPDRS_PartIII
  UPDRS3$Tremor3_Calculated <- Tremor_score_3
  UPDRS3$PIGD3_Calculated <- PIGD_score_3
  
  # Creating UPDRS values based on regular, ON, OFF, and A UPDRS3 versions
  # Creating values for Left and Right sides
  # For new PPMI raw data: Use PDSTATE and Fix duplicate rows
  PPMI <- UPDRS3 %>% select("PATNO","EVENT_ID","INFODT","PAG_NAME","PDSTATE",'PDMEDYN', 'PDTRTMNT', 'HRPOSTMED', 'PDMEDTM', 'EXAMTM', "NP3TOT",
                            "NP3FTAPL","NP3FTAPR","NP3HMOVL","NP3HMOVR",
                            "NP3KTRML","NP3KTRMR","NP3LGAGL","NP3LGAGR","NP3PRSPL","NP3PRSPR","NP3PTRML","NP3PTRMR",
                            "NP3TTAPL","NP3TTAPR","NP3RTALU","NP3RTARU","NP3RTALL","NP3RTARL","NP3RIGLU","NP3RIGRU","NP3RIGLL","NP3RIGRL",
                            "NP3TOT_Calculated","Tremor3_Calculated","PIGD3_Calculated")
  ## Correcting the UPDRS PAG_NAME and PD States
  # NUPDRS3 is similar to NUPDRSDOSE3 -> ALL PAG_NAMEs will be NUPDRS3
  # NUPDR3OF and NUPDR3ON are regular NUPDRS3 with PDSTATE OFF and ON, respectively
  # Add HRPOSTMED when missing
  PPMI[PPMI == ''] <- NA
  PPMI[PPMI == 'NaN'] <- NA
  ## Fix NUPDR3OF and NUPDR3ON values -> create a new PDSTATE_Corrected
  PPMI <- PPMI %>% rowwise() %>% mutate(PDSTATE_Corrected = case_when(is.na(PDSTATE) && PAG_NAME=='NUPDR3OF' ~ 'OFF',
                                                                      is.na(PDSTATE) && PAG_NAME=='NUPDR3ON' ~ 'ON',
                                                                      TRUE ~ PDSTATE)) %>% relocate(PDSTATE_Corrected, .after = PDSTATE) %>% ungroup()
  ## Fix PDMEDYN that are NA
  # if that visit has PDSTATE_Corrected=='ON', it means they were medicated, so set to 1
  PPMI <- PPMI %>% group_by(PATNO, EVENT_ID) %>% mutate(PDMEDYN_Corrected = case_when(any(is.na(PDMEDYN) & PDSTATE_Corrected == 'ON') ~ 1,
                                                                                      TRUE ~ PDMEDYN)) %>% ungroup()
  # if taking medication (PDMEDYN_Corrected) is NA AND PDSTATE_Corrected is also NA, then participant is not under medication
  PPMI <- PPMI %>% rowwise() %>% mutate(PDMEDYN_Corrected = case_when(is.na(PDMEDYN_Corrected) && is.na(PDSTATE_Corrected) ~ 0,
                                                                      TRUE ~ PDMEDYN_Corrected)) %>% relocate(PDMEDYN_Corrected, .after = PDMEDYN) %>% ungroup()
  ## Fix PDSTATE_Corrected -> if no medication is taken (PDMEDYN_Corrected is 0), set PDSTATE_Corrected to No_Medication
  PPMI <- PPMI %>% rowwise() %>% mutate(PDSTATE_Corrected = case_when(is.na(PDSTATE_Corrected) & PDMEDYN_Corrected == 0 ~ 'No_Medication',
                                                                      TRUE ~ PDSTATE_Corrected)) %>% ungroup()
  # after this, PDSTATE_Corrected of NA means probably invalid rows
  PPMI <- PPMI %>% filter(!is.na(PDSTATE_Corrected))
  
  ## Removing duplicates
  # Old version: Merging PAG_NAME and PDSTATE
  # PPMI <- PPMI %>% mutate(PAG_NAME = paste0(PAG_NAME,PDSTATE))# %>% select(-"PDSTATE")
  # Removing Duplicates - method: arranging and taking the column with available HRPOSTMED; then taking first column among identicals
  # PPMI <- PPMI %>% mutate(unique_id = paste0(PATNO, EVENT_ID, PAG_NAME)) # Old version
  PPMI <- PPMI %>% mutate(unique_id = paste0(PATNO, EVENT_ID, PDSTATE_Corrected))
  dups <- PPMI %>% group_by(unique_id) %>% mutate(count = n()) %>% 
    filter(count > 1) %>% select(-"count") %>% arrange(PATNO, PAG_NAME)
  dups_fixed <- dups %>% mutate(remove = is.na(HRPOSTMED)) %>% filter(remove == FALSE) %>% 
    mutate(remove = duplicated(unique_id)) %>% filter(remove == FALSE) %>% 
    select(-"remove")
  # Putting the fixed rows in PPMI
  PPMI <- PPMI %>% filter(!(unique_id %in% dups_fixed$unique_id))
  PPMI <- rbind(PPMI, dups_fixed)
  PPMI <- PPMI %>% select(-"unique_id")
  
  # Older Version: Taking only UPDRS3 columns with NUPDRS3 group
  # # Expanding columns based on PAG_NAME
  # PPMI <- PPMI %>% pivot_wider(names_from = PAG_NAME,
  #                              values_from = c("NP3TOT","NP3RIGLU","NP3RIGRU","NP3RIGLL","NP3RIGRL","NP3FTAPL","NP3FTAPR","NP3HMOVL","NP3HMOVR",
  #                              "NP3KTRML","NP3KTRMR","NP3LGAGL","NP3LGAGR","NP3PRSPL","NP3PRSPR","NP3TTAPL","NP3TTAPR",
  #                              "NP3PTRML","NP3PTRMR","NP3RTALU","NP3RTARU","NP3RTALL","NP3RTARL",
  #                              "NP3TOT_Calculated","Tremor3_Calculated","PIGD3_Calculated"))
  # 
  # 
  # 
  # # Calculating L/R UPDRS3 values
  # PPMI <- PPMI %>% rowwise() %>% 
  #   mutate(NP3TOT_NUPDRS3_L = sum(NP3FTAPL_NUPDRS3, NP3HMOVL_NUPDRS3, NP3KTRML_NUPDRS3, NP3LGAGL_NUPDRS3, NP3PRSPL_NUPDRS3, NP3PTRML_NUPDRS3,
  #                                 NP3RIGLU_NUPDRS3, NP3RIGLL_NUPDRS3, NP3TTAPL_NUPDRS3, NP3RTALU_NUPDRS3, NP3RTALL_NUPDRS3,  na.rm = FALSE),
  #          NP3TOT_NUPDRS3_R = sum(NP3FTAPR_NUPDRS3 , NP3HMOVR_NUPDRS3 , NP3KTRMR_NUPDRS3 , NP3LGAGR_NUPDRS3 , NP3PRSPR_NUPDRS3 , NP3PTRMR_NUPDRS3, 
  #                                 NP3RIGRU_NUPDRS3, NP3RIGRL_NUPDRS3, NP3TTAPR_NUPDRS3, NP3RTARU_NUPDRS3, NP3RTARL_NUPDRS3, na.rm = FALSE),
  #          NP3TOT_NUPDRS3A_L = sum(NP3FTAPL_NUPDRS3A , NP3HMOVL_NUPDRS3A , NP3KTRML_NUPDRS3A , NP3LGAGL_NUPDRS3A , NP3PRSPL_NUPDRS3A , NP3PTRML_NUPDRS3A, 
  #                                  NP3RIGLU_NUPDRS3A, NP3RIGLL_NUPDRS3A, NP3TTAPL_NUPDRS3A, NP3RTALU_NUPDRS3A, NP3RTALL_NUPDRS3A, na.rm = FALSE),
  #          NP3TOT_NUPDRS3A_R = sum(NP3FTAPR_NUPDRS3A , NP3HMOVR_NUPDRS3A , NP3KTRMR_NUPDRS3A , NP3LGAGR_NUPDRS3A , NP3PRSPR_NUPDRS3A , NP3PTRMR_NUPDRS3A, 
  #                                  NP3RIGRU_NUPDRS3A, NP3RIGRL_NUPDRS3A, NP3TTAPR_NUPDRS3A, NP3RTARU_NUPDRS3A, NP3RTARL_NUPDRS3A, na.rm = FALSE),
  #          NP3TOT_NUPDR3OF_L = sum(NP3FTAPL_NUPDR3OF , NP3HMOVL_NUPDR3OF , NP3KTRML_NUPDR3OF , NP3LGAGL_NUPDR3OF , NP3PRSPL_NUPDR3OF , NP3PTRML_NUPDR3OF, 
  #                                  NP3RIGLU_NUPDR3OF, NP3RIGLL_NUPDR3OF, NP3TTAPL_NUPDR3OF, NP3RTALU_NUPDR3OF, NP3RTALL_NUPDR3OF, na.rm = FALSE),
  #          NP3TOT_NUPDR3OF_R = sum(NP3FTAPR_NUPDR3OF , NP3HMOVR_NUPDR3OF , NP3KTRMR_NUPDR3OF , NP3LGAGR_NUPDR3OF , NP3PRSPR_NUPDR3OF , NP3PTRMR_NUPDR3OF, 
  #                                  NP3RIGRU_NUPDR3OF, NP3RIGRL_NUPDR3OF, NP3TTAPR_NUPDR3OF, NP3RTARU_NUPDR3OF, NP3RTARL_NUPDR3OF, na.rm = FALSE),
  #          NP3TOT_NUPDR3ON_L = sum(NP3FTAPL_NUPDR3ON , NP3HMOVL_NUPDR3ON , NP3KTRML_NUPDR3ON , NP3LGAGL_NUPDR3ON , NP3PRSPL_NUPDR3ON , NP3PTRML_NUPDR3ON, 
  #                                  NP3RIGLU_NUPDR3ON, NP3RIGLL_NUPDR3ON, NP3TTAPL_NUPDR3ON, NP3RTALU_NUPDR3ON, NP3RTALL_NUPDR3ON, na.rm = FALSE),
  #          NP3TOT_NUPDR3ON_R = sum(NP3FTAPR_NUPDR3ON , NP3HMOVR_NUPDR3ON , NP3KTRMR_NUPDR3ON , NP3LGAGR_NUPDR3ON , NP3PRSPR_NUPDR3ON , NP3PTRMR_NUPDR3ON, 
  #                                  NP3RIGRU_NUPDR3ON, NP3RIGRL_NUPDR3ON, NP3TTAPR_NUPDR3ON, NP3RTARU_NUPDR3ON, NP3RTARL_NUPDR3ON, na.rm = FALSE))
  # PPMI <- PPMI %>% select("PATNO","EVENT_ID","INFODT","NP3TOT_NUPDRS3","NP3TOT_NUPDRS3A","NP3TOT_NUPDR3OF","NP3TOT_NUPDR3ON",
  #                         "NP3TOT_NUPDRS3_L","NP3TOT_NUPDRS3_R","NP3TOT_NUPDRS3A_L","NP3TOT_NUPDRS3A_R","NP3TOT_NUPDR3OF_L","NP3TOT_NUPDR3OF_R","NP3TOT_NUPDR3ON_L","NP3TOT_NUPDR3ON_R",
  #                         "NP3TOT_Calculated_NUPDRS3","NP3TOT_Calculated_NUPDRS3A","NP3TOT_Calculated_NUPDR3OF","NP3TOT_Calculated_NUPDR3ON","Tremor3_Calculated_NUPDRS3",
  #                         "Tremor3_Calculated_NUPDRS3A","Tremor3_Calculated_NUPDR3OF","Tremor3_Calculated_NUPDR3ON","PIGD3_Calculated_NUPDRS3","PIGD3_Calculated_NUPDRS3A","PIGD3_Calculated_NUPDR3OF",
  #                         "PIGD3_Calculated_NUPDR3ON")
  # colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  
  # New Version: Taking ALL UPDRS3 groups + Adding another column for their group name
  # Calculating L/R UPDRS3 values
  PPMI <- PPMI %>% rowwise() %>% 
    mutate(NP3TOT_L = sum(NP3FTAPL, NP3HMOVL, NP3KTRML, NP3LGAGL, NP3PRSPL, NP3PTRML,
                          NP3RIGLU, NP3RIGLL, NP3TTAPL, NP3RTALU, NP3RTALL), #,na.rm = FALSE
           NP3TOT_R = sum(NP3FTAPR , NP3HMOVR , NP3KTRMR , NP3LGAGR , NP3PRSPR , NP3PTRMR, 
                          NP3RIGRU, NP3RIGRL, NP3TTAPR, NP3RTARU, NP3RTARL)) #,na.rm = FALSE
  PPMI <- PPMI %>% select("PATNO","EVENT_ID","INFODT", 'PDSTATE', 'PDSTATE_Corrected', 'PDMEDYN', 'PDMEDYN_Corrected', "PDTRTMNT", "HRPOSTMED", "PDMEDTM", "EXAMTM",
                          "NP3TOT",
                          "NP3TOT_L","NP3TOT_R",
                          "NP3TOT_Calculated","Tremor3_Calculated","PIGD3_Calculated")
  colnames(PPMI) <- c("Patient_Number","Visit_ID","Visit_Date", 'UPDRS3_State', 'UPDRS3_State_Corrected', 'isTaking_PD_Medication', 'isTaking_PD_Medication_Corrected', "isUnder_any_PD_Treatment", "UPDRS3_Time_Between_Examination_and_LastDose", "UPDRS3_Time_of_Medication_LastDose", "UPDRS3_Time_of_Examination",
                      "NP3TOT",
                      "NP3TOT_L","NP3TOT_R",
                      "NP3TOT_Calculated","Tremor3_Calculated","PIGD3_Calculated")
  
  
  ### Note: will make it fully wide based on 'UPDRS3_State_Corrected' in the PPMI_Wide_to_Cleaned.R script
  
  #### CLEANING DUPLICATES ####
  # creating asDate column first
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  
  ## Old version below -> was removing duplicate rows based on old PAG_NAME
  # # getting only the problematic rows -> fixing them
  # PPMI_temp <- PPMI %>% group_by(Patient_Number, Visit_ID) %>% #UPDRS3_Category
  #   filter(n() > 1) %>% arrange(Patient_Number, Visit_ID, Visit_Date_asDate) 
  # PPMI_cor <- data.frame(matrix(nrow = 0, ncol = length(PPMI_temp)))
  # colnames(PPMI_cor) <- colnames(PPMI_temp)
  # # Sort based on category first to get the main categories, if available
  # custom_order <- c("NUPDRS3", "NUPDRS3ON", "NUPDRS3OFF",
  #                   "NUPDRDOSE3", "NUPDRDOSE3ON", "NUPDRDOSE3OFF",
  #                   "NUPDRS3A", "NUPDRS3AON", "NUPDRS3AOFF",
  #                   "NUPDR3ON", "NUPDR3ONON", "NUPDR3ONOFF",
  #                   "NUPDR3OF", "NUPDR3OFON", "NUPDR3OFOFF")
  # 
  # PPMI_temp <- PPMI_temp %>% arrange(Patient_Number, Visit_ID, factor(UPDRS3_Category, levels = custom_order)) %>% 
  #   mutate(UPDRS3_Category = as.character(UPDRS3_Category))
  # # remove any 3rd extra column
  # PPMI_temp <- PPMI_temp %>%
  #   group_by(Patient_Number, Visit_ID) %>%
  #   slice(1:2)
  # # if for a Patient_Number-Visit_ID combination there is a row with NP3TOT not being NA, keep that
  # PPMI_cor_nonNA <- PPMI_temp %>% group_by(Patient_Number, Visit_ID) %>% 
  #   mutate(isNA = case_when(is.na(NP3TOT) ~ 1, TRUE ~ 0)) %>% 
  #   mutate(isNA_all = sum(isNA))
  # PPMI_temp <- PPMI_cor_nonNA %>% filter(isNA_all != 1) %>% select(-isNA, -isNA_all) # this has the rows with duplicated - needs to be processed again below
  # PPMI_cor_nonNA <- PPMI_cor_nonNA %>% filter(isNA_all == 1) %>% filter(!is.na(NP3TOT)) %>% select(-isNA, -isNA_all) # this has the correct row if one of the duplicate's NP3TOT was NA
  # for (i in 1:(dim(PPMI_temp)[1]/2)) {
  #   PPMI_cor[i,1:length(PPMI_temp)] <- PPMI_temp[(i*2)-1,]
  #   PPMI_cor[i,6:length(PPMI_temp)] <- coalesce(as.numeric(PPMI_temp[(i*2)-1,6:length(PPMI_temp)]),as.numeric(PPMI_temp[i*2,6:length(PPMI_temp)])) #can use coalesce since all columns are numbers
  # }
  # PPMI_cor <- rbind(PPMI_cor,PPMI_cor_nonNA)
  # PPMI_cor <- PPMI_cor %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  # # removing problematic rows from main file -> adding corrected rows -> rearrange
  # PPMI <- as.data.frame(PPMI)
  # PPMI <- PPMI %>% filter(!(paste0(Patient_Number,Visit_ID) %in% paste0(PPMI_cor$Patient_Number,PPMI_cor$Visit_ID)))
  # PPMI <- rbind(PPMI,PPMI_cor)
  # PPMI <- PPMI[!is.na(PPMI$Patient_Number),]
  # PPMI$Patient_Number <- as.numeric(PPMI$Patient_Number)
  # PPMI[is.na(PPMI)] <- NA
  # ## Separate calculated and regular L and R NO3TOTs
  # PPMI$NP3TOT_Calculated_L <- PPMI$NP3TOT_L
  # PPMI$NP3TOT_Calculated_R <- PPMI$NP3TOT_R
  # PPMI$NP3TOT_L[is.na(PPMI$NP3TOT)] <- NA
  # PPMI$NP3TOT_R[is.na(PPMI$NP3TOT)] <- NA
  
  # saving wide file
  PPMI <- PPMI %>% arrange(Patient_Number,Visit_Date_asDate)
  write.csv(PPMI, "../Data_Wide/MDS_UPDRS_Part_III_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## MDS-UPDRS_Part_I.csv ########
  UPDRS1 <- read.csv(paste0(raw_path,"MDS-UPDRS_Part_I_",download_date,".csv"), sep=",", header = T)
  UPDRS1PQ <- read.csv(paste0(raw_path,"MDS-UPDRS_Part_I_Patient_Questionnaire_",download_date,".csv"), sep=",", header = T)
  UPDRS1PQ <- UPDRS1PQ %>% select(c("PATNO","EVENT_ID","NP1SLPN","NP1SLPD","NP1PAIN","NP1URIN","NP1CNST","NP1LTHD","NP1FATG","NP1PTOT"))
  UPDRS1 <- UPDRS1 %>% left_join(UPDRS1PQ, by = c("PATNO","EVENT_ID"))
  UPDRS1[,6:13] <- sapply(UPDRS1[,6:13],as.numeric)
  UPDRS1[,16:23] <- sapply(UPDRS1[,16:23],as.numeric)
  UPDRS_PartI = rowSums(UPDRS1[,c("NP1COG","NP1HALL","NP1DPRS","NP1ANXS","NP1APAT","NP1DDS","NP1SLPN","NP1SLPD","NP1PAIN","NP1URIN","NP1CNST","NP1LTHD","NP1FATG")])
  UPDRS1$NP1TOT_Calculated <- UPDRS_PartI
  UPDRS1$Apathy <- UPDRS1$NP1APAT
  
  PPMI <- UPDRS1 %>% select("PATNO","EVENT_ID","INFODT","NP1RTOT","NP1TOT_Calculated", 'Apathy')
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  write.csv(PPMI, "../Data_Wide/MDS-UPDRS_Part_I_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## MDS-UPDRS_Part_I_Patient_Questionnaire.csv ########
  UPDRS1 <- read.csv(paste0(raw_path,"MDS-UPDRS_Part_I_Patient_Questionnaire_",download_date,".csv"), sep=",", header = T)
  PPMI <- UPDRS1 %>% select("PATNO","EVENT_ID","INFODT")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  write.csv(PPMI, "../Data_Wide/MDS-UPDRS_Part_I_Patient_Questionnaire_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## MDS-MDS_UPDRS_Part_II__Patient_Questionnaire.csv ########
  UPDRS2 <- read.csv(paste0(raw_path,"MDS_UPDRS_Part_II__Patient_Questionnaire_",download_date,".csv"), sep=",", header = T)
  UPDRS_PartII = rowSums(UPDRS2[,c("NP2SPCH","NP2SALV","NP2SWAL","NP2EAT","NP2DRES","NP2HYGN","NP2HWRT","NP2HOBB","NP2TURN","NP2TRMR","NP2RISE","NP2WALK","NP2FREZ")])
  Tremor_score_2 = UPDRS2$NP2TRMR
  PIGD_score_2 = rowMeans(UPDRS2[,c("NP2WALK","NP2FREZ")])
  UPDRS2$NP2TOT_Calculated <- UPDRS_PartII
  UPDRS2$Tremor2_Calculated <- Tremor_score_2
  UPDRS2$PIGD2_Calculated <- PIGD_score_2
  
  PPMI <- UPDRS2 %>% select("PATNO","EVENT_ID","INFODT","NP2PTOT","NP2TOT_Calculated","Tremor2_Calculated","PIGD2_Calculated")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  write.csv(PPMI, "../Data_Wide/MDS_UPDRS_Part_II__Patient_Questionnaire_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## MDS-UPDRS_Part_IV__Motor_Complications.csv ########
  UPDRS4 <- read.csv(paste0(raw_path,"MDS-UPDRS_Part_IV__Motor_Complications_",download_date,".csv"), sep=",", header = T)
  PPMI <- UPDRS4 %>% select("PATNO","EVENT_ID","INFODT","NP4TOT")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  write.csv(PPMI, "../Data_Wide/MDS-UPDRS_Part_IV__Motor_Complications_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Montreal_Cognitive_Assessment__MoCA_.csv ########
  MOCA <- read.csv(paste0(raw_path,"Montreal_Cognitive_Assessment__MoCA__",download_date,".csv"), sep=",", header = T)
  PPMI <- MOCA %>% select("PATNO","EVENT_ID","INFODT","MCATOT")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  write.csv(PPMI, "../Data_Wide/Montreal_Cognitive_Assessment__MoCA_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## MRI_Metadata.csv ########
  MRI <- read.csv(paste0(raw_path,"MRI_Acquisition_Metadata_",download_date,".csv"), sep=",", header = T)
  PPMI <- MRI %>% select("PATNO","MRI_SCAN_DATE","MRI_SCAN_QUALITY_RATING","MRI_SEQ_DTI","MRI_SEQ_RS","MRI_SEQ_T1_WEIGHTED","MRI_SEQ_T2_WEIGHTED")
  colnames(PPMI) <- c("Patient_Number","MRI_Scan_Date_Metadata","MRI_Scan_Quality","DTI_Available","RS_Available","T1w_Available","T2w_Available")
  PPMI[PPMI == ''] <- NA
  PPMI <- PPMI %>% mutate(MRI_Scan_Date_Metadata_asDate = as.Date(paste("01/",MRI_Scan_Date_Metadata,sep=""),"%d/%m/%Y")) %>% relocate(MRI_Scan_Date_Metadata_asDate, .after = MRI_Scan_Date_Metadata) %>% arrange(Patient_Number,MRI_Scan_Date_Metadata_asDate)
  write.csv(PPMI, "../Data_Wide/MRI_Metadata_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Magnetic_Resonance_Imaging__MRI_.csv ########
  MRI <- read.csv(paste0(raw_path,"Magnetic_Resonance_Imaging__MRI__",download_date,".csv"), sep=",", header = T)
  PPMI <- MRI %>% select("PATNO","EVENT_ID","INFODT","MRICMPLT")
  colnames(PPMI) <- c("Patient_Number","Visit_ID","MRI_Scan_Date","MRI_Completed")
  PPMI[PPMI == ''] <- NA
  PPMI <- PPMI %>% mutate(MRI_Scan_Date_asDate = as.Date(paste("01/",MRI_Scan_Date,sep=""),"%d/%m/%Y")) %>% relocate(MRI_Scan_Date_asDate, .after = MRI_Scan_Date) %>% arrange(Patient_Number,MRI_Scan_Date_asDate)
  ## CLEANING DUPLICATES ##
  PPMI_cor <- PPMI %>% group_by(Patient_Number, Visit_ID) %>% 
    filter(n() > 1) %>% arrange(Patient_Number,MRI_Scan_Date_asDate) %>% 
    mutate(remove = case_when(row_number()==2 ~ TRUE)) %>% filter(is.na(remove))
  PPMI_cor <- PPMI_cor[,-length(PPMI_cor)]
  PPMI <- PPMI %>% filter(!(paste0(Patient_Number,Visit_ID) %in% paste0(PPMI_cor$Patient_Number,PPMI_cor$Visit_ID)))
  PPMI <- rbind(PPMI,PPMI_cor) %>% arrange(Patient_Number,MRI_Scan_Date_asDate)
  # saving wide file
  write.csv(PPMI, "../Data_Wide/Magnetic_Resonance_Imaging__MRI_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## DaTScan_Metadata.csv ########
  DaT <- read.csv(paste0(raw_path,"DaTScan_Acquisition_Metadata_",download_date,".csv"), sep=",", header = T)
  PPMI <- DaT %>% select("PATNO","EVENT_ID","DATSCAN_DATE","DATSCAN_LIGAND","DATSCAN_QUALITY_RATING","DATSCAN_IMAGE_ACCEPTABLE")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","DAT_Scan_Date_Metadata")
  PPMI <- PPMI %>% mutate(DAT_Scan_Date_Metadata_asDate = as.Date(paste("01/",DAT_Scan_Date_Metadata,sep=""),"%d/%m/%Y")) %>% relocate(DAT_Scan_Date_Metadata_asDate, .after = DAT_Scan_Date_Metadata) %>% arrange(Patient_Number,DAT_Scan_Date_Metadata_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/DaTScan_Metadata_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## DaTScan_Analysis.csv ########
  DaT <- read.csv(paste0(raw_path,"DaTScan_SBR_Analysis_",download_date,".csv"), sep=",", header = T)
  PPMI <- DaT %>% select("PATNO","EVENT_ID","DATSCAN_DATE","PROTOCOL","DATSCAN_LIGAND","DATSCAN_CAUDATE_R","DATSCAN_CAUDATE_L","DATSCAN_PUTAMEN_R","DATSCAN_PUTAMEN_L","DATSCAN_PUTAMEN_R_ANT","DATSCAN_PUTAMEN_L_ANT")
  colnames(PPMI)[1:4] <- c("Patient_Number","Visit_ID","DAT_Scan_Date","DAT_Protocol")
  # adding average DaT result columns
  PPMI <- PPMI %>% mutate(DAT_Caudate_Average = ((DATSCAN_CAUDATE_R)+(DATSCAN_CAUDATE_L))/2,
                          DAT_Putamen_Average = ((DATSCAN_PUTAMEN_R)+(DATSCAN_PUTAMEN_L))/2,
                          DAT_Putamen_Average_ANT = ((DATSCAN_PUTAMEN_R_ANT)+(DATSCAN_PUTAMEN_L_ANT))/2) %>% 
    relocate(DAT_Caudate_Average, .after = DATSCAN_CAUDATE_L) %>% 
    relocate(DAT_Putamen_Average, .after = DATSCAN_PUTAMEN_L) %>% 
    relocate(DAT_Putamen_Average_ANT, .after = DATSCAN_PUTAMEN_L_ANT)
  PPMI <- PPMI %>% mutate(DAT_Scan_Date_asDate = as.Date(paste("01/",DAT_Scan_Date,sep=""),"%d/%m/%Y")) %>% relocate(DAT_Scan_Date_asDate, .after = DAT_Scan_Date) %>% arrange(Patient_Number,DAT_Scan_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/DaTScan_Analysis_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Benton_Judgement_of_Line_Orientation.csv ########
  Bent <- read.csv(paste0(raw_path,"Benton_Judgement_of_Line_Orientation_",download_date,".csv"), sep=",", header = T)
  PPMI <- Bent %>% select("PATNO","EVENT_ID","INFODT","PAG_NAME","JLO_TOTRAW","JLO_TOTCALC","AGE_ASSESS_JLO","DVS_JLO_MSSA","DVS_JLO_MSSAE")
  colnames(PPMI)[1:4] <- c("Patient_Number","Visit_ID","Visit_Date","JLO_PAG_NAME")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  ## CLEANING DUPLICATES ##
  PPMI_cor <- PPMI %>% group_by(Patient_Number, Visit_ID) %>% 
    filter(n() > 1) %>% arrange(Patient_Number,Visit_Date_asDate) %>% 
    mutate(remove = case_when(JLO_PAG_NAME=="BENTONOD" ~ TRUE)) %>% filter(is.na(remove))
  PPMI_cor <- PPMI_cor[,-length(PPMI_cor)]
  PPMI <- PPMI %>% filter(!(paste0(Patient_Number,Visit_ID) %in% paste0(PPMI_cor$Patient_Number,PPMI_cor$Visit_ID)))
  PPMI <- rbind(PPMI,PPMI_cor) %>% arrange(Patient_Number,Visit_Date_asDate)
  write.csv(PPMI, "../Data_Wide/Benton_Judgement_of_Line_Orientation_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Cognitive_Categorization.csv ########
  Cog <- read.csv(paste0(raw_path,"Cognitive_Categorization_",download_date,".csv"), sep=",", header = T)
  PPMI <- Cog %>% select("PATNO","EVENT_ID","INFODT","PAG_NAME","COGDECLN","FNCDTCOG","COGSTATE","COGDXCL","COGCAT","COGCAT_TEXT","RVWNPSY")
  colnames(PPMI)[1:4] <- c("Patient_Number","Visit_ID","Visit_Date","COG_PAG_NAME")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Cognitive_Categorization_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Cognitive_Change.csv ########
  Cog <- read.csv(paste0(raw_path,"Cognitive_Change_",download_date,".csv"), sep=",", header = T)
  PPMI <- Cog %>% select("PATNO","EVENT_ID","INFODT","COGCHG")
  colnames(PPMI)[1:4] <- c("Patient_Number","Visit_ID","Visit_Date","Cognitive_Change")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Cognitive_Change_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Epworth_Sleepiness_Scale.csv ########
  Sleep <- read.csv(paste0(raw_path,"Epworth_Sleepiness_Scale_",download_date,".csv"), sep=",", header = T)
  PPMI <- Sleep %>% select("PATNO","EVENT_ID","INFODT","ESS1","ESS2","ESS3","ESS4","ESS5","ESS6","ESS7","ESS8")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Epworth_Sleepiness_Scale_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Family_History.csv ########
  fam <- read.csv(paste0(raw_path,"Family_History_",download_date,".csv"), sep=",", header = T)
  PPMI <- fam %>% select("PATNO","EVENT_ID","INFODT","ANYFAMPD","DISFAMPD","BIOMOMPD","BIODADPD","FULSIBPD","HAFSIBPD","MAGPARPD","PAGPARPD","MATAUPD","PATAUPD","KIDSPD","MATCOUSPD","PATCOUSPD")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  ## CLEANING DUPLICATES -> ONE ROW PER PATIENT ##
  PPMI_cor <- PPMI %>% group_by(Patient_Number) %>% 
    filter(n() > 1) %>% arrange(Patient_Number,Visit_Date_asDate) %>% 
    mutate(ANYFAMPD = case_when(n() > 1 ~ max_(ANYFAMPD)),
           DISFAMPD = case_when(n() > 1 ~ max_(DISFAMPD)),
           BIOMOMPD = case_when(n() > 1 ~ max_(BIOMOMPD)),
           BIODADPD = case_when(n() > 1 ~ max_(BIODADPD)),
           FULSIBPD = case_when(n() > 1 ~ max_(FULSIBPD)),
           HAFSIBPD = case_when(n() > 1 ~ max_(HAFSIBPD)),
           MAGPARPD = case_when(n() > 1 ~ max_(MAGPARPD)),
           PAGPARPD = case_when(n() > 1 ~ max_(PAGPARPD)),
           MATAUPD = case_when(n() > 1 ~ max_(MATAUPD)),
           PATAUPD = case_when(n() > 1 ~ max_(PATAUPD)),
           KIDSPD = case_when(n() > 1 ~ max_(KIDSPD)),
           MATCOUSPD = case_when(n() > 1 ~ max_(MATCOUSPD)),
           PATCOUSPD = case_when(n() > 1 ~ max_(PATCOUSPD))) %>% 
    mutate(remove = case_when(n() > 1 & row_number()>1 ~ TRUE)) %>% filter(is.na(remove))
  PPMI_cor <- PPMI_cor[,-length(PPMI_cor)]
  PPMI <- PPMI %>% filter(!Patient_Number %in% PPMI_cor$Patient_Number)
  PPMI <- rbind(PPMI,PPMI_cor) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI <- PPMI[,-c(2:4)]
  write.csv(PPMI, "../Data_Wide/Family_History_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Geriatric_Depression_Scale__Short_Version_.csv ########
  GDS <- read.csv(paste0(raw_path,"Geriatric_Depression_Scale__Short_Version__",download_date,".csv"), sep=",", header = T)
  PPMI <- GDS %>% select("PATNO","EVENT_ID","INFODT","GDSSATIS","GDSDROPD","GDSEMPTY","GDSBORED","GDSGSPIR","GDSAFRAD","GDSHAPPY","GDSHLPLS","GDSHOME","GDSMEMRY","GDSALIVE","GDSWRTLS","GDSENRGY","GDSHOPLS","GDSBETER")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Geriatric_Depression_Scale__Short_Version_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Hopkins_Verbal_Learning_Test_-_Revised.csv ########
  hvlt <- read.csv(paste0(raw_path,"Hopkins_Verbal_Learning_Test_-_Revised_",download_date,".csv"), sep=",", header = T)
  PPMI <- hvlt %>% select("PATNO","EVENT_ID","INFODT","AGE_ASSESS_HVLT","DVT_TOTAL_RECALL","DVT_DELAYED_RECALL","DVT_RETENTION","DVT_RECOG_DISC_INDEX")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Hopkins_Verbal_Learning_Test_-_Revised_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Letter_-_Number_Sequencing.csv ########
  lns <- read.csv(paste0(raw_path,"Letter_-_Number_Sequencing_",download_date,".csv"), sep=",", header = T)
  PPMI <- lns %>% select("PATNO","EVENT_ID","INFODT","LNS_TOTRAW","AGE_ASSESS_LNS","DVS_LNS")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Letter_-_Number_Sequencing_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  # ######## Lumbar_Puncture.csv ######## - data are removed due to quality as of Oct 10 2024
  # lumb <- read.csv(paste0(raw_path,"Lumbar_Puncture_",download_date,".csv"), sep=",", header = T)
  # PPMI <- lumb %>% select("PATNO","EVENT_ID","INFODT","TOPRRSLT","TGLCRSLT")
  # colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  # PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  # PPMI[PPMI == ''] <- NA
  # write.csv(PPMI, "../Data_Wide/Lumbar_Puncture_wide.csv", row.names=FALSE)
  # 
  # cat("\014")
  
  ######## Modified_Schwab___England_Activities_of_Daily_Living.csv ########
  MSEA <- read.csv(paste0(raw_path,"Modified_Schwab___England_Activities_of_Daily_Living_",download_date,".csv"), sep=",", header = T)
  PPMI <- MSEA %>% select("PATNO","EVENT_ID","INFODT","MSEADLG")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Modified_Schwab___England_Activities_of_Daily_Living_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## QUIP-Current-Short.csv ########
  quip <- read.csv(paste0(raw_path,"QUIP-Current-Short_",download_date,".csv"), sep=",", header = T)
  PPMI <- quip %>% select("PATNO","EVENT_ID","INFODT","TMGAMBLE","CNTRLGMB","TMSEX","CNTRLSEX","TMBUY","CNTRLBUY","TMEAT","CNTRLEAT","TMTORACT","TMTMTACT","TMTRWD","TMDISMED","CNTRLDSM")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/QUIP-Current-Short_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## REM_Sleep_Behavior_Disorder_Questionnaire.csv ########
  rem <- read.csv(paste0(raw_path,"REM_Sleep_Behavior_Disorder_Questionnaire_",download_date,".csv"), sep=",", header = T)
  PPMI <- rem %>% select("PATNO","EVENT_ID","INFODT","DRMVIVID","DRMAGRAC","DRMNOCTB","SLPLMBMV","SLPINJUR","DRMVERBL","DRMFIGHT",
                         "DRMUMV","DRMOBJFL","MVAWAKEN","DRMREMEM","SLPDSTRB","STROKE","HETRA","PARKISM","RLS",
                         "NARCLPSY","DEPRS","EPILEPSY","BRNINFM","CNSOTH")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/REM_Sleep_Behavior_Disorder_Questionnaire_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## SCOPA-AUT.csv ########
  SCOPA <- read.csv(paste0(raw_path,"SCOPA-AUT_",download_date,".csv"), sep=",", header = T)
  PPMI <- SCOPA %>% select("PATNO","EVENT_ID","INFODT","SCAU1","SCAU2","SCAU3","SCAU4","SCAU5","SCAU6","SCAU7","SCAU8","SCAU9","SCAU10","SCAU11","SCAU12",
                           "SCAU13","SCAU14","SCAU15","SCAU16","SCAU17","SCAU18","SCAU19","SCAU20","SCAU21","SCAU22","SCAU23","SCAU23A","SCAU23AT",
                           "SCAU24","SCAU25","SCAU26A","SCAU26AT","SCAU26B","SCAU26BT","SCAU26C","SCAU26CT","SCAU26D","SCAU26DT")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/SCOPA-AUT_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Modified_Semantic_Fluency.csv ########
  MSF <- read.csv(paste0(raw_path,"Modified_Semantic_Fluency_",download_date,".csv"), sep=",", header = T)
  PPMI <- MSF %>% select("PATNO","EVENT_ID","INFODT","AGE_ASSESS_SFTANIM","DVS_SFTANIM","DVT_SFTANIM")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Modified_Semantic_Fluency_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## State-Trait_Anxiety_Inventory.csv ########
  STAI <- read.csv(paste0(raw_path,"State-Trait_Anxiety_Inventory_",download_date,".csv"), sep=",", header = T)
  PPMI <- STAI %>% select("PATNO","EVENT_ID","INFODT","STAIAD1","STAIAD2","STAIAD3","STAIAD4","STAIAD5","STAIAD6","STAIAD7","STAIAD8",
                          "STAIAD9","STAIAD10","STAIAD11","STAIAD12","STAIAD13","STAIAD14","STAIAD15","STAIAD16",
                          "STAIAD17","STAIAD18","STAIAD19","STAIAD20","STAIAD21","STAIAD22","STAIAD23","STAIAD24",
                          "STAIAD25","STAIAD26","STAIAD27","STAIAD28","STAIAD29","STAIAD30","STAIAD31","STAIAD32",
                          "STAIAD33","STAIAD34","STAIAD35","STAIAD36","STAIAD37","STAIAD38","STAIAD39","STAIAD40")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/State-Trait_Anxiety_Inventory_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Symbol_Digit_Modalities_Test.csv ########
  SDM <- read.csv(paste0(raw_path,"Symbol_Digit_Modalities_Test_",download_date,".csv"), sep=",", header = T)
  PPMI <- SDM %>% select("PATNO","EVENT_ID","INFODT","SDMTOTAL","AGE_ASSESS_SDM","DVSD_SDM","DVT_SDM")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Symbol_Digit_Modalities_Test_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  
  ######## University_of_Pennsylvania_Smell_Identification_Test__UPSIT_.csv ########
  UPSIT <- read.csv(paste0(raw_path,"University_of_Pennsylvania_Smell_Identification_Test_UPSIT_",download_date,".csv"), sep=",", header = T)
  PPMI <- UPSIT %>% select("PATNO","EVENT_ID","INFODT","UPSIT_PRCNTGE","TOTAL_CORRECT","UPSITFORM")
  colnames(PPMI) <- c("Patient_Number","Visit_ID","Visit_Date","UPSIT_PRCNTGE","UPSIT_TOTAL_CORRECT","UPSITFORM")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/University_of_Pennsylvania_Smell_Identification_Test__UPSIT_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Vital_Signs.csv ########
  Vital <- read.csv(paste0(raw_path,"Vital_Signs_",download_date,".csv"), sep=",", header = T)
  PPMI <- Vital %>% select("PATNO","EVENT_ID","INFODT","WGTKG","HTCM","TEMPC","SYSSUP","DIASUP","HRSUP","SYSSTND","DIASTND","HRSTND")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Vital_Signs_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## PD_Diagnosis_History.csv ########
  Hist <- read.csv(paste0(raw_path,"PD_Diagnosis_History_",download_date,".csv"), sep=",", header = T)
  PPMI <- Hist %>% select("PATNO","EVENT_ID","INFODT","SXDT","PDDXDT","DXTREMOR","DXRIGID","DXBRADY","DXPOSINS","DXOTHSX")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/PD_Diagnosis_History_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Blood_Chemistry___Hematology.csv ########
  blood <- read.csv(paste0(raw_path,"Blood_Chemistry___Hematology_",download_date,".csv"), sep=",", header = T)
  PPMI <- blood %>% filter(EVENT_ID == "SC" & 
                             LVISTYPE == "Screening" &
                             LTSTNAME %in% c("Total Protein","Albumin-QT",
                                             "Alkaline Phosphatase-QT","ALT (SGPT)",
                                             "AST (SGOT)","Urea Nitrogen",
                                             "Serum Uric Acid","Serum Glucose")) %>% select("PATNO","EVENT_ID","LCOLLDT","LTSTNAME","LUSRES")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Blood_Collection_Date")
  PPMI <- PPMI %>% mutate(Blood_Collection_Date_asDate = as.Date(paste("01/",Blood_Collection_Date,sep=""),"%d/%m/%Y")) %>% relocate(Blood_Collection_Date_asDate, .after = Blood_Collection_Date) %>% arrange(Patient_Number,Blood_Collection_Date_asDate)
  PPMI[PPMI == ''] <- NA
  ## CLEANING DUPLICATES ##
  PPMI_cor <- PPMI %>% group_by(Patient_Number,LTSTNAME) %>% 
    filter(n() > 1) %>% arrange(Patient_Number,LTSTNAME,Blood_Collection_Date_asDate) %>% 
    mutate(remove = case_when(row_number()==1 ~ TRUE)) %>% filter(is.na(remove))
  PPMI_cor <- PPMI_cor[,-length(PPMI_cor)]
  PPMI <- PPMI %>% filter(!(paste0(Patient_Number,LTSTNAME) %in% paste0(PPMI_cor$Patient_Number,PPMI_cor$LTSTNAME)))
  PPMI <- rbind(PPMI,PPMI_cor) %>% arrange(Patient_Number)
  PPMI <- PPMI %>% spread(LTSTNAME,LUSRES)
  write.csv(PPMI, "../Data_Wide/Blood_Chemistry___Hematology_wide.csv", row.names=FALSE)
  PPMI <- read.csv("../Data_Wide/Blood_Chemistry___Hematology_wide.csv", sep=",", header = T)
  PPMI <- PPMI %>% filter((is.na(Albumin.QT) &
                             is.na(ALT..SGPT.) &
                             is.na(AST..SGOT.) &
                             is.na(Serum.Glucose) &
                             is.na(Serum.Uric.Acid) &
                             is.na(Total.Protein) &
                             is.na(Urea.Nitrogen)) == FALSE) %>% mutate(blood_tested = 1) # new part to remove fully NA rows and add a new column
  write.csv(PPMI, "../Data_Wide/Blood_Chemistry___Hematology_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Socio-Economics.csv ########
  SES <- read.csv(paste0(raw_path,"Socio-Economics_",download_date,".csv"), sep=",", header = T)
  PPMI <- SES %>% select("PATNO","EVENT_ID","INFODT","EDUCYRS")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Socio-Economics_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Skin_Biopsy.csv ########
  skin <- read.csv(paste0(raw_path,"Skin_Biopsy_",download_date,".csv"), sep=",", header = T)
  PPMI <- skin %>% select("PATNO","EVENT_ID","INFODT","SKBIOCMP","ANSTHADM","SKBIOLOC","FIXSKBSID","FSHSKBSID")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Skin_Biopsy_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Prodromal_History.csv ########
  prodromal <- read.csv(paste0(raw_path,"Prodromal_History_",download_date,".csv"), sep=",", header = T)
  PPMI <- prodromal %>% select("PATNO","EVENT_ID","INFODT","PROSCRN","PRORBDENRL",
                               "RBDDIAG","RBDDISYDT","RBDDIDT","RBDPSG","RBDPSSYDT","RBDPSDT","PROGENENRL",
                               "PROHYPENRL","PRO1FAMPD","PROSYNBYP","PROPREVHYP") %>% 
    mutate(RBDDISYDT = as.Date(paste("01/",RBDDISYDT,sep=""),"%d/%m/%Y")) %>% 
    mutate(RBDDIDT = as.Date(paste("01/",RBDDIDT,sep=""),"%d/%m/%Y")) %>% 
    mutate(RBDPSSYDT = as.Date(paste("01/",RBDPSSYDT,sep=""),"%d/%m/%Y")) %>% 
    mutate(RBDPSDT = as.Date(paste("01/",RBDPSDT,sep=""),"%d/%m/%Y"))
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Prodromal_History_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  # ######## AV-133_PET_Analysis.csv ######## -> files changed and not available anymore
  # pet <- read.csv(paste0(raw_path,"AV-133_PET_Analysis_",download_date,".csv"), sep=",", header = T)
  # PPMI <- pet %>% select("PATNO","EVENT_ID","AV133_SCAN_DATE","AV133_RCAUD_S","AV133_RPUTANT_S",
  #                        "AV133_RPUTPOST_S","AV133_LCAUD_S","AV133_LPUTANT_S","AV133_LPUTPOST_S")
  # colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","PET_Scan_Date")
  # PPMI <- PPMI %>% mutate(PET_Scan_Date_asDate = as.Date(paste("01/",PET_Scan_Date,sep=""),"%d/%m/%Y")) %>% relocate(PET_Scan_Date_asDate, .after = PET_Scan_Date) %>% arrange(Patient_Number,PET_Scan_Date_asDate)
  # PPMI[PPMI == ''] <- NA
  # write.csv(PPMI, "../Data_Wide/AV-133_PET_Analysis_wide.csv", row.names=FALSE)
  # 
  # cat("\014")
  
  ######## Determination_of_Freezing_and_Falls.csv ########
  fall <- read.csv(paste0(raw_path,"Determination_of_Freezing_and_Falls_",download_date,".csv"), sep=",", header = T)
  PPMI <- fall %>% select("PATNO","EVENT_ID","INFODT","PTCGBOTH","FRZGT1W","FLNFR1W","FRZGT12M","FLNFR12M","INJFRHIP",
                          "INJFRUE","INJFRSKL","INJFROTH","HINJNOLC","HINJLOC2","INJSTCH","INJOTH","FLLDRVIS","FLLERVIS","FLLHOSP","FLLSURG","FLLINST")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Determination_of_Freezing_and_Falls_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Demographics.csv ########
  demos <- read.csv(paste0(raw_path,"Demographics_",download_date,".csv"), sep=",", header = T)
  PPMI <- demos %>% select("PATNO","EVENT_ID","INFODT","SEX","HANDED","HISPLAT","RAINDALS","RAASIAN","RABLACK","RAHAWOPI","RAWHITE","BIRTHDT")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI <- PPMI %>% mutate(BIRTHDT_asDate = as.Date(paste("01/",BIRTHDT,sep=""),"%d/%m/%Y"))
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Demographics_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  ######## Participant_Status.csv ########
  status <- read.csv(paste0(raw_path,"Participant_Status_",download_date,".csv"), sep=",", header = T)
  PPMI <- status %>% select("PATNO","COHORT","ENROLL_AGE","ENROLL_DATE","ENROLL_STATUS",
                            "ENRLPINK1", "ENRLPRKN", "ENRLSRDC", "ENRLHPSM", "ENRLRBD", "ENRLLRRK2", "ENRLSNCA", "ENRLGBA")
  colnames(PPMI)[1:5] <- c("Patient_Number","Cohort_n","ENROLL_AGE","Enroll_Date","Enroll_Status")
  PPMI <- PPMI %>% mutate(Enroll_Date_asDate = as.Date(paste("01/",Enroll_Date,sep=""),"%d/%m/%Y")) %>% relocate(Enroll_Date_asDate, .after = Enroll_Date) %>% arrange(Patient_Number,Enroll_Date_asDate)
  PPMI <- PPMI %>% mutate(Cohort = case_when(Cohort_n == 1 ~ "PD",
                                             Cohort_n == 2 ~ "HC",
                                             Cohort_n == 3 ~ "SWEDD",
                                             Cohort_n == 4 ~ "Prodromal",
                                             Cohort_n == 9 ~ "EarlyImage",)) %>% relocate(Cohort, .after = Cohort_n)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Participant_Status_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  
  
  ######## Clock_Drawing.csv ########
  clock <- read.csv(paste0(raw_path,"Clock_Drawing_",download_date,".csv"), sep=",", header = T)
  PPMI <- clock %>% select("PATNO","EVENT_ID","INFODT","CLCKPII","CLCK2HND","CLCKNMRK","CLCKNUIN","CLCKALNU","CLCKNUSP","CLCKNUED","CLCKTOT")
  colnames(PPMI)[1:3] <- c("Patient_Number","Visit_ID","Visit_Date")
  PPMI <- PPMI %>% mutate(Visit_Date_asDate = as.Date(paste("01/",Visit_Date,sep=""),"%d/%m/%Y")) %>% relocate(Visit_Date_asDate, .after = Visit_Date) %>% arrange(Patient_Number,Visit_Date_asDate)
  PPMI[PPMI == ''] <- NA
  write.csv(PPMI, "../Data_Wide/Clock_Drawing_wide.csv", row.names=FALSE)
  
  cat("\014")
  
  
  
  
  ######## Current_Biospecimen_Analysis_Results.csv ########
  bio <- read.csv(paste0(raw_path,"Current_Biospecimen_Analysis_Results_",download_date,".csv"), sep=",", header = T)
  bio_list <- c("ApoE Genotype","APOE GENOTYPE","DJ-1","SCORE","SNCA_multiplication","Triglycerides","Total Cholesterol",
                "LDL","HDL","Apolipoprotein A1","EGF ELISA","Serum IGF-1","CSF Alpha-synuclein","tTau","ABeta 1-42",
                "pTau","GRS","NfL","NFL","GFAP","S100","IL-6","a-Synuclein","NEV a-synuclein  (rep1)",
                "NEV a-synuclein  (rep2)","ABeta","ABeta raw","CSF lgG","Plasma Albumin","Plasma IgG","CSF Albumin",
                "SNCA-007","SNCA-3UTR-1","SNCA-3UTR-2","SNCA-E3E4","SNCA-E4E6","SNCA (rep 1)control","SNCA (rep 2)control",
                "SNCA (rep 3)control","SNCA (rep 1)H2O2","SNCA (rep 2)H2O2","SNCA (rep 3)H2O2","SNCA (rep 1)H2O2+CCM",
                "SNCA (rep 2)H2O2+CCM","SNCA (rep 3)H2O2+CCM","SNCA (rep 1)H2O2+UCM","SNCA (rep 2)H2O2+UCM","SNCA (rep 3)H2O2+UCM",
                "rs34995376_LRRK2_p.R1441H","rs35801418_LRRK2_p.Y1699C","rs34637584_LRRK2_p.G2019S","rs35870237_LRRK2_p.I2020T",
                "LRRK2 (rep 1)control","LRRK2 (rep 2)control","LRRK2 (rep 3)control","LRRK2 (rep 1)H2O2","LRRK2 (rep 2)H2O2",
                "LRRK2 (rep 3)H2O2","LRRK2 (rep 1)H2O2+CCM","LRRK2 (rep 2)H2O2+CCM","LRRK2 (rep 3)H2O2+CCM","LRRK2 (rep 1)H2O2+UCM",
                "LRRK2 (rep 2)H2O2+UCM","LRRK2 (rep 3)H2O2+UCM","rs76763715_GBA_p.N370S","Full GBA gene sequencing", "SAA Positive - final")
  PPMI <- bio %>% filter(TESTNAME %in% bio_list) %>% select(-c("SEX","COHORT","TYPE","UNITS","PROJECTID","PI_NAME","PI_INSTITUTION","update_stamp"))
  colnames(PPMI)[1:4] <- c("Patient_Number","Visit_ID","Bio_TESTNAME","Bio_TESTVALUE")
  PPMI <- PPMI %>% mutate(RUNDATE = as.Date(RUNDATE,"%Y-%m-%d")) %>% arrange(Patient_Number,Visit_ID,Bio_TESTNAME)
  ## CLEANING DUPLICATES ##
  # For any duplicates, we take the first row which corresponds to the earliest date of bio testing
  # These values had duplicates sometimes: "DJ-1", "SNCA-007", "SNCA-3UTR-1", "SNCA-3UTR-2", "SNCA-E3E4", "SNCA-E4E6", "CSF Alpha-synuclein", "ABeta 1-42", "pTau", "tTau"
  PPMI_cor <- PPMI %>% group_by(Patient_Number, Visit_ID, Bio_TESTNAME) %>% 
    mutate(duped_n = n()) %>% 
    filter(duped_n > 1) %>% arrange(RUNDATE) %>% 
    mutate(keep = case_when(row_number()==1 ~ TRUE)) %>% 
    filter(keep == TRUE)
  
  # mutate(keep = case_when(duped_n==2 & row_number()==2 ~ TRUE,
  #                       duped_n==3 & row_number()==3 ~ TRUE)) %>% 
  # filter(keep == TRUE) #code to take the last one instead
  
  PPMI_cor <- PPMI_cor %>% select(-c(length(PPMI_cor),length(PPMI_cor)-1,length(PPMI_cor)-2))
  PPMI <- PPMI %>% select(-RUNDATE)
  PPMI <- PPMI %>% filter(!(paste0(Patient_Number,Visit_ID,Bio_TESTNAME) %in% paste0(PPMI_cor$Patient_Number,PPMI_cor$Visit_ID,PPMI_cor$Bio_TESTNAME)))
  PPMI <- rbind(PPMI,PPMI_cor) %>% arrange(Patient_Number,Visit_ID)
  PPMI <- PPMI %>% spread(Bio_TESTNAME,Bio_TESTVALUE)
  # Add numbers to NFL columns
  colnames(PPMI)[which(colnames(PPMI)=="NfL")] <- "NfL_1"
  colnames(PPMI)[which(colnames(PPMI)=="NFL")] <- "NFL_2"
  # Merge ApoE and APOE columns
  PPMI <- PPMI %>% mutate(ApoE_Genotype = coalesce(PPMI$`ApoE Genotype`,PPMI$`APOE GENOTYPE`)) %>% select(-c(`APOE GENOTYPE`,`ApoE Genotype`)) %>% 
    mutate(ApoE_Genotype_1 = substr(ApoE_Genotype,2,2), ApoE_Genotype_2 = substr(ApoE_Genotype,5,5))
  write.csv(PPMI, "../Data_Wide/Current_Biospecimen_Analysis_Results_wide.csv", row.names=FALSE)
  cat("\014")
  
  
  
  
  ######## LEDD_Concomitant_Medication_Log.csv ########
  LEDD_Concomitant_Medication_Log <- read.csv(paste0(raw_path,"LEDD_Concomitant_Medication_Log_",download_date,".csv"), sep=",", header = T)
  # Tidy up date formats
  LEDD_Concomitant_Medication_Log <- LEDD_Concomitant_Medication_Log %>% 
    mutate(STARTDT = as.Date(paste("01/", as.character(STARTDT)), "%d/%m/%Y")) %>% 
    mutate(STOPDT = as.Date(paste("01/", as.character(STOPDT)), "%d/%m/%Y"))
  # Get the set of unique dates when LEDD changes for each PATNO
  LEDD <- unique(rbind(LEDD_Concomitant_Medication_Log %>% select(PATNO, STARTDT),
                       LEDD_Concomitant_Medication_Log %>% select(PATNO, STARTDT = STOPDT) %>%
                         filter(!is.na(STARTDT)))) %>% arrange (PATNO, STARTDT)
  # Get the base L-Dopa value at each point in time
  LDOPA_VALUES <- LEDD_Concomitant_Medication_Log[str_detect(LEDD_Concomitant_Medication_Log$LEDTRT,regex("LEV|DOPA|RYTARY|MADOPAR|SINIMET",ignore_case=TRUE)),] %>%
    select(PATNO,STARTDT,STOPDT,LEDD) %>%
    mutate(LEDD2 = case_when (is.na(LEDD) ~ 0.0, str_detect (LEDD, "LD") ~ 0.0, TRUE ~ as.double(as.character(LEDD))))
  LEDD <- left_join (LEDD, left_join(LEDD, LDOPA_VALUES, by="PATNO") %>% 
                       filter(STARTDT.y <= STARTDT.x & (is.na(STOPDT) | (STOPDT > STARTDT.x & STOPDT > STARTDT.y))) %>%
                       mutate (LEDD2 = case_when (is.na(LEDD2) ~ as.double (0.0), TRUE ~ LEDD2)) %>%
                       group_by(PATNO,STARTDT.x) %>%
                       summarize (LDOPA = sum(LEDD2), .groups = "drop") %>%
                       rename (STARTDT = STARTDT.x), by = c("PATNO","STARTDT"))
  # Get the base LEDD value at each point in time
  LEDD_VALUES <- mutate(LEDD_Concomitant_Medication_Log, LEDD2 = case_when (is.na(LEDD) ~ 0.0, str_detect (LEDD, "LD") ~ 0.0, TRUE ~ as.numeric(as.character(LEDD))))
  LEDD <- left_join (LEDD,
                     left_join(LEDD, LEDD_VALUES, by="PATNO") %>%
                       filter(STARTDT.y <= STARTDT.x & (is.na(STOPDT) | (STOPDT > STARTDT.x & STOPDT > STARTDT.y)) & !is.na(LEDD2)) %>%
                       group_by(PATNO,STARTDT.x) %>%
                       summarize (LEDD = sum(LEDD2), .groups = "drop") %>%
                       rename (STARTDT = STARTDT.x), by = c("PATNO","STARTDT"))
  # Update base LEDD value for drugs with LEDD profile of the form LD * A
  VALUES_TO_UPDATE <- LEDD_Concomitant_Medication_Log %>%
    filter (substr(LEDD,1,4) =="LD x") %>%
    select (PATNO, STARTDT,STOPDT, LEDD) %>%
    mutate (FACTOR = as.double(str_match(LEDD, "\\d+\\.\\d+$")))
  LEDD <- left_join (LEDD, left_join(LEDD, VALUES_TO_UPDATE, by="PATNO") %>%
                       filter(STARTDT.y <= STARTDT.x & (is.na(STOPDT) | (STOPDT > STARTDT.x & STOPDT > STARTDT.y))) %>%
                       group_by(PATNO,STARTDT.x) %>%
                       summarize (FACTOR1 = sum(FACTOR), .groups = "drop") %>%
                       rename (STARTDT = STARTDT.x), by = c("PATNO","STARTDT"))
  # Update base LEDD value for drugs with LEDD profile of the form (B + LD) x A
  VALUES_TO_UPDATE <- LEDD_Concomitant_Medication_Log %>%
    filter (str_detect(LEDD,"\\+\\ LD")) %>%
    select (PATNO, STARTDT,STOPDT, LEDD) %>%
    mutate (FACTOR2 = as.double(str_match(LEDD, "\\d+\\.\\d+")),
            FACTOR3 = as.double(str_match(LEDD, "\\d+\\.\\d+$")),
            FACTOR4 = as.double(str_match(LEDD, "\\d+\\.\\d+$")))
  LEDD <- left_join (LEDD, left_join(LEDD, VALUES_TO_UPDATE, by="PATNO") %>%
                       filter(STARTDT.y <= STARTDT.x & (is.na(STOPDT) | (STOPDT > STARTDT.x & STOPDT > STARTDT.y))) %>%
                       group_by(PATNO,STARTDT.x) %>%
                       summarize (FACTOR2 = sum(FACTOR2), FACTOR3 = mean(FACTOR3),FACTOR4 = sum(FACTOR4), .groups = "drop") %>% 
                       rename (STARTDT = STARTDT.x), by = c("PATNO","STARTDT"))
  # Tidy up null values and make final calculations
  LEDD <- mutate (LEDD, LDOPA = case_when(is.na(LDOPA) ~ 0.0, TRUE ~ LDOPA), LEDD = case_when(is.na(LEDD) ~ 0.0, TRUE ~ LEDD))
  LEDD <- mutate (LEDD, LEDD = LEDD
                  + case_when(!is.na(FACTOR1) ~ FACTOR1 * LDOPA, TRUE ~ 0.0)
                  + case_when(!is.na(FACTOR2) ~ (FACTOR2 * FACTOR3) + (LDOPA * FACTOR4), TRUE ~ 0.0)) %>%
    select (-FACTOR1, -FACTOR2, -FACTOR3, -FACTOR4)
  # Save
  colnames(LEDD) <- c("Patient_Number","Medication_Start_Date","LDOPA","LEDD")
  write.csv(LEDD, "../Data_Wide/LEDD_Concomitant_Medication_Log_wide.csv", row.names=FALSE)
  cat("\014")
  
  
  
  
  ######## Hoehn_Yahr_Stage.csv ########
  # Load tables into data frames
  MDS_UPDRS_Part_1 <- read_csv (paste0(raw_path,"MDS-UPDRS_Part_I_", download_date,".csv"))
  MDS_UPDRS_Part_1P <- read_csv (paste0(raw_path,"MDS-UPDRS_Part_I_Patient_Questionnaire_", download_date,".csv"))
  MDS_UPDRS_Part_2P <- read_csv (paste0(raw_path,"MDS_UPDRS_Part_II__Patient_Questionnaire_", download_date,".csv"))
  MDS_UPDRS_Part_3 <- read_csv (paste0(raw_path,"MDS-UPDRS_Part_III_", download_date,".csv"))
  # Fix date formats
  MDS_UPDRS_Part_1 <- mutate(MDS_UPDRS_Part_1, INFODT = as.Date(paste("01/", as.character(INFODT)), "%d/%m/%Y"))
  MDS_UPDRS_Part_1P <- mutate(MDS_UPDRS_Part_1P, INFODT = as.Date(paste("01/", as.character(INFODT)), "%d/%m/%Y"))
  MDS_UPDRS_Part_2P <- mutate(MDS_UPDRS_Part_2P, INFODT = as.Date(paste("01/", as.character(INFODT)), "%d/%m/%Y"))
  MDS_UPDRS_Part_3 <- mutate(MDS_UPDRS_Part_3, INFODT = as.Date(paste("01/", as.character(INFODT)), "%d/%m/%Y"))
  # Join the data frames filtering on a single participant and take the maximum scores across each INFODT/EVENT_ID
  # create a dataframe to keep all the results
  all_participant_HY_results <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(all_participant_HY_results) <- c("INFODT", "EVENT_ID", "HY_Stage", "Patient_ID")
  # go through each participant and calculate
  all_IDs <- unique(MDS_UPDRS_Part_3$PATNO)
  for (current_patient_ID in all_IDs) {
    current_participant_HY <- filter(MDS_UPDRS_Part_1, PATNO == current_patient_ID) %>%
      inner_join (MDS_UPDRS_Part_1P, by = c("PATNO","INFODT","EVENT_ID")) %>%
      inner_join (MDS_UPDRS_Part_2P, by = c("PATNO","INFODT","EVENT_ID")) %>%
      inner_join (MDS_UPDRS_Part_3, by = c("PATNO","INFODT","EVENT_ID")) %>%
      filter(!is.na(NP1RTOT), !is.na(NP1PTOT) & !is.na(NP2PTOT) & !is.na(NP3TOT) &
               !is.na(NHY)) %>%
      select (INFODT, EVENT_ID, NP1RTOT, NP1PTOT, NP2PTOT, NP3TOT, NHY) %>%
      group_by (INFODT, EVENT_ID) %>%
      summarize (Part1 = max(NP1RTOT), Part1P = max(NP1PTOT), Part2 = max(NP2PTOT), Part3
                 = max(NP3TOT), HY_Stage = max(NHY)) %>%
      arrange (INFODT, EVENT_ID)
    
    current_participant_HY <- current_participant_HY %>% select(INFODT, EVENT_ID, HY_Stage)
    current_participant_HY$Patient_ID <- current_patient_ID
    
    all_participant_HY_results <- rbind(all_participant_HY_results, current_participant_HY)
    
    rm(current_participant_HY)
  }
  colnames(all_participant_HY_results) <- c("Visit_Date_asDate", "Visit_ID", "HY_Stage", "Patient_Number")
  all_participant_HY_results <- all_participant_HY_results %>% select(Patient_Number, Visit_ID, Visit_Date_asDate, HY_Stage)
  all_participant_HY_results <- as.data.frame(all_participant_HY_results)
  all_participant_HY_results$Visit_Date <- format(all_participant_HY_results$Visit_Date_asDate, "%m/%Y") 
  all_participant_HY_results <- all_participant_HY_results %>% select(Patient_Number, Visit_ID, Visit_Date, Visit_Date_asDate, HY_Stage) %>% arrange(Patient_Number,Visit_Date_asDate)
  # Save
  write.csv(all_participant_HY_results, '../Data_Wide/Hoehn_Yahr_Stage_wide.csv', row.names = FALSE)
  rm(list = ls())
  cat("\014")
  
  
  return("COMPLETED STEP 1: Raw PPMI data turned to Wide Version")
}


