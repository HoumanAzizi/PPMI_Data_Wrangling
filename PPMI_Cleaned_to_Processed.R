# by Houman Azizi
# Uses Cleaned/Combined behavioral data -> does final calculations, removes extra columns, and renames columns
# Adds CNOs as well


PPMI_Cleaned_to_Processed <- function(folder_path) {
  
  
  setwd(folder_path)
  
  
  PPMI <- read.csv("PPMI_allVisits_cleaned.csv", sep=",", header = T)
  
  
  # Calculating New Values
  
  # Symptom duration in YEARS
  PPMI$PDDXDT <- as.Date(paste0("01/",PPMI$PDDXDT),"%d/%m/%Y")
  PPMI$SXDT <- as.Date(paste0("01/",PPMI$SXDT),"%d/%m/%Y")
  Symptom_duration <- difftime(PPMI$Visit_Date_asDate,PPMI$SXDT, units = "days")
  Symptom_duration <- Symptom_duration/365 #previously /(3600*24*365)
  Symptom_duration <- as.numeric(Symptom_duration)
  PD_diag_duration_Years <- difftime(PPMI$Visit_Date_asDate,PPMI$PDDXDT, units = "days")
  PD_diag_duration_Years <- PD_diag_duration_Years/365 #previously /(3600*24*365)
  PD_diag_duration_Years <- as.numeric(PD_diag_duration_Years)
  
  BP_Sys_drop <- PPMI$SYSSUP - PPMI$SYSSTND
  
  Tremor_score_ON <- rowMeans(PPMI[,c("Tremor2_Calculated","Tremor3_Calculated_ON")]) # not doing 'na.rm = TRUE' anymore so only proper values are calculated
  Tremor_score_OFF <- rowMeans(PPMI[,c("Tremor2_Calculated","Tremor3_Calculated_OFF")])
  Tremor_score_No_Medication <- rowMeans(PPMI[,c("Tremor2_Calculated","Tremor3_Calculated_No_Medication")])
  
  PIGD_score_ON <- rowMeans(PPMI[,c("PIGD2_Calculated","PIGD3_Calculated_ON")])
  PIGD_score_OFF <- rowMeans(PPMI[,c("PIGD2_Calculated","PIGD3_Calculated_OFF")])
  PIGD_score_No_Medication <- rowMeans(PPMI[,c("PIGD2_Calculated","PIGD3_Calculated_No_Medication")])
  
  PPMI <- PPMI %>% select(-c("Tremor2_Calculated","PIGD2_Calculated"), -starts_with('Tremor3_Calculated_'), -starts_with('PIGD3_Calculated_'))
  
  
  
  #CALCULATE NUPDRS TOTAL HERE
  UPDRS_PartI <- PPMI$NP1TOT_Calculated
  UPDRS_PartII <- PPMI$NP2TOT_Calculated
  
  UPDRS_PartIII_ON <- PPMI$NP3TOT_ON
  UPDRS_PartIII_OFF <- PPMI$NP3TOT_OFF
  UPDRS_PartIII_No_Medication <- PPMI$NP3TOT_No_Medication
  
  UPDRS_PartIII_Calculated_ON <- PPMI$NP3TOT_Calculated_ON
  UPDRS_PartIII_Calculated_OFF <- PPMI$NP3TOT_Calculated_OFF
  UPDRS_PartIII_Calculated_No_Medication <- PPMI$NP3TOT_Calculated_No_Medication
  
  UPDRS_Total_Score_ON <- rowSums(cbind(UPDRS_PartI, UPDRS_PartII, UPDRS_PartIII_ON))
  UPDRS_Total_Score_OFF <- rowSums(cbind(UPDRS_PartI, UPDRS_PartII, UPDRS_PartIII_OFF))
  UPDRS_Total_Score_No_Medication <- rowSums(cbind(UPDRS_PartI, UPDRS_PartII, UPDRS_PartIII_No_Medication))
  
  UPDRS_PartIII_Left_ON <- PPMI$NP3TOT_L_ON
  UPDRS_PartIII_Left_OFF <- PPMI$NP3TOT_L_OFF
  UPDRS_PartIII_Left_No_Medication <- PPMI$NP3TOT_L_No_Medication
  
  UPDRS_PartIII_Right_ON <- PPMI$NP3TOT_R_ON
  UPDRS_PartIII_Right_OFF <- PPMI$NP3TOT_R_OFF
  UPDRS_PartIII_Right_No_Medication <- PPMI$NP3TOT_R_No_Medication
  
  Apathy <- PPMI$Apathy
  
  
  RBD_Score <- rowSums((PPMI[,c("DRMVIVID","DRMAGRAC","DRMNOCTB","SLPLMBMV","SLPINJUR","DRMVERBL","DRMFIGHT","DRMUMV","DRMOBJFL","MVAWAKEN","DRMREMEM","SLPDSTRB")]))
  RBD_Score <- RBD_Score + (rowSums(PPMI[,c("STROKE","HETRA","PARKISM","RLS","NARCLPSY","DEPRS","EPILEPSY","BRNINFM","CNSOTH")],na.rm = T)>1)
  
  
  
  
  # Calculate UPSIT Scores
  UPSIT_Score <- PPMI$UPSIT_TOTAL_CORRECT
  source(paste0(folder_path,"UPSIT_to_percentile.R"))
  for (i in 1:length(UPSIT_Score)) {
    UPSIT_Score[i] <- UPSIT_to_percentile(UPSIT_Score[i],PPMI$Age_at_Visit[i],PPMI$SEX_n[i],folder_path)
  }
  
  
  
  # SCOPA Calculations -> changing never and NAs to 0
  SCOPA1_21 <- c("SCAU1", "SCAU2", "SCAU3", "SCAU4", "SCAU5", "SCAU6", "SCAU7", "SCAU8", "SCAU9", "SCAU10", 
                 "SCAU11", "SCAU12", "SCAU13", "SCAU14", "SCAU15", "SCAU16", "SCAU17", "SCAU18", "SCAU19", "SCAU20", "SCAU21")
  SCOPA22_25 <- c("SCAU22", "SCAU23", "SCAU23A", "SCAU24", "SCAU25")
  PPMI[,SCOPA1_21] <- replace(PPMI[,SCOPA1_21],PPMI[,SCOPA1_21]==9,3)
  PPMI[,SCOPA22_25] <- replace(PPMI[,SCOPA22_25],PPMI[,SCOPA22_25]==9,0)
  PPMI[,SCOPA22_25] <- replace(PPMI[,SCOPA22_25],is.na(PPMI[,SCOPA22_25]),0)
  SCOPA_AUT_Score <- rowSums(PPMI[,c(SCOPA1_21,SCOPA22_25)])
  
  # STAI Calculations
  STAI_positive <- c("STAIAD1", "STAIAD2", "STAIAD5", "STAIAD8", "STAIAD10", "STAIAD11", "STAIAD15", 
                     "STAIAD16", "STAIAD19", "STAIAD20", "STAIAD21", "STAIAD23", "STAIAD26", "STAIAD27", 
                     "STAIAD30", "STAIAD33", "STAIAD34", "STAIAD36", "STAIAD39")
  STAI_state <- c("STAIAD1", "STAIAD2", "STAIAD3", "STAIAD4", "STAIAD5", "STAIAD6", "STAIAD7", "STAIAD8", "STAIAD9", "STAIAD10", 
                  "STAIAD11", "STAIAD12", "STAIAD13", "STAIAD14", "STAIAD15", "STAIAD16", "STAIAD17", "STAIAD18", "STAIAD19", "STAIAD20")
  STAI_trait <- c("STAIAD21", "STAIAD22", "STAIAD23", "STAIAD24", "STAIAD25", "STAIAD26", "STAIAD27", "STAIAD28", "STAIAD29", "STAIAD30",       
                  "STAIAD31", "STAIAD32", "STAIAD33", "STAIAD34", "STAIAD35", "STAIAD36", "STAIAD37", "STAIAD38", "STAIAD39", "STAIAD40")
  PPMI[,STAI_positive] <- 5 - PPMI[,STAI_positive]
  STAI_State_Score <- rowSums(PPMI[,STAI_state]) #changed first 20 STAI for State
  STAI_Trait_Score <- rowSums(PPMI[,STAI_trait]) #changed next 20 for Trait
  STAI_Total_Score <- STAI_State_Score + STAI_Trait_Score
  
  
  
  
  PPMI[,c("GDSSATIS","GDSGSPIR","GDSHAPPY","GDSALIVE","GDSENRGY")] <- 1 - PPMI[,c("GDSSATIS","GDSGSPIR","GDSHAPPY","GDSALIVE","GDSENRGY")]
  GDS_Score <- rowSums(PPMI[,c("GDSSATIS","GDSDROPD","GDSEMPTY","GDSBORED","GDSGSPIR","GDSAFRAD","GDSHAPPY","GDSHLPLS","GDSHOME","GDSMEMRY",
                               "GDSALIVE","GDSWRTLS","GDSENRGY","GDSHOPLS","GDSBETER")])
  
  
  MOCA_adjusted_Score <- PPMI[,"MCATOT"]
  MOCA_adjusted_Score[which(PPMI[,"EDUCYRS"]<12 & PPMI[,"MCATOT"]<30)] <- MOCA_adjusted_Score[which(PPMI[,"EDUCYRS"]<12 & PPMI[,"MCATOT"]<30)]+1
  
  
  ESS_total <- rowSums(PPMI[, c("ESS1", "ESS2", "ESS3", "ESS4", "ESS5", "ESS6", "ESS7", "ESS8")])
  
  
  QUIP_Gambling <- rowSums(PPMI[,c("CNTRLGMB", "TMGAMBLE")]);QUIP_Gambling <- replace(QUIP_Gambling,QUIP_Gambling==2,1)
  QUIP_Sex  <- rowSums(PPMI[,c("CNTRLSEX", "TMSEX")]);QUIP_Sex  <- replace(QUIP_Sex ,QUIP_Sex ==2,1)
  QUIP_Buying  <- rowSums(PPMI[,c("CNTRLBUY", "TMBUY")]);QUIP_Buying  <- replace(QUIP_Buying ,QUIP_Buying ==2,1)
  QUIP_Eating  <- rowSums(PPMI[,c("CNTRLEAT", "TMEAT")]);QUIP_Eating  <- replace(QUIP_Eating ,QUIP_Eating ==2,1)
  QUIP_Others  <- rowSums(PPMI[,c("TMTORACT", "TMTMTACT", "TMTRWD")]);QUIP_Others  <- replace(QUIP_Others ,QUIP_Others ==2,1)
  QUIP_Total <- rowSums(cbind(QUIP_Gambling,QUIP_Sex,QUIP_Buying,QUIP_Eating,QUIP_Others))
  
  
  # Adding calculated data
  PPMI$BP_Sys_drop <- BP_Sys_drop
  PPMI$Symptom_duration_years <- Symptom_duration
  PPMI$PD_diag_duration_years <- PD_diag_duration_Years
  PPMI$Tremor_score_ON <- Tremor_score_ON
  PPMI$Tremor_score_OFF <- Tremor_score_OFF
  PPMI$Tremor_score_No_Medication <- Tremor_score_No_Medication
  PPMI$PIGD_score_ON <- PIGD_score_ON
  PPMI$PIGD_score_OFF <- PIGD_score_OFF
  PPMI$PIGD_score_No_Medication <- PIGD_score_No_Medication
  PPMI$UPDRS_PartI <- UPDRS_PartI
  PPMI$UPDRS_PartII <- UPDRS_PartII
  PPMI$UPDRS_PartIII_ON <- UPDRS_PartIII_ON
  PPMI$UPDRS_PartIII_OFF <- UPDRS_PartIII_OFF
  PPMI$UPDRS_PartIII_No_Medication <- UPDRS_PartIII_No_Medication
  PPMI$UPDRS_Total_Score_ON <- UPDRS_Total_Score_ON
  PPMI$UPDRS_Total_Score_OFF <- UPDRS_Total_Score_OFF
  PPMI$UPDRS_Total_Score_No_Medication <- UPDRS_Total_Score_No_Medication
  PPMI$UPDRS_PartIII_Left_ON <- UPDRS_PartIII_Left_ON
  PPMI$UPDRS_PartIII_Left_OFF <- UPDRS_PartIII_Left_OFF
  PPMI$UPDRS_PartIII_Left_No_Medication <- UPDRS_PartIII_Left_No_Medication
  PPMI$UPDRS_PartIII_Right_ON <- UPDRS_PartIII_Right_ON
  PPMI$UPDRS_PartIII_Right_OFF <- UPDRS_PartIII_Right_OFF
  PPMI$UPDRS_PartIII_Right_No_Medication <- UPDRS_PartIII_Right_No_Medication
  PPMI$RBD_Score <- RBD_Score
  PPMI$Apathy_Score <- Apathy
  PPMI$UPSIT_Score <- UPSIT_Score
  PPMI$SCOPA_AUT_Score <- SCOPA_AUT_Score
  PPMI$STAI_State_Score <- STAI_State_Score
  PPMI$STAI_Trait_Score <- STAI_Trait_Score
  PPMI$STAI_Total_Score <- STAI_Total_Score
  PPMI$GDS_Score <- GDS_Score
  PPMI$MOCA_adjusted_Score <- MOCA_adjusted_Score
  PPMI$QUIP_Total <- QUIP_Total
  PPMI$ESS_total <- ESS_total
  
  
  # Add Subgroups
  subcohort_names <- c("ENRLPINK1", "ENRLPRKN", "ENRLSRDC", "ENRLHPSM", "ENRLRBD", "ENRLLRRK2", "ENRLSNCA", "ENRLGBA", "Cohort")
  # Change all NAs into 0
  for (subcohort in subcohort_names) {
    PPMI[[subcohort]][is.na(PPMI[[subcohort]])] <- 0
  }
  sub_cohort <- as.data.frame(PPMI[,subcohort_names])
  sub_cohort <- sub_cohort %>% 
    mutate(Sub_Cohort_Regular_tmp = case_when(ENRLPINK1 == 1 | 
                                            ENRLPRKN == 1 | 
                                            ENRLLRRK2 == 1 | 
                                            ENRLSNCA == 1 | 
                                            ENRLGBA == 1 ~ "Genetic",
                                          ENRLRBD == 1 ~ "RBD",
                                          ENRLHPSM == 1 ~ "Hyposmia")) %>% 
    mutate(Sub_Cohort_Detailed_tmp = case_when(ENRLPINK1 == 1 ~ "Pink1",
                                           ENRLPRKN == 1 ~ "Parkin", 
                                           ENRLLRRK2 == 1 ~ "LRRK2",
                                           ENRLSNCA == 1 ~ "SNCA",
                                           ENRLGBA == 1 ~ "GBA",
                                           ENRLRBD == 1 ~ "RBD",
                                           ENRLHPSM == 1 ~ "Hyposmia")) %>% 
    mutate(Sub_Cohort_Regular = case_when(!is.na(Sub_Cohort_Regular_tmp) ~ paste0(Cohort,"_",Sub_Cohort_Regular_tmp),
                                          TRUE ~ Cohort)) %>%
    mutate(Sub_Cohort_Detailed = case_when(!is.na(Sub_Cohort_Detailed_tmp) ~ paste0(Cohort,"_",Sub_Cohort_Detailed_tmp),
                                          TRUE ~ Cohort))
  # Add new columns
  PPMI$Sub_Cohort_Regular <- sub_cohort$Sub_Cohort_Regular
  PPMI$Sub_Cohort_Detailed <- sub_cohort$Sub_Cohort_Detailed
  # End of "Add Subgroups"
  
  
  
  
  # NOT available columns: CNO, DXOTHCM, DOMSIDE
  # These ones are available separately in the bio file: "Abeta.42","CSF.Alpha.synuclein","p.Tau181P","Total.tau","rs34637584_LRRK2_p.G2019S","rs76763715_GBA_p.N370S"
  PPMI <- PPMI %>% select("Patient_Number", "Visit_ID", "Visit_Date_asDate", "Age_at_Visit", "BL_Age", "BIRTHDT_asDate",
                          "BL_Date", "Days_from_BL", "Cohort", "Sub_Cohort_Regular", "Sub_Cohort_Detailed", "SEX", "EDUCYRS", "HANDED",
                          "UPDRS_PartI", "UPDRS_PartII", 
                          "UPDRS_PartIII_ON", "UPDRS_PartIII_OFF", "UPDRS_PartIII_No_Medication",
                          "UPDRS_PartIII_Left_ON", "UPDRS_PartIII_Left_OFF", "UPDRS_PartIII_Left_No_Medication",
                          "UPDRS_PartIII_Right_ON", "UPDRS_PartIII_Right_OFF", "UPDRS_PartIII_Right_No_Medication",
                          "UPDRS_Total_Score_ON", "UPDRS_Total_Score_OFF", "UPDRS_Total_Score_No_Medication",
                          "UPDRS3_Time_Between_Examination_and_LastDose_ON", "UPDRS3_Time_Between_Examination_and_LastDose_OFF", "UPDRS3_Time_Between_Examination_and_LastDose_No_Medication", 
                          "UPDRS3_Time_of_Medication_LastDose_ON", "UPDRS3_Time_of_Medication_LastDose_OFF", "UPDRS3_Time_of_Medication_LastDose_No_Medication", 
                          "UPDRS3_Time_of_Examination_ON", "UPDRS3_Time_of_Examination_OFF", "UPDRS3_Time_of_Examination_No_Medication", 
                          "Tremor_score_ON", "Tremor_score_OFF", "Tremor_score_No_Medication",
                          "PIGD_score_ON", "PIGD_score_OFF", "PIGD_score_No_Medication",
                          "MSEADLG", "ESS_total", "GDS_Score", "MCATOT", "MOCA_adjusted_Score", "QUIP_Total", "RBD_Score", "Apathy_Score", 
                          "SCOPA_AUT_Score", "STAI_State_Score", "STAI_Trait_Score", "STAI_Total_Score", "UPSIT_Score", 'HY_Stage',
                          "JLO_TOTRAW", "JLO_TOTCALC", "DVS_JLO_MSSA", "DVS_JLO_MSSAE", 
                          "DVT_TOTAL_RECALL", "DVT_DELAYED_RECALL", "DVT_RETENTION", "DVT_RECOG_DISC_INDEX",
                          "LNS_TOTRAW", "DVS_LNS", "DVS_SFTANIM", "DVT_SFTANIM",
                          "DVSD_SDM", "DVT_SDM",
                          "Cognitive_Change",
                          "COGDECLN", "FNCDTCOG", "COGSTATE", "COGDXCL", "RVWNPSY",
                          "Albumin.QT", "Alkaline.Phosphatase.QT", "ALT..SGPT.", "AST..SGOT.", "Serum.Glucose", "Serum.Uric.Acid",
                          "Total.Protein", "Urea.Nitrogen", # "TOPRRSLT", "TGLCRSLT", -> 2 measures removed
                          "WGTKG", "HTCM", "TEMPC", "DIASUP", "HRSUP", "DIASTND", "HRSTND", "BP_Sys_drop", 
                          "SXDT", "Symptom_duration_years", "PD_diag_duration_years", "DXTREMOR", "DXRIGID", "DXBRADY", "DXPOSINS", "DXOTHSX",
                          "HISPLAT", "RAINDALS", "RAASIAN", "RABLACK", "RAHAWOPI", "RAWHITE", "BIOMOMPD", "BIODADPD",
                          "FULSIBPD", "HAFSIBPD", "MAGPARPD", "PAGPARPD", "MATAUPD", "PATAUPD",
                          "Cohort_n", "SEX_n",
                          "SC_Delay_Days", "Blood_Delay_Days", "Vital_Delay_Days", "UPDRS1_Delay_Days", "UPDRS2_Delay_Days", "UPDRS3_Delay_Days", # removed "Lumbar_Delay_Days",
                          "MSE_Delay_Days", "ESS_Delay_Days", "GDS_Delay_Days", "MOCA_Delay_Days", "QUIP_Delay_Days", "REM_Delay_Days", "SCOPA_Delay_Days", "STAI_Delay_Days",
                          "UPSIT_Delay_Days", "JLO_Delay_Days", "DVT_Delay_Days", "LNS_Delay_Days", "SFT_Delay_Days", "SDM_Delay_Days", "COG_Delay_Days", 'HY_Delay_Days')
  
  
  # DayDiff_Visit_Baseline -> Day_Diff (Shows day difference of this visit from Baseline)
  new_names <- c("Patient_ID", "Visit_ID", "Visit_Date", "Age", "Age_Baseline", "Birthdate",
                 "Baseline_Date", "DayDiff", "Cohort", "Sub_Cohort_Regular", "Sub_Cohort_Detailed", "Sex" , "Education_Years", "Handedness",
                 "UPDRS_Part_I", "UPDRS_Part_II", 
                 "UPDRS_Part_III_ON", "UPDRS_Part_III_OFF", "UPDRS_Part_III_No_Medication",
                 "UPDRS_Part_III_Left_ON", "UPDRS_Part_III_Left_OFF", "UPDRS_Part_III_Left_No_Medication",
                 "UPDRS_Part_III_Right_ON", "UPDRS_Part_III_Right_OFF", "UPDRS_Part_III_Right_No_Medication",
                 "UPDRS_Total_Score_ON", "UPDRS_Total_Score_OFF", "UPDRS_Total_Score_No_Medication",
                 "UPDRS_Part_III_Time_Between_Examination_and_LastDose_ON", "UPDRS_Part_III_Time_Between_Examination_and_LastDose_OFF", "UPDRS_Part_III_Time_Between_Examination_and_LastDose_No_Medication", 
                 "UPDRS_Part_III_Time_of_Medication_LastDose_ON", "UPDRS_Part_III_Time_of_Medication_LastDose_OFF", "UPDRS_Part_III_Time_of_Medication_LastDose_No_Medication", 
                 "UPDRS_Part_III_Time_of_Examination_ON", "UPDRS_Part_III_Time_of_Examination_OFF", "UPDRS_Part_III_Time_of_Examination_No_Medication", 
                 "Tremor_ON", "Tremor_OFF", "Tremor_No_Medication",
                 "PIGD_ON", "PIGD_OFF", "PIGD_No_Medication",
                 "Schwab_England", "Epworth", "GDS", "MOCA", "MOCA_adjusted", "QUIP", "RBD_Score", "Apathy_Score",
                 "SCOPA_AUT", "STAI_State", "STAI_Trait", "STAI_Total", "UPSIT_Score", 'HY_Stage',
                 "Benton_Line_Sum", "Benton_Line_Calculated_Sum", "Benton_MOANS_Age", "Benton_MOANS_Age_educ",
                 "HVLT_Total_Recall", "HVLT_Delayed_Recall", "HVLT_Retention", "HVLT_Recog_Discrim", 
                 "LNS", "LNS_Scaled", "Semantic_Fluency_Scaled", "Semantic_Fluency", 
                 "Symbol_Digit_SD", "Symbol_Digit",
                 "Cognitive_Change",
                 "Cognitive_Decline_Experienced", "Cognitive_Functional_Decline", "Cognitive_State", "Cognitive_Diagnosis_Confidence.level", "Rev_Neuropsych_Test",
                 "Blood_Albumin", "Blood_ALK_P", "Blood_ALT", "Blood_AST", "Blood_Glucose", "Blood_Uric_Acid",
                 "Blood_Total_Protein", "Blood_Urea_Nitrogen", # "Blood_Total_Protein_CSF", "Blood_Total_Glocuse_CSF", -> 2 measures removed
                 "Weight", "Height", "Temperature", "BP_Dias_Sup", "HR_Sup", "BP_Dias_Stand", "HR_Stand", "BP_Sys_drop",
                 "Symptom_Date", "Symptom_duration_years", "PD_diag_duration_years", "Diag_Rest_Tremor", "Diag_Rigidity", "Diag_Bradykinesia", "Diag_Postural_Instability", "Diag_Other_Symptoms",
                 "Race_Hispanic", "Race_Indian", "Race_Asian", "Race_Black", "Race_Hawaiian", "Race_White", "PDHistory_Mother", "PDHistory_Father",
                 "PDHistory_Full_Siblings", "PDHistory_Half_Sibling", "PDHistory_Maternal_GP", "PDHistory_Paternal_GP", "PDHistory_Maternal_AU", "PDHistory_Paternal_AU",
                 "Cohort_Number", "Sex_Number",
                 "DayDiff_Screening_Baseline", "DayDiff_BloodCollection", "DayDiff_Vitals", "DayDiff_UPDRS_I", "DayDiff_UPDRS_II", "DayDiff_UPDRS_III", # removed "DayDiff_LumbarPunc",
                 "DayDiff_SchwabEng", "DayDiff_Epworth", "DayDiff_GDS", "DayDiff_MOCA", "DayDiff_QUIP", "DayDiff_REM", "DayDiff_SCOPA", "DayDiff_STAI",
                 "DayDiff_UPSIT", "DayDiff_BentonLine", "DayDiff_HVLT", "DayDiff_LNS", "DayDiff_SemanticFluency", "DayDiff_SymbolDigit", "DayDiff_Cognitive", 'DayDiff_HY_Stage')
  
  colnames(PPMI) <- new_names
  
  
  
  # Add CNOs
  Center <- read.csv("PPMI_CNO.csv")
  colnames(Center) <- c("Patient_ID", "Center_ID")
  PPMI <- PPMI %>% left_join(Center, by = c("Patient_ID")) %>% relocate(Center_ID, .after = Baseline_Date)
  # End of section
  
  
  # Add subtypings of PD
  PPMI <- PPMI %>% mutate(Clinical_Features_Motor_ON = UPDRS_Part_II + UPDRS_Part_III_ON + PIGD_ON) %>% relocate(Clinical_Features_Motor_ON, .after = UPDRS_Total_Score_No_Medication)
  PPMI <- PPMI %>% mutate(Clinical_Features_Motor_OFF = UPDRS_Part_II + UPDRS_Part_III_OFF + PIGD_OFF) %>% relocate(Clinical_Features_Motor_OFF, .after = UPDRS_Total_Score_No_Medication)
  PPMI <- PPMI %>% mutate(Clinical_Features_Motor_No_Medication = UPDRS_Part_II + UPDRS_Part_III_No_Medication + PIGD_No_Medication) %>% relocate(Clinical_Features_Motor_No_Medication, .after = UPDRS_Total_Score_No_Medication)
  PPMI <- PPMI %>% mutate(Clinical_Features_Cognition = MOCA_adjusted + Benton_Line_Calculated_Sum + Symbol_Digit + HVLT_Total_Recall +
                            HVLT_Delayed_Recall + HVLT_Retention + HVLT_Recog_Discrim + Semantic_Fluency + 
                            LNS) %>% relocate(Clinical_Features_Cognition, .after = UPDRS_Total_Score_No_Medication)
  PPMI <- PPMI %>% mutate(Clinical_Features_RBD = RBD_Score) %>% relocate(Clinical_Features_RBD, .after = UPDRS_Total_Score_No_Medication)
  PPMI <- PPMI %>% mutate(Clinical_Features_Dysautonomia = SCOPA_AUT) %>% relocate(Clinical_Features_Dysautonomia, .after = UPDRS_Total_Score_No_Medication)
  
  
  
  
  ######## Add LEDD and LDOPA for each date ########
  LEDD <- read.csv("Data_Wide/LEDD_Concomitant_Medication_Log_wide.csv", sep=",", header = T)
  colnames(LEDD)[1] <- 'Patient_ID'
  LEDD <- LEDD %>% mutate(Medication_Start_Date = as.Date(Medication_Start_Date, format = "%Y-%m-%d"))
  # add end date for each medication period to make comparisons easier
  LEDD <- LEDD %>%
    group_by(Patient_ID) %>%
    mutate(Next_Medication_Start_Date = lead(Medication_Start_Date, default = as.Date("9999-12-31"))) %>%
    ungroup()
  # add start and end date of medication for each participant
  LEDD <- LEDD %>% 
    group_by(Patient_ID) %>% 
    mutate(Initial_Medication_Date = min(Medication_Start_Date)) %>% 
    mutate(Final_Medication_Date = max(Next_Medication_Start_Date)) %>% 
    ungroup()
  # add LEDD and LDOPA in their places
  PPMI$LDOPA_Equivalent_LEDD <- NA
  PPMI$LDOPA_Dose <- NA
  PPMI$LDOPA_Medication_Start_Date <- as.Date(NA)
  PPMI$LDOPA_Medication_End_Date <- as.Date(NA)
  for (i in 1:nrow(PPMI)) {
    current_patient_id <- PPMI$Patient_ID[i]
    current_visit_date <- as.Date(PPMI$Visit_Date[i])
    # Filter LEDD for rows with the same Patient_ID and where Visit_Date falls within the range
    patient_ledd_data <- LEDD %>%
      filter(Patient_ID == current_patient_id,
             Medication_Start_Date <= current_visit_date,
             Next_Medication_Start_Date > current_visit_date)
    # If there is a matching row, update PPMI with LEDD values
    if (nrow(patient_ledd_data) == 1) {
      PPMI$LDOPA_Equivalent_LEDD[i] <- patient_ledd_data$LEDD
      PPMI$LDOPA_Dose[i] <- patient_ledd_data$LDOPA
      PPMI$LDOPA_Medication_Start_Date[i] <- patient_ledd_data$Initial_Medication_Date
      PPMI$LDOPA_Medication_End_Date[i] <- patient_ledd_data$Final_Medication_Date
    } else if (nrow(patient_ledd_data) > 1) {
      warning("More than one LEDD entry found for Patient_ID", current_patient_id, " on Visit_Date ", current_visit_date)
    }
    rm(current_patient_id, current_visit_date,patient_ledd_data)
  }
  # add LDOPA duration
  PPMI$LDOPA_Duration <- as.numeric(difftime(PPMI$Visit_Date, PPMI$LDOPA_Medication_Start_Date, units = 'days')/365)
  # Add LDOPA start date and end date to all rows of a subject.
  PPMI <- PPMI %>%
    group_by(Patient_ID) %>%
    mutate(LDOPA_Medication_Start_Date = 
             if(any(!is.na(LDOPA_Medication_Start_Date))) {
               first(na.omit(LDOPA_Medication_Start_Date))
             } else { NA }) %>% 
    mutate(LDOPA_Medication_End_Date = 
             if(any(!is.na(LDOPA_Medication_End_Date))) {
               first(na.omit(LDOPA_Medication_End_Date))
             } else { NA }) %>% ungroup()
  # Set LDOPA LEDD dose, LDOPA dose, and LDOPA duration to 0 if before the start date of LDOPA
  PPMI <- PPMI %>% 
    mutate(LDOPA_Equivalent_LEDD = case_when(is.na(LDOPA_Equivalent_LEDD) & 
                                                              !is.na(Visit_ID) & 
                                                              !is.na(LDOPA_Medication_Start_Date) & 
                                                              Visit_Date < LDOPA_Medication_Start_Date ~ 0,
                                                            TRUE ~ LDOPA_Equivalent_LEDD)) %>% 
    mutate(LDOPA_Dose = case_when(is.na(LDOPA_Dose) & 
                                    !is.na(Visit_ID) & 
                                    !is.na(LDOPA_Medication_Start_Date) & 
                                    Visit_Date < LDOPA_Medication_Start_Date ~ 0,
                                  TRUE ~ LDOPA_Dose)) %>% 
    mutate(LDOPA_Duration = case_when(is.na(LDOPA_Duration) & 
                                    !is.na(Visit_ID) & 
                                    !is.na(LDOPA_Medication_Start_Date) & 
                                    Visit_Date < LDOPA_Medication_Start_Date ~ 0,
                                  TRUE ~ LDOPA_Duration))
  # Set LDOPA LEDD dose, LDOPA dose, and LDOPA duration to 0 for all HC subjects
  PPMI <- PPMI %>% 
    mutate(LDOPA_Equivalent_LEDD = case_when(Cohort == 'HC' & 
                                                is.na(LDOPA_Equivalent_LEDD) ~ 0,
                                              TRUE ~ LDOPA_Equivalent_LEDD)) %>% 
    mutate(LDOPA_Dose = case_when(Cohort == 'HC' & 
                                     is.na(LDOPA_Dose) ~ 0,
                                   TRUE ~ LDOPA_Dose)) %>% 
    mutate(LDOPA_Duration = case_when(Cohort == 'HC' & 
                                         is.na(LDOPA_Duration) ~ 0,
                                       TRUE ~ LDOPA_Duration))
  # relocate columns
  PPMI <- PPMI %>% relocate(LDOPA_Equivalent_LEDD, .after = Rev_Neuropsych_Test)
  PPMI <- PPMI %>% relocate(LDOPA_Dose, .after = LDOPA_Equivalent_LEDD)
  PPMI <- PPMI %>% relocate(LDOPA_Medication_Start_Date, .after = LDOPA_Dose)
  PPMI <- PPMI %>% relocate(LDOPA_Medication_End_Date, .after = LDOPA_Medication_Start_Date)
  PPMI <- PPMI %>% relocate(LDOPA_Duration, .after = LDOPA_Medication_End_Date)
  
  
  
  
  ### Add LEDD medication list for each subject visit
  rm(LEDD)
  LEDD <- read.csv("Data_Wide/LEDD_Medication_List_wide.csv", sep=",", header = T)
  
  # Create LEDD_Matched from PPMI dataframe
  LEDD_Matched <- PPMI[, c("Patient_ID", "Visit_ID", "Visit_Date")]
  # fix dates
  LEDD_Matched$Visit_Date <- as.Date(LEDD_Matched$Visit_Date)
  LEDD$Start_date <- as.Date(LEDD$Start_date)
  LEDD$Stop_date <- as.Date(LEDD$Stop_date)
  
  # Create new columns in LEDD_Matched for each medication category, initialized with NA
  unique_med_categories <- unique(LEDD$Med_category)
  for (med_category in unique_med_categories) {
    column_name <- paste0("Medication_", med_category)
    LEDD_Matched[[column_name]] <- 0
  }
  
  ## Process each row in the LEDD dataframe
  for (i in 1:nrow(LEDD)) {
    current_patient <- LEDD$Patient_ID[i]
    current_med_category <- LEDD$Med_category[i]
    current_start_date <- LEDD$Start_date[i]
    current_stop_date <- LEDD$Stop_date[i]
    
    # Find matching rows in LEDD_Matched
    matched_rows <- which(LEDD_Matched$Patient_ID == current_patient & 
                            LEDD_Matched$Visit_Date >= current_start_date &
                            LEDD_Matched$Visit_Date <= current_stop_date)
    
    # If matches found, set the corresponding medication column to 1
    if (length(matched_rows) > 0) {
      column_name <- paste0("Medication_", current_med_category)
      LEDD_Matched[matched_rows, column_name] <- 1
    }
  }
  
  ## Add to the PPMI dataset
  LEDD_Matched <- LEDD_Matched %>% select(-Visit_Date)
  PPMI <- PPMI %>% left_join(LEDD_Matched, by = c("Patient_ID", "Visit_ID"))
  
  
  
  
  ### Add Age_Baseline, Baseline_Date, and DayDiff for subjects missing them
  PPMI <- PPMI %>%
    mutate(
      Age_Baseline = ifelse(is.na(Age_Baseline) & Visit_ID == 'BL', Age, Age_Baseline),
      Baseline_Date = ifelse(is.na(Baseline_Date) & Visit_ID == 'BL', Visit_Date, Baseline_Date),
      DayDiff = ifelse(is.na(DayDiff) & Visit_ID == 'BL', 0, DayDiff)
    )
  
  
  ### Impute values that can change only in 1 direction
  PPMI <- PPMI %>% arrange(Patient_ID, Visit_Date)
  
  ## Fixing HY_Stage
  # Rule 1: Fill NA values before HY_Stage = 0
  PPMI <- PPMI %>%
    group_by(Patient_ID) %>%
    mutate(
      first_nonNA_row = which(!is.na(HY_Stage))[1], # Find the first non-NA HY_Stage value
      is_first_zero = HY_Stage[first_nonNA_row] == 0, # Check if first non-NA value is 0
      is_before_first = row_number() < first_nonNA_row, # Mark rows before first non-NA value
      HY_Stage = ifelse(is_first_zero & is_before_first & is.na(HY_Stage), 0, HY_Stage)) %>% # Replace NA values before first 0 with 0
    select(-first_nonNA_row, -is_first_zero, -is_before_first) %>% ungroup()
  # Rule 2: Fill values between same HY_Stage values
  PPMI <- PPMI %>%
    group_by(Patient_ID) %>%
    mutate(
      row_num = row_number(),
      # Find next non-NA stage
      next_stage = sapply(row_num, function(x) {
        future_vals = HY_Stage[row_num > x]
        if(length(future_vals[!is.na(future_vals)]) > 0) {
          return(future_vals[!is.na(future_vals)][1])
        } else {
          return(NA)
        }
      }),
      # Find previous non-NA stage
      previous_stage = sapply(row_num, function(x) {
        past_vals = HY_Stage[row_num < x]
        if(length(past_vals[!is.na(past_vals)]) > 0) {
          return(past_vals[!is.na(past_vals)][length(past_vals[!is.na(past_vals)])])
        } else {
          return(NA)
        }
      }),
      # Fill NA values where previous and next stages match
      HY_Stage = ifelse(is.na(HY_Stage) & !is.na(previous_stage) & !is.na(next_stage) & previous_stage == next_stage,
                        next_stage, HY_Stage)) %>%
    select(-row_num, -next_stage, -previous_stage) %>% ungroup()
  
  
  ## Fixing Cognitive_State
  # Rule 1: Fill NA values before Cognitive_State = 1
  PPMI <- PPMI %>%
    group_by(Patient_ID) %>%
    mutate(
      first_nonNA_row = which(!is.na(Cognitive_State))[1], # Find the first non-NA Cognitive_State value
      is_first_zero = Cognitive_State[first_nonNA_row] == 1, # Check if first non-NA value is 0
      is_before_first = row_number() < first_nonNA_row, # Mark rows before first non-NA value
      Cognitive_State = ifelse(is_first_zero & is_before_first & is.na(Cognitive_State), 1, Cognitive_State)) %>% # Replace NA values before first 1 with 1
    select(-first_nonNA_row, -is_first_zero, -is_before_first) %>% ungroup()
  # Rule 2: Fill values between same Cognitive_State values
  PPMI <- PPMI %>%
    group_by(Patient_ID) %>%
    mutate(
      row_num = row_number(),
      # Find next non-NA stage
      next_stage = sapply(row_num, function(x) {
        future_vals = Cognitive_State[row_num > x]
        if(length(future_vals[!is.na(future_vals)]) > 0) {
          return(future_vals[!is.na(future_vals)][1])
        } else {
          return(NA)
        }
      }),
      # Find previous non-NA stage
      previous_stage = sapply(row_num, function(x) {
        past_vals = Cognitive_State[row_num < x]
        if(length(past_vals[!is.na(past_vals)]) > 0) {
          return(past_vals[!is.na(past_vals)][length(past_vals[!is.na(past_vals)])])
        } else {
          return(NA)
        }
      }),
      # Fill NA values where previous and next stages match
      Cognitive_State = ifelse(is.na(Cognitive_State) & !is.na(previous_stage) & !is.na(next_stage) & previous_stage == next_stage,
                        next_stage, Cognitive_State)) %>%
    select(-row_num, -next_stage, -previous_stage) %>% ungroup()
  
  ## Set Height to the first available Height value among all visits
  PPMI <- PPMI %>%
    group_by(Patient_ID) %>%
    mutate(
      # Get the first non-NA Height value for each patient
      first_height = if(all(is.na(Height))) { NA_real_ } else { first(na.omit(Height)) },
      # Only update Height if first_height exists (not NA)
      Height = ifelse(!is.na(first_height), first_height, Height)
    ) %>%
    select(-first_height) %>%
    ungroup()
  
  
  ### FINAL OVERALL ADDITIONS
  # Add dates as only numbers as well (ex. 2012-08-23 to 20120823)
  PPMI <- PPMI %>% 
    mutate(Visit_Date_n = gsub("-", "", Visit_Date)) %>% relocate(Visit_Date_n, .after = Visit_Date) %>% 
    mutate(Birthdate_n = gsub("-", "", Birthdate)) %>% relocate(Birthdate_n, .after = Birthdate) %>% 
    mutate(Baseline_Date_n = gsub("-", "", Baseline_Date)) %>% relocate(Baseline_Date_n, .after = Baseline_Date) %>% 
    mutate(Symptom_Date_n = gsub("-", "", Symptom_Date)) %>% relocate(Symptom_Date_n, .after = Symptom_Date) %>% 
    mutate(LDOPA_Medication_Start_Date_n = gsub("-", "", LDOPA_Medication_Start_Date)) %>% relocate(LDOPA_Medication_Start_Date_n, .after = LDOPA_Medication_Start_Date) %>% 
    mutate(LDOPA_Medication_End_Date_n = gsub("-", "", LDOPA_Medication_End_Date)) %>% relocate(LDOPA_Medication_End_Date_n, .after = LDOPA_Medication_End_Date)
  
  # Turn any NaN to NA
  PPMI <- PPMI %>% mutate_all(~replace(., is.nan(.), NA))
  
  # Create 6 month visits
  PPMI <- PPMI %>% mutate(Visit_ID_6Months_tmp = ceiling(DayDiff/180))
  PPMI <- PPMI %>% mutate(Visit_ID_6Months = case_when(Visit_ID_6Months_tmp==0 | Visit_ID_6Months_tmp==1 ~ 'BL',
                                                       Visit_ID_6Months_tmp < 10 ~ paste0('V0', Visit_ID_6Months_tmp),
                                                       TRUE ~ paste0('V', Visit_ID_6Months_tmp)))
  PPMI <- PPMI %>% select(-Visit_ID_6Months_tmp) %>% relocate(Visit_ID_6Months, .after = Visit_ID)
  
  # Save
  setwd(folder_path)
  
  PPMI_regular <- PPMI %>% filter(Visit_ID == "BL" | substr(Visit_ID,1,1) == "V")
  PPMI_other <- PPMI %>% filter(!(Visit_ID == "BL" | substr(Visit_ID,1,1) == "V"))
  
  write.csv(PPMI,'PPMI_allVisits_cleaned_processed.csv', row.names=FALSE)
  write.csv(PPMI_regular,'PPMI_regularVisits_cleaned_processed.csv', row.names=FALSE)
  write.csv(PPMI_other,'PPMI_extraVisits_cleaned_processed.csv', row.names=FALSE)
  
  
  
  return("COMPLETED STEP 5: Cleaned Behavioral PPMI data Processed")
}
  

