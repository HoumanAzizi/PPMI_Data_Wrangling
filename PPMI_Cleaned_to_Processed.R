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
  Symptom_duration <- difftime(PPMI$Enroll_Date_asDate,PPMI$SXDT, units = "days")
  Symptom_duration <- Symptom_duration/365 #previously /(3600*24*365)
  Symptom_duration <- as.numeric(Symptom_duration)
  
  BP_Sys_drop <- PPMI$SYSSUP - PPMI$SYSSTND
  
  Tremor_score_NUPDRS3 <- rowMeans(PPMI[,c("Tremor2_Calculated","Tremor3_Calculated_NUPDRS3")],na.rm = TRUE)
  Tremor_score_NUPDRS3A <- rowMeans(PPMI[,c("Tremor2_Calculated","Tremor3_Calculated_NUPDRS3A")],na.rm = TRUE)
  Tremor_score_NUPDR3OF <- rowMeans(PPMI[,c("Tremor2_Calculated","Tremor3_Calculated_NUPDR3OF")],na.rm = TRUE)
  Tremor_score_NUPDR3ON <- rowMeans(PPMI[,c("Tremor2_Calculated","Tremor3_Calculated_NUPDR3ON")],na.rm = TRUE)
  
  PIGD_score_NUPDRS3 <- rowMeans(PPMI[,c("PIGD2_Calculated","PIGD3_Calculated_NUPDRS3")])
  PIGD_score_NUPDRS3A <- rowMeans(PPMI[,c("PIGD2_Calculated","PIGD3_Calculated_NUPDRS3A")])
  PIGD_score_NUPDR3OF <- rowMeans(PPMI[,c("PIGD2_Calculated","PIGD3_Calculated_NUPDR3OF")])
  PIGD_score_NUPDR3ON <- rowMeans(PPMI[,c("PIGD2_Calculated","PIGD3_Calculated_NUPDR3ON")])
  PPMI <- PPMI %>% select(-c("Tremor2_Calculated","Tremor3_Calculated_NUPDRS3","Tremor3_Calculated_NUPDRS3A","Tremor3_Calculated_NUPDR3OF","Tremor3_Calculated_NUPDR3ON",
                             "PIGD2_Calculated","PIGD3_Calculated_NUPDRS3","PIGD3_Calculated_NUPDRS3A","PIGD3_Calculated_NUPDR3OF","PIGD3_Calculated_NUPDR3ON"))
  # Choose which Tremor and PIGD score to use
  Tremor_score <- Tremor_score_NUPDRS3
  PIGD_score <- PIGD_score_NUPDRS3
  
  
  
  #CALCULATE NUPDRS TOTAL HERE
  UPDRS_PartI <- PPMI$NP1TOT_Calculated
  UPDRS_PartII <- PPMI$NP2TOT_Calculated
  UPDRS_PartIII <- PPMI$NP3TOT_Calculated_NUPDRS3
  UPDRS_Total_Score <- rowSums(cbind(UPDRS_PartI, UPDRS_PartII, UPDRS_PartIII))
  UPDRS_PartIII_Left <- PPMI$NP3TOT_NUPDRS3_L
  UPDRS_PartIII_Right <- PPMI$NP3TOT_NUPDRS3_R
  
  
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
  PPMI$Symptom_duration <- Symptom_duration
  PPMI$Tremor_score <- Tremor_score
  PPMI$PIGD_score <- PIGD_score
  PPMI$UPDRS_PartI <- UPDRS_PartI
  PPMI$UPDRS_PartII <- UPDRS_PartII
  PPMI$UPDRS_PartIII <- UPDRS_PartIII
  PPMI$UPDRS_Total_Score <- UPDRS_Total_Score
  PPMI$UPDRS_PartIII_Left <- UPDRS_PartIII_Left
  PPMI$UPDRS_PartIII_Right <- UPDRS_PartIII_Right
  PPMI$RBD_Score <- RBD_Score
  PPMI$UPSIT_Score <- UPSIT_Score
  PPMI$SCOPA_AUT_Score <- SCOPA_AUT_Score
  PPMI$STAI_State_Score <- STAI_State_Score
  PPMI$STAI_Trait_Score <- STAI_Trait_Score
  PPMI$STAI_Total_Score <- STAI_Total_Score
  PPMI$GDS_Score <- GDS_Score
  PPMI$MOCA_adjusted_Score <- MOCA_adjusted_Score
  PPMI$QUIP_Total <- QUIP_Total
  PPMI$ESS_total <- ESS_total
  
  
  # NOT available columns: CNO, DXOTHCM, DOMSIDE
  # These ones are available separately in the bio file: "Abeta.42","CSF.Alpha.synuclein","p.Tau181P","Total.tau","rs34637584_LRRK2_p.G2019S","rs76763715_GBA_p.N370S"
  PPMI <- PPMI %>% select("Patient_Number", "Visit_ID", "Visit_Date_asDate", "Age_at_Visit", "BL_Age", "BIRTHDT_asDate",
                          "BL_Date", "Days_from_BL", "Cohort", "SEX", "EDUCYRS", "HANDED",
                          "UPDRS_PartI", "UPDRS_PartII", "UPDRS_PartIII", "UPDRS_PartIII_Left", "UPDRS_PartIII_Right", "UPDRS_Total_Score",
                          "MSEADLG", "Tremor_score", "PIGD_score", "ESS_total", "GDS_Score", "MCATOT", "MOCA_adjusted_Score", "QUIP_Total", "RBD_Score", 
                          "SCOPA_AUT_Score", "STAI_State_Score", "STAI_Trait_Score", "STAI_Total_Score", "UPSIT_Score",
                          "JLO_TOTRAW", "JLO_TOTCALC", "DVS_JLO_MSSA", "DVS_JLO_MSSAE", 
                          "DVT_TOTAL_RECALL", "DVT_DELAYED_RECALL", "DVT_RETENTION", "DVT_RECOG_DISC_INDEX",
                          "LNS_TOTRAW", "DVS_LNS", "DVS_SFTANIM", "DVT_SFTANIM",
                          "DVSD_SDM", "DVT_SDM",
                          "COGDECLN", "FNCDTCOG", "COGSTATE", "COGDXCL", "RVWNPSY",
                          "Albumin.QT", "Alkaline.Phosphatase.QT", "ALT..SGPT.", "AST..SGOT.", "Serum.Glucose", "Serum.Uric.Acid",
                          "Total.Protein", "Urea.Nitrogen", "TOPRRSLT", "TGLCRSLT", 
                          "WGTKG", "HTCM", "TEMPC", "DIASUP", "HRSUP", "DIASTND", "HRSTND", "BP_Sys_drop", 
                          "SXDT", "Symptom_duration", "DXTREMOR", "DXRIGID", "DXBRADY", "DXPOSINS", "DXOTHSX",
                          "HISPLAT", "RAINDALS", "RAASIAN", "RABLACK", "RAHAWOPI", "RAWHITE", "BIOMOMPD", "BIODADPD",
                          "FULSIBPD", "HAFSIBPD", "MAGPARPD", "PAGPARPD", "MATAUPD", "PATAUPD",
                          "Cohort_n", "SEX_n",
                          "SC_Delay_Days", "Blood_Delay_Days", "Lumbar_Delay_Days", "Vital_Delay_Days", "UPDRS1_Delay_Days", "UPDRS2_Delay_Days", "UPDRS3_Delay_Days",
                          "MSE_Delay_Days", "ESS_Delay_Days", "GDS_Delay_Days", "MOCA_Delay_Days", "QUIP_Delay_Days", "REM_Delay_Days", "SCOPA_Delay_Days", "STAI_Delay_Days",
                          "UPSIT_Delay_Days", "JLO_Delay_Days", "DVT_Delay_Days", "LNS_Delay_Days", "SFT_Delay_Days", "SDM_Delay_Days", "COG_Delay_Days")
  
  
  # DayDiff_Visit_Baseline -> Day_Diff (Shows day difference of this visit from Baseline)
  new_names <- c("Patient_ID", "Visit_ID", "Visit_Date", "Age", "Age_Baseline", "Birthdate",
                 "Baseline_Date", "DayDiff", "Cohort", "Sex" , "Education_Years", "Handedness",
                 "UPDRS_Part_I", "UPDRS_Part_II", "UPDRS_Part_III", "UPDRS_Part_III_Left", "UPDRS_Part_III_Right", "UPDRS_Total_Score",
                 "Schwab_England", "Tremor", "PIGD", "Epworth", "GDS", "MOCA", "MOCA_adjusted", "QUIP", "RBD_Score", 
                 "SCOPA_AUT", "STAI_State", "STAI_Trait", "STAI_Total", "UPSIT_Score", 
                 "Benton_Line_Sum", "Benton_Line_Calculated_Sum", "Benton_MOANS_Age", "Benton_MOANS_Age_educ",
                 "HVLT_Total_Recall", "HVLT_Delayed_Recall", "HVLT_Retention", "HVLT_Recog_Discrim", 
                 "LNS", "LNS_Scaled", "Semantic_Fluency_Scaled", "Semantic_Fluency", 
                 "Symbol_Digit_SD", "Symbol_Digit", 
                 "Cognitive_Decline_Experienced", "Cognitive_Functional_Decline", "Cognitive_State", "Cognitive_Diagnosis_Confidence.level", "Rev_Neuropsych_Test",
                 "Blood_Albumin", "Blood_ALK_P", "Blood_ALT", "Blood_AST", "Blood_Glucose", "Blood_Uric_Acid",
                 "Blood_Total_Protein", "Blood_Urea_Nitrogen", "Blood_Total_Protein_CSF", "Blood_Total_Glocuse_CSF",
                 "Weight", "Height", "Temperature", "BP_Dias_Sup", "HR_Sup", "BP_Dias_Stand", "HR_Stand", "BP_Sys_drop",
                 "Symptom_Date", "Symptom_Duration", "Diag_Rest_Tremor", "Diag_Rigidity", "Diag_Bradykinesia", "Diag_Postural_Instability", "Diag_Other_Symptoms",
                 "Race_Hispanic", "Race_Indian", "Race_Asian", "Race_Black", "Race_Hawaiian", "Race_White", "PDHistory_Mother", "PDHistory_Father",
                 "PDHistory_Full_Siblings", "PDHistory_Half_Sibling", "PDHistory_Maternal_GP", "PDHistory_Paternal_GP", "PDHistory_Maternal_AU", "PDHistory_Paternal_AU",
                 "Cohort_Number", "Sex_Number",
                 "DayDiff_Screening_Baseline", "DayDiff_BloodCollection", "DayDiff_LumbarPunc", "DayDiff_Vitals", "DayDiff_UPDRS_I", "DayDiff_UPDRS_II", "DayDiff_UPDRS_III",
                 "DayDiff_SchwabEng", "DayDiff_Epworth", "DayDiff_GDS", "DayDiff_MOCA", "DayDiff_QUIP", "DayDiff_REM", "DayDiff_SCOPA", "DayDiff_STAI",
                 "DayDiff_UPSIT", "DayDiff_BentonLine", "DayDiff_HVLT", "DayDiff_LNS", "DayDiff_SemanticFluency", "DayDiff_SymbolDigit", "DayDiff_Cognitive")
  
  colnames(PPMI) <- new_names
  
  
  
  # Add CNOs
  Center <- read.csv("PPMI_CNO.csv")
  colnames(Center) <- c("Patient_ID", "Center_ID")
  PPMI <- PPMI %>% left_join(Center, by = c("Patient_ID")) %>% relocate(Center_ID, .after = Baseline_Date)
  # End of "Add CNOs"
  
  
  
  
  setwd(folder_path)
  
  PPMI_regular <- PPMI %>% filter(Visit_ID == "BL" | substr(Visit_ID,1,1) == "V")
  PPMI_other <- PPMI %>% filter(!(Visit_ID == "BL" | substr(Visit_ID,1,1) == "V"))
  
  write.csv(PPMI,'PPMI_allVisits_cleaned_processed.csv', row.names=FALSE)
  write.csv(PPMI_regular,'PPMI_regularVisits_cleaned_processed.csv', row.names=FALSE)
  write.csv(PPMI_other,'PPMI_extraVisits_cleaned_processed.csv', row.names=FALSE)
  
  
  
  return("COMPLETED STEP 5: Cleaned Behavioral PPMI data Processed")
}
  

