# by Houman Azizi
# Creates a processed/final Bio file

PPMI_Wide_to_Processed_Bio <- function(folder_path) {
  
  
  setwd(paste0(folder_path,"Data_Wide/"))
  
  
  # Creating a Processed Bio file
  bio <- read.csv("Current_Biospecimen_Analysis_Results_wide.csv", sep=",", header = T)
  colnames(bio)[1] <- "Patient_ID"
  
  
  ################ Merge all SCs into BLs for all files ################
  bio_BLSC <- bio %>% filter(Visit_ID == "BL" | Visit_ID == "SC") %>% 
    group_by(Patient_ID) %>% filter(n() > 1) %>% arrange(Patient_ID,Visit_ID)
  uniqueIDs <- unique(bio_BLSC$Patient_ID)
  
  # merging & saving days difference between SC and BL
  bio_BL <- data.frame(matrix(nrow = length(uniqueIDs), ncol = ncol(bio_BLSC)))
  colnames(bio_BL) <- colnames(bio_BLSC)
  for (i in 1:length(uniqueIDs)) {
    temp <- rbind(bio_BLSC[(i*2)-1,],bio_BLSC[i*2,])
    bio_BL[i,] <- setDT(temp)[, lapply(.SD, function(x) ifelse(is.na(x[1]), x[2], x))]
    # bio_BL[i,] <- setDT(temp)[, lapply(.SD,na.omit)][1] # older version - not working
  }
  # replacing BLs instead of BL/SCs
  bio <- bio %>% filter(!(paste0(Patient_ID,Visit_ID) %in% paste0(bio_BLSC$Patient_ID,bio_BLSC$Visit_ID)))
  bio <- rbind(bio,bio_BL) %>% arrange(Patient_ID,Visit_ID)
  
  # changing SC to BL for participants who only have a single SC visit
  bio$Visit_ID[bio$Visit_ID == "SC"] <- "BL"
  
  
  #change dots to underline
  newNames <- c("Patient_ID", "Visit_ID", "a_Synuclein", "ABeta", "ABeta_1_42", "ABeta_raw", "Apolipoprotein_A1",
                "CSF_Albumin", "CSF_Alpha_synuclein", "CSF_lgG", "DJ_1", "EGF_ELISA", "Full_GBA_gene_sequencing", "GFAP",
                "GRS", "HDL", "IL_6", "LDL", "LRRK2_rep_1_control", "LRRK2_rep_1_H2O2", "LRRK2_rep_1_H2O2_CCM",
                "LRRK2_rep_1_H2O2_UCM", "LRRK2_rep_2_control", "LRRK2_rep_2_H2O2", "LRRK2_rep_2_H2O2_CCM", "LRRK2_rep_2_H2O2_UCM", "LRRK2_rep_3_control", "LRRK2_rep_3_H2O2",
                "LRRK2_rep_3_H2O2_CCM", "LRRK2_rep_3_H2O2_UCM", "NfL_Serum", "NFL_CSF", "Plasma_Albumin", # removed "NEV_a_synuclein_rep1", "NEV_a_synuclein_rep2", on Oct 10, 2024
                "Plasma_IgG", "pTau", "rs34637584_LRRK2_p_G2019S", "rs34995376_LRRK2_p_R1441H", "rs35801418_LRRK2_p_Y1699C", "rs35870237_LRRK2_p_I2020T", "rs76763715_GBA_p_N370S",
                "S100", "SAA_Positive_final", "SCORE", "Serum_IGF_1", "SNCA_rep_1_control", "SNCA_rep_1_H2O2", "SNCA_rep_1_H2O2_CCM", "SNCA_rep_1_H2O2_UCM",
                "SNCA_rep_2_control", "SNCA_rep_2_H2O2", "SNCA_rep_2_H2O2_CCM", "SNCA_rep_2_H2O2_UCM", "SNCA_rep_3_control", "SNCA_rep_3_H2O2", "SNCA_rep_3_H2O2_CCM",
                "SNCA_rep_3_H2O2_UCM", "SNCA_multiplication", "SNCA_007", "SNCA_3UTR_1", "SNCA_3UTR_2", "SNCA_E3E4", "SNCA_E4E6",
                "Total_Cholesterol", "Triglycerides", "tTau", "ApoE_Genotype", "ApoE_Genotype_1", "ApoE_Genotype_2")
  colnames(bio) <- newNames
  
  
  # APOE will repeat for each subject
  bio <- bio %>% group_by(Patient_ID) %>%  filter(!all(is.na(ApoE_Genotype))) %>% 
    mutate(ApoE_Genotype = if_else(!is.na(ApoE_Genotype), ApoE_Genotype, first(na.omit(ApoE_Genotype))),
           ApoE_Genotype_1 = if_else(!is.na(ApoE_Genotype_1), ApoE_Genotype_1, first(na.omit(ApoE_Genotype_1))),
           ApoE_Genotype_2 = if_else(!is.na(ApoE_Genotype_2), ApoE_Genotype_2, first(na.omit(ApoE_Genotype_2))))
  
  
  write.csv(bio,paste0(folder_path,"PPMI_Bio_cleaned_processed.csv"), row.names=FALSE)
  
  
  
  return("COMPLETED STEP 4: Wide Bio PPMI data Cleaned AND Processed")
}
  

