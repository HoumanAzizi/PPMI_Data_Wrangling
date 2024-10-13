PPMI_EXTRA_Calculating_Missing_Value_Percentages <- function(folder_path) {
  
  setwd(folder_path)
  
  PPMI <- read.csv('PPMI_Merged_AllMeasures_AllVisits_FINAL.csv', sep=",", header = T)
  
  main_visits <- c("BL", "V01", "V02", "V03", "V04", "V05", "V06", "V07", "V08" ,"V09", "V10", 
                   "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20")
  
  PPMI <- PPMI %>% filter(Visit_ID %in% main_visits) %>% 
    filter(Cohort == 'PD')
  
  # Optional: choose some specific variables
  # PPMI <- PPMI %>% select(Patient_ID, Visit_ID,
  #                         UPDRS_Part_II, Symbol_Digit, MOCA, Semantic_Fluency_Scaled, Schwab_England)
  
  
  ## Calculating missing data
  missing_calc <- data.frame(matrix(NA, nrow = length(main_visits)+1, ncol = length(colnames(PPMI))))
  colnames(missing_calc) <- colnames(PPMI)
  missing_calc$Visit_ID <- c(main_visits, 'All_visits')
  missing_calc <- missing_calc %>% select(-Patient_ID)
  
  for (i in 2:ncol(missing_calc)) {
    tmp <- PPMI %>% select(Visit_ID, colnames(missing_calc)[i])
    colnames(tmp) <- c('Visit_ID', 'var')
    
    # add all visits
    missing_calc[[colnames(missing_calc)[i]]][missing_calc$Visit_ID=='All_visits'] <- (sum(is.na(tmp$var))/nrow(tmp))*100
    
    # count per visit
    tmp <- tmp %>% group_by(Visit_ID) %>% mutate(percentage_missing = (sum(is.na(var))/n())*100) %>% ungroup()
    tmp <- tmp[duplicated(tmp$Visit_ID)==FALSE,] %>% select(-var)
    
    # Add to the final file
    for (j in 1:nrow(tmp)) {
      missing_calc[[colnames(missing_calc)[i]]][missing_calc$Visit_ID==tmp$Visit_ID[j]] <- tmp$percentage_missing[j]
    }
    
    rm(tmp)
  }
  
  # limit to 2 decimals
  missing_calc[] <- lapply(missing_calc, function(x) if(is.numeric(x)) round(x, 2) else x)
  
  # Optional: Remove ones with BL or Overall more than 50%
  toRemove <- (missing_calc[1,] > 50) & (missing_calc[22,] > 50)
  columnsToRemove <- which(toRemove)[-1]
  missing_calc <- missing_calc[, -columnsToRemove]
  
  missing_calc <- missing_calc %>% select(-starts_with('Blood_'), -starts_with('DAT'), -starts_with('DayDiff'), -any_of(c('Cognitive_Change')))
  
  # Show as Image
  library(ggplot2)
  missing_calc_long <- pivot_longer(missing_calc, cols = -Visit_ID, names_to = "Variable", values_to = "Value")
  # Optional
  missing_calc_long <- missing_calc_long %>% filter(!(Visit_ID %in% c('V01', 'V03', 'V05', 'V07', 'V09', 'V11', 'V20')))
  
  p1 <- ggplot(missing_calc_long, aes(x = Visit_ID, y = Variable, fill = Value)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "red") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave("Missing_Data_all.pdf", p1, width = 10, height = 40)
  
  
  p2 <- ggplot(missing_calc_long, aes(x = Visit_ID, y = Variable, fill = Value > 25)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("grey", "red"), labels = c("<= 25", "> 25")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave("Missing_Data_binary_25.pdf", p2, width = 10, height = 40)
  
  
  # Save
  write.csv(missing_calc, 'PPMI_Missing_Data_Percentage_perVisit.csv', row.names=FALSE)
  
  return("Calculated Missing Data Percentage perVisit - Final filename: PPMI_Missing_Data_Percentage_perVisit.csv")
  
}







