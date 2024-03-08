PPMI_EXTRA_Calculating_Missing_Value_Percentages <- function(folder_path) {
  
  setwd(folder_path)
  
  PPMI <- read.csv('PPMI_Merged_AllMeasures_AllVisits_FINAL.csv', sep=",", header = T)
  
  main_visits <- c("BL", "V01", "V02", "V03", "V04", "V05", "V06", "V07", "V08" ,"V09", "V10", 
                   "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20")
  
  PPMI <- PPMI %>% filter(Visit_ID %in% main_visits) %>% 
    filter(Cohort == 'PD')
  
  
  
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
  
  
  # Save
  write.csv(missing_calc, 'PPMI_Missing_Data_Percentage_perVisit.csv', row.names=FALSE)
  
  return("Calculated Missing Data Percentage perVisit - Final filename: PPMI_Missing_Data_Percentage_perVisit.csv")
  
}







