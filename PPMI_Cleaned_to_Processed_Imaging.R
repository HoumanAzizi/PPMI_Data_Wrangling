# by Houman Azizi
# For Imaging data -> matches scan dates with regular visit dates + change column names

PPMI_Cleaned_to_Processed_Imaging <- function(folder_path) {
  
  setwd(folder_path)
  
  Imaging <- read.csv("PPMI_Imaging_cleaned.csv", sep=",", header = T)
  PPMI <- read.csv("PPMI_regularVisits_cleaned_processed.csv", sep=",", header = T)
  
  
  
  ####### Removing Special Case IDs ####### 
  # This row had only NA values -> removed
  Imaging <- Imaging %>% filter(!(Patient_ID == "40778" & Visit_ID == "U01"))
  # This Patient_ID does not have any behavioral data -> removed
  Imaging <- Imaging %>% filter(!(Patient_ID == 121531))
  # Remove U03 and PW visits -> very few exist (if not deleted the code will also run)
  Imaging <- Imaging %>% filter(!(Visit_ID == "U03" | Visit_ID == "PW"))
  
  
  
  
  
  
  
  
  
  ######## Matching U01, U02, U03, PW, and ST visits to regular visits ######## 
  ImagingIrregular <- Imaging %>% filter(!(Visit_ID == "BL" | substr(Visit_ID,1,1) == "V")) %>% 
    select("Patient_ID", "Visit_ID", "Visit_Date", "MRI_Scan_Date_asDate", "DAT_Scan_Date_asDate", "MRI_Delay_Days", "DAT_Delay_Days") # removed PET_Scan_Date_asDate and PET_Delay_Days
  ImagingIrregular$Visit_ID_Match <- NA
  ImagingIrregular$Visit_Date_Match <- NA
  ImagingIrregular <- ImagingIrregular %>% relocate(Visit_ID_Match, .after = Visit_ID)
  
  
  # change all NA values in the Visit_Date column to an actual date
  for (i in 1:nrow(ImagingIrregular)) {
    if (is.na(ImagingIrregular$Visit_Date[i])) {
      index <- which(!is.na(ImagingIrregular[i,]))[3]
      ImagingIrregular$Visit_Date[i] <- ImagingIrregular[i, index]
    }
  }
  
  
  # find the closet regular visit_id from the main regular_PPMI
  for (i in 1:nrow(ImagingIrregular)) {
    closest_date <- PPMI %>% filter(Patient_ID == ImagingIrregular$Patient_ID[i]) %>% 
      mutate(tempDayDiff = abs(as.integer(difftime(Visit_Date, ImagingIrregular$Visit_Date[i])))) %>% 
      slice(which.min(tempDayDiff))
    ImagingIrregular$Visit_ID_Match[i] <- closest_date$Visit_ID
    ImagingIrregular$Visit_Date_Match[i] <- closest_date$Visit_Date
  }
  
  
  # change the Visit_IDs of irregular visit on the full Imaging dataframe
  for (i in 1:nrow(ImagingIrregular)) {
    index <- which(Imaging$Patient_ID == ImagingIrregular$Patient_ID[i] & Imaging$Visit_ID == ImagingIrregular$Visit_ID[i])
    Imaging$Visit_ID[index] <- ImagingIrregular$Visit_ID_Match[i]
    Imaging$Visit_Date[index] <- ImagingIrregular$Visit_Date_Match[i]
  }
  
  
  
  
  
  
  
  
  
  
  
  ###### Fixing other columns ######
  # adding date difference of scans with Visit_Date
  Imaging <- Imaging %>% rowwise() %>%  mutate(DayDiff_MRI = as.integer(difftime(MRI_Scan_Date_asDate, Visit_Date, units = "days")),
                                               DayDiff_DAT = as.integer(difftime(DAT_Scan_Date_asDate, Visit_Date, units = "days"))) %>% # removed DayDiff_PET = as.integer(difftime(PET_Scan_Date_asDate, Visit_Date, units = "days"))
    relocate(DayDiff_MRI, .after = MRI_Scan_Date_asDate) %>%
    relocate(DayDiff_DAT, .after = DAT_Scan_Date_asDate) %>% # removed relocate(DayDiff_PET, .after = PET_Scan_Date_asDate)
    select(-c("MRI_Delay_Days", "DAT_Delay_Days")) # removed PET_Delay_Days
  
  # merging rows which have same Patient_ID and Visit_ID
  idCheck <- Imaging %>% select(Patient_ID, Visit_ID)
  dupIndex <- which(duplicated(idCheck))
  dupIDs <- paste0(Imaging$Patient_ID[dupIndex],Imaging$Visit_ID[dupIndex])
  dupImg <- Imaging %>% filter(paste0(Patient_ID, Visit_ID) %in% dupIDs) %>% arrange(Patient_ID,Visit_ID)
  
  fixedImg <- data.frame(matrix(nrow = length(dupIndex), ncol = ncol(dupImg)))
  colnames(fixedImg) <- colnames(dupImg)
  for (i in 1:length(dupIndex)) {
    temp <- rbind(dupImg[(i*2)-1,],dupImg[i*2,])
    fixedImg[i,] <- setDT(temp)[, lapply(.SD,na.omit)][1]
  }
  # replacing fixed rows into the main Imaging dataframe
  Imaging <- Imaging %>% filter(!(paste0(Patient_ID,Visit_ID) %in% dupIDs))
  Imaging <- as.data.frame(rbind(Imaging,fixedImg)) %>% arrange(Patient_ID,Visit_ID)
  
  
  
  
  
  ######## Option 1 (can comment out): removing imaging visits without behvioral visit ########
  imgIDs <- Imaging %>% select(Patient_ID, Visit_ID)
  imgIDs <- paste0(imgIDs$Patient_ID,imgIDs$Visit_ID)
  PPMIIDs <- PPMI %>% select(Patient_ID, Visit_ID)
  PPMIIDs <- paste0(PPMIIDs$Patient_ID,PPMIIDs$Visit_ID)
  unavail <- which((imgIDs %in% PPMIIDs)==FALSE) #the row number in Imaging that are not in the behavioral PPMI
  Imaging <- Imaging[-unavail,]
  
  
  
  # ######## Option 2 (can comment out): Matching imaging visits without an exact behavioural visit -> to closest behavioural visit ######## 
  # imgIDs <- Imaging %>% select(Patient_ID, Visit_ID)
  # imgIDs <- paste0(imgIDs$Patient_ID,imgIDs$Visit_ID)
  # PPMIIDs <- PPMI %>% select(Patient_ID, Visit_ID)
  # PPMIIDs <- paste0(PPMIIDs$Patient_ID,PPMIIDs$Visit_ID)
  # unavail <- which((imgIDs %in% PPMIIDs)==FALSE) #the row number in Imaging that are not in the behavioral PPMI
  # 
  # ImgUnavail <- Imaging[unavail,] %>% select("Patient_ID", "Visit_ID", "Visit_Date", "MRI_Scan_Date_asDate", "DAT_Scan_Date_asDate", "PET_Scan_Date_asDate")
  # ImgUnavail$Visit_ID_Match <- NA
  # ImgUnavail$Visit_Date_Match <- NA
  # row.names(ImgUnavail) <- 1:nrow(ImgUnavail)
  # 
  # # change all NA values in the Visit_Date column to an actual date
  # for (i in 1:nrow(ImgUnavail)) {
  #   if (is.na(ImgUnavail$Visit_Date[i])) {
  #     index <- which(!is.na(ImgUnavail[i,]))[3]
  #     ImgUnavail$Visit_Date[i] <- ImgUnavail[i, index]
  #   }
  # }
  # 
  # # find the closet behavioral visit_id from the main regular_PPMI
  # for (i in 1:nrow(ImgUnavail)) {
  #   closest_date <- PPMI %>% filter(Patient_ID == ImgUnavail$Patient_ID[i]) %>% 
  #     mutate(tempDayDiff = abs(as.integer(difftime(Visit_Date, ImgUnavail$Visit_Date[i])))) %>% 
  #     slice(which.min(tempDayDiff))
  #   ImgUnavail$Visit_ID_Match[i] <- closest_date$Visit_ID
  #   ImgUnavail$Visit_Date_Match[i] <- closest_date$Visit_Date
  # }
  # 
  # # change the Visit_IDs of irregular visit on the full Imaging dataframe
  # for (i in 1:nrow(ImgUnavail)) {
  #   index <- which(Imaging$Patient_ID == ImgUnavail$Patient_ID[i] & Imaging$Visit_ID == ImgUnavail$Visit_ID[i])
  #   Imaging$Visit_ID[index] <- ImgUnavail$Visit_ID_Match[i]
  #   Imaging$Visit_Date[index] <- ImgUnavail$Visit_Date_Match[i]
  # }
  # 
  # # removing duplicates created by this section
  # idCheck <- Imaging %>% select(Patient_ID, Visit_ID)
  # dupIndex <- which(duplicated(idCheck))
  # dupIDs <- paste0(Imaging$Patient_ID[dupIndex],Imaging$Visit_ID[dupIndex])
  # dupImg <- Imaging %>% filter(paste0(Patient_ID, Visit_ID) %in% dupIDs) %>% arrange(Patient_ID,Visit_ID)
  # # removing people with 2 DAT protocols on same date -> removing their 004 protocol
  # Imaging <- Imaging %>% filter(!(paste0(Patient_ID, Visit_ID, DAT_Protocol) %in% paste0(dupIDs,"004")))
  # # remove special case with 2 MRI at same date -> removing the one with wrong date
  # Imaging <- Imaging[-which(paste0(Imaging$Patient_ID,Imaging$Visit_ID,Imaging$MRI_Scan_Date_asDate) == "110219BL2022-04-01"),]
  # # merging the other ones as regular, keeping all the values
  # idCheck <- Imaging %>% select(Patient_ID, Visit_ID)
  # dupIndex <- which(duplicated(idCheck))
  # dupIDs <- paste0(Imaging$Patient_ID[dupIndex],Imaging$Visit_ID[dupIndex])
  # dupImg <- Imaging %>% filter(paste0(Patient_ID, Visit_ID) %in% dupIDs) %>% arrange(Patient_ID,Visit_ID)
  # fixedImg <- data.frame(matrix(nrow = length(dupIndex), ncol = ncol(dupImg)))
  # colnames(fixedImg) <- colnames(dupImg)
  # for (i in 1:length(dupIndex)) {
  #   temp <- rbind(dupImg[(i*2)-1,],dupImg[i*2,])
  #   fixedImg[i,] <- setDT(temp)[, lapply(.SD,na.omit)][1]
  # }
  # # replacing fixed rows into the main Imaging dataframe
  # Imaging <- Imaging %>% filter(!(paste0(Patient_ID,Visit_ID) %in% dupIDs))
  # Imaging <- as.data.frame(rbind(Imaging,fixedImg)) %>% arrange(Patient_ID,Visit_ID)
  # 
  # # recalculating daydiff
  # Imaging <- Imaging %>% rowwise() %>%  mutate(DayDiff_MRI = as.integer(difftime(MRI_Scan_Date_asDate, Visit_Date, units = "days")),
  #                                              DayDiff_DAT = as.integer(difftime(DAT_Scan_Date_asDate, Visit_Date, units = "days")),
  #                                              DayDiff_PET = as.integer(difftime(PET_Scan_Date_asDate, Visit_Date, units = "days"))) %>%
  #   relocate(DayDiff_MRI, .after = MRI_Scan_Date_asDate) %>%
  #   relocate(DayDiff_DAT, .after = DAT_Scan_Date_asDate) %>%
  #   relocate(DayDiff_PET, .after = PET_Scan_Date_asDate)
  # ## Comment out up to here ##
  
  
  
  ####### Finalizing #######
  
  
  # renaming columns
  Imaging <- Imaging %>% select(-c("MRI_Scan_Date", "DAT_Scan_Date")) # removed "PET_Scan_Date"
  colnames(Imaging)[which(colnames(Imaging)=="MRI_Scan_Date_asDate")] <- "MRI_Date"
  colnames(Imaging)[which(colnames(Imaging)=="DAT_Scan_Date_asDate")] <- "DAT_Date"
  # colnames(Imaging)[which(colnames(Imaging)=="PET_Scan_Date_asDate")] <- "PET_Date"
  # Saving the full version
  write.csv(Imaging,paste0(folder_path,"PPMI_Imaging_All_cleaned_processed.csv"), row.names=FALSE)
  
  
  
  
  
  
  
  
  
  
  
  
  #Imaging_Main is removing PET and MRI -> keeping DAT
  ######## Taking the values we need ########
  Imaging <- Imaging %>% select(Patient_ID, Visit_ID, Visit_Date, DAT_Protocol, DATSCAN_LIGAND, DATSCAN_CAUDATE_R, 
                                DATSCAN_CAUDATE_L, DAT_Caudate_Average, DATSCAN_PUTAMEN_R, DATSCAN_PUTAMEN_L, DAT_Putamen_Average, 
                                DATSCAN_PUTAMEN_R_ANT, DATSCAN_PUTAMEN_L_ANT, DAT_Putamen_Average_ANT, DATSCAN_QUALITY_RATING, DayDiff_DAT)
  
  # remove rows with only NA values
  removeIndex <- c()
  for (i in 1:nrow(Imaging)) {
    if (all(is.na(Imaging[i,4:ncol(Imaging)])) == TRUE) {
      removeIndex <- c(removeIndex,i)
    }
  }
  Imaging <- Imaging[-removeIndex,]
  
  # Saving main values
  write.csv(Imaging,paste0(folder_path,"PPMI_Imaging_Main_cleaned_processed.csv"), row.names=FALSE)
  
  
  
  return("COMPLETED STEP 6: Cleaned Imaging PPMI data Processed")
}











