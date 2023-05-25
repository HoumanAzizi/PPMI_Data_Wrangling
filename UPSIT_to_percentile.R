UPSIT_to_percentile <- function(upsit_score,age,sex,address){
  # started 10/03/2016
  # updated
  # by: Yashar Zeighami
  # convert UPSIT raw to percentile based on age and sex
  setwd(address)
  
  if (sex==1){
  sex <- 'male'
  } else {
  sex <- 'female'
  }
  if (sex == 'male'){
    percentile_tabel <- read.csv('UPSIT_Normative_Values_male.csv')
  } else {
    percentile_tabel <- read.csv('UPSIT_Normative_Values_female.csv')
  }
  percentile <- percentile_tabel[which(percentile_tabel[,1]==upsit_score),floor((age-5)/5)+2]
  if(length(percentile)==0) return(NA) else return(percentile)
}

