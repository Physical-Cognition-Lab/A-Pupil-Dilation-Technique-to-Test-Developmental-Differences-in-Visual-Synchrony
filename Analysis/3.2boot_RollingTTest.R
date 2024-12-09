
library(tidyverse)
library(parallel)
library(zoo)



# Functions ---------------------------------------------------------------

# Define the RollingT function
RollingT = function(df) {
  df = as.data.frame(df)
  Child_window = as.numeric(df$Children)
  Adu_window = as.numeric(df$Adult)
  # Check for NA values in each window
  ifelse(sum(is.na(Child_window)) / length(Child_window) > 0.75 || sum(is.na(Adu_window)) / length(Adu_window) > 0.75,
         NA,
         t.test(Child_window, Adu_window, paired = FALSE, alternative = "two.sided")$p.value) 
}



# ReadData ----------------------------------------------------------------

Hz = 20
window_size = 4*Hz

# Set working directory
# Set the working directory to where your data is located
setwd("C:\\Users\\tomma\\OneDrive - Birkbeck, University of London\\PupilDilationSync_2023\\A-Pupil-Dilation-Technique-to-Test-Developmental-Differences-in-Visual-Synchrony")

# Read data
Synch_Pupil = readRDS('.\\Data\\ProcessedData\\Bootstrap\\PupilSynch_Boot.rds')

# split dataframe in list based iterators
Synch_Pupil_list = split(Synch_Pupil, Synch_Pupil$Iteration)



# Rolling TTest Pupil ------------------------------------------------------------

ApplyRollingT = function(DF) {
  
  Iter = DF$Iteration[1]
    
  # Prepare data
  Synch_Pupilupil_Wide = DF %>%
    pivot_wider(names_from = Group, values_from = PupilSynch,
                id_cols = c("Seconds", "Stimulus", 'Video')) %>%
    arrange(Stimulus, Video,  Seconds) %>%
    ungroup()
  
  # Split data by stimulus
  data_by_stimulus_P = Synch_Pupilupil_Wide %>%
    split(.$Stimulus)
  
  # Apply the RollingT function to each group separately
  result_list = lapply(data_by_stimulus_P, function(group_df) {
    rollapply(group_df, width = window_size, FUN = function(df) RollingT(df), by.column = FALSE, fill = NA, partial = TRUE)
  })
  
  # Combine the results back into a single data frame
  TTEST_Pupil = Synch_Pupilupil_Wide %>%
    bind_cols( ., stack(result_list)) %>%
    rename(Pval = values) %>%
    select(-ind) %>% 
    group_by(Video,Stimulus)%>%
    mutate(Pval.fdr =   p.adjust(Pval, method = "fdr"))%>%
    ungroup()%>%
    mutate( Significance = case_when(
      Pval >= 0.05 ~ FALSE,
      Pval < 0.05 ~ TRUE,
      is.na(Pval) ~ FALSE),
      Significance.fdr = case_when(
        Pval.fdr >= 0.05 ~ FALSE,
        Pval.fdr < 0.05 ~ TRUE,
        is.na(Pval.fdr) ~ FALSE))
  TTEST_Pupil$Iteration = Iter

  return(TTEST_Pupil)
}



# Cluster preparation ----------------------------------------------------------

# Use parLapply to process each subject in parallel
cl <- makeCluster(detectCores()/2) # leave 2 core free

# Export necessary objects to each worker
clusterExport(cl, c("ApplyRollingT", "window_size", "RollingT"))

# Load necessary libraries in each worker
clusterEvalQ(cl, {
  library(tidyverse)
  library(zoo)
})



# Run Cluster -------------------------------------------------------------

DB = parLapply(cl, Synch_Pupil_list, ApplyRollingT)
stopCluster(cl)

DB = bind_rows(DB)
saveRDS(DB, '.\\Data\\ProcessedData\\Bootstrap\\TTEstPupil_Boot.rds')


