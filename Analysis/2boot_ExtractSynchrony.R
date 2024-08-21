
library(tidyverse)
library(parallel)
library(zoo)


# Read Data, functions and Settings ------------------------------------------------

Hz = 20
window_sizeP = Hz * 2  # Window size for calculating rolling correlation

# Set working directory
setwd("C:\\Users\\tomma\\Desktop\\Share")

# Load running average person correlation functions
source(".\\Scripts\\0_SynchronyFunctions.R")

# Read the input data
df = read.csv('.\\Data\\ProcessedData\\FinalData.csv', sep = ',')



# Cluster Preparation ----------------------------------------------------------

iterations = 1:1000

# Initialize parallel processing
cl = makeCluster(detectCores() / 2) # Use half of available cores

# Export necessary objects and functions to each worker
clusterExport(cl, c("df", "calculate_running_correlation", "average_fisher_z_transform", "window_sizeP"))

# Load required libraries in each worker
clusterEvalQ(cl, {
  library(tidyverse)
  library(zoo)
})



# Run the Cluster ---------------------------------------------------------

# Function to run synchronization analysis for each iteration
MultipleSynhc = function(x) {
  
  # Randomize the group labels for each subject
  db = df %>%
    group_by(Subject) %>%
    mutate(Group = sample(c('Adults', 'Children'), size = n(), replace = TRUE)) %>%
    ungroup()
  
  
  #### Adults Group Processing
  Adults = db %>%
    filter(Group == 'Adults') %>%
    arrange(Stimulus) %>%
    pivot_wider(names_from = Subject, values_from = Pupil,
                id_cols = c("Seconds", "Stimulus", "Video")) 
  
  ByStimulus = Adults %>% split(.$Stimulus)
  ByStimulus = lapply(ByStimulus, function(df) df %>% select(-1:-3))
  
  Res = lapply(ByStimulus, calculate_running_correlation, window_sizeP = window_sizeP)
  
  Adults = Adults %>%
    mutate(PupilSynch = unlist(Res, use.names = FALSE),
           Group = 'Adults') %>%
    select(Seconds, Group, Video, Stimulus, PupilSynch)
  
  
  #### Children Group Processing
  Children = db %>%
    filter(Group == 'Children') %>%
    arrange(Stimulus) %>%
    pivot_wider(names_from = Subject, values_from = Pupil,
                id_cols = c("Seconds", "Stimulus", "Video")) 
  
  ByStimulus = Children %>% split(.$Stimulus)
  ByStimulus = lapply(ByStimulus, function(df) df %>% select(-1:-3))
  
  Res = lapply(ByStimulus, calculate_running_correlation, window_sizeP = window_sizeP)
  
  Children = Children %>%
    mutate(PupilSynch = unlist(Res, use.names = FALSE),
           Group = 'Children') %>%
    select(Seconds, Group, Video, Stimulus, PupilSynch)
  
  #### Combine Adults and Children Results
  Synch_Pupil = bind_rows(Children, Adults) %>%
    mutate(Iteration = x)
  
  return(Synch_Pupil)
}


# Run the MultipleSynhc function in parallel for each iteration
DB = parLapply(cl, iterations, MultipleSynhc)
stopCluster(cl) # Stop the cluster after processing is complete

# Combine the results from all iterations
DB = bind_rows(DB)

# Save the final result as an RDS file
saveRDS(DB, '.\\Data\\ProcessedData\\Bootstrap\\PupilSynch_Boot.rds')
