
library(tidyverse)
library(easystats)
library(parallel)
library(zoo)
library(patchwork)



# Read Data and Settings ------------------------------------------------------------

Hz = 20
window_sizeP = Hz * 2  # Window size for calculating the rolling correlation

# Set the working directory to where your data is located
setwd("C:\\Users\\tomma\\OneDrive - Birkbeck, University of London\\PupilDilationSync_2023\\A-Pupil-Dilation-Technique-to-Test-Developmental-Differences-in-Visual-Synchrony")

# Load running average person correlation functions
source(".\\Analysis\\0_SynchronyFunctions.R")

# Read the input data
db = vroom::vroom('.\\Data\\ProcessedData\\FinalData.csv')



# Cluster Preparation ----------------------------------------------------------

iterations = 1:1000

# Create a cluster using half of the available cores
cl = makeCluster(detectCores() / 2)

# Export necessary objects to each worker in the cluster
clusterExport(cl, c("db", "calculate_running_correlation", "average_fisher_z_transform", "window_sizeP"))

# Load necessary libraries in each worker
clusterEvalQ(cl, {
  library(tidyverse)
  library(zoo)
})



# Run the Cluster ---------------------------------------------------------

MultipleSynhc = function(x) {
  
  #### Adults
  Adults = db %>%
    filter(Group == 'Adults') %>%
    arrange(Stimulus) %>%
    pivot_wider(names_from = Subject, values_from = Pupil_Residuals, 
                id_cols = c("Seconds", "Stimulus", "Video")) 
  
  ByStimulus = Adults %>%
    split(.$Stimulus) %>%
    lapply(function(df) df %>% select(-1:-3))  # Remove non-numeric columns
  
  Res = lapply(ByStimulus, calculate_running_correlation, window_sizeP = window_sizeP)
  Adults$PupilSynch = unlist(Res, use.names = FALSE)
  
  Adults = Adults %>%
    mutate(PupilSynch = unlist(Res, use.names = FALSE),
           Group = 'Adults') %>%
    select(Seconds, Group, Video, Stimulus, PupilSynch)
  
  
  #### Children
  Children = db %>%
    filter(Group == 'Children')
  
  Children = Children %>%
    # Randomly sample 9 children to even the sample
    filter(Subject %in% sample(unique(Children$Subject), 9, replace = FALSE)) %>% # Sample 9 subjects
    arrange(Stimulus) %>%
    pivot_wider(names_from = Subject, values_from = Pupil_Residuals, 
                id_cols = c("Seconds", "Stimulus", "Video"))
  
  ByStimulus = Children %>%
    split(.$Stimulus) %>%
    lapply(function(df) df %>% select(-1:-3))  # Remove non-numeric columns
  
  Res = lapply(ByStimulus, calculate_running_correlation, window_sizeP = window_sizeP)
  
  Children = Children %>%
    mutate(PupilSynch = unlist(Res, use.names = FALSE),
           Group = 'Children') %>%
    select(Seconds, Group, Video, Stimulus, PupilSynch)
  
  
  #### Combine Adults and Children Data
  Synch_Pupil = bind_rows(Children, Adults) %>%
    mutate(Iteration = x)  # Add iteration info
  
  gc()  # Garbage collection
  return(Synch_Pupil)
}


# Run the parallel processing and combine the results
DBlist = parLapply(cl, iterations, MultipleSynhc)
stopCluster(cl)  # Stop the cluster

DB = bind_rows(DBlist)

# Save the combined results
saveRDS(DB, '.\\Data\\ProcessedData\\Bootstrap\\MainTtEst_boot.rds')
gc()  # Garbage collection

# Load the saved results
DB = readRDS('.\\Data\\ProcessedData\\Bootstrap\\MainTtEst_boot.rds')



# Run Analysis on Bootstrapped Data ------------------------------------------

# Aggregate data by group, video, stimulus, and time bins
s = 10  # Seconds per bin
DB = DB %>%
  mutate(Bins = Seconds %/% s) %>%
  group_by(Group, Video, Stimulus, Iteration, Bins) %>%
  summarise(PupilSynch = mean(PupilSynch, na.rm = TRUE)) %>%
  mutate(Seconds = Bins * s) %>%
  ungroup()

# Define a function to run t-test on each iteration
Test_Boot = function(x) {
  DF = DB %>%
    filter(Iteration == x)
  mod = t.test(PupilSynch ~ Group, data = DF)
  
  return(mod$p.value)
}

# Run the t-test for each iteration
Tt = sapply(unique(DB$Iteration), Test_Boot)



# Plot the Distribution of p-values ---------------------------------------------------

# Estimate the density of p-values
Density = estimate_density(Tt, method = 'kernel')

# Find the maximum density point
MaxP = Density[which.max(Density$y), ]$x

# Create the plot
ggplot(Density, aes(x = x, y = y)) +
  geom_line(color = '#7f8dc4') +
  geom_ribbon(aes(ymin = 0, ymax = y), fill = '#7f8dc4', alpha = 9) +
  geom_point(x = MaxP, y = max(Density$y), size = 4, color = 'black', fill = '#5C6EB4', pch = 21) +
  annotate("text", x = MaxP * 6, y = max(Density$y)+1.3, label = as.character(round(MaxP, 4)), color = 'black', size = 9) +
  geom_segment(x = 0.05, xend = 0.05, y = 0, yend = 10, linetype = 'dashed', color = 'darkred', lwd = 1.4) +
  geom_point(x = 0.05, y = 10, color = 'darkred', size = 4) +
  annotate("text", x = 0.05, y = 11, label = '0.05', color = 'black', size = 9) +
  theme_minimal(base_size = 40) +
  labs(x = 'P-value', y = 'Density')

# Save the plot
ggsave('.\\Results\\TTestMain_boot.svg', width = 22, height = 12, dpi = 300)