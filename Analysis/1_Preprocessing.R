library(tidyverse)
library(easystats)
library(zoo)



# Load and Set Working Directory ------------------------------------------

# Set the working directory to where your data is located
setwd("C:\\Users\\tomma\\OneDrive - Birkbeck, University of London\\PupilDilationSync_2023\\A-Pupil-Dilation-Technique-to-Test-Developmental-Differences-in-Visual-Synchrony")

# Load running average person correlation functions
source(".\\Analysis\\0_SynchronyFunctions.R")

# Load the pre-processed data from CSV
db = readRDS('.\\Data\\Raw.rds')



# Settings ----------------------------------------------------------------

# Define key parameters for processing
Hz = 120  # Original sampling rate (Hz)

# Define maximum interpolation time window for missing data (in seconds)
maxTimePup = 1.5  # for pupil diameter data
maxTimeGaze = 0.5  # for gaze coordinates data

# Define the target sampling rate for down-sampling (Hz)
Hz_to_resample = 20

# Create a data frame with stimulus names and their corresponding maximum duration
stimulus_duration_df = data.frame(
  Stimulus = c('Boundin1','Boundin2','Boundin3','Dustin1','Dustin2','Dustin3',
               'Lifted1','Lifted2','Lifted3','Soar1','Soar2','Soar3'),
  MaxDuration = c(65.5, 126.26, 63.73, 128.29, 100.07, 170.07,
                  60.03, 122.86, 88.289, 77.58, 160.79, 53.85)
)



# Partial correlation preparation -----------------------------------------

db = db %>% 
  group_by(Subject) %>% 
  mutate(
    Pupil_Residuals =  Residuals_lm(pick(everything()))
    )



# Data Pre-processing -----------------------------------------------------

# Calculate the size of each time bin for down-sampling
timebin_size = 1 / Hz_to_resample

Pup = db %>%
  
  # Apply median filtering to smooth out noise in the data
  group_by(Group, Subject, Video, Stimulus) %>%
  mutate(
    Pupil = runmed(Pupil, 11, endrule = 'keep'),
    Pupil_Residuals = runmed(Pupil_Residuals, 11, endrule = 'keep'),
    GazeX = runmed(GazeX, 5, endrule = 'keep'),
    GazeY = runmed(GazeY, 5, endrule = 'keep')
  )  %>%
  ungroup() %>%
  
  # Down-sample the data to the target sampling rate (Hz_to_resample)
  mutate(Timebin = floor(Seconds / timebin_size)) %>%
  group_by(Group, Stimulus, Video, Subject, Timebin) %>%
  summarise(Pupil = mean(Pupil, na.rm = TRUE),
            Pupil_Residuals = mean(Pupil_Residuals, na.rm = TRUE),
            GazeX = mean(GazeX, na.rm = TRUE),
            GazeY = mean(GazeY, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # Recalculate the time in seconds based on the down-sampled time bins
  mutate(Seconds = Timebin * timebin_size) %>%
  mutate(Seconds = trunc(Seconds*1000)/1000) %>%
  select(-Timebin) %>%
  ungroup() %>%
  
  # Interpolate missing data within the defined maximum time window
  group_by(Group, Subject, Video, Stimulus) %>%
  mutate(Pupil = na.approx(Pupil, maxgap = maxTimePup*Hz, na.rm = FALSE),
        Pupil_Residuals = na.approx(Pupil_Residuals, maxgap = maxTimePup*Hz, na.rm = FALSE),
        GazeX = na.approx(GazeX, maxgap = maxTimeGaze*Hz, na.rm = FALSE),
        GazeY = na.approx(GazeY, maxgap = maxTimeGaze*Hz, na.rm = FALSE)) %>%
  ungroup()



# Plotting ----------------------------------------------------------------

# Create plots for each stimulus showing GazeX, GazeY, and Pupil data over time
df = Pup |> 
  mutate(
    Pupil = normalize(Pupil),
    Pupil_Residuals = normalize(Pupil_Residuals)+1.5,
    GazeX = normalize(GazeX)+3,
    GazeY = normalize(GazeY)+4.5 )

plt = list()
for (vid in unique(df$Stimulus)){
  
  plt[[vid]] = ggplot(filter(df, Stimulus == vid), aes(x = Seconds, y = GazeX)) +
    
    # Plot GazeX, GazeY, and Pupil data with different colors
    geom_line(color = 'red') +
    geom_line(aes(y = Pupil_Residuals), color = 'gold')+
    geom_line(aes(y = GazeY), color = 'green') +
    geom_line(aes(y = Pupil), color = 'blue') +
    labs(y = "Signals")+
    labs(title = vid) +
    # Customize legend colors
    scale_color_manual(
      name = "Signals", 
      values = c('GazeX (red)' = 'red', 
                 'GazeY (green)' = 'green', 
                 'Pupil (blue)' = 'blue'))+
    
    facet_wrap(~Subject)
}

# Display each plot for manual inspection
for (vid in unique(df$Stimulus)){
  print(plt[[vid]])
}



# Manual Data Rejection Based on Visual Inspection ------------------------

# Identify data points to exclude based on manual visual inspection of plots
ToExclude = Pup %>%
  rowid_to_column() %>%
  filter(
    ((Subject == 61 | Subject == 58) & (Stimulus == 'Boundin1' |
                                          Stimulus == 'Lifted3' |
                                          Stimulus == 'Soar2')) |
      (Subject == 58 & Stimulus == 'Boundin2') |
      (Subject == 43 & Stimulus == 'Boundin2' & Seconds > 100) |
      (Subject == 31 & Stimulus == 'Boundin2' & (Seconds > 82 & Seconds < 92)) |
      (Subject == 58 & (Stimulus == 'Boundin1' | Stimulus == 'Boundin2' |
                          Stimulus == 'Dustin1' | Stimulus == 'Dustin2' |
                          Stimulus == 'Soar3')) |
      (Subject == 58 & Stimulus == 'Dustin3' & Seconds < 7) |
      (Subject == 78 & Stimulus == 'Lifted2' & Seconds < 18)) %>%
  pull(rowid)

# Apply the exclusions by setting the identified rows to NA
Pup = Pup %>%
  mutate(Pupil = if_else((as.numeric(rownames(Pup)) %in% ToExclude), NA, Pupil),
         Pupil_Residuals = if_else((as.numeric(rownames(Pup)) %in% ToExclude), NA, Pupil_Residuals),
         GazeX = if_else((as.numeric(rownames(Pup)) %in% ToExclude), NA, GazeX),
         GazeY = if_else((as.numeric(rownames(Pup)) %in% ToExclude), NA, GazeY))



# Handling Missing Data ---------------------------------------------------

# Filter out trials with more than 75% missing data
# Remove subjects who do not have at least 75% valid data across trials

Pup = Pup %>%
  left_join(stimulus_duration_df, by = "Stimulus") %>%
  group_by(Subject, Stimulus) %>%
  
  mutate(percent_data = 100* sum(!is.na(Pupil_Residuals))/(first(MaxDuration) * Hz_to_resample),
         GazeX = if_else(percent_data < 25, NA, GazeX),
         GazeY = if_else(percent_data < 25, NA, GazeY),
         Pupil = if_else(percent_data < 25, NA, Pupil),
         Pupil_Residuals = if_else(percent_data < 25, NA, Pupil_Residuals)) %>%
  ungroup() 
  
# Remove subjects with insufficient valid data across multiple trials
Remove = Pup %>%
  group_by(Subject, Stimulus) %>%
  summarise(above_75 = first(percent_data)) %>%
  ungroup() %>%
  
  group_by(Subject) %>%
  summarise(videos_above_75 = sum(above_75 > 0.75)) %>%
  filter(videos_above_75 < 9) %>%
  pull(Subject)

# Filter out the identified subjects with insufficient data
Pup = Pup %>%
  filter(!Subject %in% Remove)



# Save the Processed Data -------------------------------------------------

# Save the final processed data to a CSV file for further analysis
saveRDS(Pup, '.\\Data\\ProcessedData\\FinalData.rds')

Pup |> 
  filter(Subject == 25, Seconds < 10) |> 
  ggplot( aes(x = Seconds, y = Pupil)) +
  geom_line() +
  facet_wrap( ~ Stimulus, scales = "free_x", ncol = 1) +  # Allow x-axis to vary between facets
  theme_bw(base_size = 20) 


db |> 
  filter(Stimulus == vid, Subject == 25, Seconds<20) |> 


ggplot(aes(x = Seconds, y = Pupil)) +
    
    # Plot GazeX, GazeY, and Pupil data with different colors
    geom_line(color = 'red') +
    geom_line(aes(y = Pupil_Residuals), color = 'gold')+
    geom_line(aes(y = GazeY), color = 'green') +
    geom_line(aes(y = Pupil), color = 'blue') +
    labs(y = "Signals")+
    labs(title = vid) +
    # Customize legend colors
    scale_color_manual(
      name = "Signals", 
      values = c('GazeX (red)' = 'red', 
                 'GazeY (green)' = 'green', 
                 'Pupil (blue)' = 'blue'))
