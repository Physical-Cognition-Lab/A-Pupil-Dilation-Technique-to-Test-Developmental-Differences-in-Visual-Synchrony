
library(tidyverse)
library(zoo)



# Load and Set Working Directory ------------------------------------------

# Set the working directory to where your data is located
setwd("C:\\Users\\tomma\\Desktop\\Share")

# Load the pre-processed data from CSV
db = vroom::vroom('.\\Data\\Raw.csv')



# Settings ----------------------------------------------------------------

# Define key parameters for processing
Hz = 120  # Original sampling rate (Hz)
framesize = 1/Hz  # Time interval between frames (unused but defined here)

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



# Data Pre-processing -----------------------------------------------------

# Calculate the size of each time bin for down-sampling
timebin_size = 1 / Hz_to_resample

Pup = db %>%
  
  # Apply median filtering to smooth out noise in the data
  group_by(Group, Subject, Video, Stimulus) %>%
  mutate(Pupil = runmed(Pupil, 11, endrule = 'keep')) %>%
  mutate(GazeX = runmed(GazeX, 5, endrule = 'keep')) %>%
  mutate(GazeY = runmed(GazeY, 5, endrule = 'keep')) %>%
  ungroup() %>%
  
  # Down-sample the data to the target sampling rate (Hz_to_resample)
  mutate(Timebin = floor(Seconds / timebin_size)) %>%
  group_by(Group, Stimulus, Video, Subject, Timebin) %>%
  summarise(Pupil = mean(Pupil, na.rm = TRUE),
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
         GazeX = na.approx(GazeX, maxgap = maxTimeGaze*Hz, na.rm = FALSE),
         GazeY = na.approx(GazeY, maxgap = maxTimeGaze*Hz, na.rm = FALSE)) %>%
  ungroup()



# Plotting ----------------------------------------------------------------

# Create plots for each stimulus showing GazeX, GazeY, and Pupil data over time
df = Pup
plt = list()
for (vid in unique(df$Stimulus)){
  
  plt[[vid]] = ggplot(filter(df, Stimulus == vid), aes(x = Seconds, y = GazeX)) +
    
    # Plot GazeX, GazeY, and Pupil data with different colors
    geom_line(color = 'red') +
    geom_line(aes(y = -GazeY), color = 'green') +
    geom_line(aes(y = Pupil*100), color = 'blue') +
    labs(title = vid) +
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
         GazeX = if_else((as.numeric(rownames(Pup)) %in% ToExclude), NA, GazeX),
         GazeY = if_else((as.numeric(rownames(Pup)) %in% ToExclude), NA, GazeY))



# Handling Missing Data ---------------------------------------------------

# Filter out trials with more than 75% missing data
# Remove subjects who do not have at least 75% valid data across trials
Pup = Pup %>%
  left_join(stimulus_duration_df, by = "Stimulus") %>%
  group_by(Subject, Stimulus) %>%
  
  # Mark trials with excessive missing data (more than 75%)
  mutate(percent_missing = 100 * (sum(is.na(Pupil)) / (first(MaxDuration) * Hz_to_resample)),
         GazeX = ifelse(percent_missing > 75, NA, GazeX),
         GazeY = ifelse(percent_missing > 75, NA, GazeY),
         Pupil = ifelse(percent_missing > 75, NA, Pupil)) %>%
  ungroup() %>%
  
  # Reject trials that do not have enough collected data (less than 75% of the trial duration)
  group_by(Group, Subject, Video, Stimulus) %>%
  filter(max(Seconds) * 100 / first(MaxDuration) > 75) %>%
  ungroup()

# Remove subjects with insufficient valid data across multiple trials
Remove = Pup %>%
  group_by(Subject, Video) %>%
  summarise(above_75 = first(percent_missing)) %>%
  ungroup() %>%
  
  group_by(Subject) %>%
  summarise(videos_above_75 = sum(above_75 > 0.75)) %>%
  filter(videos_above_75 > 3) %>%
  pull(Subject)

# Filter out the identified subjects with insufficient data
Pup = Pup %>%
  filter(!Subject %in% Remove)



# Save the Processed Data -------------------------------------------------

# Save the final processed data to a CSV file for further analysis
write.csv(Pup, '.\\Data\\ProcessedData\\FinalData.csv', row.names = FALSE)
