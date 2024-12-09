
library(tidyverse)
library(zoo)


# Read Data, functions and Settings ------------------------------------------------

Hz = 20
window_sizeP = Hz * 2  # Window size for calculating rolling correlation

# Set the working directory to where your data is located
setwd("C:\\Users\\tomma\\OneDrive - Birkbeck, University of London\\PupilDilationSync_2023\\A-Pupil-Dilation-Technique-to-Test-Developmental-Differences-in-Visual-Synchrony")

# Load running average person correlation functions
source(".\\Analysis\\0_SynchronyFunctions.R")

# Read the input data
db = readRDS('.\\Data\\ProcessedData\\FinalData.rds')



# Extract Synchrony in Pupil ------------------------------------------------

#### Adults Group Processing
Adults = db %>%
  filter(Group == 'Adults') %>%
  arrange(Stimulus) %>%
  pivot_wider(names_from = Subject, values_from = Pupil_Residuals,
              id_cols = c("Seconds", "Stimulus", "Video"))

# Split data by Stimulus for separate processing
ByStimulus = Adults %>% split(.$Stimulus)
ByStimulus = lapply(ByStimulus, function(df) df %>% select(-1:-3))

# Calculate running correlation for each stimulus
Res = lapply(ByStimulus, calculate_running_correlation, window_sizeP = window_sizeP)
Adults$PupilSynch = unlist(Res, use.names = FALSE)

# Add the calculated pupil synchrony back to the Adults data
Adults = Adults %>%
  mutate(PupilSynch = unlist(Res, use.names = FALSE),
         Group = 'Adults') %>%
  select(Seconds, Group, Video, Stimulus, PupilSynch)


#### Children Group Processing
Children = db %>%
  filter(Group == 'Children') %>%
  arrange(Stimulus) %>%
  pivot_wider(names_from = Subject, values_from = Pupil_Residuals,
              id_cols = c("Seconds", "Stimulus", "Video"))

# Split data by Stimulus for separate processing
ByStimulus = Children %>% split(.$Stimulus)
ByStimulus = lapply(ByStimulus, function(df) df %>% select(-1:-3))

# Calculate running correlation for each stimulus
Res = lapply(ByStimulus, calculate_running_correlation, window_sizeP = window_sizeP)

# Add the calculated pupil synchrony back to the Children data
Children = Children %>%
  mutate(PupilSynch = unlist(Res, use.names = FALSE),
         Group = 'Children') %>%
  select(Seconds, Group, Video, Stimulus, PupilSynch)


#### Combine Adults and Children Results
Synch_Pupil = bind_rows(Children, Adults)

# Save the combined results to a CSV file
saveRDS(Synch_Pupil, '.\\Data\\ProcessedData\\Pupil_Synch.rds')



plotly::ggplotly(
  ggplot(Synch_Pupil, aes(y = PupilSynch, x =Seconds ,color = Group ))+
    geom_line(lwd=1)+
    facet_wrap(~Stimulus, scales = 'free_x')+
    theme_bw(base_size = 20)
)
