library(tidyverse)
library(easystats)
library(zoo)


# Read Data, functions and Settings ------------------------------------------------

Hz = 20
window_sizeP = Hz * 2  # Window size for calculating rolling correlation

# Set the working directory to where your data is located
setwd("C://Users//tomma//OneDrive - Birkbeck, University of London//PupilDilationSync_2023//A-Pupil-Dilation-Technique-to-Test-Developmental-Differences-in-Visual-Synchrony")

# Load running average person correlation functions
source(".//Analysis//0_SynchronyFunctions.R")




#%% Plot1 ------------------------------------------------------------------
db = vroom::vroom('.//Data//ProcessedData//FinalData.csv')

db = db |> 
  filter( Subject == 30,
    Group == 'Children',
      Stimulus=='Boundin1',
      Seconds < 30) |> 
  mutate(
    Pupil = normalize(Pupil)+3.5,
    Pupil_Residuals = normalize(Pupil_Residuals),
    GazeX = normalize(GazeX)+5,
    GazeY = normalize(GazeY)+6.5 )

ggplot(db , aes(x = Seconds, y = GazeX)) +
  geom_line(aes(y = GazeX)) + 
  geom_line(aes(y = GazeY)) + 
  geom_line(aes(y = Pupil)) + 
  geom_line(aes(y = Pupil_Residuals)) + 
  theme_void(base_size = 20)



ggsave('C:/Users/tomma/OneDrive - Birkbeck, University of London/PupilDilationSync_2023/Manuscript/Figures/Residuals.svg',
       width = 22, height = 12, dpi = 300)








