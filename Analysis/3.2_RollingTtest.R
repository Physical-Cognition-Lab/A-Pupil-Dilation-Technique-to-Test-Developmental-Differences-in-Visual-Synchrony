
library(tidyverse)
library(zoo)

# Functions ---------------------------------------------------------------

# RollingT function performs a t-test on rolling windows of the data.
# It returns the p-value and t-statistic, or NA if the threshold of NA values is exceeded.
RollingT = function(df) {
  df = as.data.frame(df)
  Child_window = as.numeric(df$Children)
  Adu_window = as.numeric(df$Adult)
  
  # Check for NA values in each window and compare with threshold
  if (sum(is.na(Child_window)) / length(Child_window) > 0.75 || sum(is.na(Adu_window)) / length(Adu_window) > 0.75) {
    return(c(Pval = NA, t_value = NA))
  } else {
    test_result = t.test(Child_window, Adu_window, paired = FALSE, alternative = "two.sided")
    return(c(Pval = test_result$p.value, t_value = test_result$statistic))
  }
}



# ReadData ----------------------------------------------------------------

Hz = 20
window_size = 4 * Hz  # Set window size based on Hz

# Set the working directory to where your data is located
setwd("C:\\Users\\tomma\\OneDrive - Birkbeck, University of London\\PupilDilationSync_2023\\A-Pupil-Dilation-Technique-to-Test-Developmental-Differences-in-Visual-Synchrony")

# Read the input data files
db = readRDS('.\\Data\\ProcessedData\\FinalData.rds')
Synch_Pupil = readRDS('.\\Data\\ProcessedData\\Pupil_Synch.rds')



# Rolling TTest Pupil -----------------------------------------------------

# Prepare data: Convert data to wide format with Group as column names
Synch_Pupilupil_Wide = Synch_Pupil %>%
  pivot_wider(names_from = Group, values_from = PupilSynch,
              id_cols = c("Seconds", "Stimulus", 'Video')) %>%
  arrange(Stimulus, Video, Seconds) %>%
  ungroup()

# Split data by stimulus for separate processing
data_by_stimulus_P = Synch_Pupilupil_Wide %>%
  split(.$Stimulus)

# Apply the RollingT function to each group separately and store results
result_list = lapply(data_by_stimulus_P, function(group_df) {
  rollapply(group_df, width = window_size, FUN = function(df) RollingT(df), by.column = FALSE, fill = NA, partial = TRUE)
})

# Combine lists into a single data frame
long_2d_list = do.call(rbind, result_list)
long_2d_list = cbind(ListName = rep(names(result_list), times = sapply(result_list, nrow)), long_2d_list)

# Bind the t-test results back to the original data
TTEST_Pupil = Synch_Pupilupil_Wide %>%
  bind_cols(., long_2d_list)

# Adjust p-values using FDR correction and determine significance
TTEST_Pupil = TTEST_Pupil %>%
  group_by(Video, Stimulus) %>%
  mutate(Pval = as.numeric(Pval),
         Pval.fdr = as.numeric(p.adjust(Pval, method = "fdr"))) %>%
  ungroup() %>%
  mutate(Significance = case_when(
    Pval >= 0.05 ~ FALSE,
    Pval < 0.05 ~ TRUE,
    is.na(Pval) ~ FALSE),
    Significance.fdr = case_when(
      Pval.fdr >= 0.05 ~ FALSE,
      Pval.fdr < 0.05 ~ TRUE,
      is.na(Pval.fdr) ~ FALSE))

# Write the final results to a CSV file
saveRDS(TTEST_Pupil, '.\\Data\\ProcessedData\\RollingTtest_Pupil.rds')
