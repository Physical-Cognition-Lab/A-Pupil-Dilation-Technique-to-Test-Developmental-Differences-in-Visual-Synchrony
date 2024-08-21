
library(tidyverse)  # Core library for data manipulation and visualization
library(gridExtra)  # Arrange multiple grid-based plots in a grid layout
library(patchwork)  # Combine plots created by ggplot2
library(gt)         # Create presentation-ready tables
library(gtsummary)  # Summary statistics and tables
library(easystats)  # Easystats ecosystem for statistical modeling


# Define a custom theme for gt tables
gt_theme_538 = function(data, decimals = 3, ...) {
  data %>%
    opt_all_caps() %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),  # Use the Chivo font from Google Fonts
        default_fonts()        # Fall back to default fonts if Chivo is unavailable
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        rows = nrow(data$`_data`)  # Apply style to the bottom border of the last row
      )
    ) %>%
    tab_options(
      column_labels.background.color = "white",  # Set background color for column labels
      table.border.top.width = px(3),           # Set top border width for the table
      table.border.top.color = "transparent",   # Set top border color for the table
      table.border.bottom.color = "transparent", # Set bottom border color for the table
      table.border.bottom.width = px(3),        # Set bottom border width for the table
      column_labels.border.top.width = px(3),   # Set top border width for column labels
      column_labels.border.top.color = "transparent",  # Set top border color for column labels
      column_labels.border.bottom.width = px(3),       # Set bottom border width for column labels
      column_labels.border.bottom.color = "black",     # Set bottom border color for column labels
      data_row.padding = px(3),                       # Set padding for data rows
      source_notes.font.size = 12,                    # Set font size for source notes
      table.font.size = 16,                           # Set overall table font size
      heading.align = "left",                         # Align table heading to the left
      ...
    )
}



# Set Working Directory and Load Data -------------------------------------

# Set the working directory to the location where your data files are stored
setwd("C:\\Users\\tomma\\Desktop\\Share")

# Load the synchronized pupil data and modify the 'Stimulus' column format
Synch_Pupil = vroom::vroom('.\\Data\\ProcessedData\\Pupil_Synch.csv') %>%
  mutate(Stimulus = paste(str_sub(Stimulus, 1, -2), ' Chunk', str_sub(Stimulus, -1, -1), sep = ''))

# Load the rolling t-test results and modify the 'Stimulus' column format
Ttest_Pupil = vroom::vroom('.\\Data\\ProcessedData\\RollingTtest_Pupil.csv') %>%
  mutate(Stimulus = paste(str_sub(Stimulus, 1, -2), ' Chunk', str_sub(Stimulus, -1, -1), sep = ''))

# Load the bootstrapped t-test results from an RDS file and modify the 'Stimulus' column format
Ttest_Pupil_Boot = readRDS('.\\Data\\ProcessedData\\Bootstrap\\TTEstPupil_Boot.rds') %>%
  mutate(Stimulus = paste(str_sub(Stimulus, 1, -2), ' Chunk', str_sub(Stimulus, -1, -1), sep = ''))



# Non bootstrapped data -----------------------

## Identify and Summarize Significant Chunks of Data -----------------------

## Identify contiguous chunks of significant results
Streak_Pupil = Ttest_Pupil %>%
  group_by(Video, Stimulus) %>%
  # Create a streak column to identify contiguous significant periods
  mutate(Streak = cumsum(!Significance.fdr)) %>%
  ungroup()


## Summarize the chunks, focusing on those longer than 2 seconds
Chunks_Pupil = Streak_Pupil %>%
  filter(Significance.fdr == TRUE) %>%
  group_by(Streak, Stimulus) %>%
  summarize(
    START = min(Seconds),  # Start time of the significant chunk
    END = max(Seconds),    # End time of the significant chunk
    DUR = END - START,     # Duration of the chunk
    Children = mean(Children, na.rm = TRUE),  # Average children data
    Adults = mean(Adults, na.rm = TRUE)       # Average adults data
  ) %>%
  filter(DUR >= 2)  # Retain only chunks with a duration of 2 seconds or more



### Plot Pupil Synchrony and Significant Chunks -----------------------------

## Plot the pupil synchrony with highlighted significant chunks
Synch_Pupil %>%
  ggplot(aes(x = Seconds, y = PupilSynch, color = Group)) +
  geom_rect(
    inherit.aes = FALSE, data = Chunks_Pupil,
    aes(xmin = START, xmax = END, ymin = -Inf, ymax = Inf), 
    fill = '#F2D76A', color = 'black'
  ) + 
  geom_line(linewidth = 1.2) +
  facet_wrap(~Stimulus, ncol = 3, scales = 'free_x') +
  theme_minimal(base_size = 25) +
  scale_color_manual(values = c("#BD3538", "#455BA6")) +
  labs(x = 'Time (s)', y = 'Pupil Synchrony')
ggsave('.\\Results\\PupilSynchronyChunks.svg', height = 12, width = 20, dpi = 300)


## Plot the density distribution of chunk durations
ggplot(Chunks_Pupil, aes(x = DUR)) +
  geom_histogram(aes(y = ..ncount..), fill = '#F2D76A', color = 'black', alpha = 0.7, binwidth = 0.25) +
  geom_density(aes(y = ..ndensity..), lwd = 1.3, color = 'black', fill = '#F2D76A', alpha = 0.5) +
  theme_classic(base_size = 24) +
  labs(x = 'Duration (s)', y = 'Density') +
  scale_x_continuous(breaks = seq(0, max(Chunks_Pupil$DUR), by = 5))
ggsave('.\\Results\\DistributionChunkLength.svg', height = 12, width = 18, dpi = 300)



### Analyze Chunk Information -----------------------------------------------

## Calculate and display the maximum P-value and minimum T-value for significant chunks
Ttest_Pupil %>%
  filter(Significance.fdr == TRUE) %>%
  summarise(`Max Pvalue` = max(Pval.fdr), `Min Tvalue` = min(t_value.t)) %>%
  gt() %>%
  tab_header(title = "Max Pvalue and Min Tvalue") %>%
  gt_theme_538()


## Summarize chunk duration and count statistics
Chunks_Pupil %>%
  group_by(Stimulus) %>%
  summarise(
    N = n(),  # Number of chunks per stimulus
    Mea = mean(DUR)  # Mean duration of chunks per stimulus
  ) %>%
  ungroup() %>%
  summarise(
    DurationChunks_Mean = mean(Mea),  # Average duration of chunks across stimuli
    DurationChunks_Sd = sd(Mea),      # Standard deviation of chunk durations
    NumberChunks_Mean = mean(N),      # Average number of chunks across stimuli
    NumberChunks_Sd = sd(N)           # Standard deviation of chunk counts
  ) %>%
  pivot_longer(cols = c(DurationChunks_Mean, DurationChunks_Sd, NumberChunks_Mean, NumberChunks_Sd),
               names_to = c(".value", "Statistic"), names_sep = "_") %>%
  rename(`Duration Chunks` = DurationChunks, `Number Chunks` = NumberChunks) %>% 
  gt(rowname_col = "Statistic") %>%
  tab_header(title = "Duration of chunks") %>%
  gt_theme_538()



## Perform ANOVA -----------------------------------------

## Perform ANOVA to test if chunk durations vary significantly by stimulus
TimeChunk = aov(DUR ~ Stimulus, data = Chunks_Pupil)

## Display ANOVA results in a table
as.data.frame(parameters(TimeChunk)) %>%
  gt(rowname_col = 'Parameter') %>%
  tab_header(title = "Anova on the duration by video chunk") %>%
  gt_theme_538()





# Bootstrapped Data -----------------------

## Extract Chunks Information from Bootstrapped Data -----------------------

## Identify contiguous chunks of significant results in bootstrapped data
Streak_Pupil_Boot = Ttest_Pupil_Boot %>%
  group_by(Iteration, Video, Stimulus) %>%
  # Create a streak column to identify contiguous significant periods
  mutate(Streak = cumsum(!Significance.fdr)) %>%
  ungroup()

## Summarize the chunks, focusing on those longer than 2 seconds
Chunks_Pupil_boot = Streak_Pupil_Boot %>%
  filter(Significance.fdr == TRUE) %>%
  group_by(Iteration, Streak, Stimulus) %>%
  summarize(
    START = min(Seconds),  # Start time of the significant chunk
    END = max(Seconds),    # End time of the significant chunk
    DUR = END - START      # Duration of the chunk
  ) %>%
  filter(DUR >= 2)  # Retain only chunks with a duration of 2 seconds or more



### Identify and Count Repetitions with Threshold ---------------------------

# Define the threshold for similarity between non-random and bootstrapped data
threshold = 0.25

# Add a Counter column to Chunks_Pupil_boot for counting matching chunks
Chunks_Pupil_boot$Counter = 1
Chunks_Pupil_boot$Row = rownames(Chunks_Pupil_boot)  # Store row names for reference

# Initialize the Counter column in Chunks_Pupil
Chunks_Pupil$Counter = 0

# Loop over each row in Chunks_Pupil to identify matching chunks in bootstrapped data
for (A in 1:nrow(Chunks_Pupil)) {
  # Select the current row from Chunks_Pupil
  row_A = Chunks_Pupil[A,]
  
  # Filter bootstrapped data to only include rows with the same stimulus
  Stimulus_condition = Chunks_Pupil_boot[Chunks_Pupil_boot$Stimulus == row_A$Stimulus,]
  
  # Calculate the absolute difference between the start and end times
  diff_Start = abs(Stimulus_condition$START - row_A$START)
  diff_Stop = abs(Stimulus_condition$END - row_A$END)
  
  # Identify rows where both start and end times are within the threshold
  idx = which(diff_Start < threshold & diff_Stop < threshold)
  Chunks_Pupil[A, 'Counter'] = length(idx)  # Count the number of matching chunks
}


## Display Significant Chunks also Found in Bootstrapped Data
Chunks_Pupil %>%
  ungroup() %>%
  arrange(Stimulus, Streak) %>%
  gt(rowname_col = "Stimulus") %>%
  tab_header(title = "Significant Chunks of Pupil Also Found in the Randomized Data") %>%
  gt_theme_538()


## Summary of Chunks Found in Bootstrapped Data

# Display chunks from original data that were also found in the bootstrapped data
Chunks_Pupil %>%
  ungroup() %>%
  select(-Streak, -Adults, -Children) %>%
  arrange(Stimulus) %>%
  filter(Counter > 0) %>%
  gt(rowname_col = 'Stimulus') %>%
  tab_header(title = "Significant Chunks of Pupil Also Found in the Randomized Data") %>%
  gt_theme_538()



### Wilcoxon Test on Chunk Duration (Non-Random vs Bootstrapped) -------------

# Prepare data for Wilcoxon test comparing chunk duration between non-random and bootstrapped data
Dur_NonRandom = Chunks_Pupil %>%
  mutate(Type = 'Non Random') %>%
  select(DUR, Type, Stimulus)

Dur_Random = Chunks_Pupil_boot %>%
  group_by(Streak, Stimulus) %>%
  summarize(DUR = mean(DUR)) %>%
  mutate(Type = 'Random') %>%
  select(DUR, Type, Stimulus)

# Perform Wilcoxon test
Wilcx = wilcox.test(Dur_Random$DUR, Dur_NonRandom$DUR)
parameters(Wilcx) %>%
  select(W, p) %>%
  mutate(Parameter = 'Non-random vs Random') %>%
  gt(rowname_col = 'Parameter') %>%
  tab_header(title = 'Wilcoxon Test Comparing Duration Between Conditions') %>%
  gt_theme_538()



## Density Plot of Chunks Duration in Bootstrapped Data --------------------

# Analyze and plot the distribution of chunk duration above 2s in bootstrapped data
RandomAbove2 = Chunks_Pupil_boot %>%
  ungroup() %>%
  mutate(Chunks_above_2s = if_else(DUR > 2, 'Above', 'Below')) %>%
  filter(Chunks_above_2s == 'Above') %>%
  group_by(Iteration, Chunks_above_2s) %>%
  summarize(Number_Of_Chunks_Above_2s = n()) %>%
  select(-Chunks_above_2s) %>%
  ungroup() %>%
  mutate(Iteration = factor(Iteration))

# Display the table summarizing the number of chunks above 2s per iteration
RandomAbove2 %>%
  gt(rowname_col = 'Iteration') %>%
  tab_header(title = "Number of Chunks Above 2s in the Randomized Data") %>%
  gt_theme_538()

# Count and plot the number of 2s chunks in non-random data for comparison
NonR2sch = Chunks_Pupil %>%
  ungroup() %>%
  filter(DUR >= 2) %>%
  summarize(N = n())

# Plot the density of the number of chunks above 2s in the randomized data
ggplot(RandomAbove2, aes(x = Number_Of_Chunks_Above_2s)) +
  geom_histogram(aes(y = ..ncount..), fill = '#AF6FB0', color = 'black', alpha = 0.7, binwidth = 1) +
  geom_density(aes(y = ..ndensity..), lwd = 1.3, color = 'black', fill = '#AF6FB0', alpha = 0.5) +
  geom_line(data = data.frame(x = c(NonR2sch$N, NonR2sch$N), y = c(0, .8)), aes(x = x, y = y),
            linewidth = 3, color = '#E0C763') +
  geom_hline(yintercept = 0, linewidth = 1.2) +
  geom_point(x = NonR2sch$N, y = 0.8, shape = 21, size = 8, color = 'black', fill = '#E0C763') +
  geom_text(x = 120, y = .9, size = 8, label = '# of time windows found in\nthe non-randomized data') +
  theme_classic(base_size = 25) +
  labs(x = 'Number of Significant Time Windows', y = 'Density',
       title = 'Density of the Number of Chunks Above 2s in the Randomized Data') +
  scale_x_continuous(breaks = seq(0, NonR2sch$N, by = 20))

