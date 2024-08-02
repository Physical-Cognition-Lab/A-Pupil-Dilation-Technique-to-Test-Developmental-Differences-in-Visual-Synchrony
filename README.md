# A Pupil Dilation Technique to Test Developmental Differences in Visual Synchrony

This repository houses the R scripts used to analyze pupil dilation data in a study comparing visual synchrony between children and adults. The project explores pupil synchrony as a novel measure of visual synchrony in developmental research.

## File description

The files have been numberred to define critical subset of scripts and their intended order.

- **0_SynchronyFunctions.R**<br>
Contains the code required to extract pupil dilation synchrony between children and adults the functions to extractteh running pupil dilation synchrony between children and adults. As these functions are used repeatedly in the following scripts, they have been separated into a distinct file for improved organization.
  - ***calculate_running_correlation()***<br>
*Computes running correlations over a specified window size in a time series matrix. It handles NA values and applies Fisher's z-transformation to the correlation matrix.*
  - ***average_fisher_z_transform()***<br>
*Calculates the average correlation from a correlation matrix using Fisher's z-transformation. This function adjusts for perfect correlations and manages NA values to provide a robust measure of overall correlation.*
<br>

- **1_Preprocessing.R**<br>
Script to preprocess the pupil dilation data
<br>

- **2_ExtractSynchrony.R**<br>
This script applies the synchrony functions to extract pupil dilation synchrony measures for both children and adults. It utilizes the functions defined in **0_SynchronyFunctions.R** to calculate running correlations and average synchrony scores.
<br>

- **2boot_ExtractSynchrony.R**<br>
This script performs a bootstrapping analysis of pupil dilation synchrony. It randomly reassigns child and adult labels 1000 times, extracting synchrony measures for each iteration. The script employs parallel computing using the `parallel` library and `parlapply()` function to enhance computational efficiency.
<br>

- **3.1_MainTTest.R**<br>
This script performs a t-test comparison of pupil dilation synchrony between children and adults. To address the imbalance in group sizes, it randomly subsets the children's group to match the adult group size, repeating this process 1000 times. Synchrony data is binned into 10-second intervals, and a t-test is computed for each iteration. The resulting p-value distribution is then compared against the standard 0.05 threshold. The script employs parallel computing using the `parallel` library and `parlapply()` function to enhance computational efficiency.
<br>

- **3.2_RollingTtest.R**<br>
This script implements a rolling t-test over the pupil dilation synchrony data for children and adults. The resulting p-values are corrected for false discovery rate, allowing identification of specific data segments where the two groups' synchrony signals significantly differ.
<br>


- **3.2boot_RollingTTest.R**<br>
An adaptation of the previous script, this version is designed to run the rolling t-test over the 1000 iterations of pupil dilation synchrony extracted from **2boot_ExtractSynchrony.R**. This allows to account for the bootstrapped data.
<br>

- **4_ExportingInformation.R**<br>
The final script in the pipeline, used to compile and compare the various synchrony signals. It generates tables and plots illustrating the differences in pupil dilation synchrony between children and adults, providing a comprehensive overview of the study's findings.






