
library(tidyverse)
library(zoo)



# Functions to calculate population synchrony ------------------------------------------------------------

#### Running correlation
# This function calculates running correlations over a specified window size.
calculate_running_correlation = function(Matrix, window_sizeP) {
  # Define a function to calculate correlation for each window
  calc_correlation = function(window_rows) {
    
    # Check if more than 75% of the window has NA values
    if (sum(is.na(window_rows)) > 0.75 * prod(dim(window_rows))) {
      return(NA)
    } else {
      # Discard columns where more than 75% of values are NA
      window_rows[, colMeans(is.na(window_rows)) > 0.75] = NA
      
      # Calculate the correlation matrix for the current window and apply Fisher's z-transform
      correlation_matrix = cor(window_rows, use = "pairwise.complete.obs")
      correlation = average_fisher_z_transform(correlation_matrix)
      return(correlation)
    }
  }
  
  # Apply the calc_correlation function to each rolling window in the Matrix
  correlation_matrices = rollapply(Matrix, width = window_sizeP, FUN = calc_correlation,
                                   by.column = FALSE, align = "center", partial = TRUE)
  
  return(correlation_matrices)
}




#### Extract Average normalized person correlation
# This function applies Fisher's z-transformation to the correlation matrix and returns the average correlation.
average_fisher_z_transform = function(corr_matrix, threshold = 0.75) {
  # Check if a significant portion of columns have NA values
  if (sum(colSums(is.na(corr_matrix)) >= ncol(corr_matrix) - 1) > threshold * ncol(corr_matrix)) {
    return(NA)
  }
  # Take the lower triangle of the correlation matrix, excluding the diagonal
  lower_tri = corr_matrix[lower.tri(corr_matrix)]
  
  # Adjust perfect correlations slightly to avoid infinite z-scores
  lower_tri[lower_tri == 1] = 1 - 1e-10
  lower_tri[lower_tri == -1] = -1 + 1e-10
  
  # Apply Fisher's z transformation
  z_scores = atanh(lower_tri)
  
  # Calculate the mean z-score and convert back to the correlation scale
  average_z_score = mean(z_scores, na.rm = TRUE)
  average_correlation = tanh(average_z_score)
  
  return(average_correlation)
}


