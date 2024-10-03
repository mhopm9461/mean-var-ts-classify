#------------------------------------------
# This script sets out to compute 
# classification accuracy for FTM for all
# problems
#
# NOTE: This script requires setup.R and
# analysis/compute-mean-and-var.R to have 
# been run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 2 March 2023
#--------------------------------------

# Load data
load("data/mean_sd_test.Rda")

# Fit classifiers

ftm <- unique(mean_sd_test$problem) %>%
  purrr::map_dfr(~ evaluate_performance(mean_sd_test, .x, n_resamples = 30))

# ftm <- map(~ evaluate_performance(unique_problems, .x, n_resamples = 30))
# ftm <- bind_rows(ftm)

# Save the result
save(ftm, file = "data/ftm.Rda")

# library(purrr)
# library(dplyr)
# 
# ftm <- unique(mean_sd_test$problem) %>%
#   map(~ evaluate_performance(mean_sd_test, .x, n_resamples = 30)) %>%
#   bind_rows()
