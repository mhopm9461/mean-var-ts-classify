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

# MH attempt to resolve error with unexpected column name
mean_sd_test_new <-as.data.frame(mean_sd_test) # take copy of original data file
colnames(mean_sd_test_new) <-c('id','group','set_split','problem','method','names','values') # nolint
# need col types fix here
save(mean_sd_test_new, file = "C:/Users/mhopm/Documents/GitHub/mean-var-ts-classify/data/mean_sd_test.Rda") # nolint
# load("data/mean_sd_test.Rda")
# End

# Fit classifiers

ftm <- unique(mean_sd_test$problem) %>% # MH - getting stuck here or in next line - debugging required # nolint
  purrr::map_dfr(~ evaluate_performance(mean_sd_test, .x, n_resamples = 30))

save(ftm, file = "data/ftm.Rda")
