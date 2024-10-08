#-----------------------------------------
# This script can be used to run the whole
# project in order from start to finish.
# It requires 
# http://www.timeseriesclassification.com/Downloads/Archives/Univariate2018_arff.zip
# to be downloaded, unzipped, and put into 
# the data/ folder first.
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 9 February 2023
#-----------------------------------------
# Edited by: Michael H, 3 October 2024
#-----------------------------------------

source("setup.R") #MH reviewed & rerun

#---------------- UEA & UCR Repository analysis ------------

# Prepare time-series datasets

source("analysis/prepare-time-series-data.R") #MH reviewed & rerun

# Summarise problems

source("analysis/summarise-problems.R") #MH reviewed & rerun

# Calculate mean and variance features and classification performance

source("analysis/compute-mean-and-var.R") #MH reviewed & rerun
source("analysis/fit-ftm-classifiers.R")
source("analysis/analyse-ftm-performance.R") # <----- R file not found 28/9/2024. Commented out to minimize error codes. 
source("analysis/analyse-ftm-performance-against-chance.R")

# Calculate catch24 features and compute classification performance

source("analysis/compute-catch24.R")
source("analysis/fit-catch24-classifiers.R")

# Analyse case studies where mean + variance does very well

source("analysis/mean-and-var-case-studies.R")

# Compare classification performance between mean + variance and catch24

source("analysis/compare-catch2-catch24.R")

#---------------- Schizophrenia case study ------------

source("analysis/schizophrenia_case_study.R")
