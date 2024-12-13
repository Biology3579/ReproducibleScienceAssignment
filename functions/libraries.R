## Script name: plotting.R
##
## Purpose of script: 
##    A function to load all necessary libraries for the Pygoscelis penguins bill
##    morphology analysis, to declutter main analysis file.
##    An outline of the use of each library is also provided
##
## Author: Biology3579
##
## Date Created: 2024-10-01 
##
## ---------------------------

# Function to load all necessary libraries for the Pygoscelis penguins bill morphology analysis
load_libraries <- function() {
  library(here)            # for specifying the directory
  library(tinytex)         # for LaTeX-related tasks
  library(renv)            # for environment management
  library(palmerpenguins)  # for dataset
  library(janitor)         # for cleaning data
  library(tidyverse)       # for data manipulation and visualization
  library(dplyr)           # for data manipulation
  library(ggplot2)         # for data visualization
  library(svglite)         # for saving figures as svg
  library(lme4)            # for linear models
  library(rstatix)         # for pair-wise t-tests
  library(grid)            # for graphical layout
  library(gridExtra)       # for graphical layout
  library(ggsignif)        # for adding significance annotations to ggplot2 plots
}