## Script name: cleaning_and_curating.R
##
## Purpose of script: 
##    A collection of functions for cleaning and curating the Palmer Penguins 
##    raw_dataset for analysis.
##
## Author: Biology3579
##
## Date Created: 2024-10-01 
##
## ---------------------------
#
# Load necessary libraries
library(dplyr)
library(janitor)


# Cleaning ----
# This set of functions help to streamline the dataset, standardising variable names 
# and removing any rows or column with missing data (empty entries)
# The clean data includes all the variables available from the original set but
# in a cleaner format which might help with future analyses.

# Function to standardise column names to snake case
clean_col_names <- function(raw_data) {
  raw_data %>%
    clean_names()  # Converts column names to snake case
}

# Function to shorten species names for readability
shorten_species <- function(raw_data) {
  raw_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"))
}

# A function to remove any empty columns or rows
remove_empty_entries <- function(raw_data) {
  raw_data %>%
    remove_empty(c("rows", "cols"))
}

# A unified cleaning function combining all of the cleaning functions for simplicity of use
cleaning_penguins <- function(raw_data) {
  raw_data %>%
    clean_col_names() %>%
    remove_empty_entries() %>%
    shorten_species()
}


#Pre-processing and sub-setting raw_data ----
  #Here I curate the clean penguins data set specifically for my analysis:
  #   - I rename columns to make variable names more intuitive and analysis-friendly.
  #   - I filter out rows with missing values (NAs)
  #   - I ensure that the class type of key variables (e.g., numeric or factor) is correct 
  #     to avoid errors during analysis.
  #   - I remove variables that are not relevant to my specific analysis (e.g., administrative 
  #     or study-specific details) to reduce noise and streamline the dataset.
  #   - I create a new variable, `bill_morphology`, which represents the ratio of bill length
  #     to bill depth. This new variable is a critical metric for understanding differences 
  #     in bill shape across penguin species and serves as a key indicator in subsequent 
  #     statistical analyses.
  #This step is performed separately from the cleaning process to ensure that the 
  #original cleaned dataset remains intact and is not overwritten. 
  #This allows for easy reuse of the cleaned data for further analyses.

# Function to drop specific columns based on a provided list of column names
drop_cols <- function(clean_data, columns_names) {
  clean_data %>%
    select(-all_of(columns_names))
}

# Function to rename the columns for ease of use
rename_columns <- function(clean_data) {
  clean_data %>%
    rename(
      bill_length_mm = culmen_length_mm,  # Rename culmen_length_mm to bill_length_mm
      bill_depth_mm = culmen_depth_mm)   # Rename culmen_depth_mm to bill_depth_mm
}

# Function to filter out rows with missing values (NAs)
remove_NA <- function(raw_data) {
  raw_data %>%
    na.omit()
}

# Function for converting variables to their appropriate types
convert_variables <- function(clean_data) {
  clean_data %>%
    mutate(
      bill_length_mm = as.numeric(bill_length_mm),
      bill_depth_mm = as.numeric(bill_depth_mm),
      body_mass_g = as.numeric(body_mass_g),
      species = as.factor(species),
      sex = as.factor(tolower(sex))) # Convert to lowercase and make it a factor
}

# Function to calculate and add the bill morphology (length-to-depth ratio) to the dataset
add_bill_morphology <- function(clean_data) {
  clean_data %>%
    mutate(bill_morphology = bill_length_mm / bill_depth_mm)  # Add the bill morphology (length-to-depth ratio)
}

# A unified curating function combining all of the curating functions for simplicity of use
curating_penguins_clean <- function(clean_data){
  clean_data %>%
    drop_cols(c(
      "study_name",
      "region",
      "stage", 
      "flipper_length_mm",
      "sample_number",
      "clutch_completion",
      "date_egg",
      "delta_15_n_o_oo",
      "delta_13_c_o_oo",
      "comments")) %>%
    rename_columns() %>%
    remove_NA() %>%
    convert_variables()  %>%  
    add_bill_morphology()  
}
