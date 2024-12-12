# Reproducible Science Assignment

This repository contains the files and scripts for the Penguins Project, in which I analyse penguin morphology, specifically focusing on bill measurements across different three penguin species... 

## Repository Structure
/project_root 
 /data # Data files
 /scripts # R scripts for analysis 
 /output # Output files (generated by the scripts) my_project.Rproj 
 /RStudio project file # Optional

## Finished Reports

In the /finished_files folder, you will find the completed reports summarizing the analysis, results, and conclusions:

- PDF Report: A full PDF report containing the analysis, results, and interpretations.
- HTML Report: An interactive HTML version of the report for a more dynamic viewing experience.

## Running the Analysis
If you would like to run the analysis and reproduce the results yourself, follow these steps to set up your local environment and execute the necessary code.

### Prerequisites
1. Install Git on your system.
2. Install R and RStudio.

### Installation
**1. Clone the repository:**

First, clone the repository to your local machine
This can be done by running the following command on Git bash or Terminal application:
```bash
git clone https://github.com/Biology3579/ReproducibleScienceAssignment.git 
```
This will create a local copy of the repository on your machine. After cloning, navigate to the project directory:
```bash
cd ReproducibleScienceAssignment
```

**2. Install renv (if not already installed)**

In your R console, run the following command to install the renv package, which is used to manage the project's R dependencies:
```r
install.packages("renv")
```
**3. Restore the R environment**

After cloning the repository and installing renv, navigate to the project directory and run the following command in R to restore the project's environment:
```r
renv::restore()
```
*This command will:*

 - Install the required R packages as specified in the renv.lock file.
 - Set up the project environment to match the one used when the project was initially created, ensuring that all dependencies are correctly installed.

**4. Load necessary packages**

_Packages Used:_ 

This project requires the following R packages:
- **`here`** - Specifies directory paths relative to the project root.
- **`tinytex`** - A lightweight LaTeX distribution, useful for generating PDFs or reports.
- **`renv`** - Manages R environment and ensures reproducible package installation.
- **`janitor`** - Provides functions to clean and tidy data.
- **`tidyverse`** - A collection of data science packages for data manipulation and visualization.
- **`palmerpenguins`** - Contains the dataset of penguin measurements used in the analysis.
- **`dplyr`** - Used for data manipulation.
- **`ggplot2`** - Used for creating visualizations.
- **`lme4`** - Used for fitting linear models.
- **`grid`** - Provides functionality for graphical layout.
- **`gridExtra`** - Extends the `grid` package to arrange multiple plots.
- **`ggsignif`** - Adds significance annotations to plots.

These packages are managed through `renv`, and the necessary versions are specified in the `renv.lock` file. 

To load the necessary packages run the following command in R:
```r
# load the required packages
library(here)        # for specifying the directory
library(tinytex)     # for LaTeX-related tasks
library(renv)        # for environment management
library(palmerpenguins)  # for dataset
library(janitor)     # for cleaning data
library(tidyverse)   # for data manipulation and visualization
library(dplyr)       # for data manipulation
library(ggplot2)     # for data visualization
library(lme4)        # for linear models
library(grid)        # for graphical layout
library(gridExtra)   # for graphical layout
library(ggsignif)    # for adding significance annotations to ggplot2 plots
```

**5. Run the analysis**

Once the environment is set up and the packages are loaded, you can run the analysis by executing the ... provided in the /scripts directory. 
