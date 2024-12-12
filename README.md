# Reproducible Science Assignment

This repository contains the files and scripts for the Penguins Project, in which I analyse penguin morphology, specifically focusing on bill measurements across different three penguin species... 

## Repository Structure...
/project_root 
 /data # Data files
 /scripts # R scripts for analysis 
 /output # Output files (generated by the scripts) my_project.Rproj 
 /RStudio project file # Optional

## Prerequisites
1. Install [Git](https://git-scm.com/) on your system.
2. Install R and RStudio.

## Installation
**1. Clone the repository:**

First, clone the repository to your local machine:
```bash
 git clone https://github.com/Biology3579/ReproducibleScienceAssignment.git
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

**4. Run the analysis**

Once the environment is restored, you can proceed to run the analysis.
