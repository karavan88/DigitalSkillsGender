#-------------------------------------------------------------------
# Project: Gender Gap in Digital Skills 
# Sector: UNICEF Education Data & Analytics
# Author: Garen Avanesian
# Date: 20 Oct 2023; 
#-------------------------------------------------------------------


# List of required packages
required_packages <- c("readr", 
                       "tidyverse", 
                       "data.table", 
                       "magrittr", 
                       "countrycode",
                       "readxl",
                       "haven",
                       "srvyr",
                       "plm",
                       "sandwich",
                       "lmtest",
                       "fastDummies",
                       "lme4",
                       "arm",
                       "lmerTest",
                       "ggrepel",
                       "lfe",
                       "ggpubr",
                       "DT")


# Function to check and install packages
check_and_install_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
}

# Call the function with the list of required packages
check_and_install_packages(required_packages)