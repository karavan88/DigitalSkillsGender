#-------------------------------------------------------------------
# Project: Gender Gap in Digital Skills 
# Sector: UNICEF Education Data & Analytics
# Author: Garen Avanesian
# Date: 20 Oct 2023; 
#-------------------------------------------------------------------

# set working directories and all directories 
# this is the profile for the PROD-SDG_report_2023 project
# this profile should be loaded before running any other script

USERNAME    <- Sys.getenv("USERNAME")
USER        <- Sys.getenv("USER")

#SPECIFY if the output should be saved in the teams folder
# allow users to chose between saving in the teams folder or in the REPO folder
useTeamsFolder = TRUE

#! please edit this accordingly, and add your username
#  
# if (USERNAME == "<add your user name here>"){
#  projectFolder  <- file.path(Sys.getenv("USERPROFILE"), "<this is a demo adjust as needed> Documents/GitHub/PROD-SDG_report_2023")
#  repoFolder     <- file.path(Sys.getenv("USERPROFILE"), "<this is a demo adjust as needed> UNICEF/Chief Statistician Office - 020.SDG-Report")
#  teamsFolder    <- file.path(Sys.getenv("USERPROFILE"), "<this is a demo adjust as needed> UNICEF/Chief Statistician Office - 020.SDG-Report")
# } 

#version from everyone, the profile works for everyone

if (USERNAME == "kavanesyan") {
  
  projectFolder    <- getwd()
  
} 


if (USER == "karavan88") {
  
  projectFolder  <- getwd()
  
} 



# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))

metaData              <-  file.path(projectFolder, "00_metadata")
inputData             <-  file.path(projectFolder, "01_input_data")
rCodes                <-  file.path(projectFolder, "02_codes")
outputData            <-  file.path(projectFolder, "03_output_data")
