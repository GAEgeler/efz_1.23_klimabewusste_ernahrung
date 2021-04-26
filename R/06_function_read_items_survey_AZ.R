# function to read survey data (ASZ) --------

# status: november 2020
# author: gian-andrea egeler

# libraries
library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(magrittr)
library(readxl)


# load data of the single items, attention check path if correct
source("R/config_path.R")
filename = "items_survey_asz_20202219_egel.xlsx"

if(!file.exists(paste0(survey_asz, filename))) message("double check your path, or folder structure: file is not there")

items <- read_xlsx(path = paste0(survey_asz, filename), 
                   col_names = TRUE, 
                   trim_ws = TRUE)
  




