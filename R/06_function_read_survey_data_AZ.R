#function to read meal survey data (ASZ) --------
  
  # status: july 2020
  # author: gian-andrea egeler
  
# libraries
library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(magrittr)
library(readxl)



# load survey data and do some eidtig 
read_survey_long <- function(filename){
  #' @param filename includes the path and filename
  #' @author gian-andrea egeler
  #' @return data set containing survey data from ASZ with two waves
  #' note: there are lots of missing in the data due to long format
  #' especially in t1 (to should be clean) 
  

  # especially good for plotting  
  df_long <- readxl::read_xlsx(path = filename, skip = 1, col_names = TRUE, 
                       trim_ws = TRUE) %>% # https://github.com/tidyverse/readr/issues/892
    # gender change na to keine angaben
    mutate(gender = if_else(is.na(gender), "keine Angaben", gender)) %>% 
    # add new variable 
    #attention age is transformed back to a character: https://stackoverflow.com/questions/58124530/pivot-longer-with-multiple-classes-causes-error-no-common-type
    tidyr::pivot_longer(.,-c(ID, ASZ, time), names_to = "questions", 
               values_to = "answer", 
               values_transform = list(answer = as.character)) %>% 
    # drop all na in T0 concering the question of the intervention (were only ask in the second wave): https://community.rstudio.com/t/using-regex-to-optimise-str-detect-in-case-when/10485/3
    mutate(answer_2 = case_when(stringr::str_detect(.$time, "T0") & 
                                  stringr::str_detect(.$questions, "inter+")   ~ "delete")) %>% 
    # drop all NA's, not a nice way, however it works
    filter(is.na(answer_2)) %>% 
    select(-answer_2)
  
  message(filename, " was read successfully into long format")
  
  return(df_long)
  
  
}



# load survey data and do some eidtig 
read_survey_wide <- function(filename){
  #' @param filename includes the path and filename
  #' @author gian-andrea egeler
  #' @return data set containing survey data from ASZ with two waves
  #' note: there are lots of missing in the data due to long format
  #' especially in t1 (to should be clean) 
  
  # read csv, into two different formats
  df_wide <- readxl::read_xlsx(path =filename, skip = 1, col_names = TRUE, 
                       trim_ws = TRUE) %>% # https://github.com/tidyverse/readr/issues/892
    # gender change na to keine angaben
    mutate(gender = if_else(is.na(gender), "keine Angaben", gender)) %>% 
    # add new variable
    mutate(age_years = 2020 - as.numeric(age)) 
  
  
  message(filename, " was read successfully into wide format")
  
  return(df_wide)
  
}

message("functions\n
        -read_survey_wide 
        -read_survey_long
        \nare loaded")
