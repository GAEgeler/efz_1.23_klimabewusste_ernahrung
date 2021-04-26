# read meal menus from Alterszentren (AZ) --------

# status: december 2020
# author: gian-andrea egeler

# libraries
library(dplyr)
library(stringr)
library(here)
library(magrittr)
library(lubridate)
library(readxl)


# load data
read_xlsx_custom_asz <- function(filename, ASZ, range){
  # read csv
  df <- read_xlsx(path = filename, range = range, col_names = TRUE, trim_ws = TRUE) # https://github.com/tidyverse/readr/issues/892
  
  if("...2" %in% colnames(df)){
    
    #when data set has two variables to use for the long format
    df <- df %>% 
    pivot_longer(., -c("...1", "...2"), names_to = "date", values_to = "tot_sold") %>% 
    mutate(date = as.numeric(date)) %>% # change first in numeric values
    mutate(date = as_date(date, origin = "1899-12-30"), # adjust the origin to get the right date!
           kw = isoweek(date)) %>% 
    rename(meal_line = ...1, meal_port = ...2) %>% 
    mutate(ASZ = ASZ,
           source = str_sub(filename, start = 93)) # attention depends on the path of the user
  }else if("...9" %in% colnames(df)){
    # when data set has a variable called ...9 
    df <- df %>% 
      mutate_all(as.character) %>% 
      select(-...9) %>% 
      pivot_longer(., -c("...1"), names_to = "date", values_to = "tot") %>% 
      mutate(date = as.numeric(date)) %>% # change first in numeric values
      mutate(date = as_date(date, origin = "1899-12-30"), # adjust the origin to get the right date!
             kw = isoweek(date)) %>% 
      rename(info_amis = ...1) %>% 
      mutate(ASZ = ASZ,
             source = str_sub(filename, start = 93))
  }else{
    #when data set has only one (...1) variable to use for the long format
    df <- df %>% 
      mutate_all(as.character) %>% 
      pivot_longer(., -c("...1"), names_to = "date", values_to = "tot") %>% 
      mutate(date = as.numeric(date)) %>% # change first in numeric values
      mutate(date = as_date(date, origin = "1899-12-30"), # adjust the origin to get the right date!
             kw = isoweek(date)) %>% 
      rename(info_amis = ...1) %>% 
      mutate(ASZ = ASZ,
             source = str_sub(filename, start = 93))
  }
  
  message(str_sub(filename, start = 93), " was read successfully")
    
  return(df) 
  
}

message("function read_xlsx_custom_asz is ready")









