# read meal menus from AZ (alterszentrum))--------

# status: october 2020
# author: gian-andrea egeler

# libraries
library(tidyverse)
library(here)
library(magrittr)
library(lubridate)
# library(textreadr)
library(docxtractr)
library(zoo)

# extract tables form word: custom
extr_table_doc <- function(docx_file){
  #' @param docx_file needs to be a docx object
  #' @return extracts all tables from the docx objects, brings them into a long_format
  #' and paste them together, furthermore the function adds some additional variables 
  #' like meal_line, date and kw (calendar week)
  #' @export
  
    # extract the rest
  df <- docxtractr::docx_extract_all_tbls(docx_file) %>% # there are two tables on two documents 
    plyr::ldply(., bind_rows) %>% # bind them together
    filter(str_detect(X2020, "täglich", negate = TRUE)) %>%
    pivot_longer(-c("X2020"), names_to = "meal_line", values_to = "meal_content") %>% 
    mutate(date_ = paste0(.$X2020, "2020")) %>% 
    mutate(date = as_date(date_, format = "%d.%m.%Y")) %>% 
    drop_na() %>% # drop information about the dessert => more cleaning see house_keeping
    filter(!stringi::stri_isempty(.$meal_content)) %>% # drop empty strings
    mutate(kw = isoweek(date)) %>% 
    select(date, kw, meal_line, meal_content)
    
  
  message(attributes(docx_file)$names[5], df$kw[1], " was processed successfully.")
  
  
  return(df) 
}

message("function extr_table_doc is ready")
