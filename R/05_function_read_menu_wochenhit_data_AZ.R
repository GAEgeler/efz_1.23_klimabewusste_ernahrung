# read meal menus (only wochenhit) from AZ (alterszentrum))--------

# status: october 2020
# author: gian-andrea egeler

# libraries
library(dplyr)
library(magrittr)


# extract tables form word: custom
extr_wochenhit_doc <- function(docx_file){
  #' @param docx_file needs to be a docx object
  #' @return extracts all tables from the docx objects, brings them into a long_format
  #' and paste them together, furthermore the function adds some additional variables 
  #' like meal_line, date and kw (calendar week)
  #' @export
  
  
  # extract wochenhit: not working, not sure why!
  wochenhit <- docxtractr::docx_extract_tbl(docx_file, tbl_num = 2) %>% # select only the second table (wochenhit is always there)
    # form to long format
    pivot_longer(-c("X2020"), names_to = "meal_line", values_to = "meal_content") %>% 
    mutate(date_ = paste0(.$X2020, "2020")) %>% 
    #change date to dateformat 
    mutate(date = lubridate::as_date(date_, format = "%d.%m.%Y")) %>% 
    # add kalenderweek
    mutate(kw = lubridate::isoweek(date)) %>% 
    # drop empty stings
    filter(!stringi::stri_isempty(.$meal_content)) %>%
    #Last Observation Carried Forward, attention adds also the date (which is not correct)
    zoo::na.locf() %>% 
    # only wochenhit
    filter(stringr::str_detect(X2020, "täglich")) %>% 
    #drop date again
    mutate(meal_line = "wochenhit") %>% 
    select(kw, meal_line, meal_content)
    
 
  message(attributes(docx_file)$names[5], wochenhit$kw[1], " was processed successfully.")
  
  return(wochenhit)
   
}

message("function extr_wochenhit_doc is ready.")