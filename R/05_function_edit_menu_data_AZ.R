# house keeping meal menus AZ (alterszentrum))--------

# status: october 2020
# author: gian-andrea egeler

#requred packages
library(tm)
library(stringi)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)

# house keeping
house_keeping_asz <- function(dat){
  #' @param df input is a data frame
  #' @return cleans the meal_content and rename content of meal_line, add meal_label
  #' and paste toghether meal_content
  #' @export
  
  # add some stopwords if necessary
  stopwords_manual <- c("l")
  
  # delete all special characters => | is not recognised correctly, seems like a *l*
  df <- dat %>% 
    mutate(meal_content = stringi::stri_trans_general(meal_content,"Latin-ASCII")) %>% # replace all umlaute
    mutate(meal_content = stringr::str_to_lower(meal_content)) %>%  # all to lower capitals
    mutate(meal_content = stringr::str_replace(meal_content, '\\*', '')) %>%  # replace all asterisks => not working
    mutate(meal_content = stringr::str_replace(meal_content, '\\|', ',')) %>%  # replace | by comma (,) => not working
    mutate(meal_content = stringr::str_replace_all(meal_content, "[[:punct:]]", " ")) %>%  # remove all punctuation
    mutate(meal_content = tm::removeNumbers(meal_content)) %>%  ## remove numbers
    mutate(meal_content = tm::removeWords(meal_content, stopwords("german"))) %>%  ## remove stop words
    mutate(meal_content = tm::removeWords(meal_content, words = stopwords_manual)) %>%  # remove words form a document, see config.R
    mutate(meal_content = tm::stripWhitespace(meal_content)) %>%  # strip white space (double white spaces are collapsed to one)
    mutate(meal_content = stringr::str_trim(meal_content, side = "both")) %>%   # remove white space
    drop_na() # drop nas in date
  
  
  #paste toghether the meal content to one entry
  #https://markhneedham.com/blog/2015/06/27/r-dplyr-squashing-multiple-rows-per-group-into-one/
  if("date" %in% colnames(df)){ #check if date is in data set
    df <- df %>% 
    group_by(date, kw, meal_line) %>% 
    summarise(meal_content = paste(meal_content, collapse = " ")) %>% 
    ungroup()
  }else{
    df <- df %>% 
      group_by(kw, meal_line) %>% 
      summarise(meal_content = paste(meal_content, collapse = " ")) %>% 
      ungroup()
  }
  
  return(df) 
}

message("function house_keeping_asz is ready")
