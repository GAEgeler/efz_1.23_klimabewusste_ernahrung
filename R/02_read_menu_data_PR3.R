# read meal menus from PR3 (staff canteen)--------

# status: february 2020
# author: gian-andrea egeler

#libraries
library(dplyr)
library(docxtractr)
library(magrittr)
library(lubridate)
library(zoo)

#load config file
source("R/config_path.R")

#read menu offer 2019-----------------
# some small adjustment were made by hand => see readme-file
files = list.files(paste0(till_pr, "PR3/menu_plan_2019/") , pattern = "docx", full.names = T)
menu_ <- sapply(files[2:14], docxtractr::read_docx, simplify = FALSE) # should returns a list with docx objects, throws an error if any word-file is opened or a tmp file is in the folder

# unlist menu_: warnings are due to as_date() function
menu_1 <- tibble()
for (i in 1:length(menu_)) {
  dt <- plyr::ldply(menu_[i], docxtractr::docx_extract_tbl) %>% 
    pivot_longer(-c(".id", "X"), names_to = "date", values_to = "meal_content") %>% 
    mutate(meal_content = ifelse(.$meal_content == "", NA, .$meal_content)) %>%  # change empty strings to NA then drop them
    zoo::na.locf() %>% #Last Observation Carried Forward
    mutate(date_ = str_sub(date, start = -6)) %>% # extract date 
    mutate(date_2 = str_replace(.$date_, "\\.*", ""), # delete . before the date starts
           date_3 = paste(date_2, "2019", sep = "")) %>% # add 2019
    mutate(date = as_date(date_3, format = "%d.%m.%Y", tz = "UTC")) %>% # convert to date format 
    mutate(X = ifelse(.$X == "", NA, .$X)) %>%  # change empty strings to NA then drop them
    drop_na(X) %>% # drop nas
    select(-.id, -date_, -date_2, -date_3, date, X, meal_content) %>% 
    rename(meal_line = X)
  menu_1 <- bind_rows(dt, menu_1)
}



# do some mis en place: delete unnecessary information
menu_2 <- menu_1 %>% 
  filter(!str_detect(.$meal_line, "Dessert|Preis|Tagessuppe")) %>% # ventually also pasta & grill? check for data in sellings
  filter(!str_detect(.$meal_content, "Geschlossen|geschlossen|Auffahrt|Pfingst|Tagessuppe")) %>% # exclude some info in meal_content
  # delete every thing in parantheses (with it), see https://stackoverflow.com/questions/24173194/remove-parentheses-and-text-within-from-strings-in-r/24173271
  mutate(meal_content = str_replace(.$meal_content, "\\s*\\([^\\)]+\\)", " ")) %>% 
  mutate(meal_line = str_sub(.$meal_line, end = 6)) %>% 
  mutate(meal_line = str_replace(.$meal_line, "Wochen", "Wochenhit")) %>% # rename some strings
  mutate(meal_line = str_replace(.$meal_line, "Pasta1", "Pasta")) %>%  # rename resp. replace some strings
  mutate(meal_content = str_replace_all(.$meal_content, "[0-9|()|\\*]", "")) # replace all numbers and parantheses and * with ""

# do some last steps before data is ready for merging
# eg in selling data the meal lines are withou umlauf
menu_offer_pr3 <- menu_2 %>% 
  mutate(meal_line = str_replace(.$meal_line, "Snack", "Wochensnack")) %>% 
  mutate(meal_line = str_replace_all(.$meal_line, "\\ü", "u")) %>% 
  mutate(meal_label = case_when(.$meal_line == "Menu 1" ~ "meat",
                                .$meal_line == "Menu 2" ~ "meat",
                                .$meal_line == "Menu 3" ~ "vegetarian",
                                FALSE ~ .$meal_line))  # else condition will be replaced with NA
  

# delete all unsused datasets
rm(list = c("files", "menu_", "menu_1", "menu_2")) 
