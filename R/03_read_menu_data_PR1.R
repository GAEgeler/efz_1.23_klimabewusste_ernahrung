# read meal menus from PR1 (staff canteen)--------

# status: march 2020
# author: gian-andrea egeler

# libraries
library(tidyverse)
library(here)
library(magrittr)
library(lubridate)
# library(textreadr)


# load offer 2019--------------
# attention, i did the cleaning by my self => someonelse should do some checkings
# see readme file


# load excel sheets
# attention returns a list
# function from here: https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
read_excel_allsheets <- function(filename, tibble = T) {
  # read all sheets
  sheets <- readxl::excel_sheets(filename)
  # apply the sheets to the function read_clsc
  x <- lapply(sheets, function(X) readxl::read_xlsx(filename, sheet = X, range = "A1:D50"))
  # add a source information
  names(x) <- sheets 
  return(x)
}


# pay attention to the umlaute in file names (get some errors while sourcing the r-files)
list_all <- read_excel_allsheets(paste0(till_pr, "PR1/menuplan_2019/Personalmenus April-Juni 2019_egel.xlsx"))

# unlist list_all
menu_1 <- tibble()
for (i in 1:length(list_all)) {
  # unist and edit some of the data
  dt <- plyr::ldply(list_all[i], data.frame) %>% 
    rename(date = Datum, menu1 = MENU.1, menu2 = MENU.2...WOK) %>% 
    select(-.id) %>%
    pivot_longer(-date, names_to = "meal_line", values_to = "meal_content") %>% 
    mutate(date = as_date(date)) %>% 
    drop_na(date, meal_content) # drop na's
  #append data togheter
  menu_1 <- bind_rows(menu_1, dt)
}


# extract some useless information
# not finished
stopwords = c("gegarte|feinem|frittiertem|frischem|getrocknete|getrockneten|auf|an|Neue|sonnengetrockneten| Sautiert|geschmorten|mit|gebratener|Hausgemachter|Hausgemacht|hausgemacht|Glasierter|gebacken|gratiniert|Tranchierte|cremig") # pay attention to the
delete = c("Dessert|dessert|suppe|kl. süsse Überraschung|Sonntagsdessert")

menu_offer_pr1 <- menu_1 %>% 
  filter(!str_detect(.$meal_content, delete)) %>% # exclude some info in meal_content
  mutate(meal_content = str_remove_all(.$meal_content, stopwords)) %>% 
  # mutate(meal_content = str_replace_all(.$meal_content, "[0-9|()|\\*]", "")) # replace all numbers and parantheses and * with ""
  mutate(meal_label = case_when(.$meal_line == "menu1" ~ "meat",
                              .$meal_line == "menu2" ~ "vegetarian",
                              FALSE ~ .$meal_line)) 

# delete all unsused datasets
rm(list = c("delete", "stopwords", "list_all", "menu_1", "read_excel_allsheets"))
