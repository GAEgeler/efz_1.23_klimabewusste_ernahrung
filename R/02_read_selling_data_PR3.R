# read meal menus from PR3 (staff canteen)--------

# status: february 2020
# author: gian-andrea egeler

# libraries
library(dplyr)
library(readxl)
library(here) # pay attention with there is a conflict with lubridate!
library(lubridate)
library(stringi)

# read seling data 2019-------------------
# tab names are also the dates
tab_names <- readxl::excel_sheets(paste0(till_pr, "PR3/2019/apr_jun_2019/Verkaufsstatistik_Ap_Jun_2019_egel.xlsx")) # deleteted some sheets per hand

# unable to open file
# https://github.com/tidyverse/readxl
# attention xls and xlsx!
list_all <- lapply(tab_names, function(x) readxl::read_xlsx(path = paste0(till_pr, "PR3/2019/apr_jun_2019/Verkaufsstatistik_Ap_Jun_2019_egel.xlsx"), 
                                                   sheet = x, range = "B1:AA73", trim_ws = T, col_names = F, # range needs to be bigger, do get the dates
                                                   col_types = "guess")) 


# problem how to extract date of the datasets
# one solution to get the date
tt <- tibble()
for (i in 1:length(list_all)) {
  dt <- plyr::ldply(list_all[i], data.frame) %>% 
    mutate(date = str_sub(...18[1], start = -10)) %>% # extract date 
    mutate(date = as_date(date, format = "%d.%m.%Y", tz = "UTC")) %>% # change format
    select(...1, ...3, ...5,  ...9, ...12, date) %>% 
    rename(art_id = ...1, meal_line_orig = ...3, brutto = ...5, tot_sold = ...9, wght = ...12) %>% 
    drop_na(art_id) %>% # drop all NA
    as_tibble()
  tt <- bind_rows(dt, tt)
}

# delete some rows, and change format of some variables
selling_ <- tt %>% 
  filter(!str_detect(.$art_id,"Total+|Montag|Gruppen|Artikel")) %>%  # drop useless information
  mutate(brutto = as.numeric(.$brutto),
         tot_sold = as.numeric(.$tot_sold),
         wght = as.numeric(.$wght),
         art_id = as.integer(.$art_id))
  
#no distinction between portion sizes
selling_1 <- selling_ %>% 
  mutate(meal_line = str_replace_all(.$meal_line_orig, " 1/2 Portion| 1/2 Port", "")) %>% # whatch out for whithe space
  mutate(meal_line = str_replace(.$meal_line, "Vegetarisch", "Menu 3")) %>%  # change vegetarisch to menu 3 (according menu offer)
  mutate(meal_line = trimws(.$meal_line)) %>% 
  mutate(meal_line = str_replace_all(.$meal_line, "\\ü", "u")) # take away umlaute


# load info from menu offer
source("R/02_read_menu_data_PR3.R", encoding = "UTF-8") # dataset: menu_offer_pr3


# merge
selling_2 <- selling_1 %>% 
  left_join(., menu_offer_pr3, by = c("date", "meal_line")) # pay attention to the umlaute on menü
    

#filter only main meals
selling_PR3 <- selling_2 %>%
  filter(meal_line == "Menu 1" | meal_line == "Menu 2" | meal_line == "Menu 3" | meal_line == "Wochenhit" | meal_line == "Wochensnack") %>% 
  group_by(date, meal_line, meal_label, meal_content) %>%  # take meal_lines (half and full proportions) together
  summarise(tot_sold = sum(tot_sold)) %>%
  mutate(source = "PR3") %>%
  select(date, meal_line, tot_sold, meal_content, meal_label, source) %>%  # drop some variables wght, art_id
  rename(meal_component = meal_content) 


# delete all unused data sets
rm(list = c("selling_", "dt", "tt", "tab_names", "list_all", "selling_1", "selling_2", "menu_offer_pr3"))
