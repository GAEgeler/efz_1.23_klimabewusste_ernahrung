# read meal menus from PR5 (staff canteen)--------

# status: january 2020
# author: gian-andrea egeler


# load required packages
library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)
library(here)

## read data - menuplan--------

#define read funktion
read_menu_xls <- function(filename){
  # read excel-sheets
  tab_names<-excel_sheets(filename) 
  # loop trough tab_names
  list_all <- lapply(tab_names, function(x) read_excel(filename, sheet = x, range = "A6:H68")) # attention 2 excel_sheets (february) exceed the h63, thus put H:68 in it
  # combine all sheets to one
  # attention meal names changed over time
  menu_plan <- plyr::ldply(list_all, data.frame)
  # add filename for checking purpose
  menu_plan$source <- str_sub(filename, start = -36) # do not copy the hole path
  return(menu_plan)
}

# read files and merge them all togheter
files = list.files(paste0(till_pr, "PR5/menuplan/") , pattern = "xlsx")
menu_offer_2019 <- plyr::ldply(paste0(till_pr, "PR5/menuplan/", files), read_menu_xls) # takes around 10 sec


# drop some columns-----------
offer <- menu_offer_2019 %>% 
  select(-c("...1", "...4", "...6", "...8")) %>% 
  rename(date =...2, Fit.Triemli = Leicht...fit, Vegi.Triemli = Vegetarisch) %>%  # rename some columns
  as_tibble()

# drop some rows containing g?ste (not usuefull infomation)---------
# that code is not realy working
offer <- offer[!grepl("Gäste", offer$Tageshit.Triemli), ]
offer_ <- offer[!grepl("Gäste", offer$Fit.Triemli), ]
offer_<- offer_[!grepl("Gäste", offer_$Vegi.Triemli), ]

# drop na in date, some of them because of the year 2018 (total 656)--------
offer_1 <- drop_na(offer_, date)

# to long format: --------
# change date to dateformat
offer_2 <- offer_1 %>% 
  pivot_longer(-c("date","source"), names_to = "meal_line", values_to = "meal_content") %>% 
  mutate(date2 = as_datetime(.$date, tz = "UTC")) %>% # origin = "1970-01-01", however takes the seconds with it
  mutate(date = as_date(.$date2)) %>%  # convert it to a date to not have a datetime (much easier to filter)
  drop_na(meal_content)
  

# delete all special expressions in variable meal_component---------
offer_long <- offer_2 %>% 
  # extract string before: https://stackoverflow.com/questions/38291794/extract-string-before
  mutate(meal_con = str_replace(.$meal_content, "\\|.*", "")) %>% # deletes all after |
  mutate(meal_con = str_replace(.$meal_con, "\\.*", "")) %>% # deletes all after .
  mutate(meal_content = str_trim(.$meal_con)) %>% # delete all white spaces
  select(-meal_con, -source, -date2) %>%
  filter(meal_content != "") # drop 16 cases with no information in it



# add another variable "label" for meal_content e.g. vegi.triemli = "vegetarian"-----
menu_offer_pr5 <- offer_long %>%
  mutate(meal_label = case_when(.$meal_line == "Tageshit.Triemli" ~ "meat",
                                .$meal_line == "Fit.Triemli" ~ "meat",
                                .$meal_line == "Vegi.Triemli" ~ "vegetarian",
                                FALSE ~ .$meal_line))


# remove unused data-----
rm(list = c("offer", "offer_long", "offer_", "offer_1", "offer_2", "menu_offer_2019"))
