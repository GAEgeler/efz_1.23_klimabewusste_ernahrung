#read and merge menu and selling data from PR5 (staff canteen)-------


# status: january 2020
# author: gian-andrea egeler


# load required packages---------
library(tidyverse)
library(lubridate)
library(readxl)
library(here) # more reliable for loading data, attention with lubridate together


# read data - selling data--------
tab_names <- excel_sheets(paste0(till_pr, "PR5/Statistik_Essen_2019_wknd_egel.xls")) # deleteted some sheets per hand
tab_names <- tab_names[-c(1:3,16)]# delete some sheets, which arent relevant


#list all the tab_names
# pay attention to the date variable, there are some special characters in the variable Datum: "-"
list_all <- lapply(tab_names, function(x) read_xls(path =paste0(till_pr, "PR5/Statistik_Essen_2019_wknd_egel.xls"), 
                                                   sheet = x, range = "E6:K38", trim_ws = T, na = "-", 
                                                   col_types = "guess"))  # col_date only works for readr: https://github.com/tidyverse/readxl/issues/490
  

# combine all sheets to one data.frame
selling_d <- plyr::ldply(list_all, data.frame) %>% 
  dplyr::select(-Wochentag) %>%  # drop that variable
  drop_na(Datum) %>% 
  rename(date = Datum, Fit.Triemli = Leicht...Fit,  Vegi.Triemli = Vegi, Tageshit.Triemli = Tageshit, Budget.Triemli = Budgetteller, Buffet = Salatbuffet)


# date to long format
selling_long <- selling_d %>%
  # more infos: https://www.r-bloggers.com/data-pivoting-with-tidyr/
  # and here: https://tidyr.tidyverse.org/reference/pivot_longer.html
  pivot_longer(-date, names_to = "meal_line", values_to = "tot_sold") %>%  # take all variables as col, except date (thus minus before)
  mutate(date = as_date(date)) # change datetime to date (better for merging and filtering)

# merge selling data with offer-----------
# load data
source("R/01_read_menu_data_PR5.R", encoding = "UTF-8") # takes a while, and does not that what I want => always double check

# merge data with meal offer
selling_data <- menu_offer_pr5 %>% 
  select(date, meal_content, meal_line, meal_label) %>% 
  left_join(selling_long,., by = c("date", "meal_line")) #%>% 
  # mutate(date2 = wday(date, label = F, week_start = getOption("lubridate.week.start", 1)))  # start = 1 means monday; not anymore necessary
  

# drop all the meals with no usable information eg sellings and meal_comp
selling_PR5 <- selling_data %>% 
  drop_na(tot_sold) %>%
  mutate(source = "PR5") %>%
  select(date, meal_line, tot_sold, meal_content, meal_label, source) %>% 
  rename(meal_component = meal_content)


# delete unused files------
rm(list = c("list_all", "menu_offer_pr5", "read_menu_xls", "selling_d", "selling_data","selling_long", "tab_names"))
